subroutine appcpr(kptsc)
!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
    implicit none
! person_in_charge: natacha.bereux at edf.fr
#include "asterf_types.h"
#include "asterf.h"
#include "jeveux.h"
#include "asterc/asmpi_comm.h"
#include "asterfort/apbloc.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
    integer :: kptsc
!----------------------------------------------------------------
!
!  CREATION DU PRECONDITIONNEUR PETSC (INSTANCE NUMERO KPTSC)
!  PHASE DE PRE-TRAITEMENT (PRERES)
!
!----------------------------------------------------------------
!
#ifdef _HAVE_PETSC
#include "asterf_petsc.h"
#include "asterfort/ldsp1.h"
#include "asterfort/ldsp2.h"
!----------------------------------------------------------------
!
!     VARIABLES LOCALES
    integer :: rang, nbproc
    integer :: dimgeo, dimgeo_b, tbloc, niremp, istat 
    integer :: jnequ, jnequl
    integer :: nloc, neqg, ndprop, ieq, numno, icmp
    integer :: iret
    integer :: il, ix, iga_f, igp_f
    integer :: fill
    integer, dimension(:), pointer :: slvi => null()
    integer, dimension(:), pointer :: prddl => null()
    integer, dimension(:), pointer :: deeq => null()
    integer, dimension(:), pointer :: delg => null()
    integer, dimension(:), pointer :: nulg => null()
    integer, dimension(:), pointer :: nlgp => null()
    mpi_int :: mpicomm
!
    character(len=24) :: precon
    character(len=19) :: nomat, nosolv
    character(len=14) :: nonu
    character(len=8) :: nomail
    character(len=4) :: exilag
    character(len=24), dimension(:), pointer :: slvk => null()
!
    real(kind=8) :: fillin, val
    real(kind=8), dimension(:), pointer :: coordo => null() 
    real(kind=8), dimension(:), pointer :: slvr => null()
!
    logical :: lmd
!
!----------------------------------------------------------------
!     Variables PETSc
    PetscInt :: low, high, ierr, bs, nterm, nsmooth
    PetscReal :: fillp
    PetscScalar :: xx_v(1)
    PetscOffset :: xx_i  
    
    Mat :: a
    Vec :: coords
    KSP :: ksp
    PC  :: pc
    mpi_int :: mrank, msize
    MatNullSpace :: sp
!----------------------------------------------------------------
    call jemarq()
!
!   -- COMMUNICATEUR MPI DE TRAVAIL
    call asmpi_comm('GET', mpicomm)
!
!     -- LECTURE DU COMMUN
    nomat = nomats(kptsc)
    nosolv = nosols(kptsc)
    nonu = nonus(kptsc)
    a = ap(kptsc)
    ksp = kp(kptsc)
!
    call jeveuo(nosolv//'.SLVK', 'L', vk24=slvk)
    call jeveuo(nosolv//'.SLVR', 'L', vr=slvr)
    call jeveuo(nosolv//'.SLVI', 'L', vi=slvi)
    precon = slvk(2)
    
    fillin = slvr(3)
    niremp = slvi(4)
    lmd = slvk(10)(1:3).eq.'OUI'
!
    fill = niremp
    fillp = fillin
    bs = 1
    
!
!   -- RECUPERE DES INFORMATIONS SUR LE MAILLAGE POUR 
!      LE CALCUL DES MODES RIGIDES
    call dismoi('NOM_MAILLA', nomat, 'MATR_ASSE', repk=nomail)
    call dismoi('DIM_GEOM_B', nomail, 'MAILLAGE', repi=dimgeo_b)
    call dismoi('DIM_GEOM', nomail, 'MAILLAGE', repi=dimgeo)
!
!     -- RECUPERE LE RANG DU PROCESSUS ET LE NB DE PROCS
    call asmpi_info(rank=mrank, size=msize)
    rang = to_aster_int(mrank)
    nbproc = to_aster_int(msize)
!
!     -- CAS PARTICULIER (LDLT_INC/SOR)
!     -- CES PC NE SONT PAS PARALLELISES
!     -- ON UTILISE DONC DES VERSIONS PAR BLOC
!   -- QUE L'ON CREERA AU MOMENT DE LA RESOLUTION (DANS APPCRS)
!     -----------------------------------------------------------
    if ((precon.eq.'LDLT_INC') .or. (precon.eq.'SOR')) then
        if (nbproc .gt. 1) then
!           EN PARALLELE, ON NE PREPARE PAS LE PRECONDITIONNEUR
!           TOUT DE SUITE CAR ON NE VEUT PAS ETRE OBLIGE
!           D'APPELER KSPSetUp
            goto 999
        endif
    endif
!
!   -- VERIFICATIONS COMPLEMENTAIRES POUR LES PRE-CONDITIONNEURS MULTIGRILLES
!   -- DEFINITION DU NOYAU UNIQUEMENT EN MODELISATION SOLIDE (2D OU 3D)
!   -------------------------------------------------------------------------
    if ((precon .eq. 'ML') .or. (precon .eq. 'BOOMER') .or. (precon.eq.'GAMG')) then
!       -- PAS DE LAGRANGE
        call dismoi('EXIS_LAGR', nomat, 'MATR_ASSE', repk=exilag, arret='C',ier=iret)
        if (iret .eq. 0) then
            if (exilag .eq. 'OUI') then
                call utmess('F', 'PETSC_18')
            endif
        endif
!       -- NOMBRE CONSTANT DE DDLS PAR NOEUD
        call apbloc(nomat, nosolv, tbloc)
        if (tbloc .le. 0) then
            call utmess('A', 'PETSC_18')
        else
        bs = abs(tbloc)
        endif
    endif
    
#ifdef ASTER_PETSC_VERSION_LEQ_34
#else
    if (precon.eq.'GAMG') then 
!       -- CREATION DES MOUVEMENTS DE CORPS RIGIDE --
!           * VECTEUR RECEPTABLE DES COORDONNEES AVEC TAILLE DE BLOC
!           
!       dimgeo = 3 signifie que le maillage est 3D et que les noeuds ne sont pas
!       tous dans le plan z=0
        ASSERT(dimgeo == 3)
        call jeveuo(nonu//'.NUME.NEQU', 'L', jnequ)
        neqg=zi(jnequ)
        call jeveuo(nonu//'.NUME.DEEQ', 'L', vi=deeq)
        call jeveuo(nomail//'.COORDO    .VALE','L',vr=coordo)
        !
        call VecCreate(mpicomm, coords, ierr)
        call VecSetBlockSize(coords, bs, ierr)
        if (lmd) then
            call jeveuo(nonu//'.NUML.NEQU', 'L', jnequl)
            call jeveuo(nonu//'.NUML.PDDL', 'L', vi=prddl)
            nloc=zi(jnequl)
            ! Nb de ddls dont le proc courant est propriétaire (pour PETSc)
            ndprop = 0
            do il = 1, nloc
               if (prddl(il) == rang ) then 
                   ndprop = ndprop + 1
               endif  
            end do
!
            call VecSetSizes(coords, to_petsc_int(ndprop), to_petsc_int(neqg), ierr)
        else
            call VecSetSizes(coords, PETSC_DECIDE, to_petsc_int(neqg), ierr)
        endif
!
        call VecSetType(coords, VECMPI, ierr)
!           * REMPLISSAGE DU VECTEUR
!             coords: vecteur PETSc des coordonnées des noeuds du maillage, 
!             dans l'ordre de la numérotation PETSc des équations
        if (lmd) then
          call jeveuo(nonu//'.NUML.NULG', 'L', vi=nulg)
          call jeveuo(nonu//'.NUML.NLGP', 'L', vi=nlgp)
          do il = 1, nloc
            ! Indice global PETSc (F) correspondant à l'indice local il
            igp_f = nlgp( il )
            ! Indice global Aster (F) correspondant à l'indice local il
            iga_f = nulg( il )
            ! Noeud auquel est associé le ddl global Aster iga_f 
            numno = deeq( (iga_f -1)* 2 +1 )
            ! Composante (X, Y ou Z) à laquelle est associé 
            ! le ddl global Aster iga_f
            icmp  = deeq( (iga_f -1)* 2 +2 )
            ASSERT((numno .gt. 0) .and. (icmp .gt. 0))
            ! Valeur de la coordonnée (X,Y ou Z) icmp du noeud numno
            val = coordo( dimgeo_b*(numno-1)+icmp )
            ! On met à jour le vecteur PETSc des coordonnées
            nterm=1
            call VecSetValues( coords, nterm, to_petsc_int(igp_f - 1), val,  INSERT_VALUES, ierr ) 
            ASSERT( ierr == 0 )
          enddo 
            call VecAssemblyBegin( coords, ierr ) 
            call VecAssemblyEnd( coords, ierr )
            ASSERT( ierr == 0 )
        ! la matrice est centralisée 
        else
          call VecGetOwnershipRange(coords, low, high, ierr)
          call VecGetArray(coords,xx_v, xx_i, ierr)
          ix=0
          do ieq = low+1, high
            ! Noeud auquel est associé le ddl Aster ieq 
            numno = deeq( (ieq -1)* 2 +1 )
            ! Composante (X, Y ou Z) à laquelle est associé 
            ! le ddl Aster ieq
            icmp  = deeq( (ieq -1)* 2 +2 )
            ASSERT((numno .gt. 0) .and. (icmp .gt. 0))
            ix=ix+1 
            xx_v(xx_i+ ix) = coordo( dimgeo*(numno-1)+icmp )
          end do
         !
          call VecRestoreArray(coords,xx_v, xx_i, ierr)
          ASSERT(ierr==0)
        endif
        !
        ! 
!           * CALCUL DES MODES A PARTIR DES COORDONNEES
        if (bs.le.3) then
            call MatNullSpaceCreateRigidBody(coords, sp, ierr)
            ASSERT(ierr.eq.0)
            call MatSetNearNullSpace(a, sp, ierr)
            ASSERT(ierr.eq.0)
            call MatNullSpaceDestroy(sp, ierr)
            ASSERT(ierr.eq.0)
        endif
        call VecDestroy(coords, ierr)
        ASSERT(ierr.eq.0)
        endif
#endif    

!
!     -- CHOIX DU PRECONDITIONNEUR :
!     ------------------------------
    call KSPGetPC(ksp, pc, ierr)
    ASSERT(ierr.eq.0)
!-----------------------------------------------------------------------
    if (precon .eq. 'LDLT_INC') then
        call PCSetType(pc, PCILU, ierr)
        ASSERT(ierr.eq.0)
        call PCFactorSetLevels(pc, to_petsc_int(fill), ierr)
        ASSERT(ierr.eq.0)
        call PCFactorSetFill(pc, fillp, ierr)
        ASSERT(ierr.eq.0)
        call PCFactorSetMatOrderingType(pc, MATORDERINGNATURAL, ierr)
        ASSERT(ierr.eq.0)
!-----------------------------------------------------------------------
    else if (precon.eq.'LDLT_SP') then
        call PCSetType(pc, PCSHELL, ierr)
        ASSERT(ierr.eq.0)
!        LDLT_SP FAIT APPEL A DEUX ROUTINES EXTERNES
        call PCShellSetSetUp(pc, ldsp1, ierr)
        ASSERT(ierr.eq.0)
        call PCShellSetApply(pc, ldsp2, ierr)
        ASSERT(ierr.eq.0)
!
        ASSERT(spmat.eq.' ')
        spmat = nomat
        ASSERT(spsolv.eq.' ')
        spsolv = nosolv
!-----------------------------------------------------------------------
    else if (precon.eq.'JACOBI') then
        call PCSetType(pc, PCJACOBI, ierr)
        ASSERT(ierr.eq.0)
!-----------------------------------------------------------------------
    else if (precon.eq.'SOR') then
        call PCSetType(pc, PCSOR, ierr)
        ASSERT(ierr.eq.0)
!-----------------------------------------------------------------------
    else if (precon.eq.'ML') then
        call PCSetType(pc, PCML, ierr)
        if (ierr .ne. 0) then
            call utmess('F', 'PETSC_19', sk=precon)
        endif
        call PetscOptionsSetValue('-pc_type', 'ml', ierr)
        ASSERT(ierr.eq.0)
!        CHOIX DE LA RESTRICTION (UNCOUPLED UNIQUEMENT ACTUELLEMENT)
        call PetscOptionsSetValue('-pc_ml_CoarsenScheme', 'Uncoupled', ierr)
        ASSERT(ierr.eq.0)
!
        call PetscOptionsSetValue('-pc_ml_PrintLevel', '0', ierr)
        ASSERT(ierr.eq.0)
!        APPEL OBLIGATOIRE POUR PRENDRE EN COMPTE LES AJOUTS CI-DESSUS
        call PCSetFromOptions(pc, ierr)
        ASSERT(ierr.eq.0)
!-----------------------------------------------------------------------
    else if (precon.eq.'BOOMER') then
        call PCSetType(pc, PCHYPRE, ierr)
        if (ierr .ne. 0) then
            call utmess('F', 'PETSC_19', sk=precon)
        endif
        call PetscOptionsSetValue('-pc_hypre_type', 'boomeramg', ierr)
        ASSERT(ierr.eq.0)
!        CHOIX DE LA RESTRICTION (PMIS UNIQUEMENT ACTUELLEMENT)
        call PetscOptionsSetValue('-pc_hypre_boomeramg_coarsen_type', 'PMIS', ierr)
        ASSERT(ierr.eq.0)
!        CHOIX DU LISSAGE (SOR UNIQUEMENT POUR LE MOMENT)
        call PetscOptionsSetValue('-pc_hypre_boomeramg_relax_type_all', 'SOR/Jacobi', ierr)
        ASSERT(ierr.eq.0)
!
        call PetscOptionsSetValue('-pc_hypre_boomeramg_print_statistics', '0', ierr)
        ASSERT(ierr.eq.0)
!        APPEL OBLIGATOIRE POUR PRENDRE EN COMPTE LES AJOUTS CI-DESSUS
        call PCSetFromOptions(pc, ierr)
        ASSERT(ierr.eq.0)
!-----------------------------------------------------------------------
#ifdef ASTER_PETSC_VERSION_LEQ_32
#else 
     else if (precon.eq.'GAMG') then
        call PCSetType(pc, PCGAMG, ierr)
        if (ierr .ne. 0) then
            call utmess('F', 'PETSC_19', 1, precon)
        endif
!       CHOIX DE LA VARIANTE AGGREGATED
!        call PCGAMGSetType(pc, "agg", ierr)
!        ASSERT(ierr.eq.0)
!       CHOIX DU NOMBRE DE LISSAGES
        nsmooth=1
        call PCGAMGSetNSmooths(pc, nsmooth, ierr)
        ASSERT(ierr.eq.0)
!
        call PetscOptionsSetValue('-pc_gamg_verbose', '2', ierr)
        ASSERT(ierr.eq.0)
!       APPEL OBLIGATOIRE POUR PRENDRE EN COMPTE LES AJOUTS CI-DESSUS
        call PCSetFromOptions(pc, ierr)
        ASSERT(ierr.eq.0)
     
#endif
!-----------------------------------------------------------------------
    else if (precon.eq.'SANS') then
        call PCSetType(pc, PCNONE, ierr)
        ASSERT(ierr.eq.0)
!-----------------------------------------------------------------------
    else
        ASSERT(.false.)
    endif
!-----------------------------------------------------------------------
!
!     CREATION EFFECTIVE DU PRECONDITIONNEUR
    call PCSetUp(pc, ierr)
!     ANALYSE DU CODE RETOUR
    if (ierr .ne. 0) then
        if (precon .eq. 'LDLT_SP') then
!           ERREUR : PCENT_PIVOT PAS SUFFISANT
            call utmess('F', 'PETSC_15')
        else
            call utmess('F', 'PETSC_14')
        endif
    endif
!
999 continue
!
    call jedema()
!
#else
    integer :: idummy
    idummy = kptsc
#endif
!
end subroutine

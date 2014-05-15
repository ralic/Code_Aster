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
! person_in_charge: thomas.de-soza at edf.fr
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
    integer :: jslvk, jslvr, jslvi
    integer :: tbloc, niremp
    integer :: iret
!
    character(len=24) :: precon
    character(len=19) :: nomat, nosolv
    character(len=14) :: nonu
    character(len=4) :: exilag
!
    real(kind=8) :: fillin
!
!----------------------------------------------------------------
!     Variables PETSc
    PetscInt :: ierr
    integer :: fill
    PetscReal :: fillp
    Mat :: a
    KSP :: ksp
    PC :: pc
    mpi_int :: mrank, msize
!----------------------------------------------------------------
    call jemarq()
!
!     -- LECTURE DU COMMUN
    nomat = nomats(kptsc)
    nosolv = nosols(kptsc)
    nonu = nonus(kptsc)
    a = ap(kptsc)
    ksp = kp(kptsc)
!
    call jeveuo(nosolv//'.SLVK', 'L', jslvk)
    call jeveuo(nosolv//'.SLVR', 'L', jslvr)
    call jeveuo(nosolv//'.SLVI', 'L', jslvi)
    precon = zk24(jslvk-1+2)
    fillin = zr(jslvr-1+3)
    niremp = zi(jslvi-1+4)
!
    fill = niremp
    fillp = fillin
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
    else if (precon.eq.'SANS') then
        call PCSetType(pc, PCNONE, ierr)
        ASSERT(ierr.eq.0)
!-----------------------------------------------------------------------
    else
        ASSERT(.false.)
    endif
!-----------------------------------------------------------------------
!
!     VERIFICATION DU DOMAINE D'APPLICATION
    if (precon .eq. 'ML' .or. precon .eq. 'BOOMER') then
        call dismoi('EXIS_LAGR', nomat, 'MATR_ASSE', repk=exilag, arret='C',&
                    ier=iret)
        call apbloc(nomat, nosolv, tbloc)
        if (tbloc .le. 0) then
            call utmess('A', 'PETSC_18')
            tbloc=1
        endif
    endif
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

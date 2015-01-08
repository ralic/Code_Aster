subroutine elg_apelim(kptsc)
    implicit none
! person_in_charge: jacques.pellet at edf.fr
! aslint: disable=W0104
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/asmpi_comm.h"
#include "asterc/getres.h"
#include "asterc/matfpe.h"
#include "asterc/r8prem.h"
#include "asterfort/apalmc.h"
#include "asterfort/apmamc.h"
#include "asterfort/assert.h"
#include "asterfort/compress_sparse_pattern.h"
#include "asterfort/extract_nonzero_col.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nullbasis.h"
#include "asterfort/utmess.h"
!
    integer :: kptsc
!--------------------------------------------------------------
! BUT : calculer (dans PETSC) les matrices nécessaires à
!       ELIM_LAGR='OUI' :
!       Kproj,Ctrans,Tfinal,RCt,MatB
!       (voir la description de ces matrices dans elim_lagr.h)
!
! IN  : KPTSC (I): INDICE DE L'INSTANCE PETSC LE COMMON SPETSC
!       (pour la matrice complète).
!---------------------------------------------------------------
!
#ifdef _HAVE_PETSC
#include "elim_lagr.h"
!
!================================================================
!
!     VARIABLES LOCALES
    integer :: ifm, niv
    integer :: nequa, k 
    integer ::  kptscr, ktrou
    character(len=8)  :: k8b
    character(len=19) :: nomat, nosolv, rigi1
    character(len=14) :: nonu
    character(len=16) :: concep, nomcmd
    aster_logical :: lmd, verbose
    real(kind=8) :: eps
    integer, pointer :: delg(:) => null()
    character(len=24), pointer :: slvk(:) => null()
    mpi_int :: mpicomm
    PetscInt :: restart, nbelig
    PetscInt :: ierr, nbnvco, clag1, nlag 
    PetscInt:: nphys, nlag1, nlag2, neq, i1, j1, i, iphys
    PetscInt, parameter  :: izero=0, ione = 1
    PetscScalar, parameter :: rzero=0.d0, rone = 1.d0
    PetscInt, dimension(:), allocatable :: idphys_c, idlag1_c
    PetscInt, dimension(:), allocatable :: non_verified_cons_c
    PetscInt, dimension(:), pointer :: icolnz_c 
    PetscReal :: aster_petsc_default_real  
    PetscReal, dimension(:), allocatable :: norms_c, norms_ct
    Mat :: mat_c, mat_ct, mat_tmp, mat_t
    IS :: islag1, isphys, isall,isnvco
!----------------------------------------------------------------
    call jemarq()
    call asmpi_comm('GET_WORLD', mpicomm)
    call infniv(ifm, niv)
    verbose= ( niv == 2 )
!
#ifdef ASTER_PETSC_VERSION_LEQ_34
    aster_petsc_default_real = PETSC_DEFAULT_DOUBLE_PRECISION
#else
    aster_petsc_default_real = PETSC_DEFAULT_REAL
#endif 
!
!
!     -- ON DESACTIVE LA LEVEE D'EXCEPTION FPE DANS LES MKL
    call matfpe(-1)
!
!
!     -- LECTURE DU COMMUN
    nomat = nomats(kptsc)
    nosolv = nosols(kptsc)
    nonu = nonus(kptsc)
!
    call jeveuo(nosolv//'.SLVK', 'L', vk24=slvk)
!
!     -- ON NE VEUT PAS DE MATRICE DISTRIBUEE :
    lmd = slvk(10)(1:3).eq.'OUI'
    ASSERT(.not.lmd)
!
!
!     0.1 CREATION ET PREALLOCATION DE LA MATRICE PETSc :
!     ---------------------------------------------------
    call apalmc(kptsc)
!
!
!     0.2 COPIE DE LA MATRICE ASTER VERS LA MATRICE PETSc :
!     -----------------------------------------------------
    call apmamc(kptsc)
!
!
!     0.3 ASSEMBLAGE DE LA MATRICE PETSc :
!     ------------------------------------
    call MatAssemblyBegin(ap(kptsc), MAT_FINAL_ASSEMBLY, ierr)
    ASSERT(ierr.eq.0)
    call MatAssemblyEnd(ap(kptsc), MAT_FINAL_ASSEMBLY, ierr)
    ASSERT(ierr.eq.0)
!
!
!     1. Extraction de la matrice Ctrans de Ap(KPTSC) :
!     --------------------------------------------------------
!
!     -- calcul de KPTSCR 
!
    kptscr=kptsc
!
    ktrou=0
    do k = 1, 5
        if (nomelim(k,1) .eq. nomat) then
            ktrou=k
            goto 1
        endif
    enddo
  1 continue
    ASSERT(ktrou.gt.0)
    rigi1=nomelim(ktrou,3)(1:19)
!
    if (rigi1 .ne. ' ') then
        ktrou=0
        do k = 1, 5
            if (nomats(k) .eq. rigi1) then
                ktrou=k
                goto 2
            endif
        enddo
  2     continue
        ASSERT(ktrou.gt.0)
        kptscr=ktrou
    endif
!
!     --   Repérage des ddls : 
!          physiques, lagrange_1 et lagrange_2 
!
    call dismoi('NB_EQUA', nonu, 'NUME_DDL', repi=nequa)
    call jeveuo(nonu//'.NUME.DELG', 'L', vi=delg)
!
    neq = nequa
    nphys = count( delg(1:neq ) ==  0 )    
    nlag1 = count( delg(1:neq ) == -1 )
    nlag2 = count( delg(1:neq ) == -2 )
    ASSERT( nlag2 == nlag1 )
    nlag=nlag1
    ASSERT( neq == nphys+2*nlag )
!
    if ( nlag == 0 ) call utmess('F', 'ELIMLAGR_9')
!
    allocate( idlag1_c( nlag ),  stat = ierr )
    ASSERT( ierr == 0 )
    idlag1_c(:)= izero 
    allocate( idphys_c( nphys ), stat = ierr )
    ASSERT( ierr == 0 ) 
    idphys_c(:) = izero   
!
!   -- Construction des vecteurs d'indices idphys_c et idlag1_c 
    nlag1=0
    nphys=0
    do i1 = 1, neq
        if (delg(i1) == 0) then
          nphys = nphys + 1   
          idphys_c( nphys ) = i1-1
        elseif (delg(i1) == -1) then
          nlag1 = nlag1 + 1
          idlag1_c( nlag1 ) = i1-1 
        endif
    end do
!  --  Index sets ISphys et ISlag 
    call ISCreateGeneral(mpicomm, nphys, idphys_c, &
        PETSC_USE_POINTER, isphys, ierr)
    ASSERT( ierr == 0 )
    call ISCreateGeneral(mpicomm, nlag, idlag1_c, &
        PETSC_USE_POINTER, islag1, ierr)
    ASSERT( ierr == 0 ) 

!
!   -- On extrait la matrice C transposée 
     call MatGetSubMatrix(ap(kptscr), isphys, islag1, MAT_INITIAL_MATRIX, &
        melim(ke)%ctrans, ierr)
     ASSERT( ierr == 0 ) 
!
!   -- Extraction de MatB depuis Ap(KPTSC) :
!   -----------------------------------------
    call MatGetSubMatrix(ap(kptsc), isphys, isphys, &
        MAT_INITIAL_MATRIX, melim(ke)%matb, ierr)
    ASSERT( ierr == 0 ) 
!
    call ISDestroy( isphys, ierr ) 
    ASSERT( ierr == 0 ) 
    call ISDestroy( islag1, ierr )
    ASSERT( ierr == 0 ) 
!
!  -- Allocation et remplissage de Tfinal, 
!     qui servira pour la projection de la matrice :
!
    call MatCreateSeqAIJ(mpicomm, nphys, nphys, PETSC_NULL_INTEGER,&
        (/ (ione, i=0, nphys-1)/), melim(ke)%tfinal, ierr)
    ASSERT( ierr == 0 )
!
    do i1=0, nphys-1
        call MatSetValues(melim(ke)%tfinal, ione, [i1], &
            ione, [i1], [rone], INSERT_VALUES, ierr ) 
          ASSERT( ierr == 0 )
    enddo
    call MatAssemblyBegin( melim(ke)%tfinal, MAT_FINAL_ASSEMBLY, ierr )
    ASSERT( ierr == 0 )
    call MatAssemblyEnd( melim(ke)%tfinal, MAT_FINAL_ASSEMBLY, ierr )
    ASSERT( ierr == 0 )
!
!----------------------------------------------------!
!--                                                --!
!-- On boucle tant que les colonnes de Tfinal ne   --!
!-- sont pas orthogonales à toutes les contraintes --!
!-- ie à toutes les lignes de C                    --!
!--                                                --!
!----------------------------------------------------!
!
!
!   -- C est initialisée en transposant Ctrans 
!
    call MatTranspose(melim(ke)%ctrans, MAT_INITIAL_MATRIX, mat_c, ierr)
    ASSERT( ierr == 0 )
! Number of non-verified constraints
    nbnvco  = nlag
    allocate( non_verified_cons_c( nlag ), stat = ierr )
    ASSERT( ierr == 0 ) 
    non_verified_cons_c(:)= izero
! Nombre de tours de boucle   
    restart = 0 
do while (( nbnvco > 0 ).and.( restart < nlag ))
!
!
! -- Calcul de T, telle que CT=0 
!    en sortie : nbnvco, non_verified_cons_c caractérisent
!    les contraintes qui n'ont pas pu être prises en compte 
!    à ce tour de boucle 
!
      call nullbasis(mat_c, mat_t, nbnvco, non_verified_cons_c) 
!
! -- si toutes les contraintes ne sont pas vérifiées, 
!    on prépare les données pour le tour de boucle 
!    suivant
      if (nbnvco >  0) then
        restart=restart+1
        if (verbose) write(ifm,'(A14,I3)') 'restart #',restart
        if (verbose) write(ifm,'(A3,I3,A26)') ' * ',nbnvco, ' CONTRAINTES NON VERIFIEES'
!
        if (verbose) write(ifm,*) 'NEW NUMBER    ','OLD NUMBER    '
        do j1 = 1, nbnvco
            if (verbose) write(ifm,*) j1,' - ', non_verified_cons_c(j1) + 1
        end do
!   
! CT <- C*T (stockée dans mat_tmp)
        call MatMatMult(mat_c, mat_t, MAT_INITIAL_MATRIX, &
             aster_petsc_default_real, mat_tmp, ierr)
        ASSERT(ierr.eq.0)
        call MatDestroy(mat_c, ierr)
        ASSERT( ierr == 0 )
! On extrait de CT la sous-matrice des contraintes qui n'ont pas été éliminées
! On nomme C cette nouvelle matrice des contraintes 
        call ISCreateGeneral(mpicomm, nbnvco, non_verified_cons_c, PETSC_USE_POINTER,&
                             isnvco, ierr) 
        ASSERT( ierr == 0 )
        call ISCreateStride(mpicomm, nphys, izero, ione, &
            isall, ierr)
        ASSERT( ierr == 0 )  
        call MatGetSubMatrix(mat_tmp, isnvco, isall, MAT_INITIAL_MATRIX, mat_c,&
                             ierr)
        ASSERT( ierr == 0 )
        call MatDestroy(mat_tmp, ierr)
        ASSERT( ierr == 0 )
        call ISDestroy( isnvco, ierr)
        ASSERT( ierr == 0 )
        call ISDestroy( isall, ierr)
        ASSERT( ierr == 0 )
        
! T2 = TFinal 
        call MatDuplicate(melim(ke)%tfinal, MAT_COPY_VALUES, mat_tmp, ierr)
        ASSERT( ierr == 0 )
        call MatDestroy(melim(ke)%tfinal, ierr)
        ASSERT( ierr == 0 )
! TFinal = T2 * T 
        call MatMatMult(mat_tmp, mat_t, MAT_INITIAL_MATRIX, aster_petsc_default_real,&
                        melim(ke)%tfinal, ierr)
        ASSERT(ierr==0)
        call MatDestroy(mat_t, ierr)
        ASSERT( ierr == 0 )
      endif
    enddo
!
!   -- Derniere projection pour obtenir Tfinal
!
    call MatDuplicate(melim(ke)%tfinal, MAT_COPY_VALUES, mat_tmp, ierr)
    ASSERT( ierr == 0 )
    call MatDestroy(melim(ke)%tfinal, ierr)
    ASSERT( ierr == 0 )
    call MatMatMult(mat_tmp, mat_t, MAT_INITIAL_MATRIX, aster_petsc_default_real,&
         melim(ke)%tfinal, ierr)
    ASSERT( ierr == 0 )
    call MatDestroy(mat_tmp, ierr)
    ASSERT( ierr == 0 ) 
    call MatDestroy(mat_c, ierr)
    ASSERT( ierr == 0 )
    call MatDestroy(mat_t, ierr)
    ASSERT( ierr == 0 )
!
!
!   -- Verif de la qualite de la base
!   ----------------------------------
 
    allocate( norms_c( nlag ) , stat = ierr ) 
    allocate( norms_ct( nlag ) , stat = ierr ) 
!   -- calcul de la norme de chaque ligne de C 
    call MatGetColumnNorms(melim(ke)%ctrans, norm_2, norms_c, ierr)
    ASSERT( ierr == 0 )
!
!   -- calcul de T^T C^T = (CT)^T
!-- Changement de version PETSc 3.2 -> 3.3 
!   Renamed MatMatMultTranspose() for C=A^T*B to MatTransposeMatMult()
#ifdef ASTER_PETSC_VERSION_LEQ_32
    call MatMatMultTranspose(melim(ke)%tfinal, melim(ke)%ctrans, MAT_INITIAL_MATRIX, &
                                     PETSC_DEFAULT_DOUBLE_PRECISION, mat_tmp, ierr)
#else 
    call MatTransposeMatMult(melim(ke)%tfinal,melim(ke)%ctrans,MAT_INITIAL_MATRIX, &
                           aster_petsc_default_real, mat_tmp,ierr)
#endif  
!
    ASSERT( ierr == 0 )
!     -- calcul de la norme de chaque ligne de CT 
    call MatGetColumnNorms(mat_tmp, norm_2, norms_ct, ierr)
    ASSERT( ierr == 0 )
    if (verbose) write(ifm,*) ' '
    if (verbose) write(ifm,*) ' '
    if (verbose) write(ifm,*) '|C.T|/|C| apres elimination :'
    do i1 = 1, nlag
       if ( norms_c(i1) > eps ) then 
        if (verbose) write(ifm, '(A11,I3,A3,E11.4)') 'CONTRAINTE ', i1, ' : ',&
                   norms_ct(i1)/norms_c(i1) 
       endif
    end do
    !
    call MatDestroy( mat_tmp, ierr)
    ASSERT( ierr == 0 )
!
!   -- on "retasse" la matrice Tfinal :
!   -----------------------------------
!
    call MatDuplicate( melim(ke)%tfinal, MAT_COPY_VALUES, mat_tmp, ierr)
    ASSERT( ierr == 0 )
    call compress_sparse_pattern( mat_tmp )
    call extract_nonzero_col( mat_tmp, melim(ke)%tfinal, icolnz_c)
    call MatDestroy( mat_tmp, ierr )
    ASSERT( ierr == 0 )
!    
!   Le tableau melim(ke)%indred est dans le common elim_lagr.h 
!   il n'est pas forcément désalloué à la fin de la commande courante
!   -> allocation avec allocate (et pas AS_ALLOCATE)
    allocate( melim(ke)%indred(size( icolnz_c ) ), stat = ierr ) 
    ASSERT( ierr == 0 ) 
!   indred contient la correspondance entre un indice colonne de 
!   Tfinal et la numérotation des degrés de liberté.
!   (indices Fortran)
    melim(ke)%indred(:) = idphys_c(icolnz_c(:) + 1)+1 
!
!   -- Projection T'*(MatB*T) :
!   -----------------------------
    call MatPtAP(melim(ke)%matb, melim(ke)%tfinal, MAT_INITIAL_MATRIX, 1.d0, &
        melim(ke)%kproj, ierr)
    ASSERT( ierr == 0 )
!
!  -- Vérification du nombre de contraintes éliminées
!  --------------------------------------------------
    if (verbose) then 
        write(ifm,100) size(icolnz_c(:))
100  format(  " Taille du système réduit (projeté sur T=Ker(C)) :", I8 )
    endif 
! 
!    Nombre de contraintes éliminées = nb de lignes de C qui sont bien 
!    orthogonales à toutes les colonnes de TFinal
!    c'est à dire nombre de lignes de CT de norme (quasi)nulle
     nbelig=count(norms_ct/norms_c < 10*r8prem())
!
    if (verbose) then
        write(ifm,*) 'On a ',neq,' ddls (y compris les double-Lagrange).'
        write(ifm,*) 'On a ',nphys ,' ddls "physiques".'
        write(ifm,*) 'On avait ',nlag,' contraintes'
        write(ifm,*) 'On a ete capable d''en eliminer ',nbelig
    endif
    ASSERT(nbelig.le.nlag)
!
    if (nbelig .eq. 0) call utmess('F', 'ELIMLAGR_10', sk=nomat)
!
    if (nlag .ne. nbelig) then
!       Si la condition ci-dessus n'est pas respectée,
!       c'est que A n'est pas de rang maximum. Il faut regarder de plus près
!       et se préoccuper de la cohérence du second membre (c).
!
        call getres(k8b, concep, nomcmd)
!       -- dans la commande ELIM_LAGR, c==0, on peut donc continuer
        if (nomcmd .ne. 'ELIM_LAGR') then
! développement à faire
            ASSERT(.false.)
        endif
    endif
!
!   Libération de la mémoire 
    deallocate( norms_c, stat=ierr)
    ASSERT( ierr == 0 ) 
    deallocate( norms_ct, stat=ierr)
    ASSERT( ierr == 0 ) 
    deallocate( idphys_c, stat=ierr)
    ASSERT( ierr == 0 ) 
    deallocate( idlag1_c, stat=ierr)
    ASSERT( ierr == 0 ) 
    deallocate( non_verified_cons_c, stat = ierr )
    ASSERT( ierr == 0 ) 
    deallocate( icolnz_c, stat = ierr )
    ASSERT( ierr == 0 ) 
!
    call matfpe(1)
    call jedema()
!
#else
    ASSERT(.false.)
#endif
!
end subroutine

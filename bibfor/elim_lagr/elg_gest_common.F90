subroutine elg_gest_common(action, mat1, mat2, rigi1)
    implicit none
! aslint: disable=W0104
! person_in_charge: jacques.pellet at edf.fr
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
!
# include "asterfort/assert.h"
# include "asterfort/utmess.h"
!
    character(len=*) :: action, mat1, mat2, rigi1
!--------------------------------------------------------------
! BUT :
!   * Gestion des variables du COMMON ELIMLG.
!   * Positionner l'indice KE du common ELIMLG correspondant
!     à une matrice Aster
!
! IN  : ACTION :
!        / 'NOTE' : pour "déclarer" une nouvelle matrice
!                   (appelée par exemple dans preres.f)
!        / 'CHERCHE' : pour positionner KE
!                   (appelée par exemple dans resoud.f)
!        / 'EFFACE' : pour effacer une matrice
!                   (appelée par exemple dans detrsd.f)
! IN  : MAT1 :   / nom de la SD_MATR_ASSE complète
!                / ' ' si action='EFFACE'
! IN  : MAT2  : nom de la SD_MATR_ASSE réduite
! IN  : RIGI1 : / nom de la SD_MATR_ASSE complète qui contient
!                 réellement les relations linéaires
!               / ' '
!               Cet argument ne sert que pour action='NOTE'
!---------------------------------------------------------------
!
#ifdef _HAVE_PETSC
#include "elim_lagr.h"
!
!================================================================
    integer :: k, ktrou, iprem
    PetscInt :: ierr
    save iprem
    data iprem / 0 /
!----------------------------------------------------------------
    iprem=iprem+1
!
    ASSERT(action.eq.'NOTE' .or.action.eq.'CHERCHE'.or.action.eq.'EFFACE')
    if (action .ne. 'NOTE') ASSERT(rigi1.eq.' ')
!
!
!     -- au 1er appel on initialise le COMMON :
!     -----------------------------------------
    if (iprem .eq. 1) then
        do k = 1, 5
            nomelim(k,1)=' '
            nomelim(k,2)=' '
            nomelim(k,3)=' '
            melim(k)%kproj=0
            melim(k)%ctrans=0
            melim(k)%tfinal=0
            melim(k)%rct=0
            melim(k)%matb=0
            melim(k)%vx0=0
            melim(k)%vecb=0
            melim(k)%vecc=0
            nullify(melim(k)%indred)
        enddo
        ke=0
    endif
!
!
!
    if (action .eq. 'NOTE') then
        ktrou=0
!       -- on cherche une place libre
        do k = 1, 5
            if (nomelim(k,1) .eq. ' ') then
                ktrou=k
                goto 1
            endif
        enddo
  1     continue
        ASSERT(ktrou.gt.0)
        ke=ktrou
        nomelim(ke,1)=mat1
        nomelim(ke,2)=mat2
        nomelim(ke,3)=rigi1
    endif
!
!
    if (action .eq. 'CHERCHE') then
        ktrou=0
        do k = 1, 5
            if (nomelim(k,1) .eq. mat1) then
                ktrou=k
                goto 2
            endif
        enddo
        ASSERT(ktrou.gt.0)
        ASSERT(nomelim(ktrou, 2).eq.mat2)
        ke=ktrou
  2     continue
    endif
!
!
    if (action .eq. 'EFFACE') then
        ASSERT(mat1.eq.' ')
        ktrou=0
        do k = 1, 5
            if (nomelim(k,2) .eq. mat2) then
                ktrou=k
                goto 3
            endif
        enddo
  3     continue
        if (ktrou .eq. 0) goto 4
!
        nomelim(ktrou,1)=' '
        nomelim(ktrou,2)=' '
        nomelim(ktrou,3)=' '
!
        call MatDestroy(melim(ktrou)%kproj, ierr)
        call MatDestroy(melim(ktrou)%ctrans, ierr)
        call MatDestroy(melim(ktrou)%tfinal, ierr)
        call MatDestroy(melim(ktrou)%rct, ierr)
        call MatDestroy(melim(ktrou)%matb, ierr)
        call VecDestroy(melim(ktrou)%vx0, ierr)
        call VecDestroy(melim(ktrou)%vecb, ierr)
        call VecDestroy(melim(ktrou)%vecc, ierr)
        deallocate(melim(ktrou)%indred)
!
        melim(ktrou)%kproj=0
        melim(ktrou)%ctrans=0
        melim(ktrou)%tfinal=0
        melim(ktrou)%rct=0
        melim(ktrou)%matb=0
        melim(ktrou)%vx0=0
        melim(ktrou)%vecb=0
        melim(ktrou)%vecc=0
        nullify(melim(ktrou)%indred)
  4     continue
    endif
!
#else
    if (action .eq. 'EFFACE') then
! rien a faire
    else
        call utmess('F', 'ELIMLAGR_1')
    endif
#endif
!
end subroutine

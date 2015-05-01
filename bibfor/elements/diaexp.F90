subroutine diaexp(nno, nddl, ldim, masco, masdi)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "asterfort/r8inir.h"
    real(kind=8) :: masco(*), masdi(*)
    integer :: nno, nddl, ldim
!     ------------------------------------------------------------------
!     PASSAGE D'UNE MATRICE MASSE CONSISTANTE A UNE MATRICE MASSE
!     DIAGONALE SUIVANT LA TECHNIQUE DE SOMMATION PAR COLONNE
!     ------------------------------------------------------------------
!     IN  NNO   : NOMBRE DE NOEUDS
!     IN  NDDL  : NOMBRE DE DDL PAR NOEUD
!     IN  LDIM  : NOMBRE TOTAL DE DDL DE L ELEMENT
!     IN  MASCO : MATRICE DE MASSE CONSISTANTE
!     OUT MASDI : MATRICE DE MASSE DIAGONALE
!     ------------------------------------------------------------------
!
    integer :: idiag, i, k, i0, k0
!
    call r8inir(ldim*(ldim+1)/2, 0.d0, masdi, 1)
    do 10 i = 1, ldim
        i0 = i*(i-1)/2
        idiag = i*(i+1)/2
        masdi(idiag) = masco(idiag)
        do 20 k = (i0+1), idiag-1
            masdi(idiag) = masdi(idiag) + masco(k)
            k0 = k - i0
            k0 = k0*(k0+1)/2
            masdi(k0) = masdi(k0) + masco(k)
20      continue
10  end do
end subroutine

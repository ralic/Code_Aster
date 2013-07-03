subroutine chgrep(type, pgl1, pgl2, matl, matg)
    implicit none
#include "asterfort/assert.h"
#include "asterfort/mavec.h"
#include "asterfort/pmat.h"
#include "asterfort/tmat.h"
#include "asterfort/vecma.h"
    character(len=2) :: type
    real(kind=8) :: pgl1(3, 3), pgl2(3, 3), matl(*), matg(*)
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
! IN  : TYPE   : 'LG' > PASSAGE REPERE LOCAL  > GLOBAL
!              : 'GL' > PASSAGE REPERE GLOBAL > LOCAL
! IN  : PGL1   : MATRICE DE PASSAGE DU GLOBAL > LOCAL AU NOEUD 1
! IN  : PGL2   : MATRICE DE PASSAGE DU GLOBAL > LOCAL AU NOEUD 2
! IN  : MATL   : DEMI - MATRICE STOCKE COLONNE = VECTEUR LONGUEUR N
! OUT : MATG   : DEMI - MATRICE STOCKE COLONNE = VECTEUR LONGUEUR N
!     ------------------------------------------------------------------
!               T                                  T        -1     T
!     (MA) = (R) (ML) (R)   OU  (ML) = (R) (MA) (R)  CAR (R)  = (R)
!     ------------------------------------------------------------------
    real(kind=8) :: ml12(12, 12), mr12(12, 12), mtr12(12, 12), mv12(12, 12)
!
!-----------------------------------------------------------------------
    integer :: i, j
!-----------------------------------------------------------------------
    call assert(( type .eq. 'LG' ).or.( type .eq. 'GL' ))
    if (type .eq. 'LG') then
        do 10 i = 1, 3
            do 12 j = 1, 3
                mr12(i ,j ) = pgl1(i,j)
                mr12(i ,j+3) = 0.d0
                mr12(i ,j+6) = 0.d0
                mr12(i ,j+9) = 0.d0
                mr12(i+3,j ) = 0.d0
                mr12(i+3,j+3) = pgl1(i,j)
                mr12(i+3,j+6) = 0.d0
                mr12(i+3,j+9) = 0.d0
                mr12(i+6,j+3) = 0.d0
                mr12(i+6,j ) = 0.d0
                mr12(i+6,j+6) = pgl2(i,j)
                mr12(i+6,j+9) = 0.d0
                mr12(i+9,j ) = 0.d0
                mr12(i+9,j+3) = 0.d0
                mr12(i+9,j+6) = 0.d0
                mr12(i+9,j+9) = pgl2(i,j)
12          continue
10      continue
        call tmat(12, mr12, mtr12)
!
    else if (type .eq. 'GL') then
        do 20 i = 1, 3
            do 22 j = 1, 3
                mtr12(i ,j ) = pgl1(i,j)
                mtr12(i ,j+3) = 0.d0
                mtr12(i ,j+6) = 0.d0
                mtr12(i ,j+9) = 0.d0
                mtr12(i+3,j ) = 0.d0
                mtr12(i+3,j+3) = pgl1(i,j)
                mtr12(i+3,j+6) = 0.d0
                mtr12(i+3,j+9) = 0.d0
                mtr12(i+6,j+3) = 0.d0
                mtr12(i+6,j ) = 0.d0
                mtr12(i+6,j+6) = pgl2(i,j)
                mtr12(i+6,j+9) = 0.d0
                mtr12(i+9,j ) = 0.d0
                mtr12(i+9,j+3) = 0.d0
                mtr12(i+9,j+6) = 0.d0
                mtr12(i+9,j+9) = pgl2(i,j)
22          continue
20      continue
        call tmat(12, mtr12, mr12)
!
    endif
    call vecma(matl, 78, ml12, 12)
    call pmat(12, mtr12, ml12, mv12)
    call pmat(12, mv12, mr12, mtr12)
    call mavec(mtr12, 12, matg, 78)
!
end subroutine

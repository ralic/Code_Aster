subroutine utpsgl(nn, nc, p, sg, sl)
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
    implicit none
#include "asterfort/mavec.h"
#include "asterfort/pmat.h"
#include "asterfort/tmat.h"
#include "asterfort/vecma.h"
    real(kind=8) :: p(3, 3), sg(*), sl(*)
!     ------------------------------------------------------------------
!     PASSAGE EN 3D D'UNE MATRICE TRIANGULAIRE DE NN*NC LIGNES
!     DU REPERE GLOBAL AU REPERE LOCAL
!     ------------------------------------------------------------------
!IN   I   NN   NOMBRE DE NOEUDS
!IN   I   NC   NOMBRE DE COMPOSANTES
!IN   R   P    MATRICE DE PASSAGE 3D DE GLOBAL A LOCAL
!IN   R   SG   NN*NC COMPOSANTES DE LA TRIANGULAIRE SG DANS GLOBAL
!OUT  R   SL   NN*NC COMPOSANTES DE LA TRIANGULAIRE SL DANS LOCALL
!     ------------------------------------------------------------------
    real(kind=8) :: r(9), zero
    real(kind=8) :: ml14(14, 14), mr14(14, 14), mtr14(14, 14), mv14(14, 14)
    real(kind=8) :: ml16(16, 16), mr16(16, 16), mtr16(16, 16), mv16(16, 16)
    integer :: in(3)
!-----------------------------------------------------------------------
    integer :: i, j, k, l, m, n, nb
    integer :: nc, nn
!-----------------------------------------------------------------------
    data     zero / 0.d0 /
!
    if (mod(nc,3) .eq. 0) then
        nb = nn * nc / 3
        do 10 i = 1, nb
            k = 3 * ( i - 1 )
            do 20 j = 1, i
                in(1) = k * (k+1) / 2 + 3*(j-1)
                in(2) = (k+1) * (k+2) / 2 + 3*(j-1)
                in(3) = (k+2) * (k+3) / 2 + 3*(j-1)
                if (i .eq. j) then
!             --------- BLOC DIAGONAL
                    r(1) = sg(in(1)+1)
                    r(2) = sg(in(2)+1)
                    r(3) = sg(in(3)+1)
                    r(4) = sg(in(2)+1)
                    r(5) = sg(in(2)+2)
                    r(6) = sg(in(3)+2)
                    r(7) = sg(in(3)+1)
                    r(8) = sg(in(3)+2)
                    r(9) = sg(in(3)+3)
                    do 30 m = 1, 3
                        do 40 n = 1, m
                            sl(in(m)+n) = zero
                            do 50 l = 1, 3
                                sl(in(m)+n) = sl(&
                                              in(m)+n)+ p(m,&
                                              l)*(&
                                              r(&
                                              3*(l-1)+1)* p(n, 1) + r(3*(l-1)+2)*p(n,&
                                              2) + r(3*(l-1)+3&
                                              )*p(n, 3&
                                              )&
                                              )
50                          continue
40                      continue
30                  continue
                else
!             --------- BLOC EXTRA - DIAGONAL
                    do 60 m = 1, 3
                        do 70 n = 1, 3
                            sl(in(m)+n) = zero
                            do 80 l = 1, 3
                                sl(in(m)+n) = sl(&
                                              in(m)+n)+ p(m,&
                                              l)*(&
                                              sg(&
                                              in(l)+1)* p(n, 1) + sg(in(l)+2)*p(n,&
                                              2) + sg(in(l)+3&
                                              )*p(n, 3&
                                              )&
                                              )
80                          continue
70                      continue
60                  continue
                endif
20          continue
10      continue
!
    else if (mod(nc,3) .eq. 1) then
        do 202 i = 1, 14
            do 204 j = 1, 14
                mtr14(i,j) = 0.d0
204          continue
202      continue
        do 200 i = 1, 3
            do 210 j = 1, 3
                mtr14(i ,j ) = p(i,j)
                mtr14(i+3 ,j+3 ) = p(i,j)
                mtr14(i+7 ,j+7 ) = p(i,j)
                mtr14(i+10,j+10) = p(i,j)
210          continue
200      continue
        mtr14( 7, 7) = 1.d0
        mtr14( 14, 14) = 1.d0
        call tmat(14, mtr14, mr14)
        call vecma(sl, 105, ml14, 14)
        call pmat(14, mtr14, ml14, mv14)
        call pmat(14, mv14, mr14, mtr14)
        call mavec(mtr14, 14, sg, 105)
!
    else if (mod(nc,3) .eq. 2) then
        do 302 i = 1, 16
            do 304 j = 1, 16
                mtr16(i,j) = 0.d0
304          continue
302      continue
        do 300 i = 1, 3
            do 310 j = 1, 3
                mtr16(i ,j ) = p(i,j)
                mtr16(i+3 ,j+3 ) = p(i,j)
                mtr16(i+8 ,j+8 ) = p(i,j)
                mtr16(i+11,j+11) = p(i,j)
310          continue
300      continue
        mtr16( 7, 7) = 1.d0
        mtr16( 8, 8) = 1.d0
        mtr16( 15, 15) = 1.d0
        mtr16( 16, 16) = 1.d0
        call tmat(16, mtr16, mr16)
        call vecma(sl, 136, ml16, 16)
        call pmat(16, mtr16, ml16, mv16)
        call pmat(16, mv16, mr16, mtr16)
        call mavec(mtr16, 16, sg, 136)
    endif
!
end subroutine

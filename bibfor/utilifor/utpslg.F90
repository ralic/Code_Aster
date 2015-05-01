subroutine utpslg(nn, nc, p, sl, sg)
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
#include "asterfort/mavec.h"
#include "asterfort/pmat.h"
#include "asterfort/tmat.h"
#include "asterfort/vecma.h"
    real(kind=8) :: p(3, 3), sl(*), sg(*)
!     ------------------------------------------------------------------
!     PASSAGE EN 3D D'UNE MATRICE TRIANGULAIRE DE NN*NC LIGNES
!     DU REPERE LOCAL AU REPERE GLOBAL
!     ------------------------------------------------------------------
!IN   I   NN   NOMBRE DE NOEUDS
!IN   I   NC   NOMBRE DE COMPOSANTES
!IN   R   P    MATRICE DE PASSAGE 3D DE GLOBAL A LOCAL
!IN   R   SL   NN*NC COMPOSANTES DE LA TRIANGULAIRE SL DANS LOCAL
!OUT  R   SG   NN*NC COMPOSANTES DE LA TRIANGULAIRE SG DANS GLOBAL
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
        do 100 i = 1, nb
            k = 3 * ( i - 1 )
            do 110 j = 1, i
                in(1) = k * (k+1) / 2 + 3*(j-1)
                in(2) = (k+1) * (k+2) / 2 + 3*(j-1)
                in(3) = (k+2) * (k+3) / 2 + 3*(j-1)
                if (i .eq. j) then
!            --------- BLOC DIAGONAL
                    r(1) = sl(in(1)+1)
                    r(2) = sl(in(2)+1)
                    r(3) = sl(in(3)+1)
                    r(4) = sl(in(2)+1)
                    r(5) = sl(in(2)+2)
                    r(6) = sl(in(3)+2)
                    r(7) = sl(in(3)+1)
                    r(8) = sl(in(3)+2)
                    r(9) = sl(in(3)+3)
                    do 120 m = 1, 3
                        do 130 n = 1, m
                            sg(in(m)+n) = zero
                            do 140 l = 1, 3
                                sg(in(m)+n) = sg(&
                                              in(m)+n) + p(l,&
                                              m) * (&
                                              r(&
                                              3*(l-1)+1)*p(1, n) + r(3*(l-1)+2)*p( 2,&
                                              n) + r(3*(l-1)+3&
                                              )*p(3, n&
                                              )&
                                              )
140                          continue
130                      continue
120                  continue
                else
!              --------- BLOC EXTRA - DIAGONAL
                    do 150 m = 1, 3
                        do 160 n = 1, 3
                            sg(in(m)+n) = zero
                            do 170 l = 1, 3
                                sg(in(m)+n) = sg(&
                                              in(m)+n) + p(l,&
                                              m) * (&
                                              sl(&
                                              in(l)+1)*p(1, n) + sl(in(l)+2)*p(2, n) + sl(in(l)+3&
                                              )*p(3, n&
                                              )&
                                              )
170                          continue
160                      continue
150                  continue
                endif
110          continue
100      continue
!
    else if (mod(nc,3) .eq. 1) then
        do 202 i = 1, 14
            do 204 j = 1, 14
                mr14(i,j) = 0.d0
204          continue
202      continue
        do 200 i = 1, 3
            do 210 j = 1, 3
                mr14(i ,j ) = p(i,j)
                mr14(i+3 ,j+3 ) = p(i,j)
                mr14(i+7 ,j+7 ) = p(i,j)
                mr14(i+10,j+10) = p(i,j)
210          continue
200      continue
        mr14( 7, 7) = 1.d0
        mr14( 14, 14) = 1.d0
        call tmat(14, mr14, mtr14)
        call vecma(sl, 105, ml14, 14)
        call pmat(14, mtr14, ml14, mv14)
        call pmat(14, mv14, mr14, mtr14)
        call mavec(mtr14, 14, sg, 105)
!
    else if (mod(nc,3) .eq. 2) then
        do 302 i = 1, 16
            do 304 j = 1, 16
                mr16(i,j) = 0.d0
304          continue
302      continue
        do 300 i = 1, 3
            do 310 j = 1, 3
                mr16(i ,j ) = p(i,j)
                mr16(i+3 ,j+3 ) = p(i,j)
                mr16(i+8 ,j+8 ) = p(i,j)
                mr16(i+11,j+11) = p(i,j)
310          continue
300      continue
        mr16( 7, 7) = 1.d0
        mr16( 8, 8) = 1.d0
        mr16( 15, 15) = 1.d0
        mr16( 16, 16) = 1.d0
        call tmat(16, mr16, mtr16)
        call vecma(sl, 136, ml16, 16)
        call pmat(16, mtr16, ml16, mv16)
        call pmat(16, mv16, mr16, mtr16)
        call mavec(mtr16, 16, sg, 136)
    endif
!
end subroutine

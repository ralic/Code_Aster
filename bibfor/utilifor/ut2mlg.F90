subroutine ut2mlg(nn, nc, p, sl, sg)
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
!     PASSAGE EN 2D D'UNE MATRICE TRIANGULAIRE DE NN*NC LIGNES
!     DU REPERE LOCAL AU REPERE GLOBAL
!     ------------------------------------------------------------------
!IN   I   NN   NOMBRE DE NOEUDS
!IN   I   NC   NOMBRE DE COMPOSANTES
!IN   R   P    MATRICE DE PASSAGE 3D DE GLOBAL A LOCAL
!IN   R   SL   NN*NC COMPOSANTES DE LA TRIANGULAIRE SL DANS LOCAL
!OUT  R   SG   NN*NC COMPOSANTES DE LA TRIANGULAIRE SG DANS GLOBAL
!     ------------------------------------------------------------------
    real(kind=8) :: r(4)
    real(kind=8) :: ml6(6, 6), mr6(6, 6), mtr6(6, 6), mv6(6, 6)
    real(kind=8) :: ml3(3, 3), mr3(3, 3), mtr3(3, 3), mv3(3, 3)
    integer :: in(2)
!
!-----------------------------------------------------------------------
    integer :: i, j, k, l, m, n, nb
    integer :: nc, nn
    real(kind=8) :: un, zero
!-----------------------------------------------------------------------
    zero = 0.0d0
    un = 1.0d0
!
    if (mod(nc,2) .eq. 0) then
        nb = nn * nc / 2
        do 10 i = 1, nb
            k = 2 * ( i - 1 )
            do 20 j = 1, i
                in(1) = k * (k+1) / 2 + 2*(j-1)
                in(2) = (k+1) * (k+2) / 2 + 2*(j-1)
                if (i .eq. j) then
!            --------- BLOC DIAGONAL
                    r(1) = sl(in(1)+1)
                    r(2) = sl(in(2)+1)
                    r(3) = sl(in(2)+1)
                    r(4) = sl(in(2)+2)
!
                    do 30 m = 1, 2
                        do 40 n = 1, m
                            sg(in(m)+n) = zero
                            do 50 l = 1, 2
                                sg(in(m)+n) = sg(&
                                              in(m)+n) + p(l,&
                                              m) * (r( 2*(l-1)+1)*p(1, n) + r(2*(l-1)+2 )*p( 2, n&
                                              )&
                                              )
50                          continue
40                      continue
30                  continue
                else
!              --------- BLOC EXTRA - DIAGONAL
                    do 60 m = 1, 2
                        do 70 n = 1, 2
                            sg(in(m)+n) = zero
                            do 80 l = 1, 2
                                sg(in(m)+n) = sg(&
                                              in(m)+n) + p(l,&
                                              m) * ( sl(in(l)+1)*p(1, n) + sl(in(l)+2 )*p(2, n )&
                                              )
80                          continue
70                      continue
60                  continue
                endif
20          continue
10      continue
!
    else if (mod(nc,2) .eq. 1) then
        if (nc*nn .eq. 3) then
            do 90 i = 1, 3
                do 90 j = 1, 3
                    mr3(i,j) = zero
90              continue
!
            mr3(1,1) = p(1,1)
            mr3(1,2) = p(1,2)
            mr3(2,1) = p(2,1)
            mr3(2,2) = p(2,2)
            mr3(3,3) = un
!
            call tmat(3, mr3, mtr3)
            call vecma(sl, 6, ml3, 3)
            call pmat(3, mtr3, ml3, mv3)
            call pmat(3, mv3, mr3, mtr3)
            call mavec(mtr3, 3, sg, 6)
!
        else if (nc*nn.eq.6) then
            do 100 i = 1, 6
                do 100 j = 1, 6
                    mr6(i,j) = zero
100              continue
!
            mr6(1,1) = p(1,1)
            mr6(1,2) = p(1,2)
            mr6(2,1) = p(2,1)
            mr6(2,2) = p(2,2)
            mr6(3,3) = un
!
            mr6(4,4) = p(1,1)
            mr6(4,5) = p(1,2)
            mr6(5,4) = p(2,1)
            mr6(5,5) = p(2,2)
            mr6(6,6) = un
!
            call tmat(6, mr6, mtr6)
            call vecma(sl, 21, ml6, 6)
            call pmat(6, mtr6, ml6, mv6)
            call pmat(6, mv6, mr6, mtr6)
            call mavec(mtr6, 6, sg, 21)
!
        endif
!
    endif
!
end subroutine

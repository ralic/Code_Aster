subroutine ut2alg(nn, nc, p, sl, sg)
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
!
    implicit none
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
    integer :: in(2)
!
!-----------------------------------------------------------------------
    integer :: i, j, k, l, m, n, nb
    integer :: nc, nn
    real(kind=8) :: zero
!-----------------------------------------------------------------------
    zero = 0.0d0
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
                    r(2) = - sl(in(2)+1)
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
    endif
!
end subroutine

subroutine zzcala(npg, nno, wi, x, y,&
                  xmin, xmax, ymin, ymax, a)
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
!
!    ESTIMATEUR ZZ (2-EME VERSION 92)
!
!                                  T
!    CETTE ROUTINE CALCULE   SIGMA( P(X   ,Y   ) * P(X   ,Y   ) )
!                             IPG      IPG  IPG       IPG  IPG
!
!    AVEC P(X,Y) = (1,X,Y,XY,X2,Y2,X2Y,XY2,X2Y2)
!
!     X(IPG),Y(IPG) SONT LES COORDONNEES DES PTS DE GAUSS SUR
!     L'ELEMENT COURANT
!
    real(kind=8) :: wi(1), x(1), y(1), a(9, 9), xx, yy, b(9)
    integer :: i, ino, ipg, j, nno, npg
    real(kind=8) :: xmax, xmin, ymax, ymin
!-----------------------------------------------------------------------
    do 1 ipg = 1, npg
        xx = 0.d0
        yy = 0.d0
        do 2 ino = 1, nno
            xx = xx + wi(nno*(ipg-1)+ino)*x(ino)
            yy = yy + wi(nno*(ipg-1)+ino)*y(ino)
 2      continue
        xx = -1.d0+2.d0*(xx-xmin)/(xmax-xmin)
        yy = -1.d0+2.d0*(yy-ymin)/(ymax-ymin)
        b(1) = 1.d0
        b(2) = xx
        b(3) = yy
        b(4) = xx*yy
        b(5) = xx*xx
        b(6) = yy*yy
        b(7) = yy*b(5)
        b(8) = xx*b(6)
        b(9) = b(6)*b(5)
        do 3 i = 1, nno
            do 3 j = 1, i
                a(i,j) = a(i,j) + b(i) * b(j)
 3          continue
        do 4 i = 1, nno
            do 4 j = i+1, nno
                a(i,j) = a(j,i)
 4          continue
 1  end do
!
end subroutine

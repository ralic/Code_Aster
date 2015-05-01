subroutine vpzrbk(z, h, d, mm, izh,&
                  k, l)
    implicit none
    integer :: mm, izh, k, l
    real(kind=8) :: z(izh, 1), h(izh, 1), d(1)
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     TRANSFORMATION ARRIERE POUR OBTENIR LES VECTEURS (ROUTINE ORTBAK)
!     ------------------------------------------------------------------
!     REFERENCE: F.L. BAUER - J.H. WILKINSON - C. REINSCH
!        HANDBOOK FOR AUTOMATIC COMPUTATION - LINEAR ALGEBRA - VOL.2
!        PAGE 350
!     ------------------------------------------------------------------
    integer :: m, ma, i, j
    real(kind=8) :: g, zero
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    zero = 0.d0
    do 30 m = l-2, k, -1
        ma=m+1
        if (h(ma,m) .ne. zero) then
            do 5 i = m+2, l
                d(i)=h(i,m)
 5          continue
            if (ma .le. l) then
                do 25 j = 1, mm
                    g=zero
                    do 15 i = ma, l
                        g=g+d(i)*z(i,j)
15                  continue
!
                    g = (g/d(ma))/h(ma,m)
                    do 20 i = ma, l
                        z(i,j)=z(i,j)+g*d(i)
20                  continue
25              continue
            endif
        endif
30  end do
end subroutine

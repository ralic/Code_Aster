subroutine gcldm1(m, in, ip, prec, x,&
                  y)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! TOLE CRP_4
    implicit none
    real(kind=8) :: prec(*), x(*), y(*)
    integer(kind=4) :: ip(*)
    integer :: in(*)
    real(kind=8) :: fac
!-----------------------------------------------------------------------
!  FONCTION  :  INVERSION D'UNE MATRICE DE PRECONDITIONNEMENT LDLT_INC
!                        -1                   T
!               Y = (MAT)  *X   OU MAT = L*D*L
!          LA MATRICE MAT EST STOCKEE SOUS FORME MORSE
!                                                      -1
!          ET A LA PLACE DE 1. DANS MAT(I,I) ON A (D(I))
!               RESOLUTION DU SYSTEME :
!                                    T -1
!                          Y = (L D L ) * X
!-----------------------------------------------------------------------
!     RESOLUTION DU PREMIER SYSTEME L.W = X
!-------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, kdeb, kfin, ki, m
    real(kind=8) :: som
!-----------------------------------------------------------------------
    y(1) = x(1)
!
    do 20 i = 2, m
        som = 0.d0
        kdeb = in(i-1)+1
        kfin = in(i)-1
        do 10 ki = kdeb, kfin
            som = som + prec(ki)*y(ip(ki))
10      continue
        y(i) = (x(i)-som)
20  end do
!-------------------------------------------
!     RESOLUTION DE D.Y = W
!-------------------------------------------
    do 50 i = 1, m
        y(i) = y(i)*prec(in(i))
50  end do
!-------------------------------------------
!     RESOLUTION DU SECOND SYSTEME LT.Y = W
!-------------------------------------------
    do 40 i = m, 2, -1
        kdeb = in(i-1)+1
        kfin = in(i)-1
        fac = y(i)
!
!        ---- PROCEDURE A LA MAIN
!DIR$ IVDEP
!DIR$ NOPREFETCH Y
        do 30 ki = kdeb, kfin
            y(ip(ki)) = y(ip(ki))-prec(ki)*fac
30      continue
40  end do
end subroutine

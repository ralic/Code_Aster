subroutine gcldm1(m, in, ip, prec, x, y, perm, xtrav, ytrav)
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
! aslint: disable=W1304
    implicit none

    integer,intent(in) :: m
    real(kind=8),intent(in)  :: prec(*), x(*)
    real(kind=8),intent(out)  :: y(*)
    integer(kind=4),intent(in) :: ip(*)
    integer,intent(in)  :: in(*)
    integer, intent(in) :: perm(*)
    real(kind=8), intent(inout) :: xtrav(*)
    real(kind=8), intent(inout) :: ytrav(*)
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
    integer :: i, kdeb, kfin, ki
    real(kind=8) :: som, fac
!-----------------------------------------------------------------------

!   -- on permute x pour qu'il ait la numerotation du preconditionneur :
!   --------------------------------------------------------------------
    do i = 1, m
        xtrav(perm(i))=x(i)
    enddo

!-----------------------------------------------------------------------
!     RESOLUTION DU PREMIER SYSTEME L.W = X
!-------------------------------------------
    ytrav(1) = xtrav(1)
!
    do 20 i = 2, m
        som = 0.d0
        kdeb = in(i-1)+1
        kfin = in(i)-1
        do 10 ki = kdeb, kfin
            som = som + prec(ki)*ytrav(ip(ki))
10      continue
        ytrav(i) = (xtrav(i)-som)
20  end do

!-------------------------------------------
!     RESOLUTION DE D.Y = W
!-------------------------------------------
    do 50 i = 1, m
        ytrav(i) = ytrav(i)*prec(in(i))
50  end do

!-------------------------------------------
!     RESOLUTION DU SECOND SYSTEME LT.Y = W
!-------------------------------------------
    do 40 i = m, 2, -1
        kdeb = in(i-1)+1
        kfin = in(i)-1
        fac = ytrav(i)
!
!        ---- PROCEDURE A LA MAIN
!!DIR$_IVDEP
!!DIR$_NOPREFETCH_Y
        do 30 ki = kdeb, kfin
            ytrav(ip(ki)) = ytrav(ip(ki))-prec(ki)*fac
30      continue
40  end do

!   -- on permute ytrav pour qu'il ait la numerotation du syteme :
!   --------------------------------------------------------------
    do i = 1, m
        y(i)=ytrav(perm(i))
    enddo


end subroutine

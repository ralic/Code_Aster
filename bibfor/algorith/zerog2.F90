subroutine zerog2(x, y, z, i)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    include 'asterfort/u2mess.h'
    include 'asterfort/zerop2.h'
    real(kind=8) :: x(3), y(3), z(3)
! ----------------------------------------------------------------------
!  RESOLUTION D'EQUATIONS SCALAIRES PAR APPROXIMATION P2
! ----------------------------------------------------------------------
! VAR X(1) SOLUTION GAUCHE COURANTE TQ Y(1)<0
! VAR X(2) SOLUTION DROITE COURANTE TQ Y(2)>0
! VAR X(3) SOLUTION X(N-1) PUIS SOLUTION EN X(N)
! VAR X(4) SOLUTION X(N)   PUIS SOLUTION EN X(N+1)
! VAR Y(I) VALEUR DE LA FONCTION EN X(I)
! VAR Z(I) VALEUR DE LA DERIVEE DE LA FONCTION EN X(I)
! ----------------------------------------------------------------------
!
    integer :: nrac, i
    real(kind=8) :: rac(2), a, b, c, x0, y0, z0, x1, y1
!
!    TEST DES PRE-CONDITIONS
    if (y(1) .gt. 0 .or. y(2) .lt. 0) call u2mess('F', 'ELEMENTS4_61')
!
    if (y(3) .lt. 0.d0) then
        x(1)=x(3)
        y(1)=y(3)
        z(1)=z(3)
    else
        x(2)=x(3)
        y(2)=y(3)
        z(2)=z(3)
    endif
!
!    CONSTRUCTION D'UN NOUVEL ESTIME
    if (x(1) .eq. x(2)) call u2mess('F', 'ALGORITH9_84')
    if (mod(i,2) .eq. 0) then
        x0=x(1)
        x1=x(2)
        y0=y(1)
        y1=y(2)
        z0=z(1)
    else
        x0=x(2)
        x1=x(1)
        y0=y(2)
        y1=y(1)
        z0=z(2)
    endif
    a=(y1-y0-z0*(x1-x0))/(x1-x0)**2
    b=z0-2*a*x0
    c=y0+a*x0**2-z0*x0
!
    if (a .ne. 0.d0) then
        call zerop2(b/a, c/a, rac, nrac)
    else
        if (b .ne. 0.d0) then
            x(3)=-c/b
            goto 9999
        else
            call u2mess('F', 'ALGORITH9_84')
        endif
    endif
!
    if (((x(1)-rac(1))*(x(2)-rac(1))) .lt. 0.d0) then
        x(3)=rac(1)
    else
        x(3)=rac(2)
    endif
!
9999  continue
end subroutine

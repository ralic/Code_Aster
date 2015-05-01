subroutine dracsy(a0, b0, c0, d0, e0,&
                  f0, nbroot, x, y)
!
    implicit none
!
!-----------------------------------------------------------------------
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
!======================================================================
!
!     EVALUE LES RACINES D UN SYSTEME DE DEUX EQUATIONS POLYNOMIALES
!     DE DEGREN 2 A 2 VARIABLES (SYSTEME DE 2 FORMES QUADRATIQUES)
!
!     A(1) + B(1)*x + C(1)*y + D(1)*x*y + E(1)*x*x + F(1)*y*y = 0
!     A(2) + B(2)*x + C(2)*y + D(2)*x*y + E(2)*x*x + F(2)*y*y = 0
!
! IN  A0 : PARAMETRES DU SYSTEME
! IN  B0 : PARAMETRES DU SYSTEME
! IN  C0 : PARAMETRES DU SYSTEME
! IN  D0 : PARAMETRES DU SYSTEME
! IN  E0 : PARAMETRES DU SYSTEME
! IN  F0 : PARAMETRES DU SYSTEME
!
! OUT NBROOT : NOMBRE DE COUPLES SOLUTIONS
! OUT X ET Y : COUPLES SOLUTIONS
!
#include "asterfort/draac2.h"
#include "asterfort/draacn.h"
#include "asterfort/r8inir.h"
    integer :: deg, nbxx, nbyy, i, j, nbroox, nbroot
!
    real(kind=8) :: a0(2), b0(2), c0(2), d0(2), e0(2), f0(2)
    real(kind=8) :: x(8), y(8), a(2), b(2), c(2), d(2), e(2), f(2)
    real(kind=8) :: ff(2), aux, poly(5), xx(4), yy(2), aa, bb, cc, coef(2)
!
    nbroot=0
!
!     NORMALISATION SU SYSTEME
    call r8inir(8, 0.0d0, x, 1)
    call r8inir(8, 0.0d0, y, 1)
!
    coef(1)=max(abs(a0(1)),abs(b0(1)),abs(c0(1)),&
     &          abs(d0(1)),abs(e0(1)),abs(f0(1)))
    coef(2)=max(abs(a0(2)),abs(b0(2)),abs(c0(2)),&
     &          abs(d0(2)),abs(e0(2)),abs(f0(2)))
!
    if ((coef(1) .ne. 0.d0) .and. (coef(2) .ne. 0.d0) .and. (coef(1) .ge. 1.d-8*coef(2))&
        .and. (coef(2) .ge. 1.d-8*coef(1))) then
!
        do 10, i=1,2
        a(i)=a0(i)/coef(i)
        b(i)=b0(i)/coef(i)
        c(i)=c0(i)/coef(i)
        d(i)=d0(i)/coef(i)
        e(i)=e0(i)/coef(i)
        f(i)=f0(i)/coef(i)
10      continue
!
!     DETERMINATION DU POLYNOME RESULTANT
        poly(5)=-2*f(2)*a(1)*f(1)*a(2)-c(2)*f(2)*c(1)*a(1) +f(1)*c(2)&
        **2*a(1)+f(1)**2*a(2)**2+a(2)*f(2)*c(1)**2 +f(2)**2*a(1)**2-c(&
        2)*c(1)*f(1)*a(2)
!
        poly(4)=2*f(2)**2*a(1)*b(1)+b(2)*f(2)*c(1)**2 -2*f(2)*a(1)*f(&
        1)*b(2) -2*f(2)*b(1)*f(1)*a(2)+2*f(1)**2*b(2)*a(2)-c(2)*f(2)*&
        c(1)*b(1) -c(2)*c(1)*f(1)*b(2)-c(2)*f(2)*d(1)*a(1)-c(2)*d(1)*&
        f(1)*a(2) +f(1)*c(2)**2*b(1)+2*c(2)*f(1)*d(2)*a(1)-d(2)*f(2)*&
        c(1)*a(1) -d(2)*c(1)*f(1)*a(2)+2*a(2)*f(2)*c(1)*d(1)
!
        poly(3)=2*f(2)**2*a(1)*e(1)+f(1)**2*b(2)**2+e(2)*f(2)*c(1)**2&
        +a(2)*f(2)*d(1)**2+f(2)**2*b(1)**2-2*f(2)*a(1)*f(1)*e(2)&
        -2*f(2)*b(1)*f(1)*b(2)-2*f(2)*e(1)*f(1)*a(2)+2*f(1)**2*a(2)*e(&
        2) -c(2)*f(2)*c(1)*e(1)-c(2)*c(1)*f(1)*e(2)-c(2)*f(2)*d(1)*b(&
        1) -c(2)*d(1)*f(1)*b(2)+f(1)*c(2)**2*e(1)+2*f(1)*c(2)*d(2)*b(&
        1) -d(2)*f(2)*c(1)*b(1)-d(2)*c(1)*f(1)*b(2)-d(2)*f(2)*d(1)*a(&
        1) -d(2)*d(1)*f(1)*a(2)+d(2)**2*f(1)*a(1)+2*b(2)*f(2)*c(1)*d(&
        1)
!
        poly(2)=2*f(2)**2*b(1)*e(1)+b(2)*f(2)*d(1)**2 -2*f(2)*b(1)*f(&
        1)*e(2) -2*f(2)*f(1)*b(2)*e(1)+2*f(1)**2*b(2)*e(2)-c(2)*f(2)*&
        d(1)*e(1) -c(2)*d(1)*f(1)*e(2)+2*f(1)*c(2)*d(2)*e(1)-d(2)*f(2)&
        *c(1)*e(1) -d(2)*c(1)*f(1)*e(2)-d(2)*f(2)*d(1)*b(1)-d(2)*d(1)*&
        f(1)*b(2) +d(2)**2*f(1)*b(1)+2*e(2)*f(2)*c(1)*d(1)
!
        poly(1)=f(1)**2*e(2)**2+e(2)*f(2)*d(1)**2+f(2)**2*e(1)**2&
        -2*f(2)*e(1)*f(1)*e(2)-d(2)*f(2)*d(1)*e(1)-d(2)*d(1)*f(1)*e(2)&
        +d(2)**2*f(1)*e(1)
!
        deg=4
!
!     RECHERCHE DES RACINES DU SYSTEME
        call draacn(deg, poly, nbxx, xx)
!
        nbroot=0
!
        call r8inir(8, 0.0d0, x, 1)
        call r8inir(8, 0.0d0, y, 1)
!
        if (nbxx .gt. 0) then
            do 40, i=1,nbxx
            nbroox=0
            aa = f(1)
            bb = c(1) + d(1)*xx(i)
            cc = a(1) + b(1)*xx(i) + e(1)*xx(i)**2
!
!     VALUE LES RACINES DU POLYNOME DU SECOND DEGRE Y=AA X**2+BB X+CC
            call draac2(aa, bb, cc, yy(1), yy(2),&
                        nbyy)
!
            if (nbyy .gt. 0) then
                do 20, j=1,nbyy
                ff(j)= a(2)+b(2)*xx(i)+c(2)*yy(j)+d(2)*xx(i)*&
                        yy(j) +e(2)*xx(i)**2 + f(2)*yy(j)**2
!
                if (abs(ff(j)) .lt. 1.d-7) then
                    nbroot=nbroot+1
                    x(nbroot)=xx(i)
                    y(nbroot)=yy(j)
                    nbroox=nbroox+1
                endif
20              continue
!
                if (nbroox .eq. 0) then
                    aa = f(2)
                    bb = c(2) + d(2)*xx(i)
                    cc = a(2) + b(2)*xx(i) + e(2)*xx(i)**2
!     VALUE LES RACINES DU POLYNOME DU SECOND DEGRE Y=AA X**2+BB X+CC
                    call draac2(aa, bb, cc, yy(1), yy(2),&
                                nbyy)
!
                    if (nbyy .gt. 0) then
                        do 30, j=1,nbyy
                        ff(j)= a(1)+b(1)*xx(i)+c(1)*yy(j)+d(1)&
                                *xx(i)*yy(j) +e(1)*xx(i)**2 + f(1)*yy(&
                                j)**2
!
                        if (abs(ff(j)) .lt. 1.d-7) then
                            nbroot=nbroot+1
                            x(nbroot)=xx(i)
                            y(nbroot)=yy(j)
                            nbroox=nbroox+1
                        endif
30                      continue
!
                        if ((nbroox .eq. 0) .and. (nbyy .eq. 2)) then
                            aux=abs(ff(1)/ff(2))
!
                            if ((aux .lt. 1.d-5) .and. (ff(1) .lt. 1.d-4)) then
                                nbroot=nbroot+1
                                x(nbroot)=xx(i)
                                y(nbroot)=yy(1)
                                elseif ((aux .gt. 1.d5) .and.(ff(2)&
                                .lt. 1.d-4))then
                                nbroot=nbroot+1
                                x(nbroot)=xx(i)
                                y(nbroot)=yy(2)
                                elseif ((abs(ff(1)) .lt. 1.d-4)&
                                .and.(abs(ff(2)) .lt. 1.d-4))then
                                nbroot=nbroot+1
                                x(nbroot)=xx(i)
                                y(nbroot)=yy(1)
                                nbroot=nbroot+1
                                x(nbroot)=xx(i)
                                y(nbroot)=yy(2)
                            endif
                        endif
                    endif
                endif
            endif
40          continue
        endif
    endif
!
end subroutine

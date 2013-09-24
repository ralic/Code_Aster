subroutine mnlbil(x, omega, alpha, eta, h,&
                  hf, nt, sort)
! aslint: disable=W1306

    implicit none
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! --- DECLARATION DES ARGUMENTS DE LA ROUTINE
! ----------------------------------------------------------------------
#include "asterfort/mnlfft.h"
#include "asterc/r8depi.h"
    integer :: h, hf, nt
    real(kind=8) :: x(2*h+1), omega, alpha, eta, sort(2*hf+1)
! ----------------------------------------------------------------------
! --- DECLARATION DES VARIABLES LOCALES
! ----------------------------------------------------------------------
    real(kind=8) :: depi, t(nt), xt(nt), f(nt)
    complex(kind=8) :: a1, b1, c1, d1, h1, p, q, discr1, sd, u3, v3, u, v, f1, a2, b2, c2
    complex(kind=8) :: discr2, fk
    integer :: k, j
!
    depi=r8depi()
    t(1)=0.d0
    do 10 k = 2, nt
        t(k)=t(k-1)+(depi/omega)/nt
10  continue
!
    do 20 k = 1, nt
        xt(k)=x(1)
        do 21 j = 1, h
            xt(k)=xt(k)+x(j+1)*dcos(dble(j)*omega*t(k))
            xt(k)=xt(k)+x(h+j+1)*dsin(dble(j)*omega*t(k))
21      continue
!
        a1=1.d0/(alpha**2)
        b1=-2.d0*xt(k)/alpha
        c1=xt(k)**2-1.d0
        d1=eta*xt(k)
!
        h1=-b1/(3.d0*a1)
!
        p=(3.d0*a1*h1**2+2.d0*b1*h1+c1)/a1
        q=-(a1*h1**3+b1*h1**2+c1*h1+d1)/a1
!
        discr1=q**2 + (4.d0*p**3)/27.d0
!
        sd=(1.d0-(discr1/abs(discr1)))/2.d0
!
        u3=(q - ((0.d0,1.d0)**int(sd))*sqrt(abs(discr1)))/2.d0
        v3=(q + ((0.d0,1.d0)**int(sd))*sqrt(abs(discr1)))/2.d0
!
        u=u3**(1/3.d0);
        v=v3**(1/3.d0);
!
        f1= u + v + h1
!
        a2=a1
        b2=b1+a1*f1
        c2=c1+b1*f1 + a1*f1**2
!
        discr2= b2**2 - 4.d0*a2*c2
!
        fk= (-b2 + sqrt(b2**2 - 4.d0*a2*c2))/(2.d0*a2)
!
        f(k)=dble(fk)
20  continue
!
    call mnlfft(1, sort(1), f, hf, nt,&
                1)
!
end subroutine

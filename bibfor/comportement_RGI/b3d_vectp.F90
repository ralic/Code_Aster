subroutine b3d_vectp(aa, vp, x, n)
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
! person_in_charge: etienne.grimal at edf.fr
!=====================================================================
!=====================================================================
!     A.Sellier dim. 29 août 2010 08:00:41 CEST
!     passage en double precision des constantes des vecteurs
!     recherche vecteur propre associe a une valeur propre (matrice 3x3)
!     n : multiplicite de la valeur propre
!=====================================================================
    implicit none
#include "asterfort/affiche33.h"
    real(kind=8) :: aa(3, 3)
    real(kind=8) :: vp, a, b, c, d, e, f, det1, det2, det3, xn, xsc
    real(kind=8) :: x(*)
    integer :: n
    a=aa(1,1)-vp
    b=aa(1,2)
    c=aa(1,3)
    d=aa(2,2)-vp
    e=aa(2,3)
    f=aa(3,3)-vp
    if (n .eq. 1) then
        det3=a*d-b*b
        det2=a*f-c*c
        det1=d*f-e*e
        if (abs(det3) .ge. abs(det1) .and. abs(det3) .ge. abs(det2)) then
            x(1)=(b*e-c*d)
            x(2)=(b*c-a*e)
            x(3)=det3
        else if (abs(det1).ge.abs(det2).and.abs(det1).ge.abs(det3)) then
            x(1)=det1
            x(2)=(c*e-b*f)
            x(3)=(b*e-c*d)
        else if (abs(det2).ge.abs(det1).and.abs(det2).ge.abs(det3)) then
            x(1)=(c*e-b*f)
            x(2)=det2
            x(3)=(b*c-a*e)
        endif
        xn=dsqrt(x(1)**2+x(2)**2+x(3)**2)
!      print*,'norme de vp avant normalisation',xn
        if (xn .ne. 0.d0) then
            x(1)=x(1)/xn
            x(2)=x(2)/xn
            x(3)=x(3)/xn
        else
            print*,'norme nulle ds b3d_vectp',n
            print*,'matrice a diagonaliser :'
            call affiche33(aa)
        end if
!      xn=dsqrt(x(1)**2+x(2)**2+x(3)**2)
!      print*,'norme de vp apres normalisation',xn
    else if (n.eq.2) then
        if (abs(a) .ge. abs(d) .and. abs(a) .ge. abs(f)) then
            x(1)=-b/a
            x(2)=1.d0
            x(3)=0.d0
            x(4)=-c/a
            x(5)=0.d0
            x(6)=1.d0
        else if (abs(d).ge.abs(a).and.abs(d).ge.abs(f)) then
            x(1)=1.d0
            x(2)=-b/d
            x(3)=0.d0
            x(4)=0.d0
            x(5)=-e/d
            x(6)=1.d0
        else if (abs(f).ge.abs(a).and.abs(f).ge.abs(d)) then
            x(1)=1.d0
            x(2)=0.d0
            x(3)=-c/f
            x(4)=0.d0
            x(5)=1.d0
            x(6)=-e/f
        endif
        xn=dsqrt(x(1)**2+x(2)**2+x(3)**2)
        x(1)=x(1)/xn
        x(2)=x(2)/xn
        x(3)=x(3)/xn
        xsc=x(1)*x(4)+x(2)*x(5)+x(3)*x(6)
        x(4)=x(4)-xsc*x(1)
        x(5)=x(5)-xsc*x(2)
        x(6)=x(6)-xsc*x(3)
        xn=dsqrt(x(4)**2+x(5)**2+x(6)**2)
        x(4)=x(4)/xn
        x(5)=x(5)/xn
        x(6)=x(6)/xn
    else if (n.eq.3) then
        x(1)=1.d0
        x(2)=0.d0
        x(3)=0.d0
        x(4)=0.d0
        x(5)=1.d0
        x(6)=0.d0
        x(7)=0.d0
        x(8)=0.d0
        x(9)=1.d0
    endif
end subroutine

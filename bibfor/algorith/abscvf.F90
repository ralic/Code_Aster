subroutine abscvf(ndim, tabar, xe, s)
    implicit none
!
    include 'jeveux.h'
    include 'asterc/r8prem.h'
    include 'asterfort/assert.h'
    include 'asterfort/fcthyp.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/vecini.h'
    real(kind=8) :: xe, s, tabar(*)
    integer :: ndim
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! TOLE CRS_1404
!
!                      TROUVER L'ABSCISSE CURVILIGNE D'UN POINT
!                      SUR UNE ARETE QUADRATIQUE A PARTIR DE SES
!                      COORDONNEES DANS L'ELEMENT DE REFERENCE
!
!     ENTREE
!       NDIM    : DIMENSION TOPOLOGIQUE DU MAILLAGE
!       TABAR  : COORDONNEES DES 3 NOEUDS QUI DEFINISSENT L'ARETE
!       XE     : COORDONNEES DU POINT DANS L'ELEMENT DE REFERENCE
!
!     SORTIE
!       S        : ABSCISSE CURVILIGNE DU POINT SUR L'ARETE
!......................................................................
!
    real(kind=8) :: coef1, coef2, coef3, coef4, eps
    real(kind=8) :: pt1(ndim), pt2(ndim), pt3(ndim)
    real(kind=8) :: d, mu
    real(kind=8) :: borsup, borinf
    integer :: i
    character(len=8) :: typfct
!
!......................................................................
!
    call jemarq()
!
!     TABAR : XE2=-1  /  XE1= 1  /  XE3= 0
!     XE2 XENT LE POINT D'ORIGINE
!
!     CALCUL DE COEF1, COEF2, COEF3, D
    coef1=0.d0
    coef2=0.d0
    coef3=0.d0
    call vecini(ndim, 0.d0, pt1)
    call vecini(ndim, 0.d0, pt2)
    call vecini(ndim, 0.d0, pt3)
    eps=1.d-7
!
    do 10 i = 1, ndim
        pt1(i)=tabar(i)
        pt2(i)=tabar(ndim+i)
        pt3(i)=tabar(2*ndim+i)
10  end do
!
    do 101 i = 1, ndim
        coef1 = coef1 + (pt1(i)-2*pt3(i)+pt2(i))* (pt1(i)-2*pt3(i)+ pt2(i))
101  end do
!
    do 102 i = 1, ndim
        coef2 = coef2 + (pt2(i)-pt1(i))*(pt1(i)-2*pt3(i)+pt2(i))
102  end do
!
    do 103 i = 1, ndim
        coef3 = coef3 + (pt2(i)-pt1(i))*(pt2(i)-pt1(i))/4
103  end do
!
    d = coef2*coef2 - 4*coef1*coef3
!
!     CALCUL ABSCISSE CURVILIGNE DU POINT
!
    if (abs(coef1) .le. eps) then
        s = (xe+1)*sqrt(coef3)
    else if (abs(coef1).gt.r8prem()) then
        if (abs(d) .le. r8prem()) then
            s = (coef1*xe*xe + coef2*xe + coef2 - coef1) /(2*sqrt( coef1))
        else if (d.gt.eps) then
            mu = sqrt(d/(4*coef1*coef1))
            coef4 = mu*mu*sqrt(coef1)/4
            typfct = 'ACOSH'
            call fcthyp(typfct, (2*coef1*xe+coef2)/(2*coef1*mu), borsup)
            call fcthyp(typfct, (coef2-2*coef1)/(2*coef1*mu), borinf)
            s = coef4*( sinh(2*borsup)-2*borsup) - coef4*(sinh(2* borinf)-2*borinf)
        else if (d.lt.eps) then
            mu = sqrt(-d/(4*coef1*coef1))
            coef4 = mu*mu*sqrt(coef1)/4
            typfct = 'ASINH'
            call fcthyp(typfct, (2*coef1*xe+coef2)/(2*coef1*mu), borsup)
            call fcthyp(typfct, (coef2-2*coef1)/(2*coef1*mu), borinf)
            s = coef4*( sinh(2*borsup)+2*borsup) - coef4*(sinh(2* borinf)+2*borinf)
!
        endif
    endif
!
    if (s .lt. 0.d0) then
        call assert(2.eq.3)
    endif
!
    call jedema()
end subroutine

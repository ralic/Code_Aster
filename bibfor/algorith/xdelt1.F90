subroutine xdelt1(num, ndim, ksi, tabar, s,&
                  delta)
! aslint: disable=W1306
    implicit none
!
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/fcthyp.h"
#include "asterfort/vecini.h"
    real(kind=8) :: ksi, tabar(*)
    real(kind=8) :: delta, s
    integer :: num, ndim
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
!              CALCUL DE LA QUANTITE A MINIMISER POUR LE CALCUL
!            DES COORDONNEES DE REFERENCE DU PT MILIEU DE L'ARETE
!
!     ENTREE
!       NUM     : NUMERO DE LA FONCTION A RESOUDRE
!       NDIM    : DIMENSION TOPOLOGIQUE DU MAILLAGE
!       KSI     : COORDONNEES DE REFERENCE DU POINT
!       TABAR   : COORDONNEES DES 3 NOEUDS DE L'ARETE
!       S       : ABSCISSE CURVILIGNE DU POINT SUR L'ARETE
!
!     SORTIE
!       DELTA  : PREMIERE QUANTITE A MINIMISER
!     ----------------------------------------------------------------
!
    real(kind=8) :: fctf, fctg, fcth
    real(kind=8) :: dfctf, dfctg, dfcth
    real(kind=8) :: coef1, coef2, coef3, coef4
    real(kind=8) :: pt1(ndim), pt2(ndim), pt3(ndim)
    real(kind=8) :: mu, d
    real(kind=8) :: borsup, borinf, dersup
    integer :: i
    character(len=8) :: typfct
!
!---------------------------------------------------------------------
!
    borsup=0.d0
    borinf=0.d0
    dersup=0.d0
!
!    CALCUL DE COEF1, COEF2, COEF3, D
    coef1=0.d0
    coef2=0.d0
    coef3=0.d0
    call vecini(ndim, 0.d0, pt1)
    call vecini(ndim, 0.d0, pt2)
    call vecini(ndim, 0.d0, pt3)
!
    do 101 i = 1, ndim
        pt1(i)=tabar(i)
        pt2(i)=tabar(ndim+i)
        pt3(i)=tabar(2*ndim+i)
101  end do
!
    do 102 i = 1, ndim
        coef1 = coef1 + (pt1(i)-2*pt3(i)+pt2(i))* (pt1(i)-2*pt3(i)+ pt2(i))
102  end do
!
    do 103 i = 1, ndim
        coef2 = coef2 + (pt2(i)-pt1(i))*(pt1(i)-2*pt3(i)+pt2(i))
103  end do
!
    do 104 i = 1, ndim
        coef3 = coef3 + (pt2(i)-pt1(i))*(pt2(i)-pt1(i))/4
104  end do
!
    d = coef2*coef2 - 4*coef1*coef3
!
    fctf = 0.d0
    fctg = 0.d0
    fcth = 0.d0
    dfctf = 0.d0
    dfctg = 0.d0
    dfcth = 0.d0
!
    if (num .eq. 1) then
!     FCTF : FONCTION ABSCISSE CURVILIGNE POUR COEF1 NE 0 ET D=0
        ASSERT(abs(coef1).gt.r8prem() .and. abs(d).le.r8prem())
        fctf = coef1*ksi*ksi + coef2*ksi + coef2-coef1 -2*s*sqrt( coef1)
        dfctf = 2*coef1*ksi + coef2
        ASSERT(abs(dfctf).gt.r8prem())
        delta = fctf/dfctf
!
    else if (num.eq.2) then
!     FCTG : FONCTION ABSCISSE CURVILIGNE POUR COEF1 NE 0 ET D>0
        ASSERT(abs(coef1).gt.r8prem() .and. d.gt.r8prem())
        mu = sqrt(d/(4*coef1*coef1))
        coef4 = mu*mu*sqrt(coef1)/4
        typfct = 'ACOSH'
        call fcthyp(typfct, (2*coef1*ksi+coef2)/(2*coef1*mu), borsup)
        call fcthyp(typfct, (coef2-2*coef1)/(2*coef1*mu), borinf)
        fctg = coef4*( sinh(2*borsup)-2*borsup) - coef4*(sinh(2*borinf) -2*borinf) -s
        typfct = 'DACOSH'
        call fcthyp(typfct, (2*coef1*ksi+coef2)/(2*coef1*mu), dersup)
        dfctg= coef4*(2/mu)*dersup*(cosh(2*borsup)-1)
        ASSERT(abs(dfctg).gt.r8prem())
        delta = fctg/dfctg
!
    else if (num.eq.3) then
!     FCTH : FONCTION ABSCISSE CURVILIGNE POUR COEF1 NE 0 ET D<0
        ASSERT(abs(coef1).gt.r8prem() .and. d.lt.r8prem())
        mu = sqrt(-d/(4*coef1*coef1))
        coef4 = mu*mu*sqrt(coef1)/4
        typfct = 'ASINH'
        call fcthyp(typfct, (2*coef1*ksi+coef2)/(2*coef1*mu), borsup)
        call fcthyp(typfct, (coef2-2*coef1)/(2*coef1*mu), borinf)
        fcth = coef4*( sinh(2*borsup)+2*borsup) - coef4*(sinh(2*borinf) +2*borinf) -s
        typfct = 'DASINH'
        call fcthyp(typfct, (2*coef1*ksi+coef2)/(2*coef1*mu), dersup)
        dfcth= coef4*(2/mu)*dersup*(cosh(2*borsup)+1)
        ASSERT(abs(dfcth).gt.r8prem())
        delta = fcth/dfcth
!
    endif
!
!
end subroutine

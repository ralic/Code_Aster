subroutine rloshb(xcoq, xcent, ppp, xl, xv24,&
                  xv13, xj)
! ======================================================================
! COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
!--------------------------------------------------------
! ELEMENT SHB8-PS A.COMBESCURE, S.BAGUET INSA LYON 2003 /
!-------------------------------------------------------
!
!          REPERE LOCAL DE LA COQUE BELYTCHKO
!          ===> MATRICE DE PASSAGE PPP
!
!     XCOQ     : COORDONNEES DES 4 NOEUDS COQUES(REPERE GLOBAL)
!     PPP      : MATRICE DE PASSAGE GLOBAL -> LOCAL
!     XCENT : COORDONNEES DANS LE REPERE GLOBAL DU CENTRE DE L ELEMENT
!     XL       : COORDONNEES DES 4 NOEUDS COQUES DANS LE
!                  LE REPERE LOCAL (XCENT, E1, E2, E3)
!
    implicit none
!
!---   VARIABLES GLOBALES
#include "asterfort/assert.h"
    real(kind=8) :: xcoq(3, *), xcent(*), ppp(3, *), xl(3, *), xv13(3), xv24(3)
    real(kind=8) :: xj
!
!---   VARIABLES LOCALES
    integer :: nbn
    parameter(nbn=4)
!
    real(kind=8) :: xmean(3, 4), ss(3), aux, tmp
    integer :: ip, ii
!
!----     DEFINITION DES 4 POINTS  MILIEUX DES COTES
    ii=nbn
    do 10 ip = 1, nbn
        xmean(1,ip) = 0.5d0*(xcoq(1,ii)+xcoq(1,ip))
        xmean(2,ip) = 0.5d0*(xcoq(2,ii)+xcoq(2,ip))
        xmean(3,ip) = 0.5d0*(xcoq(3,ii)+xcoq(3,ip))
        ii=ip
10  end do
!
    xcent(1) = 0.5d0*(xmean(1,1)+xmean(1,3))
    xcent(2) = 0.5d0*(xmean(2,1)+xmean(2,3))
    xcent(3) = 0.5d0*(xmean(3,1)+xmean(3,3))
!
! XV13 EST DANS LA DIRECTION DE E2
!
    xv13(1) = 0.5d0*(xmean(1,3)-xmean(1,1))
    xv13(2) = 0.5d0*(xmean(2,3)-xmean(2,1))
    xv13(3) = 0.5d0*(xmean(3,3)-xmean(3,1))
!
! XV24 = E1
!
    xv24(1) = 0.5d0*(xmean(1,4)-xmean(1,2))
    xv24(2) = 0.5d0*(xmean(2,4)-xmean(2,2))
    xv24(3) = 0.5d0*(xmean(3,4)-xmean(3,2))
!
!-----      REPERE LOCAL
!
! LE VECTEUR UNITAIRE E1 (PPP(;,1))= XV24 / ||XV24||
!
    tmp=sqrt(xv24(1)*xv24(1)+xv24(2)*xv24(2)+xv24(3)*xv24(3))
    ppp(1,1)=xv24(1)/tmp
    ppp(2,1)=xv24(2)/tmp
    ppp(3,1)=xv24(3)/tmp
!
! LE VECTEUR UNITAIRE E3 (PPP(;,3)) = XV24 ^ XV13
!
    ss(1) = xv24(2)*xv13(3) - xv24(3)*xv13(2)
    ss(2) = xv24(3)*xv13(1) - xv24(1)*xv13(3)
    ss(3) = xv24(1)*xv13(2) - xv24(2)*xv13(1)
    xj = sqrt (ss(1)*ss(1)+ss(2)*ss(2)+ss(3)*ss(3))
    call assert(xj.gt.0)
    aux=1/xj
    ppp(1,3) = ss(1) * aux
    ppp(2,3) = ss(2) * aux
    ppp(3,3) = ss(3) * aux
!
! LE VECTEUR UNITAIRE  E2 = E3 ^ E1
!
    ppp(1,2) = ppp(2,3)*ppp(3,1) - ppp(3,3)*ppp(2,1)
    ppp(2,2) = ppp(3,3)*ppp(1,1) - ppp(1,3)*ppp(3,1)
    ppp(3,2) = ppp(1,3)*ppp(2,1) - ppp(2,3)*ppp(1,1)
!
! DANS XMEAN, ON MET XCOQ DANS LE REPERE GLOBAL TRANSLATE AU POINT XCENT
!
    do 20 ip = 1, nbn
        xmean(1,ip) = xcoq(1,ip)-xcent(1)
        xmean(2,ip) = xcoq(2,ip)-xcent(2)
        xmean(3,ip) = xcoq(3,ip)-xcent(3)
!
20  continue
!
! XL : COORD DES 4 NOEUDS COQUE DANS LE REPERE LOCAL (XCENT,E1,E2,E3)
!
    do 30 ip = 1, nbn
        xl(1,ip) = ppp(1,1)*xmean(1,ip) + ppp(2,1)*xmean(2,ip) + ppp(3,1)*xmean(3,ip)
        xl(2,ip) = ppp(1,2)*xmean(1,ip) + ppp(2,2)*xmean(2,ip) + ppp(3,2)*xmean(3,ip)
        xl(3,ip) = ppp(1,3)*xmean(1,ip) + ppp(2,3)*xmean(2,ip) + ppp(3,3)*xmean(3,ip)
!
30  continue
!
end subroutine

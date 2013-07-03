subroutine dfdm2d(nno, ipg, ipoids, idfde, coor,&
                  dfdx, dfdy, jac)
    implicit none
#include "jeveux.h"
#include "asterc/r8gaem.h"
#include "asterfort/tecael.h"
#include "asterfort/u2mesk.h"
    integer :: nno, ipg, ipoids, idfde
    real(kind=8) :: coor(1), dfdx(1), dfdy(1), jac
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
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES DERIVEES DES FONCTIONS DE FORME
!               PAR RAPPORT A UN ELEMENT COURANT EN UN POINT DE GAUSS
!
!    - ARGUMENTS:
!        DONNEES:     NNO           -->  NOMBRE DE NOEUDS
!                     POIDS         -->  POIDS DE GAUSS
!                     DFRDE,DFRDK   -->  DERIVEES FONCTIONS DE FORME
!                     COOR          -->  COORDONNEES DES NOEUDS
!
!        RESULTATS:   DFDX          <--  DERIVEES DES F. DE F. / X
!                     DFDY          <--  DERIVEES DES F. DE F. / Y
!                     JAC           <--  PRODUIT DU JACOBIEN ET DU POIDS
! ......................................................................
!
    integer :: i, ii, k, iadzi, iazk24
    character(len=8) :: nomail
    real(kind=8) :: poids, de, dk, dxde, dxdk, dyde, dydk
!
    poids = zr(ipoids+ipg-1)
!
    dxde = 0.d0
    dxdk = 0.d0
    dyde = 0.d0
    dydk = 0.d0
    do 100 i = 1, nno
        k = 2*nno*(ipg-1)
        ii = 2*(i-1)
        de = zr(idfde-1+k+ii+1)
        dk = zr(idfde-1+k+ii+2)
        dxde = dxde + coor(2*i-1)*de
        dxdk = dxdk + coor(2*i-1)*dk
        dyde = dyde + coor(2*i )*de
        dydk = dydk + coor(2*i )*dk
100  end do
!
    jac = dxde*dydk - dxdk*dyde
!
    if (abs(jac) .le. 1.d0/r8gaem()) then
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3)(1:8)
        call u2mesk('F', 'ALGORITH2_59', 1, nomail)
    endif
!
    do 200 i = 1, nno
        k = 2*nno*(ipg-1)
        ii = 2*(i-1)
        de = zr(idfde-1+k+ii+1)
        dk = zr(idfde-1+k+ii+2)
        dfdx(i) = (dydk*de-dyde*dk)/jac
        dfdy(i) = (dxde*dk-dxdk*de)/jac
200  end do
!
    jac = abs(jac)*poids
!
end subroutine

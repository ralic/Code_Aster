subroutine dfdm1b(nno, poids, dfrdk, coor, dfdx,&
                  jacp)
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
    implicit none
#include "jeveux.h"
#include "asterc/r8gaem.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
    integer :: nno
    real(kind=8) :: dfrdk(1), coor(*), dfdx(1)
    real(kind=8) :: jacp, poids
! ......................................................................
!    - BUTS:  CALCULER LA VALEUR DU POIDS D'INTEGRATION EN 1 POINT DE
!             GAUSS POUR UN SEGMENT PLONGE DANS LE 3D
!      + CALCULE LES DERIVEES DES FONCTIONS DE FORME DANS L'ELEMENT REEL
!
!    - ARGUMENTS:
!        DONNEES:     NNO           -->  NOMBRE DE NOEUDS
!                     POIDS         -->  POIDS DE GAUSS
!                     DFRDK         -->  DERIVEES FONCTIONS DE FORME
!                     COOR          -->  COORDONNEES DES NOEUDS
!
!        RESULTATS:   DFDX          <--  DERIVEES DES FONCTIONS DE FORME
!                                        / ABSCISSE CURVILIGNE
!                     JACP          <--  PRODUIT DU JACOBIEN ET DU POIDS
!
!-----------------------------------------------------------------------
    character(len=8) :: nomail
    integer :: iadzi, iazk24, i
    real(kind=8) :: jac, dxdk, dydk, dzdk
!-----------------------------------------------------------------------
    dxdk = 0.d0
    dydk = 0.d0
    dzdk = 0.d0
    do i = 1, nno
        dxdk = dxdk + coor( 3*i-2 ) * dfrdk(i)
        dydk = dydk + coor( 3*i-1 ) * dfrdk(i)
        dzdk = dzdk + coor( 3*i ) * dfrdk(i)
    end do
    jac = sqrt ( dxdk**2 + dydk**2 +dzdk**2)
!
    if (abs(jac) .le. 1.d0/r8gaem()) then
        call tecael(iadzi, iazk24)
        nomail= zk24(iazk24-1+3)(1:8)
        call utmess('F', 'ALGORITH2_59', sk=nomail)
    endif
!
    do i = 1, nno
        dfdx(i) = dfrdk(i) / jac
    end do
    jacp = jac * poids
end subroutine

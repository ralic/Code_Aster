subroutine dfdm1d(nno, poids, dfrdk, coor, dfdx,&
                  cour, jacp, cosa, sina)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
    include 'jeveux.h'
    include 'asterc/r8gaem.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesk.h'
    integer :: nno
    real(kind=8) :: dfrdk(1), coor(*), dfdx(1)
    real(kind=8) :: dxdk, dydk, cour, jac, jacp, poids, sina, cosa
! ......................................................................
!    - BUTS:  CALCULER LA VALEUR DU POIDS D'INTEGRATION EN 1 POINT DE
!             GAUSS POUR UN SEGMENT PLAN
!      + CALCULE LE SINUS ET LE COSIMUS DE L'ANGLE ENTRE LA NORMALE
!        ET L'AXE OX
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
!                     COUR          <--  COURBURE AU NOEUD
!                     COSA          <--  COS DE L'ANGLE ALPHA:
!                                        NORMALE / HORIZONTALE
!                     SINA          <--  SIN DE L'ANGLE ALPHA
!                     JACP          <--  PRODUIT DU JACOBIEN ET DU POIDS
!
!  REMARQUE :
!    - LES SEGMENTS DOIVENT ETRE "PLANS" (DANS OXY)
!
    character(len=8) :: nomail
    integer :: i
!
!-----------------------------------------------------------------------
    integer :: iadzi, iazk24
    real(kind=8) :: d2xdk, d2ydk
!-----------------------------------------------------------------------
    dxdk = 0.d0
    dydk = 0.d0
    do 100 i = 1, nno
        dxdk = dxdk + coor( 2*i-1 ) * dfrdk(i)
        dydk = dydk + coor( 2*i ) * dfrdk(i)
100  end do
    jac = sqrt ( dxdk**2 + dydk**2 )
!
    if (abs(jac) .le. 1.d0/r8gaem()) then
        call tecael(iadzi, iazk24)
        nomail= zk24(iazk24-1+3)(1:8)
        call u2mesk('F', 'ALGORITH2_59', 1, nomail)
    endif
!
    cosa = dydk/jac
    sina = -dxdk/jac
    d2xdk = coor(1) + coor(3) - 2.d0 * coor(5)
    d2ydk = coor(2) + coor(4) - 2.d0 * coor(6)
    cour = ( dxdk * d2ydk - d2xdk * dydk ) / jac**3
    do 200 i = 1, nno
        dfdx(i) = dfrdk(i) / jac
200  end do
    jacp = jac * poids
end subroutine

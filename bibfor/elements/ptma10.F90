subroutine ptma10(m, rho, a, xl, xiy,&
                  xiz)
    implicit none
    real(kind=8) :: m(*), rho, a, xl, xiy, xiz
!     ------------------------------------------------------------------
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
!     CALCUL DE LA MATRICE DE MASSE DES ELEMENTS DE POUTRE
!          - COURBE A SECTION CONSTANTE
!     PAR LA METHODE
!          - DES MASSES CONCENTREES
!     ------------------------------------------------------------------
! OUT M        -(78) MATRICE DE MASSE ELEMENT
! IN  RHO        - MASSE VOLUMIQUE DU MATERIAU
! IN  A          - AIRE DE LA SECTION DROITE
! IN  XL         - LONGUEUR DE L ELEMENT
! IN  XIY        - MOMENT D INERTIE / Y PRINCIPAL
! IN  XIZ        - MOMENT D INERTIE / Z PRINCIPAL
!     ------------------------------------------------------------------
    real(kind=8) :: zaire
    real(kind=8) :: c, cc
    real(kind=8) :: zero, c002, c015, c048, c105
    integer :: ip(12)
!-----------------------------------------------------------------------
    integer :: i
!-----------------------------------------------------------------------
    data    ip/0,1,3,6,10,15,21,28,36,45,55,66/
!
!     INITIALISATION
    zero = 0.d0
    do 10 i = 1, 78
        m(i) = zero
10  end do
    c002 = 2.d0
    c015 = 15.d0
    c048 = 48.d0
    c105 = 105.d0
!
!     MASSES CONCENTREES FORMULATION S.D.R.C.   DANS TOUS LES CAS
    zaire = rho * a * xl / c002
    m( ip( 1)+ 1) = zaire
    m( ip( 2)+ 2) = zaire
    m( ip( 3)+ 3) = zaire
!--   M( IP( 4)+ 4) = ZINEX
!--   M( IP( 5)+ 5) = ZINEY
!--   M( IP( 6)+ 6) = ZINEZ
!
    m( ip( 7)+ 7) = zaire
    m( ip( 8)+ 8) = zaire
    m( ip( 9)+ 9) = zaire
!---  M( IP(10)+10) = ZINEX
!---  M( IP(11)+11) = ZINEY
!---  M( IP(12)+12) = ZINEZ
!
    cc = m( ip( 1)+ 1) + m( ip( 7)+ 7)
    c = min ( cc * xl * xl / c105 , cc * xl / c048 )
!
    m( ip( 4)+ 4) = ( xiy + xiz ) * xl * rho / c002
    m( ip( 5)+ 5) = c + rho * xiy * xl * c002 / c015
    m( ip( 6)+ 6) = c + rho * xiz * xl * c002 / c015
!
    m( ip(10)+10) = m( ip( 4)+ 4)
    m( ip(11)+11) = m( ip( 5)+ 5)
    m( ip(12)+12) = m( ip( 6)+ 6)
!
end subroutine

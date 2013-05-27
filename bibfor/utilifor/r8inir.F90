subroutine r8inir(n, sa, sx, incx)
    implicit none
    integer :: n, incx
    real(kind=8) :: sa, sx(*)
!     ------------------------------------------------------------------
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
!     INITIALISATION D'UN VECTEUR SX A LA VALEUR SA.
!     ------------------------------------------------------------------
! IN  N    : I :  NOMBRE D'ELEMENTS DANS LE VECTEUR
! IN  SA   : R :  VALEUR D'INITIALISATION
! OUT SX   : R :  VECTEUR A INITIALISER
! IN  INCX : I :  INCREMENT  POUR  LE  VECTEUR  X
!     ------------------------------------------------------------------
!     LES  ARGUMENTS SX ET SY  CORRESPONDENT AU PREMIER TERME DE CHAQUE
!     VECTEUR  UTILISE  DANS  LA  BOUCLE .
!     EXEMPLE:
!     -  INCX EST POSITIF , CETTE  UNITE  PART  DE  X(1)  ET  DESCEND
!     EN  UTILISANT  X(1+INCX) , X(1+2*INCX) , ETC....
!     -  INCX EST NEGATIF , PRENONS -2 , CETTE   UNITE  VA  UTILISER
!     SUCCESSIVEMENT  X(1) , X(-1) , X(-3) , ETC....
!     ------------------------------------------------------------------
    integer :: i
!-----------------------------------------------------------------------
    do 1 i = 1, n
        sx( 1+(i-1)*incx ) = sa
 1  end do
end subroutine

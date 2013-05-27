subroutine gdliva(kp, vari, ca)
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
!
! FONCTION: POUR UN ELEMENT DE POUTRE EN GRAND DEPLACEMENT, LIT  , DANS
!           LES VARIABLES INTERNES 'PVARIMR', LE VECTEUR-COURBURE AU
!           POINT DE GAUSS KP A L'ITERATION PRECEDENTE
!
!     IN  : KP        : NUMERO DU POINT DE GAUSS
!           VARI      : CHAMP DES 'PVARIMR'
!
!     OUT : CA        : VECTEUR-COURBURE (3)
! ------------------------------------------------------------------
    implicit none
    real(kind=8) :: vari(*), ca(3)
!
!-----------------------------------------------------------------------
    integer :: i, k, kp
!-----------------------------------------------------------------------
    k = (kp-1) * 3
    do 1 i = 1, 3
        k = k + 1
        ca(i) = vari(k)
 1  end do
end subroutine

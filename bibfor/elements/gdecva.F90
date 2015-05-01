subroutine gdecva(kp, ca, vari)
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
!
! FONCTION: POUR UN ELEMENT DE POUTRE EN GRAND DEPLACEMENT, ECRIT, DANS
!           LES VARIABLES INTERNES 'PVARIPR', LE VECTEUR-COURBURE ACTUA-
!           LISE AU POINT DE GAUSS KP
!
!     IN  : KP        : NUMERO DU POINT DE GAUSS
!           CA        : VECTEUR-COURBURE (3)
!
!     OUT : VARI      : CHAMP DES 'PVARI(M OU P)R'
! ------------------------------------------------------------------
    implicit none
    real(kind=8) :: ca(3), vari(*)
!
!-----------------------------------------------------------------------
    integer :: i, k, kp
!-----------------------------------------------------------------------
    k = (kp-1) * 3
    do 1 i = 1, 3
        k = k + 1
        vari(k) = ca(i)
 1  end do
end subroutine

subroutine xdeffe(r, theta, fe)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
!
    real(kind=8) :: r, theta, fe(4)
!
!
!     BUT:  FONCTIONS D'ENRICHISSEMENT DANS LA BASE POLAIRE (R,THETA)
!
! IN  R      : PREMIERE COORDONNEE DANS LA BASE POLAIRE
! IN  THETA  : SECONDE COORDONNEE DANS LA BASE POLAIRE
! OUT FE      : VALEURS DES FONCTIONS D'ENRICHISSEMENT
!
!----------------------------------------------------------------
!
!     FONCTIONS D'ENRICHISSEMENT
    fe(1) = sqrt(r) * sin(theta/2.d0)
    fe(2) = sqrt(r) * cos(theta/2.d0)
    fe(3) = sqrt(r) * sin(theta/2.d0) * sin(theta)
    fe(4) = sqrt(r) * cos(theta/2.d0) * sin(theta)
!
end subroutine

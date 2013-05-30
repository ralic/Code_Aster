subroutine xderfe(r, theta, dfedp)
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
    real(kind=8) :: r, theta, dfedp(4, 2)
!
!
!     BUT : DERIVEES DES FONCTIONS D'ENRICHISSEMENT
!           DANS LA BASE POLAIRE (R,THETA)
!
! IN  R      : PREMIERE COORDONNEE DANS LA BASE POLAIRE
! IN  THETA  : SECONDE COORDONNEE DANS LA BASE POLAIRE
! OUT DFEDP  : DERIVEES DES FONCTIONS D'ENRICHISSEMENT
!
!----------------------------------------------------------------
!
    real(kind=8) :: rr, s, c, s2, c2
!
!----------------------------------------------------------------
!
!     DEFINITION DE QUELQUES VARIABLES UTILES
    rr = sqrt(r)
    s = sin(theta)
    c = cos(theta)
    s2 = sin(theta/2.d0)
    c2 = cos(theta/2.d0)
!
!     DÉRIVÉES DES FONCTIONS D'ENRICHISSEMENT DANS LA BASE POLAIRE
    dfedp(1,1) = 1.d0/(2.d0*rr) * s2
    dfedp(1,2) = rr/2.d0 * c2
    dfedp(2,1) = 1.d0/(2.d0*rr) * c2
    dfedp(2,2) = -rr/2.d0 * s2
    dfedp(3,1) = 1.d0/(2.d0*rr) * s2 * s
    dfedp(3,2) = rr * (c2*s/2.d0 + s2*c)
    dfedp(4,1) = 1.d0/(2.d0*rr) *c2 * s
    dfedp(4,2) = rr * (-s2*s/2.d0 + c2*c)
!
end subroutine

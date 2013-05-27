subroutine coesp4(tauvid, phi0)
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
    implicit none
!
!
! DESCRIPTION : VALEURS DES COEFFICIENTS DEFINISSANT
! -----------   LE SPECTRE DE TURBULENCE.
!
!
!    PHI0 DEPEND DU TAUX DE VIDE TAUVID.
!    BETA ET GAMMA SONT CONSTANTS.
!
! ******************   DECLARATION DES VARIABLES   *********************
!
! ARGUMENTS
! ---------
    real(kind=8) :: tauvid, phi0
!
! ******************   DEBUT DU CODE EXECUTABLE   **********************
!
    phi0 = (&
           24.042d0 * (tauvid**0.5d0) ) - ( 50.421d0 * (tauvid**1.5d0) ) + ( 63.483d0 * (tauvid**&
           &2.5d0) ) - ( 33.284d0 * (tauvid**3.5d0)&
           )
!
    phi0 = (10.0d0**phi0) / (6.8d-2)
!
end subroutine

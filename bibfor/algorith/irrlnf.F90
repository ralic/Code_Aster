subroutine irrlnf(nmat, materf, yf, eloupl, vinf)
    implicit none
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: jean-luc.flejou at edf.fr
! --- -------------------------------------------------------------
!
!    IRRAD3M  : CORRESPONDANCE ENTRE LES VARIABLES INTERNES ET LES
!               EQUATIONS DU SYSTEME DIFFERENTIEL INTEGREES PAR NEWTON
!
! --- -------------------------------------------------------------
!  IN
!     NMAT   : DIMENSION MATER
!     MATERF : COEF MATERIAU
!     YF     : VARIABLES INTERNES INTEGREES PAR NEWTON
!     ELOUPL : ELASTIQUE OU PLASTIQUE (0 OU 1)
!     IDPLAS : INDICATEUR PLASTIQUE
!  OUT
!     VINF   : VARIABLES INTERNES INTEGREES
! --- -------------------------------------------------------------
    integer :: nmat
    real(kind=8) :: yf(*), vinf(*), materf(nmat, 2), eloupl
!
!     DEFORMATION PLASTIQUE CUMULEE
    vinf(1) = yf(1)
!     FONCTION SEUIL DE FLUAGE
    vinf(2) = yf(2)
!     DEFORMATION EQUIVALENTE DE FLUAGE
    vinf(3) = yf(3)
!     DEFORMATION DE GONFLEMENT
    vinf(4) = yf(4)
!     INDICATEUR PLASTIQUE
    vinf(5) = eloupl
!     IRRADIATION
    vinf(6) = materf(18,2)
!     TEMPERATURE
    vinf(7) = materf(22,2)
end subroutine

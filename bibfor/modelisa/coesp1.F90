subroutine coesp1(ren, phi0, eps, frc, beta)
!-----------------------------------------------------------------------
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
!    PHI0, EPS ET BETA DEPENDENT DU REYNOLDS REN.
!    FRC EST CONSTANT.
!
! ******************   DECLARATION DES VARIABLES   *********************
!
! ARGUMENTS
! ---------
    real(kind=8) :: ren, phi0, eps, frc, beta
!
! ******************   DEBUT DU CODE EXECUTABLE   **********************
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    if (ren .le. 1.5d+4) then
        phi0 = 2.1808d0
    else if (ren .le. 5.0d+4) then
        phi0 = 20.42d0 - 14.00d-4 * ren - 9.81d-8 * ren*ren + 11.97d-12 * ren*ren*ren - 35.95d-17&
               & * ren*ren*ren*ren + 34.69d-22 * ren*ren*ren*ren*ren
    else
        phi0 = 38.6075d0
    endif
    phi0 = phi0 * 1.3d-4
!
    if (ren .le. 3.5d+4) then
        eps = 0.7d0
        beta = 3.0d0
    else if (ren .gt. 5.5d+4) then
        eps = 0.6d0
        beta = 4.0d0
    else
        eps = 0.3d0
        beta = 4.0d0
    endif
!
    frc = 0.2d0
!
end subroutine

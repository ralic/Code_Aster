subroutine cfresa(ndim, fctc, norm, rnx, rny,&
                  rnz, rn)
!
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit     none
    include 'jeveux.h'
    integer :: ndim
    real(kind=8) :: fctc(3)
    real(kind=8) :: norm(3)
    real(kind=8) :: rnx
    real(kind=8) :: rny
    real(kind=8) :: rnz
    real(kind=8) :: rn
!
! ======================================================================
! ROUTINE APPELEE PAR : CFRESU
! ======================================================================
!
! CALCUL DES REACTIONS NORMALES DE CONTACT
!
! IN  NDIM   : DIMENSION DU MODELE
! IN  FCTC   : FORCES DE CONTACT NODALES
! IN  NORM   : NORMALE
! OUT RNX    : FORCE DE REACTION NORMALE PROJETEE SUR X
! OUT RNY    : FORCE DE REACTION NORMALE PROJETEE SUR Y
! OUT RNZ    : FORCE DE REACTION NORMALE PROJETEE SUR Z
! OUT RN     : FORCE DE REACTION NORMALE RESULTANTE
!
!
!
!
!
    real(kind=8) :: proj
!
! ----------------------------------------------------------------------
!
    proj = fctc(1) * norm(1) + fctc(2) * norm(2)
!
    if (ndim .eq. 3) then
        proj = proj + fctc(3) * norm(3)
    endif
!
    rnx = proj * norm(1)
    rny = proj * norm(2)
    rnz = 0.d0
!
    if (ndim .eq. 3) then
        rnz = proj * norm(3)
    endif
!
    rn = sqrt(rnx**2+rny**2+rnz**2)
!
end subroutine

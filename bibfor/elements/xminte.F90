subroutine xminte(ndim, integ, fpg)
!
    implicit none
    integer :: ndim, integ
    character(len=8) :: fpg
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
! ROUTINE CONTACT (METHODE XFEM HPP - CALCUL ELEM.)
!
! --- SCHEMA D'INTEGRATION NUMERIQUE SUR LA SURFACE DE CONTACT
!
! ----------------------------------------------------------------------
!
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  INTEG  : NUMERO DU TYPE D'INTEGRATION
! OUT FPG    : NOM (LOCAL) DE LA FAMILLE DE POINTS DE GAUSS
!
! ----------------------------------------------------------------------
!
    if (ndim .eq. 3) then
        if (integ .eq. 1) fpg='NOEU'
        if (integ .eq. 62 .or. integ .eq. 72 .or. integ .eq. 82 .or. integ .eq. 92 .or. integ&
            .eq. 102) fpg='GAUSS'
        if (integ .eq. 13) fpg='SIMP'
        if (integ .eq. 23 .or. integ .eq. 33 .or. integ .eq. 43) fpg='SIMP1'
        if (integ .eq. 34 .or. integ .eq. 44 .or. integ .eq. 54) fpg='COTES'
        if (integ .eq. 32) fpg='FPG4'
        if (integ .eq. 42) fpg='FPG6'
        if (integ .eq. 52) fpg='FPG7'
!
    else if (ndim.eq.2) then
        if (integ .eq. 1) fpg='NOEU'
        if (integ .eq. 32) fpg='GAUSS'
        if (integ .eq. 13) fpg='SIMP'
        if (integ .eq. 23 .or. integ .eq. 33 .or. integ .eq. 43) fpg='SIMP1'
        if (integ .eq. 34) fpg='COTES'
        if (integ .eq. 54 .or. integ .eq. 44) fpg='COTES1'
        if (integ .eq. 84 .or. integ .eq. 64 .or. integ .eq. 74 .or. integ .eq. 94 .or. integ&
            .eq. 104) fpg='COTES2'
        if (integ .eq. 22) fpg='FPG2'
        if (integ .eq. 32) fpg='FPG3'
        if (integ .eq. 42 .or. integ .eq. 52 .or. integ .eq. 62 .or. integ .eq. 72 .or. integ&
            .eq. 82 .or. integ .eq. 92 .or. integ .eq. 102) fpg='FPG4'
!
    endif
!
end subroutine

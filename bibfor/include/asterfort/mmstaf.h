!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
! aslint: disable=W1504
#include "asterf_types.h"
!
interface
    subroutine mmstaf(mesh          , ndim  , chdepd, coef_frot   , lpenaf      , &
                      nummae        , aliase, nne   , nummam      , ksipc1      , &
                      ksipc2        , ksipr1, ksipr2, mult_lagr_f1, mult_lagr_f2, &
                      tang_1        , tang_2, norm  , pres_frot   , dist_frot   , &
                      indi_frot_eval)
        character(len=8), intent(in) :: mesh
        integer, intent(in) :: ndim
        character(len=19), intent(in) :: chdepd
        real(kind=8), intent(in) :: coef_frot
        aster_logical, intent(in) :: lpenaf
        integer, intent(in) :: nummae
        character(len=8), intent(in) :: aliase
        integer, intent(in) :: nne
        integer, intent(in) :: nummam
        real(kind=8), intent(in) :: ksipc1
        real(kind=8), intent(in) :: ksipc2
        real(kind=8), intent(in) :: ksipr1
        real(kind=8), intent(in) :: ksipr2
        real(kind=8), intent(in) :: mult_lagr_f1(9)
        real(kind=8), intent(in) :: mult_lagr_f2(9)
        real(kind=8), intent(in) :: tang_1(3)
        real(kind=8), intent(in) :: tang_2(3)
        real(kind=8), intent(in) :: norm(3)
        real(kind=8), intent(out) :: pres_frot(3)
        real(kind=8), intent(out) :: dist_frot(3)
        integer, intent(out) :: indi_frot_eval
    end subroutine mmstaf
end interface

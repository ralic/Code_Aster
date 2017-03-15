!
! COPYRIGHT (C) 1991 - 2017  EDF R&D                WWW.CODE-ASTER.ORG
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
#include "asterf_types.h"
!
interface
    subroutine calcGetDataMeca(list_load      , model         , mate     , cara_elem,&
                               disp_prev      , disp_cumu_inst, vari_prev, sigm_prev,&
                               ds_constitutive, l_elem_nonl, nume_harm)
        use NonLin_Datastructure_type
        character(len=19), intent(out) :: list_load
        character(len=24), intent(out) :: model
        character(len=24), intent(out) :: mate
        character(len=24), intent(out) :: cara_elem
        character(len=19), intent(out) :: disp_prev
        character(len=19), intent(out) :: disp_cumu_inst
        character(len=19), intent(out) :: vari_prev
        character(len=19), intent(out) :: sigm_prev
        type(NL_DS_Constitutive), intent(out) :: ds_constitutive
        aster_logical, intent(out) :: l_elem_nonl
        integer, intent(out) :: nume_harm
    end subroutine calcGetDataMeca
end interface

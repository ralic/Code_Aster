!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
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
! aslint: disable=W1504
!
interface
    subroutine mmeval_prep(mesh   , time_curr  , model_ndim     , sdcont_defi , sdcont_solv,&
                           l_speed, speed_field, i_zone         ,&
                           ksipc1 , ksipc2     , ksipr1         , ksipr2     ,&
                           tau1   , tau2       ,&
                           elem_slav_indx, elem_slav_nume, elem_slav_nbno,&
                           elem_slav_type, elem_slav_coor,&
                           elem_mast_nume,&
                           lagr_cont_node,&
                           norm   , &
                           gap    , gap_user, gap_speed  , lagr_cont_poin)
        character(len=8), intent(in) :: mesh
        real(kind=8), intent(in) :: time_curr
        integer, intent(in) :: model_ndim
        character(len=24), intent(in) :: sdcont_defi
        character(len=24), intent(in) :: sdcont_solv
        aster_logical, intent(in) :: l_speed
        character(len=19), intent(in) :: speed_field
        integer, intent(in) :: i_zone
        real(kind=8), intent(in) :: ksipc1
        real(kind=8), intent(in) :: ksipc2
        real(kind=8), intent(in) :: ksipr1
        real(kind=8), intent(in) :: ksipr2
        real(kind=8), intent(in) :: tau1(3)
        real(kind=8), intent(in) :: tau2(3)
        integer, intent(in) :: elem_slav_nbno
        integer, intent(in) :: elem_slav_indx
        integer, intent(in) :: elem_slav_nume
        character(len=8), intent(in) :: elem_slav_type
        real(kind=8), intent(in) :: elem_slav_coor(27)
        integer, intent(in) :: elem_mast_nume
        real(kind=8), intent(in) :: lagr_cont_node(9)
        real(kind=8), intent(out) :: norm(3)
        real(kind=8), intent(out) :: gap
        real(kind=8), intent(out) :: gap_user
        real(kind=8), intent(out) :: gap_speed
        real(kind=8), intent(out) :: lagr_cont_poin
    end subroutine mmeval_prep
end interface

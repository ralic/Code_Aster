!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine lctppe(side      , elem_dime , l_axis     ,&
                      nb_node   , elem_coor , elem_code  ,&
                      gauss_coor, shape_func, shape_dfunc,&
                      jacobian , l_jaco_upda , norm, jv_geom,&
                      shift_)
        character(len=*), intent(in) :: side
        integer, intent(in) :: elem_dime
        aster_logical, intent(in) :: l_axis
        integer, intent(in) :: nb_node
        real(kind=8), intent(in) :: elem_coor(elem_dime,nb_node)
        character(len=8), intent(in) :: elem_code   
        real(kind=8), intent(in) :: gauss_coor(2)
        real(kind=8), intent(out) :: shape_func(9)
        real(kind=8), intent(out) :: shape_dfunc(2, 9)
        real(kind=8), intent(out) :: jacobian 
        aster_logical, intent(in) :: l_jaco_upda
        real(kind=8), intent(out) :: norm(3)
        integer, intent(in) :: jv_geom
        integer, intent(in), optional :: shift_
    end subroutine lctppe
end interface

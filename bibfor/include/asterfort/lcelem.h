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
! aslint: disable=W1501
!
interface
    subroutine lcelem(nomte         , elem_dime     ,&
                      l_axis        , l_elem_frot   ,&
                      nb_dof        , nb_lagr       , indi_lagc   ,&
                      elem_slav_code, elga_fami_slav, nb_node_slav,&
                      elem_mast_code, elga_fami_mast, nb_node_mast)
        character(len=16), intent(in) :: nomte
        integer, intent(out) :: elem_dime
        aster_logical, intent(out) :: l_axis
        aster_logical, intent(out) :: l_elem_frot
        integer, intent(out) :: nb_dof
        integer, intent(out) :: nb_lagr
        integer, intent(out) :: indi_lagc(10)
        character(len=8), intent(out) :: elem_slav_code
        character(len=8), intent(out) :: elga_fami_slav
        integer, intent(out) :: nb_node_slav
        character(len=8), intent(out) :: elem_mast_code
        character(len=8), intent(out) :: elga_fami_mast
        integer, intent(out) :: nb_node_mast
    end subroutine lcelem
end interface

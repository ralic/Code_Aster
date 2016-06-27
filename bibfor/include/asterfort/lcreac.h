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
interface
    subroutine lcreac(nb_lagr       , indi_lagc      , elem_dime   , coef_upda_geom,&
                      nb_node_slav  , nb_node_mast   ,&
                      jv_geom       , jv_disp        , jv_disp_incr,&
                      elem_slav_coor, elem_mast_coor)
        integer, intent(in) :: elem_dime
        integer, intent(in) :: nb_lagr
        integer, intent(in) :: indi_lagc(10)
        integer, intent(in) :: nb_node_slav
        integer, intent(in) :: nb_node_mast
        real(kind=8), intent(in) :: coef_upda_geom     
        integer, intent(in) :: jv_geom
        integer, intent(in) :: jv_disp
        integer, intent(in) :: jv_disp_incr
        real(kind=8), intent(inout) :: elem_slav_coor(elem_dime, nb_node_slav)
        real(kind=8), intent(inout) :: elem_mast_coor(elem_dime, nb_node_mast)
    end subroutine lcreac
end interface

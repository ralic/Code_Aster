subroutine lcgeog(elem_dime     , nb_lagr       , indi_lagc ,&
                  nb_node_slav  , nb_node_mast  , &
                  algo_reso_geom, elem_mast_coor, elem_slav_coor,&
                  norm_smooth)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/jevech.h"
#include "asterfort/lcreac.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer, intent(in) :: elem_dime
    integer, intent(in) :: nb_lagr
    integer, intent(in) :: indi_lagc(10)
    integer, intent(in) :: nb_node_slav
    integer, intent(in) :: nb_node_mast
    integer, intent(in) :: algo_reso_geom
    real(kind=8), intent(inout) :: elem_slav_coor(elem_dime, nb_node_slav)
    real(kind=8), intent(inout) :: elem_mast_coor(elem_dime, nb_node_mast)
    integer, intent(out) :: norm_smooth
!
! --------------------------------------------------------------------------------------------------
!
! Contact (LAC) - Elementary computations
!
! Compute updated geometry
!
! --------------------------------------------------------------------------------------------------
!
! In  elem_dime        : dimension of elements
! In  nb_lagr          : total number of Lagrangian dof on contact element
! In  indi_lagc        : PREVIOUS node where Lagrangian dof is present (1) or not (0)
! In  nb_node_slav     : number of nodes of for slave side from contact element
! In  nb_node_mast     : number of nodes of for master side from contact element
! In  algo_reso_geom   : algorithm for geometry loop
!                         0 - fixed point
!                         1 - Newton
! IO  elem_slav_coor   : updated coordinates from slave side of contact element
! IO  elem_mast_coor   : updated coordinates from master side of contact element
! Out norm_smooth      : indicator for normals smoothing
!
! --------------------------------------------------------------------------------------------------
!
    integer :: jpcf, jv_disp, jv_geom, jv_disp_incr
    real(kind=8):: coef_upda_geom
!
! --------------------------------------------------------------------------------------------------
!
    call jevech('PCONFR', 'L', jpcf)
    call jevech('PGEOMER', 'L', jv_geom)
    call jevech('PDEPL_P', 'L', jv_disp_incr)
    call jevech('PDEPL_M', 'L', jv_disp)
!
! - Smooth normals ?
!
    norm_smooth = int(zr(jpcf-1+1))
!
! - Coefficient to update geometry
!
    if (algo_reso_geom.eq.1) then
        coef_upda_geom = 1.d0
    else
        coef_upda_geom = 0.d0
    endif            
!
! - Get updated coordinates
!
    call lcreac(nb_lagr       , indi_lagc      , elem_dime   , coef_upda_geom,&
                nb_node_slav  , nb_node_mast   ,&
                jv_geom       , jv_disp        , jv_disp_incr,&
                elem_slav_coor, elem_mast_coor)
!
end subroutine  

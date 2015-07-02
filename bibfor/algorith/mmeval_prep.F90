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

!
implicit none
!
#include "asterc/r8prem.h"
#include "asterf_types.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfdist.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/mmvalp.h"
#include "asterfort/mmvalp_scal.h"
#include "asterfort/mcopco.h"
#include "asterfort/mmnorm.h"
#include "asterfort/utmess.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/mmnewj.h"
#include "asterfort/mmmjev.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
! aslint: disable=W1504
!
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
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Continue method - Compute gap and contact pressure
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  time_curr        : current time
! In  model_ndim       : size of model
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  sdcont_solv      : name of contact solving datastructure
! In  l_speed          : .true. if speed scheme
! In  speed_field      : name of field for speed
! In  i_zone           : index of contact zone
! In  ksipc1           : first parametric coordinate of contact point in slave element
! In  ksipc2           : second parametric coordinate of contact point in slave element
! In  ksipr1           : first parametric coordinate of projection of contact point in master elem.
! In  ksipr2           : second parametric coordinate of projection of contact point in master elem.
! In  tau1             : first tangent vector for local basis
! In  tau1             : second tangent vector for local basis
! In  elem_slav_indx   : index of slave element (in contact datastructure)
! In  elem_slav_nume   : index of slave element (in mesh datastructure)
! In  elem_slav_nbno   : number of nodes of slave element
! In  elem_slav_type   : type of slave element
! In  elem_slav_coor   : coordinates of slave element
! In  elem_mast_nume   : index of master element (in mesh datastructure)
! In  lagr_cont_node   : value of contact lagrangian at (slave) nodes
! Out norm             : normal vector for local basis
! Out gap              : contact gap
! Out gap_user         : contact gap defined by user
! Out gap_speed        : contact gap for speed
! Out lagr_cont_poin   : value of contact lagrangian at contact point
!
! --------------------------------------------------------------------------------------------------
!
    integer :: node_slav_indx
    real(kind=8) :: noor
    real(kind=8) :: poin_slav_coor(3), poin_proj_coor(3)
    real(kind=8) :: speed_mast_poin(3), speed_slav_poin(3)
    character(len=19) :: newgeo
    character(len=8) :: elem_mast_name
!
! --------------------------------------------------------------------------------------------------
!
    node_slav_indx = 0
    newgeo         = sdcont_solv(1:14)//'.NEWG'
!
! - Coordinates of the contact point 
!
    call mmvalp(model_ndim, elem_slav_type, elem_slav_nbno, 3, ksipc1,&
                ksipc2    , elem_slav_coor, poin_slav_coor)
!
! - Coordinates of the projection of contact point 
!
    call mcopco(mesh  , newgeo        , model_ndim, elem_mast_nume, ksipr1,&
                ksipr2, poin_proj_coor)
!
! - Local basis on master element
!
    call mmnorm(model_ndim, tau1, tau2, norm, noor)
    if (noor .le. r8prem()) then
        call jenuno(jexnum(mesh//'.NOMMAI', elem_mast_nume), elem_mast_name)
        call utmess('F', 'CONTACT3_23', sk=elem_mast_name, nr=3, valr=poin_proj_coor)
    endif
!
! - Compute gap
!
    call mmnewj(model_ndim, poin_slav_coor, poin_proj_coor, norm, gap)
!
! - Get user gap
!
    call cfdist(sdcont_defi   , 'CONTINUE', i_zone, node_slav_indx, elem_slav_indx,&
                poin_slav_coor, gap_user, time_curr)
!
! - Interpolate contact pressure (Lagrange) at point
!
    call mmvalp_scal(model_ndim    , elem_slav_type, elem_slav_nbno, ksipc1, ksipc2,&
                     lagr_cont_node, lagr_cont_poin)
!
! - For speed schemes
!
    if (l_speed) then
        call mcopco(mesh  , speed_field    , model_ndim, elem_slav_nume, ksipc1,&
                    ksipc2, speed_slav_poin)
        call mcopco(mesh  , speed_field    , model_ndim, elem_mast_nume, ksipr1,&
                    ksipr2, speed_mast_poin)
        call mmmjev(model_ndim, norm, speed_slav_poin, speed_mast_poin, gap_speed)
    endif
!
end subroutine

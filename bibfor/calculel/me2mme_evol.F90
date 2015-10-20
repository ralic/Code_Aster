subroutine me2mme_evol(model_    , cara_elem_, mate_      , nharm    , base_    ,&
                       i_load    , load_name , ligrel_calc, inst_prev, inst_curr,&
                       inst_theta, resu_elem , vect_elem)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/load_neum_prep.h"
#include "asterfort/load_neum_evcd.h"
#include "asterfort/inical.h"
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
!
    character(len=*), intent(in) :: model_
    character(len=*), intent(in) :: cara_elem_
    character(len=*), intent(in) :: mate_
    integer, intent(in) :: nharm
    character(len=*), intent(in) :: base_
    integer, intent(in) :: i_load
    character(len=8), intent(in) :: load_name
    character(len=19), intent(in) :: ligrel_calc
    real(kind=8), intent(in) :: inst_prev 
    real(kind=8), intent(in) :: inst_curr
    real(kind=8), intent(in) :: inst_theta 
    character(len=19), intent(inout) :: resu_elem
    character(len=19), intent(in) :: vect_elem
!
! --------------------------------------------------------------------------------------------------
!
! CALC_VECT_ELEM
! 
! EVOL_CHAR loads
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_in_maxi, nbout
    parameter (nb_in_maxi = 42, nbout = 1)
    character(len=8) :: lpain(nb_in_maxi), lpaout(nbout)
    character(len=19) :: lchin(nb_in_maxi), lchout(nbout)
!
    integer :: nb_in_prep
    character(len=1) :: stop, base
    character(len=24) :: model, cara_elem, mate
!
! --------------------------------------------------------------------------------------------------
!
    stop      = 'S'
    base      = base_
    model     = model_
    cara_elem = cara_elem_
    mate      = mate_
!
! - Init fields
!
    call inical(nb_in_maxi, lpain, lchin, nbout, lpaout,&
                lchout)
!
! - Preparing input fields
!
    call load_neum_prep(model    , cara_elem , mate      , 'Dead'      , inst_prev,&
                        inst_curr, inst_theta, nb_in_maxi, nb_in_prep  , lchin    ,&
                        lpain    , nharm = nharm)
!
! - Compute composite dead Neumann loads (EVOL_CHAR)
!
    call load_neum_evcd(stop      , inst_prev , load_name, i_load, ligrel_calc,&
                        nb_in_maxi, nb_in_prep, lpain    , lchin , base       ,&
                        resu_elem , vect_elem)

end subroutine

subroutine verstp(model   , lload_name, lload_info, mate     , time     ,&
                  compor  , temp_prev , temp_iter , hydr_prev, hydr_curr,&
                  dry_prev, dry_curr  , varc_curr , vect_elem)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/calcul.h"
#include "asterfort/corich.h"
#include "asterfort/gcnco2.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/reajre.h"
#include "asterfort/load_neut_prep.h"
#include "asterfort/load_neut_comp.h"
#include "asterfort/hydr_resi.h"
#include "asterfort/inical.h"
#include "asterfort/load_list_info.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: lload_name
    character(len=24), intent(in) :: lload_info
    character(len=24), intent(in) :: time
    character(len=24), intent(in) :: mate
    character(len=24), intent(in) :: temp_prev
    character(len=24), intent(in) :: temp_iter
    character(len=24), intent(in) :: hydr_prev   
    character(len=24), intent(in) :: hydr_curr
    character(len=24), intent(in) :: dry_prev   
    character(len=24), intent(in) :: dry_curr
    character(len=24), intent(in) :: compor
    character(len=19), intent(in) :: varc_curr    
    character(len=24), intent(inout) :: vect_elem
!
! --------------------------------------------------------------------------------------------------
!
! Thermic - Loads
! 
! Neumann loads elementary vectors (residuals)
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of the model
! In  mate             : name of material characteristics (field)
! In  lload_name       : name of object for list of loads name
! In  lload_info       : name of object for list of loads info
! In  time             : time (<CARTE>)
! In  temp_prev        : previous temperature
! In  temp_iter        : temperature field at current Newton iteration
! In  hydr_prev        : previous hydratation
! In  hydr_curr        : current hydratation
! In  dry_prev         : previous drying
! In  dry_curr         : current drying
! In  compor           : name of comportment definition (field)
! In  varc_curr        : command variable for current time
! IO  vect_elem        : name of vect_elem result
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_in_maxi, nbout
    parameter (nb_in_maxi = 5, nbout = 2)
    character(len=8) :: lpain(nb_in_maxi), lpaout(nbout)
    character(len=19) :: lchin(nb_in_maxi), lchout(nbout)
!
    character(len=1) :: base, stop_calc
    character(len=8) :: load_name
    character(len=19) :: resu_elem
    integer :: load_nume
    aster_logical :: load_empty
    integer :: i_load, nb_load, nb_in_prep
    character(len=24), pointer :: v_load_name(:) => null()
    integer, pointer :: v_load_info(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
!
! - Initializations
!
    resu_elem   = '&&VERSTP.0000000'
    stop_calc   = 'S'
    base        = 'V'
!
! - Init fields
!
    call inical(nb_in_maxi, lpain, lchin, nbout, lpaout,&
                lchout)    
!
! - Loads
!
    call load_list_info(load_empty, nb_load   , v_load_name, v_load_info,&
                        lload_name, lload_info)
!
! - Hydratation vector
!
    call hydr_resi(model    , mate     , time     , compor    , temp_prev,&
                   temp_iter, hydr_prev, hydr_curr, dry_prev  , dry_curr ,&
                   varc_curr, vect_elem)
!
! - Preparing input fields
!
    call load_neut_prep(model, nb_in_maxi, nb_in_prep, lchin, lpain,&
                        temp_iter_ = temp_iter)
!
! - Computation
!
    do i_load = 1, nb_load
        load_name = v_load_name(i_load)(1:8)
        load_nume = v_load_info(nb_load+i_load+1)
        if (load_nume .gt. 0) then
            call load_neut_comp('RESI'   , stop_calc , model     , time , load_name,&
                                load_nume, nb_in_maxi, nb_in_prep, lpain, lchin    ,&
                                base     , resu_elem , vect_elem )
        endif
    end do
!
end subroutine

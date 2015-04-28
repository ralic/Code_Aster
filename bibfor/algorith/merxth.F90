subroutine merxth(model    , lload_name, lload_info, cara_elem, mate     ,&
                  time_curr, time      , temp_iter , compor   , varc_curr,&
                  dry_prev , dry_curr  , matr_elem)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/calcul.h"
#include "asterfort/ther_mtan.h"
#include "asterfort/gcnco2.h"
#include "asterfort/inical.h"
#include "asterfort/jeexin.h"
#include "asterfort/jedetr.h"
#include "asterfort/memare.h"
#include "asterfort/load_list_info.h"
#include "asterfort/load_neut_comp.h"
#include "asterfort/load_neut_prep.h"
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
    real(kind=8), intent(in) :: time_curr
    character(len=24), intent(in) :: time
    character(len=24), intent(in) :: mate
    character(len=24), intent(in) :: cara_elem
    character(len=24), intent(in) :: temp_iter
    character(len=24), intent(in) :: dry_prev   
    character(len=24), intent(in) :: dry_curr
    character(len=24), intent(in) :: compor
    character(len=19), intent(in) :: varc_curr
    character(len=24), intent(inout) :: matr_elem
!
! --------------------------------------------------------------------------------------------------
!
! Thermic
! 
! Tangent matrix (non-linear) - Volumic and surfacic terms
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of the model
! In  mate             : name of material characteristics (field)
! In  cara_elem        : name of elementary characteristics (field)
! In  lload_name       : name of object for list of loads name
! In  lload_info       : name of object for list of loads info
! In  time_curr        : current time
! In  time             : time (<CARTE>)
! In  compor           : name of comportment definition (field)
! In  temp_iter        : temperature field at current Newton iteration
! In  dry_prev         : previous drying
! In  dry_curr         : current drying
! In  varc_curr        : command variable for current time
! IO  matr_elem        : name of matr_elem result
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_in_maxi, nbout
    parameter (nb_in_maxi = 10, nbout = 1)
    character(len=8) :: lpain(nb_in_maxi), lpaout(nbout)
    character(len=19) :: lchin(nb_in_maxi), lchout(nbout)
!
    integer :: iret
    character(len=1) :: base, stop_calc
    character(len=8) :: load_name, newnom
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
    resu_elem   = '&&MERXTH.0000000'
    stop_calc   = 'S'
    base        = 'V'
!
! - Prepare MATR_ELEM
!
    call jeexin(matr_elem(1:19)//'.RELR', iret)
    if (iret .eq. 0) then
        call memare('V', matr_elem, model, mate, cara_elem,&
                    'MTAN_THER')
    else
        call jedetr(matr_elem(1:19)//'.RELR')
    endif
!
! - Generate new RESU_ELEM name
!
    newnom = resu_elem(10:16)
    call gcnco2(newnom)
    resu_elem(10:16) = newnom(2:8)
!
! - Tangent matrix - Volumic terms
!
    call ther_mtan(model    , mate    , time    , varc_curr, compor   ,&
                   temp_iter, dry_prev, dry_curr, resu_elem, matr_elem)
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
! - Preparing input fields
!
    call load_neut_prep(model, nb_in_maxi, nb_in_prep, lchin, lpain, &
                        varc_curr_ = varc_curr, temp_iter_ = temp_iter)
!
! - Computation
!
    do i_load = 1, nb_load
        load_name = v_load_name(i_load)(1:8)
        load_nume = v_load_info(nb_load+i_load+1)
        if (load_nume .gt. 0) then
            call load_neut_comp('MTAN'   , stop_calc, model     , time_curr , time ,&
                                load_name, load_nume, nb_in_maxi, nb_in_prep, lpain,&
                                lchin    , base     , resu_elem , matr_elem )
        endif
    end do
!
end subroutine

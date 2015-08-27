subroutine nmcore_swap(sderro, nume_inst, load_norm, load_mini, last_resi_conv,&
                       ds_conv)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/GetResi.h"
#include "asterfort/SetResi.h"
#include "asterfort/nmcrel.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=24), intent(in) :: sderro
    integer, intent(in) :: nume_inst
    real(kind=8), intent(in) :: load_norm
    real(kind=8), intent(in) :: load_mini
    real(kind=8), intent(in) :: last_resi_conv
    type(NL_DS_Conv), intent(inout) :: ds_conv
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Convergence management
!
! Swap convergence criterias if necessary
!
! --------------------------------------------------------------------------------------------------
!
! In  sderro           : name of datastructure for error management (events)
! In  nume_inst        : index of current time step
! In  load_norm        : norm of exterior loads
! In  last_resi_conv   : last norm of equilibrium residual when converged
! In  load_mini        : minimum value of exterior loads norm
! IO  ds_conv          : datastructure for convergence management
!
! --------------------------------------------------------------------------------------------------
!
    real(kind=8) :: resi_glob_maxi, resi_glob_rela, resi_comp_rela, swap_trig
    aster_logical :: l_rela, l_maxi, l_comp
    aster_logical :: l_swap_rela_maxi, l_swap_comp_rela
!
! --------------------------------------------------------------------------------------------------
!
    l_swap_rela_maxi = .false.
    l_swap_comp_rela = .false.
!
! - Get parameters
!
    call GetResi(ds_conv, type = 'RESI_GLOB_RELA' , user_para_ = resi_glob_rela,&
                 l_resi_test_ = l_rela)
    call GetResi(ds_conv, type = 'RESI_GLOB_MAXI' , user_para_ = resi_glob_maxi,&
                 l_resi_test_ = l_maxi)
    call GetResi(ds_conv, type = 'RESI_COMP_RELA' , user_para_ = resi_comp_rela,&
                 l_resi_test_ = l_comp)
!
! - Swap from RESI_COMP_RELA to RESI_GLOB_RELA if first step
!
    if (l_comp) then
        if (nume_inst .eq. 1) then
            l_rela           = .true._1
            l_comp           = .false._1
            l_swap_comp_rela = .true._1
            resi_glob_rela   = resi_comp_rela
            resi_glob_maxi   = resi_comp_rela
        endif
    endif
!
! - Swap from RESI_GLOB_RELA to RESI_GLOB_MAXI if exterior load vanish
!
    if (l_rela) then
        swap_trig = 1.d-6 * load_mini
        if (load_norm .le. swap_trig) then
            if (nume_inst .gt. 1) then
                l_rela           = .false._1
                l_maxi           = .true._1
                l_swap_rela_maxi = .true._1
                resi_glob_maxi   = last_resi_conv
            endif
            if (l_swap_comp_rela) then
                l_rela           = .false._1
                l_maxi           = .true._1
                l_swap_rela_maxi = .true._1
            endif
        endif
    endif
!
! - Set new active residuals and convergence parameters
!
    call SetResi(ds_conv, type_ = 'RESI_GLOB_RELA' , user_para_ = resi_glob_rela,&
                 l_resi_test_ = l_rela)
    call SetResi(ds_conv, type_ = 'RESI_GLOB_MAXI' , user_para_ = resi_glob_maxi,&
                 l_resi_test_ = l_maxi)
    call SetResi(ds_conv, type_ = 'RESI_COMP_RELA' , l_resi_test_ = l_comp)
!
! - Save events 
!
    call nmcrel(sderro, 'RESI_MAXR', l_swap_rela_maxi)
    call nmcrel(sderro, 'RESI_MAXN', l_swap_comp_rela)
!
end subroutine

subroutine nmimps(ds_print, ds_conv, sderro)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmerge.h"
#include "asterfort/GetResi.h"
#include "asterfort/utmess.h"
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
    type(NL_DS_Print), intent(in) :: ds_print
    type(NL_DS_Conv), intent(in) :: ds_conv
    character(len=24), intent(in) :: sderro
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Print management
!
! Print residuals summary at end of step
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_print         : datastructure for printing parameters
! In  ds_conv          : datastructure for convergence management
! In  sderro           : name of datastructure for error management (events)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_resi, nb_resi
    real(kind=8) :: valr(2)
    character(len=16) :: valk(2)
    aster_logical :: lprint, l_swap_rela_maxi, l_swap_comp_rela
!
! --------------------------------------------------------------------------------------------------
!
    nb_resi = ds_conv%nb_resi
!
! - Messages from convergence swapping
!
    call nmerge(sderro, 'RESI_MAXR', l_swap_rela_maxi)
    call nmerge(sderro, 'RESI_MAXN', l_swap_comp_rela)
!
! - Print for this step ?
!
    lprint = ds_print%l_print
!
! - Print residuals summary
!
    if (lprint) then
        call utmess('I', 'MECANONLINE6_60')
        if (l_swap_comp_rela) then
            call utmess('I', 'MECANONLINE6_61')
            call utmess('I', 'MECANONLINE2_96')
        endif
        if (l_swap_rela_maxi) then
            call utmess('I', 'MECANONLINE6_62')
            call GetResi(ds_conv, type = 'RESI_GLOB_MAXI' , user_para_ = valr(2))
            valr(1) = ds_conv%swap_trig
            call utmess('I', 'MECANONLINE2_98', nr=2, valr=valr)
        endif
        do i_resi = 1, nb_resi
            if (ds_conv%l_resi_test(i_resi)) then
                valk(1) = ds_conv%list_resi(i_resi)%type
                valk(2) = ds_conv%list_resi(i_resi)%locus_calc
                valr(1) = ds_conv%list_resi(i_resi)%vale_calc
                call utmess('I', 'MECANONLINE6_70', nk=2, valk=valk, sr=valr(1))
            endif
        end do
    endif
!
end subroutine

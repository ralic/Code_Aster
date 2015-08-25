subroutine nmimps(ds_print, sdconv, sderro)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmerge.h"
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
    character(len=24), intent(in) :: sdconv
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
! In  sdconv           : name of datastructure convergence
! In  sderro           : name of datastructure for error management (events)
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdconv_type
    character(len=24) :: sdconv_lieu
    character(len=24) :: sdconv_vale
    character(len=24) :: sdconv_acti
    character(len=16), pointer :: v_sdconv_type(:) => null()
    character(len=16), pointer :: v_sdconv_lieu(:) => null()
    real(kind=8), pointer :: v_sdconv_vale(:) => null()
    aster_logical, pointer :: v_sdconv_acti(:) => null()
    integer :: i_resi, iarg, nb_resi
    real(kind=8) :: valr
    character(len=16) :: valk(2)
    aster_logical :: lprint, maxrel, maxnod
!
! --------------------------------------------------------------------------------------------------
!
    sdconv_type = sdconv(1:19)//'.TYPE'
    sdconv_lieu = sdconv(1:19)//'.LIEU'
    sdconv_vale = sdconv(1:19)//'.VALE'
    sdconv_acti = sdconv(1:19)//'.ACTI'
    call jeveuo(sdconv_type, 'L', vk16 = v_sdconv_type)
    call jeveuo(sdconv_lieu, 'L', vk16 = v_sdconv_lieu)
    call jeveuo(sdconv_vale, 'L', vr = v_sdconv_vale)
    call jeveuo(sdconv_acti, 'L', vl = v_sdconv_acti)
    call jelira(sdconv_acti, 'LONMAX', ival=nb_resi)
!
! - Messages for convergence swapping
!
    call nmerge(sderro, 'RESI_MAXR', maxrel)
    call nmerge(sderro, 'RESI_MAXN', maxnod)
!
! - Print for this step ?
!
    lprint = ds_print%l_print
!
! - Print residuals summary
!
    if (lprint) then
        call utmess('I', 'MECANONLINE6_60')
        if (maxnod) then
            call utmess('I', 'MECANONLINE6_61')
        endif
        if (maxrel) then
            call utmess('I', 'MECANONLINE6_62')
        endif
        iarg = 0
        do i_resi = 1, nb_resi
            if (v_sdconv_acti(i_resi)) then
                iarg = iarg + 1
                valk(1) = v_sdconv_type(i_resi)
                valk(2) = v_sdconv_lieu(i_resi)
                valr    = v_sdconv_vale(i_resi)
                call utmess('I', 'MECANONLINE6_70', nk=2, valk=valk, sr=valr)
            endif
        end do
    endif
!
end subroutine

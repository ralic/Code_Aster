subroutine nmevim(ds_print, sddisc, sderro, loop_name)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmacto.h"
#include "asterfort/nmerge.h"
#include "asterfort/nmimpx.h"
#include "asterfort/nmlecv.h"
#include "asterfort/nmltev.h"
#include "asterfort/utdidt.h"
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
    character(len=24), intent(in) :: sderro
    character(len=19), intent(in) :: sddisc
    character(len=4), intent(in) :: loop_name
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Event management
!
! Print event messages
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_print         : datastructure for printing parameters
! In  sddisc           : datastructure for time discretization TEMPORELLE
! In  sderro           : name of datastructure for error management (events)
! In  loop_name        : name of loop
!                         'NEWT' - Newton loop
!                         'FIXE' - Fixed points loop
!                         'INST' - Step time loop
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: lacti, cvbouc, lerrei, l_sep_line, lldcbo
    integer :: i_echec_acti
    character(len=16) :: event_name, action
    integer :: ieven, zeven
    character(len=24) :: sderro_info
    character(len=24) :: sderro_eact
    character(len=24) :: sderro_eniv
    character(len=24) :: sderro_emsg
    integer, pointer :: v_sderro_info(:) => null()
    integer, pointer :: v_sderro_eact(:) => null()
    character(len=16), pointer :: v_sderro_eniv(:) => null()
    character(len=24), pointer :: v_sderro_emsg(:) => null()
    integer :: icode
    character(len=9) :: teven
    character(len=24) :: meven
!
! --------------------------------------------------------------------------------------------------
!
    call nmlecv(sderro, loop_name, cvbouc)
    call nmltev(sderro, 'ERRI', loop_name, lerrei)
    call nmerge(sderro, 'INTE_BORN', lldcbo)
!
! - Separator line to print ?
!
    l_sep_line = (.not.cvbouc.and..not.lerrei.and..not.lldcbo)
!
! - Access to error management datastructure
!
    sderro_info = sderro(1:19)//'.INFO'
    sderro_eact = sderro(1:19)//'.EACT'
    sderro_eniv = sderro(1:19)//'.ENIV'
    sderro_emsg = sderro(1:19)//'.EMSG'
    call jeveuo(sderro_info, 'L', vi = v_sderro_info)
    call jeveuo(sderro_eact, 'L', vi = v_sderro_eact)
    call jeveuo(sderro_eniv, 'L', vk16 = v_sderro_eniv)
    call jeveuo(sderro_emsg, 'L', vk24 = v_sderro_emsg)
    zeven = v_sderro_info(1)
!
! - Print event messages - Algorithm
!
    do ieven = 1, zeven
        icode = v_sderro_eact(ieven)
        teven = v_sderro_eniv(ieven)(1:9)
        meven = v_sderro_emsg(ieven)
        if ((teven(1:4).eq.'EVEN') .and. (icode.eq.1)) then
            if (meven .ne. ' ') then
                if (l_sep_line) then
                    call nmimpx(ds_print)
                endif
                if (meven .eq. 'MECANONLINE10_1') then
                    call utmess('I', 'MECANONLINE10_1')
                else if (meven.eq.'MECANONLINE10_2') then
                    call utmess('I', 'MECANONLINE10_2')
                else if (meven.eq.'MECANONLINE10_3') then
                    call utmess('I', 'MECANONLINE10_3')
                else if (meven.eq.'MECANONLINE10_4') then
                    call utmess('I', 'MECANONLINE10_4')
                else if (meven.eq.'MECANONLINE10_5') then
                    call utmess('I', 'MECANONLINE10_5')
                else if (meven.eq.'MECANONLINE10_6') then
                    call utmess('I', 'MECANONLINE10_6')
                else if (meven.eq.'MECANONLINE10_7') then
                    call utmess('I', 'MECANONLINE10_7')
                else if (meven.eq.'MECANONLINE10_8') then
                    call utmess('I', 'MECANONLINE10_8')
                else if (meven.eq.'MECANONLINE10_9') then
                    call utmess('I', 'MECANONLINE10_9')
                else if (meven.eq.'MECANONLINE10_10') then
                    call utmess('I', 'MECANONLINE10_10')
                else if (meven.eq.'MECANONLINE10_11') then
                    call utmess('I', 'MECANONLINE10_11')
                else if (meven.eq.'MECANONLINE10_12') then
                    call utmess('I', 'MECANONLINE10_12')
                else if (meven.eq.'MECANONLINE10_20') then
                    call utmess('I', 'MECANONLINE10_20')
                else if (meven.eq.'MECANONLINE10_24') then
                    call utmess('I', 'MECANONLINE10_24')
                else if (meven.eq.'MECANONLINE10_25') then
                    if (cvbouc .and. loop_name .eq. 'NEWT') then
                        call utmess('A', 'MECANONLINE10_25')
                    endif
                else
                    ASSERT(.false.)
                endif
            endif
        endif
    end do
!
! - Print event messages - User
!
    call nmacto(sddisc, i_echec_acti)
    lacti = i_echec_acti.gt.0
    if (lacti) then
        call utdidt('L', sddisc, 'ECHE', 'NOM_EVEN', index_ = i_echec_acti,&
                    valk_ = event_name)
        call utdidt('L', sddisc, 'ECHE', 'ACTION', index_ = i_echec_acti,&
                    valk_ = action)
        if (event_name .eq. 'COLLISION') then
            if (l_sep_line) then
                call nmimpx(ds_print)
            endif
            call utmess('I', 'MECANONLINE10_21')
        else if (event_name.eq.'INTERPENETRATION') then
            if (l_sep_line) then
                call nmimpx(ds_print)
            endif
            call utmess('I', 'MECANONLINE10_22')
        else if (event_name.eq.'DIVE_RESI') then
            if (l_sep_line) then
                call nmimpx(ds_print)
            endif
            call utmess('I', 'MECANONLINE10_23')
        else if (event_name.eq.'DELTA_GRANDEUR') then
            if (l_sep_line) then
                call nmimpx(ds_print)
            endif
            call utmess('I', 'MECANONLINE10_24')
        endif
    endif
!
end subroutine

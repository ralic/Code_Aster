subroutine nmobsv(meshz     , modelz, sddisc, sd_obsv  , nume_time,&
                  cara_elemz, matez , compor, varc_refe, valinc   ,&
                  sd_inout)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/diinst.h"
#include "asterfort/lobs.h"
#include "asterfort/nmobse.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmextd.h"
#include "asterfort/jeveuo.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=*), intent(in) :: meshz
    character(len=19), intent(in) :: sd_obsv
    integer, intent(in) :: nume_time
    character(len=19), intent(in) :: sddisc
    character(len=*), intent(in) :: cara_elemz
    character(len=*), intent(in) :: matez
    character(len=*), intent(in) :: modelz
    character(len=19), intent(in) :: compor
    character(len=*), intent(in) :: varc_refe
    character(len=19), intent(in) :: valinc(*)
    character(len=24), optional, intent(in) :: sd_inout
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Observation
!
! Make observation
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  sddisc           : datastructure for discretization
! In  sd_obsv          : datastructure for observation parameters
! In  nume_time        : index of time
! In  model            : name of model
! In  cara_elem        : name of datastructure for elementary parameters (CARTE)
! In  mate             : name of material characteristics (field)
! In  compor           : name of <CARTE> COMPOR
! In  varc_refe        : command variable for reference
! In  valinc           : hat variable for algorithm fields
! In  sd_inout         : datastructure for input/output parameters
!
! --------------------------------------------------------------------------------------------------
!
    real(kind=8) :: time
    aster_logical :: l_obsv
    character(len=19) :: disp_curr, strx_curr, varc_curr
    character(len=24) :: field_type
    character(len=19) :: field
    integer :: i_keyw_fact, nb_keyw_fact, i_field 
    character(len=14) :: sdextr_obsv
    character(len=24) :: extr_info, extr_field
    integer, pointer :: v_extr_info(:) => null()
    character(len=24), pointer :: v_extr_field(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    l_obsv = .false.
!
! - Current time
!
    time = diinst(sddisc, nume_time)
!
! - Observation ?
!
    call lobs(sd_obsv, nume_time, time, l_obsv)
!
! - Get fields
!
    call nmchex(valinc, 'VALINC', 'DEPPLU', disp_curr)
    call nmchex(valinc, 'VALINC', 'STRPLU', strx_curr)
    call nmchex(valinc, 'VALINC', 'COMPLU', varc_curr)
!
! - Make observation 
!
    if (l_obsv) then
        call nmobse(meshz     , sd_obsv  , time,&
                    cara_elemz, modelz   , matez    , compor, disp_curr,&
                    strx_curr , varc_curr, varc_refe)
    endif
!
! - Change fields after initial observation
!
    if (nume_time.eq.0) then
        sdextr_obsv = sd_obsv(1:14)
        extr_info   = sdextr_obsv(1:14)//'     .INFO'
        call jeveuo(extr_info, 'L', vi = v_extr_info)
        nb_keyw_fact = v_extr_info(1)
        if (nb_keyw_fact.ne.0) then
            extr_field = sdextr_obsv(1:14)//'     .CHAM'
            call jeveuo(extr_field, 'E', vk24 = v_extr_field)
        endif
        do i_keyw_fact = 1, nb_keyw_fact
            i_field      = v_extr_info(7+7*(i_keyw_fact-1)+7)
            i_field      = abs(i_field)
            field_type   = v_extr_field(4*(i_field-1)+1)
            call nmextd(field_type, sd_inout, field)
            v_extr_field(4*(i_field-1)+4) = field
        end do
    endif
!
end subroutine

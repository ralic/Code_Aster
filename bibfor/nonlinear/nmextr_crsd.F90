subroutine nmextr_crsd(sdextrz, nb_keyw_fact, nb_field, nb_field_comp)
!
implicit none
!
#include "asterfort/wkvect.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=*), intent(in) :: sdextrz
    integer, intent(in) :: nb_keyw_fact
    integer, intent(in) :: nb_field
    integer, intent(in) :: nb_field_comp
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Field extraction datastructure
!
! Create datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  sdextr           : name of datastructure for extraction
! In  nb_keyw_fact     : number of factor keyword to read extraction parameters
! In  nb_field         : total number of fields
! In  nb_field_comp    : number of fields to compute (not a default in nonlinear operator)
!
! --------------------------------------------------------------------------------------------------
!
    character(len=14) :: sdextr
    character(len=24) :: extr_info, extr_type, extr_flag, extr_field, extr_comp
    integer, pointer :: v_extr_info(:) => null()
    character(len=8), pointer :: v_extr_type(:) => null()
    aster_logical, pointer :: v_extr_flag(:) => null()
    character(len=24), pointer :: v_extr_field(:) => null()
    character(len=24), pointer :: v_extr_comp(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    sdextr = sdextrz
!
! - Create information vector
!
    extr_info = sdextr(1:14)//'     .INFO'
    call wkvect(extr_info, 'V V I', 7+7*nb_keyw_fact, vi = v_extr_info)
!
    if (nb_keyw_fact .ne. 0) then
!
! ----- Create extraction type vector
!
        extr_type = sdextr(1:14)//'     .EXTR'
        call wkvect(extr_type, 'V V K8', 4*nb_keyw_fact, vk8 = v_extr_type)
!
! ----- Create extraction flag vector
!
        extr_flag = sdextr(1:14)//'     .ACTI'
        call wkvect(extr_flag, 'V V L', nb_keyw_fact, vl = v_extr_flag)
    endif
!
! - Create extraction field vector
!
    if (nb_field .ne. 0) then
        extr_field = sdextr(1:14)//'     .CHAM'
        call wkvect(extr_field, 'V V K24', 4*nb_field, vk24 = v_extr_field)
    endif
!
! - Create extraction fields to compute (not a default in nonlinear operator)
!
    if (nb_field_comp .ne. 0) then
        extr_comp = sdextr(1:14)//'     .COMP'
        call wkvect(extr_comp, 'V V K24', 4*nb_field_comp, vk24 = v_extr_comp)
    endif
!
end subroutine

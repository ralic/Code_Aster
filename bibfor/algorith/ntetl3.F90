subroutine ntetl3(i_field, ds_inout, tempct)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/chpver.h"
#include "asterfort/nmetnc.h"
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
    integer, intent(in) :: i_field
    type(NL_DS_InOut), intent(inout) :: ds_inout
    real(kind=8), intent(in) :: tempct
!
! --------------------------------------------------------------------------------------------------
!
! THER_* - Input/output datastructure
!
! Read field for ETAT_INIT - Some checks
!
! --------------------------------------------------------------------------------------------------
!
! In  i_field          : field index
! IO  ds_inout         : datastructure for input/output management
! In  tempct           : initial temperature if constant
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iret
    character(len=24) :: field_type
    character(len=24) :: valk(2)
    character(len=24) :: algo_name, field_algo
    character(len=4) :: init_type, disc_type
    character(len=8) :: gran_name
    aster_logical :: l_acti
!
! --------------------------------------------------------------------------------------------------
!
!
! - Field to read ?
!
    if (ds_inout%field(i_field)%l_read_init) then
!
! ----- Name of field (type) in results datastructure
!
        field_type = ds_inout%field(i_field)%type
!
! ----- Name of field in algorithm
!
        algo_name  = ds_inout%field(i_field)%algo_name
        call nmetnc(algo_name, field_algo)
!
! ----- Spatial discretization of field
!
        disc_type  = ds_inout%field(i_field)%disc_type
!
! ----- Type of GRANDEUR of field
!
        gran_name  = ds_inout%field(i_field)%gran_name
!
! ----- Actual state of field
!
        init_type  = ds_inout%field(i_field)%init_type
!
! ----- Is field should been active ?
!
        l_acti     = ds_inout%l_field_acti(i_field)
!
! ----- Informations about field
!
        if (l_acti) then
            if (init_type .eq. ' ') then
                call utmess('F', 'ETATINIT_30', sk=field_type)
            else
                valk(1) = field_type
                valk(2) = ds_inout%result
                if (init_type .eq. 'ZERO') then
                    call utmess('I', 'ETATINIT_31', sk=field_type)
                else if (init_type.eq.'RESU') then
                    call utmess('I', 'ETATINIT_32', nk=2, valk=valk)
                else if (init_type.eq.'READ') then
                    call utmess('I', 'ETATINIT_33', sk=field_type)
                else if (init_type.eq.'STAT') then
                    call utmess('I', 'ETATINIT_34')
                else if (init_type.eq.'VALE') then
                    call utmess('I', 'ETATINIT_35', sr=tempct)
                else
                    ASSERT(.false.)
                endif
            endif
!
! --------- Check GRANDEUR and discretization
!
            if (gran_name .ne. ' ') then
                call chpver('F', field_algo, disc_type, gran_name, iret)
            endif
        endif
    endif
!
end subroutine

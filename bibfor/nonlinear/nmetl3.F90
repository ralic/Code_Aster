subroutine nmetl3(model, compor, i_field, ds_inout)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/chpver.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmetnc.h"
#include "asterfort/nmsigi.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
#include "asterfort/vrcomp.h"
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
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: compor
    type(NL_DS_InOut), intent(in) :: ds_inout
    integer, intent(in) :: i_field
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Input/output datastructure
!
! Read field for ETAT_INIT - Some checks
!
! --------------------------------------------------------------------------------------------------
!
! In  compor           : name of <CARTE> COMPOR
! In  model            : name of model
! In  i_field          : field index
! In  ds_inout         : datastructure for input/output management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iret
    character(len=24) :: field_type, algo_name
    character(len=4) :: disc_type
    character(len=8) :: gran_name
    character(len=24) :: valk(2)
    character(len=24) :: field_algo
    character(len=24) :: ligrmo, compom
    character(len=4) :: init_type
    aster_logical :: l_state_init, l_stin_evol, l_acti
    character(len=8) :: stin_evol
    integer :: init_nume
!
! --------------------------------------------------------------------------------------------------
!
    call dismoi('NOM_LIGREL', model, 'MODELE', repk=ligrmo)
!
! - Get parameters
!
    l_stin_evol  = ds_inout%l_stin_evol
    stin_evol    = ds_inout%stin_evol
    init_nume    = ds_inout%init_nume
    l_state_init = ds_inout%l_state_init
!
! - Field to read ?
!
    if (ds_inout%field(i_field)%l_read_init) then
!
! ----- Name of field (type) in results datastructure
!
        field_type     = ds_inout%field(i_field)%type
!
! ----- Name of field in algorithm
!
        algo_name      = ds_inout%field(i_field)%algo_name
        call nmetnc(algo_name, field_algo)
!
! ----- Spatial discretization of field
!
        disc_type      = ds_inout%field(i_field)%disc_type
!
! ----- Name of field in algorithm
!
        algo_name      = ds_inout%field(i_field)%algo_name
        call nmetnc(algo_name, field_algo)
!
! ----- Actual state of field
!
        init_type      = ds_inout%field(i_field)%init_type
!
! ----- Type of GRANDEUR of field
!
        gran_name      = ds_inout%field(i_field)%gran_name
!
! ----- Is field should been active ?
!
        l_acti         = ds_inout%l_field_acti(i_field)
!
! ----- Informations about field
!
        if (l_acti) then
            if (init_type .eq. ' ') then
                call utmess('F', 'ETATINIT_30', sk=field_type)
            else
                valk(1) = field_type
                valk(2) = stin_evol
                if (init_type .eq. 'ZERO') then
                    call utmess('I', 'ETATINIT_31', sk=field_type)
                else if (init_type.eq.'RESU') then
                    call utmess('I', 'ETATINIT_32', nk=2, valk=valk)
                else if (init_type.eq.'READ') then
                    call utmess('I', 'ETATINIT_33', sk=field_type)
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
!
! --------- For pre-stressed load
!
            if (field_type .eq. 'SIEF_ELGA') then
                call nmsigi(ligrmo, compor, field_algo(1:19))
            endif
!
! --------- Check internal variables
!
            if (field_type .eq. 'VARI_ELGA') then
                if (l_state_init) then
                    compom = ' '
                    if (l_stin_evol) then
                        call rsexch(' ', stin_evol, 'COMPORTEMENT', init_nume, compom,&
                                    iret)
                        if (iret .ne. 0) compom = ' '
                    endif
                    if (compom .eq. ' ') then
                        call vrcomp(compor, field_algo, ligrmo, iret)
                    else
                        call vrcomp(compor, field_algo, ligrmo, iret, compor_prev = compom)
                    endif
                    if (iret .eq. 1) then
                        call utmess('F', 'MECANONLINE5_2')
                    endif
                endif
            endif
        else
            if (init_type .eq. 'RESU' .or. init_type .eq. 'READ') then
                call utmess('I', 'ETATINIT_36', sk=field_type)
            endif
        endif
    endif
!
end subroutine

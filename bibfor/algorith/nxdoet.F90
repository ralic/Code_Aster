subroutine nxdoet(model, nume_dof, l_stat, ds_inout)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmdoin.h"
#include "asterfort/nmetl1.h"
#include "asterfort/nmetl2.h"
#include "asterfort/nmetnc.h"
#include "asterfort/ntetl3.h"
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
!
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: nume_dof
    type(NL_DS_InOut), intent(inout) :: ds_inout
    aster_logical, intent(out) :: l_stat
!
! --------------------------------------------------------------------------------------------------
!
! THER_* - Init
!
! Read initial state
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of model
! In  nume_dof         : name of nume_dof object (numbering equation)
! IO  ds_inout         : datastructure for input/output management
! Out l_stat           : .true. if stationnary
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=8) :: calcri
    character(len=24) :: stin_evol, field_type, algo_name, field_algo
    integer :: nb_field, i_field, neq, init_nume
    real(kind=8) :: temp_init, init_time
    aster_logical :: l_stin_evol, l_state_init, l_field_read, l_init_stat, l_init_vale, l_reuse
    real(kind=8), pointer :: vale(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
! - Initializations
!
    call dismoi('NB_EQUA', nume_dof, 'NUME_DDL', repi=neq)
!
! - Model can compute rigidity ?
!
    call dismoi('CALC_RIGI', model, 'MODELE', repk=calcri)
    if (calcri .ne. 'OUI') then
        call utmess('F', 'CALCULEL2_65', sk=model)
    endif
!
! - Parameters from input/output datastructure
!
    nb_field     = ds_inout%nb_field
    l_reuse      = ds_inout%l_reuse
    l_init_stat  = ds_inout%l_init_stat
    l_init_vale  = ds_inout%l_init_vale
    temp_init    = ds_inout%temp_init
    l_state_init = ds_inout%l_state_init
!
! - PAS D'ETAT INITIAL EN PRESENCE D'UN CONCEPT REENTRANT
!
    if (l_state_init) then
        if (niv .ge. 2) then
            write (ifm,*) '<THERNONLINE> LECTURE ETAT INITIAL'
        endif
    else
        if (l_reuse) then
            call utmess('A', 'ETATINIT_1')
        else
            call utmess('I', 'ETATINIT_20')
        endif
    endif
!
! - Get name of result datastructure in ETAT_INIT
!
    l_stin_evol = ds_inout%l_stin_evol
    stin_evol   = ds_inout%stin_evol
!
! - Initial storing index and time
!
    call nmdoin(ds_inout)
    init_time = ds_inout%init_time
    init_nume = ds_inout%init_nume
!
! - Print
!
    if (niv .ge. 2) then
        write (ifm,*) '<THERNONLINE> ... INSTANT INITIAL'
        if (init_nume.eq.-1) then
            write (ifm,*) '<THERNONLINE> ...... NON DEFINI PAR ETAT_INIT'
        else
            write (ifm,*) '<THERNONLINE> ...... VALEUR    : ',init_time
            write (ifm,*) '<THERNONLINE> ...... NUME_ORDRE: ',init_nume
        endif
    endif
!
! - No initial state => stationnary
!
    if (.not.l_state_init) then
        l_stat    = .true.
        goto 99
    endif
!
! - Loop on fields
!
    do i_field = 1, nb_field
!
! ----- Read field for ETAT_INIT
!
        if (l_stin_evol) then
!
! --------- Initial temperature: from results datastructure
!
            call nmetl1(i_field, ds_inout)
        else
!
! --------- Name of field (type) in results datastructure
!
            field_type = ds_inout%field(i_field)%type
!
! --------- Name of field in algorithm
!
            algo_name  = ds_inout%field(i_field)%algo_name
            call nmetnc(algo_name, field_algo)
!
! --------- Informations about initial state
!
            l_field_read  = ds_inout%l_field_read(i_field)
            if (field_type .eq. 'TEMP') then
                call jeveuo(field_algo(1:19)//'.VALE', 'E', vr=vale)
!
! ------------- Initial temperature: from field
!
                if (l_field_read) then
                    call nmetl2(i_field, ds_inout)
                endif
!
! ------------- Initial temperature: stationnary computation
!
                if (l_init_stat) then
                    l_stat = .true.
                    ds_inout%field(i_field)%init_type = 'STAT'
                endif
!
! ------------- Initial temperature: constant
!
                if (l_init_vale) then
                    vale(1:neq) = temp_init
                    ds_inout%field(i_field)%init_type = 'VALE'
                endif
            else
                call nmetl2(i_field, ds_inout)
            endif
        endif
!
! ----- Read field for ETAT_INIT - Some checks
!
        call ntetl3(i_field, ds_inout, temp_init)
    end do
!
 99 continue
!
    call jedema()
!
end subroutine

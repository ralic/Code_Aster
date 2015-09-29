subroutine nmetl1(i_field, ds_inout)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/nmetnc.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcopy.h"
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
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Input/output datastructure
!
! Read field for ETAT_INIT - From results datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  i_field          : field index
! IO  ds_inout         : datastructure for input/output management
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: stin_evol
    integer :: ievol, iret, init_nume
    character(len=24) :: valk(2)
    character(len=24) :: field_resu, field_algo
    character(len=16) :: field_type
    character(len=24) :: algo_name, init_name
    character(len=4) :: disc_type
!
! --------------------------------------------------------------------------------------------------
!
    field_resu = '&&NMETL1.CHAMP'
!
! - Get parameters
!
    stin_evol = ds_inout%stin_evol
    init_nume = ds_inout%init_nume
!
! - Field to read ?
!
    if (ds_inout%l_field_acti(i_field).and.ds_inout%field(i_field)%l_read_init) then
!
! ----- Name of field (type) in results datastructure
!
        field_type = ds_inout%field(i_field)%type
!
! ----- Name of field for initial state
!
        init_name  = ds_inout%field(i_field)%init_name
!
! ----- Spatial discretization of field
!
        disc_type  = ds_inout%field(i_field)%disc_type
!
! ----- Name of field in algorithm
!
        algo_name  = ds_inout%field(i_field)%algo_name
        call nmetnc(algo_name, field_algo)
!
! ----- Get field in resultats datastructure
!
        call rsexch(' '  , stin_evol, field_type, init_nume, field_resu,&
                    ievol)
!
! ----- Copy field
!
        if (ievol .eq. 0) then
            if (disc_type .eq. 'NOEU') then
                call vtcopy(field_resu, field_algo, ' ', iret)
                if (iret .ne. 0) then
                    valk(1) = field_resu
                    valk(2) = field_algo
                    call utmess('A', 'MECANONLINE_2', nk=2, valk=valk)
                endif
            elseif ((disc_type.eq.'ELGA').or.&
                    (disc_type.eq.'ELNO').or.&
                    (disc_type.eq.'ELEM')) then
                call copisd('CHAMP_GD', 'V', field_resu, field_algo)
            else
                write(6,*) 'LOCCHA: ',disc_type
                ASSERT(.false.)
            endif
            ds_inout%field(i_field)%init_type = 'RESU'
        else
            if (init_name .ne. ' '.and.ds_inout%field(i_field)%init_type.eq.' ') then
                call copisd('CHAMP', 'V', init_name, field_algo)
                ds_inout%field(i_field)%init_type = 'ZERO'
            endif
        endif
    endif
!
end subroutine

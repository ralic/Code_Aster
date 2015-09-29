subroutine nmetl2(i_field, ds_inout)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmetcv.h"
#include "asterfort/nmetnc.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcopy.h"
#include "asterfort/xetco.h"
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
! Read field for ETAT_INIT - Field by field
!
! --------------------------------------------------------------------------------------------------
!
! In  i_field          : field index
! IO  ds_inout         : datastructure for input/output management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iret
    aster_logical :: l_field_read
    character(len=24) :: valk(3)
    character(len=24) :: field_read, field_read_cv, field_algo
    character(len=24) :: field_type
    character(len=4) :: init_type, disc_type
    character(len=24) :: algo_name, field_disc_in, init_name
!
! --------------------------------------------------------------------------------------------------
!
    field_read_cv = '&&NMETL2.CHAMP.CONVER'
!
! - Field to read ?
!
    if (ds_inout%l_field_acti(i_field).and.ds_inout%field(i_field)%l_read_init) then
!
! ----- Name of field (type) in results datastructure
!
        field_type     = ds_inout%field(i_field)%type
!
! ----- Name of field for initial state
!
        init_name      = ds_inout%field(i_field)%init_name
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
        init_type     = ds_inout%field(i_field)%init_type
!
! ----- Informations about field read in ETAT_INIT
!
        field_read    = ds_inout%field(i_field)%field_read
        l_field_read  = ds_inout%l_field_read(i_field)
!
! ----- Read initial field
!
        if (l_field_read) then
!
! --------- Discretization of input field
!
            call dismoi('TYPE_CHAMP', field_read, 'CHAMP', repk=field_disc_in, arret='C', ier=iret)
            if (iret .eq. 1) then
                call utmess('F', 'ETATINIT_50', sk=field_read)
            endif
!
! --------- Try to convert field (discretization) if necessary and copy it
!
            if (field_type.eq.'COHE_ELEM') then
                call xetco(field_read, field_algo, init_name)
            else
                call nmetcv(init_name, field_read, field_disc_in, field_read_cv, disc_type)
                if (disc_type .eq. 'NOEU') then
                    call vtcopy(field_read_cv, field_algo, ' ', iret)
                    if (iret .ne. 0) then
                        valk(1) = field_read_cv
                        valk(2) = field_algo
                        call utmess('A', 'MECANONLINE_2', nk=2, valk=valk)
                    endif
                else if ((disc_type.eq.'ELGA').or.(disc_type.eq.'ELEM').or.&
                         (disc_type.eq.'ELNO')) then
                    call copisd('CHAMP_GD', 'V', field_read_cv, field_algo)
                else
                    write(6,*) 'DISCRETISATION NON TRAITEE: ',field_disc_in
                    ASSERT(.false.)
                endif
            endif
!
! --------- New state of field
!
            ds_inout%field(i_field)%init_type = 'READ'
        endif
!
! ----- Copy initial field
!
        if (.not.l_field_read) then
            if (init_name .ne. ' '.and.ds_inout%field(i_field)%init_type.eq.' ') then
                call copisd('CHAMP', 'V', init_name, field_algo)
                ds_inout%field(i_field)%init_type = 'ZERO'
            endif
        endif
    endif
!
    call detrsd('CHAMP', field_read_cv)
!
end subroutine

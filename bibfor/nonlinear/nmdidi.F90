subroutine nmdidi(ds_inout, model , list_load, nume_dof, valinc,&
                  veelem  , veasse)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/rsexch.h"
#include "asterfort/vecdid.h"
#include "asterfort/assvec.h"
#include "asterfort/nmchex.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    type(NL_DS_InOut), intent(in) :: ds_inout
    character(len=24), intent(in) :: model
    character(len=19), intent(in) :: list_load
    character(len=24), intent(in) :: nume_dof
    character(len=19), intent(in) :: valinc(*)
    character(len=19), intent(in) :: veelem(*)
    character(len=19), intent(in) :: veasse(*)
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Computation
!
! Compute vector for DIDI loads
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_inout         : datastructure for input/output management
! In  model            : name of the model
! In  list_load        : name of datastructure for list of loads
! In  nume_dof         : name of numbering (NUME_DDL)
! In  valinc           : hat variable for algorithm fields
! In  veelem           : hat variable for elementary vectors
! In  veasse           : hat variable for vectors
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iret, didi_nume
    character(len=19) :: disp_didi, disp_prev, vect_elem, vect_asse
!
! --------------------------------------------------------------------------------------------------
!
    call nmchex(valinc, 'VALINC', 'DEPMOI', disp_prev)
    call nmchex(veelem, 'VEELEM', 'CNDIDI', vect_elem)
    call nmchex(veasse, 'VEASSE', 'CNDIDI', vect_asse)
    disp_didi = disp_prev
!
! - Get displacement field
!
    didi_nume = ds_inout%didi_nume
    if ((didi_nume.ge.0) .and. (ds_inout%l_stin_evol)) then
        call rsexch(' ', ds_inout%stin_evol, 'DEPL', didi_nume, disp_didi, iret)
        if (iret .ne. 0) then
            call utmess('F', 'MECANONLINE5_20', sk=ds_inout%stin_evol)
        endif
    endif
!
! - Compute elementary vectors
!
    call vecdid(model, list_load, disp_didi, vect_elem)
!
! - Assembly
!
    call assvec('V', vect_asse, 1, vect_elem, [1.d0],&
                nume_dof, ' ', 'ZERO', 1)
!
end subroutine

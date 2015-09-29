subroutine CreateInOutDS(phenom, ds_inout)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/gnomsd.h"
#include "asterfort/CreateInOutDS_M.h"
#include "asterfort/CreateInOutDS_T.h"
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
    character(len=4), intent(in) :: phenom
    type(NL_DS_InOut), intent(out) :: ds_inout
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Input/output management
!
! Create input/output datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  phenom           : phenomenon (MECA/THER/ACOU)
! Out ds_inout         : datastructure for input/output management
!
! --------------------------------------------------------------------------------------------------
!
    character(len=19) :: list_load_resu
    character(len=24) :: noobj
!
! --------------------------------------------------------------------------------------------------
!
    ds_inout%result            = ' '
    ds_inout%stin_evol         = ' '
    ds_inout%l_stin_evol       = .false._1
    ds_inout%l_state_init      = .false._1
    ds_inout%l_reuse           = .false._1
    ds_inout%didi_nume         = -1
    ds_inout%criterion         = ' '
    ds_inout%precision         = r8vide()
    ds_inout%user_time         = r8vide()
    ds_inout%user_nume         = 0
    ds_inout%stin_time         = r8vide()
    ds_inout%l_stin_time       = .false._1
    ds_inout%l_user_time       = .false._1
    ds_inout%l_user_nume       = .false._1
    ds_inout%init_time         = r8vide()
    ds_inout%init_nume         = -1
    ds_inout%l_init_stat       = .false._1
    ds_inout%l_init_vale       = .false._1
    ds_inout%temp_init         = r8vide()
!
! - Generate name of list of loads saved in results datastructure
!
    noobj = '12345678'//'.1234'//'.EXCIT'
    call gnomsd(' ', noobj, 10, 13)
    list_load_resu = noobj(1:19)
    ds_inout%list_load_resu = list_load_resu
!
! - Specific parameters
!
    if (phenom.eq.'MECA') then
        call CreateInOutDS_M(ds_inout)
    elseif (phenom.eq.'THER') then
        call CreateInOutDS_T(ds_inout)
    else
        ASSERT(.false.)
    endif
!
end subroutine


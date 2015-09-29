subroutine nmextt(ds_inout, field_type, field_disc)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/nmetob.h"
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
    type(NL_DS_InOut), intent(in) :: ds_inout
    character(len=24), intent(in) :: field_type
    character(len=4), intent(out) :: field_disc
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Field extraction datastructure
!
! Get localization of field (discretization: NOEU or ELGA)
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_inout         : datastructure for input/output management
! In  field_type       : type of field (name in results datastructure)
! Out field_disc       : localization of field (discretization: NOEU or ELGA)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_field
!
! --------------------------------------------------------------------------------------------------
!
    field_disc = 'XXXX'
!
! - Index of field
!
    call nmetob(ds_inout, field_type, i_field)
!
! - Get localization of field
!
    field_disc = ds_inout%field(i_field)%disc_type
!
! - Check
!
    ASSERT(field_disc.eq.'NOEU'.or.field_disc.eq.'ELGA')

end subroutine

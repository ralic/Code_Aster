subroutine nmextd(field_type, ds_inout, field_algo)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/nmetnc.h"
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
    character(len=*), intent(in) :: field_type
    type(NL_DS_InOut), intent(in) :: ds_inout
    character(len=*), intent(out) :: field_algo
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Extraction (OBSERVATION/SUIVI_DDL) utilities 
!
! Get field
!
! --------------------------------------------------------------------------------------------------
!
! In  field_type       : name of field (type) in results datastructure
! In  ds_inout         : datastructure for input/output management
! Out field_algo       : name of datastructure for field
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: algo_name
    integer :: i_field_obsv
!
! --------------------------------------------------------------------------------------------------
!
!
! - Get index of field used for OBSERVATION
!
    call nmetob(ds_inout, field_type, i_field_obsv)
!
! - Get name of datastructure for field
!
    if (i_field_obsv.ne.0) then
        algo_name  = ds_inout%field(i_field_obsv)%algo_name
        call nmetnc(algo_name, field_algo)
    endif
!
end subroutine

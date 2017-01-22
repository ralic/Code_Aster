subroutine romAlgoNLTableSave(nume_store, time_curr, ds_algorom)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/jeveuo.h"
#include "asterfort/romTableSave.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer, intent(in) :: nume_store
    real(kind=8), intent(in) :: time_curr
    type(ROM_DS_AlgoPara), intent(in) :: ds_algorom
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction - Solving non-linear problem
!
! Save table for the reduced coordinates
!
! --------------------------------------------------------------------------------------------------
!
! In  nume_store       : index to store in results
! In  time_curr        : current time
! In  ds_algorom       : datastructure for ROM parameters
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: tabl_name = ' ', gamma = ' '
    integer :: nb_mode 
    real(kind=8), pointer :: v_gamma(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    tabl_name  = ds_algorom%tabl_name
    gamma      = ds_algorom%gamma
    nb_mode    = ds_algorom%ds_empi%nb_mode
!
! - Access to reduced coordinates
!
    call jeveuo(gamma, 'L', vr = v_gamma)
!
! - Save in table
!
    call romTableSave(tabl_name , nb_mode  , v_gamma  ,&
                      nume_store, time_curr)
!
end subroutine

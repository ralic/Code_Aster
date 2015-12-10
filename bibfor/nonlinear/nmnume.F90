subroutine nmnume(model   , result, compor, list_load, ds_contact,&
                  nume_dof, sdnume)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/nmprof.h"
#include "asterfort/nuendo.h"
#include "asterfort/nunuco.h"
#include "asterfort/nurota.h"
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
    character(len=8), intent(in) :: result
    character(len=24), intent(in) :: compor
    character(len=19), intent(in) :: list_load
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=24), intent(out) :: nume_dof
    character(len=19), intent(in) :: sdnume
!
! --------------------------------------------------------------------------------------------------
!
! Non-linear algorithm - Initializations
!
! Create information about numbering
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of model datastructure
! In  result           : name of result datastructure (EVOL_NOLI)
! In  compor           : name of <CARTE> COMPOR
! In  list_load        : list of loads
! In  ds_contact       : datastructure for contact management
! Out nume_dof         : name of numbering object (NUME_DDL)
! In  sdnume           : name of dof positions datastructure
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdnuro, sdnuen, sdnuco
!
! --------------------------------------------------------------------------------------------------
!

!
! - Create numbering 
!
    call nmprof(model               , result, list_load, nume_dof,&
                ds_contact%iden_rela)
!
! - Get position of large rotation dof
!
    sdnuro = sdnume(1:19)//'.NDRO'
    call nurota(model, nume_dof, compor, sdnuro)
!
! - Get position of damaged dof 
!
    sdnuen = sdnume(1:19)//'.ENDO'
    call nuendo(model, nume_dof, sdnuen)
!
! - Get position of contact dof 
!
    sdnuco = sdnume(1:19)//'.NUCO'
    if (ds_contact%l_form_cont) then
        call nunuco(nume_dof, sdnuco)
    endif  
!
end subroutine

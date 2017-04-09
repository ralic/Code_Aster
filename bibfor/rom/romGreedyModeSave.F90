subroutine romGreedyModeSave(ds_multipara, ds_empi,&
                             i_mode      , mode   ,&
                             ds_solveDOM )
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
#include "asterfort/infniv.h"
#include "asterfort/romMultiParaProdModeSave.h"
#include "asterfort/romMultiParaModeSave.h"
#include "asterfort/romModeProd.h"
#include "asterfort/jeveuo.h"
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
    type(ROM_DS_MultiPara), intent(in) :: ds_multipara
    type(ROM_DS_Empi), intent(inout)   :: ds_empi
    integer, intent(in)                :: i_mode
    character(len=19), intent(in)      :: mode
    type(ROM_DS_Solve), intent(in)     :: ds_solveDOM
!
! --------------------------------------------------------------------------------------------------
!
! Greedy algorithm
!
! Save empiric modes
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_multipara     : datastructure for multiparametric problems
! IO  ds_empi          : datastructure for empiric modes
! In  i_mode           : index of empiric mode
! In  mode             : empiric mode to save
! In  ds_solveDOM      : datastructure for datastructure to solve systems (DOM)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    complex(kind=8), pointer :: v_modec(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM5_64')
    endif
!
! - Save mode
!
    call romMultiParaModeSave(ds_multipara, ds_empi              ,&
                              i_mode      , ds_solveDOM%syst_solu)
!
! - Compute products
!
    call jeveuo(mode(1:19)//'.VALE', 'L', vc = v_modec)
    call romModeProd(ds_multipara%nb_matr  ,&
                     ds_multipara%matr_name,&
                     ds_multipara%matr_type,&
                     ds_multipara%prod_mode,&
                     'C' , v_modec = v_modec)
!
! - Save products
!
    call romMultiParaProdModeSave(ds_multipara, ds_empi,&
                                  i_mode)
!
end subroutine

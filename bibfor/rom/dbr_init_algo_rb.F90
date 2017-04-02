subroutine dbr_init_algo_rb(nb_mode, ds_para_rb)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/infniv.h"
#include "asterfort/utmess.h"
#include "asterfort/romGreedyAlgoInit.h"
#include "asterfort/romSolveDOMSystCreate.h"
#include "asterfort/romSolveROMSystCreate.h"
#include "asterfort/romMultiParaSystEvalType.h"
#include "asterfort/romMultiParaInit.h"
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
    integer, intent(in) :: nb_mode
    type(ROM_DS_ParaDBR_RB), intent(inout) :: ds_para_rb
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_BASE_REDUITE - Initializations
!
! Init algorithm - For RB methods
!
! --------------------------------------------------------------------------------------------------
!
! In  nb_mode          : number of empiric modes
! IO  ds_para_rb       : datastructure for parameters (RB)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: nb_vari_coef
    character(len=1) :: syst_matr_type, syst_2mbr_type, syst_type
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM2_41')
    endif
!
! - Evaluate type of system
!
    call romMultiParaSystEvalType(ds_para_rb%multipara,&
                                  syst_matr_type, syst_2mbr_type, syst_type)
!
! - Create objects to solve system (DOM)
!
    call romSolveDOMSystCreate(syst_matr_type, syst_2mbr_type, syst_type,&
                               ds_para_rb%multipara%matr_name(1),&
                               ds_para_rb%solveDOM)
!
! - Create objects to solve system (ROM)
!
    call romSolveROMSystCreate(syst_matr_type, syst_2mbr_type, syst_type,&
                               nb_mode,&
                               ds_para_rb%solveROM)
!
! - Initializations for multiparametric problems
!
    call romMultiParaInit(ds_para_rb%multipara)
!
! - Init algorithm
!
    nb_vari_coef = ds_para_rb%multipara%nb_vari_coef
    call romGreedyAlgoInit(syst_type , nb_mode, nb_vari_coef,&
                           ds_para_rb%solveDOM%vect_zero, ds_para_rb)
!
end subroutine

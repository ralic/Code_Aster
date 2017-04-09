subroutine dbr_main_rb(nb_mode_maxi, ds_para_rb, ds_empi)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
#include "asterfort/infniv.h"
#include "asterfort/romEvalCoef.h"
#include "asterfort/romMultiParaDOMMatrCreate.h"
#include "asterfort/romMultiParaDOM2mbrCreate.h"
#include "asterfort/romSolveDOMSystSolve.h"
#include "asterfort/romNormalize.h"
#include "asterfort/romMultiParaCoefCompute.h"
#include "asterfort/romGreedyResiInit.h"
#include "asterfort/romGreedyModeSave.h"
#include "asterfort/romGreedyResiCalc.h"
#include "asterfort/romGreedyResiMaxi.h"
#include "asterfort/romGreedyResi.h"
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
    integer, intent(in) :: nb_mode_maxi
    type(ROM_DS_ParaDBR_RB), intent(inout) :: ds_para_rb
    type(ROM_DS_Empi), intent(inout) :: ds_empi
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_BASE_REDUITE - Compute
!
! Main subroutine to compute empiric modes - For RB methods
!
! --------------------------------------------------------------------------------------------------
!
! In  nb_mode_maxi     : maximum number of empiric modes
! IO  ds_para_rb       : datastructure for parameters (RB)
! IO  ds_empi          : datastructure for empiric modes
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: i_mode, i_coef_maxi, i_coef
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM5_60')
    endif
!
! - Initializations
!
    i_mode     = 1
    i_coef     = 1
!
! - First mode
!
    if (niv .ge. 2) then
        call utmess('I', 'ROM5_92')
    endif
!
! - Evaluation of initial coefficients
!
    call romEvalCoef(ds_para_rb%multipara, l_init = .true._1)
!
! - Create initial second member
!
    call romMultiParaDOM2mbrCreate(ds_para_rb%multipara, &
                                   i_coef,&
                                   ds_para_rb%solveDOM%syst_2mbr_type,&
                                   ds_para_rb%vect_2mbr_init)
!
! - Create initial matrix
!
    call romMultiParaDOMMatrCreate(ds_para_rb%multipara,&
                                   i_coef,&
                                   ds_para_rb%solveDOM%syst_matr)
!
! - Solve initial system (DOM)
!
    ds_para_rb%solveDOM%syst_2mbr = ds_para_rb%vect_2mbr_init(1:19)
    call romSolveDOMSystSolve(ds_para_rb%solver, ds_para_rb%solveDOM)
!
! - Normalization of mode
!
    call romNormalize(ds_para_rb%solveDOM%syst_type, ds_para_rb%solveDOM%syst_solu, ds_empi%nb_equa)
!
! - Save
!
    call romGreedyModeSave(ds_para_rb%multipara, ds_empi,&
                           i_mode              , ds_para_rb%solveDOM%syst_solu,&
                           ds_para_rb%solveDOM)
!
! - Compute initial residual
!
    call romGreedyResiInit(ds_para_rb)
!
! - Loop on modes
!
    do i_mode = 2, nb_mode_maxi
! ----- Print
        if (niv .ge. 2) then
            call utmess('I', 'ROM5_61', si = i_mode)
        endif
! ----- Compute reduced coefficients
        call romMultiParaCoefCompute(ds_empi                           ,& 
                                     ds_para_rb%multipara              ,&
                                     ds_para_rb%solveDOM%syst_2mbr_type,&
                                     ds_para_rb%solveDOM%syst_2mbr     ,&
                                     ds_para_rb%solveROM               ,&
                                     i_mode-1, i_mode-1                ,&
                                     ds_para_rb%coef_redu)
! ----- Compute residual
        call romGreedyResiCalc(ds_empi, ds_para_rb, i_mode-1)
! ----- Find maximum
        call romGreedyResiMaxi(ds_para_rb, i_coef_maxi)
! ----- Compute reduced coefficients
        call romMultiParaCoefCompute(ds_empi                           ,& 
                                     ds_para_rb%multipara              ,&
                                     ds_para_rb%solveDOM%syst_2mbr_type,&
                                     ds_para_rb%solveDOM%syst_2mbr     ,&
                                     ds_para_rb%solveROM               ,&
                                     i_mode-1, i_mode-1                ,&
                                     ds_para_rb%coef_redu              ,&
                                     i_coef_ = i_coef_maxi)
! ----- Compute residual for one coefficient (is second member)
        call romGreedyResi(ds_empi, ds_para_rb, i_mode-1, i_mode-1, i_coef_maxi)
! ----- Create matrix
        call romMultiParaDOMMatrCreate(ds_para_rb%multipara,&
                                       i_coef_maxi,&
                                       ds_para_rb%solveDOM%syst_matr)
! ----- Solve system (DOM)
        ds_para_rb%solveDOM%syst_2mbr = ds_para_rb%resi_vect(1:19)
        call romSolveDOMSystSolve(ds_para_rb%solver, ds_para_rb%solveDOM)
! ----- Normalization of mode
        call romNormalize(ds_para_rb%solveDOM%syst_type,&
                          ds_para_rb%solveDOM%syst_solu, ds_empi%nb_equa)
! ----- Save
        call romGreedyModeSave(ds_para_rb%multipara, ds_empi,&
                               i_mode              , ds_para_rb%solveDOM%syst_solu,&
                               ds_para_rb%solveDOM)
        ds_para_rb%solveDOM%syst_2mbr = ds_para_rb%vect_2mbr_init(1:19)
    end do
!
end subroutine

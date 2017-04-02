subroutine romEvalCoefInit(nb_vari_para, vari_para, ds_evalcoef)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
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
    integer, intent(in) :: nb_vari_para
    type(ROM_DS_VariPara), intent(in) :: vari_para(5)
    type(ROM_DS_EvalCoef), intent(inout) :: ds_evalcoef
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction - Initializations
!
! Initialisation for evaluation of coefficients
!
! --------------------------------------------------------------------------------------------------
!
! In  nb_vari_para     : number of variables 
! In  vari_para        : datastructure for multiparametric problems - Variations
! IO  ds_evalcoef      : datastructure for multiparametric problems - Evaluation
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_vari_para, i_eval_para
!
! --------------------------------------------------------------------------------------------------
!
    do i_vari_para = 1, nb_vari_para
        i_eval_para = i_vari_para
        ds_evalcoef%para_name(i_eval_para) = vari_para(i_vari_para)%para_name
    end do
    ds_evalcoef%nb_para = nb_vari_para
!
end subroutine

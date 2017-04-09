subroutine romEvalCoef(ds_multipara, l_init, i_mode_coef_, i_coef_)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/utmess.h"
#include "asterfort/romCoefInfo.h"
#include "asterfort/romEvalCoefFunc.h"
#include "asterfort/romEvalCoefPrep.h"
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
    type(ROM_DS_MultiPara), intent(inout) :: ds_multipara
    aster_logical, intent(in) :: l_init
    integer, optional, intent(in) :: i_mode_coef_
    integer, optional, intent(in) :: i_coef_
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction
!
! Evaluation of coefficients
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_multipara     : datastructure for multiparametric problems
! In  l_init           : .true. if first evaluation
! In  i_mode_coef      : index of mode to compute coefficients
! In  i_coef           : index of coefficient
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: i_matr, nb_matr, i_coef_list, nb_vari_coef, i_coef, i_mode_coef
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        if (l_init) then
            call utmess('I', 'ROM5_93')
        else
            call utmess('I', 'ROM5_44', si = i_coef_)
        endif
    endif
!
! - Get parameters
!
    nb_vari_coef = ds_multipara%nb_vari_coef
!
! - Get index in global list of coefficients
!
    if (l_init) then
        i_coef      = 1
        i_mode_coef = 0
        i_coef_list = 0
    else
        ASSERT(present(i_coef_))
        ASSERT(present(i_mode_coef_))
        i_coef      = i_coef_
        i_mode_coef = i_mode_coef_
        i_coef_list = nb_vari_coef*(i_mode_coef-1) + i_coef
    endif
!
! - Prepare EVALCOEF datastructure
!
    call romEvalCoefPrep(i_coef_list, ds_multipara)
!
! - Evaluate coefficients for matrix
!
    nb_matr = ds_multipara%nb_matr
    do i_matr = 1, nb_matr
        call romEvalCoefFunc(ds_multipara%evalcoef, ds_multipara%matr_coef(i_matr), i_coef)
        if (niv .ge. 2) then
            call romCoefInfo('M',&
                             ds_multipara%matr_name(i_matr),&
                             i_coef,&
                             ds_multipara%matr_coef(i_matr))
        endif
    end do
!
! - Evaluate coefficients for second member
!
    call romEvalCoefFunc(ds_multipara%evalcoef, ds_multipara%vect_coef, i_coef_list)
    if (niv .ge. 2) then
        call romCoefInfo('V',&
                         ds_multipara%vect_name,&
                         i_coef,&
                         ds_multipara%vect_coef)
    endif
!
end subroutine

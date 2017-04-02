subroutine romMultiParaInfo(ds_multipara)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
#include "asterfort/romVariParaInfo.h"
#include "asterfort/romMultiCoefInfo.h"
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
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction
!
! Informations about multiparametric problems
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_multipara     : datastructure for multiparametric problems
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_matr, i_vari_para
    integer :: nb_matr, nb_vari_para
!
! --------------------------------------------------------------------------------------------------
!
!
! - For matrix
!
    nb_matr = ds_multipara%nb_matr
    call utmess('I', 'ROM3_30', si = nb_matr)
    do i_matr = 1, nb_matr
        if (ds_multipara%matr_type(i_matr) .eq. 'R') then
            call utmess('I', 'ROM3_31', sk = ds_multipara%matr_name(i_matr), si = i_matr)
        elseif (ds_multipara%matr_type(i_matr) .eq. 'C') then
            call utmess('I', 'ROM3_32', sk = ds_multipara%matr_name(i_matr), si = i_matr)          
        else
 
        endif
        call romMultiCoefInfo(ds_multipara%matr_coef(i_matr))
    end do
!
! - For second member
!
    if (ds_multipara%vect_type .eq. 'R') then
        call utmess('I', 'ROM3_33', sk = ds_multipara%vect_name)
    elseif (ds_multipara%vect_type .eq. 'C') then
        call utmess('I', 'ROM3_34', sk = ds_multipara%vect_name)          
    else
        ASSERT(.false.)
    endif
    call romMultiCoefInfo(ds_multipara%vect_coef)
!
! - Global system type
!
    if (ds_multipara%syst_type .eq. 'R') then
        call utmess('I', 'ROM3_35')
    elseif (ds_multipara%syst_type .eq. 'C') then
        call utmess('I', 'ROM3_36')          
    else
        ASSERT(.false.)
    endif
!
! - Pour la variation des coefficients
! 
    call utmess('I', 'ROM3_45')
    call utmess('I', 'ROM3_46', sk = ds_multipara%type_vari_coef)
    call utmess('I', 'ROM3_47', si = ds_multipara%nb_vari_coef)
    nb_vari_para = ds_multipara%nb_vari_para
    call utmess('I', 'ROM3_48', si = nb_vari_para)
    do i_vari_para = 1, nb_vari_para
        call romVariParaInfo(ds_multipara%vari_para(i_vari_para))
    end do  
!
end subroutine

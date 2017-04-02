subroutine romMultiParaProdModeInit(ds_multipara)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcrem.h"
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
! Initializations for products matrix x mode
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_multipara     : datastructure for multiparametric problems
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: i_matr, nb_matr
    character(len=24) :: prod_mode
    character(len=8) :: matr_name
    character(len=1) :: matr_type, prod_type, syst_type
    aster_logical :: l_coefm_cplx
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM2_34')
    endif
!
! - Get parameters
!
    nb_matr   = ds_multipara%nb_matr
    syst_type = ds_multipara%syst_type
!
! - Prepare product [Matrix] x [Mode]
!
    do i_matr = 1, nb_matr
        prod_mode    = ds_multipara%prod_mode(i_matr)
        matr_name    = ds_multipara%matr_name(i_matr)
        matr_type    = ds_multipara%matr_type(i_matr)
        l_coefm_cplx = ds_multipara%matr_coef(i_matr)%l_cplx
        prod_type    = 'R'
        if (matr_type .eq. 'C' .or. l_coefm_cplx .or. syst_type .eq. 'C') then
            prod_type    = 'C'
        endif
        call vtcrem(prod_mode, matr_name, 'V', prod_type)
    end do
!
end subroutine

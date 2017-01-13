subroutine romMultiParaDSInit(ds_multipara)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterc/r8vide.h"
#include "asterfort/infniv.h"
#include "asterfort/utmess.h"
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
    type(ROM_DS_MultiPara), intent(out) :: ds_multipara
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction - Initializations
!
! Initialisation of datastructure for multiparametric problems
!
! --------------------------------------------------------------------------------------------------
!
! Out ds_multipara     : datastructure for multiparametric problems
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM5_43')
    endif
!
    ds_multipara%syst_type       = ' '
    ds_multipara%nb_matr         = 0
    ds_multipara%matr_name(:)    = ' '
    ds_multipara%matr_type(:)    = ' '
    ds_multipara%l_coefm_cplx(:) = .false.
    ds_multipara%l_coefm_real(:) = .false.
    ds_multipara%coefm_cplx(:)   = (0.d0,0.d0)
    ds_multipara%coefm_real(:)   = 0.d0
    ds_multipara%vect_name       = ' '
    ds_multipara%vect_type       = ' '
    ds_multipara%l_coefv_cplx    = .false.
    ds_multipara%l_coefv_real    = .false.
    ds_multipara%coefv_cplx      = (0.d0,0.d0)
    ds_multipara%coefv_real      = 0.d0
    ds_multipara%prod_mode(1)    = '&&OP0053.PRODMODE_1'
    ds_multipara%prod_mode(2)    = '&&OP0053.PRODMODE_2'
    ds_multipara%prod_mode(3)    = '&&OP0053.PRODMODE_3'
    ds_multipara%prod_mode(4)    = '&&OP0053.PRODMODE_4'
    ds_multipara%prod_mode(5)    = '&&OP0053.PRODMODE_5'
    ds_multipara%prod_mode(6)    = '&&OP0053.PRODMODE_6'
    ds_multipara%prod_mode(7)    = '&&OP0053.PRODMODE_7'
    ds_multipara%prod_mode(8)    = '&&OP0053.PRODMODE_8'
!
end subroutine

subroutine romMultiParaClean(ds_multipara)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterfort/romMultiCoefClean.h"
#include "asterfort/romVariParaClean.h"
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
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction
!
! Clean datastructure for multiparametric problems
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_multipara     : datastructure for multiparametric problems
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_matr, nb_matr, i_vari_para, nb_vari_para
!
! --------------------------------------------------------------------------------------------------
!
    nb_matr = ds_multipara%nb_matr
    do i_matr = 1, nb_matr
        call romMultiCoefClean(ds_multipara%matr_coef(i_matr))
    end do
    call romMultiCoefClean(ds_multipara%vect_coef)
    nb_vari_para = ds_multipara%nb_vari_para
    do i_vari_para = 1, nb_vari_para
        call romVariParaClean(ds_multipara%vari_para(i_vari_para))
    end do
!
end subroutine

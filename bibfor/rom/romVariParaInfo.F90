subroutine romVariParaInfo(ds_varipara)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
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
    type(ROM_DS_VariPara), intent(in) :: ds_varipara
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction
!
! Variation of parameters for multiparametric problems - Print informations
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_varipara      : datastructure for multiparametric problems - Variations
!
! --------------------------------------------------------------------------------------------------
!
    call utmess('I', 'ROM3_50', sk = ds_varipara%para_name)
    call utmess('I', 'ROM3_51', si = ds_varipara%nb_vale_para)
    call utmess('I', 'ROM3_52', sr = ds_varipara%para_init)
!
end subroutine

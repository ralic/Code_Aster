subroutine megeom(modelz, chgeoz)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=*), intent(in) :: modelz
    character(len=*), intent(inout) :: chgeoz
!
! --------------------------------------------------------------------------------------------------
!
! Prepare geometry field
!
! --------------------------------------------------------------------------------------------------
!
! In  model  : name of model
! IO  chgeom : name geometry field
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: chgeom
    character(len=8) :: model
    character(len=8), pointer :: p_model_lgrf(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    model = modelz
!
    ASSERT(model.ne.' ')
    call jeveuo(model//'.MODELE    .LGRF', 'L', vk8 = p_model_lgrf)
    chgeom = p_model_lgrf(1)//'.COORDO'
!
    chgeoz = chgeom
end subroutine

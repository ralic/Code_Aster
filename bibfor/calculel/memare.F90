subroutine memare(base  , matr_vect_elemz, modelz, mate, cara_elem,&
                  suropt)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/jedetr.h"
#include "asterfort/wkvect.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=1), intent(in) :: base
    character(len=*), intent(in) :: matr_vect_elemz
    character(len=*), intent(in) :: modelz
    character(len=*), intent(in) :: mate
    character(len=*), intent(in) :: cara_elem
    character(len=*), intent(in) :: suropt
!
! --------------------------------------------------------------------------------------------------
!
! RESU_ELEM Management
!
! Create RERR object for matr_elem or vect_elem
!
! --------------------------------------------------------------------------------------------------
!
! In  base           : JEVEUX basis
! In  matr_vect_elem : name of matr_elem or vect_elem
! In  modelz         : name of model
! In  mate           : name of material characteristics (field)         
! In  cara_elem      : name of elementary characteristics (field)
! In  suropt         : name of "SUR_OPTION"
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: model
    character(len=19) :: matr_vect_elem
    character(len=24), pointer :: p_rerr(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    matr_vect_elem = matr_vect_elemz
    model          = modelz
    ASSERT(model.ne.' ')
!
    call jedetr(matr_vect_elem//'.RERR')
    call wkvect(matr_vect_elem//'.RERR', base//' V K24', 5, vk24 = p_rerr)
    p_rerr(1) = model
    p_rerr(2) = suropt
    p_rerr(3) = 'NON_SOUS_STRUC'
    p_rerr(4) = mate
    p_rerr(5) = cara_elem
!
end subroutine

subroutine messtr(base      , option_, model_, cara_elem_, mate_,&
                  matr_elem_)
!
implicit none
!
#include "jeveux.h"
#include "asterf_types.h"
#include "asterfort/memare.h"
#include "asterfort/jeveuo.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=1), intent(in) :: base
    character(len=*), intent(in) :: model_
    character(len=*), intent(in) :: cara_elem_
    character(len=*), intent(in) :: mate_
    character(len=*), intent(in) :: option_
    character(len=*), intent(in) :: matr_elem_
!
    character(len=24), pointer :: v_rerr(:) => null()
!
    call memare(base, matr_elem_, model_, mate_, cara_elem_,&
                option_)
    call jeveuo(matr_elem_//'.RERR', 'E', vk24=v_rerr)
    v_rerr(3) = 'OUI_SOUS_STRUC'
!
end subroutine

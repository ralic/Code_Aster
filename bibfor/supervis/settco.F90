!> Register the supervisor type name in the result itself.
!
!> The type name is the name of the Python class in uppercases.
!>
!> Only the datastructures created by calling an opXXXX subroutine are registered
!> by the supervisor (see E_ETAPE.Exec).
!
!> @param[in]  name     name of the datastructure
!> @param[in]  typeco   name of the supervisor type.
!
subroutine settco(name, typeco)
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mathieu.courtois@edf.fr
!
    implicit none
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: typeco
!
#include "jeveux.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: name8
    character(len=16) :: attr
    character(len=24) :: type24
    integer :: iret
    character(len=24), pointer :: vk(:) => null()
!
    name8 = name
    type24 = typeco
    attr = name8//'._TYPCO_'
    call jeexin(attr, iret)
    if (iret .eq. 0) then
        call wkvect(attr, 'G V K24', 1, vk24=vk)
    else
        call jeveuo(attr, 'E', vk24=vk)
    endif
    vk(1) = type24
!
end subroutine settco

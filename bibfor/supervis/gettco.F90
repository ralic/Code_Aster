!> Returns the supervisor type name of the result.
!
!> The type name is the name of the Python class in uppercases.
!>
!> It returns ' ' if the datastructure has not been registered.
!
!> @param[in]  name     name of the datastructure
!> @param[out] typeco   name of the supervisor type.
!
subroutine gettco(name, typeco)
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
    character(len=*), intent(out) :: typeco
!
#include "jeveux.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: name8
    character(len=16) :: attr
    integer :: iret
    character(len=24), pointer :: vk(:) => null()
!
    name8 = name
    attr = name8//'._TYPCO_'
    call jeexin(attr, iret)
    if (iret .eq. 0) then
        typeco = ' '
    else
        call jeveuo(attr, 'L', vk24=vk)
        typeco = vk(1)
    endif
!
end subroutine gettco

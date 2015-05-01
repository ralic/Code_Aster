subroutine nmvcd2(name_varcz, matez, exis_varc)
!
    implicit none
!
#include "asterf_types.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=*), intent(in) :: name_varcz
    character(len=*), intent(in) :: matez
    aster_logical, intent(out) :: exis_varc
!
! --------------------------------------------------------------------------------------------------
!
! Command variables
!
! Is command variable exists ?
!
! --------------------------------------------------------------------------------------------------
!
! In  name_varc : name of command variable
! In  mate      : name of material field
! Out exis_varc : .true. if this command variable has been affected
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nmax,  i, iret
    character(len=8) :: mate
    character(len=8) :: name_varc
    character(len=8), pointer :: cvrcvarc(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
    mate      = matez
    name_varc = name_varcz
    exis_varc = .false.
!
    call jeexin(mate// '.CVRCVARC', iret)
    if (iret .ne. 0) then
        call jelira(mate// '.CVRCVARC', 'LONMAX', ival=nmax)
        call jeveuo(mate// '.CVRCVARC', 'L'     , vk8 =cvrcvarc)
        do i = 1, nmax
            if (cvrcvarc(i) .eq. name_varc) then
                exis_varc=.true.
                goto 2
            endif
        end do
 2      continue
    endif
!
    call jedema()
end subroutine

subroutine op0034()
!
implicit none
!
#include "asterc/getres.h"
#include "asterfort/charth.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
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
! person_in_charge: mickael.abbas at edf.fr
!

!
! --------------------------------------------------------------------------------------------------
!
! COMMAND:  AFFE_CHAR_THER_*
!
! --------------------------------------------------------------------------------------------------
!
    character(len=4) :: vale_type
    character(len=8) :: load
    character(len=16) :: command, k16dummy
    character(len=8), pointer :: p_load_type(:) => null() 
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
!
! - Which command ?
!
    call getres(load, k16dummy, command)
    if (command .eq. 'AFFE_CHAR_THER') then
        vale_type = 'REEL'
    else if (command .eq. 'AFFE_CHAR_THER_F') then
        vale_type = 'FONC'
    endif
!
! - Load type
!
    call wkvect(load//'.TYPE', 'G V K8', 1, vk8 = p_load_type)
    if (vale_type .eq. 'REEL') then
        p_load_type(1) = 'THER_RE '
    else if (vale_type .eq. 'FONC') then
        p_load_type(1) = 'THER_FO '
    endif
!
! - Loads treatment
!
    call charth(load, vale_type)
!
    call jedema()
end subroutine

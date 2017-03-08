subroutine varc_prep(chmate)
!
use calcul_module, only : ca_jvcnom_, ca_nbcvrc_
!
implicit none
!
#include "jeveux.h"
#include "asterc/ismaem.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveut.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: chmate
!
! --------------------------------------------------------------------------------------------------
!
! Material - External state variables (VARC)
!
! Preparation (for calcul_module)
!
! --------------------------------------------------------------------------------------------------
!
! In  chmate           : name of material field (CHAM_MATER)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iret
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
    call jeexin(chmate//'.CVRCNOM', iret)
    if (iret .ne. 0) then
        call jeveut(chmate//'.CVRCNOM', 'L', ca_jvcnom_)
        call jelira(chmate//'.CVRCNOM', 'LONMAX', ca_nbcvrc_)
    else
        ca_nbcvrc_ = 0
        ca_jvcnom_ = ismaem()
    endif
!
    call jedema()
end subroutine

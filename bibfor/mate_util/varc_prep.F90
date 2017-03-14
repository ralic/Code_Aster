subroutine varc_prep(chmate, l_thm)
!
use calcul_module, only : ca_jvcnom_, ca_nbcvrc_, ca_ctempl_, ca_ctempr_, ca_ctempm_, ca_ctempp_
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/ismaem.h"
#include "asterc/r8nnem.h"
#include "asterc/indik8.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveut.h"
#include "asterfort/utmess.h"
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
    aster_logical, intent(in) :: l_thm
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
! In  l_thm            : .true. if THM
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iret, varc_indx
    character(len=8) :: varc_name
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    varc_name = 'TEMP'
!
    call jeexin(chmate//'.CVRCNOM', iret)
    if (iret .ne. 0) then
        call jeveut(chmate//'.CVRCNOM', 'L', ca_jvcnom_)
        call jelira(chmate//'.CVRCNOM', 'LONMAX', ca_nbcvrc_)
        ca_ctempl_ = 0
    else
        ca_nbcvrc_ = 0
        ca_jvcnom_ = ismaem()
        ca_ctempl_ = 0
    endif
!
! - For coupled problems (THM)
!
    if (l_thm) then
        if (ca_nbcvrc_ .eq. 0) then
            ca_ctempl_ = 1
            ca_ctempr_ = r8nnem()
            ca_ctempm_ = r8nnem()
            ca_ctempp_ = r8nnem()
        else
            varc_indx = indik8(zk8(ca_jvcnom_), varc_name, 1, ca_nbcvrc_)
            if (varc_indx .ne. 0) then
                call utmess('F', 'MATERIAL2_51')
            endif
            ca_ctempl_ = 1
            ca_ctempr_ = r8nnem()
            ca_ctempm_ = r8nnem()
            ca_ctempp_ = r8nnem()
        endif
    endif
!
    call jedema()
end subroutine

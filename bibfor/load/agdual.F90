subroutine agdual(load, nbliai, type_liai)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/juveca.h"
#include "asterfort/jeexin.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_load: jacques.pellet at edf.fr
!
    character(len=8), intent(in) :: load
    integer, intent(in) :: nbliai
    character(len=*), intent(in) :: type_liai
!
! --------------------------------------------------------------------------------------------------
!
! Loads affectation
!
! Allocate/Extend char_dual datastructure for a load
!
! --------------------------------------------------------------------------------------------------
!
! In  load        : name of load
! In  nbliai      : number of linear relations "packets"
! In  type_liai   : "type" of relations : 'LIN' /'?'
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8), pointer :: rctyr(:) => null()
    character(len=8), pointer :: rcnom(:) => null()
    integer, pointer :: nmata(:) => null()
    integer :: nbliai_max, n1,n2,iexi
    character(len=13) :: chdual
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    ASSERT(type_liai.eq.'LIN' .or. type_liai.eq.'?')
    ASSERT(nbliai.ge.1)
    chdual=load//'.DUAL'
!
!   -- faut-il creer la SD ?
!   --------------------------
!
    call jeexin(chdual//'.RCTYR', iexi)
    if (iexi.eq.0) then
        nbliai_max=max(nbliai,30)
        call wkvect(load//'.DUAL.RCTYR', 'G V K8', nbliai_max, vk8 = rctyr)
        call wkvect(load//'.DUAL.NMATA', 'G V I', 2*nbliai_max, vi = nmata)
        call wkvect(load//'.DUAL.RCNOM', 'G V K8', 4*nbliai_max, vk8 = rcnom)
        call jeecra(chdual//'.RCTYR', 'LONUTI', ival=nbliai)
        rctyr(1:nbliai)=type_liai
        goto 999
    endif
!
!   -- faut-il agrandir la SD ?
!   ----------------------------
!
    call jelira(chdual//'.RCTYR', 'LONMAX', ival=n1)
    call jelira(chdual//'.RCTYR', 'LONUTI', ival=n2)
    if (n1.lt.(n2+nbliai)) then
        nbliai_max=2*(n2+nbliai)
        call juveca(load//'.DUAL.RCTYR',nbliai_max)
        call juveca(load//'.DUAL.NMATA',2*nbliai_max)
        call juveca(load//'.DUAL.RCNOM',4*nbliai_max)
    endif

    call jeveuo(chdual//'.RCTYR', 'E', vk8 = rctyr)
    call jeecra(chdual//'.RCTYR', 'LONUTI', ival=n2+nbliai)
    rctyr(n2+1:n2+nbliai)=type_liai
!
999 continue
    call jedema()

end subroutine

subroutine exresl(modatt, iparg, chin)

use calcul_module, only : ca_iachii_, ca_iachlo_, ca_iawlo2_, ca_igr_,&
     ca_iichin_, ca_ilchlo_, ca_nbelgr_, ca_nbgr_, ca_typegd_,&
     ca_lparal_, ca_paral_, ca_iel_

implicit none

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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr

#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/digde2.h"
#include "asterfort/jacopo.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"

    integer :: modatt, iparg
    character(len=19) :: chin
!----------------------------------------------------------------------
!     Entrees:
!        modatt : mode local attendu
!        iparg  : numero du parametre dans l'option
!        chin   : nom du champ global sur lequel on fait l'extraction
!----------------------------------------------------------------------
    integer :: desc, mode, ncmpel, iaux1, iaux2, iaux0, k
    integer :: jresl, debugr, lggrel
!----------------------------------------------------------------------


    call jemarq()

    lggrel=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+4)
    debugr=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+5)

    desc=zi(ca_iachii_-1+11*(ca_iichin_-1)+4)

    ASSERT(modatt.gt.0)
    mode=zi(desc-1+2+ca_igr_)

    if (mode .eq. 0) then
!       -- le resuelem n'existe pas sur le grel :
!          on sort sans mettre zl(ilchlo+debugr-1-1+k)=.true.
!          on aura alors toujours iret=3 avec tecach
        goto 999
    endif

    ASSERT(mode.eq.modatt)
    ncmpel=digde2(mode)
    ASSERT(lggrel.eq.ncmpel*ca_nbelgr_)
    call jeveuo(jexnum(chin//'.RESL', ca_igr_), 'L', jresl)
    if (ca_lparal_) then
        do ca_iel_ = 1, ca_nbelgr_
            if (ca_paral_(ca_iel_)) then
                iaux0=(ca_iel_-1)*ncmpel
                iaux1=jresl+iaux0
                iaux2=ca_iachlo_+debugr-1+iaux0
                call jacopo(ncmpel, ca_typegd_, iaux1, iaux2)
            endif
        enddo
    else
        call jacopo(lggrel, ca_typegd_, jresl, ca_iachlo_+debugr-1)
    endif


    if (ca_lparal_) then
        do ca_iel_ = 1, ca_nbelgr_
            if (ca_paral_(ca_iel_)) then
                iaux1=ca_ilchlo_+debugr-1+(ca_iel_-1)*ncmpel
                do k = 1, ncmpel
                    zl(iaux1-1+k)=.true.
                enddo
            endif
        enddo
    else
        do k = 1, lggrel
            zl(ca_ilchlo_+debugr-1-1+k)=.true.
        enddo
    endif

999 continue
    call jedema()
end subroutine

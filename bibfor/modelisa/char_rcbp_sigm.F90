subroutine char_rcbp_sigm(cabl_prec, iocc, nbchs, jlces, jll,&
                          jlr)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/codent.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetc.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ltnotb.h"
#include "asterfort/tbexve.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=8), intent(in) :: cabl_prec
    integer, intent(in) :: iocc
    integer, intent(inout) :: nbchs
    integer, intent(in) :: jlces
    integer, intent(in) :: jll
    integer, intent(in) :: jlr
!
! --------------------------------------------------------------------------------------------------
!
! Loads affectation
!
! RELA_CINE_BP - Combine stresses
!
! --------------------------------------------------------------------------------------------------
!
! In  cabl_prec       : prestress information from CABLE_BP
! In  iocc            : numero d'occurence
! I/O nbchs           : nombres de champs a fusionner
!
! --------------------------------------------------------------------------------------------------
!
    character(len=4) :: chen
    character(len=8) :: k8bid
    character(len=19) :: cabl_sigm_read, tabl2, lisnom
    integer :: iret
    integer :: nbnom, jlsnom
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - on interdit ADHERENT = NON de DEFI_CABLE_BP dans ce cas
!
    call ltnotb(cabl_prec, 'CABLE_GL', tabl2)
    lisnom = '&&CAPREC.ADHERENT'
    call tbexve(tabl2, 'ADHERENT', lisnom, 'V', nbnom,&
                k8bid)
    call jeveuo(lisnom, 'L', jlsnom)
    if (zk8(jlsnom)(1:3) .eq. 'NON') then
        call utmess('F', 'MODELISA3_38')
    endif
    call jedetr(lisnom)
!
! - Transformation de la carte en champ
!
    cabl_sigm_read = cabl_prec//'.CHME.SIGIN'
    call jeexin(cabl_sigm_read//'.DESC', iret)
    if (iret .eq. 0) then
        call utmess('F', 'CHARGES2_49', sk=cabl_prec)
    endif
!
    ASSERT(nbchs.lt.10000)
    call codent(nbchs, 'D0', chen)
    call carces(cabl_sigm_read, 'ELEM', ' ', 'V', '&&CAPREC.CES'// chen,&
                'A', iret)
    zk16(jlces+iocc-1)='&&CAPREC.CES'//chen
    zr(jlr+iocc-1)=1.d0
    zl(jll+iocc-1)=.true.
    nbchs=nbchs+1
    call jelibe(cabl_sigm_read//'.DESC')
    call jelibe(cabl_sigm_read//'.VALE')
    call jedetc('V', cabl_sigm_read, 1)
!
    call jedema()
end subroutine

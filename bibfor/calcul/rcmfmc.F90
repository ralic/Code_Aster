subroutine rcmfmc(chmatz, chmacz)
use calcul_module, only : ca_jvcnom_, ca_nbcvrc_

implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "jeveux.h"
#include "asterc/ismaem.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jeveut.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/rcmaco.h"
#include "asterfort/wkvect.h"

    character(len=*) :: chmatz, chmacz
!-----------------------------------------------------------------------
! Creation de la carte du materiau code a partir du champ_mater
!
! in/jxin   chmatz    : cham_mater
! out/jxout chmacz    : carte de materiau code
!-----------------------------------------------------------------------
    integer :: nbval,  iret, jvale, igd, jdesc, kk
    integer :: nbgrp, i, icompt, igrp, ingrp, nbcmp, j, k, nbmat
    integer :: inbmat
    character(len=4) :: knumat
    character(len=8) :: chmat, nomgd
    character(len=19) :: codi
    character(len=19) :: chemat, chmace
    character(len=8), pointer :: vale(:) => null()
!-----------------------------------------------------------------------
    call jemarq()

    chmat=chmatz
    chemat=chmat//'.CHAMP_MAT'
    chmace=chmat//'.MATE_CODE'

    call jelira(chemat//'.VALE', 'LONMAX', nbval)
    call jeveuo(chemat//'.VALE', 'L', vk8=vale)


!   -- si chmace existe, c'est que l'on a deja appele
!      la routine rcmfmc. tout est deja fait :
!   ----------------------------------------------------------
    call exisd('CARTE', chmace, iret)
    if (iret .gt. 0) goto 90


!   -- mise a jour des variables ca_nbcvrc_ et ca_jvcnom_
!   -----------------------------------------------
    call jeexin(chmat//'.CVRCNOM', iret)
    if (iret .ne. 0) then
        call jeveut(chmat//'.CVRCNOM', 'L', ca_jvcnom_)
        call jelira(chmat//'.CVRCNOM', 'LONMAX', ca_nbcvrc_)
    else
        ca_nbcvrc_=0
        ca_jvcnom_=ismaem()
    endif


!   -- traitement du materiau par elements :
!   ----------------------------------------
    call jeveuo(chemat//'.DESC', 'L', jdesc)
    call jenuno(jexnum('&CATA.GD.NOMCMP', zi(jdesc)), nomgd)
    call dismoi('NB_CMP_MAX', nomgd, 'GRANDEUR', repi=nbcmp)
    ASSERT(nbcmp.ge.30)
    ASSERT((nbval/nbcmp)*nbcmp.eq.nbval)

    call copisd('CHAMP_GD', 'V', chemat, chmace)
    call jedetr(chmace//'.VALE')
    nbgrp=nbval/nbcmp
    call wkvect(chmace//'.VALE', 'V V I', nbgrp, jvale)
    call jenonu(jexnom('&CATA.GD.NOMGD', 'ADRSJEVE'), igd)
    call jeveuo(chmace//'.DESC', 'E', jdesc)
    zi(jdesc)=igd


!   -- codage du materiau :
!   -----------------------
    icompt=0
    do i = 1, nbval
        if (vale(i) .ne. ' ') icompt=icompt+1
    end do
    ASSERT(icompt.gt.0)

    call jedetr(chmat//'.MATE_CODE.GRP')
    call jedetr(chmat//'.MATE_CODE.NGRP')
    call wkvect(chmat//'.MATE_CODE.GRP', 'V V K8', icompt, igrp)
    call wkvect(chmat//'.MATE_CODE.NGRP', 'V V I', nbgrp, ingrp)

    icompt=0
    inbmat=0
    do i = 1, nbgrp
!        -- il ne peut pas y avoir plus de 26 materiaux
        do j = 1, 26
            k=(i-1)*nbcmp+j
            if (vale(k) .eq. 'TREF=>') goto 30
            if (vale(k) .ne. ' ') then
                zk8(igrp+icompt)=vale(k)
                icompt=icompt+1
                inbmat=inbmat+1
            endif
        end do
 30     continue
        zi(ingrp-1+i)=inbmat
        inbmat=0
    end do

    codi=' '
    call jeveuo(chmat//'.MATE_CODE.GRP', 'L', igrp)
    call jeveuo(chmat//'.MATE_CODE.NGRP', 'L', ingrp)
    icompt=0
    do kk = 1, nbgrp
        nbmat=zi(ingrp-1+kk)
        if (nbmat .eq. 0) goto 50
        call rcmaco(chmat(1:8), icompt, nbmat, kk)
        call codent(kk, 'D0', knumat)

!       -- le nom du codi est celui du premier materiau du groupe kk
        codi(1:8)=zk8(igrp+icompt)
        codi(9:13)='.'//knumat
        call jeveuo(codi//'.CODI', 'L', zi(jvale+kk-1))
        icompt=icompt+nbmat
 50     continue
    end do


 90 continue
    chmacz=chmace
    call jedema()

end subroutine

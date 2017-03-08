subroutine rcmfmc(chmatz, chmacz)
!
implicit none
!
#include "jeveux.h"
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
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/rcmaco.h"
#include "asterfort/wkvect.h"
#include "asterfort/varc_prep.h"
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
!
    character(len=*), intent(in) :: chmatz
    character(len=*), intent(out) :: chmacz
!
! --------------------------------------------------------------------------------------------------
!
! Material
!
! Creation de la carte du materiau code a partir du champ_mater
!
! --------------------------------------------------------------------------------------------------
!
! In  chmate           : name of material field (CHAM_MATER)
! Out chmace           : name of CODED material field (CHAM_MATER)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nbval,  iret, jvale, igd, kk
    integer :: nbgrp, i, icompt, igrp, ingrp, nbcmp, j, k, nbmat
    integer :: inbmat
    character(len=4) :: knumat
    character(len=8) :: chmat, nomgd
    character(len=19) :: codi
    character(len=19) :: chemat, chmace
    character(len=8), pointer :: v_vale(:) => null()
    integer, pointer :: v_desc(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
    chmat  = chmatz
    chemat = chmat//'.CHAMP_MAT'
    chmace = chmat//'.MATE_CODE'
!
    call exisd('CARTE', chmace, iret)
    if (iret .eq. 0) then
! ----- Preparation for external state variables (VARC)
        call varc_prep(chmat)

! ----- Traitement du materiau par elements
        call jelira(chemat//'.VALE', 'LONMAX', nbval)
        call jeveuo(chemat//'.VALE', 'L', vk8= v_vale)
        call jeveuo(chemat//'.DESC', 'L', vi = v_desc)
        call jenuno(jexnum('&CATA.GD.NOMCMP', v_desc(1)), nomgd)
        call dismoi('NB_CMP_MAX', nomgd, 'GRANDEUR', repi=nbcmp)
        ASSERT(nbcmp.ge.30)
        ASSERT((nbval/nbcmp)*nbcmp.eq.nbval)
        call copisd('CHAMP_GD', 'V', chemat, chmace)
        call jedetr(chmace//'.VALE')
        nbgrp=nbval/nbcmp
        call wkvect(chmace//'.VALE', 'V V I', nbgrp, jvale)
        call jenonu(jexnom('&CATA.GD.NOMGD', 'ADRSJEVE'), igd)
        call jeveuo(chmace//'.DESC', 'E', vi = v_desc)
        v_desc(1) = igd

! ----- Codage du materiau
        icompt = 0
        do i = 1, nbval
            if (v_vale(i) .ne. ' ') then
                icompt=icompt+1
            endif
        end do
        ASSERT(icompt .gt. 0)

        call jedetr(chmat//'.MATE_CODE.GRP')
        call jedetr(chmat//'.MATE_CODE.NGRP')
        call wkvect(chmat//'.MATE_CODE.GRP', 'V V K8', icompt, igrp)
        call wkvect(chmat//'.MATE_CODE.NGRP', 'V V I', nbgrp, ingrp)

        icompt=0
        inbmat=0
        do i = 1, nbgrp
            do j = 1, 26
                k=(i-1)*nbcmp+j
                if (v_vale(k) .eq. 'TREF=>') exit
                if (v_vale(k) .ne. ' ') then
                    zk8(igrp+icompt)=v_vale(k)
                    icompt=icompt+1
                    inbmat=inbmat+1
                endif
            end do
            zi(ingrp-1+i)=inbmat
            inbmat=0
        end do

        codi=' '
        call jeveuo(chmat//'.MATE_CODE.GRP', 'L', igrp)
        call jeveuo(chmat//'.MATE_CODE.NGRP', 'L', ingrp)
        icompt=0
        do kk = 1, nbgrp
            nbmat=zi(ingrp-1+kk)
            if (nbmat .ne. 0) then
            call rcmaco(chmat(1:8), icompt, nbmat, kk)
            call codent(kk, 'D0', knumat)

!       -- le nom du codi est celui du premier materiau du groupe kk
            codi(1:8)=zk8(igrp+icompt)
            codi(9:13)='.'//knumat
            call jeveuo(codi//'.CODI', 'L', zi(jvale+kk-1))
            icompt=icompt+nbmat
            endif
        end do

    endif
    chmacz=chmace
!
    call jedema()
end subroutine

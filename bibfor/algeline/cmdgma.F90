subroutine cmdgma(mailla)
    implicit none
#include "jeveux.h"
#include "asterfort/cpclma.h"
#include "asterfort/getvem.h"
#include "asterfort/getvis.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=*) :: mailla
!     ------------------------------------------------------------------
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
!     OPERATEUR CREA_MAILLAGE   MOT CLE FACTEUR "DETR_GROUP_MA"
!     ------------------------------------------------------------------
!
    integer :: limit, long, ndetr, nugr, igma, n1, iret, ngrma
    integer :: i, jvg, jgg, nbma, ii, adetr, ngrman
    character(len=8) :: ma
    character(len=24) :: nomg
    integer :: iarg
    character(len=24), pointer :: ligrma_a_detr(:) => null()
    integer, pointer :: nugrma_a_detr(:) => null()
!     ------------------------------------------------------------------
!
    call jemarq()
!
    ma = mailla
    call jelira(ma//'.GROUPEMA', 'NMAXOC', ival=ngrma)
    AS_ALLOCATE(vi=nugrma_a_detr, size=ngrma)
!
    call getvis('DETR_GROUP_MA', 'NB_MAILLE', iocc=1, scal=limit, nbret=n1)
!     --------------------------------------------------------
    if (limit .gt. 0) then
        do 1,igma=1,ngrma
        call jeexin(jexnum(ma//'.GROUPEMA', igma), iret)
        if (iret .gt. 0) then
            call jelira(jexnum(ma//'.GROUPEMA', igma), 'LONUTI', ival=long)
            if (long .le. limit) nugrma_a_detr(igma)=1
        endif
 1      continue
    endif
!
    call getvem(ma, 'GROUP_MA', 'DETR_GROUP_MA', 'GROUP_MA', 1,&
                iarg, 0, zk24(1), ndetr)
!     ----------------------------------------------------------
    if (ndetr .lt. 0) then
        ndetr=-ndetr
        AS_ALLOCATE(vk24=ligrma_a_detr, size=ndetr)
        call getvem(ma, 'GROUP_MA', 'DETR_GROUP_MA', 'GROUP_MA', 1,&
                    iarg, ndetr, ligrma_a_detr, n1)
        do 2,igma=1,ndetr
        call jenonu(jexnom(ma//'.GROUPEMA', ligrma_a_detr(igma)), nugr)
        if (nugr .gt. 0) nugrma_a_detr(nugr)=1
 2      continue
    endif
!
!
!     -- DESTRUCTION DES GROUPES DE MAILLES :
!     ---------------------------------------
!
    adetr = 0
    do 10, i= 1 , ngrma
    if (nugrma_a_detr(i) .ne. 0) adetr = adetr + 1
    10 end do
!
    ngrman = ngrma - adetr
!
    call cpclma(ma, '&&CMDGMA', 'GROUPEMA', 'V')
    call jedetr(ma//'.GROUPEMA')
    call jedetr(ma//'.PTRNOMMAI')
!
    if (ngrman .gt. 0) then
        call jecreo(ma//'.PTRNOMMAI', 'G N K24')
        call jeecra(ma//'.PTRNOMMAI', 'NOMMAX', ival=ngrman)
        call jecrec(ma//'.GROUPEMA', 'G V I', 'NO '//ma//'.PTRNOMMAI', 'DISPERSE', 'VARIABLE',&
                    ngrman)
    endif
!
    do 3, i=1,ngrma
    call jeexin(jexnum('&&CMDGMA.GROUPEMA', i), iret)
    if (iret .le. 0) goto 3
    if (nugrma_a_detr(i) .eq. 0) then
        call jenuno(jexnum('&&CMDGMA.GROUPEMA', i), nomg)
        call jecroc(jexnom(ma//'.GROUPEMA', nomg))
        call jeveuo(jexnum('&&CMDGMA.GROUPEMA', i), 'L', jvg)
        call jelira(jexnum('&&CMDGMA.GROUPEMA', i), 'LONUTI', ival=nbma)
        call jeecra(jexnom(ma//'.GROUPEMA', nomg), 'LONMAX', ival=max(1, nbma))
        call jeecra(jexnom(ma//'.GROUPEMA', nomg), 'LONUTI', ival=nbma)
        call jeveuo(jexnom(ma//'.GROUPEMA', nomg), 'E', jgg)
        do 4,ii=1,nbma
        zi(jgg-1+ii)=zi(jvg-1+ii)
 4      continue
    endif
    3 end do
!
    AS_DEALLOCATE(vi=nugrma_a_detr)
    AS_DEALLOCATE(vk24=ligrma_a_detr)
    call jedetr('&&CMDGMA.GROUPEMA')
!
    call jedema()
end subroutine

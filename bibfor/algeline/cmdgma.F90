subroutine cmdgma(mailla)
    implicit none
    include 'jeveux.h'
!
    include 'asterc/getvis.h'
    include 'asterfort/cpclma.h'
    include 'asterfort/getvem.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jecreo.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/wkvect.h'
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
    integer :: limit, long, ndetr, nugr, igma, n1, iret, ngrma, ianugr, ialigr
    integer :: i, jvg, jgg, nbma, ii, adetr, ngrman
    character(len=1) :: k1b
    character(len=8) :: ma
    character(len=24) :: nomg
    integer :: iarg
!     ------------------------------------------------------------------
!
    call jemarq()
!
    ma = mailla
    call jelira(ma//'.GROUPEMA', 'NMAXOC', ngrma, k1b)
    call wkvect('&&CMDGMA.NUGRMA_A_DETR', 'V V I', ngrma, ianugr)
!
    call getvis('DETR_GROUP_MA', 'NB_MAILLE', 1, iarg, 1,&
                limit, n1)
!     --------------------------------------------------------
    if (limit .gt. 0) then
        do 1,igma=1,ngrma
        call jeexin(jexnum(ma//'.GROUPEMA', igma), iret)
        if (iret .gt. 0) then
            call jelira(jexnum(ma//'.GROUPEMA', igma), 'LONUTI', long, k1b)
            if (long .le. limit) zi(ianugr-1+igma)=1
        endif
 1      continue
    endif
!
    call getvem(ma, 'GROUP_MA', 'DETR_GROUP_MA', 'GROUP_MA', 1,&
                iarg, 0, zk24(1), ndetr)
!     ----------------------------------------------------------
    if (ndetr .lt. 0) then
        ndetr=-ndetr
        call wkvect('&&CMDGMA.LIGRMA_A_DETR', 'V V K24', ndetr, ialigr)
        call getvem(ma, 'GROUP_MA', 'DETR_GROUP_MA', 'GROUP_MA', 1,&
                    iarg, ndetr, zk24(ialigr), n1)
        do 2,igma=1,ndetr
        call jenonu(jexnom(ma//'.GROUPEMA', zk24(ialigr-1+igma)), nugr)
        if (nugr .gt. 0) zi(ianugr-1+nugr)=1
 2      continue
    endif
!
!
!     -- DESTRUCTION DES GROUPES DE MAILLES :
!     ---------------------------------------
!
    adetr = 0
    do 10, i= 1 , ngrma
    if (zi(ianugr-1+i) .ne. 0) adetr = adetr + 1
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
        call jeecra(ma//'.PTRNOMMAI', 'NOMMAX', ngrman, ' ')
        call jecrec(ma//'.GROUPEMA', 'G V I', 'NO '//ma//'.PTRNOMMAI', 'DISPERSE', 'VARIABLE',&
                    ngrman)
    endif
!
    do 3, i=1,ngrma
    call jeexin(jexnum('&&CMDGMA.GROUPEMA', i), iret)
    if (iret .le. 0) goto 3
    if (zi(ianugr-1+i) .eq. 0) then
        call jenuno(jexnum('&&CMDGMA.GROUPEMA', i), nomg)
        call jecroc(jexnom(ma//'.GROUPEMA', nomg))
        call jeveuo(jexnum('&&CMDGMA.GROUPEMA', i), 'L', jvg)
        call jelira(jexnum('&&CMDGMA.GROUPEMA', i), 'LONUTI', nbma, k1b)
        call jeecra(jexnom(ma//'.GROUPEMA', nomg), 'LONMAX', max(1, nbma), ' ')
        call jeecra(jexnom(ma//'.GROUPEMA', nomg), 'LONUTI', nbma, ' ')
        call jeveuo(jexnom(ma//'.GROUPEMA', nomg), 'E', jgg)
        do 4,ii=1,nbma
        zi(jgg-1+ii)=zi(jvg-1+ii)
 4      continue
    endif
    3 end do
!
    call jedetr('&&CMDGMA.NUGRMA_A_DETR')
    call jedetr('&&CMDGMA.LIGRMA_A_DETR')
    call jedetr('&&CMDGMA.GROUPEMA')
!
    call jedema()
end subroutine

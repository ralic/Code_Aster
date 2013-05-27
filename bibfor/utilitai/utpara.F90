subroutine utpara(bas1, nomsd, typsd, nbordr)
    implicit none
    include 'jeveux.h'
!
    include 'asterc/isnnem.h'
    include 'asterc/r8vide.h'
    include 'asterfort/assert.h'
    include 'asterfort/codent.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jecreo.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/utpar1.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: nomsd, typsd
    character(len=1) :: bas1
    integer :: nbordr
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! ----------------------------------------------------------------------
    character(len=8) :: ch8, type, acces, kbid
    character(len=5) :: suffix
    character(len=19) :: noms2
    integer :: nbpamx, nbpara
    parameter (nbpamx=100)
    character(len=32) :: para, lipara(nbpamx)
    character(len=16) :: nopara
    character(len=8) :: liacce(nbpamx), litype(nbpamx)
    integer :: i, jtava, ico, jpara, iundef, i1
    integer :: nbr, nbi, nbc, nbk8, nbk16, nbk24, nbk32, nbk80, n1, n2
    real(kind=8) :: rundef
!     ------------------------------------------------------------------
!
    call jemarq()
    noms2 = nomsd
    rundef = r8vide()
    iundef = isnnem()
!
!
!     -- RECUPERATION DE LA LISTE DES PARAMETRES :
!     --------------------------------------------
    call utpar1(typsd, nbpamx, lipara, nbpara)
!
!
!     -- CREATION DE .NOVA ET .TAVA :
!     --------------------------------------------
    call jecreo(noms2//'.NOVA', bas1//' N K16')
    call jeecra(noms2//'.NOVA', 'NOMMAX', nbpara, ' ')
    do 2 i = 1, nbpara
        para=lipara(i)
        i1= index(para,'#')
        call assert(i1.ge.2)
        nopara=para(1:i1-1)
        call jecroc(jexnom(noms2//'.NOVA', nopara))
 2  end do
!
    call jecrec(noms2//'.TAVA', bas1//' V K8', 'NU', 'CONTIG', 'CONSTANT',&
                nbpara)
    call jeecra(noms2//'.TAVA', 'LONMAX', 4, ' ')
!
!
!     -- CALCUL DE NBR, NBC, NBI, NBK8, ... :
!     -- CALCUL DE LIACCE et LITYPE :
!     ----------------------------------------
    nbr=0
    nbc=0
    nbi=0
    nbk8=0
    nbk16=0
    nbk24=0
    nbk32=0
    nbk80=0
    do 1 i = 1, nbpara
        para=lipara(i)
        i1= index(para,'#')
        call assert(para(i1:i1+2).eq.'#A#'.or.para(i1:i1+2).eq.'#P#')
        type=para(i1+3:32)
        litype(i)=type
        acces=para(i1+1:i1+1)
        if (acces .eq. 'A') then
            liacce(i)='ACCES'
        else
            liacce(i)='PARA'
        endif
        if (type .eq. 'R') then
            nbr=nbr+1
        else if (type.eq.'C') then
            nbc=nbc+1
        else if (type.eq.'I') then
            nbi=nbi+1
        else if (type.eq.'K8') then
            nbk8=nbk8+1
        else if (type.eq.'K16') then
            nbk16=nbk16+1
        else if (type.eq.'K24') then
            nbk24=nbk24+1
        else if (type.eq.'K32') then
            nbk32=nbk32+1
        else if (type.eq.'K80') then
            nbk80=nbk80+1
        else
            call assert(.false.)
        endif
 1  end do
!
!
!     -- PARAMETRES REELS :
!     ---------------------
    if (nbr .gt. 0) then
        suffix='.RSPR'
        n1=nbr
        n2=n1*nbordr
        call wkvect(noms2//suffix, bas1//' V R', n2, jpara)
        call jeecra(noms2//suffix, 'LONUTI', 0, kbid)
        do 10 i = 1, n2
            zr(jpara+i-1) = rundef
10      end do
!
        call codent(n1, 'G', ch8)
        ico=0
        do 11 i = 1, nbpara
            para=lipara(i)
            i1= index(para,'#')
            type=litype(i)
            acces=liacce(i)
            if (type .eq. 'R') then
                ico=ico+1
                nopara= para(1:i1-1)
                call jenonu(jexnom(noms2//'.NOVA', nopara), i1)
                call jeveuo(jexnum(noms2//'.TAVA', i1), 'E', jtava)
                zk8(jtava-1+1) = suffix
                call codent(ico, 'G', zk8(jtava-1+2))
                zk8(jtava-1+3) = ch8
                zk8(jtava-1+4) = acces
            endif
11      continue
    endif
!
!
!     -- PARAMETRES COMPLEXES :
!     -------------------------
    if (nbc .gt. 0) then
        suffix='.RSPC'
        n1=nbc
        n2=n1*nbordr
        call wkvect(noms2//suffix, bas1//' V C', n2, jpara)
        call jeecra(noms2//suffix, 'LONUTI', 0, kbid)
        do 20 i = 1, n2
            zc(jpara+i-1) = dcmplx(rundef,rundef)
20      continue
!
        call codent(n1, 'G', ch8)
        ico=0
        do 21 i = 1, nbpara
            para=lipara(i)
            i1= index(para,'#')
            type=litype(i)
            acces=liacce(i)
            acces=para(i1+1:i1+1)
            if (type .eq. 'C') then
                ico=ico+1
                nopara= para(1:i1-1)
                call jenonu(jexnom(noms2//'.NOVA', nopara), i1)
                call jeveuo(jexnum(noms2//'.TAVA', i1), 'E', jtava)
                zk8(jtava-1+1) = suffix
                call codent(ico, 'G', zk8(jtava-1+2))
                zk8(jtava-1+3) = ch8
                zk8(jtava-1+4) = acces
            endif
21      continue
    endif
!
!
!     -- PARAMETRES ENTIERS :
!     ---------------------
    if (nbi .gt. 0) then
        suffix='.RSPI'
        n1=nbi
        n2=n1*nbordr
        call wkvect(noms2//suffix, bas1//' V I', n2, jpara)
        call jeecra(noms2//suffix, 'LONUTI', 0, kbid)
        do 30 i = 1, n2
            zi(jpara+i-1) = iundef
30      continue
!
        call codent(n1, 'G', ch8)
        ico=0
        do 31 i = 1, nbpara
            para=lipara(i)
            i1= index(para,'#')
            acces=para(i1+1:i1+1)
            type=litype(i)
            acces=liacce(i)
            if (type .eq. 'I') then
                ico=ico+1
                nopara= para(1:i1-1)
                call jenonu(jexnom(noms2//'.NOVA', nopara), i1)
                call jeveuo(jexnum(noms2//'.TAVA', i1), 'E', jtava)
                zk8(jtava-1+1) = suffix
                call codent(ico, 'G', zk8(jtava-1+2))
                zk8(jtava-1+3) = ch8
                zk8(jtava-1+4) = acces
            endif
31      continue
    endif
!
!
!     -- PARAMETRES K8 :
!     ---------------------
    if (nbk8 .gt. 0) then
        suffix='.RSP8'
        n1=nbk8
        n2=n1*nbordr
        call wkvect(noms2//suffix, bas1//' V K8', n2, jpara)
        call jeecra(noms2//suffix, 'LONUTI', 0, kbid)
!
        call codent(n1, 'G', ch8)
        ico=0
        do 41 i = 1, nbpara
            para=lipara(i)
            i1= index(para,'#')
            acces=para(i1+1:i1+1)
            type=litype(i)
            acces=liacce(i)
            if (type .eq. 'K8') then
                ico=ico+1
                nopara= para(1:i1-1)
                call jenonu(jexnom(noms2//'.NOVA', nopara), i1)
                call jeveuo(jexnum(noms2//'.TAVA', i1), 'E', jtava)
                zk8(jtava-1+1) = suffix
                call codent(ico, 'G', zk8(jtava-1+2))
                zk8(jtava-1+3) = ch8
                zk8(jtava-1+4) = acces
            endif
41      continue
    endif
!
!
!     -- PARAMETRES K16 :
!     ---------------------
    if (nbk16 .gt. 0) then
        suffix='.RS16'
        n1=nbk16
        n2=n1*nbordr
        call wkvect(noms2//suffix, bas1//' V K16', n2, jpara)
        call jeecra(noms2//suffix, 'LONUTI', 0, kbid)
!
        call codent(n1, 'G', ch8)
        ico=0
        do 51 i = 1, nbpara
            para=lipara(i)
            i1= index(para,'#')
            acces=para(i1+1:i1+1)
            type=litype(i)
            acces=liacce(i)
            if (type .eq. 'K16') then
                ico=ico+1
                nopara= para(1:i1-1)
                call jenonu(jexnom(noms2//'.NOVA', nopara), i1)
                call jeveuo(jexnum(noms2//'.TAVA', i1), 'E', jtava)
                zk8(jtava-1+1) = suffix
                call codent(ico, 'G', zk8(jtava-1+2))
                zk8(jtava-1+3) = ch8
                zk8(jtava-1+4) = acces
            endif
51      continue
    endif
!
!
!     -- PARAMETRES K24 :
!     ---------------------
    if (nbk24 .gt. 0) then
        suffix='.RS24'
        n1=nbk24
        n2=n1*nbordr
        call wkvect(noms2//suffix, bas1//' V K24', n2, jpara)
        call jeecra(noms2//suffix, 'LONUTI', 0, kbid)
!
        call codent(n1, 'G', ch8)
        ico=0
        do 61 i = 1, nbpara
            para=lipara(i)
            i1= index(para,'#')
            acces=para(i1+1:i1+1)
            type=litype(i)
            acces=liacce(i)
            if (type .eq. 'K24') then
                ico=ico+1
                nopara= para(1:i1-1)
                call jenonu(jexnom(noms2//'.NOVA', nopara), i1)
                call jeveuo(jexnum(noms2//'.TAVA', i1), 'E', jtava)
                zk8(jtava-1+1) = suffix
                call codent(ico, 'G', zk8(jtava-1+2))
                zk8(jtava-1+3) = ch8
                zk8(jtava-1+4) = acces
            endif
61      continue
    endif
!
!
!     -- PARAMETRES K32 :
!     ---------------------
    if (nbk32 .gt. 0) then
        suffix='.RS32'
        n1=nbk32
        n2=n1*nbordr
        call wkvect(noms2//suffix, bas1//' V K32', n2, jpara)
        call jeecra(noms2//suffix, 'LONUTI', 0, kbid)
!
        call codent(n1, 'G', ch8)
        ico=0
        do 71 i = 1, nbpara
            para=lipara(i)
            i1= index(para,'#')
            acces=para(i1+1:i1+1)
            type=litype(i)
            acces=liacce(i)
            if (type .eq. 'K32') then
                ico=ico+1
                nopara= para(1:i1-1)
                call jenonu(jexnom(noms2//'.NOVA', nopara), i1)
                call jeveuo(jexnum(noms2//'.TAVA', i1), 'E', jtava)
                zk8(jtava-1+1) = suffix
                call codent(ico, 'G', zk8(jtava-1+2))
                zk8(jtava-1+3) = ch8
                zk8(jtava-1+4) = acces
            endif
71      continue
    endif
!
!
!     -- PARAMETRES K80 :
!     ---------------------
    if (nbk80 .gt. 0) then
        suffix='.RS80'
        n1=nbk80
        n2=n1*nbordr
        call wkvect(noms2//suffix, bas1//' V K80', n2, jpara)
        call jeecra(noms2//suffix, 'LONUTI', 0, kbid)
!
        call codent(n1, 'G', ch8)
        ico=0
        do 81 i = 1, nbpara
            para=lipara(i)
            i1= index(para,'#')
            acces=para(i1+1:i1+1)
            type=litype(i)
            acces=liacce(i)
            if (type .eq. 'K80') then
                ico=ico+1
                nopara= para(1:i1-1)
                call jenonu(jexnom(noms2//'.NOVA', nopara), i1)
                call jeveuo(jexnum(noms2//'.TAVA', i1), 'E', jtava)
                zk8(jtava-1+1) = suffix
                call codent(ico, 'G', zk8(jtava-1+2))
                zk8(jtava-1+3) = ch8
                zk8(jtava-1+4) = acces
            endif
81      continue
    endif
!
!
    call jedema()
end subroutine

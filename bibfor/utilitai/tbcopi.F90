subroutine tbcopi(base, sd1, sd2)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/codent.h'
    include 'asterfort/jecreo.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: base, sd1, sd2
! ----------------------------------------------------------------------
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
!
!   BUT:
!   DUPLIQUER UNE STRUCTURE DE DONNEES TABLE.
!
!     IN:
!     BASE     : 'G' , 'V' , ... : BASE DE CREATION DE SD2
!     SD1 (K*) : NOM DE LA SD A DUPPLIQUER
!     SD2 (K*) : NOM DE LA SD A CREER
!
!     OUT:
!     SD2 EST CREEE ET A LE MEME CONTENU QUE SD1
!
!-----------------------------------------------------------------------
!
    integer :: i, j, nbpm, nbpu, jnjv, knjv, kvale, jvale, nbpara, jtbba, ktbnp
    integer :: nblign, jtbnp, ndim, jtblp, ktblp
    character(len=1) :: bas2, kbid
    character(len=4) :: type, knume
    character(len=19) :: tab1, tab2
    character(len=24) :: nomjv
!
! DEB-------------------------------------------------------------------
!
    call jemarq()
    bas2 = base
!
    tab1 = sd1
    tab2 = sd2
!
    call wkvect(tab2//'.TBBA', bas2//' V K8', 1, jtbba)
    zk8(jtbba) = bas2
    call jeveuo(tab1//'.TBNP', 'L', ktbnp)
    nbpara = zi(ktbnp )
    nblign = zi(ktbnp+1)
!
    call wkvect(tab2//'.TBNP', bas2//' V I', 2, jtbnp)
    zi(jtbnp ) = nbpara
    zi(jtbnp+1) = nblign
    ndim = 4 * nbpara
    call jecreo(tab2//'.TBLP', bas2//' V K24')
    call jeecra(tab2//'.TBLP', 'LONMAX', ndim, ' ')
    call jeecra(tab2//'.TBLP', 'LONUTI', ndim, ' ')
    call jeveuo(tab2//'.TBLP', 'E', jtblp)
    call jeveuo(tab1//'.TBLP', 'L', ktblp)
    do 10 i = 1, nbpara
        zk24(jtblp+4*(i-1) ) = zk24(ktblp+4*(i-1) )
        type = zk24(ktblp+4*(i-1)+1)
        zk24(jtblp+4*(i-1)+1) = type
        nomjv = zk24(ktblp+4*(i-1)+2)
        call jelira(nomjv, 'LONMAX', nbpm, kbid)
        call jelira(nomjv, 'LONUTI', nbpu, kbid)
        call codent(i, 'D0', knume)
        nomjv = tab2(1:17)//'LG.'//knume
        zk24(jtblp+4*(i-1)+3) = nomjv
        call jecreo(nomjv, bas2//' V I')
        call jeecra(nomjv, 'LONMAX', nbpm, ' ')
        call jeecra(nomjv, 'LONUTI', nbpu, ' ')
        call jeveuo(nomjv, 'E', jnjv)
        nomjv = tab1(1:17)//'LG.'//knume
        call jeveuo(nomjv, 'L', knjv)
        do 12 j = 1, nbpm
            zi(jnjv+j-1) = zi(knjv+j-1)
12      continue
        nomjv = tab1//'.'//knume
        call jeveuo(nomjv, 'L', kvale)
        nomjv = tab2//'.'//knume
        zk24(jtblp+4*(i-1)+2) = nomjv
        call jecreo(nomjv, bas2//' V '//type)
        call jeecra(nomjv, 'LONMAX', nbpm, ' ')
        call jeecra(nomjv, 'LONUTI', nbpu, ' ')
        call jeveuo(nomjv, 'E', jvale)
        if (type(1:1) .eq. 'I') then
            do 20 j = 1, nbpm
                zi(jvale+j-1) = zi(kvale+j-1)
20          continue
        else if (type(1:1) .eq. 'R') then
            do 21 j = 1, nbpm
                zr(jvale+j-1) = zr(kvale+j-1)
21          continue
        else if (type(1:1) .eq. 'C') then
            do 22 j = 1, nbpm
                zc(jvale+j-1) = zc(kvale+j-1)
22          continue
        else if (type(1:3) .eq. 'K80') then
            do 23 j = 1, nbpm
                zk80(jvale+j-1) = zk80(kvale+j-1)
23          continue
        else if (type(1:3) .eq. 'K32') then
            do 24 j = 1, nbpm
                zk32(jvale+j-1) = zk32(kvale+j-1)
24          continue
        else if (type(1:3) .eq. 'K24') then
            do 25 j = 1, nbpm
                zk24(jvale+j-1) = zk24(kvale+j-1)
25          continue
        else if (type(1:3) .eq. 'K16') then
            do 26 j = 1, nbpm
                zk16(jvale+j-1) = zk16(kvale+j-1)
26          continue
        else if (type(1:2) .eq. 'K8') then
            do 27 j = 1, nbpm
                zk8(jvale+j-1) = zk8(kvale+j-1)
27          continue
        endif
        call jeecra(nomjv, 'LONUTI', nbpu, ' ')
10  end do
!
    call jedema()
end subroutine

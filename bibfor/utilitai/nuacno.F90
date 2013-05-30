subroutine nuacno(nuage, lno, chno)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/dismoi.h'
    include 'asterfort/exisdg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/nbec.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: nuage, lno, chno
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
!     PASSAGE D'UNE UNE SD NUAGE A SD CHAM_NO
!
! IN  NUAGE  : NOM DE LA SD NUAGE
! IN  LNO    : LISTE DES NOEUDS A PRENDRE EN COMPTE
! IN  CHNO   : NOM DE LA SD CHAM_NO
!     ------------------------------------------------------------------
    integer :: gd
    character(len=4) :: type
    character(len=8) :: k8b, noma, nomgd
    character(len=19) :: kchno, klno, knuage, nonu
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iad, iaec, ianueq, iaprno, ibid, icmp
    integer :: icompt, ie, iec, ieq, ino, itype, ival
    integer :: j, jdesc, jlno, jnuai, jnuav, jrefe, k
    integer :: kcomp, kvale, nc, ncmp, ncmpmx, nec, np
    integer :: num
!-----------------------------------------------------------------------
    call jemarq()
    knuage = nuage
    klno = lno
    kchno = chno
!
    call jeveuo(kchno//'.DESC', 'L', jdesc)
    gd = zi(jdesc-1+1)
    num = zi(jdesc-1+2)
    call jelira(jexnum('&CATA.GD.NOMCMP', gd), 'LONMAX', ncmpmx, k8b)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', gd), 'L', iad)
    call jenuno(jexnum('&CATA.GD.NOMGD', gd), nomgd)
    nec = nbec(gd)
    call wkvect('&&NUACNO.NOMCMP', 'V V I', ncmpmx, kcomp)
    call wkvect('&&NUACNO.ENT_COD', 'V V I', nec, iaec)
!
    call jeveuo(kchno//'.REFE', 'L', jrefe)
    noma = zk24(jrefe-1+1) (1:8)
    nonu = zk24(jrefe-1+2) (1:19)
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', np,&
                k8b, ie)
!
    if (klno .ne. ' ') then
        call jelira(klno, 'LONUTI', np, k8b)
        call jeveuo(klno, 'L', jlno)
    else
        call wkvect('&&NUACNO.NOEUD', 'V V I', np, jlno)
        do 10 i = 1, np
            zi(jlno+i-1) = i
10      continue
    endif
!
    call jelira(kchno//'.VALE', 'TYPE', ibid, type)
    call jeveuo(kchno//'.VALE', 'E', kvale)
    if (type(1:1) .eq. 'R') then
        itype = 1
    else if (type(1:1) .eq. 'C') then
        itype = 2
    else
        call u2mess('F', 'UTILITAI_36')
    endif
!
    call jeveuo(knuage//'.NUAV', 'L', jnuav)
    call jeveuo(knuage//'.NUAI', 'L', jnuai)
    nc = zi(jnuai+2)
!
!     --SI LE CHAMP EST A REPRESENTATION CONSTANTE ---
!
    if (num .lt. 0) then
        ncmp = -num
        do 200 iec = 1, nec
            zi(iaec+iec-1) = zi(jdesc-1+2+iec)
200      continue
        do 202 j = 1, np
            ino = zi(jlno+j-1)
            ival = ncmp * ( ino - 1 )
            icompt = 0
            do 204 icmp = 1, ncmpmx
                if (exisdg(zi(iaec) , icmp )) then
                    icompt = icompt + 1
                    k = nc*(j-1) + icompt
                    if (itype .eq. 1) then
                        zr(kvale-1+ival+icmp) = zr(jnuav+k-1)
                    else
                        zc(kvale-1+ival+icmp) = zc(jnuav+k-1)
                    endif
                endif
204          continue
202      continue
    else
!
!     --- SI LE CHAMP EST DECRIT PAR 1 "PRNO" ---
!
        call jeveuo(nonu//'.NUEQ', 'L', ianueq)
        call jenonu(jexnom(nonu//'.LILI', '&MAILLA'), ibid)
        call jeveuo(jexnum(nonu//'.PRNO', ibid), 'L', iaprno)
        do 210 j = 1, np
            ino = zi(jlno+j-1)
            ival = zi(iaprno-1+ (ino-1)*(nec+2)+1 )
            ncmp = zi(iaprno-1+ (ino-1)*(nec+2)+2 )
            if (ncmp .eq. 0) goto 210
            do 212 iec = 1, nec
                zi(iaec+iec-1) = zi(iaprno-1+ (ino-1)*(nec+2)+2+iec )
212          continue
            icompt = 0
            do 220 icmp = 1, ncmpmx
                if (exisdg(zi(iaec) , icmp )) then
                    icompt = icompt + 1
                    ieq = zi(ianueq-1+ival-1+icompt)
                    k = nc*(j-1) + icompt
                    if (itype .eq. 1) then
                        zr(kvale-1+ieq) = zr(jnuav+k-1)
                    else
                        zc(kvale-1+ieq) = zc(jnuav+k-1)
                    endif
                endif
220          continue
210      continue
    endif
!
    call jedetr('&&NUACNO.NOMCMP')
    call jedetr('&&NUACNO.ENT_COD')
    call jedetr('&&NUACNO.NOEUD')
    call jedema()
end subroutine

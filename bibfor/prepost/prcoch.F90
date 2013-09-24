subroutine prcoch(noche8, nochs8, nocmp, ktype, itopo,&
                  ngroup, group)
!
    implicit none
! person_in_charge: jacques.pellet at edf.fr
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/cesexi.h"
#include "asterfort/cesred.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnsred.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: itopo, ngroup
    character(len=8) :: noche8, nochs8, nocmp, ktype
    character(len=8) :: group(ngroup)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=24) :: litrou
    character(len=24) :: valk
    character(len=19) :: celz, cesz
    character(len=8) :: nomma, k8bid, nogd, tsca
    integer :: jcesd, jcesl, jcesc, jcesv, jcesk, ncmpmx, icmp, numcmp
    integer :: nbma, ima, nbpt, nbsp, ipt, isp, iad, jval, jma, jpo, jsp
    integer :: ival, iret, itrma, itrno, igr, nbtrou, itbma, lma, nbval
    integer :: jcnsd, jcnsl, jcnsc, jcnsv, nbno, ino, jno, nbn, in, ibid
    integer :: jcmp, cmpmin, cmpmax
    logical :: ltopo
!
!
    call jemarq()
    ltopo=(itopo.eq.1)
    celz=noche8
    cesz=nochs8
    litrou='&&PRCOCH.NUM_MAILLE'
!
    numcmp = 0
    if (nocmp(1:2) .eq. ' ') then
        numcmp = -1
    endif
!
    if (noche8 .eq. '__DETR__') then
!     --  SI NOCHE8='__DETR__' : ON DETRUIT LES VECTEURS
        call jedetr(cesz // '.V')
        if (ltopo) then
            call jedetr(cesz // '.M')
            call jedetr(cesz // '.P')
            call jedetr(cesz // '.SP')
            call jedetr(cesz // '.N')
            call jedetr(cesz // '.C')
!          ENDIF
        endif
        goto 9999
    endif
!
!
    call dismoi('F', 'NOM_MAILLA', celz, 'CHAMP', ibid,&
                nomma, iret)
    call dismoi('F', 'NOM_GD', celz, 'CHAMP', ibid,&
                nogd, iret)
    call dismoi('F', 'TYPE_SCA', nogd, 'GRANDEUR', ibid,&
                tsca, iret)
    call dismoi('F', 'NB_MA_MAILLA', nomma, 'MAILLAGE', nbma,&
                k8bid, iret)
    call dismoi('F', 'NB_NO_MAILLA', nomma, 'MAILLAGE', nbma,&
                k8bid, iret)
    ASSERT(tsca.eq.'R'.or.tsca.eq.'C'.or.tsca.eq.'I')
!
!
!
!
! -------------------------------------------
! CHAM_ELEM
! -------------------------------------------
    if (ktype(1:2) .eq. 'EL') then
!
! -- 1ERE ETAPE : CREATION CHAM_ELEM_S BEAUCOUP PLUS FACILE A MANIPULER
!                 ET DANS LA FOULEE REDUCTION SUR LES GROUP_MA
!                 QUI NOUS INTERESSENT
!
        call celces(celz, 'V', cesz)
!
!  --- TRAITEMENT DES LISTES DE GROUPES DE MAILLES---
        if (ngroup .ne. 0) then
            call jeveuo(cesz//'.CESK', 'L', jcesk)
            nomma=zk8(jcesk-1+1)
            call jeexin(litrou, iret)
            if (iret .ne. 0) call jedetr(litrou)
!     --- RECUPERATION DU NUMERO DE MAILLE----
            call dismoi('F', 'NB_MA_MAILLA', nomma, 'MAILLAGE', nbma,&
                        k8bid, iret)
            call wkvect('&&PRCOCH.INDIC_MAILLE', 'V V I', nbma, itrma)
!
            do 13 igr = 1, ngroup
                call jeexin(jexnom(nomma//'.GROUPEMA', group(igr)), iret)
                if (iret .eq. 0) then
                    valk = group(igr)
                    call utmess('A', 'PREPOST5_31', sk=valk)
                else
                    call jelira(jexnom(nomma//'.GROUPEMA', group(igr)), 'LONMAX', nbn)
                    call jeveuo(jexnom(nomma//'.GROUPEMA', group(igr)), 'L', iad)
                    do 14 in = 1, nbn
                        ima = zi(iad-1+in)
                        zi(itrma-1+ima) = 1
14                  continue
                endif
13          continue
!
            nbtrou = 0
            do 100 ima = 1, nbma
                if (zi(itrma-1+ima) .ne. 0) nbtrou = nbtrou + 1
100          continue
            if (nbtrou .eq. 0) then
                call utmess('F', 'CHAMPS_4')
            endif
!
!
            call wkvect(litrou, 'V V I', nbtrou, itbma)
!         --- RANGEMENT DES NUMEROS DE MAILLES ---
            lma = 0
            do 110 ima = 1, nbma
                if (zi(itrma-1+ima) .ne. 0) then
                    lma = lma + 1
                    zi(itbma-1+lma) = ima
                endif
110          continue
            call cesred(cesz,nbtrou,zi(itbma),0,[k8bid],&
                        'V', cesz)
            call jedetr('&&PRCOCH.INDIC_MAILLE')
        endif
!
! -- 2EME ETAPE : RECUPERATION DU NUMCMP QUI NOUS INTERESSE
!
        call jeveuo(cesz//'.CESD', 'L', jcesd)
        call jeveuo(cesz//'.CESL', 'L', jcesl)
        call jeveuo(cesz//'.CESC', 'L', jcesc)
        call jeveuo(cesz//'.CESV', 'L', jcesv)
!
        ncmpmx = zi(jcesd-1+2)
!
        do 5 icmp = 1, ncmpmx
            if (zk8(jcesc-1+icmp) .eq. nocmp) then
                numcmp=icmp
                goto 6
            endif
 5      continue
!
 6      continue
!
! -- 3EME ETAPE : RECUPERATION DE LA LONGUEUR DU VECTEUR DE VALEURS
!                   UTILES - ON GARDE QUE LES VALEURS DE LA CMP REMPLIES
!                   ET CREATION DES VECTEURS
!
        nbval=0
        nbma = zi(jcesd-1+1)
!
        do 10 ima = 1, nbma
!
!
            nbpt=zi(jcesd-1+5+4*(ima-1)+1)
            nbsp=zi(jcesd-1+5+4*(ima-1)+2)
!
            do 10 ipt = 1, nbpt
                do 10 isp = 1, nbsp
                    call cesexi('C', jcesd, jcesl, ima, ipt,&
                                isp, numcmp, iad)
                    if (iad .gt. 0) nbval=nbval+1
10              continue
!
!
        if (nbval .eq. 0) then
            call utmess('F', 'CHAMPS_1')
        endif
        if (tsca .eq. 'R') then
            call wkvect(cesz // '.V', 'G V R', nbval, jval)
        else if (tsca.eq.'I') then
            call wkvect(cesz // '.V', 'G V I', nbval, jval)
        else if (tsca.eq.'C') then
            call wkvect(cesz // '.V', 'G V C', nbval, jval)
        endif
        if (ltopo) then
            call wkvect(cesz // '.M', 'G V I', nbval, jma)
            call wkvect(cesz // '.P', 'G V I', nbval, jpo)
            call wkvect(cesz // '.SP', 'G V I', nbval, jsp)
        endif
!
!
! -- 4EME ETAPE : REMPLISSAGE DES VECTEURS
        ival=0
        do 20 ima = 1, nbma
!
!
            nbpt=zi(jcesd-1+5+4*(ima-1)+1)
            nbsp=zi(jcesd-1+5+4*(ima-1)+2)
!
            do 20 ipt = 1, nbpt
                do 20 isp = 1, nbsp
                    call cesexi('C', jcesd, jcesl, ima, ipt,&
                                isp, numcmp, iad)
                    if (iad .gt. 0) then
                        ival=ival+1
                        if (tsca .eq. 'R') then
                            zr(jval-1+ival)=zr(jcesv-1+iad)
                        else if (tsca.eq.'I') then
                            zi(jval-1+ival)=zi(jcesv-1+iad)
                        else if (tsca.eq.'C') then
                            zc(jval-1+ival)=zc(jcesv-1+iad)
                        endif
                        if (ltopo) then
                            zi(jma-1+ival)=ima
                            zi(jpo-1+ival)=ipt
                            zi(jsp-1+ival)=isp
                        endif
                    endif
20              continue
!
!
!
! -------------------------------------------
! CHAM_NO
! -------------------------------------------
    else if (ktype(1:2).eq.'NO') then
!
! -- 1ERE ETAPE : CREATION CHAM_NO_S BEAUCOUP PLUS FACILE A MANIPULER
!                 DANS LA FOULEE ON LE REDUIT SUR LA LISTE DE GROUP_NO
!
        call cnocns(celz, 'V', cesz)
!
!  --- TRAITEMENT DES LISTES DE GROUPES DE MAILLES---
        if (ngroup .ne. 0) then
            call jeveuo(cesz//'.CNSK', 'L', jcesk)
            nomma=zk8(jcesk-1+1)
            call jeexin(litrou, iret)
            if (iret .ne. 0) call jedetr(litrou)
!     --- RECUPERATION DU NUMERO DE NOEUDS----
            call dismoi('F', 'NB_NO_MAILLA', nomma, 'MAILLAGE', nbno,&
                        k8bid, iret)
            call wkvect('&&PRCOCH.INDIC_NOEUD', 'V V I', nbno, itrno)
!
            do 130 igr = 1, ngroup
                call jeexin(jexnom(nomma//'.GROUPENO', group(igr)), iret)
                if (iret .eq. 0) then
                    valk = group(igr)
                    call utmess('A', 'PREPOST5_31', sk=valk)
                else
                    call jelira(jexnom(nomma//'.GROUPENO', group(igr)), 'LONMAX', nbn)
                    call jeveuo(jexnom(nomma//'.GROUPENO', group(igr)), 'L', iad)
                    do 140 in = 1, nbn
                        ino = zi(iad-1+in)
                        zi(itrno-1+ino) = 1
140                  continue
                endif
130          continue
!
            nbtrou = 0
            do 1000 ino = 1, nbno
                if (zi(itrno-1+ino) .ne. 0) nbtrou = nbtrou + 1
1000          continue
            if (nbtrou .eq. 0) then
                call utmess('F', 'CHAMPS_5')
            endif
!
!
            call wkvect(litrou, 'V V I', nbtrou, itbma)
!         --- RANGEMENT DES NUMEROS DE NOEUDS ---
            lma = 0
            do 1100 ino = 1, nbno
                if (zi(itrno-1+ino) .ne. 0) then
                    lma = lma + 1
                    zi(itbma-1+lma) = ino
                endif
1100          continue
            call cnsred(cesz, nbtrou, zi(itbma), 0, [k8bid],&
                        'V', cesz)
            call jedetr('&&PRCOCH.INDIC_NOEUD')
        endif
!
!
! -- 2EME ETAPE : RECUPERATION DU NUMCMP QUI NOUS INTERESSE
!
        call jeveuo(cesz//'.CNSD', 'L', jcnsd)
        call jeveuo(cesz//'.CNSC', 'L', jcnsc)
        call jeveuo(cesz//'.CNSL', 'L', jcnsl)
        call jeveuo(cesz//'.CNSV', 'L', jcnsv)
!
        ncmpmx = zi(jcnsd-1+2)
!
        if (numcmp .lt. 0) then
            goto 60
        endif
        do 50 icmp = 1, ncmpmx
            if (zk8(jcnsc-1+icmp) .eq. nocmp) then
                numcmp=icmp
                goto 60
            endif
50      continue
!
!       SI LA COMPOSANTE DEMANDEE N EXISTE PAS, ERREUR FATALE
        call utmess('F', 'CHAMPS_3', sk=nocmp)
!
60      continue
!
!
! -- 3EME ETAPE : RECUPERATION DE LA LONGUEUR DU VECTEUR DE VALEURS
!                   UTILES - ON GARDE QUE LES VALEURS DE LA CMP REMPLIES
!                   ET CREATION DES VECTEURS
!
        nbval=0
        nbno = zi(jcnsd-1+1)
!
        if (numcmp .lt. 0) then
            do 70 icmp = 1, ncmpmx
                do 71 ino = 1, nbno
                    if (zl(jcnsl-1+(ino-1)*ncmpmx+icmp)) nbval=nbval+ 1
71              continue
70          continue
        else
            do 72 ino = 1, nbno
                if (zl(jcnsl-1+(ino-1)*ncmpmx+numcmp)) nbval=nbval+1
72          continue
        endif
!
!
!
        if (nbval .eq. 0) then
            call utmess('F', 'CHAMPS_1')
        endif
!
        if (tsca .eq. 'R') then
            call wkvect(cesz // '.V', 'G V R', nbval, jval)
        else if (tsca.eq.'I') then
            call wkvect(cesz // '.V', 'G V I', nbval, jval)
        else if (tsca.eq.'C') then
            call wkvect(cesz // '.V', 'G V C', nbval, jval)
        endif
        if (ltopo) then
            call wkvect(cesz // '.N', 'G V I', nbval, jno)
            if (numcmp .lt. 0) then
                call wkvect(cesz // '.C', 'G V K8', nbval, jcmp)
            endif
        endif
!
! -- 4EME ETAPE : REMPLISSAGE DES VECTEURS
        ival=0
        do 200 ino = 1, nbno
            if (numcmp .gt. 0) then
                cmpmin=numcmp
                cmpmax=numcmp
            else
                cmpmin=1
                cmpmax=ncmpmx
            endif
            do 201 icmp = cmpmin, cmpmax
                if (zl(jcnsl-1+(ino-1)*ncmpmx+icmp)) then
                    ival=ival+1
                    if (tsca .eq. 'R') then
                        zr(jval-1+ival)=zr(jcnsv-1+(ino-1)*ncmpmx+&
                        icmp)
                    else if (tsca.eq.'I') then
                        zi(jval-1+ival)=zi(jcnsv-1+(ino-1)*ncmpmx+&
                        icmp)
                    else if (tsca.eq.'C') then
                        zc(jval-1+ival)=zc(jcnsv-1+(ino-1)*ncmpmx+&
                        icmp)
                    endif
                    if (ltopo) then
                        zi(jno-1+ival)=ino
                        if (numcmp .lt. 0) zk8(jcmp-1+ival)=zk8(jcnsc-1+ icmp)
                    endif
                endif
201          continue
200      continue
!
    else
        ASSERT(.false.)
    endif
!
!
9999  continue
    call jedema()
end subroutine

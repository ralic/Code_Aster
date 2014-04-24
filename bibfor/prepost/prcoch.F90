subroutine prcoch(noche8, nochs8, nocmp, ktype, itopo,&
                  ngroup, group)
!
    implicit none
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
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=8), intent(in) :: noche8
    character(len=8), intent(in) :: nochs8
    character(len=8), intent(in) :: nocmp
    character(len=8), intent(in) :: ktype
    integer, intent(in) :: itopo
    integer, intent(in) :: ngroup
    character(len=24), intent(in) :: group(ngroup)
!
    character(len=24) :: litrou
    character(len=24) :: valk
    character(len=19) :: celz, cesz
    character(len=8) :: nomma, k8bid, nogd, tsca
    integer :: jcesd, jcesl,  jcesv, jcesk, ncmpmx, icmp, numcmp
    integer :: nbma, ima, nbpt, nbsp, ipt, isp, iad, jval, jma, jpo, jsp
    integer :: ival, iret,   igr, nbtrou, itbma, lma, nbval
    integer ::  jcnsl,  jcnsv, nbno, ino, jno, nbn, in
    integer :: jcmp, cmpmin, cmpmax
    logical :: ltopo
    character(len=8), pointer :: cnsc(:) => null()
    integer, pointer :: cnsd(:) => null()
    character(len=8), pointer :: cesc(:) => null()
    integer, pointer :: indic_maille(:) => null()
    integer, pointer :: indic_noeud(:) => null()
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
        goto 999
    endif
!
!
    call dismoi('NOM_MAILLA', celz, 'CHAMP', repk=nomma)
    call dismoi('NOM_GD', celz, 'CHAMP', repk=nogd)
    call dismoi('TYPE_SCA', nogd, 'GRANDEUR', repk=tsca)
    call dismoi('NB_MA_MAILLA', nomma, 'MAILLAGE', repi=nbma)
    call dismoi('NB_NO_MAILLA', nomma, 'MAILLAGE', repi=nbma)
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
            call dismoi('NB_MA_MAILLA', nomma, 'MAILLAGE', repi=nbma)
            AS_ALLOCATE(vi=indic_maille, size=nbma)
!
            do igr = 1, ngroup
                call jeexin(jexnom(nomma//'.GROUPEMA', group(igr)), iret)
                if (iret .eq. 0) then
                    valk = group(igr)
                    call utmess('A', 'PREPOST5_31', sk=valk)
                else
                    call jelira(jexnom(nomma//'.GROUPEMA', group(igr)), 'LONMAX', nbn)
                    call jeveuo(jexnom(nomma//'.GROUPEMA', group(igr)), 'L', iad)
                    do in = 1, nbn
                        ima = zi(iad-1+in)
                        indic_maille(ima) = 1
                    end do
                endif
            end do
!
            nbtrou = 0
            do ima = 1, nbma
                if (indic_maille(ima) .ne. 0) nbtrou = nbtrou + 1
            end do
            if (nbtrou .eq. 0) then
                call utmess('F', 'CHAMPS_4')
            endif
!
!
            call wkvect(litrou, 'V V I', nbtrou, itbma)
!         --- RANGEMENT DES NUMEROS DE MAILLES ---
            lma = 0
            do ima = 1, nbma
                if (indic_maille(ima) .ne. 0) then
                    lma = lma + 1
                    zi(itbma-1+lma) = ima
                endif
            end do
            call cesred(cesz, nbtrou, zi(itbma), 0, [k8bid],&
                        'V', cesz)
            AS_DEALLOCATE(vi=indic_maille)
        endif
!
! -- 2EME ETAPE : RECUPERATION DU NUMCMP QUI NOUS INTERESSE
!
        call jeveuo(cesz//'.CESD', 'L', jcesd)
        call jeveuo(cesz//'.CESL', 'L', jcesl)
        call jeveuo(cesz//'.CESC', 'L', vk8=cesc)
        call jeveuo(cesz//'.CESV', 'L', jcesv)
!
        ncmpmx = zi(jcesd-1+2)
!
        do icmp = 1, ncmpmx
            if (cesc(icmp) .eq. nocmp) then
                numcmp=icmp
                goto 6
            endif
        end do
!
  6     continue
!
! -- 3EME ETAPE : RECUPERATION DE LA LONGUEUR DU VECTEUR DE VALEURS
!                   UTILES - ON GARDE QUE LES VALEURS DE LA CMP REMPLIES
!                   ET CREATION DES VECTEURS
!
        nbval=0
        nbma = zi(jcesd-1+1)
!
        do ima = 1, nbma
!
!
            nbpt=zi(jcesd-1+5+4*(ima-1)+1)
            nbsp=zi(jcesd-1+5+4*(ima-1)+2)
!
            do ipt = 1, nbpt
                do isp = 1, nbsp
                    call cesexi('C', jcesd, jcesl, ima, ipt,&
                                isp, numcmp, iad)
                    if (iad .gt. 0) nbval=nbval+1
                end do
            end do
        end do
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
        do ima = 1, nbma
!
!
            nbpt=zi(jcesd-1+5+4*(ima-1)+1)
            nbsp=zi(jcesd-1+5+4*(ima-1)+2)
!
            do ipt = 1, nbpt
                do isp = 1, nbsp
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
                end do
            end do
        end do
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
            call dismoi('NB_NO_MAILLA', nomma, 'MAILLAGE', repi=nbno)
            AS_ALLOCATE(vi=indic_noeud, size=nbno)
!
            do igr = 1, ngroup
                call jeexin(jexnom(nomma//'.GROUPENO', group(igr)), iret)
                if (iret .eq. 0) then
                    valk = group(igr)
                    call utmess('A', 'PREPOST5_31', sk=valk)
                else
                    call jelira(jexnom(nomma//'.GROUPENO', group(igr)), 'LONMAX', nbn)
                    call jeveuo(jexnom(nomma//'.GROUPENO', group(igr)), 'L', iad)
                    do in = 1, nbn
                        ino = zi(iad-1+in)
                        indic_noeud(ino) = 1
                    end do
                endif
            end do
!
            nbtrou = 0
            do ino = 1, nbno
                if (indic_noeud(ino) .ne. 0) nbtrou = nbtrou + 1
            end do
            if (nbtrou .eq. 0) then
                call utmess('F', 'CHAMPS_5')
            endif
!
!
            call wkvect(litrou, 'V V I', nbtrou, itbma)
!         --- RANGEMENT DES NUMEROS DE NOEUDS ---
            lma = 0
            do ino = 1, nbno
                if (indic_noeud(ino) .ne. 0) then
                    lma = lma + 1
                    zi(itbma-1+lma) = ino
                endif
            end do
            call cnsred(cesz, nbtrou, zi(itbma), 0, [k8bid],&
                        'V', cesz)
            AS_DEALLOCATE(vi=indic_noeud)
        endif
!
!
! -- 2EME ETAPE : RECUPERATION DU NUMCMP QUI NOUS INTERESSE
!
        call jeveuo(cesz//'.CNSD', 'L', vi=cnsd)
        call jeveuo(cesz//'.CNSC', 'L', vk8=cnsc)
        call jeveuo(cesz//'.CNSL', 'L', jcnsl)
        call jeveuo(cesz//'.CNSV', 'L', jcnsv)
!
        ncmpmx = cnsd(2)
!
        if (numcmp .lt. 0) then
            goto 60
        endif
        do icmp = 1, ncmpmx
            if (cnsc(icmp) .eq. nocmp) then
                numcmp=icmp
                goto 60
            endif
        end do
!
!       SI LA COMPOSANTE DEMANDEE N EXISTE PAS, ERREUR FATALE
        call utmess('F', 'CHAMPS_3', sk=nocmp)
!
 60     continue
!
!
! -- 3EME ETAPE : RECUPERATION DE LA LONGUEUR DU VECTEUR DE VALEURS
!                   UTILES - ON GARDE QUE LES VALEURS DE LA CMP REMPLIES
!                   ET CREATION DES VECTEURS
!
        nbval=0
        nbno = cnsd(1)
!
        if (numcmp .lt. 0) then
            do icmp = 1, ncmpmx
                do ino = 1, nbno
                    if (zl(jcnsl-1+(ino-1)*ncmpmx+icmp)) nbval=nbval+ 1
                end do
            end do
        else
            do ino = 1, nbno
                if (zl(jcnsl-1+(ino-1)*ncmpmx+numcmp)) nbval=nbval+1
            end do
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
        do ino = 1, nbno
            if (numcmp .gt. 0) then
                cmpmin=numcmp
                cmpmax=numcmp
            else
                cmpmin=1
                cmpmax=ncmpmx
            endif
            do icmp = cmpmin, cmpmax
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
                        if (numcmp .lt. 0) zk8(jcmp-1+ival)=cnsc(icmp)
                    endif
                endif
            end do
        end do
!
    else
        ASSERT(.false.)
    endif
!
!
999 continue
    call jedema()
end subroutine

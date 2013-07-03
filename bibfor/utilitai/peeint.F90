subroutine peeint(resu, modele, nbocc)
    implicit   none
#include "jeveux.h"
!
#include "asterc/getvid.h"
#include "asterc/getvis.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterc/indik8.h"
#include "asterfort/alchml.h"
#include "asterfort/chpchd.h"
#include "asterfort/chsut1.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnscno.h"
#include "asterfort/codent.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismlg.h"
#include "asterfort/dismoi.h"
#include "asterfort/exlima.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/nopar2.h"
#include "asterfort/peecal.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsorac.h"
#include "asterfort/rsutnu.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: nbocc
    character(len=8) :: modele
    character(len=19) :: resu
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     OPERATEUR   POST_ELEM
!     TRAITEMENT DU MOT CLE-FACTEUR "INTEGRALE"
!     ------------------------------------------------------------------
!
    integer :: iret, nbcmp, nzero, ibid, nbordr, iocc, jnuma, nbma, ncmpm
    integer :: jcmp, n1, numa, nr, np, nc, im, ni, no, jno, jin, numo, i
    integer :: nbgma, jgma, nma, jma, igm, nbpa1, nbpa2, nn, inum, nli, nlo
    integer :: nd, ib, jlicmp, jlicm2, jlicm1, nucmp, jcpini
    parameter(nzero=0,nbpa1=4,nbpa2=2)
    real(kind=8) :: rbid, prec, inst
    complex(kind=8) :: cbid
    character(len=8) :: k8b, kbid, mailla, resuco, crit, nopar
    character(len=4) :: tych, ki, exirdm
    character(len=8) :: nomgd, tout, grpma, maille, typpa1(nbpa1), typpa2(nbpa2)
    parameter(tout='TOUT',grpma='GROUP_MA',maille='MAILLE')
    character(len=16) :: nompa1(nbpa1), nompa2(nbpa2), optio2
    character(len=19) :: knum, cham, kins, lisins, chamg, celmod, ligrel, tmpcha
    character(len=19) :: cham2, cham3, chamtm, ligtmp
    character(len=24) :: nomcha, valk2(5)
    logical :: exiord, toneut
    integer :: iarg
    data nompa1/'NOM_CHAM','NUME_ORDRE','INST','VOL'/
    data typpa1/'K16','I','R','R'/
    data nompa2/'CHAM_GD','VOL'/
    data typpa2/'K16','R'/
!     ------------------------------------------------------------------
!
    call jemarq()
!
!     --- RECUPERATION DU MAILLAGE ET DU NOMBRE DE MAILLES
    call dismoi('F', 'NOM_MAILLA', modele, 'MODELE', ibid,&
                mailla, iret)
    call dismoi('F', 'NB_MA_MAILLA', mailla, 'MAILLAGE', nbma,&
                k8b, iret)
!
!
!     --- RECUPERATION DU RESULTAT ET DU NUMERO D'ORDRE
    call getvid(' ', 'RESULTAT', 1, iarg, 1,&
                resuco, nr)
    call getvr8(' ', 'PRECISION', 1, iarg, 1,&
                prec, np)
    call getvtx(' ', 'CRITERE', 1, iarg, 1,&
                crit, nc)
    call getvr8(' ', 'INST', 1, iarg, 0,&
                rbid, ni)
    call getvis(' ', 'NUME_ORDRE', 1, iarg, 0,&
                ibid, no)
    call getvid(' ', 'LIST_INST', 1, iarg, 0,&
                kbid, nli)
    call getvid(' ', 'LIST_ORDRE', 1, iarg, 0,&
                kbid, nlo)
    call getvid(' ', 'CHAM_GD', 1, iarg, 1,&
                chamg, nd)
!
!     --- CREATION DE LA TABLE
    call tbcrsd(resu, 'G')
    if (nr .ne. 0) then
        call tbajpa(resu, nbpa1, nompa1, typpa1)
    else
        call tbajpa(resu, nbpa2, nompa2, typpa2)
    endif
!
    knum = '&&PEEINT.NUME_ORDRE'
    kins = '&&PEEINT.INST'
    exiord=.false.
    toneut=.false.
!
    if (nd .ne. 0) then
        nbordr = 1
        call wkvect(knum, 'V V I', nbordr, jno)
        zi(jno) = 1
        exiord=.true.
    else
        if (no .ne. 0) then
            exiord=.true.
            nbordr=-no
            call wkvect(knum, 'V V I', nbordr, jno)
            call getvis(' ', 'NUME_ORDRE', 1, iarg, nbordr,&
                        zi(jno), iret)
        endif
!
        if (ni .ne. 0) then
            nbordr=-ni
            call wkvect(kins, 'V V R', nbordr, jin)
            call getvr8(' ', 'INST', 1, iarg, nbordr,&
                        zr(jin), iret)
        endif
!
        if (nli .ne. 0) then
            call getvid(' ', 'LIST_INST', 1, iarg, 1,&
                        lisins, iret)
            call jeveuo(lisins // '.VALE', 'L', jin)
            call jelira(lisins // '.VALE', 'LONMAX', nbordr, kbid)
        endif
!
        if (nlo .ne. 0) then
            exiord=.true.
            call getvid(' ', 'LIST_ORDRE', 1, iarg, 1,&
                        lisins, iret)
            call jeveuo(lisins // '.VALE', 'L', jno)
            call jelira(lisins // '.VALE', 'LONMAX', nbordr, kbid)
        endif
!
        nn=nlo+nli+no+ni
        if (nn .eq. 0) then
            exiord=.true.
            call rsutnu(resuco, ' ', 0, knum, nbordr,&
                        prec, crit, iret)
            call jeveuo(knum, 'L', jno)
        endif
    endif
!
!
!     --- ON PARCOURT LES OCCURENCES DU MOT CLE 'INTEGRALE':
!     =====================================================
    if (nr .eq. 0) then
        tmpcha='TMP_CHAMP_GD'
        call copisd('CHAMP', 'V', chamg, tmpcha)
    endif
!
    do 10 iocc = 1, nbocc
!
!     --- VERIFICATION SI ON VA TRAITER DES ELEMENTS DE STRUCTURE
!     ===========================================================
!
        ligtmp='&&PEEINT.LIGTMP'
        call exlima('INTEGRALE', iocc, 'V', modele, ligtmp)
        call dismlg('EXI_RDM', ligtmp, ibid, exirdm, iret)
        if (exirdm .eq. 'OUI') then
            call u2mess('F', 'UTILITAI8_60')
        endif
!
!
!     --- BOUCLE SUR LES NUMEROS D'ORDRE:
!     ===================================
!
        do 5 inum = 1, nbordr
!
!         --- SI RESULTAT ---
!         --- NUME_ORDRE, INST ---
            if (nr .ne. 0) then
                if (exiord) then
                    numo=zi(jno+inum-1)
                    call rsadpa(resuco, 'L', 1, 'INST', numo,&
                                0, jin, kbid)
                    inst=zr(jin)
                else
                    inst=zr(jin+inum-1)
                    call rsorac(resuco, 'INST', 0, zr(jin+inum-1), kbid,&
                                cbid, prec, crit, numo, nbordr,&
                                iret)
                endif
!
!         --- CHAMP DU POST-TRAITEMENT
                call getvtx('INTEGRALE', 'NOM_CHAM', iocc, iarg, 1,&
                            nomcha, iret)
                if (iret .eq. 0) call u2mess('F', 'POSTELEM_4')
!
                call rsexch('F', resuco, nomcha, numo, cham2,&
                            iret)
!
            else
!         --- SI CHAM_GD ---
                numo = nbordr
                nomcha = chamg
                cham2 = tmpcha
!
            endif
!
            call dismoi('C', 'TYPE_CHAMP', cham2, 'CHAMP', ibid,&
                        tych, iret)
            call dismoi('C', 'NOM_GD', cham2, 'CHAMP', ibid,&
                        nomgd, iret)
!
            if (nomgd(6:6) .eq. 'C') goto 10
!
            if (tych(1:2) .ne. 'EL') then
!
!          --- 1. TRANSFORMATION DU CHAMP EN CHAMP NEUTRE:
!              - CHANGEMENT DE LA GRANDEUR EN NEUT_R
!              - CHAMGEMENT DES COMPOSANTES EN X1,X2,X3,...
                toneut=.true.
                chamtm='&&PEEINT.CHS1'
                call cnocns(cham2, 'V', chamtm)
                call jeveuo(chamtm//'.CNSC', 'L', jlicmp)
                call jelira(chamtm//'.CNSC', 'LONMAX', ncmpm, k8b)
                call jedetr('&&PEEINT.CMP1')
                call wkvect('&&PEEINT.CMP1', 'V V K8', ncmpm, jlicm1)
                call jedetr('&&PEEINT.CMP2')
                call wkvect('&&PEEINT.CMP2', 'V V K8', ncmpm, jlicm2)
                do 15 i = 1, ncmpm
                    call codent(i, 'G', ki)
                    zk8(jlicm2+i-1)='X'//ki(1:len(ki))
                    zk8(jlicm1+i-1)=zk8(jlicmp+i-1)
15              continue
                call chsut1(chamtm, 'NEUT_R', ncmpm, zk8(jlicm1), zk8(jlicm2),&
                            'V', chamtm)
                cham3='&&PEEINT.CHAM_3'
                call cnscno(chamtm, ' ', 'NON', 'V', cham3,&
                            'F', ibid)
                call detrsd('CHAM_NO_S', chamtm)
!
!           --- 2. CHANGEMENT DE DISCRETISATION : NOEU -> ELGA
                optio2 ='TOU_INI_ELGA'
                call dismoi('C', 'NOM_GD', cham3, 'CHAMP', ibid,&
                            nomgd, iret)
                nopar = nopar2(optio2,nomgd,'OUT')
                celmod = '&&PEEINT.CELMOD'
                ligrel = modele//'.MODELE'
                call alchml(ligrel, optio2, nopar, 'V', celmod,&
                            ib, ' ')
                if (ib .ne. 0) then
                    valk2(1)=ligrel
                    valk2(2)=nopar
                    valk2(3)=optio2
                    call u2mesk('F', 'UTILITAI3_23', 3, valk2)
                endif
                cham='&&CHPCHD.CHAM'
                call chpchd(cham3, 'ELGA', celmod, 'OUI', 'V',&
                            cham)
                call detrsd('CHAMP', celmod)
                call detrsd('CHAMP', cham3)
!
            else
                cham=cham2
            endif
!
            call dismoi('C', 'TYPE_CHAMP', cham, 'CHAMP', ibid,&
                        tych, iret)
!
!         --- COMPOSANTES DU POST-TRAITEMENT
            call getvtx('INTEGRALE', 'NOM_CMP', iocc, iarg, nzero,&
                        k8b, nbcmp)
            nbcmp=-nbcmp
            call wkvect('&&PEEINT.CMP', 'V V K8', nbcmp, jcmp)
            call getvtx('INTEGRALE', 'NOM_CMP', iocc, iarg, nbcmp,&
                        zk8(jcmp), iret)
!
!         COMPOSANTES A AFFICHER DANS LA TABLE: ZK8(JCPINI)
            call wkvect('&&PEEINT.CMP_INIT', 'V V K8', nbcmp, jcpini)
            do 50 i = 1, nbcmp
                zk8(jcpini+i-1)=zk8(jcmp+i-1)
50          continue
!
            if (toneut) then
                do 55 i = 1, nbcmp
                    nucmp=indik8(zk8(jlicm1),zk8(jcpini+i-1),1,ncmpm)
                    zk8(jcmp+i-1)=zk8(jlicm2+nucmp-1)
55              continue
            endif
!
!         --- CALCUL ET STOCKAGE DES MOYENNE : MOT-CLE 'TOUT'
            call getvtx('INTEGRALE', 'TOUT', iocc, iarg, nzero,&
                        k8b, iret)
            if (iret .ne. 0) then
                call peecal(tych, resu, nomcha, tout, tout,&
                            modele, nr, cham, nbcmp, zk8(jcmp),&
                            zk8(jcpini), numo, inst, iocc)
            endif
!
!         --- CALCUL ET STOCKAGE DES MOYENNES : MOT-CLE 'GROUP_MA'
            call getvtx('INTEGRALE', 'GROUP_MA', iocc, iarg, nzero,&
                        k8b, n1)
            if (n1 .ne. 0) then
                nbgma=-n1
                call wkvect('&&PEEINT_GMA', 'V V K24', nbgma, jgma)
                call getvtx('INTEGRALE', 'GROUP_MA', iocc, iarg, nbgma,&
                            zk24(jgma), n1)
                do 20 igm = 1, nbgma
                    call jelira(jexnom(mailla//'.GROUPEMA', zk24(jgma+ igm-1)), 'LONMAX', nma,&
                                k8b)
                    call jeveuo(jexnom(mailla//'.GROUPEMA', zk24(jgma+ igm-1)), 'L', jnuma)
                    call peecal(tych, resu, nomcha, grpma, zk24(jgma+igm- 1),&
                                modele, nr, cham, nbcmp, zk8(jcmp),&
                                zk8(jcpini), numo, inst, iocc)
20              continue
                call jedetr('&&PEEINT_GMA')
            endif
!
!         --- CALCUL ET STOCKAGE DES MOYENNES : MOT-CLE 'MAILLE'
            call getvtx('INTEGRALE', 'MAILLE', iocc, iarg, nzero,&
                        k8b, n1)
            if (n1 .ne. 0) then
                nma=-n1
                call wkvect('&&PEEINT_MAIL', 'V V K8', nma, jma)
                call getvtx('INTEGRALE', 'MAILLE', iocc, iarg, nma,&
                            zk8(jma), n1)
                do 30 im = 1, nma
                    call jenonu(jexnom(mailla//'.NOMMAI', zk8(jma+im-1) ), numa)
                    call peecal(tych, resu, nomcha, maille, zk8(jma+im-1),&
                                modele, nr, cham, nbcmp, zk8(jcmp),&
                                zk8(jcpini), numo, inst, iocc)
30              continue
                call jedetr('&&PEEINT_MAIL')
            endif
!
            call jedetr('&&PEEINT.CMP')
            call jedetr('&&PEEINT.CMP_INIT')
!
!
!
 5      end do
!
10  end do
!
    if (nr .eq. 0) then
        call detrsd('CHAMP', tmpcha)
    endif
!
    call jedema()
!
end subroutine

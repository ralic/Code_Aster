subroutine rc36th(noma, nbma, listma, chth, iocs,&
                  nbths, liths)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/cescre.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/rcveri.h"
#include "asterfort/reliem.h"
#include "asterfort/tbexip.h"
#include "asterfort/tbexv1.h"
#include "asterfort/tbliva.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: nbma, listma(*), iocs, nbths, liths(*)
    character(len=8) :: noma
    character(len=24) :: chth(*)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3600
!     RECUPERATION DES DONNEES DE "RESU_THER"
!
!     ------------------------------------------------------------------
!
    integer :: nbreth, nbcmp, iths, ith, iret, n1, n2, iocc, ierd, ino, iad, in
    integer :: ima, im, jmail, jnoeu, nbmail, nbmat, nbtou, nbnoeu, jcesd, jcesl
    integer :: jcesv, nbpt, decal, i, ipt, jconx1, jconx2, icmp, it1, vali(4)
    integer :: jinst, jther, jmoye, jabsc, nbabsc, nbinst, ibid
    parameter   ( nbcmp = 2 )
    real(kind=8) :: inst, epsi, vmoy, ta, tint, text, vale(2), prec(2)
    complex(kind=8) :: cbid
    logical :: exist
    character(len=8) :: k8b, nomgd, licmp(nbcmp), tbther, tbmoye, kioc, crit(2)
    character(len=16) :: motclf, motcls(2), typmcs(2), motcln(2), typmcn(2)
    character(len=16) :: nopara(2)
    character(len=19) :: chams0
    character(len=24) :: instan, abscur, mesmai, mesnoe, nojvth, nojvmy, valk(7)
! DEB ------------------------------------------------------------------
    call jemarq()
!
    motclf = 'RESU_THER'
    call getfac(motclf, nbreth)
!
    epsi = 1.0d-06
    prec(1) = 1.0d-06
    prec(2) = 1.0d-06
    crit(1) = 'RELATIF'
    crit(2) = 'RELATIF'
    instan = '&&RC36TH.INSTANT'
    abscur = '&&RC36TH.ABSC_CURV'
!
    nomgd = 'RCCM_K'
    licmp(1) = 'TB_TEMP'
    licmp(2) = 'TB_MOYE'
!
    mesmai = 'RC36TH.MES_MAILLES'
    motcls(1) = 'GROUP_MA'
    motcls(2) = 'MAILLE'
    typmcs(1) = 'GROUP_MA'
    typmcs(2) = 'MAILLE'
    mesnoe = 'RC36TH.MES_NOEUDS'
    motcln(1) = 'GROUP_NO'
    motcln(2) = 'NOEUD'
    typmcn(1) = 'GROUP_NO'
    typmcn(2) = 'NOEUD'
!
! --- POUR CHAQUE SITUATION, UN CHAMELEM ELNO EST CREE
!
!     PAS DE SURCHARGE AUTORISEE POUR REMPLIR CE CHAMP
!
    call codent(iocs, 'D0', k8b)
    chams0 = 'RC36TH.CHAM'//k8b
    call jeexin(chams0, iret)
    if (iret .eq. 0) then
        call cescre('V', chams0, 'ELNO', noma, nomgd,&
                    nbcmp, licmp, [-1], [-1], [-nbcmp])
        chth(iocs) = chams0
    else
        call utmess('F', 'POSTRCCM_19')
    endif
!
    call jeveuo(chams0//'.CESD', 'L', jcesd)
    call jeveuo(chams0//'.CESL', 'E', jcesl)
    call jeveuo(chams0//'.CESV', 'E', jcesv)
!
    call dismoi('F', 'NB_MA_MAILLA', noma, 'MAILLAGE', nbmat,&
                k8b, ierd)
    call jeveuo(noma//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
!
    do 10, it1 = 1, nbths, 1
!
    do 12, iocc = 1, nbreth, 1
!
    call getvis(motclf, 'NUME_RESU_THER', iocc=iocc, scal=ith, nbret=n1)
!
    iths = liths(it1)
    if (ith .eq. iths) goto 14
12  continue
    vali(1) = iocc
    vali(2) = iths
    call utmess('F', 'POSTRCCM_23', ni=2, vali=vali)
14  continue
!
    call getvid(motclf, 'TABL_RESU_THER', iocc=iocc, scal=tbther, nbret=n1)
!        ON VERIFIE L'ORDRE DES NOEUDS DANS LA TABLE
    call rcveri(tbther)
!
    call tbexip(tbther, 'INST', exist, k8b)
    if (.not. exist) then
        valk(1) = tbther
        valk(2) = 'INST'
        call utmess('F', 'POSTRCCM_1', nk=2, valk=valk)
    endif
    call tbexip(tbther, 'ABSC_CURV', exist, k8b)
    if (.not. exist) then
        valk(1) = tbther
        valk(2) = 'ABSC_CURV'
        call utmess('F', 'POSTRCCM_1', nk=2, valk=valk)
    endif
    call tbexv1(tbther, 'INST', instan, 'V', nbinst,&
                k8b)
    call jeveuo(instan, 'L', jinst)
    call tbexv1(tbther, 'ABSC_CURV', abscur, 'V', nbabsc,&
                k8b)
    call jeveuo(abscur, 'L', jabsc)
!
    call getvid(motclf, 'TABL_MOYE_THER', iocc=iocc, scal=tbmoye, nbret=n1)
!
    call tbexip(tbmoye, 'INST', exist, k8b)
    if (.not. exist) then
        valk(1) = tbmoye
        valk(2) = 'INST'
        call utmess('F', 'POSTRCCM_1', nk=2, valk=valk)
    endif
!
    exist = .false.
    call codent(iocc, 'D0', kioc)
    nojvmy = '&&RC36TH_MOYENE_'//kioc
    call jeexin(nojvmy, iret)
    if (iret .eq. 0) then
        call wkvect(nojvmy, 'V V R', 2*nbinst, jmoye)
    else
        exist = .true.
    endif
    nojvth = '&&RC36TH_TEMPER_'//kioc
    if (iret .eq. 0) then
        call wkvect(nojvth, 'V V R', 2*nbinst, jther)
    else
        exist = .true.
    endif
    if (exist) goto 22
!
    do 20 i = 1, nbinst
        inst = zr(jinst+i-1)
!
! --------- ON RECUPERE TEMP_INT, TEMP_EXT
!
        nopara(1) = 'INST'
        nopara(2) = 'ABSC_CURV'
        vale(1) = inst
        vale(2) = zr(jabsc)
        call tbliva(tbther, 2, nopara, ibid, vale,&
                    cbid, k8b, crit, prec, 'TEMP',&
                    k8b, ibid, tint, cbid, k8b,&
                    iret)
        if (iret .ne. 0) then
            valk (1) = tbther
            valk (2) = 'TEMP'
            valk (3) = nopara(1)
            valk (4) = nopara(2)
            call utmess('F', 'POSTRCCM_2', nk=4, valk=valk, nr=2,&
                        valr=vale)
        endif
        vale(2) = zr(jabsc+nbabsc-1)
        call tbliva(tbther, 2, nopara, ibid, vale,&
                    cbid, k8b, crit, prec, 'TEMP',&
                    k8b, ibid, text, cbid, k8b,&
                    iret)
        if (iret .ne. 0) then
            valk (1) = tbther
            valk (2) = 'TEMP'
            valk (3) = nopara(1)
            valk (4) = nopara(2)
            call utmess('F', 'POSTRCCM_2', nk=4, valk=valk, nr=2,&
                        valr=vale)
        endif
        zr(jther-1+2*(i-1)+1) = tint
        zr(jther-1+2*(i-1)+2) = text
!
! --------- ON RECUPERE LES MOYENNES
!
        nopara(1) = 'INST'
        nopara(2) = 'QUANTITE'
        call tbliva(tbmoye, 2, nopara, ibid, inst,&
                    cbid, 'MOMENT_0', 'RELATIF', epsi, 'TEMP',&
                    k8b, ibid, ta, cbid, k8b,&
                    iret)
        if (iret .ne. 0) then
            valk (1) = tbmoye
            valk (2) = 'TEMP'
            valk (3) = nopara(1)
            valk (4) = nopara(2)
            valk (5) = 'MOMENT_0'
            call utmess('F', 'POSTRCCM_16', nk=5, valk=valk, sr=inst)
        endif
        call tbliva(tbmoye, 2, nopara, ibid, inst,&
                    cbid, 'MOMENT_1', 'RELATIF', epsi, 'TEMP',&
                    k8b, ibid, vmoy, cbid, k8b,&
                    iret)
        if (iret .ne. 0) then
            valk (1) = tbmoye
            valk (2) = 'TEMP'
            valk (3) = nopara(1)
            valk (4) = nopara(2)
            valk (5) = 'MOMENT_1'
            call utmess('F', 'POSTRCCM_16', nk=5, valk=valk, sr=inst)
        endif
        zr(jmoye-1+2*(i-1)+1) = ta
        zr(jmoye-1+2*(i-1)+2) = vmoy
20  continue
22  continue
!
    call getvtx(motclf, 'TOUT', iocc=iocc, scal=k8b, nbret=nbtou)
!
    if (nbtou .ne. 0) then
        nbmail = nbmat
        call wkvect(mesmai, 'V V I', nbmail, jmail)
        do 30 ima = 1, nbmail
            zi(jmail+ima-1) = ima
30      continue
    else
        call reliem(' ', noma, 'NU_MAILLE', motclf, iocc,&
                    2, motcls, typmcs, mesmai, nbmail)
        call jeveuo(mesmai, 'L', jmail)
    endif
!
    call getvtx(motclf, 'GROUP_NO', iocc=iocc, nbval=0, nbret=n1)
    call getvtx(motclf, 'NOEUD', iocc=iocc, nbval=0, nbret=n2)
    if (n1+n2 .ne. 0) then
        call reliem(' ', noma, 'NU_NOEUD', motclf, iocc,&
                    2, motcln, typmcn, mesnoe, nbnoeu)
        call jeveuo(mesnoe, 'L', jnoeu)
    else
        nbnoeu = 0
    endif
!
    if (nbnoeu .eq. 0) then
        do 100 im = 1, nbmail
            ima = zi(jmail+im-1)
            nbpt = zi(jcesd-1+5+4*(ima-1)+1)
            decal= zi(jcesd-1+5+4*(ima-1)+4)
            do 110,ipt = 1,nbpt
            icmp = 1
            iad = decal + (ipt-1)*nbcmp + icmp
            if (.not.zl(jcesl-1+iad)) then
                zl (jcesl-1+iad) = .true.
                zk24(jcesv-1+iad) = nojvth
            else
                vali(1) = iocs
                vali(2) = ima
                call utmess('F', 'POSTRCCM_24', ni=2, vali=vali)
            endif
            icmp = 2
            iad = decal + (ipt-1)*nbcmp + icmp
            zl (jcesl-1+iad) = .true.
            zk24(jcesv-1+iad) = nojvmy
110          continue
100      continue
    else
        do 200 im = 1, nbmail
            ima = zi(jmail+im-1)
            nbpt = zi(jcesd-1+5+4*(ima-1)+1)
            decal= zi(jcesd-1+5+4*(ima-1)+4)
            do 210,ipt = 1,nbpt
            ino = zi(jconx1-1+zi(jconx2+ima-1)+ipt-1)
            do 220,in = 1,nbnoeu
            if (zi(jnoeu+in-1) .eq. ino) then
                icmp = 1
                iad = decal + (ipt-1)*nbcmp + icmp
                if (.not.zl(jcesl-1+iad)) then
                    zl (jcesl-1+iad) = .true.
                    zk24(jcesv-1+iad) = nojvth
                else
                    vali(1) = iocs
                    vali(2) = ima
                    vali(3) = ino
                    call utmess('F', 'POSTRCCM_25', ni=3, vali=vali)
                endif
                icmp = 2
                iad = decal + (ipt-1)*nbcmp + icmp
                zl (jcesl-1+iad) = .true.
                zk24(jcesv-1+iad) = nojvmy
                goto 210
            endif
220          continue
210          continue
200      continue
    endif
!
    call jedetr(instan)
    call jedetr(abscur)
    call jedetr(mesmai)
    call jedetr(mesnoe)
!
    10 end do
!
! --- VERIF QUE TOUTES LES MAILLES ANALYSEES SONT AFFECTEES
!
    do 300 im = 1, nbma
        ima = listma(im)
        nbpt = zi(jcesd-1+5+4*(ima-1)+1)
        decal= zi(jcesd-1+5+4*(ima-1)+4)
        do 310,ipt = 1,nbpt
        do 320 icmp = 1, 2
            iad = decal + (ipt-1)*nbcmp + icmp
            if (.not.zl(jcesl-1+iad)) then
                vali(1) = iocs
                vali(2) = ima
                call utmess('F', 'POSTRCCM_24', ni=2, vali=vali)
            endif
320      continue
310      continue
300  end do
!
    call jedema()
end subroutine

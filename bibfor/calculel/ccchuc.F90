subroutine ccchuc(resuin, resuou, chin, nchout, crit,&
                  nf, nfor, lisord, nbordr)
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/ccchcf.h"
#include "asterfort/ccchci.h"
#include "asterfort/ccchcr.h"
#include "asterfort/celces.h"
#include "asterfort/cescel.h"
#include "asterfort/cescrm.h"
#include "asterfort/cesexi.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnscno.h"
#include "asterfort/cnscre.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exlim1.h"
#include "asterfort/gnomsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeundf.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesi.h"
#include "asterfort/u2mesk.h"
#include "asterfort/wkvect.h"
    integer :: nbordr, nchout, nf
    character(len=8) :: resuin, resuou, nfor(nf)
    character(len=16) :: chin, crit
    character(len=19) :: lisord
! person_in_charge: mathieu.courtois at edf.fr
! ----------------------------------------------------------------------
!  CALC_CHAMP - TRAITEMENT DE CHAM_UTIL - CRITERE
!  -    -                     --   -      -
! ----------------------------------------------------------------------
! IN  :
!   RESUIN K8    NOM DE LA SD IN
!   RESUOU K8    NOM DE LA SD OUT
!   CHIN   K16   NOM DU CHAMP EN ENTREE
!   CHOUT  K16   NOM DU CHAMP EN SORTIE
!   CRIT   K16   NOM DU CRITERE A CALCULER (UTILISE SI NF=0)
!   NF     I     NOMBRE DE FORMULES
!   NFOR   K8(*) NOMS DES FORMULES
!   LISORD K19   NOM DE LA LISTE DES NUMEROS D'ORDRE
!   NBORDR I     NOMBRE DE NUMEROS D'ORDRE
! ----------------------------------------------------------------------
    integer :: jordr, i, iret, ima, iordr, ipt, isp, icmp, nbaj
    integer :: jchsd, jchsl, jchsv, jchsc, jchrd, jchrl, jchrv, jval
    integer :: jvres, iad, ibid, jcmp, jlima, jlast, ichk, iv
    integer :: nbma, nbpt, nbsp, nbcmp, nbcmpp, nbcmpr
    integer :: vali(3)
    logical :: idem
    real(kind=8) :: rbid
    character(len=2) :: cnum
    character(len=4) :: typces
    character(len=8) :: ma, model, nomgd
    character(len=16) :: chout, typs, valk(3)
    character(len=19) :: chs, chr, wkin, ligrel
    character(len=19) :: wkout, wkcmp, wlima, wlast
    character(len=24) :: chps, chres, noojb
    character(len=8) :: nompro
    data nompro /'&&CCCHUC'/
!     ----- FIN  DECLARATIONS ------------------------------------------
!
    call jemarq()
    typces = '    '
    nomgd = '        '
    chout = '                '
    chs = nompro//'.CHSIN'
    chr = nompro//'.CHSOUT'
    chres = nompro//'.CHRES'
    wkin = nompro//'.VALIN'
    wkcmp = nompro//'.CMPS'
    wkout = nompro//'.VALRES'
    wlima = nompro//'.WLIMA'
    wlast = nompro//'.WLAST'
    ligrel = 'NOT_INIT'
    call codent(nchout, 'D0', cnum)
    call assert((nf.eq.0 .and. crit.ne.' ') .or.(nf.ne.0 .and. crit.eq.' '))
!
!     RECUPERATION DE LA LISTE DE NUMEROS D'ORDRE
    call jeveuo(lisord, 'L', jordr)
!
! --- BOUCLE SUR LES NUMEROS D'ORDRE
    do 10 i = 1, nbordr
        iordr = zi(jordr-1+i)
!       TEST L'EXISTENCE DANS RESUIN OU RESUOU
        call rsexch(' ', resuin, chin, iordr, chps,&
                    iret)
        if (iret .ne. 0) then
            if (resuin .eq. resuou) then
                valk(1) = chin
                valk(2) = resuin
                vali(1) = iordr
                call u2mesg('F', 'CHAMPS_6', 2, valk, 1,&
                            vali, 0, rbid)
            else
                call rsexch(' ', resuou, chin, iordr, chps,&
                            iret)
                if (iret .ne. 0) then
                    valk(1) = chin
                    valk(2) = resuin
                    valk(3) = resuou
                    vali(1) = iordr
                    call u2mesg('F', 'CHAMPS_9', 3, valk, 1,&
                                vali, 0, rbid)
                endif
            endif
        endif
        if (i .eq. 1) then
            call dismoi('F', 'NOM_GD', chps, 'CHAMP', ibid,&
                        nomgd, iret)
            call dismoi('F', 'TYPE_CHAMP', chps, 'CHAMP', ibid,&
                        typces, iret)
            call assert(typces.ne.'CART' .and. typces.ne.'RESL')
            chout = 'UT'//cnum//'_'//typces
        endif
        call dismoi('F', 'NOM_MAILLA', chps, 'CHAMP', ibid,&
                    ma, iret)
        if (nf .eq. 0) then
            call ccchci(crit, 'NBCMP', nbcmpr)
        else
            nbcmpr = nf
        endif
!
!       CHAM_NO_S OU CHAM_ELEM_S ?
        if (typces .eq. 'NOEU') then
!
! ------- TRAITEMENT DES CHAM_ELEM
            typs = 'CHAM_NO_S'
!
            call cnocns(chps, 'V', chs)
            call jeveuo(chs//'.CNSD', 'L', jchsd)
            call jeveuo(chs//'.CNSC', 'L', jchsc)
            call jeveuo(chs//'.CNSV', 'L', jchsv)
            call jeveuo(chs//'.CNSL', 'L', jchsl)
!
!         CREATION DU CHAM_NO_S RESULTAT
            call wkvect(wkcmp, 'V V K8', nbcmpr, jcmp)
            do 100 icmp = 1, nbcmpr
                call codent(icmp, 'G', cnum)
                zk8(jcmp-1+icmp) = 'X'//cnum
100          continue
            call cnscre(ma, 'NEUT_R', nbcmpr, zk8(jcmp), 'V',&
                        chr)
            call jedetr(wkcmp)
!
            call jeveuo(chr//'.CNSD', 'E', jchrd)
            call jeveuo(chr//'.CNSL', 'E', jchrl)
            call jeveuo(chr//'.CNSV', 'E', jchrv)
!         VECTEURS DE TRAVAIL DES VALEURS PAR COMPOSANTE
            nbcmp = zi(jchsd-1+2)
            call wkvect(wkin, 'V V R', nbcmp, jval)
            call wkvect(wkcmp, 'V V K8', nbcmp, jcmp)
            call wkvect(wkout, 'V V R', nbcmpr, jvres)
!
            nbpt = zi(jchsd-1+1)
            nbaj = 0
            do 110 ipt = 1, nbpt
                call jeundf(wkin)
                call jeundf(wkcmp)
                iv = 0
                do 112 icmp = 1, nbcmp
                    if (zl(jchsl-1+(ipt-1)*nbcmp+icmp)) then
                        iv = iv + 1
                        zr(jval-1+iv) = zr(jchsv-1+(ipt-1)*nbcmp+icmp)
                        zk8(jcmp-1+iv) = zk8(jchsc-1+icmp)
                    endif
112              continue
!
                if (nf .eq. 0) then
                    call ccchcr(crit, nomgd, iv, zr(jval), zk8(jcmp),&
                                nbcmpr, zr(jvres), ichk)
                else
                    call ccchcf(nfor, iv, zr(jval), zk8(jcmp), nbcmpr,&
                                zr(jvres), ichk)
                endif
                if (ichk .ne. 0) then
                    goto 110
                endif
!
                nbaj = nbaj + 1
                do 114 icmp = 1, nbcmpr
                    zl(jchrl-1+(ipt-1)*nbcmpr+icmp) = .true.
                    zr(jchrv-1+(ipt-1)*nbcmpr+icmp) = zr(jvres-1+icmp)
114              continue
110          continue
!
            vali(1) = iordr
            vali(2) = nbaj
            vali(3) = nbpt
            call u2mesi('I', 'CHAMPS_10', 3, vali)
!
!         STOCKAGE DU CHAMP
            call rsexch(' ', resuou, chout, iordr, chres,&
                        iret)
            if (iret .ne. 100) then
                valk(1) = chout
                valk(2) = resuou
                call u2mesk('F', 'CHAMPS_14', 2, valk)
            endif
            call cnscno(chr, ' ', 'UNUSED', 'G', chres,&
                        'F', iret)
            call assert(iret.eq.0)
            call rsnoch(resuou, chout, iordr)
!
            call detrsd(typs, chs)
            call detrsd(typs, chr)
            call jedetr(wkcmp)
            call jedetr(wkin)
            call jedetr(wkout)
!
! ------- ENDIF CHAM_NO
        else
!
! ------- TRAITEMENT DES CHAM_ELEM
            typs = 'CHAM_ELEM_S'
!
            if (i .eq. 1) then
                call dismoi('F', 'NOM_LIGREL', chps, 'CHAMP', ibid,&
                            ligrel, iret)
            endif
!
            call celces(chps, 'V', chs)
            call jeveuo(chs//'.CESD', 'L', jchsd)
            call jeveuo(chs//'.CESL', 'L', jchsl)
            call jeveuo(chs//'.CESV', 'L', jchsv)
            call jeveuo(chs//'.CESC', 'L', jchsc)
!
!         CREATION DU CHAM_ELEM_S RESULTAT
            call cescrm('V', chr, typces, 'NEUT_R', nbcmpr,&
                        ' ', chs)
!
            call jeveuo(chr//'.CESD', 'E', jchrd)
            call jeveuo(chr//'.CESL', 'E', jchrl)
            call jeveuo(chr//'.CESV', 'E', jchrv)
!         VECTEURS DE TRAVAIL DES VALEURS PAR COMPOSANTE
            nbcmp = zi(jchsd-1+2)
            call wkvect(wkin, 'V V R', nbcmp, jval)
            call wkvect(wkcmp, 'V V K8', nbcmp, jcmp)
            call wkvect(wkout, 'V V R', nbcmpr, jvres)
!
            nbma = zi(jchsd-1+1)
            call wkvect(wlima, 'V V I', nbma, jlima)
            nbaj = 0
            do 200 ima = 1, nbma
                ichk = -1
                nbpt = zi(jchsd-1+5+4*(ima-1)+1)
                nbsp = zi(jchsd-1+5+4*(ima-1)+2)
                nbcmpp = zi(jchsd-1+5+4*(ima-1)+3)
                do 210 ipt = 1, nbpt
                    do 212 isp = 1, nbsp
                        call jeundf(wkin)
                        call jeundf(wkcmp)
                        iv = 0
                        do 214 icmp = 1, nbcmpp
                            call cesexi('S', jchsd, jchsl, ima, ipt,&
                                        isp, icmp, iad)
                            if (iad .gt. 0) then
                                iv = iv + 1
                                zr(jval-1+iv) = zr(jchsv-1+iad)
                                zk8(jcmp-1+iv) = zk8(jchsc-1+icmp)
                            endif
214                      continue
!
                        if (nf .eq. 0) then
                            call ccchcr(crit, nomgd, iv, zr(jval), zk8( jcmp),&
                                        nbcmpr, zr(jvres), ichk)
                        else
                            call ccchcf(nfor, iv, zr(jval), zk8(jcmp), nbcmpr,&
                                        zr(jvres), ichk)
                        endif
                        if (ichk .ne. 0) then
                            goto 200
                        endif
!
                        do 216 icmp = 1, nbcmpr
                            call cesexi('S', jchrd, jchrl, ima, ipt,&
                                        isp, icmp, iad)
                            iad = -iad
                            zl(jchrl-1+iad) = .true.
                            zr(jchrv-1+iad) = zr(jvres-1+icmp)
216                      continue
212                  continue
210              continue
                if (ichk .eq. 0) then
!             ON CONSERVE LA MAILLE SI LE CRITERE A PU ETRE CALCULE SUR
!             TOUS SES POINTS
                    nbaj = nbaj + 1
                    zi(jlima-1+nbaj) = ima
                endif
200          continue
!
            vali(1) = iordr
            vali(2) = nbaj
            vali(3) = nbma
            call u2mesi('I', 'CHAMPS_8', 3, vali)
!
!         FAUT-IL CREER UN NOUVEAU LIGREL ?
            idem = .true.
            call jeexin(wlast, iret)
            if (iret .eq. 0) then
!           ON STOCKE EN INDICE 1 LE NOMBRE DE MAILLES DU LIGREL
                call wkvect(wlast, 'V V I', nbma+1, jlast)
                if (nbaj .ne. nbma) then
                    idem = .false.
                endif
            else
                if (zi(jlast-1+1) .ne. nbaj) goto 51
                do 50 ima = 1, nbaj
                    if (zi(jlast-1+ima+1) .ne. zi(jlima-1+ima)) then
                        idem = .false.
                        goto 51
                    endif
50              continue
51              continue
            endif
            zi(jlast-1+1) = nbaj
            do 52 ima = 1, nbaj
                zi(jlast-1+ima+1) = zi(jlima-1+ima)
52          continue
!
            if (.not.idem) then
!           SI PAS LES MEMES MAILLES, ON CREE UN NOUVEAU LIGREL
                call dismoi('F', 'NOM_MODELE', chps, 'CHAMP', ibid,&
                            model, ibid)
                noojb='12345678.LIGR000000.NBNO'
                call gnomsd(' ', noojb, 14, 19)
                ligrel=noojb(1:19)
                call exlim1(zi(jlima), nbaj, model, 'G', ligrel)
            endif
!
!         STOCKAGE DU CHAMP
            call rsexch(' ', resuou, chout, iordr, chres,&
                        iret)
            if (iret .ne. 100) then
                valk(1) = chout
                valk(2) = resuou
                call u2mesk('F', 'CHAMPS_14', 2, valk)
            endif
            call cescel(chr, ligrel, ' ', ' ', 'NAN',&
                        ibid, 'G', chres, 'F', iret)
            call assert(iret.eq.0)
            call rsnoch(resuou, chout, iordr)
!
            call detrsd(typs, chs)
            call detrsd(typs, chr)
            call jedetr(wkcmp)
            call jedetr(wkin)
            call jedetr(wkout)
            call jedetr(wlima)
!
! ------- ENDIF CHAM_ELEM
        endif
!
10  end do
!
    call jedetr(wlast)
!
    call jedema()
!
end subroutine

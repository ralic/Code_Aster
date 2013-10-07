subroutine peritr(resu, modele, cara, nh, nbocc)
    implicit none
#include "jeveux.h"
#include "asterc/gettco.h"
#include "asterfort/calcul.h"
#include "asterfort/chpve2.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/getvem.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jerazo.h"
#include "asterfort/jerecu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/mecact.h"
#include "asterfort/mecham.h"
#include "asterfort/memaxm.h"
#include "asterfort/memoy.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsutnu.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: nh, nbocc
    character(len=*) :: resu, modele, cara
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     OPERATEUR   POST_ELEM
!     TRAITEMENT DU MOT CLE-FACTEUR "RICE_TRACEY"
!     ------------------------------------------------------------------
!
    integer :: nbparr, nbpard, numa, long, mxvale
    integer :: ifm, nd, nr, niv, i, ni, np, nq, n1, n2, iret, jord, jins
    integer :: iord, iainst, lvale, nbin, iocc, nt, nm, nc
    integer :: ng, kk, nbgrma, jgr, ig, nbma, jad, nbmail, jma, im, nume, ier
    integer :: numord, numomu, nbordr
    parameter (mxvale=5,nbparr=6,nbpard=4)
    real(kind=8) :: prec, inst, rsr0, volu, numema, triax, lnrsr0
    real(kind=8) :: vr(5), rtval(2), valer(3)
    character(len=8) :: k8b, noma, resul, crit, nomail, nommai, lpain(7), lpaout(2)
    character(len=8) :: typarr(nbparr), typard(nbpard), valek(2), tabcmp(5)
    character(len=16) :: typres, option, optio2, optcal(2), toptca(2), noparr(nbparr)
    character(len=16) :: nopard(nbpard), tabtyp(3)
    character(len=19) :: chelem, knum, kins, varnul
    character(len=24) :: chgeom, chcara(18), chharm, ligrel, lchin(7)
    character(len=24) :: mlggma, mlgnma, compor, nomma2
    character(len=24) :: lchout(2), contg, varipg, varimg, depla, ssoup
    complex(kind=8) :: c16b
    integer :: iarg
!
    data noparr/'NUME_ORDRE','INST','LIEU','ENTITE',&
     &     'TX_CROIS_CAVITES','VOLUME_CONCERNE'/
    data typarr/'I','R','K24','K8','R','R'/
    data nopard/'LIEU','ENTITE','TX_CROIS_CAVITES','VOLUME_CONCERNE'/
    data typard/'K8','K8','R','R'/
!      DATA VARIMG /'&&PERITR.VARIMR'/
    data varnul/'&&PERITR.VARNUL'/
    data tabtyp/'NOEU#DEPL_R','NOEU#TEMP_R','ELEM#ENER_R'/
    data tabcmp/'TRIAX','RSR0','VOLU','NUMEMA','DEPSEQ'/
!     ------------------------------------------------------------------
    call jemarq()
    c16b=(0.d0,0.d0)
!
! --- RECUPERATION DU NIVEAU D'IMPRESSION
    call infniv(ifm, niv)
!
    inst = 0.d0
    call getvid(' ', 'CHAM_GD', scal=contg, nbret=nd)
    if (nd .ne. 0) then
        call chpve2(contg, 3, tabtyp, ier)
    endif
    call getvid(' ', 'RESULTAT', scal=resul, nbret=nr)
    call getvr8(' ', 'INST', scal=inst, nbret=ni)
    call getvtx('RICE_TRACEY', 'OPTION', iocc=1, scal=optcal(1), nbret=np)
    call getvtx('RICE_TRACEY', 'LOCAL', iocc=1, scal=optcal(2), nbret=nq)
    if (nbocc .gt. 1) then
        do i = 2, nbocc
            call getvtx('RICE_TRACEY', 'OPTION', iocc=i, scal=toptca(1), nbret=n1)
            call getvtx('RICE_TRACEY', 'LOCAL', iocc=i, scal=toptca(2), nbret=n2)
            if ((toptca(1).ne.optcal(1)) .or. (toptca(2).ne.optcal(2))) then
                call utmess('F', 'UTILITAI3_83')
            endif
        end do
    endif
!
    option = 'RICE_TRACEY'
    call mecham(option, modele, cara, nh, chgeom,&
                chcara, chharm, iret)
    if (iret .ne. 0) goto 110
    noma = chgeom(1:8)
    mlgnma = noma//'.NOMMAI'
    mlggma = noma//'.GROUPEMA'
!
!      NOMLIG = '&&PERITR'
!      CALL EXLIMA ( 'RICE_TRACEY', 'V', MODELE, NOMLIG, LIGREL )
!     IL FAUT FAIRE LE CALCUL SUR TOUT LE MODELE
!
    ligrel = modele//'.MODELE'
!
    knum = '&&PERITR.NUME_ORDRE'
    kins = '&&PERITR.INSTANT'
    if (nd .ne. 0) then
        nbordr = 1
        call wkvect(knum, 'V V I', nbordr, jord)
        zi(jord) = 1
        call wkvect(kins, 'V V R', nbordr, jins)
        zr(jins) = inst
        call tbcrsd(resu, 'G')
        call tbajpa(resu, nbpard, nopard, typard)
    else
        call gettco(resul, typres)
        if (typres(1:9) .ne. 'EVOL_NOLI') then
            call utmess('F', 'UTILITAI3_84')
        endif
        call getvr8(' ', 'PRECISION', scal=prec, nbret=np)
        call getvtx(' ', 'CRITERE', scal=crit, nbret=nc)
        call rsutnu(resul, ' ', 0, knum, nbordr,&
                    prec, crit, iret)
        if (iret .ne. 0) goto 100
        call jeveuo(knum, 'L', jord)
!        --- ON RECUPERE LES INSTANTS ---
        call wkvect(kins, 'V V R', nbordr, jins)
        call jenonu(jexnom(resul//'           .NOVA', 'INST'), iret)
        if (iret .ne. 0) then
            do iord = 1, nbordr
                numord = zi(jord+iord-1)
                call rsadpa(resul, 'L', 1, 'INST', numord,&
                            0, sjv=iainst, styp=k8b)
                zr(jins+iord-1) = zr(iainst)
            end do
        endif
        call tbcrsd(resu, 'G')
        call tbajpa(resu, nbparr, noparr, typarr)
    endif
!
!     --- INITIALISATIONS DES CHAMPS ---
!
    lnrsr0 = 0.d0
!      VARIPG = '&&PERITR.VARIPG'
    call mecact('V', '&&PERITR.SDRMR', 'MAILLA', noma, 'NEUT_R',&
                ncmp=1, nomcmp='X1', sr=0.d0)
!
    call wkvect('&&PERITR.TRAV1', 'V V R', mxvale, lvale)
    do iord = 1, nbordr
        call jemarq()
        call jerecu('V')
        numord = zi(jord+iord-1)
        inst = zr(jins+iord-1)
        valer(1) = inst
!
        call rsexch(' ', resul, 'COMPORTEMENT', numord, compor,&
                    iret)
        if (nr .ne. 0) then
            call rsexch('F', resul, 'SIEF_ELGA', numord, contg,&
                        iret)
            call rsexch('F', resul, 'VARI_ELGA', numord, varipg,&
                        iret)
            if (iord .ge. 2) then
                numomu = zi(jord+iord-2)
                call rsexch('F', resul, 'VARI_ELGA', numomu, varimg,&
                            iret)
            else
                call copisd('CHAMP_GD', 'V', varipg, varnul)
                call jelira(varnul//'.CELV', 'LONUTI', long)
                call jerazo(varnul//'.CELV', long, 1)
            endif
            call rsexch('F', resul, 'DEPL', numord, depla,&
                        iret)
        endif
!
!        --- AFFECTATION D'UNE CARTE CONSTANTE SUR LE MAILLAGE :
!            OPTION DE CALCUL RICE_TRACEY ---
!
        ssoup = optcal(1)//optcal(2)(1:8)
        call mecact('V', '&&PERITR.CH.SOUSOP', 'MAILLA', noma, 'NEUT_K24',&
                    ncmp=1, nomcmp='Z1', sk=ssoup)
!
        optio2 = 'RICE_TRACEY'
        chelem = '&&PERITR.RITR'
        nbin = 7
        lchin(1) = chgeom
        lpain(1) = 'PGEOMER'
        lchin(2) = contg
        lpain(2) = 'PCONTPR'
        if (iord .ge. 2) then
            lchin(3) = varimg
        else
            lchin(3) = varnul
        endif
        lpain(3) = 'PVARIMR'
        lchin(4) = varipg
        lpain(4) = 'PVARIPR'
        lchin(5) = '&&PERITR.SDRMR'
        lpain(5) = 'PSDRMR'
        lchin(6) = '&&PERITR.CH.SOUSOP'
        lpain(6) = 'PSOUSOP'
        lchin(7) = compor
        lpain(7) = 'PCOMPOR'
        lchout(1) = chelem
        lpaout(1) = 'PRICTRA'
        lchout(2) = '&&PERITR.SDRPR'
        lpaout(2) = 'PSDRPR'
        call calcul('S', optio2, ligrel, nbin, lchin,&
                    lpain, 2, lchout, lpaout, 'V',&
                    'OUI')
!
        do iocc = 1, nbocc
            call getvtx(option(1:11), 'TOUT', iocc=iocc, nbval=0, nbret=nt)
            call getvem(noma, 'MAILLE', option(1:11), 'MAILLE', iocc,&
                        iarg, 0, k8b, nm)
            call getvem(noma, 'GROUP_MA', option(1:11), 'GROUP_MA', iocc,&
                        iarg, 0, k8b, ng)
!
            if (nt .ne. 0) then
                if (optcal(2) .eq. 'OUI') then
                    call memaxm('MAX', chelem, 'RSR0', mxvale, tabcmp,&
                                vr, 0, [0])
                    do kk = 1, mxvale
                        zr(lvale+kk-1) = vr(kk)
                    end do
                else if (optcal(2).eq.'NON') then
                    call memoy(chelem, 1, chelem, 3, vr,&
                               0, [0])
                    zr(lvale) = vr(1)
                    zr(lvale+2) = vr(2)
                    triax = zr(lvale)
                    call memoy(chelem, 5, chelem, 3, vr,&
                               0, [0])
                    zr(lvale+4) = vr(1)
                    lnrsr0 = lnrsr0 + 0.283d0*sign(1.d0,triax)* exp(1.5d0*abs(triax))*zr(lvale+4)
                    zr(lvale+1) = exp(lnrsr0)
                    zr(lvale+3) = 0.d0
                endif
                rsr0 = zr(lvale+1)
                volu = zr(lvale+2)
                numema = zr(lvale+3)
                if (optcal(2) .eq. 'OUI') then
                    numa = nint(numema)
                    call jenuno(jexnum(mlgnma, numa), nomail)
                    valek(1) = nomail
                    valek(2) = 'MAILLE'
                else
                    valek(1) = noma
                    valek(2) = 'TOUT'
                endif
                rtval(1) = rsr0
                rtval(2) = volu
                if (nr .ne. 0) then
                    valer(2) = rtval(1)
                    valer(3) = rtval(2)
                    call tbajli(resu, nbparr, noparr, [numord], valer,&
                                [c16b], valek, 0)
                else
                    call tbajli(resu, nbpard, nopard, [numord], rtval,&
                                [c16b], valek, 0)
                endif
            endif
!
            if (ng .ne. 0) then
                nbgrma = -ng
                call wkvect('&&PERITR_GROUPM', 'V V K24', nbgrma, jgr)
                call getvem(noma, 'GROUP_MA', option(1:11), 'GROUP_MA', iocc,&
                            iarg, nbgrma, zk24(jgr), ng)
                do ig = 1, nbgrma
                    nomma2 = zk24(jgr+ig-1)
                    call jeexin(jexnom(mlggma, nomma2), iret)
                    if (iret .eq. 0) then
                        call utmess('A', 'UTILITAI3_46', sk=nomma2)
                        goto 50
                    endif
                    call jelira(jexnom(mlggma, nomma2), 'LONUTI', nbma)
                    if (nbma .eq. 0) then
                        call utmess('A', 'UTILITAI3_47', sk=nomma2)
                        goto 50
                    endif
                    call jeveuo(jexnom(mlggma, nomma2), 'L', jad)
                    if (optcal(2) .eq. 'OUI') then
                        call memaxm('MAX', chelem, 'RSR0', mxvale, tabcmp,&
                                    vr, nbma, zi(jad))
                        do kk = 1, mxvale
                            zr(lvale+kk-1) = vr(kk)
                        end do
                    else if (optcal(2).eq.'NON') then
                        call memoy(chelem, 1, chelem, 3, vr,&
                                   nbma, zi(jad))
                        zr(lvale) = vr(1)
                        zr(lvale+2) = vr(2)
                        triax = zr(lvale)
                        call memoy(chelem, 5, chelem, 3, vr,&
                                   nbma, zi(jad))
                        zr(lvale+4) = vr(1)
                        lnrsr0 = lnrsr0 + 0.283d0*sign(1.d0,triax)* exp(1.5d0*abs(triax))*zr(lval&
                                 &e+4)
                        zr(lvale+1) = exp(lnrsr0)
                        zr(lvale+3) = 0.d0
                    endif
                    rsr0 = zr(lvale+1)
                    volu = zr(lvale+2)
                    numema = zr(lvale+3)
                    if (optcal(2) .eq. 'OUI') then
                        numa = nint(numema)
                        call jenuno(jexnum(mlgnma, numa), nomail)
                        valek(1) = nomail
                        valek(2) = 'MAILLE'
                    else
                        valek(1) = noma
                        valek(2) = 'TOUT'
                    endif
                    rtval(1) = rsr0
                    rtval(2) = volu
                    if (nr .ne. 0) then
                        valer(2) = rtval(1)
                        valer(3) = rtval(2)
                        call tbajli(resu, nbparr, noparr, [numord], valer,&
                                    [c16b], valek, 0)
                    else
                        call tbajli(resu, nbpard, nopard, [numord], rtval,&
                                    [c16b], valek, 0)
                    endif
 50                 continue
                end do
                call jedetr('&&PERITR_GROUPM')
            endif
!
            if (nm .ne. 0) then
                nbmail = -nm
                call wkvect('&&PERITR_MAILLE', 'V V K8', nbmail, jma)
                call getvem(noma, 'MAILLE', option(1:11), 'MAILLE', iocc,&
                            iarg, nbmail, zk8(jma), nm)
                do im = 1, nbmail
                    nommai = zk8(jma+im-1)
                    call jeexin(jexnom(mlgnma, nommai), iret)
                    if (iret .eq. 0) then
                        call utmess('A', 'UTILITAI3_49', sk=nommai)
                        goto 70
                    endif
                    call jenonu(jexnom(mlgnma, nommai), nume)
                    if (optcal(2) .eq. 'OUI') then
                        call memaxm('MAX', chelem, 'RSR0', mxvale, tabcmp,&
                                    vr, 1, [nume])
                        do kk = 1, mxvale
                            zr(lvale+kk-1) = vr(kk)
                        end do
                    else if (optcal(2).eq.'NON') then
                        call memoy(chelem, 1, chelem, 3, vr,&
                                   1, [nume])
                        zr(lvale) = vr(1)
                        zr(lvale+2) = vr(2)
                        triax = zr(lvale)
                        call memoy(chelem, 5, chelem, 3, vr,&
                                   1, [nume])
                        zr(lvale+4) = vr(1)
                        lnrsr0 = lnrsr0 + 0.283d0*sign(1.d0,triax)* exp(1.5d0*abs(triax))*zr(lval&
                                 &e+4)
                        zr(lvale+1) = exp(lnrsr0)
                        zr(lvale+3) = 0.d0
                    endif
                    rsr0 = zr(lvale+1)
                    volu = zr(lvale+2)
                    numema = zr(lvale+3)
                    if (optcal(2) .eq. 'OUI') then
                        numa = nint(numema)
                        call jenuno(jexnum(mlgnma, numa), nomail)
                        valek(1) = nomail
                        valek(2) = 'MAILLE'
                    else
                        valek(1) = noma
                        valek(2) = 'TOUT'
                    endif
                    rtval(1) = rsr0
                    rtval(2) = volu
                    if (nr .ne. 0) then
                        valer(2) = rtval(1)
                        valer(3) = rtval(2)
                        call tbajli(resu, nbparr, noparr, [numord], valer,&
                                    [c16b], valek, 0)
                    else
                        call tbajli(resu, nbpard, nopard, [numord], rtval,&
                                    [c16b], valek, 0)
                    endif
 70                 continue
                end do
                call jedetr('&&PERITR_MAILLE')
            endif
        end do
        call copisd('CHAMP_GD', 'V', '&&PERITR.SDRPR', '&&PERITR.SDRMR')
        call detrsd('CARTE', '&&PERITR.CH.SOUSOP')
        call detrsd('CHAM_ELEM', chelem)
        call jedema()
    end do
!
100 continue
!
! --- MENAGE
    call jedetr(knum)
    call jedetr(kins)
    call jedetr('&&PERITR.TRAV1')
    call detrsd('CHAMP_GD', varnul)
    call detrsd('CHAMP_GD', '&&PERITR.SDRPR')
    call detrsd('CHAMP_GD', '&&PERITR.SDRMR')
!
110 continue
    call jedema()
end subroutine

subroutine peepot(resu, modele, mate, cara, nh,&
                  nbocc)
    implicit   none
    include 'jeveux.h'
!
    include 'asterfort/chpve2.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/exlim3.h'
    include 'asterc/gettco.h'
    include 'asterfort/getvem.h'
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jerecu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/mecalc.h'
    include 'asterfort/mecham.h'
    include 'asterfort/mechnc.h'
    include 'asterfort/mechti.h'
    include 'asterfort/meharm.h'
    include 'asterfort/peenca.h'
    include 'asterc/r8vide.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/rsutnu.h'
    include 'asterfort/tbajli.h'
    include 'asterfort/tbajpa.h'
    include 'asterfort/tbcrsd.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/vrcins.h'
    include 'asterfort/vrcref.h'
    include 'asterfort/wkvect.h'
    integer :: nh, nbocc
    character(len=*) :: resu, modele, mate, cara
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
!     TRAITEMENT DU MOT CLE-FACTEUR "ENER_POT"
!     ------------------------------------------------------------------
!
!
    integer :: nd, nr, ni, iret, np, nc, jord, jins, jad, nbordr, iord, numord, iainst, jnmo, ibid
    integer :: ie, ire1, ire2, nt, nm, ng, nbgrma, ig, jgr, nbma, nume, im, nbparr, nbpard, nbpaep
    integer :: iocc, jma, icheml, ifm, niv, ier
    parameter (nbpaep=2,nbparr=6,nbpard=4)
    real(kind=8) :: prec, varpep(nbpaep), alpha, inst, valer(3), rundf
    character(len=1) :: base
    character(len=2) :: codret
    character(len=8) :: k8b, noma, resul, crit, nommai, typarr(nbparr), typard(nbpard), valk(2)
    character(len=8) :: nomgd
    character(len=16) :: typres, option, optio2, noparr(nbparr), nopard(nbpard)
    character(len=19) :: chelem, knum, kins, depla, ligrel, tabtyp(3), chvarc, chvref
    character(len=24) :: chtime, chnumc, chamgd, typcha, chgeom, chcara(18), chtemp, chharm
    character(len=24) :: compor, mlggma, mlgnma, k24b, nomgrm, valk2(2)
!
    logical :: exitim
    complex(kind=8) :: c16b, calpha
    integer :: iarg
!
    data noparr/'NUME_ORDRE','INST','LIEU','ENTITE','TOTALE',&
     &     'POUR_CENT'/
    data typarr/'I','R','K24','K8','R','R'/
    data nopard/'LIEU','ENTITE','TOTALE','POUR_CENT'/
    data typard/'K8','K8','R','R'/
    data tabtyp/'NOEU#DEPL_R','NOEU#TEMP_R','ELEM#ENER_R'/
    data chvarc,chvref /'&&PEEPOT.VARC','&&PEEPOT.VARC_REF'/
!
!     ------------------------------------------------------------------
    call jemarq()
!
! --- RECUPERATION DU NIVEAU D'IMPRESSION
    call infniv(ifm, niv)
!
    base = 'V'
    k24b = ' '
    rundf = r8vide()
    exitim = .false.
    inst = 0.d0
    alpha = 1.d0
    calpha = (1.d0,1.d0)
    chtemp= ' '
!
    call getvid(' ', 'CHAM_GD', 1, iarg, 1,&
                depla, nd)
    if (nd .ne. 0) then
        call chpve2(depla, 3, tabtyp, ier)
    endif
    call getvid(' ', 'RESULTAT', 1, iarg, 1,&
                resul, nr)
    call getvr8(' ', 'INST', 1, iarg, 1,&
                inst, ni)
    if (ni .ne. 0) exitim = .true.
    if (nr .ne. 0) then
        call gettco(resul, typres)
        if (typres(1:9) .eq. 'MODE_MECA') then
            noparr(2) = 'FREQ'
            else if (typres(1:9).eq.'EVOL_THER' .or. typres(1:9)&
        .eq.'EVOL_ELAS' .or. typres(1:9).eq.'MULT_ELAS' .or. typres(1:&
        9).eq.'EVOL_NOLI' .or. typres(1:10).eq.'DYNA_TRANS') then
            noparr(2) = 'INST'
        else
            call u2mess('F', 'UTILITAI3_75')
        endif
    endif
!
    option = 'ENER_POT'
    call mecham(option, modele, cara, nh, chgeom,&
                chcara, chharm, iret)
    if (iret .ne. 0) goto 90
    noma = chgeom(1:8)
    mlgnma = noma//'.NOMMAI'
    mlggma = noma//'.GROUPEMA'
    call mechnc(noma, ' ', 0, chnumc)
!
    call exlim3('ENER_POT', 'V', modele, ligrel)
!
    knum = '&&PEEPOT.NUME_ORDRE'
    kins = '&&PEEPOT.INSTANT'
    typres = ' '
    if (nd .ne. 0) then
        nbordr = 1
        call wkvect(knum, 'V V I', nbordr, jord)
        zi(jord) = 1
        call wkvect(kins, 'V V R', nbordr, jins)
        zr(jins) = inst
        call tbcrsd(resu, 'G')
        call tbajpa(resu, nbpard, nopard, typard)
    else
        call getvr8(' ', 'PRECISION', 1, iarg, 1,&
                    prec, np)
        call getvtx(' ', 'CRITERE', 1, iarg, 1,&
                    crit, nc)
        call rsutnu(resul, ' ', 0, knum, nbordr,&
                    prec, crit, iret)
        if (iret .ne. 0) goto 80
        call jeveuo(knum, 'L', jord)
!        --- ON RECUPERE LES INSTANTS ---
        call wkvect(kins, 'V V R', nbordr, jins)
        call jenonu(jexnom(resul//'           .NOVA', 'INST'), iret)
        if (iret .ne. 0) then
            exitim = .true.
            do 10 iord = 1, nbordr
                numord = zi(jord+iord-1)
                call rsadpa(resul, 'L', 1, 'INST', numord,&
                            0, iainst, k8b)
                zr(jins+iord-1) = zr(iainst)
10          continue
        else
            call jenonu(jexnom(resul//'           .NOVA', 'FREQ'), iret)
            if (iret .ne. 0) then
                do 20 iord = 1, nbordr
                    numord = zi(jord+iord-1)
                    call rsadpa(resul, 'L', 1, 'FREQ', numord,&
                                0, iainst, k8b)
                    zr(jins+iord-1) = zr(iainst)
20              continue
            endif
        endif
        call tbcrsd(resu, 'G')
        call tbajpa(resu, nbparr, noparr, typarr)
    endif
!
    do 70 iord = 1, nbordr
        call jemarq()
        call jerecu('V')
        icheml = 0
        numord = zi(jord+iord-1)
        inst = zr(jins+iord-1)
        valer(1) = inst
        if (typres .eq. 'FOURIER_ELAS') then
            call rsadpa(resul, 'L', 1, 'NUME_MODE', numord,&
                        0, jnmo, k8b)
            call meharm(modele, zi(jnmo), chharm)
        endif
        chtime = ' '
        if (exitim) call mechti(noma, inst, rundf, rundf, chtime)
!
        if (nr .ne. 0) then
            call rsexch(' ', resul, 'EPOT_ELEM', numord, depla,&
                        iret)
            if (iret .gt. 0) then
                call rsexch(' ', resul, 'DEPL', numord, depla,&
                            ire1)
                if (ire1 .gt. 0) then
                    call rsexch(' ', resul, 'TEMP', numord, depla,&
                                ire2)
                    if (ire2 .gt. 0) goto 72
                endif
            endif
        endif
!
        call dismoi('F', 'TYPE_SUPERVIS', depla, 'CHAMP', ibid,&
                    typcha, ie)
        call dismoi('F', 'NOM_GD', depla, 'CHAMP', ibid,&
                    nomgd, ie)
        if (typcha(1:7) .eq. 'CHAM_NO') then
            if (nomgd(1:4) .eq. 'DEPL') then
                chamgd = depla
                optio2 = 'EPOT_ELEM'
                call vrcins(modele, mate, cara, inst, chvarc,&
                            codret)
                call vrcref(modele(1:8), mate(1:8), cara(1:8), chvref(1: 19))
            else if (nomgd(1:4).eq.'TEMP') then
                optio2 = 'ETHE_ELEM'
                chamgd = ' '
                chtemp = depla
            else
                call u2mess('F', 'UTILITAI3_73')
            endif
        else if (typcha(1:9).eq.'CHAM_ELEM') then
            if (nomgd(1:4) .eq. 'ENER') then
                chelem = depla
                goto 30
            else
                call u2mess('F', 'UTILITAI3_73')
            endif
        else
            call u2mess('F', 'UTILITAI3_73')
        endif
        icheml = 1
        chelem = '&&PEEPOT.CHAM_ELEM'
        compor = mate(1:8)//'.COMPOR'
        ibid = 0
        call mecalc(optio2, modele, chamgd, chgeom, mate,&
                    chcara, chtemp, k24b, chtime, chnumc,&
                    chharm, k24b, k24b, k24b, k24b,&
                    k24b, k24b, k24b, alpha, calpha,&
                    k24b, k24b, chelem, k24b, ligrel,&
                    base, chvarc, chvref, k24b, compor,&
                    k24b, k24b, k8b, ibid, k24b,&
                    iret)
30      continue
!
!        --- ON CALCULE L'ENERGIE TOTALE ---
        call peenca(chelem, nbpaep, varpep, 0, ibid)
!
        do 60 iocc = 1, nbocc
            call getvtx(option(1:9), 'TOUT', iocc, iarg, 0,&
                        k8b, nt)
            call getvem(noma, 'MAILLE', option(1:9), 'MAILLE', iocc,&
                        iarg, 0, k8b, nm)
            call getvem(noma, 'GROUP_MA', option(1:9), 'GROUP_MA', iocc,&
                        iarg, 0, k8b, ng)
            if (nt .ne. 0) then
                call peenca(chelem, nbpaep, varpep, 0, ibid)
                valk(1) = noma
                valk(2) = 'TOUT'
                if (nr .ne. 0) then
                    valer(2) = varpep(1)
                    valer(3) = varpep(2)
                    call tbajli(resu, nbparr, noparr, numord, valer,&
                                c16b, valk, 0)
                else
                    call tbajli(resu, nbpard, nopard, numord, varpep,&
                                c16b, valk, 0)
                endif
            endif
            if (ng .ne. 0) then
                nbgrma = -ng
                call wkvect('&&PEEPOT_GROUPM', 'V V K24', nbgrma, jgr)
                call getvem(noma, 'GROUP_MA', option(1:9), 'GROUP_MA', iocc,&
                            iarg, nbgrma, zk24(jgr), ng)
                valk2(2) = 'GROUP_MA'
                do 40 ig = 1, nbgrma
                    nomgrm = zk24(jgr+ig-1)
                    call jeexin(jexnom(mlggma, nomgrm), iret)
                    if (iret .eq. 0) then
                        call u2mesk('A', 'UTILITAI3_46', 1, nomgrm)
                        goto 40
                    endif
                    call jelira(jexnom(mlggma, nomgrm), 'LONUTI', nbma, k8b)
                    if (nbma .eq. 0) then
                        call u2mesk('A', 'UTILITAI3_47', 1, nomgrm)
                        goto 40
                    endif
                    call jeveuo(jexnom(mlggma, nomgrm), 'L', jad)
                    call peenca(chelem, nbpaep, varpep, nbma, zi(jad))
                    valk2(1) = nomgrm
                    if (nr .ne. 0) then
                        valer(2) = varpep(1)
                        valer(3) = varpep(2)
                        call tbajli(resu, nbparr, noparr, numord, valer,&
                                    c16b, valk2, 0)
                    else
                        call tbajli(resu, nbpard, nopard, numord, varpep,&
                                    c16b, valk2, 0)
                    endif
40              continue
                call jedetr('&&PEEPOT_GROUPM')
            endif
            if (nm .ne. 0) then
                nbma = -nm
                call wkvect('&&PEEPOT_MAILLE', 'V V K8', nbma, jma)
                call getvem(noma, 'MAILLE', option(1:9), 'MAILLE', iocc,&
                            iarg, nbma, zk8(jma), nm)
                valk(2) = 'MAILLE'
                do 50 im = 1, nbma
                    nommai = zk8(jma+im-1)
                    call jeexin(jexnom(mlgnma, nommai), iret)
                    if (iret .eq. 0) then
                        call u2mesk('A', 'UTILITAI3_49', 1, nommai)
                        goto 50
                    endif
                    call jenonu(jexnom(mlgnma, nommai), nume)
                    call peenca(chelem, nbpaep, varpep, 1, nume)
                    valk(1) = nommai
                    if (nr .ne. 0) then
                        valer(2) = varpep(1)
                        valer(3) = varpep(2)
                        call tbajli(resu, nbparr, noparr, numord, valer,&
                                    c16b, valk, 0)
                    else
                        call tbajli(resu, nbpard, nopard, numord, varpep,&
                                    c16b, valk, 0)
                    endif
50              continue
                call jedetr('&&PEEPOT_MAILLE')
            endif
60      continue
        call jedetr('&&PEEPOT.PAR')
        if (icheml .ne. 0) call jedetr(chelem)
72      continue
        call jedema()
70  continue
!
80  continue
    call jedetr(knum)
    call jedetr(kins)
!
90  continue
    call jedema()
end subroutine

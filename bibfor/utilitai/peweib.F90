subroutine peweib(resu, modele, mate, cara, chmat,&
                  nh, nbocc, iresu, nomcmd)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/calcul.h'
    include 'asterfort/chmrck.h'
    include 'asterfort/chpve2.h'
    include 'asterfort/copisd.h'
    include 'asterfort/detrsd.h'
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
    include 'asterfort/mecact.h'
    include 'asterfort/mecham.h'
    include 'asterfort/mesomm.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/rsutnu.h'
    include 'asterfort/tbajli.h'
    include 'asterfort/tbajpa.h'
    include 'asterfort/tbcrsd.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/vrcins.h'
    include 'asterfort/wkvect.h'
    integer :: iresu, nh, nbocc
    character(len=*) :: resu, modele, mate, cara, nomcmd
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
!     ( TRAITEMENT DU MOT CLE-FACTEUR "WEIBULL" )
!     OPERATEUR   RECA_WEIBULL
!     ( RECALAGE DE LA METHODE DE WEIBULL )
!     ------------------------------------------------------------------
!
    real(kind=8) :: valr(3)
    integer :: nbparr, nbpard, nbmtcm, anommt, inum, nbout
    integer :: vali
    integer :: ibid, ibik, mxvale, ifm, niv
    integer :: nd, ng, ni, nm, np, nq, nr, nt, i, n1, n2, n3
    integer :: iret, nbordr, jord, jins, nc, nbgrma, jgr, ig, nbma, jad
    integer :: nbmail, jma, im, nume, imc, ier
    integer :: numord, iainst, iord, nbmtrc, lvale, nbin, iocc
    parameter (mxvale=3,nbparr=7,nbpard=5)
    real(kind=8) :: r8b, rtval(mxvale), prec, inst, valer(4), vref, coesym, mref, sref, probaw
    real(kind=8) :: sigmaw
    character(len=1) :: base
    character(len=2) :: codret
    character(len=8) :: k8b, noma, resul, crit, chmat, nommai, typarr(nbparr), typard(nbpard)
    character(len=8) :: lpain(9), lpaout(2), valek(2)
    character(len=16) :: typres, option, optio2, optcal(2), toptca(2), nomrc, noparr(nbparr)
    character(len=16) :: nopard(nbpard), motcl1, motcl2, motcl3
    character(len=19) :: chelem, knum, kins, tabtyp(3), chvarc
    character(len=24) :: chgeom, chcara(18), chharm
    character(len=24) :: valk(2), nomgrm
    character(len=24) :: mlggma, mlgnma, ligrel, lchin(9), compor
    character(len=24) :: lchout(2), contg, defog, varig, depla, ssoup
    character(len=24) :: kvalrc, kvalrk, vale2(2)
    logical :: opti
    complex(kind=8) :: c16b
    integer :: iarg
!
    data noparr/'NUME_ORDRE','INST','LIEU','ENTITE','SIGMA_WEIBULL',&
     &     'PROBA_WEIBULL','SIGMA_WEIBULL**M'/
    data typarr/'I','R','K24','K8','R','R','R'/
    data nopard/'LIEU','ENTITE','SIGMA_WEIBULL','PROBA_WEIBULL',&
     &     'SIGMA_WEIBULL**M'/
    data typard/'K8','K8','R','R','R'/
    data tabtyp/'NOEU#DEPL_R','NOEU#TEMP_R','ELEM#ENER_R'/
    data chvarc/'&&PEWEIB.CHVARC'/
!     ------------------------------------------------------------------
    call jemarq()
!
! --- RECUPERATION DU NIVEAU D'IMPRESSION
    call infniv(ifm, niv)
!
    if (nomcmd(1:12) .eq. 'RECA_WEIBULL') then
        opti = .true.
        motcl1 = ' '
        motcl2 = 'RESU'
        motcl3 = 'RESU'
        base='V'
    else if (nomcmd(1:9).eq.'POST_ELEM') then
        opti = .false.
        motcl1 = 'WEIBULL'
        motcl2 = ' '
        motcl3 = 'WEIBULL'
        base='G'
    endif
!
    inst = 0.d0
!
    nd = 0
    if (.not.opti) call getvid(' ', 'CHAM_GD', 1, iarg, 1,&
                               contg, nd)
    if (nd .ne. 0) then
        call chpve2(contg, 3, tabtyp, ier)
    endif
    ni = 0
    if (.not.opti) call getvr8(' ', 'INST', 1, iarg, 1,&
                               inst, ni)
!
    if (.not.opti) then
        call getvid(motcl2, 'RESULTAT', 1, iarg, 1,&
                    resul, nr)
    else
        call getvid(motcl2, 'EVOL_NOLI', iresu, iarg, 1,&
                    resul, nr)
    endif
    call getvtx(motcl1, 'OPTION', 1, iarg, 1,&
                optcal(1), np)
    call getvtx(motcl1, 'CORR_PLAST', 1, iarg, 1,&
                optcal(2), nq)
    if (nbocc .gt. 1) then
        do 10 i = 2, nbocc
            call getvtx(motcl1, 'OPTION', i, iarg, 1,&
                        toptca(1), n1)
            call getvtx(motcl1, 'CORR_PLAST', i, iarg, 1,&
                        toptca(2), n2)
            if ((toptca(1).ne.optcal(1)) .or. (toptca(2).ne.optcal(2))) then
                call u2mess('F', 'UTILITAI3_83')
            endif
10      continue
    endif
!
    option = 'WEIBULL'
    call mecham(option, modele, cara, nh, chgeom,&
                chcara, chharm, iret)
    if (iret .ne. 0) goto 100
    noma = chgeom(1:8)
    mlgnma = noma//'.NOMMAI'
    mlggma = noma//'.GROUPEMA'
!
    call exlim3(motcl3, 'V', modele, ligrel)
!
!     CREATION CARTE CONSTANTE ET NULLE SUR TOUT LE MAILLAGE
    call mecact('V', '&&PEWEIB.SIGIE', 'MAILLA', noma, 'DOMA_R',&
                1, 'DOMA', ibid, 0.d0, c16b,&
                k8b)
!
    knum = '&&PEWEIB.NUME_ORDRE'
    kins = '&&PEWEIB.INSTANT'
    if (nd .ne. 0) then
        nbordr = 1
        call wkvect(knum, 'V V I', nbordr, jord)
        zi(jord) = 1
        call wkvect(kins, 'V V R', nbordr, jins)
        zr(jins) = inst
        call tbcrsd(resu, base)
        call tbajpa(resu, nbpard, nopard, typard)
    else
        call gettco(resul, typres)
        if (typres(1:9) .ne. 'EVOL_NOLI') then
            call u2mess('F', 'UTILITAI3_84')
        endif
!
        np = 0
        if (.not.opti) call getvr8(' ', 'PRECISION', 1, iarg, 1,&
                                   prec, np)
        nc = 0
        if (.not.opti) call getvtx(' ', 'CRITERE', 1, iarg, 1,&
                                   crit, nc)
!
        if (.not.opti) then
            call rsutnu(resul, motcl2, 1, knum, nbordr,&
                        prec, crit, iret)
        else
            call rsutnu(resul, motcl2, iresu, knum, nbordr,&
                        prec, crit, iret)
        endif
!
        if (iret .ne. 0) goto 90
        call jeveuo(knum, 'L', jord)
!        --- ON RECUPERE LES INSTANTS ---
        call wkvect(kins, 'V V R', nbordr, jins)
        call jenonu(jexnom(resul//'           .NOVA', 'INST'), iret)
        if (iret .ne. 0) then
            do 20 iord = 1, nbordr
                numord = zi(jord+iord-1)
                call rsadpa(resul, 'L', 1, 'INST', numord,&
                            0, iainst, k8b)
                zr(jins+iord-1) = zr(iainst)
20          continue
        endif
        call tbcrsd(resu, base)
        call tbajpa(resu, nbparr, noparr, typarr)
    endif
!
!     --- VERIF D'HOMOGENEITE WEIBULL ---
!
    if (.not.opti) then
        call getvid(motcl2, 'CHAM_MATER', iresu, iarg, 1,&
                    chmat, n3)
        if (n3 .eq. 0) chmat = mate(1:8)
    endif
    call jelira(chmat//'.CHAMP_MAT .VALE', 'LONMAX', nbmtcm, k8b)
    call wkvect('&&PEWEIB.L_NOM_MAT', 'V V K8', nbmtcm, anommt)
    nomrc = 'WEIBULL         '
    call chmrck(chmat, nomrc, zk8(anommt), nbmtrc)
    if (nbmtrc .gt. 1) then
        vali = nbmtrc
        valk (1) = k8b
        valk (2) = k8b
        call u2mesg('A', 'UTILITAI6_60', 2, valk, 1,&
                    vali, 0, 0.d0)
    endif
!
!     --- RECUPERATION DES PARAMETRES DE LA RC WEIBULL ---
    kvalrc(1:8) = zk8(anommt)
    kvalrc(9:24) = '.WEIBULL   .VALR'
    kvalrk(1:8) = zk8(anommt)
    kvalrk(9:24) = '.WEIBULL   .VALK'
    call jeveuo(kvalrc, 'L', ibid)
    call jeveuo(kvalrk, 'L', ibik)
    call jelira(kvalrk, 'LONMAX', imc, k8b)
    sref = 0.d0
    do 30 i = 1, imc
        if (zk8(ibik+i-1) .eq. 'SIGM_CNV') sref = zr(ibid+i-1)
        if (zk8(ibik+i-1) .eq. 'M       ') mref = zr(ibid+i-1)
        if (zk8(ibik+i-1) .eq. 'VOLU_REF') vref = zr(ibid+i-1)
30  continue
! CAS WEIBULL_FO
    if (sref .eq. 0.d0) then
        do 40 i = 1, imc
            if (zk8(ibik+i-1) .eq. 'SIGM_REF') sref = zr(ibid+i-1)
40      continue
        valr (1) = mref
        valr (2) = vref
        valr (3) = sref
        call u2mesg('I', 'UTILITAI6_61', 0, ' ', 0,&
                    0, 3, valr)
! CAS WEIBULL
    else
        valr (1) = mref
        valr (2) = vref
        valr (3) = sref
        call u2mesg('I', 'UTILITAI6_62', 0, ' ', 0,&
                    0, 3, valr)
    endif
!
    call wkvect('&&PEWEIB.TRAV1', 'V V R', mxvale, lvale)
    do 80 iord = 1, nbordr
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
            call rsexch('F', resul, 'VARI_ELGA', numord, varig,&
                        iret)
            call rsexch('F', resul, 'DEPL', numord, depla,&
                        iret)
        endif
!
!        --- DANS LE CAS D'UNE OPTION AVEC CORRECTION DE DEFORMATION
!            RECUPERATION DES DEFORMATIONS DE GREEN LAGRANGE ---
!
        if (optcal(2) .eq. 'OUI') then
            call rsexch('F', resul, 'EPSG_ELGA', numord, defog,&
                        iret)
        else
            defog = '&&PEWEIB.EPSG'
        endif
!
!        --- RECUPERATION DU CHAMP DE TEMPERATURE
!            UTILE POUR LE CAS OU SIGU(T)
        call vrcins(modele, mate, cara, inst, chvarc,&
                    codret)
!
!        --- AFFECTATION D'UNE CARTE CONSTANTE SUR LE MAILLAGE :
!            OPTION DE CALCUL WEIBULL ---
!
        ssoup = optcal(1)//optcal(2)(1:8)
        call mecact('V', '&&PEWEIB.CH.SOUSOP', 'MAILLA', noma, 'NEUT_K24',&
                    1, 'Z1', ibid, r8b, c16b,&
                    ssoup)
!
        optio2 = 'WEIBULL'
        chelem = '&&PEWEIB.WEIBULL'
        nbin = 9
        lchin(1) = chgeom
        lpain(1) = 'PGEOMER'
        lchin(2) = contg
        lpain(2) = 'PCONTRG'
        lchin(3) = varig
        lpain(3) = 'PVARIPG'
        lchin(4) = defog
        lpain(4) = 'PDEFORR'
        lchin(5) = mate
        lpain(5) = 'PMATERC'
        lchin(6) = '&&PEWEIB.CH.SOUSOP'
        lpain(6) = 'PSOUSOP'
!  EN ENTREE : CHELEM DES SIGI MAX ATTEINTE AU COURS DU TEMPS
        lchin(7) = '&&PEWEIB.SIGIE'
        lpain(7) = 'PDOMMAG'
!  EN ENTREE : CHELEM DES VARIABLES DE COMMANDES
        lchin(8) = chvarc
        lpain(8) = 'PVARCPR'
        lchin(9) = compor
        lpain(9) = 'PCOMPOR'
        nbout = 2
        lchout(1) = chelem
        lpaout(1) = 'PWEIBUL'
        lchout(2) = '&&PEWEIB.SIGIS'
        lpaout(2) = 'PSIGISG'
        call calcul('S', optio2, ligrel, nbin, lchin,&
                    lpain, nbout, lchout, lpaout, 'V',&
                    'OUI')
!
!        RECOPIE DE SIGIS DANS SIGIE
        call copisd('CHAMP_GD', 'V', '&&PEWEIB.SIGIS', '&&PEWEIB.SIGIE')
!
        do 70 iocc = 1, nbocc
            if (.not.opti) then
                inum = iocc
            else
                inum = iresu
            endif
            call getvtx(motcl3, 'TOUT', inum, iarg, 0,&
                        k8b, nt)
            call getvem(noma, 'MAILLE', motcl3, 'MAILLE', inum,&
                        iarg, 0, k8b, nm)
            call getvem(noma, 'GROUP_MA', motcl3, 'GROUP_MA', inum,&
                        iarg, 0, k8b, ng)
            call getvr8(motcl3, 'COEF_MULT', inum, iarg, 1,&
                        coesym, n1)
!
            if (nt .ne. 0) then
                call mesomm(chelem, mxvale, ibid, zr(lvale), c16b,&
                            0, ibid)
                probaw = coesym*zr(lvale)
                sigmaw = probaw* (sref**mref)
                probaw = 1.0d0 - exp(-probaw)
                rtval(3) = sigmaw
                rtval(2) = probaw
                rtval(1) = sigmaw** (1.0d0/mref)
                valek(1) = noma
                valek(2) = 'TOUT'
                if (nr .ne. 0) then
                    valer(2) = rtval(1)
                    valer(3) = rtval(2)
                    valer(4) = rtval(3)
                    call tbajli(resu, nbparr, noparr, numord, valer,&
                                c16b, valek, 0)
                else
                    call tbajli(resu, nbpard, nopard, numord, rtval,&
                                c16b, valek, 0)
                endif
            endif
!
            if (ng .ne. 0) then
                nbgrma = -ng
                call wkvect('&&PEWEIB_GROUPM', 'V V K24', nbgrma, jgr)
                call getvem(noma, 'GROUP_MA', motcl3, 'GROUP_MA', inum,&
                            iarg, nbgrma, zk24(jgr), ng)
                vale2(2) = 'GROUP_MA'
                do 50 ig = 1, nbgrma
                    nomgrm = zk24(jgr+ig-1)
                    call jeexin(jexnom(mlggma, nomgrm), iret)
                    if (iret .eq. 0) then
                        call u2mesk('A', 'UTILITAI3_46', 1, nomgrm)
                        goto 50
                    endif
                    call jelira(jexnom(mlggma, nomgrm), 'LONUTI', nbma, k8b)
                    if (nbma .eq. 0) then
                        call u2mesk('A', 'UTILITAI3_47', 1, nomgrm)
                        goto 50
                    endif
                    call jeveuo(jexnom(mlggma, nomgrm), 'L', jad)
                    call mesomm(chelem, mxvale, ibid, zr(lvale), c16b,&
                                nbma, zi(jad))
                    sigmaw = coesym*zr(lvale)* (sref**mref)
                    probaw = sigmaw/ (sref**mref)
                    probaw = 1.0d0 - exp(-probaw)
                    rtval(3) = sigmaw
                    rtval(2) = probaw
                    rtval(1) = sigmaw** (1.0d0/mref)
                    vale2(1) = nomgrm
                    if (nr .ne. 0) then
                        valer(2) = rtval(1)
                        valer(3) = rtval(2)
                        valer(4) = rtval(3)
                        call tbajli(resu, nbparr, noparr, numord, valer,&
                                    c16b, vale2, 0)
                    else
                        call tbajli(resu, nbpard, nopard, numord, rtval,&
                                    c16b, vale2, 0)
                    endif
50              continue
                call jedetr('&&PEWEIB_GROUPM')
            endif
!
            if (nm .ne. 0) then
                nbmail = -nm
                call wkvect('&&PEWEIB_MAILLE', 'V V K8', nbmail, jma)
                call getvem(noma, 'MAILLE', motcl3, 'MAILLE', inum,&
                            iarg, nbmail, zk8(jma), nm)
                valek(2) = 'MAILLE'
                do 60 im = 1, nbmail
                    nommai = zk8(jma+im-1)
                    call jeexin(jexnom(mlgnma, nommai), iret)
                    if (iret .eq. 0) then
                        call u2mesk('A', 'UTILITAI3_49', 1, nommai)
                        goto 60
                    endif
                    call jenonu(jexnom(mlgnma, nommai), nume)
                    call mesomm(chelem, mxvale, ibid, zr(lvale), c16b,&
                                1, nume)
                    probaw = coesym*zr(lvale)
                    sigmaw = probaw* (sref**mref)
                    probaw = 1.0d0 - exp(-probaw)
                    rtval(3) = sigmaw
                    rtval(2) = probaw
                    rtval(1) = sigmaw** (1.0d0/mref)
                    valek(1) = nommai
                    if (nr .ne. 0) then
                        valer(2) = rtval(1)
                        valer(3) = rtval(2)
                        valer(4) = rtval(3)
                        call tbajli(resu, nbparr, noparr, numord, valer,&
                                    c16b, valek, 0)
                    else
                        call tbajli(resu, nbpard, nopard, numord, rtval,&
                                    c16b, valek, 0)
                    endif
60              continue
                call jedetr('&&PEWEIB_MAILLE')
            endif
70      continue
!
        call detrsd('CHAMP_GD', '&&PEWEIB.EPSG')
        call detrsd('CARTE', '&&PEWEIB.CH.SOUSOP')
        call jedema()
80  continue
! FIN BOUCLE SUR LES NUMEROS D ORDRE
90  continue
!
! -- MENAGE
    call jedetr('&&PEWEIB.NUME_ORDRE')
    call jedetr('&&PEWEIB.INSTANT')
    call detrsd('CHAMP_GD', '&&PEWEIB.SIGIE')
    call detrsd('CHAMP_GD', '&&PEWEIB.SIGIS')
    call jedetr('&&PEWEIB.TRAV1')
    call jedetr('&&PEWEIB.L_NOM_MAT')
!
100  continue
!
    call jedema()
end subroutine

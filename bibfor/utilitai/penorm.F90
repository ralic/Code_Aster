subroutine penorm(resu, modele)
    implicit   none
    include 'jeveux.h'
    include 'asterc/getvid.h'
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/alchml.h'
    include 'asterfort/assert.h'
    include 'asterfort/calcul.h'
    include 'asterfort/celces.h'
    include 'asterfort/cescel.h'
    include 'asterfort/cesred.h'
    include 'asterfort/chpchd.h'
    include 'asterfort/chsut1.h'
    include 'asterfort/cnocns.h'
    include 'asterfort/cnscno.h'
    include 'asterfort/codent.h'
    include 'asterfort/copisd.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dismlg.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mecact.h'
    include 'asterfort/megeom.h'
    include 'asterfort/mesomm.h'
    include 'asterfort/nopar2.h'
    include 'asterfort/reliem.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/rsorac.h'
    include 'asterfort/rsutnu.h'
    include 'asterfort/tbajli.h'
    include 'asterfort/tbajpa.h'
    include 'asterfort/tbcrsd.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/utflmd.h'
    include 'asterfort/wkvect.h'
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
!     ------------------------------------------------------------------
!     OPERATEUR :  POST_ELEM
!     TRAITEMENT DU MOT CLE-FACTEUR : "NORME"
!     ------------------------------------------------------------------
!
! TOLE CRS_512
! TOLE CRP_20
    integer :: ibid, iret, nbmato, nr, nd, np, nc, ni, no, nli, nlo, nco
    integer :: jno, jin, jcoef, jco
    integer :: nbpar, nbpmax, inum, numo, iresma, nbordr, jlicmp, jlicm1, jma
    integer :: nn
    integer :: jlicm2, i, nncp, nbma, jvalk, jvalr, jvali, ncmpm, ncp, ifm, niv
    integer :: jlicmx, nb30, ncmpt, nbcoef
    parameter(nbpmax=13,nb30=30)
    real(kind=8) :: r8b, prec, inst, vnorm(1)
    complex(kind=8) :: c16b
    logical :: exiord
    character(len=4) :: tych, ki, exirdm
    character(len=8) :: mailla, k8b, resuco, chamg, typmcl(1), tout
    character(len=8) :: tmpres, typpar(nbpmax), nomgd, lpain(3), lpawpg(1), crit
    character(len=8) :: nopar, infoma, lpaout(1), tynorm
    character(len=19) :: knum, kins, lisins, cham2, chamtm, celmod, ligrel
    character(len=19) :: liscoe
    character(len=19) :: tmpcha, cham1
    character(len=16) :: optio2, nomcha, valk, nompar(nbpmax), mocles(1), option
    character(len=24) :: mesmai, mesmaf, valr, vali, lchin(3), lchwpg(1)
    character(len=24) :: lchout(1)
    character(len=24) :: chgeom, coefca, valk2(5), grouma
    integer :: iarg
!     ------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
! --- 1- RECUPERATION DU MAILLAGE ET DU NOMBRE DE MAILLES
!     ===================================================
    call dismoi('F', 'NOM_MAILLA', modele, 'MODELE', ibid,&
                mailla, iret)
    call dismoi('F', 'NB_MA_MAILLA', mailla, 'MAILLAGE', nbmato,&
                k8b, iret)
!
!
! --- 2- RECUPERATION DU RESULTAT ET DES NUMEROS D'ORDRE
!     ==================================================
    call getvid('NORME', 'RESULTAT', 1, iarg, 1,&
                resuco, nr)
    call getvid('NORME', 'CHAM_GD', 1, iarg, 1,&
                chamg, nd)
!
    call getvr8('NORME', 'PRECISION', 1, iarg, 1,&
                prec, np)
    call getvtx('NORME', 'CRITERE', 1, iarg, 1,&
                crit, nc)
    call getvr8('NORME', 'INST', 1, iarg, 0,&
                r8b, ni)
    call getvis('NORME', 'NUME_ORDRE', 1, iarg, 0,&
                ibid, no)
    call getvid('NORME', 'LIST_INST', 1, iarg, 0,&
                k8b, nli)
    call getvid('NORME', 'LIST_ORDRE', 1, iarg, 0,&
                k8b, nlo)
    call getvtx('NORME', 'TYPE_NORM', 1, iarg, 1,&
                tynorm, iret)
    call getvr8('NORME', 'COEF_MULT', 1, iarg, 0,&
                r8b, nco)
!
    valr = '&&PENORM.VALR'
    vali = '&&PENORM.VALI'
    valk = '&&PENORM.VALK'
    knum = '&&PENORM.NUME_ORDRE'
    kins = '&&PENORM.INST'
    mesmai = '&&PENORM.MES_MAILLES'
    mesmaf = '&&PENORM.MAIL_FILTRE'
    cham1 = '&&PENORM.CHAM1'
    cham2 = '&&PENORM.CHAM2'
    chamtm = '&&PENORM.CHAMTM'
    ligrel = '&&PENORM.LIGREL'
    coefca = '&&PENORM.CARTE_COEF'
    liscoe = '&&PENORM.COEFMULT'
!
    exiord=.false.
    ncmpt =0
!
    if (nd .ne. 0) then
!
        nbordr = 1
        call wkvect(knum, 'V V I', nbordr, jno)
        zi(jno) = 1
        exiord=.true.
!
    else
!
!       -- NUME_ORDRE --
        if (no .ne. 0) then
            exiord=.true.
            nbordr=-no
            call wkvect(knum, 'V V I', nbordr, jno)
            call getvis('NORME', 'NUME_ORDRE', 1, iarg, nbordr,&
                        zi(jno), iret)
        endif
!
!       -- LIST_ORDRE --
        if (nlo .ne. 0) then
            exiord=.true.
            call getvid('NORME', 'LIST_ORDRE', 1, iarg, 1,&
                        lisins, iret)
            call jeveuo(lisins // '.VALE', 'L', jno)
            call jelira(lisins // '.VALE', 'LONMAX', nbordr, k8b)
        endif
!
!       -- INST --
        if (ni .ne. 0) then
            nbordr=-ni
            call wkvect(kins, 'V V R', nbordr, jin)
            call getvr8('NORME', 'INST', 1, iarg, nbordr,&
                        zr(jin), iret)
        endif
!
!       -- LIST_INST --
        if (nli .ne. 0) then
            call getvid('NORME', 'LIST_INST', 1, iarg, 1,&
                        lisins, iret)
            call jeveuo(lisins // '.VALE', 'L', jin)
            call jelira(lisins // '.VALE', 'LONMAX', nbordr, k8b)
        endif
!
!       -- TOUT_ORDRE --
        nn=nli+ni+no+nlo
        if (nn .eq. 0) then
            exiord=.true.
            call rsutnu(resuco, ' ', 0, knum, nbordr,&
                        prec, crit, iret)
            call jeveuo(knum, 'L', jno)
        endif
!
    endif
!
! --- 3- CREATION DE LA TABLE
!     =======================
    call tbcrsd(resu, 'G')
    if (nr .ne. 0) then
!
        nbpar=8
!
        nompar(1) ='RESULTAT'
        nompar(2) ='NOM_CHAM'
        nompar(3) ='NUME_ORDRE'
        nompar(4) ='INST'
        nompar(5) ='GROUP_MA'
        nompar(6) ='TYPE_MAIL'
        nompar(7) ='TYPE_NORM'
        nompar(8) ='VALE_NORM'
!
        typpar(1) ='K8'
        typpar(2) ='K16'
        typpar(3) ='I'
        typpar(4) ='R'
        typpar(5) ='K24'
        typpar(6) ='K8'
        typpar(7) ='K8'
        typpar(8) ='R'
    else
!
        nbpar=5
!
        nompar(1)='CHAM_GD'
        nompar(2)='GROUP_MA'
        nompar(3)='TYPE_MAIL'
        nompar(4)='TYPE_NORM'
        nompar(5)='VALE_NORM'
!
        typpar(1)='K8'
        typpar(2)='K24'
        typpar(3)='K8'
        typpar(4)='K8'
        typpar(5)='R'
!
    endif
    call tbajpa(resu, nbpar, nompar, typpar)
!
! --- 4- REMPLISSAGE DE LA TABLE
!     ==========================
!
!  -- ON COPIE LE CHAMP POUR NE PAS MODIFIER LA DISCRETISATION
!     DU CHAMP INITIAL
    if (nr .ne. 0) then
        tmpres='TMP_RESU'
        call copisd('RESULTAT', 'V', resuco, tmpres)
    else
        tmpcha='TMP_CHAMP_GD'
        call copisd('CHAMP', 'V', chamg, tmpcha)
    endif
!
!     -- VERIFICATION DE L'UTILISATION DE COEF_MULT
    if (nr .eq. 0 .and. nco .ne. 0) then
        call dismoi('C', 'NOM_GD', tmpcha, 'CHAMP', ibid,&
                    nomgd, iret)
        if (nomgd(1:6) .ne. 'NEUT_R') then
            call u2mess('F', 'POSTELEM_2')
        endif
    endif
!
!  --- BOUCLE SUR LES NUMEROS D'ORDRE:
!     ----------------------------------
    do 5 inum = 1, nbordr
!
!      -- 4.1 RECUPERATION DU CHAMP --
!
        if (nr .ne. 0) then
!         --  RESULTAT --
            if (exiord) then
!           - ORDRE -
                numo=zi(jno+inum-1)
                call rsadpa(resuco, 'L', 1, 'INST', numo,&
                            0, jin, k8b)
                inst=zr(jin)
            else
!           - INST -
                inst=zr(jin+inum-1)
                call rsorac(resuco, 'INST', 0, zr(jin+inum-1), k8b,&
                            c16b, prec, crit, numo, nbordr,&
                            iret)
            endif
            call getvtx('NORME', 'NOM_CHAM', 1, iarg, 1,&
                        nomcha, iret)
            if (iret .eq. 0) call u2mess('F', 'POSTELEM_4')
            call rsexch(' ', tmpres, nomcha, numo, cham2,&
                        iret)
!
        else
!         -- CHAM_GD --
            numo = nbordr
            cham2 = tmpcha
            nomcha= chamg
        endif
!
        call dismoi('C', 'TYPE_CHAMP', cham2, 'CHAMP', ibid,&
                    tych, iret)
        call dismoi('C', 'NOM_GD', cham2, 'CHAMP', ibid,&
                    nomgd, iret)
!
!         ON RESTREINT LE CALCUL DE LA NORME AUX CHAMPS DE DEPLACEMENTS,
!         CONTRAINTES, DEFORMATION, TEMPERATURE, FLUX ...
        if (nomgd(6:6) .eq. 'C') goto 999
        if (nomgd(1:4) .ne. 'DEPL' .and. nomgd(1:4) .ne. 'EPSI' .and. nomgd(1:4) .ne. 'TEMP'&
            .and. nomgd(1:4) .ne. 'FLUX' .and. nomgd(1:4) .ne. 'SIEF' .and. nomgd(1:6) .ne.&
            'NEUT_R') goto 999
!
!      -- 4.2 RECUPERATION DES MAILLES --
!
        call getvtx('NORME', 'TOUT', 1, iarg, 1,&
                    tout, iret)
!
        if (iret .ne. 0) then
            mocles(1) = 'TOUT'
            typmcl(1) = 'TOUT'
            grouma = '-'
        else
            mocles(1) = 'GROUP_MA'
            typmcl(1) = 'GROUP_MA'
            call getvtx('NORME', 'GROUP_MA', 1, iarg, 1,&
                        grouma, iret)
        endif
!
!       - MAILLES FOURNIES PAR L'UTILISATEUR -
        call reliem(modele, mailla, 'NU_MAILLE', 'NORME', 1,&
                    1, mocles, typmcl, mesmai, nbma)
!
!       - MAILLES EVENTUELLEMENT FILTREES EN FONCTION DE LA DIMENSION
!         GEOMETRIQUE (2D OU 3D)
        call getvtx('NORME', 'TYPE_MAILLE', 1, iarg, 1,&
                    infoma, iret)
        if (iret .ne. 0) then
            if (infoma(1:2) .eq. '2D') then
                iresma=2
            else if (infoma(1:2).eq.'3D') then
                iresma=3
            else
                call assert(.false.)
            endif
            call utflmd(mailla, mesmai, iresma, ' ', nbma,&
                        mesmaf)
            call jedetr(mesmai)
            mesmai=mesmaf
        else
            infoma='-'
        endif
!
!       - VERIFICATION SI ON VA TRAITER DES ELEMENTS DE STRUCTURE
        call dismlg('EXI_RDM', mesmai, ibid, exirdm, iret)
        if (exirdm .eq. 'OUI') then
            call u2mess('F', 'UTILITAI8_60')
        endif
!
!       - INFOS
        if (niv .gt. 1) then
            write(6,*) '<PENORM> NOMBRE DE MAILLES A TRAITER : ',nbma
        endif
!
!      -- 4.3 CHANGEMENT DE LA GRANDEUR DU CHAMP (-> NEUT_R) --
!
        if (tych(1:4) .eq. 'NOEU') then
            call cnocns(cham2, 'V', chamtm)
            call jeveuo(chamtm//'.CNSC', 'L', jlicmp)
            call jelira(chamtm//'.CNSC', 'LONMAX', ncmpm, k8b)
        else if (tych(1:2).eq.'EL') then
            call celces(cham2, 'V', chamtm)
            call jeveuo(chamtm//'.CESC', 'L', jlicmp)
            call jelira(chamtm//'.CESC', 'LONMAX', ncmpm, k8b)
        endif
        call jedetr('&&PENORM.CMP1')
        call wkvect('&&PENORM.CMP1', 'V V K8', ncmpm, jlicm1)
        call jedetr('&&PENORM.CMP2')
        call wkvect('&&PENORM.CMP2', 'V V K8', ncmpm, jlicm2)
        do 15 i = 1, ncmpm
            call codent(i, 'G', ki)
            zk8(jlicm2+i-1)='X'//ki(1:len(ki))
            zk8(jlicm1+i-1)=zk8(jlicmp+i-1)
15      continue
        ligrel = modele//'.MODELE'
        call chsut1(chamtm, 'NEUT_R', ncmpm, zk8(jlicm1), zk8(jlicm2),&
                    'V', chamtm)
!
!       - INFOS
        if (niv .gt. 1) then
            write(6,*) '<PENORM> NOMBRE DE COMPOSANTES : ',ncmpm
        endif
!
!      -- 4.4 CREATION D'UNE CARTE DE COEFFICIENTS
!             (UTILE POUR LES TENSEURS) --
!
        call jedetr('&&PENORM.COEF')
        call wkvect('&&PENORM.COEF', 'V V R', nb30, jcoef)
        call jedetr('&&PENORM.CMPX')
        call wkvect('&&PENORM.CMPX', 'V V K8', nb30, jlicmx)
        if (nco .eq. 0) then
            do 18 i = 1, nb30
                call codent(i, 'G', ki)
                zk8(jlicmx+i-1)='X'//ki(1:len(ki))
                zr(jcoef+i-1)=1.d0
18          continue
        else
            do 19 i = 1, nb30
                call codent(i, 'G', ki)
                zk8(jlicmx+i-1)='X'//ki(1:len(ki))
                zr(jcoef+i-1)=0.d0
19          continue
        endif
        if (nomgd(1:4) .eq. 'DEPL') then
            do 20 i = 1, ncmpm
                if (zk8(jlicm1+i-1) .eq. 'DX      ' .or. zk8(jlicm1+i-1) .eq. 'DY      '&
                    .or. zk8(jlicm1+i-1) .eq. 'DZ      ') then
                    zr(jcoef+i-1)=1.d0
                    ncmpt=ncmpt+1
                else
                    zr(jcoef+i-1)=0.d0
                endif
20          continue
        else if (nomgd(1:4).eq.'TEMP') then
            do 21 i = 1, ncmpm
                if (zk8(jlicm1+i-1) .eq. 'TEMP    ') then
                    zr(jcoef+i-1)=1.d0
                    ncmpt=ncmpt+1
                else
                    zr(jcoef+i-1)=0.d0
                endif
21          continue
        else if (nomgd(1:4).eq.'FLUX') then
            do 22 i = 1, ncmpm
                if (zk8(jlicm1+i-1) .eq. 'FLUX    ' .or. zk8(jlicm1+i-1) .eq. 'FLUY    '&
                    .or. zk8(jlicm1+i-1) .eq. 'FLUZ    ') then
                    zr(jcoef+i-1)=1.d0
                    ncmpt=ncmpt+1
                else
                    zr(jcoef+i-1)=0.d0
                endif
22          continue
        else if (nomgd(1:4).eq.'EPSI') then
            do 23 i = 1, ncmpm
                if (zk8(jlicm1+i-1) .eq. 'EPXX    ' .or. zk8(jlicm1+i-1) .eq. 'EPYY    '&
                    .or. zk8(jlicm1+i-1) .eq. 'EPZZ    ') then
                    zr(jcoef+i-1)=1.d0
                    ncmpt=ncmpt+1
                    elseif(zk8(jlicm1+i-1).eq.'EPXY    '.or.&
     &               zk8(jlicm1+i-1).eq.'EPXZ    '.or.&
     &               zk8(jlicm1+i-1).eq.'EPYZ    ')then
                    zr(jcoef+i-1)=2.d0
                    ncmpt=ncmpt+1
                else
                    zr(jcoef+i-1)=0.d0
                endif
23          continue
        else if (nomgd(1:4).eq.'SIEF') then
            do 24 i = 1, ncmpm
                if (zk8(jlicm1+i-1) .eq. 'SIXX    ' .or. zk8(jlicm1+i-1) .eq. 'SIYY    '&
                    .or. zk8(jlicm1+i-1) .eq. 'SIZZ    ') then
                    zr(jcoef+i-1)=1.d0
                    ncmpt=ncmpt+1
                    elseif(zk8(jlicm1+i-1).eq.'SIXY    '.or.&
     &               zk8(jlicm1+i-1).eq.'SIXZ    '.or.&
     &               zk8(jlicm1+i-1).eq.'SIYZ    ')then
                    zr(jcoef+i-1)=2.d0
                    ncmpt=ncmpt+1
                else
                    zr(jcoef+i-1)=0.d0
                endif
24          continue
        else if (nomgd(1:6).eq.'NEUT_R') then
            if (nco .ne. 0) then
                nbcoef=-nco
                call wkvect(liscoe, 'V V R', nbcoef, jno)
                call getvr8('NORME', 'COEF_MULT', 1, iarg, nbcoef,&
                            zr(jno), iret)
                do 25 i = 1, nbcoef
                    zr(jcoef+i-1)=zr(jno+i-1)
                    if (zr(jno+i-1) .ne. 0.d0) ncmpt=ncmpt+1
25              continue
            endif
        endif
!
!       - INFOS
        if (niv .gt. 1) then
            write(6,*) '<PENORM> NOMBRE DE COMPOSANTES TRAITEES: ',&
            ncmpt
        endif
!
        call mecact('V', coefca, 'MODELE', modele, 'NEUT_R',&
                    nb30, zk8( jlicmx), ibid, zr(jcoef), c16b,&
                    k8b)
!
        if (tych(1:4) .eq. 'NOEU') then
            call cnscno(chamtm, ' ', 'OUI', 'V', cham2,&
                        'F', iret)
            call cnocns(cham2, 'V', chamtm)
            call detrsd('CHAM_NO_S', chamtm)
        else if (tych(1:4).eq.'ELNO') then
            optio2 ='TOU_INI_ELNO'
            nopar = nopar2(optio2,'NEUT_R','OUT')
            call cescel(chamtm, ligrel, optio2, nopar, 'OUI',&
                        nncp, 'V', cham2, 'F', ibid)
            call detrsd('CHAM_ELEM_S', chamtm)
        else if (tych(1:4).eq.'ELGA') then
            optio2 ='TOU_INI_ELGA'
            nopar = nopar2(optio2,'NEUT_R','OUT')
            call cescel(chamtm, ligrel, optio2, nopar, 'OUI',&
                        nncp, 'V', cham1, 'F', ibid)
            call detrsd('CHAM_ELEM_S', chamtm)
        else
            goto 999
        endif
!
!      -- 4.4 CHANGEMENT DE DISCRETISATION DU CHAMP --
!
        if (tych(1:4) .ne. 'ELGA') then
            optio2 ='NORME_L2'
            nopar='PCHAMPG'
            celmod = '&&PENORM.CELMOD'
            call alchml(ligrel, optio2, nopar, 'V', celmod,&
                        ibid, ' ')
            if (ibid .ne. 0) then
                valk2(1)=ligrel
                valk2(2)=nopar
                valk2(3)=optio2
                call u2mesk('F', 'UTILITAI3_23', 3, valk2)
            endif
            call chpchd(cham2, 'ELGA', celmod, 'OUI', 'V',&
                        cham1)
            call detrsd('CHAMP', celmod)
        endif
!
!      -- 4.5 REDUCTION DU CHAMP EN FONCTION DES MAILLES --
!
        call celces(cham1, 'V', chamtm)
        call jeveuo(mesmai, 'L', jma)
        call cesred(chamtm, nbma, zi(jma), 0, k8b,&
                    'V', chamtm)
        optio2 ='NORME_L2'
        nopar='PCHAMPG'
        call cescel(chamtm, ligrel, optio2, nopar, 'OUI',&
                    nncp, 'V', cham1, 'F', ibid)
        call detrsd('CHAM_ELEM_S', chamtm)
!
!      -- 4.6 CALCUL DES COORD. ET DES POIDS DES POINTS DE GAUSS
!
        call megeom(modele, chgeom)
        lpain(1) = 'PGEOMER'
        lchin(1) = chgeom
        lchwpg(1)='&&PEECAL.PGCOOR'
        lpawpg(1)='PCOORPG'
        option = 'COOR_ELGA'
!
        call calcul('S', option, ligrel, 1, lchin,&
                    lpain, 1, lchwpg, lpawpg, 'V',&
                    'OUI')
!
!      -- 4.7 CALCUL DE LA NORME SUR CHAQUE ELEMENT --
!
        lpaout(1) = 'PNORME'
        lchout(1) = '&&PNORME'
        lpain(1) = 'PCOORPG'
        lchin(1) = lchwpg(1)
        lpain(2) = 'PCHAMPG'
        lchin(2) = cham1
        lpain(3) = 'PCOEFR'
        lchin(3) = coefca
        option = 'NORME_L2'
!
        call calcul('S', option, ligrel, 3, lchin,&
                    lpain, 1, lchout, lpaout, 'V',&
                    'OUI')
!
!      -- 4.8 SOMMATION DE LA NORME SUR LES ELEMENTS DESIRES --
!
        call mesomm(lchout(1), 1, ibid, vnorm, c16b,&
                    0, ibid)
!
!      -- 4.9 ON REMPLIT LA TABLE --
!
        if (nompar(1) .eq. 'RESULTAT') then
            call wkvect(valk, 'V V K24', 4, jvalk)
            zk24(jvalk) =resuco
            zk24(jvalk+1)=nomcha
            zk24(jvalk+2)=grouma
            zk24(jvalk+3)=infoma
            zk24(jvalk+4)=tynorm
            call wkvect(valr, 'V V R', 2, jvalr)
            zr(jvalr)=inst
            zr(jvalr+1)=sqrt(vnorm(1))
            call wkvect(vali, 'V V I', 1, jvali)
            zi(jvali)=numo
            call tbajli(resu, nbpar, nompar, zi(jvali), zr(jvalr),&
                        c16b, zk24(jvalk), 0)
        else
            call wkvect(valk, 'V V K24', 3, jvalk)
            zk24(jvalk) =nomcha
            zk24(jvalk+1)=grouma
            zk24(jvalk+2)=infoma
            zk24(jvalk+3)=tynorm
            call wkvect(valr, 'V V R', 1, jvalr)
            zr(jvalr)=sqrt(vnorm(1))
            call tbajli(resu, nbpar, nompar, ibid, zr(jvalr),&
                        c16b, zk24(jvalk), 0)
        endif
!
!      -- 4.10 NETTOYAGE POUR L'OCCURRENCE SUIVANTE --
!
        call detrsd('CHAMP', cham1)
        call detrsd('CHAMP', cham2)
        call jedetr(valr)
        call jedetr(vali)
        call jedetr(valk)
        call jedetr(mesmai)
!
!     --- FIN DE LA BOUCLE SUR LES NUMEROS D'ORDRE:
!     ---------------------------------------------
 5  continue
!
!     --- FIN DE LA BOUCLE SUR LES OCCURRENCES DU MOT-CLE NORME
!     ---------------------------------------------------------
999  continue
!
    if (nr .ne. 0) then
        call detrsd('RESULTAT', tmpres)
    else
        call detrsd('CHAMP', tmpcha)
    endif
!
    call jedema()
end subroutine

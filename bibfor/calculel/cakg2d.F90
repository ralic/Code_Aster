subroutine cakg2d(optioz, result, modele, depla, theta,&
                  mate, nchar, lchar, symech, fondf,&
                  noeud, time, iord, nbprup, noprup,&
                  lmelas, nomcas, lmoda, puls, compor)
! aslint: disable=W1504
    implicit none
!
    include 'jeveux.h'
!
    include 'asterc/getvid.h'
    include 'asterc/getvis.h'
    include 'asterfort/assert.h'
    include 'asterfort/calcul.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/gcharg.h'
    include 'asterfort/impfic.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/mecact.h'
    include 'asterfort/megeom.h'
    include 'asterfort/mesomm.h'
    include 'asterfort/tbajli.h'
    include 'asterfort/tbajvi.h'
    include 'asterfort/tbajvk.h'
    include 'asterfort/tbajvr.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/vrcins.h'
    include 'asterfort/vrcref.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: modele, lchar(*), fondf, result, symech
    character(len=8) :: noeud
    character(len=16) :: optioz, noprup(*), nomcas
    character(len=24) :: depla, mate, theta, compor
    real(kind=8) :: time, puls
    integer :: iord, nchar, nbprup
    logical :: lmelas, lmoda
! ......................................................................
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
!     - FONCTION REALISEE:   CALCUL DES COEFFICIENTS D'INTENSITE DE
!                            CONTRAINTES K1 ET K2 EN 2D
!
! IN   OPTION  --> CALC_K_G   (SI CHARGES REELLES)
!              --> CALC_K_G_F (SI CHARGES FONCTIONS)
!              --> CALC_K_X   (SI FISSURE X-FEM)
! IN   RESULT  --> NOM UTILISATEUR DU RESULTAT ET TABLE
! IN   MODELE  --> NOM DU MODELE
! IN   DEPLA   --> CHAMPS DE DEPLACEMENT
! IN   THETA   --> CHAMP THETA
! IN   MATE    --> CHAMP DE MATERIAUX
! IN   NCHAR   --> NOMBRE DE CHARGES
! IN   LCHAR   --> LISTE DES CHARGES
! IN   SYMECH  --> SYMETRIE DU CHARGEMENT
! IN   FONDF   --> FOND DE FISSURE
! IN   NOEUD   --> NOM DU NOEUD DU FOND
! IN   TIME    --> INSTANT DE CALCUL
! IN   IORD    --> NUMERO D'ORDRE DE LA SD
! IN   NBPRUP  --> NOMBRE DE PARAMETRES RUPTURE DANS LA TABLE
! IN   NOPRUP  --> NOMS DES PARAMETRES RUPTURE
! IN   LMELAS  --> TRUE SI LE TYPE DE LA SD RESULTAT = MULT_ELAS
! IN   NOMCAS  --> NOM DU CAS DE CHARGE SI LMELAS
! IN   LMODA   --> TRUE SI LE TYPE DE LA SD RESULTAT = MODE_MECA
! IN   PULS    --> PULSATION SI LMODA
! IN   COMPOR  --> COMPORTEMENT
! ......................................................................
! CORPS DU PROGRAMME
!
    integer :: nbmxpa
    parameter (nbmxpa = 20)
!
    integer :: nbinmx, nboumx
    parameter   (nbinmx=50,nboumx=1)
    character(len=8) :: lpain(nbinmx), lpaout(nboumx)
    character(len=24) :: lchin(nbinmx), lchout(nboumx)
!
    integer :: i, ibid, ific, inorma, init, ifm, niv, jnor, jbasfo
    integer :: iadrma, iadrff, icoode, iadrco, iadrno
    integer :: lobj2, ndimte, nunoff, ndim, nchin, ier, ixfem, jfond, numfon
    integer :: iret, livi(nbmxpa)
    real(kind=8) :: fic(5), rcmp(4), livr(nbmxpa), girwin
    integer :: mxstac
    complex(kind=8) :: cbid, livc(nbmxpa)
    logical :: fonc, epsi
    parameter   (mxstac=1000)
    character(len=2) :: codret
    character(len=8) :: noma, fond, licmp(4), typmo, fiss, mosain
    character(len=8) :: k8bid
    character(len=16) :: option, optio2, valk
    character(len=19) :: cf1d2d, chpres, chrota, chpesa, chvolu, cf2d3d, chepsi
    character(len=19) :: chvref, chvarc
    character(len=19) :: basefo
    character(len=19) :: basloc, pintto, cnseto, heavto, loncha, lnno, ltno
    character(len=19) :: pmilto
    character(len=19) :: pinter, ainter, cface, longco, baseco
    character(len=24) :: chgeom, chfond
    character(len=24) :: ligrmo, nomno, norma
    character(len=24) :: obj1, obj2, coord, coorn, chtime
    character(len=24) :: pavolu, pa1d2d, papres, chpuls, chsigi, livk(nbmxpa)
!
    character(len=1) :: k1bid
    integer :: iarg
    data chvarc/'&&CAKG2D.CH_VARC_R'/
    data chvref/'&&CAKG2D.CHVREF'/
!
!
    call jemarq()
    iadrno = 1
!
!     VERIF QUE LES TABLEAUX LOCAUX DYNAMIQUES NE SONT PAS TROP GRANDS
!     (VOIR CRS 1404)
    call assert(nchar.le.mxstac)
!
    call infniv(ifm, niv)
    option = optioz
    if (optioz .eq. 'CALC_K_X') option = 'CALC_K_G'
!
!- RECUPERATION DU CHAMP GEOMETRIQUE
!
    call megeom(modele, chgeom)
    noma = chgeom(1:8)
!
!- RECUPERATION DE L'ETAT INITIAL (NON TRAITE DANS CETTE OPTION)
!
    call getvid('COMP_INCR', 'SIGM_INIT', 1, iarg, 1,&
                chsigi, init)
    if (init .ne. 0) then
        valk='CALC_K_G'
        call u2mesk('F', 'RUPTURE1_13', 1, valk)
    endif
!
!
!- RECUPERATION (S'ILS EXISTENT) DES CHAMP DE TEMPERATURES (T,TREF)
    call vrcins(modele, mate, 'BIDON', time, chvarc,&
                codret)
    call vrcref(modele, mate(1:8), 'BIDON   ', chvref(1:19))
!
! - TRAITEMENT DES CHARGES
!
    chvolu = '&&CAKG2D.VOLU'
    cf1d2d = '&&CAKG2D.1D2D'
    cf2d3d = '&&CAKG2D.2D3D'
    chpres = '&&CAKG2D.PRES'
    chepsi = '&&CAKG2D.EPSI'
    chpesa = '&&CAKG2D.PESA'
    chrota = '&&CAKG2D.ROTA'
    call gcharg(modele, nchar, lchar, chvolu, cf1d2d,&
                cf2d3d, chpres, chepsi, chpesa, chrota,&
                fonc, epsi, time, iord)
    if (fonc) then
        pavolu = 'PFFVOLU'
        pa1d2d = 'PFF1D2D'
        papres = 'PPRESSF'
        option = 'CALC_K_G_F'
        optio2 = 'CALC_K_G_F'
    else
        pavolu = 'PFRVOLU'
        pa1d2d = 'PFR1D2D'
        papres = 'PPRESSR'
        optio2 = 'CALC_K_G'
    endif
!
! OBJET DECRIVANT LE MAILLAGE
!
    obj1 = modele//'.MODELE    .LGRF'
    call jeveuo(obj1, 'L', iadrma)
    noma = zk8(iadrma)
    nomno = noma//'.NOMNOE'
    coorn = noma//'.COORDO    .VALE'
    coord = noma//'.COORDO    .DESC'
    call jeveuo(coorn, 'L', iadrco)
    call jeveuo(coord, 'L', icoode)
    ndim = -zi(icoode-1+2)
!
!
!     CAS X-FEM
!     MOSAIN = MODELE ISSU DE AFFE_MODELE
    call getvid('THETA', 'FISSURE', 1, iarg, 1,&
                fiss, ixfem)
    if (ixfem .gt. 0) then
        call dismoi('F', 'NOM_MODELE', fiss, 'FISS_XFEM', ibid,&
                    mosain, ier)
        call dismoi('F', 'MODELISATION', mosain, 'MODELE', ibid,&
                    typmo, ier)
    else
        call dismoi('F', 'MODELISATION', modele, 'MODELE', ibid,&
                    typmo, ier)
    endif
!
!     OBJET CONTENANT LES NOEUDS DU FOND DE FISSURE
    if (ixfem .eq. 0) then
        fond = fondf(1:8)
        obj2 = fond//'.FOND.NOEU'
        call jelira(obj2, 'LONMAX', lobj2, k1bid)
        if (lobj2 .ne. 1) then
            call u2mess('F', 'RUPTURE1_10')
        endif
        call jeveuo(obj2, 'L', iadrno)
        call jenonu(jexnom(nomno, zk8(iadrno)), nunoff)
!
!       OBJET CONTENANT LA BASE LOCALE AU FOND DE FISSURE
!       SI L'OBJET NORMALE EXISTE, ON LE PREND
!       SINON, ON PREND BASEFOND
        norma = fond//'.NORMALE'
        call jeexin(norma, iret)
        if (iret .ne. 0) then
            call jeveuo(norma, 'L', inorma)
            rcmp(3) = zr(inorma-1+1)
            rcmp(4) = zr(inorma-1+2)
        else if (iret.eq.0) then
            basefo = fond//'.BASEFOND'
            call jeveuo(basefo, 'L', jbasfo)
!         ATTENTION, ON NE SE SERT PAS DU VECTEUR NORMAL DE BASEFOND
!         MAIS ON FAIT TOURNER DE 90 DEGRES LE VECTEUR DE PROPA
            rcmp(3) = -zr(jbasfo-1+4)
            rcmp(4) = zr(jbasfo-1+3)
        endif
    endif
!
!
!     CREATION OBJET CONTENANT COORDONNEES DU NOEUD DE FOND
!     DE FISSURE ET LA NORMALE A LA FISSURE
    chfond = '&&CAKG2D.FOND'
    call wkvect(chfond, 'V V R8', 4, iadrff)
!
    licmp(1) = 'XA'
    licmp(2) = 'YA'
    licmp(3) = 'XNORM'
    licmp(4) = 'YNORM'
    if (ixfem .eq. 0) then
        rcmp(1) = zr(iadrco+ndim* (nunoff-1))
        rcmp(2) = zr(iadrco+ndim* (nunoff-1)+1)
    else
        call jeveuo(fiss//'.FONDFISS', 'L', jfond)
        call getvis('THETA', 'NUME_FOND', 1, iarg, 1,&
                    numfon, ibid)
        rcmp(1) = zr(jfond-1+4*(numfon-1)+1)
        rcmp(2) = zr(jfond-1+4*(numfon-1)+2)
        call jeveuo(fiss//'.BASEFOND', 'L', jnor)
        rcmp(3) = -zr(jnor-1+4*(numfon-1)+4)
        rcmp(4) = zr(jnor-1+4*(numfon-1)+3)
        write(ifm,*)'   '
        write(ifm,*)'    TRAITEMENT DU FOND DE FISSURE NUMERO ',numfon
        write(ifm,*)'    NOMME ',noeud
        write(ifm,*)'    DE COORDONNEES',rcmp(1),rcmp(2)
        write(ifm,*)'    LA NORMALE A LA FISSURE EN CE POINT EST ',&
     &                                              rcmp(3),rcmp(4)
    endif
    zr(iadrff) = rcmp(1)
    zr(iadrff+1) = rcmp(2)
    zr(iadrff+2) = rcmp(3)
    zr(iadrff+3) = rcmp(4)
!
    call mecact('V', chfond, 'MAILLA', noma, 'FISS_R',&
                4, licmp, ibid, rcmp, cbid,&
                ' ')
!
!
! --- RECUPERATION DES DONNEES XFEM (TOPOSE)
    pintto = modele//'.TOPOSE.PIN'
    cnseto = modele//'.TOPOSE.CNS'
    heavto = modele//'.TOPOSE.HEA'
    loncha = modele//'.TOPOSE.LON'
    pmilto = modele//'.TOPOSE.PMI'
!     ON NE PREND PAS LES LSN ET LST DU MODELE
!     CAR LES CHAMPS DU MODELE SONT DEFINIS QUE AUTOUR DE LA FISSURE
!     OR ON A BESOIN DE LSN ET LST MEME POUR LES ELEMENTS CLASSIQUES
    lnno = fiss//'.LNNO'
    ltno = fiss//'.LTNO'
    basloc = fiss//'.BASLOC'
!
! --- RECUPERATION DES DONNEES XFEM (TOPOFAC)
    pinter = modele//'.TOPOFAC.OE'
    ainter = modele//'.TOPOFAC.AI'
    cface = modele//'.TOPOFAC.CF'
    longco = modele//'.TOPOFAC.LO'
    baseco = modele//'.TOPOFAC.BA'
!
    ndimte = 5
    call wkvect('&&CAKG2D.VALG', 'V V R8', ndimte, ific)
    lpaout(1) = 'PGTHETA'
    lchout(1) = '&&FICGELE'
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpain(2) = 'PDEPLAR'
    lchin(2) = depla
    lpain(3) = 'PTHETAR'
    lchin(3) = theta
    lpain(4) = 'PMATERC'
    lchin(4) = mate
    lpain(5) = 'PVARCPR'
    lchin(5) = chvarc
    lpain(6) = 'PVARCRR'
    lchin(6) = chvref
    lpain(7) = pavolu(1:8)
    lchin(7) = chvolu
    lpain(8) = pa1d2d(1:8)
    lchin(8) = cf1d2d
    lpain(9) = papres(1:8)
    lchin(9) = chpres
    lpain(10) = 'PPESANR'
    lchin(10) = chpesa
    lpain(11) = 'PROTATR'
    lchin(11) = chrota
    lpain(12) = 'PFISSR'
    lchin(12) = chfond
!
    lpain(13) = 'PBASLOR'
    lchin(13) = basloc
    lpain(14) = 'PPINTTO'
    lchin(14) = pintto
    lpain(15) = 'PCNSETO'
    lchin(15) = cnseto
    lpain(16) = 'PHEAVTO'
    lchin(16) = heavto
    lpain(17) = 'PLONCHA'
    lchin(17) = loncha
    lpain(18) = 'PLSN'
    lchin(18) = lnno
    lpain(19) = 'PLST'
    lchin(19) = ltno
!
    lpain(20) = 'PCOMPOR'
    lchin(20) = compor
!
    lpain(21) = 'PPMILTO'
    lchin(21) = pmilto
!
    lpain(22) = 'PPINTER'
    lchin(22) = pinter
    lpain(23) = 'PAINTER'
    lchin(23) = ainter
    lpain(24) = 'PCFACE'
    lchin(24) = cface
    lpain(25) = 'PLONGCO'
    lchin(25) = longco
    lpain(26) = 'PBASECO'
    lchin(26) = baseco
!
    nchin = 26
!
    ligrmo = modele//'.MODELE'
!
    chtime = '&&CAKG2D.CH_INST_R'
    if (option .eq. 'CALC_K_G_F') then
        call mecact('V', chtime, 'MODELE', ligrmo, 'INST_R  ',&
                    1, 'INST   ', ibid, time, cbid,&
                    k8bid)
        nchin = nchin + 1
        lpain(nchin) = 'PTEMPSR'
        lchin(nchin) = chtime
    endif
!
    if (lmoda) then
        chpuls = '&&CAKG2D.PULS'
        call mecact('V', chpuls, 'MODELE', ligrmo, 'FREQ_R  ',&
                    1, 'FREQ   ', ibid, puls, cbid,&
                    ' ')
        nchin = nchin + 1
        lpain(nchin) = 'PPULPRO'
        lchin(nchin) = chpuls
    endif
!
    call calcul('S', optio2, ligrmo, nchin, lchin,&
                lpain, 1, lchout, lpaout, 'V',&
                'OUI')
!
!  SOMMATION DES FIC ET G ELEMENTAIRES
!
    call mesomm(lchout(1), 5, ibid, fic, cbid,&
                0, ibid)
!
    do 20 i = 1, 5
        zr(ific+i-1) = fic(i)
20  end do
!
    if (typmo(1:4) .eq. 'AXIS') then
        do 21 i = 1, 5
            zr(ific+i-1) = zr(ific+i-1)/rcmp(1)
21      continue
    endif
!
    if (symech .eq. 'OUI') then
        zr(ific) = 2.d0*zr(ific)
        zr(ific+1) = 2.d0*zr(ific+1)
        zr(ific+2) = 0.d0
        zr(ific+3) = 2.d0*zr(ific+3)
        zr(ific+4) = 0.d0
    endif
!
    girwin = zr(ific+1)*zr(ific+1) + zr(ific+2)*zr(ific+2)
!
! IMPRESSION DE K1,K2,G ET ECRITURE DANS LA TABLE RESU
!
    if (niv .ge. 2) then
        call impfic(zr(ific), zk8(iadrno), rcmp, ifm, ixfem)
    endif
!
    if ((ixfem.ne.0) .and. (option(1:8).eq.'CALC_K_G') .and. (.not.lmoda)) then
        call tbajvi(result, nbprup, 'NUME_FOND', numfon, livi)
    endif
!
    if (lmelas) then
        call tbajvi(result, nbprup, 'NUME_CAS', iord, livi)
        call tbajvk(result, nbprup, 'NOM_CAS', nomcas, livk)
    else if (lmoda) then
        call tbajvi(result, nbprup, 'NUME_MODE', iord, livi)
    else
        call tbajvi(result, nbprup, 'NUME_ORDRE', iord, livi)
        call tbajvr(result, nbprup, 'INST', time, livr)
    endif
!
    call tbajvr(result, nbprup, 'K1', zr(ific+3), livr)
    call tbajvr(result, nbprup, 'K2', zr(ific+4), livr)
    call tbajvr(result, nbprup, 'G', zr(ific), livr)
    call tbajvr(result, nbprup, 'G_IRWIN', girwin, livr)
    call tbajli(result, nbprup, noprup, livi, livr,&
                livc, livk, 0)
!
    call detrsd('CHAMP_GD', chtime)
    call detrsd('CHAMP_GD', chvolu)
    call detrsd('CHAMP_GD', cf1d2d)
    call detrsd('CHAMP_GD', cf2d3d)
    call detrsd('CHAMP_GD', chpres)
    call detrsd('CHAMP_GD', chepsi)
    call detrsd('CHAMP_GD', chpesa)
    call detrsd('CHAMP_GD', chrota)
    call jedetr('&&CAKG2D.VALG')
    call jedetr('&&CAKG2D.FOND')
!
    call jedema()
end subroutine

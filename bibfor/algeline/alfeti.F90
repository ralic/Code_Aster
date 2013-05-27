subroutine alfeti(opt, sdfeti, matas, chsecm, chsol,&
                  niter, epsi, criter, testco, nbreor,&
                  tyreor, preco, scalin, stogi, nbreoi,&
                  acma, acsm, reacre)
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! TOLE CRP_20
! TOLE CRP_4
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  RESOLUTION FETI EN REEL SYMETRIQUE
!                          CF. ALGO. 9 DE LA NOTE HI-23/03/009
!     ------------------------------------------------------------------
!     IN  OPT    :  K24 : OPTION DE CALCUL: "NETTOYAGE_SDI"/
!                         "NETTOYAGE_SDT"/"RESOLUTION"
!     IN  SDFETI :  K19 : SD DECRIVANT LE PARTIONNEMENT FETI
!     IN  MATAS  :  K19 : NOM DE LA MATR_ASSE GLOBALE
!     IN CHSECM  :  K19 : CHAM_NO SECOND MEMBRE GLOBAL
!     OUT CHSOL  :  K19 : CHAM_NO SOLUTION GLOBAL
!     IN  NITER  :  IN  : NOMBRE D'ITERATIONS MAXIMALES ADMISSIBLES DU
!                         GCPPC DE FETI
!     IN  EPSI   :  R8  : CRITERE D'ARRET RELATIF DU GCPPC
!     IN  CRITER :  K19 : STRUCTURE DE DONNEE STOCKANT INFOS DE CV
!     IN  TESTCO :  R8  : PARAMETRE DE TEST DE LA CONT. A L'INTERFACE
!     IN  NBREOR :  IN  : NBRE DE DD A REORTHOGONALISER
!     IN  TYREOR :  K24 : TYPE DE REORTHOGONALISATION
!     IN  PRECO  :  K24 : TYPE DE PRECONDITIONNEMENT
!     IN  SCALIN :  K24 : PARAMETRE DE SCALING DANS LE PRECOND
!     IN  STOGI  :  K24 : PARAMETRE DE STOCKAGE DE LA MATRICE GI
!     IN  NBREOI :  IN  : NBRE DE PAS DE TEMPS A REORTHOGONALISER
!     IN  ACMA/ACSM : ACCELERATION POUR PB A MULTIPLES MATRICES ET
!               SECONDS MEMBRES
!     IN  REACRE : IN   : PARAMETRE DE RECALCUL DU RESIDU
!----------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
    include 'jeveux.h'
!
    include 'asterc/matfpe.h'
    include 'asterc/r8miem.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/fetacc.h'
    include 'asterfort/fetarp.h'
    include 'asterfort/fetcpu.h'
    include 'asterfort/fetfiv.h'
    include 'asterfort/fetggt.h'
    include 'asterfort/feting.h'
    include 'asterfort/fetinl.h'
    include 'asterfort/fetmon.h'
    include 'asterfort/fetmpi.h'
    include 'asterfort/fetprc.h'
    include 'asterfort/fetprj.h'
    include 'asterfort/fetreo.h'
    include 'asterfort/fetrin.h'
    include 'asterfort/fetsca.h'
    include 'asterfort/fettor.h'
    include 'asterfort/fettsd.h'
    include 'asterfort/gcncon.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetc.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelibe.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jerazo.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    include 'blas/daxpy.h'
    include 'blas/dcopy.h'
    include 'blas/ddot.h'
    include 'blas/dnrm2.h'
    integer :: niter, nbreor, nbreoi, reacre
    real(kind=8) :: epsi, testco
    character(len=*) :: criter
    character(len=19) :: sdfeti, chsecm, chsol, matas
    character(len=24) :: tyreor, preco, scalin, stogi, acma, acsm, opt
!
!
! DECLARATION VARIABLES LOCALES
    integer :: idime, nbsd, nbi, ivlagi, ifetf, idd, ifeth, nb, i, irz, jcrr
    integer :: irr, irg, irp, ir1, ir2, iter, ifm, niv, iret, jcri, jcrk, ipsro
    integer :: iddro, nbreo2, iddfro, iaux1, iaux2, nbreo1, iaux3, ir3, irh
    integer :: dimgi, iprj, itps, ifetc, long, mod, ifiv, iinf, irex, ifeti
    integer :: ilimpi, ifetpt, nbtot, ifet1, nbi2, ipiv, mamoy, rang, ibid
    integer :: nivmpi, nbsd1, nbproc, imsmk, imsmi, dimtet, nbddsm, nbreoa
    integer :: ideeq, assmat, iacsm, iret1, iret2, ifm17, iiter, nmoyre, ninfre
    integer :: ninfma, niter1, iterm, iteri, irg1, irefe, nbddl, iadval
    real(kind=8) :: anorm, anormk, anorm0, epsik, paraaf, alpha, alphan, alphad
    real(kind=8) :: rmin, temps(6), rbid, anormd, resmin, gainre, valr(2)
    character(len=8) :: nomsd, k8bid, k8bid1
    character(len=19) :: matdd, k19bid, crit19
    character(len=24) :: colaux, colaui, colau2, nomggt, infofe, nomgi, k24bid
    character(len=24) :: sdfetg, kfetf, kfval, k24irr, k24lai, k24ir2, k24irg
    character(len=24) :: k24irh, k24irz, k24ir1, k24irp, k24ir3, k24psr, kinf
    character(len=24) :: k24ddr, k24fir, k24rex, k24fiv, k24prj, k24ipi, k24sm1
    character(len=24) :: k24sm2, k24pas, kfvaf, kfnbn, kfacs, kasse, kfacn
    character(len=24) :: kelem, kmpi, kmpib, kv4, kgitr, kfidd, knum, kres
    character(len=24) :: kvide, kcf, k24mul, colau3, chsmdd, prfdee
    logical :: reorth, igsmkp, gs, lumpe, lrigid, lstogi, lpara, lacma, lacsm
    logical :: logbid, lreac, lresta, lresok
    integer(kind=4) :: nbi4
!
! CORPS DU PROGRAMME
    call matfpe(-1)
!
    call jemarq()
!----------------------------------------------------------------------
!----  0. GESTION DE L'OPTION DE CALCUL
!----------------------------------------------------------------------
! NOMS DES OBJETS TEMPORAIRES
    k24lai='&&FETI.LAGR.INTERFACE'
    k24irr='&&FETI.RESIDU.R'
    k24irg='&&FETI.REPROJ.G'
    k24irh='&&FETI.REPCPJ.H'
    k24irp='&&FETI.DDP'
    k24irz='&&FETI.FIDDZ'
    k24ir1='&&FETI.VECNBI.AUX1'
    k24ir2='&&FETI.VECNBI.AUX2'
    k24ir3='&&FETI.VECNBI.AUX3'
    colaux='&&FETI.COLLECTIONR'
    colaui='&&FETI.COLLECTIONI'
    colau2='&&FETI.COLLECTIONL'
    k24rex='&&FETI.FETREX.AUX'
    k24fiv='&&FETI.FETFIV.AUX'
    k24ipi='&&FETI.LAPACK.IPIV'
    nomggt='&&FETI.GITGI.R'
    nomgi='&&FETI.GI.R'
    k24prj='&&FETI.FETPRJ.AUX'
    k24sm1='&FETI.MULTIPLE.SM.K24'
    k24sm2='&FETI.MULTIPLE.SM.IN'
    k24pas='&FETI.PAS.TEMPS'
    kfidd='&FETI.INFO.STOCKAGE.FIDD'
    kfval='&FETI.INFO.STOCKAGE.FVAL'
    kfvaf='&FETI.INFO.STOCKAGE.FVAF'
    kfnbn='&FETI.INFO.STOCKAGE.FNBN'
    kfacs='&FETI.INFO.CPU.FACS'
    kasse='&FETI.INFO.CPU.ASSE'
    kfacn='&FETI.INFO.CPU.FACN'
    kelem='&FETI.INFO.CPU.ELEM'
    kinf='&FETI.FINF'
    kmpi='&FETI.LISTE.SD.MPI'
    kmpib='&FETI.LISTE.SD.MPIB'
    kv4='&&FETI.GGT.V4'
    kgitr='&&FETPRJ.GITVI.R'
    knum='&FETI.MAILLE.NUMSD'
    kres='&FETI.ITERATION.RESIDU'
    kvide='VIDE'
    kcf='&&NUMERO.FETI.CONTACT'
    k24mul='&&FETI.MULT'
    colau3='&&FETI.DVALG'
    if (opt(1:12) .eq. 'NETTOYAGE_SD') then
! ON DETRUIT TOUTES LES SD TEMPORAIRES LIEES AUX RESOLUTIONS PASSEES
! SI ELLES EXISTENT
        goto 500
    else if (opt(1:10).eq.'RESOLUTION') then
! ON FAIT LA RESOLUTION STANDARD
    else
        call u2mess('F', 'ALGELINE_1')
    endif
!----------------------------------------------------------------------
!----  1. PREPARATION DES DONNEES
!----------------------------------------------------------------------
!----  1.1 DONNEES GENERALES
!
    call infniv(ifm, niv)
    call jeveuo(kinf, 'L', iinf)
    infofe=zk24(iinf)
    crit19=criter
    call jeexin(crit19//'.CRTI', iret)
    call jeexin(crit19//'.CRTR', iret1)
    call jeexin(crit19//'.CRDE', iret2)
    iret=iret*iret1*iret2
    if (iret .eq. 0) then
        if (crit19 .ne. ' ') then
            call wkvect(crit19//'.CRTI', 'V V I', 1, jcri)
            call wkvect(crit19//'.CRTR', 'V V R8', 1, jcrr)
            call wkvect(crit19//'.CRDE', 'V V K16', 2, jcrk)
            zk16(jcrk)='ITER_GCPC'
            zk16(jcrk+1)='RESI_GCPC'
        else
            jcri=0
        endif
    else
        call jeveuo(crit19//'.CRTI', 'E', jcri)
        call jeveuo(crit19//'.CRTR', 'E', jcrr)
    endif
!
!  PLUS PETITE VALEUR REELLE DISCERNABLE
    rmin=r8miem()**(2.0d+0/3.0d+0)
!  PARAMETRE D'AFFICHAGE DE LA DECROISSANCE DU RESIDU
    paraaf = 0.1d0
! DEF UNITE LOGIQUE DE SORTIE (ITER,RESIDU) SI INFOFE(13:13)='T'
    ifm17=17
! POUR REACTUALISATION DU RESIDU (POUR L'INSTANT ON DEBRANCHE)
    lresok=.false.
! NBRE DE POINTS OBSERVES EN ARRIERE
    nmoyre=3
! NBRE DE REPETITIONS ACCEPTEES
    ninfma=3
    gainre=5.d0
! VALEUR DE L'ITERATION COURANTE
    iterm=0
    lresta=.false.
    ninfre=0
!
!----  1.2 DONNEES POUR PARALLELISME
!
    call jeveuo(kmpi, 'L', ilimpi)
    if (infofe(10:10) .eq. 'T') then
        nivmpi=2
    else
        nivmpi=1
    endif
    call fetmpi(2, ibid, ifm, nivmpi, rang,&
                ibid, k24bid, k24bid, k24bid, rbid)
    call fetmpi(3, ibid, ifm, nivmpi, ibid,&
                nbproc, k24bid, k24bid, k24bid, rbid)
    if (nbproc .eq. 1) then
        lpara=.false.
    else
        lpara=.true.
    endif
! INITIALISATION DE UTTCPU POUR PROFILING
    call fetcpu(1, temps, infofe, rang, ifm)
!
!----   1.3 DONNEES LIEES A LA DECOMPOSITION EN SOUS-DOMAINES
!
    call jeveuo(sdfeti(1:19)//'.FDIM', 'L', idime)
!  NOMBRE DE SOUS-DOMAINES
    nbsd=zi(idime)
    nbsd1=nbsd+1
!  NOMBRE DE LAGRANGE D'INTERFACE
    nbi2=zi(idime+1)
!  NOMBRE DE DDLS D'INTERFACE
    nbi=zi(idime+3)
    nbi4=nbi
!  NOMBRE DE DDLS TOTAL
    nbtot=zi(idime+4)
!
!  TRAITEMENT DU NOMBRE D'ITERATION SI NULLE
    if (niter .eq. 0) then
        niter = max(nbi/100,10)
        if ((rang.eq.0) .and. ((infofe(9:9).eq.'T').or. (infofe(11:11) .eq.'T'))) write(ifm,1060&
                                                                                  )niter
    endif
    niter1=niter-1
    call wkvect(kres, 'V V R', niter, iiter)
!  VECTEUR DE LA LISTE DES NOMBRES DE DDLS PAR SOUS-DOMAINE
    call jeveuo(sdfeti(1:19)//'.FETH', 'L', ifeth)
!
!  PRECONDITIONNEMENT LUMPE OU NON ?
    if (preco(1:5) .eq. 'LUMPE') then
        lumpe=.true.
    else
        lumpe=.false.
    endif
!
! EVENTUELLE ECRITURE DANS FICHIER SI FETI ET INFO_FETI(14:14)='T'
    if (infofe(14:14) .eq. 'T') then
        call jeveuo(chsecm//'.FETC', 'L', ifetc)
        do 8 idd = 1, nbsd
            if (zi(ilimpi+idd) .eq. 1) then
                chsmdd=zk24(ifetc+idd-1)(1:19)
                call jeveuo(chsmdd(1:19)//'.VALE', 'L', iadval)
                call jeveuo(chsmdd(1:19)//'.REFE', 'L', irefe)
                prfdee=zk24(irefe+1)(1:19)//'.DEEQ'
                call jeveuo(prfdee, 'L', ideeq)
                call jelira(prfdee, 'LONMAX', nbddl, k8bid)
                nbddl=nbddl/2
                call fettsd(infofe, idd, nbddl, ibid, sdfeti(1:19),&
                            k24bid, ideeq, iadval, ibid, ibid,&
                            logbid, ibid, ibid, ibid, k19bid,&
                            7, logbid)
            endif
 8      continue
    endif
!----  1.4 DONNEES LIEES A LA REORTHOGONALISATION DU GCPPC
!
    if (tyreor(1:4) .ne. 'SANS') then
        reorth=.true.
!  INTER-VALIDATION DES DONNEES
        if (nbreor .eq. 0) then
            nbreor = max(niter/10,5)
            if ((rang.eq.0) .and. ((infofe(9:9).eq.'T').or. (infofe(11: 11).eq.'T'))) write(&
                                                                                      ifm,1065&
                                                                                      )nbreor
        endif
        if (nbreor .gt. niter1) then
            nbreor=niter1
            if ((rang.eq.0) .and. ((infofe(9:9).eq.'T').or. (infofe(11: 11).eq.'T'))) write(&
                                                                                      ifm,1064&
                                                                                      )nbreor
        endif
        igsmkp=.false.
        gs=.false.
        if (tyreor .eq. 'IGSM') igsmkp=.true.
        if (tyreor .eq. 'GS') gs=.true.
    else
        reorth=.false.
    endif
!
!----  1.5 DONNEES POUR ACCELERATION ACMA ET ACSM
!
    call jeveuo(k24pas, 'E', ifetpt)
    itps=zi(ifetpt+1)
    assmat=zi(ifetpt+2)
    call jeveuo(k24sm1, 'E', imsmk)
    call jeveuo(k24sm2, 'E', imsmi)
    lacma=.false.
    lacsm=.false.
    dimtet=0
    nbreoa=0
    iacsm=1
    if ((reorth) .and. (nbreoi.gt.0)) then
!
!  ACTIVATION DES ACCELERATIONS
        if ((acma(1:3).eq.'OUI') .and. (assmat.eq.1)) lacma=.true.
        if (acsm(1:3) .eq. 'OUI') lacsm=.true.
!  RECHERCHE DE LA DIMENSION DE L'ESPACE DE REPROJECTION DU PB EN
!  TENANT COMPTE DES PAS DE TEMPS PRECEDENT
        nbreoa=0
        dimtet=0
        if (itps .gt. nbreoi) then
            iacsm=nbreoi+1
        else
            iacsm=itps
        endif
        if ((lacsm) .or. (lacma)) then
! NBREOA: NBRE DE REORTHO EFFECTIF ENTRE PAS DE TEMPS
            nbreoa=min(itps-1,nbreoi)
            do 21 i = 1, nbreoa
                iaux1=zi(imsmi-1+i)
                if (iaux1 .gt. dimtet) dimtet=iaux1
21          continue
        endif
    endif
!
!  GESTION DES OBJETS DE REORTHO LIES AUX PAS DE TEMPS
    if ((reorth) .and. (rang.eq.0)) then
!  NOUVELLE OCCURENCE POUR L'INSTANT EN COURS
        call gcncon('.', k8bid1)
        zk24(imsmk+iacsm-1)=k8bid1
        nbreo1=nbreor+2
        nbreo2=nbreo1*nbi
        k24psr='&&FETI.PS.'//k8bid1
        call wkvect(k24psr, 'V V R', nbreo1, ipsro)
        k24ddr='&&FETI.DD.'//k8bid1
        call wkvect(k24ddr, 'V V R', nbreo2, iddro)
        k24fir='&&FETI.FIDD.'//k8bid1
        call wkvect(k24fir, 'V V R', nbreo2, iddfro)
        call jelibe(k24ddr)
        call jelibe(k24fir)
    endif
!
!----  1.6 COLLECTIONS TEMPORAIRES DE VECTEURS DE TAILLES VARIABLES
!
!  REMPLISSAGE QU'AU PREMIER PAS DE TEMPS
    sdfetg=sdfeti//'.FETG'
    call jeexin(colau3, iret)
    if (iret .eq. 0) then
! OBJET POUR FAIRE LA JOINTURE DDL LOCAUX/DDL GLOBAUX DANS FETRIN
        call jecrec(colau3, 'V V I', 'NO', 'DISPERSE', 'VARIABLE',&
                    nbsd)
! OBJET POUR (KI)+*U
        call jecrec(colaux, 'V V R', 'NO', 'DISPERSE', 'VARIABLE',&
                    nbsd)
        if (lumpe) call jecrec(colau2, 'V V R', 'NO', 'DISPERSE', 'VARIABLE',&
                               nbsd)
        do 10 idd = 1, nbsd
            if (zi(ilimpi+idd) .eq. 1) then
                call jenuno(jexnum(sdfetg, idd), nomsd)
                call jecroc(jexnom(colau3, nomsd))
                call jecroc(jexnom(colaux, nomsd))
                nb=zi(ifeth+idd-1)
                call jeecra(jexnom(colaux, nomsd), 'LONMAX', nb, k8bid)
! ATTENTION DECALAGE DE 1, LE PREMIER ENTIER VAUT  SI L'OBJET A DEJA
! ETE REMPLI PAR FETRIN
                call jeecra(jexnom(colau3, nomsd), 'LONMAX', nb+1, k8bid)
                if (lumpe) then
                    call jecroc(jexnom(colau2, nomsd))
                    call jeecra(jexnom(colau2, nomsd), 'LONMAX', nb, k8bid)
                endif
            endif
10      continue
! REMPLISSAGE COLAUI
        call feting(nbsd, sdfeti, chsecm, colaui, infofe,&
                    ifm, ilimpi, rang)
    endif
!
!----  1.7 VECTEURS TEMPORAIRES D'INTERFACE
!
!  CREATION QU'AU PREMIER PAS DE TEMPS
    call jeexin(k24lai, iret)
    if (iret .eq. 0) then
        call wkvect(k24lai, 'V V R', nbi, ivlagi)
        call wkvect(k24irr, 'V V R', nbi, irr)
        call wkvect(k24irg, 'V V R', nbi, irg)
        call wkvect(k24irh, 'V V R', nbi, irh)
        call wkvect(k24irp, 'V V R', nbi, irp)
        call wkvect(k24irz, 'V V R', nbi, irz)
        call wkvect(k24ir1, 'V V R', nbi, ir1)
        call wkvect(k24ir2, 'V V R', nbi, ir2)
        call wkvect(k24ir3, 'V V R', nbi, ir3)
    else
        call jeveuo(k24lai, 'E', ivlagi)
        call jerazo(k24lai, nbi, 1)
        call jeveuo(k24irr, 'E', irr)
        call jerazo(k24irr, nbi, 1)
        call jeveuo(k24irg, 'E', irg)
        call jerazo(k24irg, nbi, 1)
        call jeveuo(k24irh, 'E', irh)
        call jerazo(k24irh, nbi, 1)
        call jeveuo(k24irp, 'E', irp)
        call jerazo(k24irp, nbi, 1)
        call jeveuo(k24irz, 'E', irz)
        call jerazo(k24irz, nbi, 1)
        call jeveuo(k24ir1, 'E', ir1)
        call jerazo(k24ir1, nbi, 1)
        call jeveuo(k24ir2, 'E', ir2)
        call jerazo(k24ir2, nbi, 1)
        call jeveuo(k24ir3, 'E', ir3)
        call jerazo(k24ir3, nbi, 1)
    endif
!
!----  1.8 PREPARATION D'OBJETS ANNEXES AFIN D'ECONOMISER DES JEVEUO...
!
!  POUR FETREX
    call jeveuo(sdfeti//'.FETI', 'L', ifeti)
    i=1+3*nbsd
    call wkvect(k24rex, 'V V I', i, irex)
    zi(irex)=ifeti
    do 11 idd = 1, nbsd
        if (zi(ilimpi+idd) .eq. 1) then
            iaux3=irex+3*(idd-1)+1
            call jenuno(jexnum(sdfetg, idd), nomsd)
            call jeveuo(jexnom(sdfetg, nomsd), 'L', zi(iaux3))
            iaux3=iaux3+1
            call jelira(jexnom(sdfetg, nomsd), 'LONMAX', long, k8bid)
            if (mod(long,2) .ne. 0) then
                call u2mesk('F', 'ALGELINE_2', 1, sdfeti)
            else
                zi(iaux3)=(long/2)-1
                iaux3=iaux3+1
            endif
            call jeveuo(jexnom(colaui, nomsd), 'L', zi(iaux3))
        endif
11  end do
!
!  POUR FETFIV
    i=2+5*nbsd
    call wkvect(k24fiv, 'V V I', i, ifiv)
    zi(ifiv)=ifm
    call jeveuo(matas//'.FETM', 'L', iaux1)
    zi(ifiv+1)=iaux1
    do 12 idd = 1, nbsd
        if (zi(ilimpi+idd) .eq. 1) then
            iaux3=ifiv+2+5*(idd-1)
            matdd=zk24(iaux1+idd-1)(1:19)
            call jeveuo(matdd//'.&INT', 'L', zi(iaux3))
            iaux3=iaux3+1
            call jeexin(matdd//'.CONL', iret)
            zi(iaux3)=iret
            iaux3=iaux3+1
            if (iret .ne. 0) then
                call jeveuo(matdd//'.CONL', 'L', iaux2)
            else
                iaux2=1
            endif
            zi(iaux3)=iaux2
            iaux3=iaux3+1
            call jenuno(jexnum(sdfetg, idd), nomsd)
            call jeveuo(jexnom(colaux, nomsd), 'E', zi(iaux3))
            iaux3=iaux3+1
            if (lumpe) call jeveuo(jexnom(colau2, nomsd), 'E', zi(iaux3))
            iaux3=iaux3+1
        endif
12  end do
!
!----  1.9 TEST DE COHERENCE DE LA SD_FETI
    call fettsd(infofe, nbi, nbsd, zi(ifeth), sdfeti,&
                colaux, irex, nbi2, ifeti, ifm,&
                lpara, itps, nivmpi, rang, chsol,&
                1, logbid)
!
!----------------------------------------------------------------------
!----  2. INITIALISATION PROPREMENT DITE DE L'ALGORITHME FETI
!----------------------------------------------------------------------
    call fetcpu(2, temps, infofe, rang, ifm)
!
!----  2.1 SYNCHRONISATION DE VECTEURS DE MONITORING POUR
!----      LE PARALLELISME
!
!  VECTEUR INDIQUANT SI UN SOUS-DOMAINE EST FLOTTANT ET SON NOMBRE DE
!  MODES DE CORPS RIGIDES
!  EN SEQUENTIEL ON PREND L'ADRESSE DE MATAS.FETF
!  EN PARALLELE ON SOMME CETTE DONNEE LOCALE ET ON LA TRANSFERRE A TOUS
!  PROCESSEUR PAR UN MPI_ALLREDUCE + MPI_SUM (AU PREMIER PAS DE TEMPS)
    kfetf=matas//'.FETF'
    if ((lpara) .and. (itps.eq.1)) call fetmpi(6, nbsd, ifm, nivmpi, ibid,&
                                               ibid, kfetf, k24bid, k24bid, rbid)
    call jeveuo(kfetf, 'L', ifetf)
!
!  VECTEUR INDIQUANT LE NBRE DE COEFF DES MATRICES LOCALES
!  EN SEQUENTIEL ON PREND L'ADRESSE DE '&FETI.INFO.STOCKAGE.FVAL'
!  EN PARALLELE ON TRANSFERE CETTE DONNEE LOCALE A CHAQUE PROCESSEUR PAR
!  UN MPI_ALLREDUCE + MPI_SUM (AU PREMIER PAS DE TEMPS)
    if ((lpara) .and. (itps.eq.1)) call fetmpi(6, nbsd1, ifm, nivmpi, ibid,&
                                               ibid, kfval, k24bid, k24bid, rbid)
    call jeveuo(kfval, 'E', ifet1)
    mamoy=zi(ifet1+nbsd)/nbsd
!
!  TANT QU'A SYNCHRONISER ON FAIT LE MEME TRAVAIL POUR LES AUTRES OBJETS
!  PERIPHERIQUES (SAUF POUR FVAF/FNBN ET FACS QU'AU PREMIER PAS)
    if (lpara) then
        if (itps .eq. 1) then
            call fetmpi(4, nbsd1, ifm, nivmpi, ibid,&
                        ibid, kfvaf, k24bid, k24bid, rbid)
            call fetmpi(4, nbsd1, ifm, nivmpi, ibid,&
                        ibid, kfnbn, k24bid, k24bid, rbid)
            call fetmpi(5, nbsd1, ifm, nivmpi, ibid,&
                        ibid, kfacs, k24bid, k24bid, rbid)
        endif
        call fetmpi(5, nbsd1, ifm, nivmpi, ibid,&
                    ibid, kasse, k24bid, k24bid, rbid)
        call fetmpi(5, nbsd1, ifm, nivmpi, ibid,&
                    ibid, kfacn, k24bid, k24bid, rbid)
        call fetmpi(5, nbproc, ifm, nivmpi, ibid,&
                    ibid, kelem, k24bid, k24bid, rbid)
    endif
!
!----  2.2 CALCUL DE GI, (GI)T*GI ET DIMGI (AU PREMIER PAS DE TEMPS)
!
!  NOMS DES OBJETS JEVEUX STOCKANT GI ET (GI)T*GI SI LRIGID=.TRUE.
    call fetggt(nbsd, matas, zi(ifetf), zi(ifeth), lrigid,&
                nbi, nomggt, dimgi, nomgi, stogi,&
                lstogi, mamoy, infofe, irex, ifm,&
                sdfeti, nbproc, rang, itps)
    call fetcpu(3, temps, infofe, rang, ifm)
!
!----  2.3 PREPARATION D'OBJETS ANNEXES AFIN D'ECONOMISER DES JEVEUO...
!
!  POUR FETPRJ
    call wkvect(k24prj, 'V V I', 3, iprj)
    zi(iprj)=ifm
    if (lrigid) then
        call wkvect(kgitr, 'V V R', dimgi, zi(iprj+1))
        call wkvect(kv4, 'V V R', nbi, zi(iprj+2))
    endif
!
!----  2.4 MONITORING SI INFOFE(11:11)='T'
!
    call fetmon(infofe, nbi2, nbi, nbtot, nbsd,&
                dimgi, ifm, mamoy, lstogi, ifet1,&
                rang, itps, lpara, 1)
!
!----  2.5 CALCUL DU VECTEUR LAGRANGE_FETI INITIAL LANDA0 (ZR(IVLAGI))
!
    if ((lrigid) .and. (rang.eq.0)) then
        if (itps .eq. 1) then
            call wkvect(k24ipi, 'V V S', dimgi, ipiv)
        else
            call jeveuo(k24ipi, 'E', ipiv)
        endif
    endif
    call fetinl(nbi, zr(ivlagi), matas, chsecm, lrigid,&
                dimgi, nbsd, zi(ifetf), zi(ifeth), nomggt,&
                ipiv, nomgi, lstogi, infofe, irex,&
                ifm, sdfeti, nbproc, rang, k24lai,&
                itps)
!
!----  2.6 CALCUL DU RESIDU INITIAL (ZR(IRR)): R0 = D - FI*LANDA0
!----      ET DU SECOND MEMBRE (ZR(IR1)): D DE NORME ANORMD
! LABEL POUR REDEMARRAGE
14  continue
    call fetrin(nbsd, nbi, zr(irr), zr(ir1), matas,&
                zi(ifetf), zi(ifeth), colaux, chsecm, sdfeti,&
                zr(ivlagi), 1, chsol, testco, lrigid,&
                dimgi, irr, nomggt, ipiv, nomgi,&
                lstogi, infofe, irex, iprj, ifm,&
                ifiv, nbproc, rang, k24irr)
! ON NE CALCULE LA NORME DU SECOND MEMBRE QUE SI ON NE REDEMARRE PAS
    if (.not.lresta) then
        call jerazo(k24ir3, nbi, 1)
        call fetrin(nbsd, nbi, zr(ir1), zr(ir2), matas,&
                    zi(ifetf), zi(ifeth), colaux, chsecm, sdfeti,&
                    zr(ir3), 1, chsol, testco, lrigid,&
                    dimgi, irr, nomggt, ipiv, nomgi,&
                    lstogi, infofe, irex, iprj, ifm,&
                    ifiv, nbproc, rang, k24ir1)
        if (rang .eq. 0) anormd=dnrm2(nbi,zr(ir1),1)
!
! EN PARALLELE, DIFFUSION DE LA NORME DU SM INITIAL A TOUS LES PROC POUR
!  LACONSTRUCTION DU PREMIER TEST D'ARRET
        if (lpara) call fetmpi(10, ibid, ifm, nivmpi, rang,&
                               ibid, k24bid, k24bid, k24bid, anormd)
    endif
!
!----  2.7 CALCUL DU RESIDU PROJETE INITIAL (ZR(IRG)): G0=P*R0
!----      AVEC OU SANS ACCELERATION SECOND MEMBRE
!
!----  2.7.1 SANS ACCELERATION SECOND MEMBRE
!
    if ((itps.eq.1) .or. (.not.lacsm)) then
!  EN PARALLELE ON DIFFUSE G0
        call fetprj(nbi, zr(irr), zr(irg), nomggt, lrigid,&
                    dimgi, 1, sdfeti, ipiv, nbsd,&
                    zi(ifetf), zi(ifeth), matas, nomgi, lstogi,&
                    infofe, irex, iprj, nbproc, rang,&
                    k24irg)
        anorm=dnrm2(nbi,zr(irg),1)
! STOCKAGE RESIDU_INIT
        zr(iiter+iterm)=anorm
    else
!
!----  2.7.2 AVEC ACCELERATION SECOND MEMBRE
!
!  RECALCUL DU LAMBDA0 POUR TENIR COMPTE INFO PAS DE TEMPS PRECEDENTS
!  ON NE DIFFUSE PAS CAR LE PROC MAITRE VA RETRAVAILLER SUR CETTE
!  DONNES
        call fetprj(nbi, zr(irr), zr(irg), nomggt, lrigid,&
                    dimgi, 1, sdfeti, ipiv, nbsd,&
                    zi(ifetf), zi(ifeth), matas, nomgi, lstogi,&
                    infofe, irex, iprj, nbproc, rang,&
                    kvide)
        if (rang .eq. 0) anorm=dnrm2(nbi,zr(irg),1)
! MISE A JOUR PAR PROC 0 DE LAMBDA0, R0 ET G0
        call fetacc(1, rang, dimtet, imsmi, imsmk,&
                    nbreoa, itps, irg, irr, ivlagi,&
                    nbi, ir1, ir2, ir3, nomggt,&
                    lrigid, dimgi, sdfeti, ipiv, nbsd,&
                    zi(ifetf), zi(ifeth), matas, nomgi, lstogi,&
                    infofe, irex, iprj, nbproc)
!  ON DIFFUSE GO ET LAMBDA0 A TOUS LES PROCESSEURS POUR CALCUL SUIVANT
! EN PARALLELE, DIFFUSION DE LA NORME DU RESIDU INITIAL NON CORRIGE
! A TOUS LES PROC POUR LA CONSTRUCTION DU PREMIER TEST D'ARRET
        if (lpara) then
            call fetmpi(9, nbi, ifm, nivmpi, ibid,&
                        ibid, k24irg, k24bid, k24bid, rbid)
            call fetmpi(9, nbi, ifm, nivmpi, ibid,&
                        ibid, k24lai, k24bid, k24bid, rbid)
            call fetmpi(10, ibid, ifm, nivmpi, rang,&
                        ibid, k24bid, k24bid, k24bid, anorm)
        endif
    endif
!----  2.8 CALCUL DE LA NORME DU RESIDU PROJETE POUR CRITERES D'ARRET
!
    if (.not.lresta) then
        resmin=anormd*epsi
        epsik=epsi*anorm
    endif
    if (((anorm.gt.resmin).and.(anormd.gt.rmin)) .or. (lresta)) then
        if ((rang.eq.0) .and. ((infofe(9:9).eq.'T').or. (infofe(11:11) .eq.'T'))) then
            write(ifm,*)'******************************************'
            write (ifm,1020)anormd,anorm,epsik,epsi
            write(ifm,*)'******************************************'
        endif
        iterm=iterm+1
    else
        if ((rang.eq.0) .and. ((infofe(9:9).eq.'T').or. (infofe(11:11) .eq.'T'))) then
            write(ifm,*)'******************************************'
            write (ifm,1021)anormd,anorm
            if (resmin .gt. rmin) then
                write(ifm,*)'RESIDU INITIAL <= A : ',resmin
            else
                write(ifm,*)'SECOND MEMBRE QUASI-NUL   : ',resmin
            endif
            write(ifm,*)'NUL BESOIN DE CALCULER DE NOUVELLE SOLUTION'
            write(ifm,*)'        LAMBDA_SOL = LAMBDA_INIT'
            write(ifm,*)'******************************************'
            write(ifm,1040)anorm,anorm,1.d0
            write(ifm,1050)iterm
        endif
        if (jcri .ne. 0) then
            zi(jcri)=1
            zr(jcrr)=anorm
        endif
!  CALCUL SOLUTION U GLOBALE
!  UI_SOL = (KI)+ * (FI - RIT*LANDA_SOL) - BI*ALPHAI_SOL
        call fetrin(nbsd, nbi, zr(irr), zr(ir2), matas,&
                    zi(ifetf), zi( ifeth), colaux, chsecm, sdfeti,&
                    zr(ivlagi), 2, chsol, testco, lrigid,&
                    dimgi, irr, nomggt, ipiv, nomgi,&
                    lstogi, infofe, irex, iprj, ifm,&
                    ifiv, nbproc, rang, k24irr)
        if (rang .eq. 0) zi(imsmi-1+iacsm)=0
        goto 200
    endif
!  CONSTANTES POUR AFFICHAGE (ANORMK) ET NORME INITIALE (ANORM0)
    if (rang .eq. 0) anormk=anorm*paraaf
    if (.not.lresta) anorm0=anorm
!
!----  2.9 CALCUL DU RESIDU PRECOND PROJETE P INITIAL
!----      (ZR(IRH)): H0=P*A*M-1*A*G0
!
    call fetsca(nbi, zr(irg), zr(ir1), scalin, infofe,&
                nbi2, ifeti, ifm)
    call fetprc(nbsd, nbi, zr(ir1), zr(ir3), zr(ir2),&
                matas, zi(ifeth), preco, infofe, irex,&
                ifiv, nbproc, rang, k24ir2)
!
!----  2.9.1 ACCELERATION_MA
!   MISE A JOUR PAR PROC 0 DE H0_TILDE=M-1*A*G0
    if (lacma) call fetacc(2, rang, dimtet, imsmi, imsmk,&
                           nbreoa, itps, irg, irr, ivlagi,&
                           nbi, ir1, ir2, ir3, nomggt,&
                           lrigid, dimgi, sdfeti, ipiv, nbsd,&
                           zi(ifetf), zi(ifeth), matas, nomgi, lstogi,&
                           infofe, irex, iprj, nbproc)
    if (rang .eq. 0) call fetsca(nbi, zr(ir2), zr(ir3), scalin, infofe,&
                                 nbi2, ifeti, ifm)
    call fetprj(nbi, zr(ir3), zr(irh), nomggt, lrigid,&
                dimgi, 1, sdfeti, ipiv, nbsd,&
                zi(ifetf), zi(ifeth), matas, nomgi, lstogi,&
                infofe, irex, iprj, nbproc, rang,&
                k24irh)
!
!----  2.10 CALCUL DE LA DD INITIALE (ZR(IRP)): P0=H0 (=G0 SI NON PREC)
!  EVENTUELLEMENT REORTHOGONALISEE PAR RAPPORT AUX PAS DE TEMPS
!  PRECEDENT + CALCUL DE ALPHAN0 = G0.P0 (=G0.G0 SI NON PRECOND)
    if ((itps.eq.1) .or. (.not.lacsm)) then
        call dcopy(nbi4, zr(irh), 1, zr(irp), 1)
        if (rang .eq. 0) alphan=ddot(nbi,zr(irg),1,zr(irp),1)
    else
        call fetreo(reorth, alphan, nbi, irg, 0,&
                    nbreor, irp, k24fir, k24ddr, k24psr,&
                    gs, igsmkp, rmin, irh, infofe,&
                    ifm, nbproc, rang, k24irp, itps,&
                    nbreoi, 1, lacsm)
    endif
!
!----  2.11 TEST DEFINIE-POSITIVITE DE P*FI*P ET ORTHO GCPPC
!
    call fetarp(infofe, ifm, niter, nbi, nbreor,&
                lrigid, dimgi, sdfeti, ipiv, nomggt,&
                nbsd, ifetf, ifeth, nomgi, lstogi,&
                irex, iprj, ir2, ifiv, matas,&
                nbproc, rang)
    call fetcpu(4, temps, infofe, rang, ifm)
!
!
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!----  3  BOUCLES DU GCPPC DE FETI
!----------------------------------------------------------------------
!----------------------------------------------------------------------
    iteri=iterm
    do 100 iter = iteri, niter1
!
! STOCKAGE PK
        if ((reorth) .and. (rang.eq.0)) then
            if (iter .le. nbreor) then
                iaux1=iter*nbi
            else
                iaux1=0
            endif
            call jeveuo(k24ddr, 'E', iddro)
            call dcopy(nbi4, zr(irp), 1, zr(iddro+iaux1), 1)
            call jelibe(k24ddr)
        endif
!
!---------------------------------------------------
!----  3.1 CALCUL DU PRODUIT FI * PK = ZK
!---------------------------------------------------
!
        call fetcpu(5, temps, infofe, rang, ifm)
        call fetfiv(nbsd, nbi, zr(irp), zr(ir2), zr(irz),&
                    matas, zi(ifetf), zi(ifeth), infofe, irex,&
                    ifiv, nbproc, rang, k24irz, sdfeti)
        call fetcpu(6, temps, infofe, rang, ifm)
!  STOCKAGE FI*PK
        if ((reorth) .and. (rang.eq.0)) then
            call jeveuo(k24fir, 'E', iddfro)
            call dcopy(nbi4, zr(irz), 1, zr(iddfro+iaux1), 1)
            call jelibe(k24fir)
        endif
!
!---------------------------------------------------
!----  3.2  CALCUL DE ALPHAK = GK.PK/ZK.PK = ALPHANK/ALPHADK
!---------------------------------------------------
!
        if (rang .eq. 0) then
            alphad=ddot(nbi,zr(irp),1,zr(irz),1)
            if (abs(alphad) .lt. rmin) then
                alphad=rmin
                call u2mess('A', 'ALGELINE_3')
            endif
            alpha=alphan/alphad
!  STOCKAGE ZK.PK SI REORTHO
            if (reorth) then
                if (iter .le. nbreor) then
                    iaux1=iter
                else
                    iaux1=0
                endif
                zr(ipsro+iaux1)=alphad
            endif
        endif
!
!---------------------------------------------------
!----  3.3  CALCUL NOUVEAUX VECTEUR D'INTERFACE ET RESIDU
!----      (ZR(IVLAGI)) LANDAK+1 = LANDAK + ALPHAK * PK
!----      (ZR(IRR)))   GK+1     = GK     - ALPHAK * PZK
!----  3.4  CALCUL DE LA PROJECTION 1 (ZR(IRG)): GK+1 = P * RK+1
!---------------------------------------------------
!
        call daxpy(nbi4, alpha, zr(irp), 1, zr(ivlagi),&
                   1)
        if (reacre .eq. 0) then
            lreac=.false.
        else
            lreac=(mod(iter,reacre).eq.0)
        endif
        if (lreac) then
! CALCUL DE GK+1 VIA LE RESIDU RECALCULE PROJETE
            if (lpara) call fetmpi(9, nbi, ifm, nivmpi, ibid,&
                                   ibid, k24lai, k24bid, k24bid, rbid)
            call fetrin(nbsd, nbi, zr(irr), zr(ir1), matas,&
                        zi(ifetf), zi(ifeth), colaux, chsecm, sdfeti,&
                        zr(ivlagi), 1, chsol, testco, lrigid,&
                        dimgi, irr, nomggt, ipiv, nomgi,&
                        lstogi, infofe, irex, iprj, ifm,&
                        ifiv, nbproc, rang, k24irr)
            call fetprj(nbi, zr(irr), zr(irg), nomggt, lrigid,&
                        dimgi, 1, sdfeti, ipiv, nbsd,&
                        zi(ifetf), zi(ifeth), matas, nomgi, lstogi,&
                        infofe, irex, iprj, nbproc, rang,&
                        k24irg)
            irg1=irg
        else
!
!  CALCUL DE GK+1 VIA RECURRENCE ET PROJECTION SANS DIFFUSION
            call fetprj(nbi, zr(irz), zr(ir1), nomggt, lrigid,&
                        dimgi, 1, sdfeti, ipiv, nbsd,&
                        zi(ifetf), zi(ifeth), matas, nomgi, lstogi,&
                        infofe, irex, iprj, nbproc, rang,&
                        kvide)
            call daxpy(nbi4, -alpha, zr(ir1), 1, zr(irg),&
                       1)
            irg1=ir1
!  MAINTENANT ON DIFFUSE ZR(IRG)
            if (lpara) call fetmpi(9, nbi, ifm, nivmpi, ibid,&
                                   ibid, k24irg, k24bid, k24bid, rbid)
        endif
!
        call fettor(1, infofe, rang, nbi, irg1,&
                    irg, irp, irz, ifm)
        call fetcpu(7, temps, infofe, rang, ifm)
!
!---------------------------------------------------
!----  3.5  CALCUL TEST D'ARRET ET AFFICHAGE
!---------------------------------------------------
!
        anorm=dnrm2(nbi,zr(irg),1)
!
        if ((rang.eq.0) .and. (anorm.ge.epsik)) then
            if (anorm .le. anormk) then
                if ((infofe(9:9).eq.'T') .or. (infofe(11:11).eq.'T')) then
                    write(ifm,*)&
                    '******************************************'
                    if (lreac) then
                        write(ifm,1041)iter,anorm,anorm/anorm0
                    else
                        write(ifm,1042)iter,anorm,anorm/anorm0
                    endif
                    write(ifm,*)&
                    '******************************************'
                endif
                anormk=anorm*paraaf
            else
                if (infofe(9:9) .eq. 'T') then
                    write(ifm,*)&
                    '******************************************'
                    if (lreac) then
                        write(ifm,1041)iter,anorm,anorm/anorm0
                    else
                        write(ifm,1042)iter,anorm,anorm/anorm0
                    endif
                    write(ifm,*)&
                    '******************************************'
                endif
            endif
        endif
!
! REDEMARRAGE OR NOT ? ON COMMENCE PAR REGARDER SI LE NUMERO D'ITERA
! TION LOCAL (DEPUIS LE DERNIER RESTART) EST > A NMOYRE
        lresta=.false.
        if (((iter-iteri+1).ge.nmoyre) .and. lresok) then
            rbid=0.d0
            do 90 i = 1, nmoyre
                rbid=rbid+zr(iiter+iter-i)
90          continue
            rbid=rbid/nmoyre
            if (abs(100.d0*(anorm-rbid)/rbid) .lt. gainre) ninfre= ninfre+1
            if (ninfre .ge. ninfma) lresta=.true.
        endif
        if (lresta) then
            if ((rang.eq.0) .and. ((infofe(9:9).eq.'T').or. (infofe(11: 11).eq.'T'))) then
                write(ifm,*)&
                '******************************************'
                write(ifm,1043)gainre,ninfma
                write(ifm,*)&
                '******************************************'
            endif
            if (lpara) call fetmpi(9, nbi, ifm, nivmpi, ibid,&
                                   ibid, k24lai, k24bid, k24bid, rbid)
            ninfre=0
            call jerazo(k24irr, nbi, 1)
            call jerazo(k24irg, nbi, 1)
            call jerazo(k24irh, nbi, 1)
            call jerazo(k24irp, nbi, 1)
            call jerazo(k24irz, nbi, 1)
            call jerazo(k24ir1, nbi, 1)
            call jerazo(k24ir2, nbi, 1)
! ON REPART A ZERO CONCERNANT LES REORTHOGONALISATIONS AU SEIN D'UN
! PAS DE TEMPS
            if (rang .eq. 0) then
                call jerazo(k24psr, nbreo1, 1)
                call jerazo(k24ddr, nbreo2, 1)
                call jerazo(k24fir, nbreo2, 1)
            endif
            call fetcpu(9, temps, infofe, rang, ifm)
            call fetcpu(31, temps, infofe, rang, ifm)
            goto 14
        endif
!
!-----------------------------
!----  3.6 TEST DE CONVERGENCE
!-----------------------------
! STOCKAGE RESIDU SI ON LE GARDE IE SI ON NE REDEMARRE PAS
        zr(iiter+iterm)=anorm
        if (anorm .lt. epsik) then
            if ((rang.eq.0) .and. ((infofe(9:9).eq.'T').or. (infofe(11: 11).eq.'T'))) then
                write(ifm,1040)anorm0,anorm,anorm/anorm0
                write(ifm,1050)iter
            endif
            if (jcri .ne. 0) then
                zi(jcri)=iter
                zr(jcrr)=anorm
            endif
!
!  CALCUL SOLUTION U GLOBALE
!  ON DIFFUSE ZR(IVLAGI)
            if (lpara) call fetmpi(9, nbi, ifm, nivmpi, ibid,&
                                   ibid, k24lai, k24bid, k24lai, rbid)
!  RECALCUL DU RESIDU AVEC CE NOUVEAU LANDA_SOL (POUR ALPHA)
            if (lrigid) then
                call fetrin(nbsd, nbi, zr(irr), zr(ir1), matas,&
                            zi(ifetf), zi(ifeth), colaux, chsecm, sdfeti,&
                            zr(ivlagi), 1, chsol, testco, lrigid,&
                            dimgi, irr, nomggt, ipiv, nomgi,&
                            lstogi, infofe, irex, iprj, ifm,&
                            ifiv, nbproc, rang, k24irr)
            endif
!  CALCUL DE USOL LOCALE PUIS GLOBAL PROPREMENT DIT
!  UI_SOL = (KI)+ * (FI - RIT*LANDA_SOL) - BI*ALPHAI_SOL
            call fetrin(nbsd, nbi, zr(irr), zr(ir2), matas,&
                        zi(ifetf), zi(ifeth), colaux, chsecm, sdfeti,&
                        zr(ivlagi), 2, chsol, testco, lrigid,&
                        dimgi, irr, nomggt, ipiv, nomgi,&
                        lstogi, infofe, irex, iprj, ifm,&
                        ifiv, nbproc, rang, k24irr)
!  STOCKAGE
            if (rang .eq. 0) zi(imsmi-1+iacsm)=min(iter,nbreor)
            call fetcpu(9, temps, infofe, rang, ifm)
            goto 200
        endif
        call fetcpu(9, temps, infofe, rang, ifm)
!
!---------------------------------------------------
!----  3.7  PRECONDITIONNEMENT (ZR(IRH)): HK+1 =P*A*M-1*A*GK+1
!---------------------------------------------------
        call fetcpu(10, temps, infofe, rang, ifm)
!  PHASE DE SCALING 1 (ZR(IR1)): AUX1 = A * GK+1
        call fetsca(nbi, zr(irg), zr(ir1), scalin, infofe,&
                    nbi2, ifeti, ifm)
!  CALCUL DU RESIDU PRECOND PROJETE P INITIAL (ZR(IR2)): AUX2 = M-1*AUX1
        call fetprc(nbsd, nbi, zr(ir1), zr(ir3), zr(ir2),&
                    matas, zi(ifeth), preco, infofe, irex,&
                    ifiv, nbproc, rang, k24ir2)
!----  3.7.1 ACCELERATION_MA
!   MISE A JOUR PAR PROC 0 DE HK+1_TILDE=M-1*A*GK+1
        if (lacma) call fetacc(2, rang, dimtet, imsmi, imsmk,&
                               nbreoa, itps, irg, irr, ivlagi,&
                               nbi, ir1, ir2, ir3, nomggt,&
                               lrigid, dimgi, sdfeti, ipiv, nbsd,&
                               zi(ifetf), zi(ifeth), matas, nomgi, lstogi,&
                               infofe, irex, iprj, nbproc)
!  PHASE DE SCALING 2 (ZR(IR1)): AUX1 = A * AUX2
        if (rang .eq. 0) call fetsca(nbi, zr(ir2), zr(ir3), scalin, infofe,&
                                     nbi2, ifeti, ifm)
!  CALCUL DE LA PROJECTION 2 (ZR(IRH)): HK+1 = P * AUX1
        call fetprj(nbi, zr(ir3), zr(irh), nomggt, lrigid,&
                    dimgi, 1, sdfeti, ipiv, nbsd,&
                    zi(ifetf), zi(ifeth), matas, nomgi, lstogi,&
                    infofe, irex, iprj, nbproc, rang,&
                    kvide)
        call fetcpu(11, temps, infofe, rang, ifm)
!
!---------------------------------------------------
!----  3.8  NEW DIRECTION DE DESCENTE (ZR(IRP)): PK+1=HK+1 + ...
!----       AVEC EVENTUELLEMENT UNE PHASE DE REORTHONORMALISATION
!----       AU SEIN D'UN MEME PAS DE TEMPS VOIRE ENTRE PAS DE TEMPS
!----       SI ACCELERATION_SM
!---------------------------------------------------
        call fetreo(reorth, alphan, nbi, irg, iter,&
                    nbreor, irp, k24fir, k24ddr, k24psr,&
                    gs, igsmkp, rmin, irh, infofe,&
                    ifm, nbproc, rang, k24irp, itps,&
                    nbreoi, 2, lacsm)
        call fetcpu(12, temps, infofe, rang, ifm)
        call fettor(2, infofe, rang, nbi, irg1,&
                    irg, irp, irz, ifm)
!
!
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!----  FIN BOUCLES DU GCPPC DE FETI
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! MISE A JOUR DE L'ITERATION COURANTE POUR STOCKAGE RESIDU ET DD ORTHO
        iterm=iterm+1
100  end do
!
!----------------------------------------------------------------------
!----  4.  DIVERS
!----------------------------------------------------------------------
!
!----  4.1 NON CONVERGENCE
    if (rang .eq. 0) then
        valr (1) = anorm
        valr (2) = anorm/anorm0
        call u2mesg('F', 'ALGELINE4_8', 0, ' ', 1,&
                    iter, 2, valr)
    endif
!
!----  4.2 FORMAT AFFICHAGE
!
    1020 format(' * FETI: NORME DU SECOND MEMBRE/RESIDU INITIAL =',&
     & d11.4,d11.4/,&
     & ' *       NORME DU RESIDU A ATTEINDRE EN ABS/RELA=',&
     & d11.4,d11.4)
    1021 format(' * FETI: NORME DU SECOND MEMBRE/RESIDU INITIAL =',&
     & d11.4,d11.4)
    1040 format(' * FETI: NORME DU RESIDU INITIAL/FINAL/RELATIF=',&
     &         d11.4,d11.4,d11.4)
    1041 format(' * ITER',i5,' NORME DU RESIDU (RECALCULE) EN ABS/RELA =',&
     &         d11.4,d11.4)
    1042 format(' * ITER',i5,' NORME DU RESIDU (RECURRENCE) EN ABS/RELA =',&
     &         d11.4,d11.4)
    1043 format('! STAGNATION/OSCILLATION DU RESIDU DE MOINS DE ',d11.4,&
     &       ' % PLUS DE ',i2,' FOIS ==> ON REDEMARRE !')
    1050 format(1x,/,2x,32 ('*')/'  * CONVERGENCE EN ',i4,&
     &       ' ITERATION(S)'/2x,32 ('*'),/)
    1060 format('! FETI: NMAX_ITER FIXE A MAX(NB_NOEUD_INTERFACE/10,10)= ',&
     &       i6,' !')
    1064 format('! FETI: NB_REORTHO_DD FIXE A NMAX_ITER= ',i6,' !')
    1065 format('! FETI: NB_REORTHO_DD FIXE A MAX(NMAX_ITER/10,5)= ',&
     &       i6,' !')
! DESTRUCTION DES OBJETS JEVEUX TEMPORAIRES
200  continue
!
!----  4.2.1 PROFILING MPI SI INFOFE(10:10)='T' ET LPARA
!
    call fetmon(infofe, nbi2, nbi, nbtot, nbsd,&
                dimgi, ifm, mamoy, lstogi, ifet1,&
                rang, itps, lpara, 2)
!
!---- 4.2.2 CREATION FICHIER RESIDU
    if ((rang.eq.0) .and. (infofe(13:13).eq.'T')) then
        rbid=zr(iiter)
        do 210 i = 1, iterm-1
            write(ifm17,*)i,zr(iiter+i),zr(iiter+i)/rbid
210      continue
    endif
!
!----  4.3 MENAGE DES DONNEES TEMPORAIRES
!
!----  4.3.1 DONNEES A DETRUIRE A CHAQUE PAS DE TEMPS
!
    call jedetr(k24rex)
    call jedetr(k24fiv)
    call jedetr(k24prj)
    call jedetr(kgitr)
    call jedetr(kv4)
    call jedetr(kres)
!
!----  4.3.2 DONNEES DE TYPE REORTHO
!
! ON EFFACE LES DONNEES DE CE PAS DE TEMPS SI PAS ACCELERATION OU SI
! PAS DE TEMPS SUPERIEUR A NBREOI
    if (rang .eq. 0) then
        if ((.not.lacsm.and..not.lacma) .or. (itps.gt.nbreoi)) then
            nbddsm=zi(imsmi+iacsm-1)
            if (nbddsm .ne. 0) then
                call jedetr(k24psr)
                call jedetr(k24ddr)
                call jedetr(k24fir)
            endif
        endif
    endif
!
!----  4.3.3  DONNEES A DETRUIRE
!
500  continue
    if (opt(1:12) .eq. 'NETTOYAGE_SD') then
! NETTOYAGE PARTIEL SI NECESSAIRE (2IEME PASSAGE DANS PRERES OU FIN
! OPERATEUR)
        call jeveuo(k24pas, 'E', ifetpt)
        itps=zi(ifetpt+1)
        if (itps .ne. 0) then
            call jedetr(k24rex)
            call jedetr(k24fiv)
            call jedetr(k24ipi)
            call jedetr(k24prj)
            call jedetr(nomggt)
            call jedetr(nomgi)
            call jedetc('V', '&&FETI.PS.', 1)
            call jedetc('V', '&&FETI.DD.', 1)
            call jedetc('V', '&&FETI.FIDD.', 1)
! ON INITIALISE L'INDICE DE L'INCREMENT DE TEMPS POUR MULTIPLE SECONDS
! MEMBRES
            zi(ifetpt+1)=0
!
        endif
    endif
    if (opt(1:13) .eq. 'NETTOYAGE_SDT') then
! NETTOYAGE TOTAL OBJETS TEMPORAIRES CREES DANS CRESOL,NUMERO
        call jedetr(kinf)
        call jedetr(k24pas)
        call jedetr(k24sm1)
        call jedetr(k24sm2)
        call jedetr(kfidd)
        call jedetr(kfvaf)
        call jedetr(kfnbn)
        call jedetr(kfacs)
        call jedetr(kasse)
        call jedetr(kfacn)
        call jedetr(kelem)
        call jedetr(knum)
        call jedetc('V', '.FEL', 20)
        call jedetr(kmpi)
        call jedetr(kmpib)
! DESTRUCTION DES LIGRELS DE MODELE CONSTRUIT PAR SD DANS NUMERO.F
        call jelira(kfval, 'LONMAX', nbsd, k8bid)
        nbsd=nbsd-1
        call jeveuo(kcf, 'L', ir1)
        do 600 idd = 1, nbsd
            call detrsd('LIGREL', zk24(ir1+idd-1))
600      continue
        call jedetr(kcf)
        call jedetr(kfval)
        call jedetr(k24mul)
! PUISQU'ON GARDE LE PROF_CHNO TOUT AU LONG DU CALCUL, ON GARDE
! CES OBJETS MEME APRES CHAQUE PRERES (OU SEUL LE STOCKAGE CHANGE)
        call jedetr(k24lai)
        call jedetr(k24irr)
        call jedetr(k24irg)
        call jedetr(k24irh)
        call jedetr(k24irp)
        call jedetr(k24irz)
        call jedetr(k24ir1)
        call jedetr(k24ir2)
        call jedetr(k24ir3)
        call jedetr(colaux)
        call jedetr(colaui)
        call jedetr(colau2)
        call jedetr(colau3)
    endif
    if (opt(1:10) .eq. 'RESOLUTION') then
!
!----  4.3.4  TEMPS INTERMEDIAIRES: INIT DES VECTEURS DE MONITORING
!
        call jerazo(kfacn, nbsd1, 1)
        call jerazo(kasse, nbsd1, 1)
        call jerazo(kfacs, nbsd1, 1)
        call jerazo(kelem, nbproc, 1)
    endif
!
    call matfpe(1)
!
    call jedema()
end subroutine

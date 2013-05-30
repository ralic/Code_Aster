subroutine mditm2(np2, np3, np4, n2, nbm,&
                  nbmcd, nbmp, nbnl, indic, impr,&
                  itrans, epst, icoupl, tpfl, veci1,&
                  locfl0, dt0, tfexm, ts, dttr,&
                  vecdt, iarch, vitg0, depg0, masgi,&
                  amori, pulsi, phii, vecr5, vecr3,&
                  vecr1, vecr2, vgap, vecr4, xsi0,&
                  nbsauv, indx, indxf, intge1, intge2,&
                  iconfb, tconf1, tconf2, tconfe, typch,&
                  nbseg, oldia, itforn, amor, amor0,&
                  amor00, puls, puls0, puls00, trans,&
                  pulsd, fmod0, fmod00, fmodt, fmod0t,&
                  fexmod, fnlmod, fmoda, fmres, depg,&
                  depge, depgc, depgt, depg0t, vitg,&
                  vitge, vitgc, vitgt, vitg0t, accg,&
                  accg0, accgt, dep, vit, acc,&
                  dep0, vit0, acc0, kmod, cmod,&
                  kmod0, cmod0, kmod00, cmod00, kmodca,&
                  cmodca, cmodfa, amflu0, amfluc, vg,&
                  vd, ttr, vg0, vd0, vvg,&
                  rr, ri, rr0, mtmp1, mtmp2,&
                  mtmp6, ftmp, dd, u, w,&
                  omegaf, aa, bb, fext, fextts,&
                  text, textts, fexttr, fextt0, nomch,&
                  choc, orig, rc, theta, alpha,&
                  beta, gamma, old, locflc, loc,&
                  s0, z0, sr0, za1, za2,&
                  za3, za4, za5, zin, zitr,&
                  nbchoc, parcho, noecho)
!
    implicit none
!-----------------------------------------------------------------------
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
! TOLE  CRP_21
! TOLE  CRP_20
!-----------------------------------------------------------------------
! DESCRIPTION : CALCUL DE LA REPONSE DYNAMIQUE NON-LINEAIRE D'UNE
! -----------   STRUCTURE PAR UNE METHODE INTEGRALE
!               (VERSION MULTI-MODALE)
!
!               APPELANT : MDITM1
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! -------------------------
!
! ARGUMENTS
! ---------
    include 'jeveux.h'
    include 'asterc/etausr.h'
    include 'asterc/getres.h'
    include 'asterc/r8miem.h'
    include 'asterc/r8prem.h'
    include 'asterfort/accele.h'
    include 'asterfort/adimve.h'
    include 'asterfort/alitmi.h'
    include 'asterfort/caltol.h'
    include 'asterfort/calvol.h'
    include 'asterfort/defmcf.h'
    include 'asterfort/defttr.h'
    include 'asterfort/ecrbas.h'
    include 'asterfort/ecrcho.h'
    include 'asterfort/ecrgen.h'
    include 'asterfort/inialg.h'
    include 'asterfort/inipar.h'
    include 'asterfort/inipct.h'
    include 'asterfort/iunifi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mdallo.h'
    include 'asterfort/mdchof.h'
    include 'asterfort/mdicho.h'
    include 'asterfort/mditm3.h'
    include 'asterfort/projmp.h'
    include 'asterfort/projvd.h'
    include 'asterfort/sigusr.h'
    include 'asterfort/sommve.h'
    include 'asterfort/transi.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/utexcm.h'
    include 'asterfort/uttcpr.h'
    include 'asterfort/uttcpu.h'
    include 'asterfort/vardep.h'
    integer :: np2, np3, np4, n2, nbm, nbmcd, nbmp, nbnl, indic, impr, itrans
    real(kind=8) :: epst, parcho(nbnl, *)
    integer :: icoupl
    character(len=8) :: tpfl, noecho(nbnl, *)
    integer :: veci1(*), vali
    logical :: locfl0(*)
    real(kind=8) :: dt0, tfexm, ts, dttr, vecdt(*), valr(3)
    integer :: iarch, nbchoc
    real(kind=8) :: vitg0(*), depg0(*), masgi(*), amori(*), pulsi(*)
    real(kind=8) :: phii(np2, nbm, *), vecr5(*), vecr3(*), vecr1(*), vecr2(*)
    real(kind=8) :: vgap, vecr4(*), xsi0(*)
    integer :: nbsauv, indx(*), indxf(*), intge1(*), intge2(*), iconfb(*)
    real(kind=8) :: tconf1(4, *), tconf2(4, *), tconfe(4, *)
    integer :: typch(*), nbseg(*), oldia(*), itforn(*)
    real(kind=8) :: amor(*), amor0(*), amor00(*), puls(*), puls0(*), puls00(*)
    real(kind=8) :: trans(2, 2, *), pulsd(*)
    real(kind=8) :: fmod0(*), fmod00(*), fmodt(*), fmod0t(*), fexmod(*)
    real(kind=8) :: fnlmod(*), fmoda(*), fmres(*)
    real(kind=8) :: depg(*), depge(*), depgc(*), depgt(*), depg0t(*), vitg(*)
    real(kind=8) :: vitge(*), vitgc(*), vitgt(*), vitg0t(*), accg(*), accg0(*)
    real(kind=8) :: accgt(*)
    real(kind=8) :: dep(3, *), vit(3, *), acc(3, *), dep0(3, *), vit0(3, *)
    real(kind=8) :: acc0(3, *)
    real(kind=8) :: kmod(nbm, *), cmod(nbm, *), kmod0(nbm, *), cmod0(nbm, *)
    real(kind=8) :: kmod00(nbm, *), cmod00(nbm, *), kmodca(nbm, *)
    real(kind=8) :: cmodca(nbm, *), cmodfa(nbm, *), amflu0(nbm, *)
    real(kind=8) :: amfluc(nbm, *)
    real(kind=8) :: vg(nbm, *), vd(nbm, *), ttr(n2, *), vg0(nbm, *), vd0(nbm, *)
    real(kind=8) :: vvg(nbm, *), rr(*), ri(*), rr0(*), mtmp1(nbm, *)
    real(kind=8) :: mtmp2(nbm, *), mtmp6(3, *), ftmp(*), dd(*), u(*), w(*)
    real(kind=8) :: omegaf(*), aa(np4, *), bb(np4, *), fext(np4, *)
    real(kind=8) :: fextts(np4, *), text(*), textts(*), fexttr(*), fextt0(*)
    character(len=8) :: nomch(*)
    real(kind=8) :: choc(6, *), orig(6, *), rc(np3, *), theta(np3, *)
    real(kind=8) :: alpha(2, *), beta(2, *), gamma(2, *), old(9, *)
    logical :: locflc(*), loc(*)
    complex(kind=8) :: s0(*), z0(*), sr0(*), za1(*), za2(*), za3(*), za4(np4, *)
    complex(kind=8) :: za5(np4, *), zin(*), zitr(*)
!
! VARIABLES LOCALES
! -----------------
    integer :: itest, testc, ier, ivar, indnew, indne0, xit0, it0, idecrt
    integer :: ntrans, xnbr0, ibid, ilong, npf, npfmax, npfts, nttr, ndef, indt
    integer :: nbr0
    integer :: jordre, jtemps, jdepg, jvitg, jaccg, jdep, jfor, jvit, kdepl
    integer :: kvite, kacce, kordr, kinst, kptem, kfcho, kdcho, kvcho, kadcho
    integer :: krevc, krevd, jptem
    integer :: ifr, ifm, latest, iercpu, nbrede, nbrevi, kredc, kredd
    real(kind=8) :: tc, dt, div, tc0, ts0, tol, tolc, toln, tolv, ftest, ftest0
    real(kind=8) :: ttrans, premac, prerel, tps1(4)
    logical :: lsauv
    character(len=8) :: resu, k8b
    character(len=16) :: nomcmd, typres, chain1, chain2, chain3, chain4, chain5
    character(len=16) :: chain6, chain7, chain8, chain9, method
    character(len=4) :: k4bid(3)
!
! FONCTIONS INTRINSEQUES
! ----------------------
!     INTRINSIC     MOD
!
! FONCTIONS EXTERNES
! ------------------
!     EXTERNAL      IUNIFI, R8MIEM, R8PREM
!
! ROUTINES EXTERNES
! -----------------
!     EXTERNAL      ACCELE, ADIMVE, ALITMI, CALTOL, CALVOL, DEFMCF,
!    &              DEFTTR, ECRBAS, ECRCHO, ECRGEN, INIALG, INIPAR,
!    &              INIPCT, MDALLO, MDCHOF, MDITM3, PROJMP,
!    &              PROJVD, SOMMVE, TRANSI
!    &              UTTCPU('CPU.MDITM2', VARDEP,
!    &              GETRES, JEDEMA, JELIRA, JEMARQ, JEVEUO
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    call jemarq()
    premac = r8prem()
    prerel = r8miem()/r8prem()
    call getres(resu, typres, nomcmd)
!
! 1.  PREPARATION TABLEAUX ASTER
!     --------------------------
    chain1 = '&&MDITM2.ORDRE'
    chain2 = '&&MDITM2.TEMPS'
    chain3 = '&&MDITM2.DEPLG'
    chain4 = '&&MDITM2.VITEG'
    chain5 = '&&MDITM2.ACCEG'
    chain6 = '&&MDITM2.DEPNC'
    chain7 = '&&MDITM2.FORNC'
    chain8 = '&&MDITM2.VITNC'
    chain9 = '&&MDITM2.PTEMP'
    ilong = 0
    call mditm3(chain1, chain2, chain3, chain4, chain5,&
                chain6, chain7, chain8, chain9, ilong,&
                nbm, nbnl)
    call jeveuo(chain1, 'E', jordre)
    call jeveuo(chain2, 'E', jtemps)
    call jeveuo(chain3, 'E', jdepg)
    call jeveuo(chain4, 'E', jvitg)
    call jeveuo(chain5, 'E', jaccg)
    call jeveuo(chain6, 'E', jdep)
    call jeveuo(chain7, 'E', jfor)
    call jeveuo(chain8, 'E', jvit)
    call jeveuo(chain9, 'E', jptem)
!
! 2.  INITIALISATION DES PARAMETRES DE CALCUL
!     ---------------------------------------
    call inipct(itest, div, nbr0, tc)
!
! 3.  CALCUL DES TOLERANCES DE CALCUL EN FONCTION DE LA CONFIGURATION
!     ---------------------------------------------------------------
    call caltol(np3, nbnl, typch, nbseg, rc,&
                theta, tol, tolc, toln, tolv)
!
! 4.  CALCUL DES MATRICES DE RAIDEUR ET AMORTISSEMENT DU SYSTEME EN VOL
!     -----------------------------------------------------------------
    call calvol(nbm, nbm, icoupl, indic, kmod00,&
                cmod00, amor00, puls00, pulsi, amori,&
                masgi, tpfl, veci1, vecr1, vecr2,&
                vecr5, vecr3, vgap, vecr4, locfl0,&
                amflu0, xsi0)
!
! 5.  DEFINITION DES MODES PRIS EN COMPTE POUR LE CALCUL DU SAUT DE
!     FORCE FLUIDELASTIQUE D'AMORTISSEMENT EN PHASE DE CHOC
!     -----------------------------------------------------
    call defmcf(nbm, nbmp, locfl0, locflc)
!
! 6.  CALCUL DU TRANSITOIRE
!     ---------------------
    tc0 = 0.0d0
    npfmax = np4
    if (itrans .eq. 1) then
        call transi(nbm, np2, np3, np4, nbmcd,&
                    nbnl, npfmax, npfts, dttr, ttrans,&
                    epst, fext, text, fextts, textts,&
                    fexttr, fextt0, masgi, amori, pulsi,&
                    phii, typch, nbseg, rc, alpha,&
                    beta, gamma, orig, theta, vitg,&
                    depg, amor, pulsd, omegaf, aa,&
                    bb, old, s0, z0, sr0,&
                    za1, za2, za3, za4, za5,&
                    zitr, zin, trans, amor00, puls00,&
                    accg0, vitg0, depg0, iconfb, tconf1,&
                    ftest0, ier)
    else
        ttrans = 0.0d0
        ntrans = 0
        npf = 0
        call defttr(nbm, np4, nbmcd, npf, nttr,&
                    ntrans, ttrans, ttrans, text, fext,&
                    fexttr, fexttr, dttr)
        call adimve(nbmcd, fexttr, masgi)
        call inialg(nbm, np2, np3, np4, nbmcd,&
                    nbnl, nttr, npfmax, npfts, depg,&
                    vitg, depg0, vitg0, accg0, amor00,&
                    puls00, fexttr, fext, text, fextts,&
                    textts, typch, nbseg, phii, alpha,&
                    beta, gamma, orig, rc, theta,&
                    iconfb, tconf1, ftest0)
    endif
    call ecrgen(nbr0, nbm, tc0, dt0, depg0,&
                vitg0, accg0, zr(jdepg), zr(jvitg), zr(jaccg),&
                zr(jtemps), zi(jordre), zr(jptem))
    call ecrcho(nbr0, nbnl, old, zr(jdep), zr(jvit),&
                zr(jfor))
!
!-----------------------------------------------------------------------
! 7.  BOUCLE TEMPORELLE APRES PASSAGE DU TRANSITOIRE
!-----------------------------------------------------------------------
! 7.1 INITIALISATIONS
!     ---------------
    it0 = 0
    idecrt = 0
    testc = 0
    indne0 = 0
    indnew = 0
    ndef = 0
!
    dt = dt0
    dt0 = 0.0d0
    ts0 = tfexm - ttrans
    if (ts0 .lt. ts) then
        ts = ts0
        call u2mess('A', 'ALGORITH5_52')
    endif
!
    tc = 0.0d0
    tc = tc + dt
    lsauv = .true.
!
! 7.2 IMPRESSIONS AVANT EXECUTION DU CALCUL
!     -------------------------------------
    1001 format(/3x,'====================================================',&
     &'===========================')
    1002 format(3x,'          RESULTAT DU CALCUL TEMPOREL AVEC COUPLAGE ',&
     &'FLUIDE-STRUCTURE           ')
    1003 format(3x,'=====================================================',&
     &'=========================='/)
    1007 format(3x,'FIN DU TRANSITOIRE                   -            ',&
     &'TEMPS COURANT: ',1pd14.6/)
    1004 format(3x,'DEBUT DU CALCUL TEMPOREL             -            ',&
     &'TEMPS COURANT: ',1pd14.6/)
    1005 format(3x,'FIN DU CALCUL TEMPOREL               -            ',&
     &'TEMPS COURANT: ',1pd14.6/)
    1006 format(3x,'NOMBRE D APPELS A ALITMI             -            ',&
     &'    ',i9/)
    1009 format(3x,'****************      ARRET  PAR  MANQUE  DE  TEMPS  ',&
     &'CPU     ******************'/)
!
    if (impr .ge. 1) then
        ifr = iunifi('RESULTAT')
        ifm = iunifi('MESSAGE')
        if (impr .ge. 2) then
            write(ifr,1001)
            write(ifr,1002)
            write(ifr,1003)
            write(ifr,1007) ttrans
            write(ifr,1003)
            write(ifr,1004) (tc - dt)
        endif
        write(ifm,1001)
        write(ifm,1002)
        write(ifm,1003)
        write(ifm,1007) ttrans
        write(ifm,1003)
        write(ifm,1004) (tc - dt)
    endif
!
! 7.3 DEBUT DU BLOC 'TANT QUE'
!     ------------------------
    call uttcpu('CPU.MDITM2', 'INIT', ' ')
100  continue
    call uttcpu('CPU.MDITM2', 'DEBUT', ' ')
    latest = testc
!
! --- TANT QUE TC EST INFERIEUR A LA DUREE DE LA SIMULATION
!
    if (tc .le. ts) then
!
! 7.3.1  CALCUL DU VECTEUR D'ETAT A L'INSTANT N+1 EN FONCTION DU VECTEUR
!        D'ETAT A L'INSTANT N
!
        call alitmi(nbm, np2, np3, np4, n2,&
                    nbm, nbmcd, icoupl, tc, dt0,&
                    dt, vecdt, nbnl, testc, itest,&
                    indnew, indne0, idecrt, ftest, ftest0,&
                    iconfb, tconf1, tconf2, tconfe, typch,&
                    nbseg, phii, choc, alpha, beta,&
                    gamma, orig, rc, theta, old,&
                    oldia, itforn, vgap, vecr4, xsi0,&
                    indic, tpfl, veci1, vecr1, vecr2,&
                    vecr5, vecr3, masgi, amori, pulsi,&
                    amor, amor0, puls, puls0, accg0,&
                    vitg0, depg0, vitge, depge, vitg,&
                    depg, vitgc, depgc, vitgt, depgt,&
                    vitg0t, depg0t, cmod0, kmod0, cmod,&
                    kmod, cmodca, kmodca, amflu0, amfluc,&
                    cmodfa, locflc, npfts, textts, fextts,&
                    ndef, indt, fexmod, fnlmod, fmres,&
                    fmoda, fmod0, fmod00, fmodt, fmod0t,&
                    div, tol, tolc, toln, tolv,&
                    intge1, intge2, indx, indxf, ftmp,&
                    mtmp1, mtmp2, mtmp6, ttr, u,&
                    w, dd, loc, vvg, vg,&
                    vg0, vd, vd0, rr, rr0,&
                    ri, premac, prerel, trans, pulsd,&
                    s0, z0, sr0, za1, za2,&
                    za3, zin)
!
! 7.3.2  ACTUALISATION DE LA FORCE NON LINEAIRE RESIDUELLE
!        A L'INSTANT N+1
!
        tc0 = tc - dt
        testc = latest
        if (indnew .eq. 0) then
            call mdchof(nbm, np2, np3, nbm, impr,&
                        tc0, nbnl, typch, nbseg, phii,&
                        nomch, choc, alpha, beta, gamma,&
                        orig, rc, theta, vitge, depge,&
                        vitg0, depg0, old, oldia, fmres,&
                        fnlmod, ftmp, testc, itforn, toln)
        else
            call mdchof(nbm, np2, np3, nbm, impr,&
                        tc0, nbnl, typch, nbseg, phii,&
                        nomch, choc, alpha, beta, gamma,&
                        orig, rc, theta, vitgc, depgc,&
                        vitg0, depg0, old, oldia, fmres,&
                        fnlmod, ftmp, testc, itforn, toln)
        endif
        indne0 = indnew
        indnew = 0
        call sommve(nbm, fexmod, nbm, fnlmod, nbm,&
                    fmod00)
        call sommve(nbm, fexmod, nbm, fmres, nbm,&
                    fmoda)
        call adimve(nbm, fmoda, masgi)
!
! 7.3.3  PROJECTION DE LA FORCE MODALE SUR LA BASE MODALE
!
        call projvd(testc, nbm, nbm, nbm, vg,&
                    fmoda, fmodt)
!
! 7.3.4  CALCUL DE L'ACCELERATION
!
        if (testc .eq. 0) then
            call accele(nbmcd, amor00, puls00, fmoda, accg,&
                        vitg, depg)
        else
            call accele(nbmcd, amor, puls, fmodt, accgt,&
                        vitgt, depgt)
            call projvd(testc, nbm, nbm, nbmcd, vd,&
                        accgt, accg)
        endif
!
! 7.3.5  PASSAGE EN BASE PHYSIQUE ET ECRITURE DES RESULTATS
!
        xit0 = mod(it0,iarch)
        if (lsauv) then
            xit0 = 0
            lsauv = .false.
        endif
        if (xit0 .eq. 0) then
            nbr0 = nbr0 + 1
            if (testc .eq. 0) then
                call projmp(nbm, np2, nbmcd, nbnl, phii,&
                            accg, vitg, depg, acc, vit,&
                            dep)
            else
                call projmp(nbm, np2, nbm, nbnl, phii,&
                            accg, vitg, depg, acc, vit,&
                            dep)
            endif
            call vardep(nbnl, dep, dep0, tconf2, tconf1,&
                        ivar, dt0, toln, tolc, tolv)
            if (ivar .ne. 0) call u2mess('A', 'ALGORITH5_53')
            xnbr0 = mod(nbr0+1,10000)
            if (xnbr0 .eq. 0) then
                call jelira(chain1, 'LONMAX', ilong, k8b)
                call mditm3(chain1, chain2, chain3, chain4, chain5,&
                            chain6, chain7, chain8, chain9, ilong,&
                            nbm, nbnl)
                call jeveuo(chain1, 'E', jordre)
                call jeveuo(chain2, 'E', jtemps)
                call jeveuo(chain3, 'E', jdepg)
                call jeveuo(chain4, 'E', jvitg)
                call jeveuo(chain5, 'E', jaccg)
                call jeveuo(chain6, 'E', jdep)
                call jeveuo(chain7, 'E', jfor)
                call jeveuo(chain8, 'E', jvit)
                call jeveuo(chain9, 'E', jptem)
            endif
            call ecrgen(nbr0, nbm, tc0, dt, depg,&
                        vitg, accg, zr(jdepg), zr(jvitg), zr(jaccg),&
                        zr(jtemps), zi(jordre), zr(jptem))
            call ecrcho(nbr0, nbnl, old, zr(jdep), zr(jvit),&
                        zr(jfor))
        endif
!
! 7.3.6  CONDITIONS INITIALES POUR LE PAS DE TEMPS SUIVANT
!
        it0 = it0 + 1
        idecrt = 0
        if (testc .eq. 0) then
            call inipar(nbm, nbm, nbnl, testc, cmod0,&
                        cmod00, kmod0, kmod00, amor00, amor0,&
                        puls00, puls0, acc, vit, dep,&
                        acc0, vit0, dep0, accg, vitg,&
                        depg, accg0, vitg0, depg0, tconf1,&
                        ftest0, tconf2, ftest)
        else if (testc.eq.1) then
            if (icoupl .eq. 0) then
                call inipar(nbm, nbm, nbnl, testc, cmod0,&
                            cmodca, kmod0, kmodca, amor, amor0,&
                            puls, puls0, acc, vit, dep,&
                            acc0, vit0, dep0, accg, vitg,&
                            depg, accg0, vitg0, depg0, tconf1,&
                            ftest0, tconf2, ftest)
            else if (icoupl.eq.1) then
                call inipar(nbm, nbm, nbnl, testc, cmod0,&
                            cmodfa, kmod0, kmodca, amor, amor0,&
                            puls, puls0, acc, vit, dep,&
                            acc0, vit0, dep0, accg, vitg,&
                            depg, accg0, vitg0, depg0, tconf1,&
                            ftest0, tconf2, ftest)
            endif
        endif
!
! --- SI TC EST SUPERIEUR A LA DUREE DE LA SIMULATION,
! --- SORTIE DU BLOC 'TANT QUE'
!
    else
        goto 999
    endif
!
! --- SORTIE DU BLOC 'TANT QUE' SI MANQUE DE TEMPS CPU
!
    iercpu = 0
    call uttcpu('CPU.MDITM2', 'FIN', ' ')
    call uttcpr('CPU.MDITM2', 4, tps1)
    if ((tps1(4).gt.(0.90d0*(tps1(1)-20.0d0))) .or. (tps1(1).lt.20.0d0)) then
        iercpu = 1
        goto 999
    endif
!
! --- RETOURNER AU DEBUT DU BLOC 'TANT QUE'
!
    goto 100
!
! 7.4 IMPRESSIONS APRES EXECUTION DU CALCUL
!     -------------------------------------
999  continue
    if (impr .ge. 1) then
        write(ifm,1005) tc
        write(ifm,1003)
        write(ifm,1006) it0
        if (iercpu .eq. 1) write(ifm,1009)
        write(ifm,1003)
        if (impr .ge. 2) then
            write(ifr,1005) tc
            write(ifr,1003)
            write(ifr,1006) it0
            if (iercpu .eq. 1) write(ifr,1009)
            write(ifr,1003)
        endif
    endif
!
! 7.5 ARCHIVAGE DES RESULTATS
!     -----------------------
    nbsauv = nbr0
    nbrede = 0
    nbrevi = 0
    method = 'ITMI'
!
    call mdallo(resu, k8b, k8b, k8b, k8b,&
                nbm, dt0, nbsauv, nbnl, k8b,&
                k8b, nbrede, k8b, nbrevi, k8b,&
                kdepl, kvite, kacce, kptem, kordr,&
                kinst, kfcho, kdcho, kvcho, kadcho,&
                kredc, kredd, krevc, krevd, method,&
                ibid, k4bid, 'TRAN', 'GLOB')
!
    call ecrbas(nbsauv, nbnl, nbm, zr(jdepg), zr(jvitg),&
                zr(jaccg), zr(jtemps), zi(jordre), zr(jptem), zr(jdep),&
                zr(jvit), zr(jfor), zr(kdepl), zr(kvite), zr(kacce),&
                zr(kinst), zi(kordr), zr(kptem), zr(kdcho), zr(kvcho),&
                zr(kfcho))
!     --- IMPRESSION DES RESULTATS DE CHOC
!
    if (nbnl .ne. 0) then
        call mdicho(resu, nbsauv, zr(kinst), zr(kfcho), zr(kdcho),&
                    zr(kvcho), nbnl, nbchoc, parcho, noecho)
    endif
!
! 7.6 IMPRESSIONS SUPPLEMENTAIRES SI ARRET PAR MANQUE DE TEMPS CPU
!     ------------------------------------------------------------
    if (iercpu .eq. 1) then
        vali = it0
        valr (1) = tc
        valr (2) = tps1(4)
        valr (3) = tps1(1)
        call utexcm(28, 'ALGORITH16_87', 0, ' ', 1,&
                    vali, 3, valr)
    endif
!
! --- VERIFICATION SI INTERRUPTION DEMANDEE PAR SIGNAL USR1
!
    if (etausr() .eq. 1) then
        call sigusr()
    endif
!
    call jedema()
!
! --- FIN DE MDITM2.
end subroutine

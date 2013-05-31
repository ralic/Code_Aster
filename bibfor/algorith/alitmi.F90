subroutine alitmi(np1, np2, np3, np4, n2,&
                  nbm, nbmcd, icoupl, tc, dt0,&
                  dt, vecdt, nbnl, ichoc, itest,&
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
! aslint: disable=W1504
    implicit none
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
! DESCRIPTION : PROCEDURE DE CALCUL DU VECTEUR D'ETAT A L'INSTANT N+1
! -----------   EN FONCTION DU VECTEUR D'ETAT A L'INSTANT N
!
!               APPELANT : MDITM2
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    include 'asterfort/adimve.h'
    include 'asterfort/calcmd.h'
    include 'asterfort/calcmi.h'
    include 'asterfort/calfmn.h'
    include 'asterfort/calfnl.h'
    include 'asterfort/coupla.h'
    include 'asterfort/estivd.h'
    include 'asterfort/matini.h'
    include 'asterfort/newton.h'
    include 'asterfort/projmd.h'
    include 'asterfort/projvd.h'
    include 'asterfort/sommma.h'
    include 'asterfort/testch.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/vecini.h'
    include 'blas/dcopy.h'
    integer :: np1, np2, np3, np4, n2, nbm, nbmcd, icoupl
    real(kind=8) :: tc, dt0, dt, vecdt(*)
    integer :: nbnl, ichoc, itest, indnew, indne0, idecrt
    real(kind=8) :: ftest, ftest0
    integer :: iconfb(*)
    real(kind=8) :: tconf1(4, *), tconf2(4, *), tconfe(4, *)
    integer :: typch(*), nbseg(*)
    real(kind=8) :: phii(np2, np1, *), choc(6, *), alpha(2, *), beta(2, *)
    real(kind=8) :: gamma(2, *), orig(6, *), rc(np3, *), theta(np3, *)
    real(kind=8) :: old(9, *)
    integer :: oldia(*), itforn(*)
    real(kind=8) :: vgap, vecr4(*), xsi0(*)
    integer :: indic
    character(len=8) :: tpfl
    integer :: veci1(*)
    real(kind=8) :: vecr1(*), vecr2(*), vecr5(*), vecr3(*)
    real(kind=8) :: masgi(*), amori(*), pulsi(*)
    real(kind=8) :: amor(*), amor0(*), puls(*), puls0(*)
    real(kind=8) :: accg0(*), vitg0(*), depg0(*), vitge(*), depge(*), vitg(*)
    real(kind=8) :: depg(*), vitgc(*), depgc(*), vitgt(*), depgt(*), vitg0t(*)
    real(kind=8) :: depg0t(*)
    real(kind=8) :: cmod0(np1, *), kmod0(np1, *), cmod(np1, *), kmod(np1, *)
    real(kind=8) :: cmodca(np1, *), kmodca(np1, *), amflu0(np1, *)
    real(kind=8) :: amfluc(np1, *), cmodfa(np1, *)
    logical :: locflc(*)
    integer :: npfts
    real(kind=8) :: textts(*), fextts(np4, *)
    integer :: ndef, indt
    real(kind=8) :: fexmod(*), fnlmod(*), fmres(*), fmoda(*), fmod0(*)
    real(kind=8) :: fmod00(*), fmodt(*), fmod0t(*)
    real(kind=8) :: div, tol, tolc, toln, tolv
    integer :: intge1(*), intge2(*), indx(*), indxf(*)
    real(kind=8) :: ftmp(*), mtmp1(np1, *), mtmp2(np1, *), mtmp6(3, *)
    real(kind=8) :: ttr(n2, *), u(*), w(*), dd(*)
    logical :: loc(*)
    real(kind=8) :: vvg(np1, *), vg(np1, *), vg0(np1, *), vd(np1, *)
    real(kind=8) :: vd0(np1, *), rr(*), rr0(*), ri(*)
    real(kind=8) :: premac, prerel, trans(2, 2, *), pulsd(*)
    complex(kind=8) :: s0(*), z0(*), sr0(*), za1(*), za2(*), za3(*), zin(*)
!
! VARIABLES LOCALES
! -----------------
    integer :: i, ic, ier, nbche, nbchee, iconfe, nbcha, nbchea, iconfa, iconf
    integer :: inewto, ichoc0, niter, niter0, typj
    real(kind=8) :: omega, tetaes, maxvit, fteste, somvit, dtc
! DEBUG
    real(kind=8) :: tolch
! DEBUG
!
! FONCTIONS INTRINSEQUES
! ----------------------
!     INTRINSIC     ABS, MIN
!
! ROUTINES EXTERNES
! -----------------
!     EXTERNAL      ADIMVE, CALCMD, CALCMI, CALFMN, CALFNL, COUPLA,
!    &              ESTIVD, MATINI, LCINVN, NEWTON, PROJMD,
!    &              PROJVD, SOMMMA, TESTCH,
! DEBUG
!     EXTERNAL      DCOPY
! DEBUG
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
!-----------------------------------------------------------------------
!     INITIALISATIONS
!-----------------------------------------------------------------------
! DEBUG
    tolch = 10.0d0 * toln
! DEBUG
    ichoc0 = ichoc
    niter = 0
    niter0 = 10
    tetaes = 1.0d0
    maxvit = 1.0d0
    inewto = 0
    call matini(np1, np1, 0.d0, vg)
    call matini(np1, np1, 0.d0, vd)
    call vecini(np1, 0.d0, fmodt)
    call vecini(np1, 0.d0, depg0t)
    call vecini(np1, 0.d0, vitg0t)
    call vecini(np1, 0.d0, fmod0t)
!
!-----------------------------------------------------------------------
!     ESTIMATION DES DDLS GENERALISES A L'INSTANT N+1
!     (REPETER JUSQU'A VALIDATION DE L'INSTANT N+1)
!-----------------------------------------------------------------------
10  continue
    call vecini(np1, 0.d0, depge)
    call vecini(np1, 0.d0, vitge)
    call vecini(np1, 0.d0, depg)
    call vecini(np1, 0.d0, vitg)
!C
!C 1.  ESTIMATION DES DDLS A L'INSTANT N+1 PAR LE SCHEMA D'EULER
!C     ---------------------------------------------------------
!C     INEWTO = 0 INDIQUE A ESTIVD QUE LA ROUTINE APPELANTE EST ALITMI
!C
    call estivd(nbm, dt, vitge, depge, accg0,&
                vitg0, depg0, tetaes, maxvit, inewto)
!C
!C 2.  TEST DE CHANGEMENT DE CONFIGURATION ENTRE LES INSTANTS N ET N+1
!C     AVEC LA SOLUTION DU SCHEMA D'EULER
!C     ----------------------------------
!
    call testch(np1, np2, np3, nbm, nbnl,&
                toln, tolc, tolv, typch, nbseg,&
                phii, alpha, beta, gamma, orig,&
                rc, theta, tconf1, depge, nbche,&
                nbchee, iconfe, fteste, iconfb, tconfe)
!
! 3.  RAFFINEMENT DU PAS DE TEMPS SI LA SOLUTION DU SCHEMA D'EULER
!     CONDUIT A UNE VARIATION IMPORTANTE DU DEPLACEMENT PHYSIQUE
!     ENTRE LES INSTANTS N ET N+1
!     ---------------------------
    if ((iconfe.eq.-1) .and. (niter.le.niter0)) then
!
! 3.1    SI L'INSTANT N EST UN INSTANT DE CHANGEMENT DE CONFIGURATION
!        OU SI ON EST PROCHE D'UN CHANGEMENT D'ETAT A L'INSTANT N
!        => DT = DT/10
!
        if ((indne0.eq.1) .or. (abs(ftest0).lt.tol)) then
            tc = tc - dt
            dt = dt/10.0d0
            tc = tc + dt
            idecrt = 1
            niter = niter + 1
! --------- ON RETOURNE A L'ESTIMATION D'EULER ESTIVD
            goto 10
!
! 3.2    SINON => DT = DT/2
!
        else
            tc = tc - dt
            dt = dt/2.0d0
            tc = tc + dt
            idecrt = 1
            niter = niter + 1
! --------- ON RETOURNE A L'ESTIMATION D'EULER ESTIVD
            goto 10
!
        endif
    endif
!
! 4.  ESTIMATION DE LA FORCE NON-LINEAIRE A L'INSTANT N+1 A L'AIDE DE
!     LA SOLUTION DU SCHEMA D'EULER
!     -----------------------------
!     INEWTO = 0 INDIQUE A MDCHOE QUE LA ROUTINE APPELANTE EST ALITMI
!
    call calfnl(np1, np2, np3, np4, nbm,&
                nbm, npfts, tc, nbnl, typch,&
                nbseg, phii, choc, alpha, beta,&
                gamma, orig, rc, theta, masgi,&
                amori, pulsi, vitge, depge, vitg0,&
                depg0, cmod, kmod, cmodca, kmodca,&
                textts, fextts, ndef, indt, niter,&
                fexmod, fnlmod, fmres, fmoda, ftmp,&
                mtmp1, mtmp6, old, oldia, ichoc,&
                itforn, inewto, toln)
!
! 5.  DIAGONALISATION DE LA MATRICE DE RAIDEUR A L'INSTANT N+1
!     --------------------------------------------------------
    ier = 0
    call calcmd(np1, kmodca, kmod0, nbm, nbmcd,&
                typj, vvg, vg, vg0, vd,&
                vd0, rr, rr0, ri, n2,&
                ier, ichoc, premac, prerel, mtmp1,&
                mtmp2, ttr, u, w, dd,&
                intge1, intge2, indx, indxf, loc)
!
    if (ier .ne. 0) call u2mess('F', 'ALGORITH_10')
!
    do 20 i = 1, nbm
        puls(i) = rr(i)
20  end do
    do 30 i = 1, nbm
        if ((puls(i).eq.0.0d0) .and. (i.le.nbmcd)) then
            puls(i) = puls0(i)
            call u2mess('I', 'ALGORITH_11')
        endif
30  end do
!
    if ((ichoc0.eq.1) .and. (ichoc.eq.0)) typj = 1
!
! 6.  CALCUL DES FORCES DE COUPLAGE A L'INSTANT N+1 LE CAS ECHEANT
!     ------------------------------------------------------------
    if ((ichoc.eq.1) .and. (icoupl.eq.1)) then
        call coupla(np1, nbm, indic, tpfl, veci1,&
                    vgap, vecr4, vecr1, vecr2, vecr5,&
                    vecr3, masgi, puls, locflc, amflu0,&
                    amfluc, xsi0)
        call sommma(np1, nbm, nbm, amfluc, cmodca,&
                    cmodfa)
    endif
!
! 7.  CALCUL DES EXCITATIONS GENERALISEES A L'INSTANT N
!     -------------------------------------------------
    call calfmn(np1, nbm, ichoc0, fmod0, fmod00,&
                cmod, kmod, vitg0, depg0)
    call adimve(nbm, fmod0, masgi)
!
! 8.  PROJECTIONS SUR LA BASE MODALE A L'INSTANT N+1
!     ----------------------------------------------
! 8.1 PROJECTION DE LA MATRICE D'AMORTISSEMENT
!
    if ((ichoc.eq.1) .and. (icoupl.eq.1)) then
        call projmd(ichoc, np1, nbm, nbmcd, cmodfa,&
                    vg, vd, amor, mtmp1, mtmp2)
    else
        call projmd(ichoc, np1, nbm, nbmcd, cmodca,&
                    vg, vd, amor, mtmp1, mtmp2)
    endif
!
! 8.2 PROJECTIONS DU VECTEUR EXCITATIONS  GENERALISEES  A L'INSTANT N
!                 DU VECTEUR EXCITATIONS  GENERALISEES  A L'INSTANT N+1
!                 DU VECTEUR DEPLACEMENTS GENERALISES   A L'INSTANT N
!                 DU VECTEUR VITESSES     GENERALISEES  A L'INSTANT N
!
    call projvd(ichoc, np1, nbm, nbm, vg,&
                fmod0, fmod0t)
    call projvd(ichoc, np1, nbm, nbm, vg,&
                fmoda, fmodt)
    call projvd(ichoc, np1, nbm, nbm, vg,&
                depg0, depg0t)
    call projvd(ichoc, np1, nbm, nbm, vg,&
                vitg0, vitg0t)
!
! 9.  CALCUL DES DDLS GENERALISES A L'INSTANT N+1 PAR METHODE INTEGRALE
!     APPLICATION DU SCHEMA ITMI
!     --------------------------
    call calcmi(np1, nbmcd, dt0, dt, vitgt,&
                depgt, vitg0t, depg0t, fmodt, fmod0t,&
                amor, amor0, puls, puls0, trans,&
                pulsd, s0, z0, sr0, za1,&
                za2, za3, zin)
!
! 10. RETOUR SUR LA BASE MODALE EN VOL
!     --------------------------------
! 10.1 PROJECTION DU VECTEUR DEPLACEMENTS GENERALISES A L'INSTANT N+1
!
    call projvd(ichoc, np1, nbm, nbmcd, vd,&
                depgt, depg)
!
! 10.2 PROJECTION DU VECTEUR VITESSES GENERALISEES A L'INSTANT N+1
!
    call projvd(ichoc, np1, nbm, nbmcd, vd,&
                vitgt, vitg)
!
! 11. TEST DE CHANGEMENT DE CONFIGURATION ENTRE LES INSTANTS N ET N+1
!     AVEC LA SOLUTION DU SCHEMA ITMI
!     -------------------------------
    if (ichoc .eq. 1) then
        call testch(np1, np2, np3, nbm, nbnl,&
                    toln, tolc, tolv, typch, nbseg,&
                    phii, alpha, beta, gamma, orig,&
                    rc, theta, tconf1, depg, nbcha,&
                    nbchea, iconfa, ftest, iconfb, tconf2)
    else
        call testch(np1, np2, np3, nbmcd, nbnl,&
                    toln, tolc, tolv, typch, nbseg,&
                    phii, alpha, beta, gamma, orig,&
                    rc, theta, tconf1, depg, nbcha,&
                    nbchea, iconfa, ftest, iconfb, tconf2)
    endif
!
    if (iconfe .ne. 0) then
        iconf = iconfa
    else
        iconf = 0
    endif
!
! 12. VALIDATION DE L'INSTANT N+1 OU RAFFINEMENT DU PAS DE TEMPS
!     EN FONCTION DU CHANGEMENT DE CONFIGURATION
!     ------------------------------------------
! 12.1 SI UN CHANGEMENT DE CONFIGURATION SE PRODUIT ENTRE LES
!      INSTANTS N ET N+1
!
    if (iconf .eq. 0) then
!
! 12.1.1 SI LE CHANGEMENT DE CONFIGURATION EST PROCHE, APPEL A NEWTON
!
        if (((abs(ftest).lt.tol).and.(abs(ftest0).lt.tolc)) .or. ( niter.ge.niter0)) then
!
            ichoc = ichoc0
!
            call newton(np1, np2, np3, np4, nbm,&
                        n2, nbmcd, icoupl, tc, dt,&
                        dtc, vecdt, nbnl, typch, nbseg,&
                        phii, choc, alpha, beta, gamma,&
                        orig, rc, theta, vgap, vecr4,&
                        indic, tpfl, veci1, vecr1, vecr2,&
                        vecr5, vecr3, masgi, amori, pulsi,&
                        amor, amor0, puls, puls0, xsi0,&
                        vitg, depg, accg0, vitg0, depg0,&
                        vitgc, depgc, vitgt, depgt, cmod,&
                        kmod, cmod0, kmod0, cmodca, kmodca,&
                        amflu0, amfluc, locflc, cmodfa, npfts,&
                        textts, fextts, ndef, indt, fexmod,&
                        fnlmod, fmoda, fmres, fmod0, fmod00,&
                        fmodt, fmod0t, vitg0t, depg0t, ftmp,&
                        mtmp1, mtmp2, mtmp6, ttr, u,&
                        w, dd, loc, intge1, intge2,&
                        indx, indxf, vvg, vg, vg0,&
                        vd, vd0, rr, rr0, ri,&
                        premac, prerel, trans, pulsd, s0,&
                        z0, sr0, za1, za2, za3,&
                        zin, old, oldia, iconfe, iconfa,&
                        nbcha, nbchea, ftest, iconfb, tconf1,&
                        tconf2, toln, tolc, tolv, ichoc,&
                        itforn)
!
! --------- VALIDATION DE L'INSTANT N+1
            dt0 = dt
! ......... LES PULSATIONS PULS(I) SONT CELLES DE LA BASE MODALE DE LA
! ......... DE LA CONFIGURATION A L'INSTANT N, CAR LES JACOBIENS DES
! ......... FORCES NON LINEAIRES SONT IMPLICITES A L'INSTANT N LORS DE
! ......... L'APPEL A NEWTON. ON NE PEUT DONC PAS ESTIMER A PRIORI LE
! ......... NOUVEAU PAS DE TEMPS.
! ......... ON CHOISIT DT = MIN(DT0,ABS(DTC)), DTC ETANT L'INCREMENT
! ......... TEMPOREL AYANT PERMIS D'ATTEINDRE LE CHANGEMENT DE CONF.
! DEBUG
!            IF ( DTC.NE.0.0D0 ) DT = MIN(DT0,ABS(DTC))
            if (dtc .eq. 0.0d0) then
                call dcopy(nbm, depge(1), 1, depgc(1), 1)
                call dcopy(nbm, vitge(1), 1, vitgc(1), 1)
            else
                dt = min(dt0,abs(dtc))
            endif
! DEBUG
            tc = tc + dt
            indnew = 1
            itest = 1
!
! 12.1.2 SINON ON RECALCULE LA CONFIGURATION A L'INSTANT N+1
!        EN RAFFINANT LE PAS DE TEMPS
!
        else
!
            tc = tc - dt
!
! --------- SI UN CHANGEMENT DE CONFIGURATION S'EST PRODUIT
! --------- A L'INSTANT N
            if (indne0 .eq. 1) then
!
! ............ CALCUL DU PRODUIT SCALAIRE DES VECTEURS VITESSES
! ............ GENERALISEES (INSTANTS N ET N+1)
                somvit = 0.0d0
                do 50 i = 1, nbm
                    somvit = somvit + vitg(i)*vitg0(i)
50              continue
!
! ............ SI LE PRODUIT SCALAIRE EST NEGATIF => DT = DT/5
                if (somvit .lt. 0.0d0) then
                    dt = dt/5.0d0
!
! ............ SINON => DT = DT/2
                else
                    dt = dt/2.0d0
                endif
!
! --------- SINON (PAS DE CHANGEMENT DE CONFIGURATION A L'INSTANT N)
! --------- => DT = DT/2
            else
                dt = dt/2.0d0
            endif
!
            tc = tc + dt
            itest = 0
            niter = niter + 1
!
! --------- ON RETOURNE A L'ESTIMATION D'EULER ESTIVD
            goto 10
!
        endif
!
! 12.2 SI UNE VARIATION IMPORTANTE DU DEPLACEMENT PHYSIQUE SE PRODUIT
!      ENTRE LES INSTANTS N ET N+1, ON RECALCULE LA CONFIGURATION
!      A L'INSTANT N+1 EN RAFFINANT LE PAS DE TEMPS (DT = DT/2)
!
    else if (iconf.eq.-1) then
!
        tc = tc - dt
        dt = dt/2.0d0
        tc = tc + dt
        itest = 0
        niter = niter + 1
!
! ------ ON RETOURNE A L'ESTIMATION D'EULER ESTIVD
        goto 10
!
! 12.3 S'IL N'Y A PAS DE CHANGEMENT DE CONFIGURATION ENTRE LES
!      INSTANTS N ET N+1, VALIDATION DE L'INSTANT N+1
!      (ICONF = 1 DANS CE DERNIER CAS)
!
    else
!
! DEBUG
        do 55 ic = 1, nbnl
            if ((abs(tconfe(4,ic)).gt.tolch) .and. (abs(tconf2(4,ic)) .gt.tolch) .and.&
                (tconfe(4,ic)*tconf2(4,ic).lt.0.0d0)) then
                tc = tc - dt
                dt = dt/2.0d0
                tc = tc + dt
                itest = 0
                niter = niter + 1
!
! ------------ ON RETOURNE A L'ESTIMATION D'EULER ESTIVD
                goto 10
            endif
55      continue
! DEBUG
        dt0 = dt
!
! 12.3.1 SI ITEST = 0
!        ON A RAFFINE LE PAS DE TEMPS LORS D'UNE ITERATION PRECEDENTE :
!        - POUR DETERMINER L'INSTANT N : RAFFINEMENT DU PAS DE TEMPS
!          APRES L'ESTIMATION D'EULER (3.1 OU 3.2) CAR VARIATION
!          IMPORTANTE DU DEPLACEMENT PHYSIQUE
!          NOUVELLE ESTIMATION D'EULER PUIS APPLICATION DU SCHEMA ITMI
!          PAS DE CHANGEMENT DE CONFIGURATION NI DE VARIATION IMPORTANTE
!          DU DEPLACEMENT PHYSIQUE APRES APPLICATION DU SCHEMA ITMI
!          PASSAGE EN 11.3.2 AVEC IDECRT = 1 => ITEST = 0
!        - POUR DETERMINER L'INSTANT N+1 : RAFFINEMENT DU PAS DE TEMPS
!          APRES APPLICATION DU SCHEMA ITMI
!          CHANGEMENT DE CONFIGURATION PAS ASSEZ PROCHE (11.1.2) OU
!          VARIATION IMPORTANTE DU DEPLACEMENT PHYSIQUE (11.2)
!        ON NE DETECTE PAS DE CHANGEMENT DE CONFIGURATION A L'ITERATION
!        COURANTE, NI DE VARIATION IMPORTANTE DU DEPLACEMENT PHYSIQUE
!        ON CONSERVE LE PAS DE TEMPS RAFFINE POUR L'ITERATION SUIVANTE
!        AFIN DE SE RAPPROCHER DU CHANGEMENT DE CONFIGURATION OU DE NE
!        PAS CAUSER DE VARIATION IMPORTANTE DU DEPLACEMENT PHYSIQUE
!
        if (itest .eq. 0) then
!
! ......... SI ON SE RETROUVE EN 11.3 A L'ISSUE DE L'ITERATION SUIVANTE,
! ......... IL FAUDRA CALCULER UN NOUVEAU PAS DE TEMPS
            itest = 1
!
! 12.3.2 SINON (ITEST = 1)
!
        else
!
! --------- SI ON A RAFFINE LE PAS DE TEMPS POUR L'ESTIMATION D'EULER,
! --------- ON CONSERVE LE PAS DE TEMPS RAFFINE
            if (idecrt .eq. 1) then
                itest = 0
!
! --------- SINON ON CALCULE LE NOUVEAU PAS DE TEMPS
            else
                omega = 0.0d0
                do 60 i = 1, nbmcd
                    if (puls(i) .gt. omega) omega = puls(i)
60              continue
                dt = 1.0d0/(div*omega*15.0d0)
                itest = 1
            endif
        endif
!
        tc = tc + dt
!
    endif
!
! --- FIN DE ALITMI.
end subroutine

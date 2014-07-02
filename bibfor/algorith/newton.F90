subroutine newton(np1, np2, np3, np4, nbm,&
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
                  tconf2, toln, tolc, tolv, testc,&
                  itforn)
!
! aslint: disable=,W1504
    implicit none
!-----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
! DESCRIPTION : PROCEDURE D'ATTEINTE DE L'INSTANT DE CHANGEMENT DE
! -----------   CONFIGURATION PAR UNE METHODE DE NEWTON.
!
!               APPELANT : ALITMI
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
#include "asterf_types.h"
#include "asterfort/adimve.h"
#include "asterfort/calcmd.h"
#include "asterfort/calcmi.h"
#include "asterfort/calfmn.h"
#include "asterfort/calfnl.h"
#include "asterfort/calnd1.h"
#include "asterfort/calnd2.h"
#include "asterfort/coupla.h"
#include "asterfort/dtcycl.h"
#include "asterfort/estivd.h"
#include "asterfort/inipan.h"
#include "asterfort/projmd.h"
#include "asterfort/projmg.h"
#include "asterfort/projvd.h"
#include "asterfort/sommma.h"
#include "asterfort/testch.h"
#include "asterfort/utmess.h"
#include "asterfort/vecini.h"
    integer :: np1, np2, np3, np4, nbm, n2, nbmcd, icoupl
    real(kind=8) :: tc, dt, dtc, vecdt(*)
    integer :: nbnl, typch(*), nbseg(*)
    real(kind=8) :: phii(np2, np1, *), choc(6, *), alpha(2, *), beta(2, *)
    real(kind=8) :: gamma(2, *), orig(6, *), rc(np3, *), theta(np3, *), vgap
    real(kind=8) :: vecr4(*)
    integer :: indic
    character(len=8) :: tpfl
    integer :: veci1(*)
    real(kind=8) :: vecr1(*), vecr2(*), vecr5(*), vecr3(*), masgi(*), amori(*)
    real(kind=8) :: pulsi(*), amor(*), amor0(*), puls(*), puls0(*), xsi0(*)
    real(kind=8) :: vitg(*), depg(*), accg0(*), vitg0(*), depg0(*), vitgc(*)
    real(kind=8) :: depgc(*), vitgt(*), depgt(*), cmod(np1, *), kmod(np1, *)
    real(kind=8) :: cmod0(np1, *), kmod0(np1, *), cmodca(np1, *), kmodca(np1, *)
    real(kind=8) :: amflu0(np1, *), amfluc(np1, *)
    aster_logical :: locflc(*)
    real(kind=8) :: cmodfa(np1, *)
    integer :: npfts
    real(kind=8) :: textts(*), fextts(np4, *)
    integer :: ndef, indt
    real(kind=8) :: fexmod(*), fnlmod(*), fmoda(*), fmres(*), fmod0(*)
    real(kind=8) :: fmod00(*), fmodt(*), fmod0t(*), vitg0t(*), depg0t(*)
    real(kind=8) :: ftmp(*), mtmp1(np1, *), mtmp2(np1, *), mtmp6(3, *)
    real(kind=8) :: ttr(n2, *), u(*), w(*), dd(*)
    aster_logical :: loc(*)
    integer :: intge1(*), intge2(*), indx(*), indxf(*)
    real(kind=8) :: vvg(np1, *), vg(np1, *), vg0(np1, *), vd(np1, *)
    real(kind=8) :: vd0(np1, *), rr(*), rr0(*), ri(*), premac, prerel
    real(kind=8) :: trans(2, 2, *), pulsd(*)
    complex(kind=8) :: s0(*), z0(*), sr0(*), za1(*), za2(*), za3(*), zin(*)
    real(kind=8) :: old(9, *)
    integer :: oldia(*), iconfe, iconfa, nbcha, nbchea
    real(kind=8) :: ftest
    integer :: iconfb(*)
    real(kind=8) :: tconf1(4, *), tconf2(4, *), toln, tolc, tolv
    integer :: testc, itforn(*)
!
! VARIABLES LOCALES
! -----------------
    integer :: i, ic, icompt, ier, inewto, nitern, nitnew, nitmax, testc0, typj
    real(kind=8) :: absdtc, ddist2, dist2, dt0, dtc0, maxvit, tdtc, tetaes
    character(len=10) :: kb10
    character(len=3) :: kb3
    character(len=24) :: valk(2)
! DEBUG
    integer :: icycl, idt, ilast, itback
    real(kind=8) :: dt00, dtmin, epsdtc, tdt, scalv, vglo(3), vglo0(3)
! DEBUG
!
! FONCTIONS INTRINSEQUES
! ----------------------
!     INTRINSIC    ABS, DBLE
!
! ROUTINES EXTERNES
! -----------------
!     EXTERNAL     ADIMVE, CALCMD, CALCMI, CALFMN, CALFNL, CALND1,
!    &             CALND2, COUPLA, ESTIVD, INIPAN, PRBRED, PROJMD,
!    &             PROJVD, SOMMMA, TESTCH
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    testc0 = testc
    nitern = 1
    nitnew = 0
    nitmax = 150
    inewto = 1
    tetaes = 1.0d0
! DEBUG
    itback = 0
    ilast = 0
    icycl = 0
    epsdtc = 1.0d-04
    dt00 = dt
    call vecini(nitmax+1, 0.d0, vecdt)
! DEBUG
!
!-----------------------------------------------------------------------
! 1.  PREMIER AJUSTEMENT DU PAS DE TEMPS ET DE L'INSTANT N+1
!-----------------------------------------------------------------------
! 1.1 LA DERNIERE SOLUTION DETERMINEE PAR ALITMI A ETE CALCULEE SUR
!     LA BASE MODALE DE LA CONFIGURATION A L'INSTANT N
!
    if ((iconfe.eq.1) .and. (iconfa.eq.0)) then
!
! ------ LE CHANGEMENT DE CONFIGURATION EST DEJA ATTEINT
!
        if (nbcha .eq. nbchea) then
            dtc = 0.0d0
            goto 999
        endif
!
! ------ ON SE PLACE A L'INSTANT N+1
! ------ ON DETERMINE L'INCREMENT TEMPOREL NEGATIF DE PLUS GRANDE VALEUR
! ------ ABSOLUE AFIN DE REVENIR AU CHANGEMENT D'ETAT LE PLUS PROCHE DE
! ------ L'INSTANT N. ON NE CONSIDERE QUE LES BUTEES EN LESQUELLES UN
! ------ CHANGEMENT D'ETAT A ETE DETECTE.
!
        dt0 = dt
        tc = tc - dt0
        icompt = 0
        dtc = 0.0d0
        absdtc = 0.0d0
        do 10 ic = 1, nbnl
            if (iconfb(ic) .eq. 0) then
                icompt = icompt + 1
                call calnd1(ic, np1, np2, np3, nbm,&
                            alpha, beta, gamma, orig, rc,&
                            theta, typch, nbseg, phii, depg,&
                            dist2)
                call calnd2(ic, np1, np2, np3, nbm,&
                            typch, nbseg, alpha, beta, gamma,&
                            orig, rc, theta, phii, depg,&
                            vitg, ddist2)
                tdtc = -dist2 / ddist2
                if ((tdtc.lt.0.0d0) .and. (abs(tdtc).gt.absdtc)) then
                    dtc = tdtc
                    absdtc = abs(tdtc)
                endif
            endif
            if (icompt .eq. nbcha) goto 11
 10     continue
 11     continue
! ...... SI DTC = 0.0D0 ON EFFECTUE UN DEMI-TOUR ENTRE LES INSTANTS N
! ...... ET N+1 => DTC = -DT0/2.0D0
        if (dtc .eq. 0.0d0) then
            dtc = -dt0/2.0d0
        endif
!
        if ((dt0+dtc) .lt. 0.0d0) dtc = -dt0/2.0d0
        dtc0 = dtc
        dt = dt0 + dtc
        tc = tc + dt
!
! 1.2 LA DERNIERE SOLUTION DETERMINEE PAR ALITMI A ETE CALCULEE SUR
!     LA BASE MODALE DE LA CONFIGURATION A L'INSTANT N+1
!
    else
!
! ------ ON SE PLACE A L'INSTANT N
! ------ ON DETERMINE L'INCREMENT TEMPOREL POSITIF DE PLUS PETITE VALEUR
! ------ ABSOLUE AFIN D'ATTEINDRE LE CHANGEMENT D'ETAT LE PLUS PROCHE DE
! ------ L'INSTANT N. ON CONSIDERE TOUTES LES BUTEES.
!
        dt0 = dt
        tc = tc - dt0
        dt = 1.0d+10
        do 20 ic = 1, nbnl
            call calnd1(ic, np1, np2, np3, nbm,&
                        alpha, beta, gamma, orig, rc,&
                        theta, typch, nbseg, phii, depg0,&
                        dist2)
            call calnd2(ic, np1, np2, np3, nbm,&
                        typch, nbseg, alpha, beta, gamma,&
                        orig, rc, theta, phii, depg0,&
                        vitg0, ddist2)
            tdt = -dist2 / ddist2
            if ((tdt.gt.0.0d0) .and. (tdt.lt.dt)) dt = tdt
 20     continue
! DEBUG
! ------ ON VALIDE L'INSTANT N+1 EN CAS D'ELOIGNEMENT DU CHANGEMENT DE
! ------ CONFIGURATION
        if (dt .eq. 1.0d+10) then
!   PREMIER AJUSTEMENT DU PAS DE TEMPS,
!   CAS 2 : INCREMENT TEMPOREL INDETERMINE
            dt = dt0
            dtc = 0.0d0
            tc = tc + dt0
            goto 999
        endif
! DEBUG
!
        dtc = dt - dt0
        dtc0 = dtc
        tc = tc + dt
!
    endif
    vecdt(1) = dt
!
!-----------------------------------------------------------------------
! 2.  PREMIERE ESTIMATION DES DDLS GENERALISES A L'INSTANT N+1
!-----------------------------------------------------------------------
! 2.1 DETERMINATION DE LA COMPOSANTE DE PLUS GRANDE VALEUR ABSOLUE
!     DU VECTEUR VITESSES GENERALISEES A L'INSTANT N
!
    maxvit = 0.0d0
    do 30 i = 1, nbm
        if (abs(vitg0(i)) .gt. maxvit) maxvit = abs(vitg0(i))
 30 end do
!
! 2.2 ESTIMATION DES DDLS GENERALISES A L'INSTANT N+1 PAR LE SCHEMA
!     D'EULER AFIN DE DETERMINER UNE POSITION AUTOUR DE LAQUELLE
!     ON VA LINEARISER LES FORCES DE CHOC
!     INEWTO = 1 INDIQUE A ESTIVD QUE LA ROUTINE APPELANTE EST NEWTON
!
    call estivd(nbm, dt, vitgc, depgc, accg0,&
                vitg0, depg0, tetaes, maxvit, inewto)
!
!-----------------------------------------------------------------------
! 3.  CALCUL DES DDLS GENERALISES A L'INSTANT N+1 PAR LE SCHEMA ITMI
!     JUSQU'A VALIDATION DE L'INSTANT N+1 (BLOC REPETER)
!-----------------------------------------------------------------------
100 continue
    call vecini(nbm, 0.d0, depg)
    call vecini(nbm, 0.d0, vitg)
!
! 3.1 ESTIMATION DE LA FORCE NON-LINEAIRE A L'INSTANT N+1
!     INEWTO = 1 INDIQUE A MDCHOE QUE LA ROUTINE APPELANTE EST NEWTON,
!     DONC LE CALCUL EST REALISE DANS LA CONFIGURATION A L'INSTANT N
!
    call calfnl(np1, np2, np3, np4, nbm,&
                nbm, npfts, tc, nbnl, typch,&
                nbseg, phii, choc, alpha, beta,&
                gamma, orig, rc, theta, masgi,&
                amori, pulsi, vitgc, depgc, vitg0,&
                depg0, cmod, kmod, cmodca, kmodca,&
                textts, fextts, ndef, indt, nitern,&
                fexmod, fnlmod, fmres, fmoda, ftmp,&
                mtmp1, mtmp6, old, oldia, testc,&
                itforn, inewto, toln)
!
!
! 3.2 DIAGONALISATION DE LA MATRICE DE RAIDEUR
!     (CONFIGURATION A L'INSTANT N)
!
    ier = 0
    call calcmd(np1, kmodca, kmod0, nbm, nbmcd,&
                typj, vvg, vg, vg0, vd,&
                vd0, rr, rr0, ri, n2,&
                ier, testc, premac, prerel, mtmp1,&
                mtmp2, ttr, u, w, dd,&
                intge1, intge2, indx, indxf, loc)
    if (ier .ne. 0) then
        call utmess('F', 'ALGORITH_10')
    endif
    do 110 i = 1, nbm
        puls(i) = rr(i)
110 end do
    do 120 i = 1, nbm
        if ((puls(i).eq.0.0d0) .and. (i.le.nbmcd)) then
            puls(i) = puls0(i)
            call utmess('I', 'ALGORITH_11')
        endif
120 end do
!
    if ((testc0.eq.1) .and. (testc.eq.0)) typj = 1
!
! 3.3 CALCUL DES FORCES DE COUPLAGE (CONFIGURATION A L'INSTANT N)
!
    if ((testc.eq.1) .and. (icoupl.eq.1)) then
        call coupla(np1, nbm, indic, tpfl, veci1,&
                    vgap, vecr4, vecr1(1), vecr2, vecr5,&
                    vecr3, masgi, puls, locflc, amflu0,&
                    amfluc, xsi0)
        call sommma(np1, nbm, nbm, amfluc, cmodca,&
                    cmodfa)
    endif
!
! 3.4 CALCUL DES EXCITATIONS GENERALISEES A L'INSTANT N
!
    call calfmn(np1, nbm, testc0, fmod0, fmod00,&
                cmod, kmod, vitg0, depg0)
    call adimve(nbm, fmod0, masgi)
!
! 3.5 PROJECTIONS SUR LA BASE MODALE (CONFIGURATION A L'INSTANT N)
!
    if ((testc.eq.1) .and. (icoupl.eq.1)) then
        call projmd(testc, np1, nbm, nbmcd, cmodfa,&
                    vg, vd, amor, mtmp1, mtmp2)
    else
        call projmd(testc, np1, nbm, nbmcd, cmodca,&
                    vg, vd, amor, mtmp1, mtmp2)
    endif
!
    call projvd(testc, np1, nbm, nbm, vg,&
                fmod0, fmod0t)
    call projvd(testc, np1, nbm, nbm, vg,&
                fmoda, fmodt)
    call projvd(testc, np1, nbm, nbm, vg,&
                depg0, depg0t)
    call projvd(testc, np1, nbm, nbm, vg,&
                vitg0, vitg0t)
!
! 3.6 CALCUL DES DDLS GENERALISES A L'INSTANT N+1 PAR LE SCHEMA ITMI
!     PUIS RETOUR SUR LA BASE EN VOL
!
    call calcmi(np1, nbmcd, dt0, dt, vitgt,&
                depgt, vitg0t, depg0t, fmodt, fmod0t,&
                amor, amor0, puls, puls0, trans,&
                pulsd, s0, z0, sr0, za1,&
                za2, za3, zin)
    call projvd(testc, np1, nbm, nbmcd, vd,&
                depgt, depg)
    call projvd(testc, np1, nbm, nbmcd, vd,&
                vitgt, vitg)
!
! 3.7 TEST DE CHANGEMENT DE CONFIGURATION ENTRE LES INSTANTS N ET N+1
!     AVEC LA SOLUTION DU SCHEMA ITMI
!
    if (testc .eq. 1) then
        call testch(np1, np2, np3, nbm, nbnl,&
                    toln, tolc, tolv, typch, nbseg,&
                    phii, alpha, beta, gamma, orig,&
                    rc, theta, tconf1, depg, nbcha,&
                    nbchea, iconfa, ftest, iconfb, tconf2)
    else if (testc.eq.0) then
        call testch(np1, np2, np3, nbmcd, nbnl,&
                    toln, tolc, tolv, typch, nbseg,&
                    phii, alpha, beta, gamma, orig,&
                    rc, theta, tconf1, depg, nbcha,&
                    nbchea, iconfa, ftest, iconfb, tconf2)
    endif
    if (ilast .eq. 1) then
        if (iconfa .eq. 1) goto 999
        call utmess('F', 'ALGORITH6_52')
    endif
!
! 3.8 AJUSTEMENT DU PAS DE TEMPS ET DE L'INSTANT N+1 SI LE CHANGEMENT
!     DE CONFIGURATION N'EST PAS ENCORE ATTEINT OU DEJA DEPASSE
!     DANS LE CAS CONTRAIRE, L'INSTANT N+1 EST VALIDE ET ON RETOURNE
!     A L'APPELANT ALITMI
!
    nitnew = nitnew + 1
!
!*****CAS 1 : VARIATION IMPORTANTE DU DEPLACEMENT PHYSIQUE D'UN NOEUD
!             DE CHOC AU MOINS ENTRE LES INSTANTS N ET N+1
!
    if (iconfa .eq. -1) then
        if (nitnew .gt. nitmax) then
            write(kb3,'(I3)') nitmax
            write(kb10,'(1PD11.4)') tc
            valk(1) = kb3
            valk(2) = kb10
            call utmess('F', 'ALGORITH6_53', nk=2, valk=valk)
        endif
!
! ------ AJUSTEMENT DU PAS DE TEMPS ET DE L'INSTANT N+1
        dt0 = dt
        dtc0 = dtc
        tc = tc - dt0
        dtc = -dt0/2.0d0
! ...... SI DTC = -DTC0 (VALEUR A L'ITERATION PRECEDENTE)
! ...... ON DIVISE DTC PAR 2.0D0
! DEBUG
        if (abs((dtc0+dtc)/dtc) .lt. 1.0d-02) then
            dtc = dtc/2.0d0
        endif
        dt = dt0 + dtc
!....... DETECTION D'EVENTUELLES ITERATIONS CYCLIQUES
        vecdt(nitnew+1) = dt
        call dtcycl(vecdt, nitnew, icycl, dtmin)
        if (icycl .eq. 1) then
!.......... ON FIXE DT A DTMIN SUR LE CYCLE POUR EFFECTUER UNE
!.......... DERNIERE ITERATION AVANT DE RETOURNER A ALITMI.
            ilast = 1
            dt = dtmin
            dtc = dtmin - dt00
            tc = tc + dtmin
        else
            tc = tc + dt
        endif
! DEBUG
!
! ------ ESTIMATION DES DDLS GENERALISES A L'INSTANT N+1 PAR LE SCHEMA
! ------ D'EULER AFIN DE DETERMINER UNE POSITION AUTOUR DE LAQUELLE
! ------ ON VA LINEARISER LES FORCES DE CHOC
        call estivd(nbm, dt, vitgc, depgc, accg0,&
                    vitg0, depg0, tetaes, maxvit, inewto)
!
! ------ INITIALISATION DES PARAMETRES POUR L'ITERATION SUIVANTE
        call inipan(np1, nbm, cmod0, kmod0, cmodca,&
                    kmodca, amor0, puls0, amor, puls,&
                    fnlmod, fexmod, fmod00)
!
! ------ RETOURNER A REPETER
        goto 100
!
!*****CAS 2 : AUCUN CHANGEMENT D'ETAT N'EST ENCORE ATTEINT
!
    else if (iconfa.eq.1) then
        if (nitnew .gt. nitmax) then
            write(kb3,'(I3)') nitmax
            write(kb10,'(1PD11.4)') tc
            valk(1) = kb3
            valk(2) = kb10
            call utmess('F', 'ALGORITH6_53', nk=2, valk=valk)
        endif
!
! ------ ON SE PLACE A L'INSTANT N+1 SI LA VITESSE NE CHANGE PAS DE SENS
! ------ OU A L'INSTANT N DANS LE CAS CONTRAIRE.
! ------ ON DETERMINE L'INCREMENT TEMPOREL POSITIF DE PLUS PETITE VALEUR
! ------ ABSOLUE AFIN D'ATTEINDRE LE CHANGEMENT D'ETAT LE PLUS PROCHE DE
! ------ L'INSTANT N. ON CONSIDERE TOUTES LES BUTEES.
!
! DEBUG
        dt0 = dt
        dtc0 = dtc
        tc = tc - dt0
        dt = 1.0d+10
        dtc = 0.0d0
        do 140 ic = 1, nbnl
            call projmg(np1, np2, ic, nbm, phii,&
                        vitg0, vglo0)
            call projmg(np1, np2, ic, nbm, phii,&
                        vitg, vglo)
            scalv = vglo0(1) * vglo(1) + vglo0(2) * vglo(2) + vglo0(3) * vglo(3)
            if (scalv .gt. 0.0d0) then
                call calnd1(ic, np1, np2, np3, nbm,&
                            alpha, beta, gamma, orig, rc,&
                            theta, typch, nbseg, phii, depg,&
                            dist2)
                call calnd2(ic, np1, np2, np3, nbm,&
                            typch, nbseg, alpha, beta, gamma,&
                            orig, rc, theta, phii, depg,&
                            vitg, ddist2)
                tdtc = -dist2 / ddist2
                if ((tdtc.gt.0.0d0) .and. ((dt0+tdtc).lt.dt)) then
                    dt = dt0 + tdtc
                    dtc = tdtc
                    idt = 1
                endif
            else
                call calnd1(ic, np1, np2, np3, nbm,&
                            alpha, beta, gamma, orig, rc,&
                            theta, typch, nbseg, phii, depg0,&
                            dist2)
                call calnd2(ic, np1, np2, np3, nbm,&
                            typch, nbseg, alpha, beta, gamma,&
                            orig, rc, theta, phii, depg0,&
                            vitg0, ddist2)
                tdt = -dist2 / ddist2
                if ((tdt.gt.0.0d0) .and. (tdt.lt.dt)) then
                    dt = tdt
                    dtc = dt - dt0
                    idt = 0
                endif
! POUR ECRITURE
                tdtc = tdt - dt0
! POUR ECRITURE
            endif
140     continue
!
! ------ ON VALIDE L'INSTANT N+1 EN CAS D'ELOIGNEMENT DU CHANGEMENT DE
! ------ CONFIGURATION
        if (dt .eq. 1.0d+10) then
!     AJUSTEMENT DU PAS DE TEMPS,
!     CAS 2 : INCREMENT TEMPOREL INDETERMINE
            dt = dt0
            dtc = dtc0
            tc = tc + dt0
            goto 999
        endif
!
! ------ DEMI-TOUR SANS CHANGEMENT DE CONFIGURATION ENTRE LES
! ------ INSTANTS N ET N+1
        if ((abs(dtc)/dt0) .lt. epsdtc) then
            if (idt .eq. 0) itback = itback + 1
! ......... AU PREMIER PASSAGE ON DIVISE LE PAS DE TEMPS PAR 2.0D0
            if (itback .lt. 2) then
                dtc = -dt0/2.0d0
                dt = dt0 + dtc
! ......... AU PASSAGE SUIVANT ON CONCLUT AU DEMI-TOUR SANS CHANGEMENT
! ......... DE CONFIGURATION. ON FIXE DT A VECDT(1) POUR EFFECTUER UNE
! ......... DERNIERE ITERATION AVANT DE RETOURNER A L'APPELANT ALITMI.
            else
                ilast = 1
                dt = vecdt(1)
                dtc = dt - dt00
                tc = tc + dt
            endif
        endif
!
! ------ AJUSTEMENT DU PAS DE TEMPS ET DE L'INSTANT N+1
        if (ilast .eq. 0) then
! ......... SI DTC = -DTC0 (VALEUR A L'ITERATION PRECEDENTE)
! ......... ON DIVISE DTC PAR 2.0D0
            if (abs((dtc0+dtc)/dtc) .lt. 1.0d-02) then
                dtc = dtc/2.0d0
                dt = dt0 + dtc
            endif
!.......... DETECTION D'EVENTUELLES ITERATIONS CYCLIQUES
            vecdt(nitnew+1) = dt
            call dtcycl(vecdt, nitnew, icycl, dtmin)
            if (icycl .eq. 1) then
!............. ON FIXE DT A DTMIN SUR LE CYCLE POUR EFFECTUER UNE
!............. DERNIERE ITERATION AVANT DE RETOURNER A ALITMI.
                ilast = 1
                dt = dtmin
                dtc = dtmin - dt00
                tc = tc + dtmin
            else
                tc = tc + dt
            endif
        endif
! DEBUG
!
! ------ ESTIMATION DES DDLS GENERALISES A L'INSTANT N+1 PAR LE SCHEMA
! ------ D'EULER AFIN DE DETERMINER UNE POSITION AUTOUR DE LAQUELLE
! ------ ON VA LINEARISER LES FORCES DE CHOC
        call estivd(nbm, dt, vitgc, depgc, accg0,&
                    vitg0, depg0, tetaes, maxvit, inewto)
!
! ------ INITIALISATION DES PARAMETRES POUR L'ITERATION SUIVANTE
        call inipan(np1, nbm, cmod0, kmod0, cmodca,&
                    kmodca, amor0, puls0, amor, puls,&
                    fnlmod, fexmod, fmod00)
!
! ------ RETOURNER A REPETER
        goto 100
!
!*****CAS 3 : AU MOINS UN CHANGEMENT D'ETAT EST DEPASSE
!
    else if (nbcha.gt.nbchea) then
        if (nitnew .gt. nitmax) then
            write(kb3,'(I3)') nitmax
            write(kb10,'(1PD11.4)') tc
            valk(1) = kb3
            valk(2) = kb10
            call utmess('F', 'ALGORITH6_53', nk=2, valk=valk)
        endif
!
! ------ ON SE PLACE A L'INSTANT N+1
! ------ ON DETERMINE L'INCREMENT TEMPOREL NEGATIF DE PLUS GRANDE VALEUR
! ------ ABSOLUE AFIN DE REVENIR AU CHANGEMENT D'ETAT LE PLUS PROCHE DE
! ------ L'INSTANT N. ON NE CONSIDERE QUE LES BUTEES EN LESQUELLES UN
! ------ CHANGEMENT D'ETAT A ETE DETECTE.
!
        dt0 = dt
        dtc0 = dtc
        tc = tc - dt0
        icompt = 0
        dtc = 0.0d0
        absdtc = 0.0d0
        do 150 ic = 1, nbnl
            if (iconfb(ic) .eq. 0) then
                icompt = icompt + 1
                call calnd1(ic, np1, np2, np3, nbm,&
                            alpha, beta, gamma, orig, rc,&
                            theta, typch, nbseg, phii, depg,&
                            dist2)
                call calnd2(ic, np1, np2, np3, nbm,&
                            typch, nbseg, alpha, beta, gamma,&
                            orig, rc, theta, phii, depg,&
                            vitg, ddist2)
                tdtc = -dist2 / ddist2
                if ((tdtc.lt.0.0d0) .and. (abs(tdtc).gt.absdtc)) then
                    dtc = tdtc
                    absdtc = abs(tdtc)
                endif
            endif
            if (icompt .eq. nbcha) goto 151
150     continue
151     continue
! DEBUG
! ------ SI DTC = 0.0D0 ON EFFECTUE UN DEMI-TOUR ENTRE LES INSTANTS N
! ------ ET N+1 => DTC = -DT0/2.0D0
        if (dtc .eq. 0.0d0) then
            dtc = -dt0/2.0d0
        endif
!
! ------ AJUSTEMENT DU PAS DE TEMPS ET DE L'INSTANT N+1
        if ((dt0+dtc) .lt. 0.0d0) dtc = -dt0/2.0d0
! ...... SI DTC = -DTC0 (VALEUR A L'ITERATION PRECEDENTE)
! ...... ON DIVISE DTC PAR 2.0D0
        if (abs((dtc0+dtc)/dtc) .lt. 1.0d-02) then
            dtc = dtc/2.0d0
        endif
        dt = dt0 + dtc
!....... DETECTION D'EVENTUELLES ITERATIONS CYCLIQUES
        vecdt(nitnew+1) = dt
        call dtcycl(vecdt, nitnew, icycl, dtmin)
        if (icycl .eq. 1) then
!.......... ON FIXE DT A DTMIN SUR LE CYCLE POUR EFFECTUER UNE
!.......... DERNIERE ITERATION AVANT DE RETOURNER A ALITMI.
            ilast = 1
            dt = dtmin
            dtc = dtmin - dt00
            tc = tc + dtmin
        else
            tc = tc + dt
        endif
! DEBUG
!
! ------ ESTIMATION DES DDLS GENERALISES A L'INSTANT N+1 PAR LE SCHEMA
! ------ D'EULER AFIN DE DETERMINER UNE POSITION AUTOUR DE LAQUELLE
! ------ ON VA LINEARISER LES FORCES DE CHOC
        call estivd(nbm, dt, vitgc, depgc, accg0,&
                    vitg0, depg0, tetaes, maxvit, inewto)
!
! ------ INITIALISATION DES PARAMETRES POUR L'ITERATION SUIVANTE
        call inipan(np1, nbm, cmod0, kmod0, cmodca,&
                    kmodca, amor0, puls0, amor, puls,&
                    fnlmod, fexmod, fmod00)
!
! ------ RETOURNER A REPETER
        goto 100
!
    endif
999 continue
!
! --- FIN DE NEWTON.
end subroutine

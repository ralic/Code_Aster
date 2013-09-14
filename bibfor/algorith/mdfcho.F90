subroutine mdfcho(nbmode, depgen, vitgen, accgen, fexgen,&
                  masgen, phicar, pulsa2, amogen, nbchoc,&
                  logcho, dplmod, parcho, noecho, saucho,&
                  temps, nofdep, nofvit, nofacc, nbexci,&
                  psidel, nonmot)
! aslint: disable=W1306,W1501,W1504
    implicit none
#include "asterfort/distno.h"
#include "asterfort/fnorm.h"
#include "asterfort/fointe.h"
#include "asterfort/ftang.h"
#include "asterfort/gloloc.h"
#include "asterfort/locglo.h"
#include "asterfort/mdfdas.h"
#include "asterfort/mdfflu.h"
#include "asterfort/mdflam.h"
#include "asterfort/mdmasf.h"
#include "asterfort/ponder.h"
#include "asterfort/togene.h"
#include "asterfort/tophy3.h"
#include "asterfort/tophys.h"
    integer :: logcho(nbchoc, *)
    real(kind=8) :: depgen(*), vitgen(*), fexgen(*), accgen(*)
    real(kind=8) :: parcho(nbchoc, *), saucho(nbchoc, *), masgen(*), phicar(*)
    real(kind=8) :: pulsa2(*), amogen(*)
    real(kind=8) :: dplmod(nbchoc, nbmode, *)
    character(len=8) :: noecho(nbchoc, *)
    character(len=8) :: nonmot
    integer :: nbexci
    character(len=8) :: nofdep(nbexci), nofvit(nbexci), nofacc(nbexci)
    real(kind=8) :: temps, psidel(nbchoc, nbexci, *)
! ----------------------------------------------------------------------
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
!
!     CALCUL LES FORCES DE CHOC DE LA STRUCTURE
!     ------------------------------------------------------------------
! IN  : NBMODE : NOMBRE DE MODES
! IN  : DEPGEN : DEPLACEMENTS GENERALISES
! IN  : VITGEN : VITESSES GENERALISEES
! IN  : ACCGEN : ACCELERATIONS GENERALISEES
! VAR : FEXGEN : FORCES GENERALISEES
! VAR : MASGEN : MASSES GENERALISEES (VAR SI LAME FLUIDE)
! VAR : PULSA2 : CARRES DES PULSATIONS (VAR SI LAME FLUIDE)
! VAR : AMOGEN : AMORTISSEMENT GENERALISES
! IN  : NBCHOC : NOMBRE DE NOEUDS DE CHOC
! VAR : LOGCHO : INDICATEUR D'ADHERENCE ET DE FORCE FLUIDE ET DE
!                PRESENCE D UN DISPOSITIF ANTI SISMIQUE
! IN  : DPLMOD : TABLEAU DES DEPL MODAUX AUX NOEUDS DE CHOC
! IN  : PARCHO : PARAMETRES DES NOEUDS DE CHOC
! IN  : NOECHO : NOM DES NOEUDS DE CHOC ET TYPE D'OBSTACLE
! OUT : SAUCHO : SAUVEGARDE DES VALEURS DE CHOC
!
! IN  : TEMPS  : INSTANT DE CALCUL DES DEPL_IMPO
! IN  : NOFDEP : NOM DE LA FONCTION DEPL_IMPO
! IN  : NOFVIT : NOM DE LA FONCTION VITE_IMPO
! IN  : NOFACC : NOM DE LA FONCTION ACCE_IMPO
! IN  : NBEXCI : NOMBRE D'ACCELERO DIFFERENTS
! IN  : PSIDEL : TABLEAU DE VALEURS DE PSI*DELTA
! IN  : NONMOT : = OUI SI MULTI-APPUIS
! ----------------------------------------------------------------------
    real(kind=8) :: knorm, ktang, deploc(6), depglo(6), flocal(3), fgloba(3)
    real(kind=8) :: vitglo(6), vitloc(6), orig(3), origob(3), accglo(6)
    real(kind=8) :: accloc(6), ftange(2), vtang(2), ddeplo(3), oldft(2)
    real(kind=8) :: oldxl(3), oldvt(2), pond, signe(2), fdispo
!     ------------------------------------------------------------------
    integer :: iex, i, j
    character(len=8) :: nompar
    real(kind=8) :: coedep(nbexci), coevit(nbexci), coeacc(nbexci)
!
!-----------------------------------------------------------------------
    integer :: ier, nbchoc, nbmode
    real(kind=8) :: anorm, ax1, ax2, ay1, ay2, az1, az2
    real(kind=8) :: cfrotd, cfrots, cl, cnorm, coefa, coefad, coefb
    real(kind=8) :: coefc, coefcc, coefd, coefk1, coefk2, coefpy, cosa
    real(kind=8) :: cosb, cosg, cost, ctang, defpla, dist1, dist2
    real(kind=8) :: dnorm, ffluid, flim, fn, fseuil, rigifl, sina
    real(kind=8) :: sinb, sing, sint, ux1, ux2, uy1, uy2
    real(kind=8) :: uz1, uz2, vnorm, vx1, vx2, vy1, vy2
    real(kind=8) :: vz1, vz2, xjeu, xmax, zero
!-----------------------------------------------------------------------
    zero = 0.d0
    orig(1) = zero
    orig(2) = zero
    orig(3) = zero
!
    nompar = 'INST'
    if (nonmot(1:3) .ne. 'NON') then
        do 11 iex = 1, nbexci
            coedep(iex) = zero
            coevit(iex) = zero
            coeacc(iex) = zero
            if (nofdep(iex) .ne. ' ') then
                call fointe('F', nofdep(iex), 1, [nompar], [temps],&
                            coedep(iex), ier)
            endif
            if (nofvit(iex) .ne. ' ') then
                call fointe('F', nofvit(iex), 1, [nompar], [temps],&
                            coevit(iex), ier)
            endif
            if (nofacc(iex) .ne. ' ') then
                call fointe('F', nofacc(iex), 1, [nompar], [temps],&
                            coeacc(iex), ier)
            endif
11      continue
    endif
!
!
    do 10 i = 1, nbchoc
!
        fn = zero
        ftange(1) = zero
        ftange(2) = zero
        ffluid = zero
        fdispo = zero
        vnorm = zero
        anorm = zero
        vtang(1) = zero
        vtang(2) = zero
        defpla = zero
        do 12 j = 1, 6
            deploc(j) = zero
            vitloc(j) = zero
12      continue
!
        origob(1) = parcho(i,14)
        origob(2) = parcho(i,15)
        origob(3) = parcho(i,16)
        sina = parcho(i,17)
        cosa = parcho(i,18)
        sinb = parcho(i,19)
        cosb = parcho(i,20)
        sing = parcho(i,21)
        cosg = parcho(i,22)
        signe(1) = parcho(i,37)
        signe(2) = parcho(i,38)
!
!        --- CONVERSION DDLS GENERALISES DDLS PHYSIQUES ---
!        POUR LE NOEUD 1
!
        if (nonmot(1:3) .eq. 'NON') then
            call tophys(i, 0, dplmod, nbchoc, nbmode,&
                        depgen, ux1, uy1, uz1)
        else
            call tophy3(i, 0, dplmod, nbchoc, nbmode,&
                        depgen, ux1, uy1, uz1, nbexci,&
                        psidel, coedep)
        endif
!        POSITION DU NOEUD 1 DANS LE REPERE GLOBAL
        depglo(1) = ux1 + parcho(i,8)
        depglo(2) = uy1 + parcho(i,9)
        depglo(3) = uz1 + parcho(i,10)
!        VITESSE DU NOEUD 1 DANS LE REPERE GLOBAL
        if (nonmot(1:3) .eq. 'NON') then
            call tophys(i, 0, dplmod, nbchoc, nbmode,&
                        vitgen, vx1, vy1, vz1)
        else
            call tophy3(i, 0, dplmod, nbchoc, nbmode,&
                        vitgen, vx1, vy1, vz1, nbexci,&
                        psidel, coevit)
        endif
        vitglo(1) = vx1
        vitglo(2) = vy1
        vitglo(3) = vz1
!        ACCELERATION DU NOEUD 1 DANS LE REPERE GLOBAL
        if (nonmot(1:3) .eq. 'NON') then
            call tophys(i, 0, dplmod, nbchoc, nbmode,&
                        accgen, ax1, ay1, az1)
        else
            call tophy3(i, 0, dplmod, nbchoc, nbmode,&
                        accgen, ax1, ay1, az1, nbexci,&
                        psidel, coeacc)
        endif
        accglo(1) = ax1
        accglo(2) = ay1
        accglo(3) = az1
!
!        --- PASSAGE DANS LE REPERE LOCAL ---
!        POUR LE NOEUD 1
        call gloloc(depglo, origob, sina, cosa, sinb,&
                    cosb, sing, cosg, deploc)
        call gloloc(vitglo, orig, sina, cosa, sinb,&
                    cosb, sing, cosg, vitloc)
        call gloloc(accglo, orig, sina, cosa, sinb,&
                    cosb, sing, cosg, accloc)
!        DEPLACEMENT DIFFERENTIEL = DEPLOC SI 1 NOEUD
        ddeplo(1) = deploc(1)
        ddeplo(2) = deploc(2)
        ddeplo(3) = deploc(3)
!
        if (noecho(i,9)(1:2) .eq. 'BI') then
!
!           MEME TRAVAIL POUR LE NOEUD 2
!
            if (nonmot(1:3) .eq. 'NON') then
                call tophys(i, 3, dplmod, nbchoc, nbmode,&
                            depgen, ux2, uy2, uz2)
            else
                call tophy3(i, 3, dplmod, nbchoc, nbmode,&
                            depgen, ux2, uy2, uz2, nbexci,&
                            psidel, coedep)
            endif
!           POSITION DU NOEUD 2 DANS LE REPERE GLOBAL
            depglo(4) = ux2 + parcho(i,11)
            depglo(5) = uy2 + parcho(i,12)
            depglo(6) = uz2 + parcho(i,13)
!           VITESSE DU NOEUD 2 DANS LE REPERE GLOBAL
            if (nonmot(1:3) .eq. 'NON') then
                call tophys(i, 3, dplmod, nbchoc, nbmode,&
                            vitgen, vx2, vy2, vz2)
            else
                call tophy3(i, 3, dplmod, nbchoc, nbmode,&
                            vitgen, vx2, vy2, vz2, nbexci,&
                            psidel, coevit)
            endif
            vitglo(4) = vx2
            vitglo(5) = vy2
            vitglo(6) = vz2
!           ACCELERATION DU NOEUD 2 DANS LE REPERE GLOBAL
            if (nonmot(1:3) .eq. 'NON') then
                call tophys(i, 3, dplmod, nbchoc, nbmode,&
                            accgen, ax2, ay2, az2)
            else
                call tophy3(i, 3, dplmod, nbchoc, nbmode,&
                            accgen, ax2, ay2, az2, nbexci,&
                            psidel, coeacc)
            endif
            accglo(4) = ax2
            accglo(5) = ay2
            accglo(6) = az2
!           --- PASSAGE DANS LE REPERE LOCAL --- POUR LE NOEUD 2
            call gloloc(depglo(4), origob, sina, cosa, sinb,&
                        cosb, sing, cosg, deploc(4))
            call gloloc(vitglo(4), orig, sina, cosa, sinb,&
                        cosb, sing, cosg, vitloc(4))
            call gloloc(accglo(4), orig, sina, cosa, sinb,&
                        cosb, sing, cosg, accloc(4))
!           ACCELERATION DIFFERENTIELLE ENTRE NOEUD1 ET NOEUD2
            accloc(1) = accloc(1)-accloc(4)
            accloc(2) = accloc(2)-accloc(5)
            accloc(3) = accloc(3)-accloc(6)
!           VITESSE DIFFERENTIELLE ENTRE NOEUD1 ET NOEUD2
            vitloc(1) = vitloc(1)-vitloc(4)
            vitloc(2) = vitloc(2)-vitloc(5)
            vitloc(3) = vitloc(3)-vitloc(6)
!           DEPLACEMENT DIFFERENTIEL ENTRE NOEUD1 ET NOEUD2
            ddeplo(1) = deploc(1)-deploc(4)
            ddeplo(2) = deploc(2)-deploc(5)
            ddeplo(3) = deploc(3)-deploc(6)
        endif
!
        xjeu = parcho(i,1)
        knorm = parcho(i,2)
        cnorm = parcho(i,3)
        ktang = parcho(i,4)
        ctang = parcho(i,5)
        cfrotd = parcho(i,6)
        cfrots = parcho(i,7)
        dist1 = parcho(i,30)
        dist2 = parcho(i,31)
        coefa = parcho(i,32)
        coefb = parcho(i,33)
        coefc = parcho(i,34)
        coefd = parcho(i,35)
        cl = parcho(i,36)
        coefk1 = parcho(i,39)
        coefk2 = parcho(i,40)
        coefpy = parcho(i,41)
        coefcc = parcho(i,42)
        coefad = parcho(i,43)
        xmax = parcho(i,44)
!        --- PARAMETRES DE FLAMBAGE ---
        flim = parcho(i,50)
        fseuil = parcho(i,51)
        rigifl = parcho(i,52)
        defpla = saucho(i,14)
!
!        ---  CALCUL DE LA DISTANCE NORMALE ---
        call distno(deploc, signe, noecho(i, 9), xjeu, dist1,&
                    dist2, dnorm, cost, sint)
!
        if (logcho(i,2) .eq. 1) then
!
!        --- CAS LAME FLUIDE ---
!
            if (dnorm .gt. zero) then
!
!              --- CALCUL DE LA FORCE FLUIDE REPERE LOCAL ---
                call mdfflu(dnorm, vnorm, anorm, vitloc, accloc,&
                            cost, sint, coefa, coefb, coefc,&
                            coefd, ffluid, flocal)
!
!               --- MISE A JOUR COUCHE LIMITE ET INDIC CHOC SEC ---
!
                if (logcho(i,3) .eq. 1) then
                    if (dnorm .ge. cl) then
                        logcho(i,3) = 0
                        parcho(i,36) = zero
                        cl = zero
                    endif
                else
                    cl = ffluid/knorm
                    if (dnorm .le. cl) then
                        logcho(i,3)=1
                        parcho(i,36)=cl
                    endif
                endif
!
                call ponder(dnorm, cl, pond)
!
!               --- PONDERATION ---
                flocal(1) = flocal(1) * pond
                flocal(2) = flocal(2) * pond
                flocal(3) = flocal(3) * pond
                ffluid = ffluid * pond
!
!
!               --- PASSAGE DE LA FORCE DANS LE REPERE GLOBAL ---
                call locglo(flocal, sina, cosa, sinb, cosb,&
                            sing, cosg, fgloba)
!
!               --- PASSAGE A LA FORCE GENERALISEE NOEUD_1 ---
                call togene(i, 0, dplmod, nbchoc, nbmode,&
                            fgloba(1), fgloba(2), fgloba(3), fexgen)
!               --- LA FORCE OPPOSEE SUR NOEUD_2 ---
                if (noecho(i,9)(1:2) .eq. 'BI') then
                    call togene(i, 3, dplmod, nbchoc, nbmode,&
                                -fgloba(1), - fgloba(2), -fgloba(3), fexgen)
                endif
                call mdmasf(i, dnorm, masgen, nbmode, phicar,&
                            fexgen, accgen, pulsa2, amogen, coefa*pond)
!
            else
                ffluid = zero
            endif
!
!
            if (dnorm .le. cl) then
!
!           --- CALCUL DE LA FORCE NORMALE REPERE LOCAL ---
                call fnorm(dnorm-cl, vitloc, knorm, cnorm, cost,&
                           sint, fn, flocal, vnorm)
!
!
                call ponder(dnorm, cl, pond)
!
!           --- PONDERATION ---
                flocal(1) = flocal(1)*(1.d0-pond)
                flocal(2) = flocal(2)*(1.d0-pond)
                flocal(3) = flocal(3)*(1.d0-pond)
                fn = fn *(1.d0-pond)
!
!
                if ((( cfrots .ne. zero ).or.( cfrotd .ne. zero )) .and. (dnorm .le. zero )) then
                    oldft(1) = parcho(i,26)
                    oldft(2) = parcho(i,27)
                    oldxl(1) = parcho(i,23)
                    oldxl(2) = parcho(i,24)
                    oldxl(3) = parcho(i,25)
                    oldvt(1) = parcho(i,28)
                    oldvt(2) = parcho(i,29)
                    call ftang(fn, ddeplo, vitloc, cfrotd, cfrots,&
                               ktang, ctang, logcho(i, 1), oldvt, oldft,&
                               oldxl, cost, sint, ftange, flocal,&
                               vtang)
                    parcho(i,26) = oldft(1)
                    parcho(i,27) = oldft(2)
                    parcho(i,23) = oldxl(1)
                    parcho(i,24) = oldxl(2)
                    parcho(i,25) = oldxl(3)
                    parcho(i,28) = oldvt(1)
                    parcho(i,29) = oldvt(2)
                endif
!
!           --- PASSAGE DE LA FORCE DANS LE REPERE GLOBAL ---
                call locglo(flocal, sina, cosa, sinb, cosb,&
                            sing, cosg, fgloba)
!
!           --- PASSAGE A LA FORCE GENERALISEE NOEUD_1 ---
                call togene(i, 0, dplmod, nbchoc, nbmode,&
                            fgloba(1), fgloba(2), fgloba(3), fexgen)
!           --- LA FORCE OPPOSEE SUR NOEUD_2 ---
                if (noecho(i,9)(1:2) .eq. 'BI') then
                    call togene(i, 3, dplmod, nbchoc, nbmode,&
                                -fgloba(1), - fgloba(2), -fgloba(3), fexgen)
                endif
!
            else
                fn = zero
            endif
!
            parcho(i,23) = ddeplo(1)
            parcho(i,24) = ddeplo(2)
            parcho(i,25) = ddeplo(3)
            parcho(i,26) = zero
            parcho(i,27) = zero
            logcho(i,1) = 0
!
!        DE FACON PROVISOIRE ON STOCKE FFLUID DANS FN POUR VISU
!
            fn = ffluid +fn
!
        else if (logcho(i,4).eq.1) then
!
!        --- CAS DISPOSITIF ANTI SISMIQUE ----
!
!**           IF ( DNORM .LE. ZERO ) THEN
!
!             --- CALCUL DE LA FORCE NORMALE REPERE LOCAL
!                 DU AU DISPOSITIF ANTI SISMIQUE  ---
            call mdfdas(dnorm, vnorm, vitloc, cost, sint,&
                        coefk1, coefk2, coefpy, coefcc, coefad,&
                        xmax, fdispo, flocal)
!
!             --- PASSAGE DE LA FORCE DANS LE REPERE GLOBAL ---
            call locglo(flocal, sina, cosa, sinb, cosb,&
                        sing, cosg, fgloba)
!
!             --- PASSAGE A LA FORCE GENERALISEE NOEUD_1 ---
            call togene(i, 0, dplmod, nbchoc, nbmode,&
                        fgloba(1), fgloba(2), fgloba(3), fexgen)
!             --- LA FORCE OPPOSEE SUR NOEUD_2 ---
            if (noecho(i,9)(1:2) .eq. 'BI') then
                call togene(i, 3, dplmod, nbchoc, nbmode,&
                            -fgloba(1), - fgloba(2), -fgloba(3), fexgen)
            endif
!**           ELSE
!**              FDISPO = ZERO
!**           ENDIF
            parcho(i,23) = ddeplo(1)
            parcho(i,24) = ddeplo(2)
            parcho(i,25) = ddeplo(3)
            parcho(i,26) = zero
            parcho(i,27) = zero
!           LOGCHO(I,1) = 0
!
!        DE FACON PROVISOIRE ON STOCKE FDISPO DANS FN POUR VISU
!
            fn = fdispo
!
        else if (logcho(i,5).eq.1) then
!
!        --- CAS DU FLAMBAGE ----
!
            if (dnorm .le. zero) then
!
!           --- CALCUL DE LA FORCE NORMALE REPERE LOCAL ---
                call mdflam(dnorm, vitloc, knorm, cost, sint,&
                            flim, fseuil, rigifl, defpla, fn,&
                            flocal, vnorm)
                if (( cfrots .ne. zero ) .or. ( cfrotd .ne. zero )) then
                    oldft(1) = parcho(i,26)
                    oldft(2) = parcho(i,27)
                    oldxl(1) = parcho(i,23)
                    oldxl(2) = parcho(i,24)
                    oldxl(3) = parcho(i,25)
                    oldvt(1) = parcho(i,28)
                    oldvt(2) = parcho(i,29)
                    call ftang(fn, ddeplo, vitloc, cfrotd, cfrots,&
                               ktang, ctang, logcho(i, 1), oldvt, oldft,&
                               oldxl, cost, sint, ftange, flocal,&
                               vtang)
                    parcho(i,26) = oldft(1)
                    parcho(i,27) = oldft(2)
                    parcho(i,23) = oldxl(1)
                    parcho(i,24) = oldxl(2)
                    parcho(i,25) = oldxl(3)
                    parcho(i,28) = oldvt(1)
                    parcho(i,29) = oldvt(2)
                endif
!
!           --- PASSAGE DE LA FORCE DANS LE REPERE GLOBAL ---
                call locglo(flocal, sina, cosa, sinb, cosb,&
                            sing, cosg, fgloba)
!
!           --- PASSAGE A LA FORCE GENERALISEE NOEUD_1 ---
                call togene(i, 0, dplmod, nbchoc, nbmode,&
                            fgloba(1), fgloba(2), fgloba(3), fexgen)
!           --- LA FORCE OPPOSEE SUR NOEUD_2 ---
                if (noecho(i,9)(1:2) .eq. 'BI') then
                    call togene(i, 3, dplmod, nbchoc, nbmode,&
                                -fgloba(1), - fgloba(2), -fgloba(3), fexgen)
                endif
            else
                parcho(i,23) = ddeplo(1)
                parcho(i,24) = ddeplo(2)
                parcho(i,25) = ddeplo(3)
                parcho(i,26) = zero
                parcho(i,27) = zero
                logcho(i,1) = 0
                ftange(1) = zero
                ftange(2) = zero
                vtang(1) = zero
                vtang(2) = zero
            endif
        else
!
!        --- CAS DU CHOC SEC ----
!
            if (dnorm .le. zero) then
!
!           --- CALCUL DE LA FORCE NORMALE REPERE LOCAL ---
                call fnorm(dnorm, vitloc, knorm, cnorm, cost,&
                           sint, fn, flocal, vnorm)
                if (( cfrots .ne. zero ) .or. ( cfrotd .ne. zero )) then
                    oldft(1) = parcho(i,26)
                    oldft(2) = parcho(i,27)
                    oldxl(1) = parcho(i,23)
                    oldxl(2) = parcho(i,24)
                    oldxl(3) = parcho(i,25)
                    oldvt(1) = parcho(i,28)
                    oldvt(2) = parcho(i,29)
                    call ftang(fn, ddeplo, vitloc, cfrotd, cfrots,&
                               ktang, ctang, logcho(i, 1), oldvt, oldft,&
                               oldxl, cost, sint, ftange, flocal,&
                               vtang)
                    parcho(i,26) = oldft(1)
                    parcho(i,27) = oldft(2)
                    parcho(i,23) = oldxl(1)
                    parcho(i,24) = oldxl(2)
                    parcho(i,25) = oldxl(3)
                    parcho(i,28) = oldvt(1)
                    parcho(i,29) = oldvt(2)
                endif
!
!           --- PASSAGE DE LA FORCE DANS LE REPERE GLOBAL ---
                call locglo(flocal, sina, cosa, sinb, cosb,&
                            sing, cosg, fgloba)
!
!           --- PASSAGE A LA FORCE GENERALISEE NOEUD_1 ---
                call togene(i, 0, dplmod, nbchoc, nbmode,&
                            fgloba(1), fgloba(2), fgloba(3), fexgen)
!           --- LA FORCE OPPOSEE SUR NOEUD_2 ---
                if (noecho(i,9)(1:2) .eq. 'BI') then
                    call togene(i, 3, dplmod, nbchoc, nbmode,&
                                -fgloba(1), - fgloba(2), -fgloba(3), fexgen)
                endif
            else
                parcho(i,23) = ddeplo(1)
                parcho(i,24) = ddeplo(2)
                parcho(i,25) = ddeplo(3)
                parcho(i,26) = zero
                parcho(i,27) = zero
                logcho(i,1) = 0
                ftange(1) = zero
                ftange(2) = zero
                vtang(1) = zero
                vtang(2) = zero
            endif
        endif
!
        saucho(i,1) = fn
        saucho(i,2) = ftange(1)
        saucho(i,3) = ftange(2)
!        DEPLACEMENT LOCAL DU NOEUD NOEUD_1
        saucho(i,4) = deploc(1)
        saucho(i,5) = deploc(2)
        saucho(i,6) = deploc(3)
        saucho(i,7) = vnorm
        saucho(i,8) = vtang(1)
        saucho(i,9) = vtang(2)
!        DEPLACEMENT LOCAL DU NOEUD NOEUD_2
        saucho(i,10) = deploc(4)
        saucho(i,11) = deploc(5)
        saucho(i,12) = deploc(6)
!        INDICATEUR ADHERENCE
        saucho(i,13) = logcho(i,1)
!        FLAMBAGE : ECRASEMENT CUMULE (VARIABLE INTERNE)
        saucho(i,14) = defpla
!
10  end do
!
end subroutine

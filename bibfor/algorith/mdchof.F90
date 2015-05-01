subroutine mdchof(np1, np2, np3, nbm, impr,&
                  tc, nbnl, typch, nbseg, phii,&
                  nomch, choc, alpha, beta, gamma,&
                  orig, rc, theta, vitg, depg,&
                  vitg0, depg0, old, oldia, fmres,&
                  fmod, ftmp, testc, itforn, toln)
! aslint: disable=W1504
    implicit none
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! DESCRIPTION : ESTIMATION ET "LINEARISATION" DE LA FORCE NON-LINEAIRE
! -----------   (NON-LINEARITE DE TYPE CHOC)
!               CALCUL DE LA FORCE EXTERIEURE APRES PASSAGE DE
!               L'ALGORITHME TEMPOREL
!
!               APPELANT : MDITM2
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
#include "asterfort/caljac.h"
#include "asterfort/calres.h"
#include "asterfort/deimpj.h"
#include "asterfort/disbut.h"
#include "asterfort/ftest1.h"
#include "asterfort/gloloc.h"
#include "asterfort/iunifi.h"
#include "asterfort/locglo.h"
#include "asterfort/projgm.h"
#include "asterfort/projmg.h"
#include "asterfort/vecini.h"
    integer :: np1, np2, np3, nbm, impr
    real(kind=8) :: tc
    integer :: nbnl, typch(*), nbseg(*)
    real(kind=8) :: phii(np2, np1, *)
    character(len=8) :: nomch(*)
    real(kind=8) :: choc(6, *), alpha(2, *), beta(2, *), gamma(2, *), orig(6, *)
    real(kind=8) :: rc(np3, *), theta(np3, *), vitg(*), depg(*), vitg0(*)
    real(kind=8) :: depg0(*), old(9, *)
    integer :: oldia(*)
    real(kind=8) :: fmres(*), fmod(*), ftmp(*)
    integer :: testc, itforn(*)
    real(kind=8) :: toln
!
! VARIABLES LOCALES
! -----------------
    integer :: ic, j, iforn, typobs, nbs, itestc, itest0, testcv, ifr, ifm
    integer :: latest, nbchsi
    real(kind=8) :: xglo0(3), xxglo0(3), xloc0(3), vglo0(3), vloc0(3), xglo(3)
    real(kind=8) :: xxglo(3), xloc(3), vglo(3), vloc(3), x00(3), excloc(3)
    real(kind=8) :: xorig(3), vorig(3), sina, cosa, sinb, cosb, sing, cosg, xjeu
    real(kind=8) :: cost, sint, dnorm, fglo(3), floc(3), fglres(3), flres(3)
    real(kind=8) :: jacobc(3, 3), jacobk(3, 3), chockc(2), tetaj
!
! FONCTIONS INTRINSEQUES
! ----------------------
!     INTRINSIC    ABS
!
! FONCTIONS EXTERNES
! ------------------
!     EXTERNAL     IUNIFI
!
! ROUTINES EXTERNES
! -----------------
!     EXTERNAL     CALJAC, CALRES, DEIMPJ, DISBUT, FTEST1, GLOLOC,
!    &             LCINVN, LOCGLO, PROJGM, PROJMG
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
!  0. INITIALISATIONS
!     ---------------
!
    if (impr .ge. 1) then
        ifm = iunifi('MESSAGE')
        if (impr .ge. 2) ifr = iunifi('RESULTAT')
    endif
!
    call vecini(np1, 0.d0, fmod)
    call vecini(np1, 0.d0, fmres)
    call vecini(3, 0.d0, floc)
!
    latest = testc
    testc = 0
!
    nbchsi = 0
!
!  1. BOUCLE SUR LES NON-LINEARITES
!     -----------------------------
!
    do 10 ic = 1, nbnl
!
        tetaj = 1.0d0
!
        typobs = typch(ic)
        nbs = nbseg(ic)
!
        if ((typobs.eq.0) .or. (typobs.eq.1) .or. (typobs.eq.2)) xjeu = rc(1,ic)
!
!  1.1.  CONVERSION DDLS GENERALISES -> DDLS PHYSIQUES
!        ---------------------------------------------
!        (DEPLACEMENTS)
        call projmg(np1, np2, ic, nbm, phii,&
                    depg, xglo)
        call projmg(np1, np2, ic, nbm, phii,&
                    depg0, xglo0)
!        (VITESSES)
        call projmg(np1, np2, ic, nbm, phii,&
                    vitg, vglo)
        call projmg(np1, np2, ic, nbm, phii,&
                    vitg0, vglo0)
!
!  1.2.  PASSAGE REPERE GLOBAL -> LOCAL
!        ------------------------------
        xorig(1) = orig(1,ic)
        xorig(2) = orig(2,ic)
        xorig(3) = orig(3,ic)
        vorig(1) = 0.0d0
        vorig(2) = 0.0d0
        vorig(3) = 0.0d0
!
        x00(1) = orig(4,ic)
        x00(2) = orig(5,ic)
        x00(3) = orig(6,ic)
!
        xxglo(1) = xglo(1) + x00(1)
        xxglo(2) = xglo(2) + x00(2)
        xxglo(3) = xglo(3) + x00(3)
        xxglo0(1) = xglo0(1) + x00(1)
        xxglo0(2) = xglo0(2) + x00(2)
        xxglo0(3) = xglo0(3) + x00(3)
!
        sina = alpha(1,ic)
        cosa = alpha(2,ic)
        sinb = beta(1,ic)
        cosb = beta(2,ic)
        sing = gamma(1,ic)
        cosg = gamma(2,ic)
!        (DEPLACEMENTS)
        call gloloc(xxglo, xorig, sina, cosa, sinb,&
                    cosb, sing, cosg, xloc)
        call gloloc(xxglo0, xorig, sina, cosa, sinb,&
                    cosb, sing, cosg, xloc0)
!        (VITESSES)
        call gloloc(vglo, vorig, sina, cosa, sinb,&
                    cosb, sing, cosg, vloc)
        call gloloc(vglo0, vorig, sina, cosa, sinb,&
                    cosb, sing, cosg, vloc0)
!        (EXCENTREMENT)
        call gloloc(x00, xorig, sina, cosa, sinb,&
                    cosb, sing, cosg, excloc)
!
!  1.3.  TEST CHOC SUR BUTEE IC
!        ----------------------
        call disbut(np3, ic, xloc, typobs, xjeu,&
                    rc, theta, nbs, cost, sint,&
                    dnorm)
        call ftest1(np3, rc, theta, typch, nbseg,&
                    xloc, ic, itestc, toln)
        call ftest1(np3, rc, theta, typch, nbseg,&
                    xloc0, ic, itest0, toln)
        testcv = 0
        call deimpj(itestc, itest0, tetaj, testcv)
!
!  1.4.  CALCULS REALISES EN CAS DE CHOC SUR LA BUTEE IC
!        -----------------------------------------------
        if (testcv .eq. 1) then
!
            testc = 1
!
!  1.4.1.   CALCUL DES MATRICES JACOBIENNES LIEES A LA FORCE NON-LIN
!           --------------------------------------------------------
            iforn = itforn(ic)
            if (iforn .eq. 0) then
                chockc(1) = choc(1,ic)
                chockc(2) = choc(2,ic)
            else
                chockc(1) = choc(1,ic)
                chockc(2) = 0.0d0
            endif
            call caljac(np3, ic, typch, nbseg, chockc,&
                        rc, theta, vloc, xloc, vloc0,&
                        xloc0, tetaj, jacobc, jacobk)
!
!  1.4.2.   CALCUL DES FORCES NON-LINEAIRES ET RESIDUELLES
!           ----------------------------------------------
            call calres(np3, ic, typch, nbseg, choc,&
                        rc, theta, vloc, xloc, vloc0,&
                        xloc0, excloc, tetaj, jacobc, jacobk,&
                        floc, flres, old, oldia, iforn,&
                        toln)
!
!  1.4.3.   PASSAGE REPERE LOCAL -> GLOBAL (FORCES RESIDUELLES)
!           ---------------------------------------------------
            call locglo(floc, sina, cosa, sinb, cosb,&
                        sing, cosg, fglo)
            call locglo(flres, sina, cosa, sinb, cosb,&
                        sing, cosg, fglres)
!
!  1.4.4.   CONVERSION DDLS PHYSIQUES -> DDLS GENERALISES
!           (FORCES MODALES)
!           ----------------
            call vecini(np1, 0.d0, ftmp)
            call projgm(np1, np2, ic, nbm, phii,&
                        fglo, ftmp)
            do 20 j = 1, nbm
                fmod(j) = fmod(j) + ftmp(j)
20          continue
!
!  1.4.5.   CONVERSION DDLS PHYSIQUES -> DDLS GENERALISES
!           (FORCES RESIDUELLES)
!           --------------------
            call vecini(np1, 0.d0, ftmp)
            call projgm(np1, np2, ic, nbm, phii,&
                        fglres, ftmp)
            do 30 j = 1, nbm
                fmres(j) = fmres(j) + ftmp(j)
30          continue
!
!  1.4.6.   IMPRESSIONS LE CAS ECHEANT
!           --------------------------
!.......... ITEST0.LT.1
!.......... <=> VOL OU CONTACT EXACT A L'INSTANT PRECEDENT
            if ((itest0.lt.1) .and. (impr.ge.1)) then
                nbchsi = nbchsi + 1
                write(ifm,1001) nomch(ic), tc
                if (impr .ge. 2) write(ifr,1001) nomch(ic), tc
            endif
!
!  1.5.  REMPLISSAGE DU TABLEAU OLD LORSQU'IL N'Y A PAS DE CHOC
!        SUR LA BUTEE IC
!        ---------------
        else
!
!  1.5.1.   REMPLISSAGE DU TABLEAU OLD
!           --------------------------
            old(1,ic) = -sint*vloc(2) + cost*vloc(3)
            old(2,ic) = vloc(1)
            old(3,ic) = 0.0d0
            old(4,ic) = 0.0d0
            old(5,ic) = xloc(1)
            old(6,ic) = xloc(2)
            old(7,ic) = xloc(3)
            old(8,ic) = 0.0d0
            old(9,ic) = cost*vloc(2) + sint*vloc(3)
!
!  1.5.2.   IMPRESSIONS LE CAS ECHEANT
!           --------------------------
!.......... ABS(ITEST0).EQ.1
!.......... <=> CONTACT EXACT OU CHOC A L'INSTANT PRECEDENT
            if ((abs(itest0).eq.1) .and. (impr.ge.1)) then
                write(ifm,1002) nomch(ic), tc
                if (impr .ge. 2) write(ifr,1002) nomch(ic), tc
            endif
!
        endif
!
10  end do
!
!  2. IMPRESSIONS COMPLEMENTAIRES LE CAS ECHEANT
!     ------------------------------------------
!
    if ((latest.ne.testc) .and. (impr.ge.1)) then
        if ((testc.eq.1) .and. (nbchsi.gt.1)) then
            write(ifm,1003) nbchsi, tc
            if (impr .ge. 2) write(ifr,1003) nbchsi, tc
        else if (testc.eq.0) then
            write(ifm,1004) tc
            if (impr .ge. 2) write(ifr,1004) tc
        endif
    endif
!
!  3. FORMATS
!     -------
!
    1001 format(1x,'PASSAGE EN PHASE DE CHOC AU NOEUD ',a8,7x,&
     &' - TEMPS COURANT: ',1pe12.5e2/)
    1002 format(1x,'PASSAGE EN PHASE DE VOL  AU NOEUD ',a8,7x,&
     &' - TEMPS COURANT: ',1pe12.5e2/)
    1003 format(1x,'PASSAGE EN PHASE DE CHOCS SIMULTANES EN ',i2,' NOEUDS',&
     &' - TEMPS COURANT: ',1pe12.5e2/)
    1004 format(1x,'PASSAGE EN PHASE DE VOL',26x,&
     &' - TEMPS COURANT: ',1pe12.5e2/)
!
! --- FIN DE MDCHOF.
end subroutine

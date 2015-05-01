subroutine mdchoe(np1, np2, np3, nbm, nbmcd,&
                  nbnl, typch, nbseg, phii, choc,&
                  alpha, beta, gamma, orig, rc,&
                  theta, cmod, kmod, vitg, depg,&
                  vitg0, depg0, old, oldia, fmod,&
                  fmres, ftmp, mtmp1, mtmp6, testc,&
                  itforn, inewto, toln)
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
!               CALCUL DE LA FORCE EXTERIEURE
!
!               APPELANT : CALFNL
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
#include "asterfort/locglo.h"
#include "asterfort/matini.h"
#include "asterfort/prlgma.h"
#include "asterfort/projac.h"
#include "asterfort/projgm.h"
#include "asterfort/projmg.h"
#include "asterfort/vecini.h"
    integer :: np1, np2, np3, nbm, nbmcd, nbnl, typch(*), nbseg(*)
    real(kind=8) :: phii(np2, np1, *), choc(6, *), alpha(2, *), beta(2, *)
    real(kind=8) :: gamma(2, *), orig(6, *), rc(np3, *), theta(np3, *)
    real(kind=8) :: cmod(np1, *), kmod(np1, *), vitg(*), depg(*), vitg0(*)
    real(kind=8) :: depg0(*), old(9, *)
    integer :: oldia(*)
    real(kind=8) :: fmod(*), fmres(*), ftmp(*), mtmp1(np1, *), mtmp6(3, *)
    integer :: testc, itforn(*), inewto
    real(kind=8) :: toln
!
! VARIABLES LOCALES
! -----------------
    integer :: ic, j, k, iforn, typobs, nbs, itestc, itest0, testcv
    real(kind=8) :: xglo0(3), xxglo0(3), xloc0(3), vglo0(3), vloc0(3), xglo(3)
    real(kind=8) :: xxglo(3), xloc(3), vglo(3), vloc(3), x00(3), excloc(3)
    real(kind=8) :: xorig(3), vorig(3), sina, cosa, sinb, cosb, sing, cosg, xjeu
    real(kind=8) :: cost, sint, dnorm, fglo(3), floc(3), fglres(3), flres(3)
    real(kind=8) :: jacobc(3, 3), jacobk(3, 3), chockc(2), tetaj
!
! ROUTINES EXTERNES
! -----------------
!     EXTERNAL  CALJAC, CALRES, DEIMPJ, DISBUT, FTEST1, GLOLOC, MATINI,
!    &          LCINVN, LOCGLO, PRLGMA, PROJAC, PROJGM, PROJMG
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
!  0. INITIALISATIONS
!     ---------------
    call matini(np1, np1, 0.d0, cmod)
    call matini(np1, np1, 0.d0, kmod)
    call vecini(np1, 0.d0, fmod)
    call vecini(3, 0.d0, floc)
    call vecini(np1, 0.d0, fmres)
!
! --- SI INEWTO = 1, LA ROUTINE APPELANTE EST NEWTON, LES CALCULS SONT
! --- EFFECTUES DANS LA CONFIGURATION A TC0. SI INEWTO = 0, LA ROUTINE
! --- APPELANTE EST ALITMI, LES CALCULS SONT EFFECTUES SELON LE
! --- CHANGEMENT DE CONFIGURATION
!
    testc = 0
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
        call projmg(np1, np2, ic, nbmcd, phii,&
                    depg, xglo)
        call projmg(np1, np2, ic, nbmcd, phii,&
                    depg0, xglo0)
!        (VITESSES)
        call projmg(np1, np2, ic, nbmcd, phii,&
                    vitg, vglo)
        call projmg(np1, np2, ic, nbmcd, phii,&
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
!
!  ----- DETERMINATION DU POINT D'IMPLICITATION DU JACOBIEN
!
        testcv = 0
        call deimpj(itestc, itest0, tetaj, testcv)
!
!  ----- MDCHOE EST APPELE PAR NEWTON : TESTCV = ITEST0
!        ----------------------------------------------
        if ((inewto.eq.1) .and. (itest0.ne.-1)) testcv = itest0
!
!  1.4.  CALCULS REALISES EN CAS DE CHOC SUR LA BUTEE IC
!        -----------------------------------------------
        old(8,ic) = 0.0d0
        old(9,ic) = 0.0d0
!
        if (testcv .eq. 1) then
!
            testc = 1
!
!  1.4.1.   CALCUL DES MATRICES JACOBIENNES LIEES A LA FORCE NON-LIN
!           --------------------------------------------------------
            chockc(1) = choc(1,ic)
            chockc(2) = choc(2,ic)
            call caljac(np3, ic, typch, nbseg, chockc,&
                        rc, theta, vloc, xloc, vloc0,&
                        xloc0, tetaj, jacobc, jacobk)
!
!  1.4.2.   CALCUL DES FORCES NON-LINEAIRES RESIDUELLES
!           -------------------------------------------
            call calres(np3, ic, typch, nbseg, choc,&
                        rc, theta, vloc, xloc, vloc0,&
                        xloc0, excloc, tetaj, jacobc, jacobk,&
                        floc, flres, old, oldia, iforn,&
                        toln)
            itforn(ic) = iforn
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
            call projgm(np1, np2, ic, nbmcd, phii,&
                        fglo, ftmp)
            do 20 j = 1, nbmcd
                fmod(j) = fmod(j) + ftmp(j)
20          continue
!
!  1.4.5.   CONVERSION DDLS PHYSIQUES -> DDLS GENERALISES
!           (FORCES RESIDUELLES)
!           --------------------
            call vecini(np1, 0.d0, ftmp)
            call projgm(np1, np2, ic, nbmcd, phii,&
                        fglres, ftmp)
            do 30 j = 1, nbmcd
                fmres(j) = fmres(j) + ftmp(j)
30          continue
!
!  1.4.6.   PASSAGE REPERE LOCAL -> GLOBAL (FORCES NON-LINEAIRES)
!           -----------------------------------------------------
            call prlgma(jacobk, sina, cosa, sinb, cosb,&
                        sing, cosg, jacobk)
            call prlgma(jacobc, sina, cosa, sinb, cosb,&
                        sing, cosg, jacobc)
!
!  1.4.7.   CONVERSION DDLS PHYSIQUES -> DDLS GENERALISES
!           (FORCES NON-LINEAIRES)
!           ----------------------
            call projac(np1, np2, nbm, ic, phii,&
                        jacobc, mtmp1, mtmp6)
            do 40 k = 1, nbm
                do 41 j = 1, nbm
                    cmod(j,k) = cmod(j,k) + mtmp1(j,k)
41              continue
40          continue
            call projac(np1, np2, nbm, ic, phii,&
                        jacobk, mtmp1, mtmp6)
            do 50 k = 1, nbm
                do 51 j = 1, nbm
                    kmod(j,k) = kmod(j,k) + mtmp1(j,k)
51              continue
50          continue
!
!  1.5.  REMPLISSAGE DU TABLEAU OLD LORSQU'IL N'Y A PAS CHOC
!        SUR LA BUTEE IC
!        ---------------
        else
!
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
        endif
!
10  end do
!
! --- FIN DE MDCHOE.
end subroutine

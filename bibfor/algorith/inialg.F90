subroutine inialg(nbm, np2, np3, np4, nbmc,&
                  nbnl, npf, npfmax, npfts, depg,&
                  vitg, depg0, vitg0, accg0, amor00,&
                  puls00, fexttr, fext, text, fextts,&
                  texts, typch, nbseg, phii, alpha,&
                  beta, gamma, orig, rc, theta,&
                  iconfb, tconf1, ftest0)
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
! TOLE  CRP_21
!-----------------------------------------------------------------------
! DESCRIPTION : INITIALISATIONS POUR ALGO ITMI EN REGIME ETABLI
! -----------
!               APPELANTS : MDITM2, TRANSI
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    include 'asterfort/accele.h'
    include 'asterfort/disbut.h'
    include 'asterfort/gloloc.h'
    include 'asterfort/matini.h'
    include 'asterfort/projmg.h'
    include 'asterfort/vecini.h'
    integer :: nbm, np2, np3, np4, nbmc, nbnl, npf, npfmax, npfts
    real(kind=8) :: depg(*), vitg(*), depg0(*), vitg0(*), accg0(*), amor00(*)
    real(kind=8) :: puls00(*), fexttr(*), fext(np4, *), text(*), fextts(np4, *)
    real(kind=8) :: texts(*)
    integer :: typch(*), nbseg(*)
    real(kind=8) :: phii(np2, nbm, *), alpha(2, *), beta(2, *), gamma(2, *)
    real(kind=8) :: orig(6, *), rc(np3, *), theta(np3, *)
    integer :: iconfb(*)
    real(kind=8) :: tconf1(4, *), ftest0
!
! VARIABLES LOCALES
! -----------------
    integer :: i, ic, j, typobs, nbs
    real(kind=8) :: xglo0(3), xloc0(3), xorig(3), sina, cosa, sinb, cosb, sing
    real(kind=8) :: cosg, dnorm0, xjeu, sint, cost
!
! FONCTIONS INTRINSEQUES
! ----------------------
!     INTRINSIC  ABS
!
! ROUTINES EXTERNES
! -----------------
!     EXTERNAL   ACCELE, DISBUT, GLOLOC, MATINI, LCINVN, PROJMG
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    npfts = npfmax - npf + 1
    call matini(np4, nbm, 0.d0, fextts)
    call vecini(np4, 0.d0, texts)
!
!  1. DEPLACEMENT, VITESSE ET ACCELERATION INITIAUX POUR ALGO ITMI
!     ------------------------------------------------------------
!
    do 10 i = 1, nbmc
        depg0(i) = depg(i)
10  end do
    do 20 i = 1, nbmc
        vitg0(i) = vitg(i)
20  end do
    call accele(nbmc, amor00, puls00, fexttr, accg0,&
                vitg0, depg0)
!
!  2. FORCES MODALES POUR SIMULATION NON-LINEAIRE ITMI
!     ------------------------------------------------
!
    do 60 i = 1, nbmc
        do 61 j = 1, npfts
            fextts(j,i) = fext(npf+j-1,i)
61      continue
60  end do
!
!  3. TABLEAU TEMPS POUR SIMULATION NON-LINEAIRE ITMI
!     -----------------------------------------------
!
    do 70 j = 1, npfts
        texts(j) = text(npf+j-1) - text(npf)
70  end do
!
!  4. SUPPRIME (BASE REDUITE)
!
!  5. INDICATEURS DE CHANGEMENT DE CONFIGURATION
!     ------------------------------------------
!
    do 90 ic = 1, nbnl
        iconfb(ic) = 1
90  end do
!
!  6. COORDONNEES DES NOEUDS DE CHOC DANS LES REPERES LOCAUX LIES AUX
!     BUTEES ET DISTANCES NORMALES DES NOEUDS DE CHOC AUX BUTEES
!     ----------------------------------------------------------
!
    ftest0 = 1.0d+10
!
    do 100 ic = 1, nbnl
!
!  6.1   CONVERSION DDLS GENERALISES -> DDLS PHYSIQUES
!
        call projmg(nbm, np2, ic, nbmc, phii,&
                    depg0, xglo0)
!
!  6.2   PASSAGE REPERE GLOBAL -> REPERE LOCAL
!
        xorig(1) = orig(1,ic)
        xorig(2) = orig(2,ic)
        xorig(3) = orig(3,ic)
        xglo0(1) = xglo0(1) + orig(4,ic)
        xglo0(2) = xglo0(2) + orig(5,ic)
        xglo0(3) = xglo0(3) + orig(6,ic)
        sina = alpha(1,ic)
        cosa = alpha(2,ic)
        sinb = beta(1,ic)
        cosb = beta(2,ic)
        sing = gamma(1,ic)
        cosg = gamma(2,ic)
!
        call gloloc(xglo0, xorig, sina, cosa, sinb,&
                    cosb, sing, cosg, xloc0)
        tconf1(1,ic) = xloc0(1)
        tconf1(2,ic) = xloc0(2)
        tconf1(3,ic) = xloc0(3)
!
!  6.3   CALCUL DE LA DISTANCE NORMALE DU NOEUD DE CHOC A LA BUTEE
!
        typobs = typch(ic)
        nbs = nbseg(ic)
        if ((typobs.eq.0) .or. (typobs.eq.1) .or. (typobs.eq.2)) xjeu = rc(1,ic)
!
        call disbut(np3, ic, xloc0, typobs, xjeu,&
                    rc, theta, nbs, cost, sint,&
                    dnorm0)
!
        tconf1(4,ic) = dnorm0
!
        if (abs(dnorm0) .lt. ftest0) ftest0 = abs(dnorm0)
!
100  end do
!
! --- FIN DE INIALG.
end subroutine

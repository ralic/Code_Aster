subroutine testch(np1, np2, np3, nbmcd, nbnl,&
                  toln, tolc, tolv, typch, nbseg,&
                  phii, alpha, beta, gamma, orig,&
                  rc, theta, tconf1, depg, nbch,&
                  nbchex, iconf, ftest, iconfb, tconf2)
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
! DESCRIPTION : TESTE LE CHANGEMENT DE CONFIGURATION ENTRE N ET N+1
! -----------
!               APPELANTS : ALITMI, NEWTON
!
! INDICATEURS GLOBAUX RENVOYES EN SORTIE :
! --------------------------------------
!
! NBCH   = NOMBRE DE CHANGEMENTS D'ETAT ENTRE LES INSTANTS N ET N+1
!
! NBCHEX = NOMBRE DE CHANGEMENTS D'ETAT AVEC CONTACT EXACT
!          A L'INSTANT N+1
!
! ICONF = INDICATEUR DE CHANGEMENT DE CONFIGURATION
! ICONF =  1  AUCUN CHANGEMENT D'ETAT ENTRE LES INSTANTS N ET N+1
! ICONF =  0  AU MOINS UN CHANGEMENT D'ETAT ENTRE LES INSTANTS N ET N+1
!             SANS VARIATION IMPORTANTE DE DEPLACEMENT PHYSIQUE
! ICONF = -1  VARIATION IMPORTANTE DU DEPLACEMENT PHYSIQUE D'UN NOEUD
!             DE CHOC AU MOINS ENTRE LES INSTANTS N ET N+1
!
! FTEST = DISTANCE NORMALE MINIMALE (EN VALEUR ABSOLUE) D'UN NOEUD DE
!         CHOC A LA BUTEE A L'INSTANT N+1
!
! INDICATEURS RENVOYES POUR CHAQUE BUTEE :
! --------------------------------------
!
! ICONFB(IC) =  1  PAS DE CHANGEMENT D'ETAT ENTRE LES INSTANTS N ET N+1
!                  SUR LA BUTEE IC
! ICONFB(IC) =  0  CHANGEMENT D'ETAT ENTRE LES INSTANTS N ET N+1
!                  SUR LA BUTEE IC
! ICONFB(IC) = -1  VARIATION IMPORTANTE DU DEPLACEMENT PHYSIQUE
!                  DU NOEUD DE CHOC ENTRE LES INSTANTS N ET N+1
!
! TCONF2(1,IC) = XLOC(1)  COORDONNEES DU NOEUD DE CHOC DANS LE REPERE
! TCONF2(2,IC) = XLOC(2)  LOCAL A L'INSTANT N+1
! TCONF2(3,IC) = XLOC(3)
!
! TCONF2(4,IC) = DNORM    DISTANCE NORMALE DU NOEUD DE CHOC A LA BUTEE
!                         A L'INSTANT N+1
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    include 'asterfort/disbut.h'
    include 'asterfort/gloloc.h'
    include 'asterfort/projmg.h'
    include 'asterfort/vardec.h'
    integer :: np1, np2, np3, nbmcd, nbnl
    real(kind=8) :: toln, tolc, tolv
    integer :: typch(*), nbseg(*)
    real(kind=8) :: phii(np2, np1, 3), alpha(2, *), beta(2, *), gamma(2, *)
    real(kind=8) :: orig(6, *), rc(np3, *), theta(np3, *), tconf1(4, *), depg(*)
    integer :: nbch, nbchex, iconf
    real(kind=8) :: ftest
    integer :: iconfb(*)
    real(kind=8) :: tconf2(4, *)
!
! VARIABLES LOCALES
! -----------------
    integer :: ic, ivar
    integer :: typobs, nbs
    real(kind=8) :: dvar, tolch, zero
    real(kind=8) :: xloc0(3), xglo(3), xloc(3)
    real(kind=8) :: dnorm0, dnorm, xjeu, sint, cost
    real(kind=8) :: xorig(3), sina, cosa, sinb, cosb, sing, cosg
!
! FONCTIONS INTRINSEQUES
! ----------------------
!     INTRINSIC  ABS
!
! ROUTINES EXTERNES
! -----------------
!     EXTERNAL   DISBUT, GLOLOC, PROJMG, VARDEC
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
!  0. INITIALISATIONS
!     ---------------
!
    nbch = 0
    nbchex = 0
    ivar = 0
    ftest = 1.0d+10
    dvar = 1.0d0
    tolch = 10.0d0 * toln
    zero = 0.0d0
!
!  1. BOUCLE SUR LES NON-LINEARITES
!     -----------------------------
!
    do 10 ic = 1, nbnl
!
!  1.1   RECUPERATION DU DEPLACEMENT PHYSIQUE DU NOEUD DE CHOC
!        A L'INSTANT N DANS LE REPERE LOCAL
!
        xloc0(1) = tconf1(1,ic)
        xloc0(2) = tconf1(2,ic)
        xloc0(3) = tconf1(3,ic)
!
!  1.2   RECUPERATION DE LA DISTANCE NORMALE DU NOEUD DE CHOC
!        A LA BUTEE A L'INSTANT N
!
        dnorm0 = tconf1(4,ic)
        if (abs(dnorm0) .lt. tolch) dnorm0 = zero
!
!  1.3   CALCUL DU DEPLACEMENT PHYSIQUE DU NOEUD DE CHOC
!        A L'INSTANT N+1 DANS LE REPERE LOCAL
!
!  1.3.1 CONVERSION DDLS GENERALISES -> DDLS PHYSIQUES
!
        call projmg(np1, np2, ic, nbmcd, phii,&
                    depg, xglo)
!
!  1.3.2 PASSAGE REPERE GLOBAL -> REPERE LOCAL
!
        xorig(1) = orig(1,ic)
        xorig(2) = orig(2,ic)
        xorig(3) = orig(3,ic)
        xglo(1) = xglo(1) + orig(4,ic)
        xglo(2) = xglo(2) + orig(5,ic)
        xglo(3) = xglo(3) + orig(6,ic)
        sina = alpha(1,ic)
        cosa = alpha(2,ic)
        sinb = beta(1,ic)
        cosb = beta(2,ic)
        sing = gamma(1,ic)
        cosg = gamma(2,ic)
!
        call gloloc(xglo, xorig, sina, cosa, sinb,&
                    cosb, sing, cosg, xloc)
        tconf2(1,ic) = xloc(1)
        tconf2(2,ic) = xloc(2)
        tconf2(3,ic) = xloc(3)
!
!  1.4   CALCUL DE LA DISTANCE NORMALE DU NOEUD DE CHOC
!        A LA BUTEE A L'INSTANT N+1
!
        typobs = typch(ic)
        nbs = nbseg(ic)
        if ((typobs.eq.0) .or. (typobs.eq.1) .or. (typobs.eq.2)) xjeu = rc(1,ic)
!
        call disbut(np3, ic, xloc, typobs, xjeu,&
                    rc, theta, nbs, cost, sint,&
                    dnorm)
!
        tconf2(4,ic) = dnorm
!
        if (abs(dnorm) .lt. ftest) ftest = abs(dnorm)
        if (abs(dnorm) .lt. tolch) dnorm = zero
!
!  1.5   EVOLUTION DU NOEUD DE CHOC PAR RAPPORT A LA BUTEE ENTRE LES
!        INSTANTS N ET N+1
!
!  1.5.1 CHOC A L'INSTANT N+1
!
        if (dnorm .lt. zero) then
!
!  1.5.1.1  CHOC OU CONTACT EXACT A L'INSTANT N
!
            if (dnorm0 .le. zero) then
!
                call vardec(xloc, xloc0, ivar, dvar, tolc)
                if (ivar .eq. 1) then
                    iconfb(ic) = -1
                else
                    iconfb(ic) = 1
                endif
!
!  1.5.1.2  VOL A L'INSTANT N
!
            else
!
                nbch = nbch + 1
                call vardec(xloc, xloc0, ivar, dvar, tolv)
                if (ivar .eq. 1) then
                    iconfb(ic) = -1
                else
                    iconfb(ic) = 0
                endif
!
            endif
!
!  1.5.2 CONTACT EXACT A L'INSTANT N+1
!
        else if (dnorm.eq.zero) then
!
!  1.5.2.1  CHOC A L'INSTANT N
!
            if (dnorm0 .lt. zero) then
!
                nbch = nbch + 1
                nbchex = nbchex + 1
                call vardec(xloc, xloc0, ivar, dvar, tolc)
                if (ivar .eq. 1) then
                    iconfb(ic) = -1
                else
                    iconfb(ic) = 0
                endif
!
!  1.5.2.2  CONTACT EXACT A L'INSTANT N
!
            else if (dnorm0.eq.zero) then
!
                call vardec(xloc, xloc0, ivar, dvar, tolc)
                if (ivar .eq. 1) then
                    iconfb(ic) = -1
                else
                    iconfb(ic) = 1
                endif
!
!  1.5.2.3  VOL A L'INSTANT N
!
            else
!
                nbch = nbch + 1
                nbchex = nbchex + 1
                call vardec(xloc, xloc0, ivar, dvar, tolv)
                if (ivar .eq. 1) then
                    iconfb(ic) = -1
                else
                    iconfb(ic) = 0
                endif
!
            endif
!
!  1.5.3 VOL A L'INSTANT N+1
!
        else
!
!  1.5.3.1  CHOC A L'INSTANT N
!
            if (dnorm0 .lt. zero) then
!
                nbch = nbch + 1
                call vardec(xloc, xloc0, ivar, dvar, tolc)
                if (ivar .eq. 1) then
                    iconfb(ic) = -1
                else
                    iconfb(ic) = 0
                endif
!
!  1.5.3.2  CONTACT EXACT OU VOL A L'INSTANT N
!
            else
!
                call vardec(xloc, xloc0, ivar, dvar, tolv)
                if (ivar .eq. 1) then
                    iconfb(ic) = -1
                else
                    iconfb(ic) = 1
                endif
!
            endif
!
        endif
!
10  end do
!
!  2. AFFECTATION DE L'INDICATEUR DE CHANGEMENT DE CONFIGURATION
!     ----------------------------------------------------------
!
    iconf = 1
    do 20 ic = 1, nbnl
        if (iconfb(ic) .eq. 0) then
            iconf = 0
        else if (iconfb(ic).eq.-1) then
            iconf = -1
            goto 999
        endif
20  end do
!
999  continue
!
! --- FIN DE TESTCH.
end subroutine

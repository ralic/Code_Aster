subroutine nmlmab(pgl, nno, npg, nc, ugl,&
                  effnom, tempm, tempp, imate, crit,&
                  tmoins, tplus, xlong0, e, a,&
                  coelma, irram, irrap, varim, varip,&
                  kls, flc, effnoc, em, iret)
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
! ----------------------------------------------------------------------
! aslint: disable=W1504
    implicit none
    include 'asterfort/granac.h'
    include 'asterfort/nmcri3.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/utpvgl.h'
    include 'asterfort/verifm.h'
    include 'asterfort/zeroco.h'
    include 'blas/dcopy.h'
    integer :: nno, npg, nc, neq, nbt, ncoefl, niter, imate
!-----------------------------------------------------------------------
    integer :: iret, it, nchan
!-----------------------------------------------------------------------
    parameter (neq = 12, nbt = 78, ncoefl = 12)
    real(kind=8) :: crit(3)
    real(kind=8) :: e, a, xlong0, em
    real(kind=8) :: irram, irrap, coelma(ncoefl)
    real(kind=8) :: tmoins, tplus, effnom, tempm, tempp
    real(kind=8) :: ugl(neq), pgl(3, 3)
    real(kind=8) :: varim(21), varip(21)
    real(kind=8) :: kls(nbt), effnoc, flc, depsv, rigela
! ----------------------------------------------------------------------
!
!     TRAITEMENT DE LA RELATION DE COMPORTEMENT NON LINEAIRE
!     DE FLUAGE DU LMAB ET DE GRANDISSEMENT POUR LES ELEMENTS DE
!     POUTRE : CALCUL DE LA MATRICE DE RAIDEUR TANGENTE ET DES FORCES
!     NODALES.
!
!
! ----------------------------------------------------------------------
! IN  :
!       COMPOR : NOM DE LA RELATION DE COMPORTEMENT
!       PGL    : MATRICE DE PASSAGE
!       NNO    : NOMBRE DE NOEUDS
!       NC     : NOMBRE DE DDL
!       UGL    : ACCROIS. DEPLACEMENTS EN REPERE GLOBAL
!       EFFNOM : EFFORT NORMAL ELASTIQUE PRECEDENT
!       TEMPM  : TEMPERATURE IMPOSEE A L'INSTANT PRECEDENT
!       TEMPP  : TEMPERATURE IMPOSEE A L'INSTANT COURANT
!       IRET   : CODE RETOUR SUR LA TEMPERATURE
!       CRIT   : CRITERES DE CONVERGENCE LOCAUX
!       TMOINS : INSTANT PRECEDENT
!       TPLUS  : INSTANT COURANT
!       XLONG0 : LONGUEUR DE L'ELEMENT DE POUTRE AU REPOS
!       E      : MODULE D'YOUNG A L'INSTANT COURANT
!       EM     : MODULE D'YOUNG A L'INSTANT PRECEDENT
!       A      : SECTION DE LA POUTRE
!       EPSTHE  : DILATATION THERMIQUE
!       COELMA : COEFFICIENTS CONSTANTS POUR LE FLUAGE
!       IRRAM : FLUX NEUTRONIQUE A L'INSTANT PRECEDENT
!       IRRAP : FLUX NEUTRONIQUE A L'INSTANT COURANT
!       VARIM  : VARIABLE INTERNE A L'INSTANT PRECEDENT
!       VARIP  : VARIABLE INTERNE A L'INSTANT COURANT
!
! OUT : KLS    : SOUS MATRICE DE RAIDEUR TANGENTE EN REPERE LOCAL
!       FLC    : FORCE NODALE AXIALE CORRIGEE EN REPERE LOCAL
!       EFFNOC : EFFORT NORMAL CORRIGE
!
! ----------------------------------------------------------------------
! *************** DECLARATION DES VARIABLES LOCALES ********************
    real(kind=8) :: ul(12), dlong0, deltat, prec, ba, bb, fa, fb
    real(kind=8) :: depgrd, depthe, depst, xrig, correc, sig, yg, sigp
    real(kind=8) :: epsv, xx, x1, x2, dx, dx1, dx2, coeflm(ncoefl), sigsig, ier
!      EXTERNAL NMCRI3
    real(kind=8) :: x(4), y(4), xi
    integer :: i
!
! ----------------------------------------------------------------------
    common/rconm3/coeflm,epsv,depst,sig,xx,x1,x2,dx,dx1,dx2,deltat,yg,&
     &               sigsig,depthe,depgrd,ier
! ----------------------------------------------------------------------
!
!-- INITIALISATION DES VARIABLES
!
    call r8inir(nbt, 0.d0, kls, 1)
    call r8inir(12, 0.d0, ul, 1)
    yg=e
!
    call dcopy(ncoefl, coelma, 1, coeflm, 1)
!
    epsv = varim(1)
    xx = varim(2)
    x1 = varim(3)
    x2 = varim(4)
!
    call utpvgl(nno, nc, pgl, ugl, ul)
!
    dlong0 = ul(7) - ul(1)
    deltat = tplus - tmoins
    sig = (e/em) * (effnom/a)
    niter = int(crit(1))
    prec = crit(3)*1.d-3
!     PREC = CRIT(3)**2
    prec = crit(3)
!
    if (abs(sig-xx) .lt. prec) then
        sigsig = dlong0/abs(dlong0)
    else
        sigsig = (sig-xx)/abs(sig-xx)
    endif
!
!-- CALCUL DES INCREMENTS DE DEFORMATION
    call verifm('RIGI', npg, 1, 'T', imate,&
                'ELAS', 1, depthe, iret)
    depst = dlong0/xlong0
!
    if (iret .eq. 0) then
        call granac('RIGI', 1, 1, imate, '        ',&
                    'LMARC_IRRA      ', irrap, irram, tempm, tempp,&
                    depgrd)
        varip(21) = varim(21) + depgrd
    else
        depgrd = 0.d0
    endif
!
50  continue
    ba = 0.d0
    bb = abs(depst + epsv)
!
!     RECHERCHE DU ZERO DE LA FONCTION NMCRI3
!     ANCIEN : CALL ZEROF3 (NMCRI3,BA,BB,PREC,NITER,SOLU)
!
!     EXAMEN DE LA SOLUTION X = 0
    nchan = 0
    fa = nmcri3(ba)
    if (abs(fa) .le. prec) then
        xi = ba
        goto 200
    else if (fa.lt.0.d0) then
        x(1) = ba
        y(1) = fa
!        EXAMEN DE LA SOLUTION X = BB
!        FA < 0 ON CHERCHE DONC BB TEL QUE FB > 0
        do 10 i = 1, 5
            fb = nmcri3(bb)
            if (ier .gt. 0.5d0) goto 11
            if (fb .ge. 0.d0) then
                x(2) = bb
                y(2) = fb
                goto 30
            else
                bb = bb * 10.d0
            endif
10      continue
11      continue
        call u2mess('A', 'ALGORITH8_12')
        goto 40
    else
        x(2) = ba
        y(2) = fa
!        EXAMEN DE LA SOLUTION X = BB
!        FA > 0 ON CHERCHE DONC BB TEL QUE FB < 0
        do 20 i = 1, 5
            fb = nmcri3(bb)
            if (ier .gt. 0.5d0) goto 21
            if (fb .le. 0.d0) then
                x(1) = bb
                y(1) = fb
                goto 30
            else
                bb = bb * 10.d0
            endif
20      continue
21      continue
        call u2mess('A', 'ALGORITH8_13')
        goto 40
    endif
40  continue
    nchan = nchan + 1
    if (nchan .gt. 1) then
        call u2mess('F', 'ALGORITH8_14')
    else
        sigsig = -sigsig
        call u2mess('A', 'ALGORITH8_15')
        goto 50
    endif
30  continue
!
!     CALCUL DE X(4) SOLUTION EQUATION SCALAIRE F=0
!
    x(3) = x(1)
    y(3) = y(1)
    x(4) = x(2)
    y(4) = y(2)
!
    do 100 it = 1, niter
        if (abs(y(4)) .lt. prec) goto 110
        call zeroco(x, y)
        xi = x(4)
        y(4) = nmcri3(xi)
100  end do
    call u2mess('F', 'ALGORITH8_16')
110  continue
!
200  continue
    depsv = sigsig * xi
    sigp = sig + e*(depst-depthe-depgrd-depsv)
!
    effnoc = sigp*a
    varip(1) = varim(1) + xi
    varip(2) = varim(2) + dx
    varip(3) = varim(3) + dx1
    varip(4) = varim(4) + dx2
!
!-- CALCUL DES COEFFICIENTS NON ELASTIQUES DE LA MATRICE TANGENTE
!
    correc = 0.d0
    rigela = e*a/xlong0
    xrig = rigela / (1.d0 + correc)
    kls(1) = xrig
    kls(22) = -xrig
    kls(28) = xrig
!
!-- CALCUL DES FORCES NODALES
!
    flc = effnoc
!
! ----------------------------------------------------------------------
!
end subroutine

subroutine dpvpot(mod, vim, vip, nbmat, mater,&
                  sig, dt, dp, plas, dsidep)
!
    implicit      none
    include 'asterfort/dpvpdv.h'
    include 'asterfort/dpvpva.h'
    include 'asterfort/lcdevi.h'
    include 'asterfort/lceqma.h'
    include 'asterfort/lcinma.h'
    include 'asterfort/lcinve.h'
    include 'asterfort/lcopli.h'
    include 'asterfort/lcprmv.h'
    include 'asterfort/lcprsc.h'
    include 'asterfort/lcprsm.h'
    include 'asterfort/lcprsv.h'
    include 'asterfort/lcprte.h'
    include 'asterfort/lcsoma.h'
    include 'asterfort/lcsove.h'
    include 'asterfort/lctrma.h'
    include 'asterfort/trace.h'
    integer :: ndt, ndi
    integer :: nbmat
    real(kind=8) :: dt, dp, plas
    real(kind=8) :: mater(nbmat, 2), vim(4), vip(4), sig(6), dsidep(6, 6)
    character(len=8) :: mod
! =====================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! =====================================================================
! --- BUT   OPERATEUR TANGENT COHERENT POUR LA LOI --------------------
! --- VISC_DRUC_PRAG --------------------------------------------------
! =====================================================================
    integer :: ii, jj
    real(kind=8) :: zero, un, deux, trois, neuf, unstr
    real(kind=8) :: troisk, deuxmu, k, mu
    real(kind=8) :: pref, a, n
    real(kind=8) :: fonc1, fonc2, fonc3, fonc4, fonc, foncp
    real(kind=8) :: beta
    real(kind=8) :: alpham, betam, rm
    real(kind=8) :: dalpdp, dbetdp, drdp
    real(kind=8) :: fonecm(3), fonecp(3), fonder(3)
    real(kind=8) :: sii, seq, i1
    real(kind=8) :: scal1, scal3, scal4, scal5, scal6
    real(kind=8) :: scal12
    real(kind=8) :: denom, dfdp, const, const1
    real(kind=8) :: kron(6)
    real(kind=8) :: dsede(6, 6)
    real(kind=8) :: s(6)
    real(kind=8) :: dsdsig(6, 6), dqdsig(6), dfdsig(6), dpdsig(6)
    real(kind=8) :: adidsi(6), bdidsi(6), cdidsi(6)
    real(kind=8) :: dgdsig(6)
    real(kind=8) :: dqdeps(6, 6), dsdeps(6, 6), dpdeps(6)
    real(kind=8) :: di1ede(6), di1de(6)
    real(kind=8) :: vect1(6), vect2(6), vect3(6), vect4(6)
    real(kind=8) :: matr1(6, 6), matr2(6, 6), matr3(6, 6)
    real(kind=8) :: matr1a(6, 6), matr1b(6, 6)
    real(kind=8) :: part1(6, 6), part2(6, 6), part3(6, 6), part4(6, 6)
    real(kind=8) :: inter1(6, 6), inter2(6, 6), inter3(6, 6)
    real(kind=8) :: int2a(6, 6), int2b(6, 6), dsdept(6, 6)
    real(kind=8) :: tol
! =====================================================================
    parameter  ( zero  = 0.0d0 )
    parameter  ( un    = 1.0d0 )
    parameter  ( deux  = 2.0d0 )
    parameter  ( trois = 3.0d0 )
    parameter  ( neuf  = 9.0d0 )
    parameter  ( unstr = 1.d0/3.0d0)
    parameter  ( tol = 1.d-12)
!
! =====================================================================
    common /tdim/   ndt , ndi
! =================================================================
    data   kron /un , un , un , zero ,zero ,zero/
! =================================================================
! ---- RECUPERATION DES PARAMETRES MATERIAUX ----------------------
! =================================================================
!
    mu = mater(4,1)
    k = mater(5,1)
    pref = mater(1,2)
    a = mater(2,2)
    n = mater(3,2)
    troisk = trois*k
    deuxmu = deux*mu
! =====================================================================
! --- INITIALISATIONS DES VECTEURS ------------------------------------
! =====================================================================
    call lcinve(0.0d0, vect1)
    call lcinve(0.0d0, vect2)
    call lcinve(0.0d0, vect3)
    call lcinve(0.0d0, vect4)
    call lcinve(0.0d0, dqdsig)
    call lcinve(0.0d0, dfdsig)
    call lcinve(0.0d0, dpdsig)
    call lcinve(0.0d0, dgdsig)
    call lcinve(0.0d0, dpdeps)
    call lcinve(0.0d0, di1ede)
    call lcinve(0.0d0, di1de)
    call lcinve(0.0d0, adidsi)
    call lcinve(0.0d0, bdidsi)
    call lcinve(0.0d0, cdidsi)
    call lcinve(0.0d0, s)
! =====================================================================
! --- INITIALISATIONS DES MATRICES ------------------------------------
! =====================================================================
    call lcinma(0.0d0, matr1)
    call lcinma(0.0d0, matr1a)
    call lcinma(0.0d0, matr1b)
    call lcinma(0.0d0, matr2)
    call lcinma(0.0d0, matr3)
    call lcinma(0.0d0, part1)
    call lcinma(0.0d0, part2)
    call lcinma(0.0d0, part3)
    call lcinma(0.0d0, part4)
    call lcinma(0.0d0, inter1)
    call lcinma(0.0d0, inter2)
    call lcinma(0.0d0, int2a)
    call lcinma(0.0d0, int2b)
    call lcinma(0.0d0, inter3)
    call lcinma(0.0d0, dsdsig)
    call lcinma(0.0d0, dsdeps)
    call lcinma(0.0d0, dsdept)
    call lcinma(0.0d0, dqdeps)
    call lcinma(0.0d0, dsidep)
    call lcinma(0.0d0, dsede)
!
    call lcopli('ISOTROPE', mod, mater(1, 1), dsede)
! =====================================================================
! --- CAS ELASTIQUE ---------------------------------------------------
    if ((plas.eq.0.0d0) .or. (dp.eq.0.d0) .or. (abs(dp).lt.tol)) then
        call lceqma(dsede, dsidep)
        goto 9999
    else
! =================================================================
! ----  CALCUL DU DEVIATEUR - DE LA CONTRAINTE EQUIVALENTE  -------
! ----  ET DE LA TRACE --------------------------------------------
! =================================================================
        call lcdevi(sig, s)
        call lcprsc(s, s, sii)
        seq = sqrt(trois*sii/deux)
        i1 = trace (ndi,sig)
!
! =====================================================================
! --- FONCTIONS D ECROUISSAGE ET LEURS DERIVEES------------------------
! =====================================================================
        call dpvpva(vim, nbmat, mater, fonecm)
        call dpvpva(vip, nbmat, mater, fonecp)
        call dpvpdv(vip, nbmat, mater, fonder)
!
        alpham = fonecm(1)
        rm = fonecm(2)
        betam = fonecm(3)
!
        beta = fonecp(3)
!
        dalpdp = fonder(1)
        drdp = fonder(2)
        dbetdp = fonder(3)
!
        const = a*dt/(pref)**n
! =====================================================================
! --- CALCUL DE DSIDEP ------------------------------------------------
! =====================================================================
        do 30 ii = 1, ndi
            do 40 jj = 1, ndi
                dsdsig(ii,jj) = - un/trois
40          continue
30      continue
        do 50 ii = 1, ndt
            dsdsig(ii,ii) = dsdsig(ii,ii) + un
50      continue
!
!
! =====================================================================
! --- CALCUL DE LA TROISIEME PARTIE DU TERME DS/DEPS ------------------
! =====================================================================
! --- CALCUL DE DSIEQ/ DSIG -------------------------------------------
! =====================================================================
!
        scal1 = trois/deux/seq
!
        call lcprmv(dsdsig, s, dqdsig)
        call lcprsv(scal1, dqdsig, dqdsig)
! =====================================================================
! --- CALCUL DE ALPHA * DI1/ DSIG -------------------------------------
! =====================================================================
        call lcprsv(alpham, kron, adidsi)
! =====================================================================
! --- CALCUL DE ALPHA_CONS * DI1/ DSIG * DP ---------------------------
! =====================================================================
        scal12 = dalpdp * dp
        call lcprsv(scal12, kron, bdidsi)
! =====================================================================
! --- CALCUL DE Df/ DSIG ----------------------------------------------
! =====================================================================
        call lcsove(dqdsig, adidsi, cdidsi)
        call lcsove(cdidsi, bdidsi, dfdsig)
! =====================================================================
! --- CALCUL DE DfDp --------------------------------------------------
! =====================================================================
        fonc1 = seq + alpham * i1 - rm
!
        fonc2 = trois*mu + drdp - dalpdp*i1 +neuf*k *alpham*betam
!
        fonc3 = neuf*k*(alpham*dbetdp+betam*dalpdp)
!
        fonc4 = neuf*k*dalpdp*dbetdp
!
!
        fonc = fonc1 - fonc2*dp - fonc3*dp**2 - fonc4*dp**3
        foncp = -fonc2 -deux*dp*fonc3-trois*dp**2*fonc4
!
        if (fonc .gt. zero) then
            fonc = fonc
        else
            call lceqma(dsede, dsidep)
            goto 9999
        endif
!
        const1 = n * const * fonc**(n-un)
        dfdp = const1 * foncp - un
!
        if (dfdp .eq. zero) then
            call lceqma(dsede, dsidep)
            goto 9999
        else
            denom = -un / dfdp
        endif
! =====================================================================
! --- CALCUL DE Df/ DSIG ----------------------------------------------
! =====================================================================
        call lcprsv(const1, dfdsig, dfdsig)
! =====================================================================
! --- CALCUL DE d deltap/dSIG -----------------------------------------
! =====================================================================
        call lcprsv(denom, dfdsig, dpdsig)
! =====================================================================
! --- CALCUL DE d deltap/dEPS -----------------------------------------
! =====================================================================
        call lcprmv(dsede, dpdsig, dpdeps)
! =====================================================================
! --- CALCUL DE 3GDT/SEQ *se * deltap/dEPS ----------------------------
! =====================================================================
        call lcprte(dpdeps, s, matr1a)
! =====================================================================
! --- TRANSPOSEE ------------------------------------------------------
! =====================================================================
        call lctrma(matr1a, matr1b)
! =====================================================================
! --- SYMETRISATION  --------------------------------------------------
! =====================================================================
        do 68 ii = 1, ndt
            do 67 jj = 1, ndt
                matr1(ii,jj) = un/deux*(matr1a(ii,jj)+matr1b(ii,jj))
67          continue
68      continue
!
        scal3 = -trois*mu/seq
!
        call lcprsm(scal3, matr1, part3)
!
! =====================================================================
! --- CALCUL DE LA  PREMIERE PARTIE DU TERME DS/DEPS ------------------
! =====================================================================
! --- CALCUL DE dse / deps *(1-3GDP/SEQ) ----------------------------
! =====================================================================
        scal4 = deuxmu * (un - trois*mu * dp / seq)
        call lcprsm(scal4, dsdsig, part1)
! =====================================================================
! --- CALCUL DE LA  DEUXIEME PARTIE DU TERME DS/DEPS ------------------
! =====================================================================
! --- CALCUL DE 3GDP/SEQ**2 *(se * dSEQ/dEPS ------------------------
! =====================================================================
        scal5 = neuf * mu *mu* dp/seq/ seq/ seq
        call lcprte(s, s, matr3)
!
        call lcprsm(scal5, matr3, part2)
! =====================================================================
! --- SOMMATION DES PARTIES DE ds/dEPS --------------------------------
! =====================================================================
        call lcsoma(part1, part2, inter1)
!
        call lcsoma(inter1, part3, dsdeps)
!
        call lctrma(dsdeps, dsdept)
        do 980 ii = 1, ndt
            do 990 jj = 1, ndt
                dsdeps(ii,jj) = un/deux*(dsdeps(ii,jj)+dsdept(ii,jj))
990          continue
980      continue
! =====================================================================
! --- CALCUL  DU TERME DI/DEPS ----------------------------------------
! =====================================================================
! --- CALCUL DE dI1E/dEPS ---------------------------------------------
! =====================================================================
        call lcprsv(troisk, kron, di1ede)
! =====================================================================
! --- CALCUL DE 9KBETAdp/dEPS -----------------------------------------
! =====================================================================
        scal6 = -neuf*k*beta
        call lcprsv(scal6, dpdeps, vect2)
! =====================================================================
! --- CALCUL DE dI1/dEPS ----------------------------------------------
! =====================================================================
        call lcsove(di1ede, vect2, di1de)
! =====================================================================
! --- CALCUL DE I * dI/dEPS -------------------------------------------
! =====================================================================
        call lcprte(kron, di1de, int2a)
! =====================================================================
! --- TRANSPOSEE DE I * dI/dEPS ---------------------------------------
! =====================================================================
        call lctrma(int2a, int2b)
! =====================================================================
! --- SYMETRISATION  --------------------------------------------------
! =====================================================================
        do 98 ii = 1, ndt
            do 99 jj = 1, ndt
                inter2(ii,jj) = un/deux*(int2a(ii,jj)+int2b(ii,jj))
99          continue
98      continue
        call lcprsm(unstr, inter2, inter2)
        call lcsoma(dsdeps, inter2, dsidep)
    endif
! =====================================================================
9999  continue
! =====================================================================
end subroutine

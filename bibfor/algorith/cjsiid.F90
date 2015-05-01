subroutine cjsiid(mod, mater, epsd, deps, yd,&
                  gd, dy)
    implicit none
! ======================================================================
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
!
!     ------------------------------------------------------------------
!     LOI CJS :  MECANISMES ISOTROPE ET DEVIATOIRE
!     CALCUL DE LA SOLUTION D ESSAI EXPLICITE DY = (DSIG, DVIN, DLAMB )
!                           AVEC               Y = ( SIG,  VIN,  LAMB )
!     PARTIR DE LA PREDICTION ELASTIQUE
!     ------------------------------------------------------------------
!     IN   MOD      :  MODELISATION
!          MATER    :  COEFFICIENTS MATERIAU A T+DT
!          EPSD     :  DEFORMATION A T+DT
!          DEPS     :  INCREMENT DE DEFORMATION
!          YD       :  VARIABLES A T = (SIGD, VIND, LAMB)
!     VAR  GD       :  TENSEUR DE LA LOI D ECOULEMENT PLASTIQUE DEV.
!     OUT  DY       :  SOLUTION D ESSAI (DSIG, DVIN, DLAMB)
!     ------------------------------------------------------------------
!     Y CONTIENT LES CONTRAINTES : SIG
!                LES VARIABLES INTERNES : QISO, R, X
!                LES MULTIPLICATEURS PLASTIQUES : LAMBI, LAMBD
! ======================================================================
#include "asterfort/calcq.h"
#include "asterfort/cjsqco.h"
#include "asterfort/cjst.h"
#include "asterfort/cos3t.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/lcinma.h"
#include "asterfort/lcprmv.h"
#include "asterfort/lcprsc.h"
#include "asterfort/lcsove.h"
#include "asterfort/trace.h"
#include "asterfort/utmess.h"
    integer :: ndt, ndi, i, j, codret
    real(kind=8) :: epsd(6), deps(6), depse(6), trdeps, hooknl(6, 6)
    real(kind=8) :: dsig(6), sigd(6), sige(6), qiso, gqiso, dqiso
    real(kind=8) :: yd(*), dy(*), rcos3t, dq(6), dqe(6)
    real(kind=8) :: mater(14, 2), n, rm, rc, a, b, c, pco, pc, pa
    real(kind=8) :: ke, koe, kp, kop, beta, betapr, gamma, mucjs
    real(kind=8) :: e, nu, al, la, mu, i1d, i1e
    real(kind=8) :: truc, signe, dlambi, dlambd, denomi
    real(kind=8) :: s(6), sii, siic, hts, dets, cos3ts, siirel
    real(kind=8) :: se(6), siie, htse, detse, co3tse, siiere
    real(kind=8) :: q(6), qii, htq, detq, cos3tq, qiirel
    real(kind=8) :: qe(6), qiie, htqe, detqe, co3tqe, qiiere
    real(kind=8) :: tangs, tangq, tetas, tetaq, kron(6)
    real(kind=8) :: qq(6), qqii, norm(6), dfdds(6), gd(6), trgd
    real(kind=8) :: coef1, coef3, coef4, coef5, coef6, prod1
    real(kind=8) :: rd, xd(6), gr, gx(6), xii, dr, dx(6)
    real(kind=8) :: epsv, phi, phio, rr, cosa, cosdif
    real(kind=8) :: fi, fd, dfidli, dfidld, dfddli, dfddld
    real(kind=8) :: drdli, drdld, di1dli, di1dld, dqdld(6), dqdli(6)
    real(kind=8) :: dq2dli, dq2dld, dhdli, dhdld
    real(kind=8) :: mun5, zero, un, d12, deux, trois, cinq
    real(kind=8) :: epssig, pref, qinit
    character(len=8) :: mod
! ======================================================================
    parameter     ( mun5 =-1.5d0  )
    parameter     ( d12  = .5d0   )
    parameter     ( un   = 1.d0   )
    parameter     ( zero = 0.d0   )
    parameter     ( deux = 2.d0   )
    parameter     ( trois= 3.d0   )
    parameter     ( cinq = 5.d0   )
    parameter     ( epssig = 1.d-8 )
! ======================================================================
    common /tdim/   ndt, ndi
    data          kron /un , un , un , zero ,zero ,zero/
! ======================================================================
    call jemarq()
! ======================================================================
! --- PROPRIETES CJS MATERIAU ------------------------------------------
! ======================================================================
    beta = mater(1,2)
    rm = mater(2,2)
    n = mater(3,2)
    kop = mater(4,2)
    rc = mater(5,2)
    a = mater(6,2)
    b = mater(7,2)
    c = mater(8,2)
    gamma = mater(9,2)
    mucjs = mater(10,2)
    pco = mater(11,2)
    pa = mater(12,2)
    koe = mater(1,1)/trois/( un-deux*mater(2,1) )
    qinit = mater(13,2)
! ======================================================================
! --- PREMIER INVARIANT ET AUTRES GRANDEURS UTILES ---------------------
! ======================================================================
    i1d = trace(ndi,yd)
    if ((i1d+qinit) .eq. 0.d0) then
        i1d = -qinit+1.d-12 * pa
        pref = abs(pa)
    else
        pref = abs(i1d+qinit)
    endif
    qiso = yd(ndt+1)
    rd = yd(ndt+2)
    do 15 i = 1, ndt
        xd(i)= yd(ndt+2+i)
15  continue
    ke = koe * ( (i1d+qinit)/trois/pa )**n
    kp = kop * ( qiso/pa )**n
! ======================================================================
! --- OPERATEUR DE RIGIDITE NON LINEAIRE -------------------------------
! ======================================================================
! --- OPERATEUR LINEAIRE NON LINEAIRE ----------------------------------
! ======================================================================
    call lcinma(zero, hooknl)
!
    e = mater(1,1) * ((i1d+qinit)/trois/pa)**n
    nu = mater(2,1)
    al = e * (un-nu) / (un+nu) / (un-deux*nu)
    la = nu * e / (un+nu) / (un-deux*nu)
    mu = e * d12 / (un+nu)
! ======================================================================
! --- 3D/DP/AX ---------------------------------------------------------
! ======================================================================
    if (mod(1:2) .eq. '3D' .or. mod(1:6) .eq. 'D_PLAN' .or. mod(1:4) .eq. 'AXIS') then
        do 20 i = 1, ndi
            do 20 j = 1, ndi
                if (i .eq. j) hooknl(i,j) = al
                if (i .ne. j) hooknl(i,j) = la
20          continue
        do 25 i = ndi+1, ndt
            do 25 j = ndi+1, ndt
                if (i .eq. j) hooknl(i,j) = deux* mu
25          continue
! ======================================================================
! --- CP/1D ------------------------------------------------------------
! ======================================================================
    else if (mod(1:6) .eq. 'C_PLAN' .or. mod(1:2) .eq. '1D') then
        call utmess('F', 'ALGORITH2_15')
    endif
! ======================================================================
! --- LOIS D ECROUISSAGE : GQISO, GR ET GX -----------------------------
! ======================================================================
! --- ECROUISSAGE ISOTROPE DU MECANISME ISOTROPE -----------------------
! ======================================================================
    gqiso = - kp
! ======================================================================
! --- ECROUISSAGE ISOTROPE DU MECANISME DEVIATOIRE ---------------------
! ======================================================================
    coef1 = ( (i1d+qinit)/trois/pa )**mun5
    gr = - a * (un-rd/rm)**deux * (i1d+qinit) * coef1
! ======================================================================
! --- ECROUISSAGE CINEMATIQUE DU MECANISME DEVIATOIRE ------------------
! ======================================================================
    do 30 i = 1, ndt
        sigd(i) = yd(i)
30  continue
! ======================================================================
! --- ON CALCULE DE TOUTES FACONS UNE PREDICTION -----------------------
! --- ELASTIQUE EN TANT QUE DE BESOIN ----------------------------------
! ======================================================================
    call lcprmv(hooknl, deps, dsig)
    call lcsove(sigd, dsig, sige)
    i1e = trace(ndi, sige)
    if ((i1e+qinit) .eq. zero) then
        i1e = -qinit+1.d-12*pa
    endif
! ======================================================================
! --- ON CALCULE DE TOUTES FACONS UNE PREDICTION -----------------------
! --- SE, SIIE, ... A PARTIR DE LA PREDICTION ELASTIQUE ----------------
! --- EN TANT QUE DE BESOIN --------------------------------------------
! ======================================================================
    call cjsqco(gamma, sige, xd, pref, epssig,&
                i1e, se, siie, siiere, co3tse,&
                htse, detse, qe, qiie, qiiere,&
                co3tqe, htqe, detqe)
! ======================================================================
! --- CALCUL DE S, SII, COS3TS, .... -----------------------------------
! ======================================================================
    call cjsqco(gamma, sigd, xd, pref, epssig,&
                i1d, s, sii, siirel, cos3ts,&
                hts, dets, q, qii, qiirel,&
                cos3tq, htq, detq)
! ======================================================================
! --- SI QII EST QUASI-NULL, IL N'Y A PAS DE DEVIATEUR. ----------------
! --- DONC LE TENSEUR QQ(SIGD) N'EXISTE PAS. ON PRENDRA ALORS ----------
! --- A LA PLACE QQ(SIG_PREDICTION ELAS) -------------------------------
! ======================================================================
    if (qiirel .le. epssig) then
        call calcq(qe, gamma, pref, epssig, qq,&
                   codret)
    else
        call calcq(q, gamma, pref, epssig, qq,&
                   codret)
    endif
    call lcprsc(qq, qq, qqii)
    qqii = sqrt(qqii)
!
    call lcprsc(xd, xd, xii)
    xii = sqrt(xii)
!
    epsv = zero
    do 50 i = 1, ndi
        epsv = epsv + epsd(i)+ deps(i)
50  continue
!
    pc = pco*exp(-c*epsv)
!
    if (xii .le. epssig) then
        phi = un
    else if (siirel .le. epssig) then
        cosa = un
        cosdif = un
        rr = rc + mucjs*max(zero,log(trois*pc/(i1d+qinit)))
        phio = cosa/( rr - hts/htq*rm*cosdif)
        phi = phio * hts * qqii
    else
        cosa = ( qii*qii - sii*sii - i1d*i1d*xii*xii ) / (deux*sii* i1d*xii)
!
        tangs = sqrt(un-cos3ts*cos3ts) / cos3ts
        tangq = sqrt(un-cos3tq*cos3tq) / cos3tq
        tetas = atan2(tangs,1.d0) / trois
        tetaq = atan2(tangq,1.d0) / trois
        cosdif = cos(tetas-tetaq)
!
        rr = rc + mucjs*max(zero,log(trois*pc/(i1d+qinit)))
        phio = cosa/( rr - hts/htq*rm*cosdif)
        phi = phio * hts * qqii
    endif
!
    do 60 i = 1, ndt
        gx(i) = (i1d+qinit)/b*( qq(i) + phi*xd(i) ) * coef1
60  continue
! ======================================================================
! --- LOIS D ECOULEMENTS : ---------------------------------------------
! ======================================================================
! --- LOI D'ECOULEMENT DU MECANISME ISOTROPE ---------------------------
! --- ON NE LE CALCUL PAS CAR CA VAUT SIMPLEMENT -1/3 KRON(I) ----------
! ======================================================================
! --- LOI D'ECOULEMENT DU MECANISME DEVIATOIRE : GD --------------------
! ======================================================================
    call lcprsc(qq, xd, truc)
    truc = truc - rd
!
    do 70 i = 1, ndt
        dfdds(i) = qq(i) - truc*kron(i)
70  continue
! ======================================================================
! --- HYPOTHESE : SIGNE(S,DEPS) = SIGNE(S,DEPSDP) ----------------------
! ======================================================================
    call lcprsc(s, deps, truc)
    if (truc .ge. zero) then
        signe = un
    else
        signe = - un
    endif
!
    siic = -rc * (i1d+qinit) / hts
    betapr = beta * (sii/siic - un) * signe
    if (siirel .gt. epssig) then
        coef3 = betapr / sii
        coef4 = un / sqrt( betapr*betapr + trois )
!
        do 80 i = 1, ndt
            norm(i) = coef4 * ( coef3 * s(i) + kron(i) )
80      continue
    else
        coef3 = betapr / siie
        coef4 = un / sqrt( betapr*betapr + trois )
        do 81 i = 1, ndt
            norm(i) = coef4 * ( coef3 * se(i) + kron(i) )
81      continue
    endif
!
    call lcprsc(dfdds, norm, prod1)
    do 90 i = 1, ndt
        gd(i) = dfdds(i) - prod1 * norm(i)
90  continue
!
    trgd = trace(ndi, gd)
    trdeps = trace(ndi, deps)
! ======================================================================
! --- CALCUL DE DLAMBI, DLAMBD -----------------------------------------
! ======================================================================
! --- PAR RESOLUTION DU SYSTEME : --------------------------------------
!        _
!       (     D FI               D FI
!       (   --------  * DDLI + --------  * DDLD = - FI
!       (   D DLAMBI           D DLAMBD
!      -(
!       (     D FD               D FD
!       (   --------  * DDLI + --------  * DDLD = - FD
!       (_  D DLAMBI           D DLAMBD
!
! ======================================================================
! --- CALCUL DES SECONDS MEMBRES : FONCTIONS DE CHARGE FI ET FD --------
! ======================================================================
    fi = -(i1e+qinit)/trois + qiso
    fd = qiie * htqe + rd * (i1e+qinit)
! ======================================================================
! --- CALCUL DU PREMIER MEMBRE: DERIVEE DES FONCTIONS FI ET FD ---------
! --- PAR RAPPORT AUX VARIATIONS DES MULTIPLICATEURS PLASTIQUES --------
! ======================================================================
    di1dli = trois*ke
    di1dld = - trois*ke*trgd
    drdli = zero
    drdld = gr
!
    call lcprmv(hooknl, kron, dqdli)
    call lcprmv(hooknl, gd, dqdld)
    do 210 i = 1, ndt
        dqdli(i) = dqdli(i)/trois - ke*( kron(i) + trois*xd(i) )
        dqdld(i) = - dqdld(i) + ke*trgd*( kron(i) + trois*xd(i) ) - gx(i)*( i1d + trois*ke*trdeps&
                   & )
210  continue
! ======================================================================
! ---   SI QII EST QUASI-NULL, IL N'Y A PAS DE DEVIATEUR. --------------
! ======================================================================
    if (qiirel .le. epssig) then
        dq2dli = zero
        dq2dld = zero
        do 225 i = 1, ndt
            dq2dli = dq2dli + qe(i)/qiie * dqdli(i)
            dq2dld = dq2dld + qe(i)/qiie * dqdld(i)
225      continue
!
        rcos3t = cos3t(qe, pref, epssig)
        call cjst(qe, dqe)
        coef5 = sqrt(trois/deux)*gamma/htqe**cinq/qiie**trois
        coef6 = - gamma*rcos3t/(deux*htqe**cinq*qiie**deux)
!
        dhdli = zero
        dhdld = zero
        do 235 i = 1, ndt
            dhdli = dhdli + (coef5*dqe(i) + coef6*qe(i)) * dqdli(i)
            dhdld = dhdld + (coef5*dqe(i) + coef6*qe(i)) * dqdld(i)
235      continue
!
        dfidli = - di1dli/trois + gqiso
        dfidld = - di1dld/trois
        dfddli = htqe*dq2dli + qii*dhdli + rd*di1dli + (i1d+qinit)* drdli
        dfddld = htqe*dq2dld + qii*dhdld + rd*di1dld + (i1d+qinit)* drdld
    else
! ======================================================================
! --- SINON ------------------------------------------------------------
! ======================================================================
        dq2dli = zero
        dq2dld = zero
        do 220 i = 1, ndt
            dq2dli = dq2dli + q(i)/qii * dqdli(i)
            dq2dld = dq2dld + q(i)/qii * dqdld(i)
220      continue
!
        rcos3t = cos3t(q, pref, epssig)
        call cjst(q, dq)
        coef5 = sqrt(trois/deux)*gamma/htq**cinq/qii**trois
        coef6 = - gamma*rcos3t/(deux*htq**cinq*qii**deux)
!
        dhdli = zero
        dhdld = zero
        do 230 i = 1, ndt
            dhdli = dhdli + ( coef5*dq(i) + coef6*q(i) ) * dqdli(i)
            dhdld = dhdld + ( coef5*dq(i) + coef6*q(i) ) * dqdld(i)
230      continue
!
        dfidli = - di1dli/trois + gqiso
        dfidld = - di1dld/trois
        dfddli = htq*dq2dli + qii*dhdli + rd*di1dli + (i1d+qinit)* drdli
        dfddld = htq*dq2dld + qii*dhdld + rd*di1dld + (i1d+qinit)* drdld
    endif
!
    denomi = dfidli*dfddld - dfddli*dfidld
    dlambi = ( dfidld*fd - dfddld*fi ) / denomi
    dlambd = ( dfddli*fi - dfidli*fd ) / denomi
! ======================================================================
! --- CALCUL DES INCREMENTS DE DEFORMATIONS ELASTIQUE ------------------
! ======================================================================
    do 240 i = 1, ndt
        depse(i) = deps(i) + dlambi/trois*kron(i) - dlambd*gd(i)
240  continue
! ======================================================================
! --- CALCUL INCREMENT DE CONTRAINTES  DSIG = HOOKNL.DEPSE -------------
! ======================================================================
    call lcprmv(hooknl, depse, dsig)
! ======================================================================
! --- CALCUL INCREMENT DE LA VARIABLE INTERNE QISO ---------------------
! ======================================================================
    dqiso = dlambi*gqiso
! ======================================================================
! --- CALCUL INCREMENT DE LA VARIABLE INTERNE R ------------------------
! ======================================================================
    dr = dlambd*drdld
! ======================================================================
! --- CALCUL INCREMENT DE LA VARIABLE INTERNE X ------------------------
! ======================================================================
    do 250 i = 1, ndt
        dx(i) = dlambd*gx(i)
250  continue
! ======================================================================
! --- SOLUTION D ESSAI -------------------------------------------------
! ======================================================================
    do 260 i = 1, ndt
        dy(i) = dsig(i)
260  continue
!
    dy(ndt+1) = dqiso
    dy(ndt+2) = dr
!
    do 270 i = 1, ndt
        dy(ndt+2+i) = dx(i)
270  continue
!
    dy(2*ndt+3) = dlambi
    dy(2*ndt+4) = dlambd
! ======================================================================
    call jedema()
! ======================================================================
end subroutine

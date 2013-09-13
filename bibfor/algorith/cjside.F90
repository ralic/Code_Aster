subroutine cjside(mod, mater, epsd, deps, yd,&
                  gd, dy)
    implicit none
! ======================================================================
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
!     ------------------------------------------------------------------
!     LOI CJS :  MECANISME DEVIATOIRE
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
!                LES VARIABLES INTERNES :  R, X
!                LE MULTIPLICATEUR PLASTIQUE :  LAMBD
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
    real(kind=8) :: epsd(6), deps(6), depse(6), trdeps, kron(6)
    real(kind=8) :: dsig(6), sigd(6), sige(6), yd(*), dy(*), dqe(6)
    real(kind=8) :: mater(14, 2), n, rm, rc, a, b, c, pco, pc, pa
    real(kind=8) :: ke, koe, beta, betapr, gamma, mucjs, dq(6)
    real(kind=8) :: hooknl(6, 6), epssig, pref, qinit
    real(kind=8) :: e, nu, al, la, mu, i1d, i1e
    real(kind=8) :: truc, signe, dlambd, rcos3t
    real(kind=8) :: s(6), sii, siic, hts, dets, cos3ts, siirel
    real(kind=8) :: se(6), siie, htse, detse, co3tse, siiere
    real(kind=8) :: q(6), qii, htq, detq, cos3tq, qiirel
    real(kind=8) :: qe(6), qiie, htqe, detqe, co3tqe, qiiere
    real(kind=8) :: tangs, tangq, tetas, tetaq
    real(kind=8) :: qq(6), qqii, norm(6), dfdds(6), gd(6), trgd
    real(kind=8) :: coef1, coef3, coef4, coef5, coef6, prod1
    real(kind=8) :: rd, xd(6), gr, gx(6), xii, dr, dx(6)
    real(kind=8) :: epsv, phi, phio, rr, cosa, cosdif
    real(kind=8) :: fd, dfddl, drdl, di1dl, dqiidl, dhdl
    real(kind=8) :: dqdl(6), mun5, zero, un, d12, deux, trois, cinq
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
    data            kron /un , un , un , zero ,zero ,zero/
! ======================================================================
    call jemarq()
! ======================================================================
! --- PROPRIETES CJS MATERIAU ------------------------------------------
! ======================================================================
    beta = mater(1,2)
    rm = mater(2,2)
    n = mater(3,2)
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
    i1d = trace(ndi, yd)
    if ((i1d+qinit) .eq. 0.0d0) then
        i1d = -qinit+1.d-12 * pa
        pref = abs(pa)
    else
        pref = abs(i1d+qinit)
    endif
!
    rd = yd(ndt+1)
    do 15 i = 1, ndt
        xd(i)= yd(ndt+1+i)
15  continue
    ke = koe * ( (i1d+qinit)/trois/pa )**n
! ======================================================================
! --- OPERATEUR DE RIGIDITE NON LINEAIRE -------------------------------
! ======================================================================
! --- OPERATEUR LINEAIRE NON LINEAIRE ----------------------------------
! ======================================================================
    call lcinma(zero, hooknl)
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
! --- LOIS D ECROUISSAGE : GR ET GX ------------------------------------
! ======================================================================
! --- ECROUISSAGE ISOTROPE ---------------------------------------------
! ======================================================================
    coef1 = ( (i1d+qinit)/trois/pa )**mun5
    gr = - a * (un-rd/rm)**deux * (i1d+qinit) * coef1
! ======================================================================
! --- ECROUISSAGE CINEMATIQUE ------------------------------------------
! ======================================================================
    do 30 i = 1, ndt
        sigd(i) = yd(i)
30  continue
! ======================================================================
! --- CALCUL DE S, SII, COS3TS, .... -----------------------------------
! ======================================================================
    call cjsqco(gamma, sigd, xd, pref, epssig,&
                i1d, s, sii, siirel, cos3ts,&
                hts, dets, q, qii, qiirel,&
                cos3tq, htq, detq)
! ======================================================================
! --- ON CALCULE DE TOUTES FACONS UNE PREDICTION ELASTIQUE -------------
! ======================================================================
    call lcprmv(hooknl, deps, dsig)
    call lcsove(sigd, dsig, sige)
    i1e = trace(ndi, sige)
    if ((i1e +qinit) .eq. zero) then
        i1e = -qinit+1.d-12*pa
    endif
!
    call cjsqco(gamma, sige, xd, pref, epssig,&
                i1e, se, siie, siiere, co3tse,&
                htse, detse, qe, qiie, qiiere,&
                co3tqe, htqe, detqe)
!
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
! --- LOI D ECOULEMENT : GD --------------------------------------------
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
!
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
! ======================================================================
! --- CALCUL DE DLAMBD -------------------------------------------------
! ======================================================================
    fd = qiie * htqe + rd * (i1e+qinit)
    trgd = trace(ndi,gd)
    trdeps = trace(ndi,deps)
    di1dl = - trois*ke*trgd
    drdl = gr
    call lcprmv(hooknl, gd, dqdl)
    do 110 i = 1, ndt
        dqdl(i) = - dqdl(i) + ke*trgd*( kron(i) + trois*xd(i) ) - gx(i) * ( i1d + trois*ke*trdeps&
                  & )
110  continue
! ======================================================================
! --- POUR LE CALCUL DE DQIIDL, DHDL: ----------------------------------
! --- SI QII EST QUASI-NULL, IL N'Y A PAS DE DEVIATEUR. ----------------
! ======================================================================
    if (qiirel .le. epssig) then
        dqiidl = zero
        do 125 i = 1, ndt
            dqiidl = dqiidl + qe(i)/qiie * dqdl(i)
125      continue
!
        rcos3t = cos3t(qe, pref, epssig)
        call cjst(qe, dqe)
        coef5 = sqrt(trois/deux)*gamma/htqe**cinq/qiie**trois
        coef6 = - gamma*rcos3t/(deux*htqe**cinq*qiie**deux)
!
        dhdl = zero
        do 135 i = 1, ndt
            dhdl = dhdl + ( coef5*dqe(i) + coef6*qe(i) ) * dqdl(i)
135      continue
        dfddl = htqe*dqiidl + qii*dhdl + rd*di1dl + (i1d+qinit)*drdl
    else
        dqiidl = zero
        do 120 i = 1, ndt
            dqiidl = dqiidl + q(i)/qii * dqdl(i)
120      continue
!
        rcos3t = cos3t(q, pref, epssig)
        call cjst(q, dq)
        coef5 = sqrt(trois/deux)*gamma/htq**cinq/qii**trois
        coef6 = - gamma*rcos3t/(deux*htq**cinq*qii**deux)
!
        dhdl = zero
        do 130 i = 1, ndt
            dhdl = dhdl + ( coef5*dq(i) + coef6*q(i) ) * dqdl(i)
130      continue
        dfddl = htq*dqiidl + qii*dhdl + rd*di1dl + (i1d+qinit)*drdl
    endif
!
    dlambd = - fd/dfddl
! ======================================================================
! --- CALCUL DES INCREMENTS DE DEFORMATIONS ELASTIQUE ------------------
! ======================================================================
    do 140 i = 1, ndt
        depse(i) = deps(i) - dlambd*gd(i)
140  continue
! ======================================================================
! --- CALCUL INCREMENT DE CONTRAINTES  DSIG = HOOKNL.DEPSE -------------
! ======================================================================
    call lcprmv(hooknl, depse, dsig)
! ======================================================================
! --- CALCUL INCREMENT DE LA VARIABLE INTERNE R ------------------------
! ======================================================================
    dr = dlambd*drdl
! ======================================================================
! --- CALCUL INCREMENT DE LA VARIABLE INTERNE X ------------------------
! ======================================================================
    do 150 i = 1, ndt
        dx(i) = dlambd*gx(i)
150  continue
! ======================================================================
! --- SOLUTION D ESSAI -------------------------------------------------
! ======================================================================
    do 160 i = 1, ndt
        dy(i) = dsig(i)
160  continue
    dy(ndt+1) = dr
    do 170 i = 1, ndt
        dy(ndt+1+i) = dx(i)
170  continue
    dy(2*ndt+2) = dlambd
! ======================================================================
    call jedema()
! ======================================================================
end subroutine

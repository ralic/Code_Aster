subroutine nmchat(matel, mat, nbvar, memo, visc,&
                  plast, sigmdv, depsdv, pm, dp,&
                  ndimsi, dt, rpvp, qp, vim,&
                  idelta, n1, n2, beta1, beta2,&
                  dsidep)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jean-michel.proix at edf.fr
!.======================================================================
! aslint: disable=W1504
    implicit none
! ----ARGUMENTS
#include "asterc/r8prem.h"
#include "asterfort/matini.h"
#include "blas/ddot.h"
    integer :: memo, visc, nbvar, idelta
    real(kind=8) :: mat(*), matel(*), sigmdv(6), dsidep(6, 6), plast, dp, pm
    real(kind=8) :: rpvp
    real(kind=8) :: depsdv(6), vim(*), dt, qp, n1, n2, delta1, delta2, dn1, dn2
    real(kind=8) :: dbeta1, dbeta2, sigma(6), dsigma(6), sigdsi, vbeta(6)
    real(kind=8) :: beta1, beta2
! ----VARIABLES LOCALES
    real(kind=8) :: e, nu, deuxmu, sigedv(6), dside(6, 6), s(6), pdev(6, 6)
    real(kind=8) :: rpvm
    real(kind=8) :: alfam(6), alfa2m(6), rac2, mu, troisk, r0, rinf, b, cinf
    real(kind=8) :: k, w, gamma0, ainf, c2inf, gamm20, valden, kvi, pp, rp, cp
    real(kind=8) :: gammap
    real(kind=8) :: mp, c2p, gamm2p, m2p, vp, corr, dvp, denomi, dgamap, dcp
    real(kind=8) :: dmp
    real(kind=8) :: dgam2p, dc2p, dm2p, ap, ep, e2p, bp, b2p, ddenom, ip, dap
    real(kind=8) :: dep, de2p
    real(kind=8) :: dbp, db2p, seq, l1p, l2p, l22p, l3p, hp, h2p, isp, iap, ia2p
    real(kind=8) :: h1s
    real(kind=8) :: h1a1, h1a2, h2a1, h2a2, gq0, gqmax, mumem, qm, gqp, drp, h2s
    real(kind=8) :: rpm
    logical :: plasti
    integer :: ndimsi, i, j, l
!
    rac2 = sqrt(2.d0)
    plasti=(plast.ge.0.5d0)
    e = matel(3)
    nu = matel(4)
    deuxmu = e/(1.d0+nu)
    mu = deuxmu / 2.d0
    troisk = e/(1.d0-2.d0*nu)
    r0 = mat(1)
    rinf = mat(2)
    b = mat(3)
    cinf = mat(4)
    k = mat(5)
    w = mat(6)
    gamma0 = mat(7)
    ainf = mat(8)
    c2inf = mat(9)
    gamm20 = mat(10)
    if (visc .eq. 1) then
        valden= mat(11)
        kvi = mat(12)
    endif
    delta1 = mat(17)
    delta2 = mat(18)
!
    do 140 i = 1, ndimsi
        sigedv(i) = sigmdv(i) + deuxmu * depsdv(i)
140  end do
!
! --- MISE AU FORMAT DES CONTRAINTES DE RAPPEL :
!     ========================================
    do 10 i = 1, 3
        alfam(i) = vim(i+2)
        if (nbvar .eq. 2) then
            alfa2m(i) = vim(i+8)
        else
            alfa2m(i)=0.d0
        endif
10  end do
!
    do 20 i = 4, ndimsi
        alfam(i) = vim(i+2)*rac2
        if (nbvar .eq. 2) then
            alfa2m(i) = vim(i+8)*rac2
        else
            alfa2m(i)=0.d0
        endif
20  end do
    if (memo .eq. 0) then
        rpm = rinf + (r0-rinf)*exp(-b*pm)
    else if (memo.eq.1) then
        rpvm = vim(15)
        rpm = rpvm + r0
        qm = vim(16)
    endif
!
!
! --- NITIALISATION :
!     -------------
    call matini(6, 6, 0.d0, dside)
    call matini(6, 6, 0.d0, dsidep)
!
! --- PARTIE ELASTIQUE DE LA MATRICE TANGENTE :
!     ---------------------------------------
    do 170 i = 1, ndimsi
        dsidep(i,i) = deuxmu
170  end do
!
    do 180 i = 1, 3
        do 190 j = 1, 3
            dsidep(i,j) = dsidep(i,j) + troisk/3.d0 - deuxmu/3.d0
190      continue
180  end do
!
! --- PARTIE PLASTIQUE DE LA MATRICE TANGENTE :
!        =======================================
! ---   CALCUL DES DERIVEES PAR RAPPORT A LA DEFORMATION PLASTIQUE
! ---   CUMULEE DES CARACTERISTIQUES D'ECROUISSAGE DU MATERIAU :
!       ------------------------------------------------------
    if (plasti) then
!
! ---     VISCOSIFICATION: SI DP=0 (RIGI_MECA_TANG), ON IMPOSE
! ---     DP = EPSILON > 0 POUR DERIVER LE TERME (DP/DT)**(1/N)
        if (dp .eq. 0.d0) dp = r8prem()
!
        pp = pm + dp
        if (memo .eq. 0) then
            rp = rinf + (r0-rinf)*exp(-b*pp)
        else if (memo.eq.1) then
            rp = rpvp + r0
        endif
        cp = cinf * (1.d0 + (k-1.d0)*exp(-w*pp))
        gammap = gamma0 * (ainf + (1.d0-ainf)*exp(-b*pp))
        mp = cp/(1.d0+gammap*dp*delta1)
        c2p = c2inf * (1.d0 + (k-1.d0)*exp(-w*pp))
        gamm2p = gamm20 * (ainf + (1.d0-ainf)*exp(-b*pp))
        m2p = c2p/(1.d0+gamm2p*dp*delta2)
        if (visc .eq. 1) then
            vp = kvi*((dp/dt)**(1.d0/valden))
            corr = dp/dt
            corr = (corr**(1.d0/valden))/corr
            dvp = kvi*corr/(valden*dt)
        else
            vp=0.d0
            dvp=0.d0
        endif
        denomi = rp + (3.d0*mu+mp*n1+m2p*n2)*dp + vp
        dgamap = -b*gamma0*(1.d0-ainf)*exp(-b*pp)
        dcp = -w*cinf*(k-1.d0)*exp(-w*pp)
        if (memo .eq. 0) then
            drp = -b*(r0-rinf)*exp(-b*pp)
        else if (memo.eq.1) then
            gq0 = mat(14)
            gqmax = mat(15)
            mumem=mat(16)
            gqp=gqmax+(gq0-gqmax)*exp(-2.d0*mumem*qp)
            drp=(gqp-rpm)/(1.d0+b*dp)-2.d0*mumem*(gq0-gqmax)*(qp-qm)
            drp=b*drp/(1.d0+b*dp)
        endif
        dmp = dcp/(1.d0+gammap*dp*delta1) - cp*(dgamap*dp*delta1+ gammap*delta1)/ (1.d0+gammap*dp&
              &*delta1)**2
!
        dgam2p = -b*gamm20*(1.d0-ainf)*exp(-b*pp)
        dc2p = -w*c2inf*(k-1.d0)*exp(-w*pp)
        dm2p = dc2p/(1.d0+gamm2p*dp*delta2) - c2p*(dgam2p*dp*delta2+ gamm2p*delta2)/ (1.d0+gamm2p&
               &*dp*delta2)**2
!
        ap = (rp+vp)/denomi
        ep = -mp
        e2p = -m2p
        bp = - 2.d0/3.d0*mp*(rp+vp)/denomi
        b2p = - 2.d0/3.d0*m2p*(rp+vp)/denomi
!
        seq = 0.d0
        do 200 i = 1, ndimsi
            s(i) = ap*sigedv(i)+bp*alfam(i)+b2p*alfa2m(i)
            seq = seq + s(i)*s(i)
            sigma(i) = sigedv(i)-(mp*alfam(i)+m2p*alfa2m(i))/1.5d0
            dsigma(i) = -(dmp*alfam(i)+dm2p*alfa2m(i))/1.5d0
200      continue
        seq = sqrt(1.5d0*seq)
!
        if (idelta .gt. 0) then
            sigdsi=ddot(ndimsi,sigma,1,dsigma,1)
            do 201 i = 1, ndimsi
                vbeta(i)=(dsigma(i)-1.5d0*sigdsi*sigma(i)/denomi**2)
201          continue
            vbeta(i)=vbeta(i)/denomi
            dbeta1=ddot(ndimsi,alfam,1,vbeta,1)
            dbeta2=ddot(ndimsi,alfa2m,1,vbeta,1)
!
            dn1=1.d0+gammap*(delta1+dbeta1*(delta1-1.d0))
            dn1=dn1+dgamap*(delta1*dp+beta1*(delta1-1.d0))
            dn1=(dn1-n1*(gammap+dgamap*dp))/(1.d0+gammap*dp)
!
            dn2=1.d0+gamm2p*(delta2+dbeta2*(delta2-1.d0))
            dn2=dn2+dgam2p*(delta2*dp+beta2*(delta2-1.d0))
            dn2=(dn2-n2*(gamm2p+dgam2p*dp))/(1.d0+gamm2p*dp)
!
            ddenom=drp+3.d0*mu+mp*n1+m2p*n2+(dmp*n1+dm2p*n2)*dp+dvp&
            +dp*mp*dn1+dp*mp*dn2
        else
            ddenom=drp+3.d0*mu+mp+m2p+(dmp+dm2p)*dp+dvp
        endif
!
        ip = 1.d0/denomi - ddenom*dp/(denomi*denomi)
        dap = (drp+dvp)/denomi - (rp+vp)*ddenom/denomi/denomi
        dep = -dmp
        de2p=-dm2p
        dbp = -2.d0/3.d0*(dmp*ap+mp*dap)
        db2p = -2.d0/3.d0*(dm2p*ap+m2p*dap)
        l1p = ap*ap/seq
        l2p = ap*bp/seq
        l22p = ap*b2p/seq
        l3p = 0.d0
        do 210 i = 1, ndimsi
            l3p=l3p+(dap*sigedv(i)+dbp*alfam(i)+db2p*alfa2m(i))*s(i)
210      continue
        l3p = 1.5d0/seq*l3p
        l3p = l3p - drp - dvp
        hp = dep*dp/denomi + ep*ip
        h2p = de2p*dp/denomi + e2p*ip
        isp = -1.5d0*ip*l1p/l3p
        iap = -1.5d0*ip*l2p/l3p
        ia2p = -1.5d0*ip*l22p/l3p
        h1s = -1.5d0*hp*l1p/l3p
        h1a1 = -1.5d0*hp*l2p/l3p
        h2s = -1.5d0*h2p*l1p/l3p
        h1a2 = -1.5d0*hp*l22p/l3p
        h2a1 = -1.5d0*h2p*l2p/l3p
        h2a2 = -1.5d0*h2p*l22p/l3p
!
        do 220 i = 1, ndimsi
            dside(i,i) = -6.d0*mu*mu*dp/denomi
            do 230 j = 1, ndimsi
                dside(i,j) = dside(i,j) - 6.d0*mu*mu*isp * sigedv(i)* sigedv(j) - 4.d0*mu*mu*h1a1&
                             &* alfam(i)*alfam(j) - 4.d0*mu*mu*h1a2* alfa2m(i)*alfam(j) - 4.d0*mu&
                             &*mu* h2a1* alfam(i)*alfa2m(j) - 4.d0*mu*mu*h2a2* alfa2m(i)* alfa2m(&
                             &j) - 6.d0*mu*mu*iap * alfam(i)*sigedv(j) - 6.d0*mu*mu*ia2p * alfa2m&
                             &(i)*sigedv(j) - 4.d0*mu*mu* h1s * sigedv(i)*alfam(j) - 4.d0*mu*mu*h&
                             &2s * sigedv(i)* alfa2m(j)
230          continue
220      continue
!
! ---   MATRICE DE PROJECTION DEVIATORIQUE :
!       ----------------------------------
        call matini(6, 6, 0.d0, pdev)
!
        pdev(1,1) = 2.d0/3.d0
        pdev(2,2) = 2.d0/3.d0
        pdev(3,3) = 2.d0/3.d0
        pdev(4,4) = 1.d0
        pdev(5,5) = 1.d0
        pdev(6,6) = 1.d0
        pdev(1,2) = -1.d0/3.d0
        pdev(1,3) = -1.d0/3.d0
        pdev(2,3) = -1.d0/3.d0
        pdev(2,1) = -1.d0/3.d0
        pdev(3,1) = -1.d0/3.d0
        pdev(3,2) = -1.d0/3.d0
        do 250 i = 1, ndimsi
            do 260 j = 1, ndimsi
                do 270 l = 1, ndimsi
                    dsidep(i,j) = dsidep(i,j) + dside(i,l)*pdev(l,j)
270              continue
260          continue
250      continue
!
    endif
end subroutine

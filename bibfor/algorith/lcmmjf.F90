subroutine lcmmjf(taus, coeft, materf, ifa, nmat,&
                  nbcomm, dt, necoul, is, ir,&
                  nbsys, vind, dy, nfs, nsg,&
                  hsr, rp, alphap, dalpha, gammap,&
                  dgamma, sgns, dgdtau, dgdal, dfdr,&
                  petith, iret)
! aslint: disable=W1504
    implicit none
#include "asterc/r8miem.h"
    integer :: ifa, nmat, nbcomm(nmat, 3), nfs, nsg
    real(kind=8) :: taus, coeft(nmat), rp, dt, alphap, dalpha, gammap, dgamma
    real(kind=8) :: dgdtau
    real(kind=8) :: dgdal, dfdr, hsr(nsg, nsg), dy(*), vind(*), materf(nmat)
    character(len=16) :: necoul
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jean-michel.proix at edf.fr
! ======================================================================
!  CALCUL DES DERIVEES DES VARIABLES INTERNES DES LOIS MONOCRISTALLINES
!  POUR LA LOI D'ECOULEMENT
!       IN  TAUS    :  SCISSION REDUITE
!           COEFT   :  PARAMETRES MATERIAU
!           MATERF :  COEFFICIENTS MATERIAU A T+DT
!           IFA     :  NUMERO DE FAMILLE
!           NMAT   :  DIMENSION MATER
!           NBCOMM :  INCIDES DES COEF MATERIAU
!           DT     :  ACCROISSEMENT INSTANT ACTUEL
!           NECOUL  :  NOM DE LA LOI D'ECOULEMENT
!           IS     :  NUMERO DU SYST. GLIS. S
!           IR     :  NUMERO DU SYST. GLIS. R
!           NBSYS  :  NOMBRE DE SYSTEMES DE GLISSEMENT FAMILLE IFA
!           HSR    :  MATRICE D'INTERACTION
!           VIND   :  VARIABLES INTERNES A L'INSTANT PRECEDENT
!           DY     :  SOLUTION           =  ( DSIG DX1 DX2 DP (DEPS3) )
!           RP     :  ECROUISSAGE
!           ALPHAP :  ECR. CINEMATIQUE
!           DALPHA :  ACCR. ECR. CINEMATIQUE
!           GAMMAP :  GLISSEMENT PLASTIQUE INSTANT ACTUEL
!           DGAMMA :  ACCR. GLISSEMENT PLASTIQUE
!     OUT:
!           SGNS=FTAU/ABS(FTAU)
!           DGDTAU  :  dF/dTau
!           DGDAL   :  dF/dAlpha  (particulier pour KR)
!           DFDR    :  dF/dR (ou dhs/dalphar pour KR)
!           PETITH  :  hs pour KR
!       OUT IRET   :  CODE RETOUR
!     ----------------------------------------------------------------
    real(kind=8) :: c, p, q, k, n, ftau, crit, a, gamma0, d
    real(kind=8) :: tperd, tabs, alpha, taumu, tauv
    real(kind=8) :: deltg0, sgns, aux, alphas
    real(kind=8) :: taur, tau0, tauef, bsd, gcb, kdcs
    real(kind=8) :: som, cisa2, deltgg, terme, petitg, cs
    real(kind=8) :: dtedto, dggdto, dtedal, dggdal, deltsr, dhdal, petith
!
    integer :: ifl, is, nbsys, iu, ir, iret, nuecou
!     ----------------------------------------------------------------
!
!     DANS VIS : 1 = ALPHA, 2=GAMMA, 3=P
!
    ifl=nbcomm(ifa,1)
    nuecou=nint(coeft(ifl))
    iret=0
!
!      IF (NECOUL.EQ.'MONO_VISC1') THEN
    if (nuecou .eq. 1) then
!
        n=coeft(ifl+1)
        k=coeft(ifl+2)
        c=coeft(ifl+3)
!
        ftau=taus-c*alphap
        crit=abs(ftau)-rp
!
!         dF/dTau
!
        if (crit .gt. 0.d0) then
            dgdtau=(n*dt/(k**n))*(crit**(n-1))
            dgdal=-c*dgdtau
            dfdr=-sgns*dgdtau
        else
            dgdtau=0.d0
            dgdal=0.d0
            dfdr=0.d0
        endif
!
        if (abs(ftau) .le. r8miem()) then
            sgns=1.d0
        else
            sgns=ftau/abs(ftau)
        endif
!
!      IF (NECOUL.EQ.'MONO_VISC2') THEN
    else if (nuecou.eq.2) then
!
        n=coeft(ifl+1)
        k=coeft(ifl+2)
        c=coeft(ifl+3)
        a=coeft(ifl+4)
        d=coeft(ifl+5)
!
        ftau=taus-c*alphap-a*gammap
!
        crit=abs(ftau)-rp + 0.5d0*c*d*alphap**2
        if (abs(ftau) .le. r8miem()) then
            sgns=1.d0
        else
            sgns=ftau/abs(ftau)
        endif
!
        if (crit .gt. 0.d0) then
            dgdtau=(n*dt/(k**n))*(crit**(n-1))
        else
            dgdtau=0.d0
        endif
!
!         DGDAL
        dgdal=(-c*sgns+d*alphap*dalpha)*dgdtau*sgns
!
!         DFDR
        dfdr=-sgns*dgdtau
!
    else if (nuecou.eq.4) then
!             MATRICE JACOBIENNE DU SYSTEME :
!  R1 = D-1*SIGMA - (D_M-1*SIGMA_M)-(DEPS-DEPS_TH)+Somme(ms*Gamma_s)=0
!  R2 = dALPHA - g(Taus,alphas)*h(alphas)
! avec Gamma_s=g(Taus,alphas)*sgn(taus)
!
! ON VEUT CALCULER :
!        dg(taus,alphas)/dtaus
        dgdtau=0.d0
!        dg(taus,alphas)/dalphar
        dgdal=0.d0
!        dh(alphas)/dalphar
        dhdal=0.d0
!        DFDR=DHDAL
        dfdr=0.d0
!
        k =coeft(ifl+1)
        taur =coeft(ifl+2)
        tau0 =coeft(ifl+3)
        gamma0 =coeft(ifl+4)
        deltg0 =coeft(ifl+5)
        bsd =coeft(ifl+6)
        gcb =coeft(ifl+7)
        kdcs =coeft(ifl+8)
        p =coeft(ifl+9)
        q =coeft(ifl+10)
        tperd =coeft(ifl+11)
        if (materf(nmat) .eq. 0) then
            cisa2 = (materf(1)/2.d0/(1.d0+materf(2)))**2
        else
            cisa2 = (materf(36)/2.d0)**2
        endif
        tauv=abs(taus)-tau0
        if (abs(taus) .le. r8miem()) then
            sgns=1.d0
        else
            sgns=taus/abs(taus)
        endif
        if (tauv .gt. 0.d0) then
            som = 0.d0
            taumu = 0.d0
            dgdal = 0.d0
            do 1 iu = 1, nbsys
                alpha = vind(3*(iu-1)+1)+dy(iu)
!             PARTIE POSITIVE DE ALPHA
                if (alpha .gt. 0.d0) then
                    taumu = taumu + hsr(is,iu)*alpha
                    if (iu .ne. is) som = som+alpha
                endif
 1          continue
            alphas= vind(3*(is-1)+1)+dy(is)
            som=sqrt(som)
            taumu = cisa2 * taumu/tauv
            tauef = tauv-taumu
            if (tauef .gt. 0.d0) then
                aux= (1.d0-(tauef/taur)**p)
                if (aux .le. 0.d0) then
                    iret=1
                    goto 9999
                endif
                tabs=tperd+273.15d0
!              PROTECTION DE l'EXPONENTIELLE
                deltgg=deltg0*(aux**q)
                terme=-deltgg/k/tabs
                if (terme .gt. 10.d0) then
                    iret=1
                    goto 9999
                endif
!              CALCUL DE dg/dtau
                petitg=gamma0*exp(terme)*dt
                petith=bsd+som/kdcs-gcb*alphas
                if (petith .lt. 0.d0) petith=0.d0
                cs=-(p*q*deltg0/taur)
                cs=cs*(aux**(q-1.d0))*(tauef/taur)**(p-1.d0)
                dtedto=sgns*(1.d0+taumu/tauv)
                dggdto=cs*dtedto
                dgdtau =-petitg*sgns*dggdto/k/tabs
!
!              CALCUL DE dgs/dalphar
                dtedal=-cisa2/tauv*hsr(is,ir)
                dggdal=cs*dtedal
                dgdal=-petitg/k/tabs*dggdal
!
!              CALCUL DE dhs/dalphar
                deltsr=0.d0
                if (ir .eq. is) deltsr=1.d0
!
                if (petith .gt. 0.d0) then
                    if (som .gt. 0.d0) then
                        dhdal=(1-deltsr)/som/2.d0/kdcs-gcb*deltsr
                    endif
                else
                    dhdal=0.d0
                endif
                dfdr=dhdal
            endif
        endif
    endif
9999  continue
end subroutine

subroutine lcddcc(taus, coeft, ifa, nmat, nbcomm,&
                  is, nbsys, nfs, nsg, hsr,&
                  vind, dy, dt, rp, nuecou,&
                  dalpha, dgamma, dp, iret)
    implicit none
#include "asterf_types.h"
#include "asterc/r8maem.h"
#include "asterc/r8miem.h"
#include "asterc/r8pi.h"
#include "asterfort/assert.h"
    integer :: ifa, nmat, nbcomm(nmat, 3), iret
    integer :: ifl, is, ir, nbsys, nfs, nsg, nuecou, irr2
    real(kind=8) :: taus, coeft(nmat), dgamma, dp, vind(*), dalpha
    real(kind=8) :: rp, sgns, hsr(nsg, nsg), dy(12), dt, depsdt
    real(kind=8) :: n, gamma0, rmin, rhop(12)
    real(kind=8) :: tauf, rhom(12), rmax, hs, gampro, gp1, gp2, ys, taueff
    real(kind=8) :: b, h, deltg0, tau0, d, beta, temp, dlat, kf, kself, rhomob
    real(kind=8) :: kboltz
    real(kind=8) :: yat, mu, lc, rhotot, dg, deltag, t1, t2, t3, t4, t5, t6, t7
    real(kind=8) :: t8, t9
    real(kind=8) :: rs, d1, lambda, alphat, ls, tauslt, tauslr, gamnuc, asr
    real(kind=8) :: delta1, delta2, airr, rhoirr, depdt, tauc, t10
    aster_logical :: new
    common /deps6/depsdt
    integer :: irr, decirr, nbsyst, decal, gdef
    common/polycr/irr,decirr,nbsyst,decal,gdef
! ----------------------------------------------------------------------
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
! ======================================================================
!  COMPORTEMENT MONOCRISTALLIN : ECOULEMENT (VISCO)PLASTIQUE
!  INTEGRATION DE LA LOI MONOCRISTALLINE DD-CC. CALCUL DE DALPHA DGAMMA
!
! ARGUMENTS
!
!       IN  TAUS    :  SCISSION REDUITE
!           COEFT   :  PARAMETRES MATERIAU
!           IFA     :  NUMERO DE FAMILLE
!           CISA2   :  COEF DE CISAILLEMENT MU
!           NMAT    :  NOMBRE MAXI DE MATERIAUX
!           NBCOMM  :  NOMBRE DE COEF MATERIAU PAR FAMILLE
!           NBSYS   :  NOMBRE DE SYSTEMES DE GLISSEMENT
!           HSR     :  Hsr
!           VIND    :  tous les variables internes instant precedent
!           DT      :  INTERVALLE DE TEMPS EVENTULLEMENT REDECOUPE
!           YD      :
!           DY      :
!     OUT:
!           DALPHA  :  VARIABLE densite de dislocation
!           DGAMMA  :  GLISSEMENT PLASTIQUE DU SYSTEME IS
!           DP      :  ABS(DGAMMA)
!           IRET    :  CODE RETOUR
! ======================================================================
!
    ifl=nbcomm(ifa,1)
    rmin=r8miem()
    rmax=sqrt(r8maem())
!
    b=     coeft(ifl+1)
    h=     coeft(ifl+2)
    deltg0=coeft(ifl+3)
    tau0 = coeft(ifl+4)
    d=     coeft(ifl+5)
    gamma0=coeft(ifl+6)
    n     =coeft(ifl+7)
    depdt =coeft(ifl+8)
    yat   =coeft(ifl+9)
    dlat=  coeft(ifl+10)
    kf=    coeft(ifl+11)
    kself= coeft(ifl+12)
    tauf=  coeft(ifl+13)
    rhomob=coeft(ifl+14)
    kboltz=coeft(ifl+15)
    temp=  coeft(ifl+16)
    mu    =coeft(ifl+17)
    irr2   =nint(coeft(ifl+18))
    ASSERT(irr2.eq.irr)
    if (irr .gt. 0) then
        airr =coeft(ifl+19)
!         XI    =COEFT(IFL+20)
    endif
! initialisation des arguments en sortie
    dgamma=0.d0
    dalpha=0.d0
    dp=0.d0
    iret=0
!
    lc=500.d0*b*(temp/300.d0)**2
!
    do 55 ir = 1, nbsys
        rhom(ir)=vind(decal+3*(ir-1)+1)
        rhop(ir)=rhom(ir)+dy(ir)
 55 end do
!
    if (rhop(is) .lt. rmin) then
        iret=1
        goto 9999
    endif
!
!     on resout en alpha=rho
!
! 1.  CALCUL de DeltaG approximatif
    rhotot=0.d0
! rho tot represente rho_f (foret)
    do 11 ir = 1, 12
        if (ir .eq. is) goto 11
        rhotot=rhotot+rhop(ir)
 11 continue
!
    if (rhotot .lt. rmin) then
        iret=1
        goto 9999
    endif
    if (irr .gt. 0) then
        rhoirr=vind(decirr+is)
        rhotot=rhotot+rhoirr
    endif
!
    if (depdt .gt. rmin) then
!        DEPDST FOURNI PAR L'UTILISATEUR
        dg=kboltz*temp*log(rhomob*b*h/sqrt(rhotot)/depdt)
        deltag=min(deltg0,dg)
    else if (depsdt.gt.rmin) then
!        DEPSDT DU POINT DE GAUSS
        dg=kboltz*temp*log(rhomob*b*h/sqrt(rhotot)/depsdt)
        deltag=min(deltg0,dg)
    else
        deltag=deltg0
    endif
!
! 2.  Calcul de Rs
    t1=1.d0-deltag/deltg0
    if (t1 .lt. 0.d0) then
        ASSERT(.false.)
    else if (t1.lt.rmin) then
        rs=rmax
    else
        rs=mu*b/(2.d0*tau0*t1*t1)
    endif
!
! 3.  calcul de lambda
    d1=(d+2.d0*rs)*rhotot
    t2=min(sqrt(rhotot),d1)
    lambda=1.d0/t2 - d
!
! 4.  calcul de Alpha-s_AT et Ls
!
    alphat=0.d0
    do 21 ir = 1, 12
        if (ir .eq. is) goto 21
        alphat=alphat+rhop(ir)*hsr(is,ir)
 21 continue
!
    if (alphat .lt. rmin) then
        iret=1
        goto 9999
    endif
    if (irr .eq. 1) then
        if (abs(airr) .gt. rmin) then
            alphat=alphat+airr*rhoirr
        endif
    endif
    alphat=sqrt(alphat/rhotot)
    ls=max((lambda-2.d0*alphat*rs),lc)
!
! 5.  calcul de Taus_LT
    t3 = 2.d0*alphat*rs+lc
    t4=1.d0/lambda-1.d0/t3
!
    tauslt=max(0.d0,(alphat*mu*b*t4))
!
! 6.  calcul de Taus_LR
!
    tauslr=mu*b*sqrt(rhop(is)*hsr(is,is))
!
! 7.  calcul de Taus_eff
    tauc=tauf + sqrt( tauslt**2+tauslr**2)
    taueff=abs(taus)-tauc
!
    if (abs(taus) .gt. rmin) then
        sgns=taus/abs(taus)
    else
        sgns=0.d0
    endif
!
! 8.  calcul de gamma_nuc
    if (taueff .gt. tau0) then
        iret=1
        goto 9999
    else if (taueff.lt.rmin) then
        t5=0.d0
    else
        t5=sqrt(taueff/tau0)
    endif
    gamnuc=rhomob*b*h*ls*exp(-deltg0*(1.d0-t5)/kboltz/temp)
!
!     ON POURRAIT DESACTIVER CE SYSTEME SI TAU_EFF < 0
!
    gamnuc=gamnuc*sgns
!
! 9.  calcul de gamma_prob
    gampro=gamma0*(abs(taus)/tauc)**n
    gampro=gampro*sgns
!
! 10. ECOULEMENT CALCUL DE DGAMMA,DP
    if (abs(gampro) .gt. rmin) then
        gp1=1.d0/gampro
    else
        gp1=0.d0
    endif
    if (abs(gamnuc) .gt. rmin) then
        gp2=1.d0/gamnuc
    else
        gp2=0.d0
    endif
    if (abs(gp1+gp2) .gt. rmin) then
        dgamma=1.d0/(gp1+gp2)*dt
        dp=abs(dgamma)
    endif
!
    t10=1.d0
    if (taueff .gt. rmin) t10=(1.d0-taueff/tau0)
!
! 11. CALCUL DE RHO_POINT RENOMME DALPHA
    if (rhop(is) .gt. rmin) then
        t7= sqrt(hsr(is,is)*rhop(is))*t10
    else
        t7=0.d0
!        ou bien IRET=1, a voir
    endif
!
    t8=alphat*rhotot*lambda*t10
!
    if (taueff .gt. rmin) then
        t9=1.d0/yat+2.d0*r8pi()*taueff/mu/b
    else
        t9=1.d0/yat
    endif
    ys=1.d0/t9
    hs=1.d0/dlat+t7/kself+t8/kf-ys*rhop(is)
    dalpha=hs*dp/b
!
9999 continue
! 12. irradiation mise ajout dans LCDPEC / LCDPEQ
end subroutine

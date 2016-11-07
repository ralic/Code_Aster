subroutine srdndx(nmat,mater,i1,devsig,bprime,val,para,xi,tmp,dpardx,dndxi)

!
! ===================================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ===================================================================================

!!!
!!! MODELE LKR : CALCUL DU TERME DN/DXI
!!!

! ===================================================================================
! IN  : NMAT          : DIMENSION TABLE DES PARAMETRES MATERIAU
!     : MATER(NMAT,2) : TABLE DES PARAMETRES MATERIAU
!     : I1            : TRACE DU TENSEUR DES CONTRAINTES
!     : DEVISG(6)     : DEVIATEUR DU TENSEUR DES CONTRAINTES
!     : BPRIME        : PARAMETRE DE DILATANCE FCTN SIGMA
!     : VAL           : BOOLEEN SUR DILATANCE EN PRE(0) OU POST-PIC(1)
!     : XI            : VARIABLE D'EXROUISSAGE XI(P OU VP)
!     : PARA(3)       : CONTIENT VALEURS DE A(XI),S(XI),M(XI)
!     : DPARDX(3)     : DERIVEE DE A(XI),S(XI),M(XI) PAR RAPPORT A XI
! OUT : DNDXI(6)      : DN/DXI
! ===================================================================================

    implicit   none

#include "asterc/r8pi.h"
#include "asterc/r8prem.h"
#include "asterfort/cos3t.h"
#include "asterfort/lcinve.h"
#include "asterfort/lcprsc.h"
#include "asterfort/srhtet.h"

    !!!
    !!! Variables globales
    !!!
    
    integer :: nmat,val
    real(kind=8) :: i1,devsig(6),dndxi(6),bprime,mater(nmat,2)
    real(kind=8) :: para(3),dpardx(3),xi,tmp
    
    !!!
    !!! Variables locales
    !!!
    
    integer :: i, ndt,ndi    
    real(kind=8) :: pref,sigc,m1,a2,a5,m5,s5
    real(kind=8) :: s1,qi,fi,m3,xi1,xi2,fp
    real(kind=8) :: rho_1,rho_2,rho_4
    real(kind=8) :: sii,vident(6),dbpdxi
    real(kind=8) :: sinpsi,dsindx,pi,alres,rcos3t,dsindst,dstdxi
    real(kind=8) :: r0c,rtheta,fact3,fact4,c,phi,tiers
    real(kind=8) :: sigmin,sigmax,sigcar,alpha
    real(kind=8) :: daxdxi,dmxdxi,dsxdxi
    real(kind=8) :: sigtil,sp,fact1,fact2,ax,sx,mx
    real(kind=8) :: m10,qi0,xi10,xi20,rq,rm,trr,dtmp,m00,m0,s0,a0
    real(kind=8) :: rs,rx1,rx2,spre,spos,f3p,phip,cp,sigtilp,ffp
    common /tdim/   ndt,ndi
    
    !!!
    !!! Construction de variables tmp
    !!!
    
    call lcinve(0.d0,vident)
    
    do i=1,ndi
        vident(i)=1.d0
    end do
    
    !!!
    !!! Recuperation des derivees et fonctions d'ecrouissage
    !!!
    
    daxdxi=dpardx(1)
    dsxdxi=dpardx(2)
    dmxdxi=dpardx(3)
    ax=para(1)
    sx=para(2)
    mx=para(3)
    
    !!! Calcul de sii
    call lcprsc(devsig,devsig,sii)
    sii=sqrt(sii)
    
    !!!
    !!! Recuperation de parametres du modele
    !!!
    
    !!! a T0
    pi=r8pi()
    pref=mater(1,2)
    sigc=mater(3,2)
    a0=5.d-1
    a2=mater(8,2)
    m00=mater(9,2)
    m10=mater(10,2)
    qi0=mater(11,2)
    xi10=mater(12,2)
    xi20=mater(13,2)
    fp=mater(15,2)
    rho_1=mater(18,2)
    rho_2=mater(19,2)
    rho_4=mater(20,2)
    rq=mater(21,2)
    rm=mater(22,2)
    rs=mater(23,2)
    rx1=mater(24,2)
    rx2=mater(25,2)
    
    !!! temperatures
    trr=mater(8,1)
    
    !!! a T
    if ((tmp.ge.trr).and.(trr.gt.0.d0)) then
        qi=qi0*(1.d0-rq*log(tmp/trr))
        dtmp=tmp-trr
    else
        qi=qi0
        dtmp=0.d0
    endif
    
    m0=m00*exp(-rm*(dtmp**2.d0))
    m1=m10*exp(-rm*(dtmp**2.d0))
    s0=(m0*1.d-1/(1.d0-1.d-1**2.d0))**2.d0
    s1=1.d0*exp(-rs*(dtmp**2.d0))
    xi2=xi20*exp(rx2*dtmp)
    xi1=xi10*exp(rx1*dtmp)
    fi=qi/sigc
    m3=m1*fi/(fi**2.d0-s1)
    
    if (fp.le.0.d0) then
        s5=s0
        m5=m0
        a5=a0
    else
        ffp=s0*(1.d0-fp)/m0+fp*s1/m1
        fact1=ffp*m1*fi**(1.d0/a2)
        fact2=fi**2.d0-s1+m1*ffp
        !!!
        s5=fact1/fact2
        m5=s5/ffp
        a5=a2
    endif
    
    !!! Parametres d'ecrouissage
    sp=para(2)
    
    !!! Calcul de h(theta)
    rcos3t=cos3t(devsig, pref, 1.d-8)
    call srhtet(nmat, mater, rcos3t, r0c, rtheta)
    
    !!! Calcul de alpha_res
    alres=1.d0+m3
    
    !!! Calcul de sii
    call lcprsc(devsig, devsig, sii)
    sii=sqrt(sii)
    
    !!! Calculs de sigma_min et sigma_max
    tiers=1.d0/3.d0
    sigmin=i1*tiers-sii*rtheta/sqrt(6.d0)/r0c
    sigmax=i1*tiers+sqrt(2.d0/3.d0)*sii*rtheta/r0c
    
    if (sigmax.le.r8prem()) sigmax=r8prem()
    if (sigmin.le.r8prem()) sigmin=r8prem()
    
    !!! Calcul de sigma_car
    fact4=sigmin*m5/sigc+s5
    
    if (fact4.ge.0.d0) then
        sigcar=sigmin+sigc*fact4**a5
        if (sigcar.le.r8prem()) sigcar=r8prem()
    else
        sigcar=r8prem()
    endif
    
    !!! Calcul de sigma_tilde et alpha
    if ((xi1.le.xi).and.(xi.lt.xi2)) then
        fact3=1.d0+para(1)*para(3)*para(2)**(para(1)-1.d0)
        c=sigc*(para(2)**para(1))/2.d0/sqrt(fact3)
        phi=2.d0*atan(sqrt(fact3))-pi/2.d0
        sigtil=c/tan(phi)
        if (sigmin+sigtil.ge.r8prem()) then
            alpha=(sigmax+sigtil)/(sigmin+sigtil)
        else
            alpha=(sigmax+sigtil)/r8prem()
        endif
    else if (xi.ge.xi2) then 
        sigtil=0.d0
        if (sigmin+sigtil.ge.r8prem()) then
            alpha=(sigmax+sigtil)/(sigmin+sigtil)
        else
            alpha=(sigmax+sigtil)/r8prem()
        endif
    endif
    
    !!! Calcul de sigma_tilde au pic de resistance
    f3p=1.d0+m1/2.d0/s1**(1.d0/2.d0)
    cp=sigc*(s1**(1.d0/2.d0))/2.d0/sqrt(f3p)
    phip=2.d0*atan(sqrt(f3p))-pi/2.d0
    sigtilp=cp/tan(phip)
    
    if (sigtilp.le.r8prem()) then
        sigtilp=r8prem()
    endif
    
    !!! Calcul de sin(psi)
    if (val.eq.0) then
        sinpsi=rho_1*(sigmax-sigcar)/(rho_2*sigmax+sigcar)
    else 
        if (sigtil.gt.0.d0) then
            if (sigmax-sigcar.gt.0.d0) then
                spre=rho_1*(sigmax-sigcar)/(rho_2*sigmax+sigcar)
                spos=rho_1*(alpha-alres)/(rho_4*alpha+alres)
            else
                spre=0.d0
                spos=rho_1*(alpha-alres)/(rho_4*alpha+alres)
            endif
            sinpsi=spre+(1.d0-sigtil/sigtilp)*spos
        else
            if (sigmax-sigcar.gt.0.d0) then
                spre=rho_1*(sigmax-sigcar)/(rho_2*sigmax+sigcar)
                spos=rho_1*(alpha-alres)/(rho_4*alpha+alres)
            else
                spre=0.d0
                spos=rho_1*(alpha-alres)/(rho_4*alpha+alres)
            endif
            sinpsi=spre+spos
        endif
    endif
    
    if (sinpsi.le.-1.d0) then
        sinpsi=-1.d0
    else if (sinpsi.ge.1.d0) then
        sinpsi=1.d0
    endif
    
    !!! Calcul de d(sin)/d(xi)
    if (val.eq.1) then
        if (sp.gt.0.d0) then
            dsindst=rho_1*(alpha*alres*(rho_4-1)*(sigmin+sigtil)**2.d0-&
                  & alpha**2.d0*rho_4*(sigmin+sigtil)**2.d0+&
                  & alres*(alres*(sigmin+sigtil)**2.d0+&
                  & (1.d0+rho_4)*(sigmax-sigmin)*(sigtil-sigtilp)))/&
                  & (alres+alpha*rho_4)**2.d0/(sigmin+sigtil)**2.d0/sigtilp
            dstdxi=-sx*sigc/ax**2.d0/mx*daxdxi+&
                 & sigc/ax/mx*dsxdxi-sx*sigc/ax/mx**2.d0*dmxdxi
            dsindx=dsindst*dstdxi
        else
            dsindx=0.d0
        endif
    else
        dsindx=0.d0
    endif
    
    !!! Calcul de d(beta')/dxi
    dbpdxi=-6.d0*sqrt(6.d0)/(3.d0-sinpsi)**2.d0*dsindx
    
    !!! Assemblage
    do i=1, ndt
        dndxi(i)=((3.d0*devsig(i)+bprime*sii*vident(i))/sii/&
                 &(3.d0+bprime**2.d0)**(3.d0/2.d0))*dbpdxi
    end do

end subroutine

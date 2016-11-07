subroutine srdbds(nmat,mater,i1,devsig,nvi,vint,para,val,tmp,dbetds,dbetdi)

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
!!! MODELE LKR : CALCUL DE LA DERIVEE DE BPRIME PAR RAPPORT A I1 ET S
!!!

! ===================================================================================
! IN  : VAL           : ENTIER PRECISANT DILATANCE EN PRE(0) OU POST-PIC(1)
!     : TMP           : TEMPERATURE A L'INSTANT "-" OU "+"
!     : NMAT          : DIMENSION TABLE DES PARAMETRES MATERIAU
!     : MATER(NMAT,2) : PARAMETRES MATERIAU A T+DT
!     : DEVSIG(6)     : DEVIATEUR DES CONTRAINTES
!     : I1            : TRACE DES CONTRAINTES
!     : NVI           : NOMBRE DE VARIABLES INTERNES
!     : VINT(NVI)     : VARIABLES INTERNES
!     : PARA(3)       : VECTEUR CONTENANT AXI, SXI ET MXI
! OUT : DBETDS(6)     : DERIVEE DE BPRIME PAR RAPPORT A DEVSIG (DEVIATEUR)
!     : DBETDI        : DERIVEE DE BPRIME PAR RAPPORT A I1 (TRACE SIGMA)
! ===================================================================================

    implicit   none

#include "asterc/r8pi.h"
#include "asterc/r8prem.h"
#include "asterfort/cos3t.h"
#include "asterfort/lcprsc.h"
#include "asterfort/lcprsv.h"
#include "asterfort/srdhds.h"
#include "asterfort/srhtet.h"

    !!!
    !!! Variables globales
    !!!
    
    integer :: nmat,nvi,iret,val
    real(kind=8) :: mater(nmat,2),devsig(6),i1,para(3)
    real(kind=8) :: vint(nvi),dbetds(6),dbetdi,tmp
    
    !!!
    !!! Variables locales
    !!!
    
    integer :: i,ndt,ndi
    real(kind=8) :: pi,pref,sigc,m3,xi2
    real(kind=8) :: m1,a2,s1,qi,fi,a5,m5,s5,ffp
    real(kind=8) :: xi1,fp,ucar,fact6,fact7
    real(kind=8) :: rho_1,rho_2,alres,rcos3t,rtheta
    real(kind=8) :: c,phi,xip,tiers,a0,m00,s0,m0
    real(kind=8) :: sigmin,sigmax,sii,sigcar,alpha,sigtil,sinpsi
    real(kind=8) :: dsinds(6),dsmids(6),dsmads(6),r0c,dhds(6),dscardi
    real(kind=8) :: dsmidi,dsmadi,dsindi,rho_4,dbdsin,fact3,fact4,fact5
    real(kind=8) :: m10,qi0,xi10,xi20,rq,rm,f3p,cp,phip,sigtilp,dscards(6)
    real(kind=8) :: rx1,rx2,rs,trr,dtmp,spre,spos,temp(6),fact1,fact2
    common /tdim/ ndt,ndi
    
    !!!
    !!! Calcul de sii
    !!!
    
    call lcprsc(devsig,devsig,sii)
    sii=sqrt(sii)
    
    !!!
    !!! Recup. de para. du modele
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
    
    !!! Temperatures
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
    xi1=xi10*exp(rx1*tmp)
    xi2=xi20*exp(rx2*tmp)
    fi=qi/sigc
    m3=m1*fi/(fi**2.d0-s1)
    
    !!! Calculs de s5, m5 et a5
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
    
    !!!
    !!! Recup. de h(theta)
    !!!
    
    rcos3t=cos3t(devsig, pref, 1.d-8)
    call srhtet(nmat, mater, rcos3t, r0c, rtheta)
    
    !!!
    !!! Calcul de alpha_res
    !!!
    
    alres=1.d0+m3
    
    !!!
    !!! Calcul de sii
    !!!
    
    call lcprsc(devsig, devsig, sii)
    sii=sqrt(sii)
    
    !!!
    !!! Calcul de sigma_min et sigma_max
    !!!
    
    tiers=1.d0/3.d0
    sigmin=i1*tiers-sii*rtheta/sqrt(6.d0)/r0c
    sigmax=i1*tiers+sqrt(2.d0/3.d0)*sii*rtheta/r0c
    
    if (sigmax.le.r8prem()) sigmax=r8prem()
    if (sigmin.le.r8prem()) sigmin=r8prem()
    
    !!!
    !!! Calcul de sigma_car
    !!!
    
    fact4=sigmin*m5/sigc+s5
    
    if (fact4.ge.0.d0) then
        sigcar=sigmin+sigc*fact4**a5
        if (sigcar.le.r8prem()) sigcar=r8prem()
    else
        sigcar=r8prem()
    endif
    
    !!!
    !!! Calcul de sigma_tilde et alpha
    !!!

    xip=vint(1)

    if ((xi1.le.xip).and.(xip.lt.xi2)) then
        
        fact3=1.d0+para(1)*para(3)*para(2)**(para(1)-1.d0)
        c=sigc*(para(2)**para(1))/2.d0/sqrt(fact3)
        phi=2.d0*atan(sqrt(fact3))-pi/2.d0
        sigtil=c/tan(phi)
        
        if (sigmin+sigtil.ge.r8prem()) then
            alpha=(sigmax+sigtil)/(sigmin+sigtil)
        else
            alpha=(sigmax+sigtil)/r8prem()
        endif
        
    else if (xip.ge.xi2) then 
        
        sigtil=0.d0
        
        if (sigmin+sigtil.ge.r8prem()) then
            alpha=(sigmax+sigtil)/(sigmin+sigtil)
        else
            alpha=(sigmax+sigtil)/r8prem()
        endif
        
    endif
    
    !!!
    !!! Calcl de sigma_tilde au pic de resistance
    !!!
    
    f3p=1.d0+m1/2.d0/s1**(1.d0/2.d0)
    cp=sigc*(s1**(1.d0/2.d0))/2.d0/sqrt(f3p)
    phip=2.d0*atan(sqrt(f3p))-pi/2.d0
    sigtilp=cp/tan(phip)
    
    if (sigtilp.le.r8prem()) then
        sigtilp=r8prem()
    endif
    
    !!!
    !!! Calcul de sin(psi)
    !!!
    
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
    
    !!!
    !!! Calcul de d(beta')/d(sin(psi))
    !!!    
    
    dbdsin=-6.d0*sqrt(6.d0)/(3.d0-sinpsi)**2.d0
    
    !!!
    !!! Termes communs
    !!!
    
    fact5=rho_1*(1.d0+rho_2)/(rho_2*sigmax+sigcar)**2.d0
    ucar=sigmin*m5/sigc+s5
    
    if (ucar.gt.0.d0) then
        fact6=1.d0+m5*a5*(ucar**(a5-1.d0))
    else 
        fact6=1.d0
    endif
    
    fact7=rho_1*alres*(1.d0+rho_4)/(rho_4*alpha+alres)**2.d0/(sigmin+sigtil)**2.d0
    
    !!!
    !!! Calcul de d(sigma_max)/d(s), d(sigma_min)/d(s) et d(sigma_car)/d(s)
    !!!
    
    call srdhds(nmat, mater, devsig, dhds, iret)
    
    do i=1,ndt
        dsmids(i)=-(sii*dhds(i)+rtheta*devsig(i)/sii)/r0c/sqrt(6.d0)
        dsmads(i)=sqrt(2.d0/3.d0)*(sii*dhds(i)+rtheta*devsig(i)/sii)/r0c
        dscards(i)=dsmids(i)*fact6
    end do
    
    !!!
    !!! Calcul de d(sin(psi))/d(s)
    !!!
    
    !!! Assemblage d(sin)/d(s) pre-pic et visco.
    
    if (val.eq.0) then
        
        do i=1,ndt
            dsinds(i)=fact5*(sigcar*dsmads(i)-sigmax*dscards(i))
        end do
        
    else if (val.eq.1) then
        
        if (sigmax-sigcar.gt.0.d0) then
            
            do i=1,ndt
                temp(i)=(sigmin+sigtil)*dsmads(i)-(sigmax+sigtil)*dsmids(i)
                dsinds(i)=fact5*(sigcar*dsmads(i)-sigmax*dscards(i)) + &
                          (1.d0-sigtil/sigtilp)*fact7*temp(i)
            end do
            
        else
            
            do i=1,ndt
                temp(i)=(sigmin+sigtil)*dsmads(i)-(sigmax+sigtil)*dsmids(i)
                dsinds(i)=(1.d0-sigtil/sigtilp)*fact7*temp(i)
            end do
            
        endif
    endif
    
    !!!
    !!! Calcul de d(sin(psi))/d(i1)
    !!!
    
    !!!
    !!! Calcul de d(sigmax)/d(i1) et d(sigmin)/d(i1)
    !!!
    
    dsmidi=tiers
    dsmadi=tiers
    dscardi=dsmidi*fact6
    
    !!!
    !!! Assemblage de d(sin)/d(i1)
    !!!
    
    if (val.eq.0) then
        
        dsindi=fact5*(sigcar*dsmadi-sigmax*dscardi)
        
    else if (val.eq.1) then
        
        if (sigmax-sigcar.gt.0.d0) then
            
            dsindi=fact5*(sigcar*dsmadi-sigmax*dscardi) + &
                     (1.d0-sigtil/sigtilp)*fact7*(sigmin-sigmax)/3.d0
            
        else
        
            dsindi=(1.d0-sigtil/sigtilp)*fact7*(sigmin-sigmax)/3.d0
            
        endif
    endif
    
    !!!
    !!! Calcul de d(beta')/d(s)
    !!!
    
    call lcprsv(dbdsin,dsinds,dbetds)
    
    !!!
    !!! Calcul de d(beta')/d(i1)
    !!!
    
    dbetdi=dbdsin*dsindi

end subroutine

function srbpri(val,vin,nvi,nbmat,mater,para,invar,s,tmp)

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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.                      
! ===================================================================================

!!!
!!! MODELE LKR : CALCUL DU PARAMETRE BPRIME
!!!

! ===================================================================================
! IN  : VAL            : INDICATEUR POUR LE CALCUL DE SIN(PSI)
!     : VIN(NVI)       : VECTEUR DES VARIABLES INTERNES
!     : NBMAT          : NOMBRE DE PARAMETRES MATERIAU
!     : MATER(NBMAT,2) : MATRICE DES PARAMETRES DU MODELE COLONNE 1 --> ELAS
!                                                         COLONNE 2 --> MODELE LKR 
!     : PARA(3)        : PARAMETRES D'ECROUISSAGE
!                           PARA(1) = AXI
!                           PARA(2) = SXI
!                           PARA(3) = MXI
!     : INVAR          : PREMIER INVARIANT DU TENSEUR DES CONTRAINTES
!     : S(6)           : DEVIATEUR DES CONTRAINTES
!     : TMP            : TEMPERATURE A L'INSTANT - OU +
! OUT : SRBPRI         : PARAMETRE CONTROLANT LE COMPORTEMENT VOLUMIQUE
!                        DU MATERIAU
! ===================================================================================

    implicit  none

#include "asterc/r8pi.h"
#include "asterc/r8prem.h"
#include "asterfort/cos3t.h"
#include "asterfort/lcprsc.h"
#include "asterfort/srhtet.h"

    !!!
    !!! Variables globales
    !!!
    
    integer :: val,nbmat,nvi
    real(kind=8) :: vin(nvi),mater(nbmat,2),para(3),invar,s(6),srbpri,tmp
    
    !!!
    !!! Variables locales
    !!!
    
    real(kind=8) :: pi,a0,m00,m0,a5,m5
    real(kind=8) :: xip,pref,sigc,m3,xi2,s5,s0,fp,rho_1,rho_2,rho_4,a2,fi,ffp
    real(kind=8) :: sii,rcos3t,r0c,rtheta,f3p,cp,phip,sigtilp,fact2
    real(kind=8) :: c,phi,alres,sigtil,sigmin,sigmax,sigcar,alpha
    real(kind=8) :: sinpsi,m1,s1,spre,spos,xi20,rm,rq,rs,xi1,fact1
    real(kind=8) :: m10,qi0,rx2,trr,qi,dtmp,tiers,fact3,fact4,xi10,rx1
    
    !!!
    !!! Recuperation de parametres du modele
    !!!
    
    !!! parametres a T0
    pi=r8pi()
    a0=5.d-1
    pref=mater(1,2)
    sigc=mater(3,2)
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
    
    !!! parametres a T
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
    
    !!! calcul de s5, m5 et a5
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
    !!! Calcul de H(theta)
    !!!
    
    rcos3t=cos3t(s,pref,1.d-8)
    call srhtet(nbmat,mater,rcos3t,r0c,rtheta)
    
    !!!
    !!! Calcul de alpha_res
    !!!
    
    alres=1.d0+m3
    
    !!!
    !!! Calcul de sii
    !!!
    
    call lcprsc(s,s,sii)
    sii=sqrt(sii)
    
    !!!
    !!! Calcul de sigma_min et sigma_max
    !!!
    
    tiers=1.d0/3.d0
    sigmin=invar*tiers-sii*rtheta/sqrt(6.d0)/r0c
    sigmax=invar*tiers+sqrt(2.d0/3.d0)*sii*rtheta/r0c
    
    if (sigmax.le.r8prem()) sigmax=r8prem()
    
    if (sigmin.le.r8prem()) sigmin=r8prem()
    
    !!!
    !!! Calcul de sigma_car
    !!!
    
    fact4=sigmin*m5/sigc+s5
    
    if (fact4.gt.0.d0) then
        sigcar=sigmin+sigc*(fact4**a5)
        if (sigcar.le.r8prem()) sigcar=r8prem()
    else
        sigcar=sigmin
        if (sigcar.le.r8prem()) sigcar=r8prem()
    endif
    
    !!!
    !!! Calcul de sigma_tilde et alpha
    !!!
    
    xip=vin(1)
    
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
    !!! Calcul de sigma_tilde au pic de resistance
    !!!
    
    f3p=1.d0+m1/2.d0/s1**(1.d0/2.d0)
    cp=sigc*(s1**(1.d0/2.d0))/2.d0/sqrt(f3p)
    phip=2.d0*atan(sqrt(f3p))-pi/2.d0
    sigtilp=cp/tan(phip)
    
    if (sigtilp.le.r8prem()) sigtilp=r8prem()
    
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
    !!! Calcul de bprime
    !!!
    
    srbpri=-2.d0*sqrt(6.d0)*sinpsi/(3.d0-sinpsi)

end function

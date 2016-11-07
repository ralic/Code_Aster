subroutine srdpdt(vin,nvi,nbmat,mater,paraep,dpdt)

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
!!! MODELE LKR : CALCUL DES DERIVEES DES PARA. D'ECROUISSAGE PLASTIQUES / A T
!!!

! ===================================================================================
! IN  : VIN(NVI)       : VECTEUR DES VARIABLES INTERNES
!     : NVI            : NOMBRE DE VARIABLES INTERNES
!     : NBMAT          : NOMBRE DE PARAMETRES MATERIAU
!     : MATER(NBMAT,2) : TABLEAU DES PARA. MATER.
!     : PARAEP(3)      : VECTEUR DES PARAMETRES D ECROUISSAGE
! OUT : DPDT(3)        : VECTEUR CONTENANT LES DERIVEES DES PARA. D ECROUISSAGE/T
! ===================================================================================
    
    implicit    none
    
    !!!
    !!! Variables globales
    !!!
    
    integer :: nvi, nbmat
    real(kind=8) :: vin(nvi),mater(nbmat,2),paraep(3),dpdt(3)
    
    !!!
    !!! Variables locales
    !!!
    
    real(kind=8) :: v1,v2,a2,a1,m00,m10,qi0,xi10,xi20,rq,rm,rs,rx1
    real(kind=8) :: rx2,dtmp,qi,m0,m1,s1,xi1,xi2,s0,fi
    real(kind=8) :: sigc,dqi,dm0,dm1,ds1,dxi1,dxi2,ds0
    real(kind=8) :: ap,sp,mp,xip,fact1,fact2,dadxi1,dsdxi1,dmdxi1
    real(kind=8) :: dsds1,dsds0,dmdm1,dmdm0,fact3,fact4,fact5
    real(kind=8) :: dadxi2,dsdxi2,dmds1,dmdqi,fact8,dmda,dmds
    real(kind=8) :: dadt,dsdt,dmdt,tmm,trr

    !!!
    !!! Recuperation des parametres materiau
    !!!
    
    !!! para. a T0
    sigc=mater(3,2)
    v1=mater(6,2)
    v2=mater(7,2)
    a1=5.d-1
    a2=mater(8,2)
    m00=mater(9,2)
    m10=mater(10,2)
    qi0=mater(11,2)
    xi10=mater(12,2)
    xi20=mater(13,2)
    rq=mater(21,2)
    rm=mater(22,2)
    rs=mater(23,2)
    rx1=mater(24,2)
    rx2=mater(25,2)
    
    !!!! temperatures
    tmm=mater(6,1)
    trr=mater(8,1)
    
    !!! para. a T
    if ((tmm.ge.trr).and.(trr.gt.0.d0)) then
        qi=qi0*(1.d0-rq*log(tmm/trr))
        dtmp=tmm-trr
    else
        qi=qi0
        dtmp=0.d0
    endif
    
    m0=m00*exp(-rm*(dtmp**2.d0))
    m1=m10*exp(-rm*(dtmp**2.d0))
    s1=1.d0*exp(-rs*(dtmp**2.d0))
    xi1=xi10*exp(rx1*dtmp)
    xi2=xi20*exp(rx2*dtmp)
    s0=(m0*1.d-1/(1.d0-1.d-1**2.d0))**2.d0
    fi=qi/sigc
    
    !!!
    !!! Derivees des para. dependant de T / T
    !!!
    
    if ((tmm.ge.trr).and.(trr.gt.0.d0)) then
        dqi=-qi0*rq/tmm
    else
        dqi=0.d0
    endif
    
    dm0=-2.d0*dtmp*rm*m0
    dm1=-2.d0*dtmp*rm*m1
    ds1=-2.d0*dtmp*rs*s1
    dxi1=rx1*xi1
    dxi2=rx2*xi2
    ds0=-2.d0*2.d0*dtmp*rm*s0
    
    !!!
    !!! Derivees des para. d'ecrouissage par rapport a T
    !!!
    
    ap=paraep(1)
    sp=paraep(2)
    mp=paraep(3)
    xip=vin(1)
    
    !!! Pre-pic
    if ((xip.ge.0.d0).and.(xip.lt.xi1)) then
        
        fact1=(v1*xip/xi1**2.d0)*(1.d0-xip/xi1)**(v1-1.d0)
        fact2=(1.d0-xip/xi1)**v1
        dsdxi1=(s0-s1)*fact1
        dmdxi1=(m0-m1)*fact1
        dsds1=1.d0-fact2
        dsds0=fact2
        dmdm1=1.d0-fact2
        dmdm0=fact2
        
        dadt=0.d0
        dsdt=dsdxi1*dxi1+dsds0*ds0+dsds1*ds1
        dmdt=dmdxi1*dxi1+dmdm0*dm0+dmdm1*dm1
    
    !!! Post-pic 1
    else if ((xip.ge.xi1).and.(xip.lt.xi2)) then
        
        fact1=v2*(a1-a2)/(xi2-xi1)
        fact2=s1*v2*(1.d0+v2)
        fact3=(xip-xi1)/(xi2-xi1)
        dadxi1=fact1*(xi2-xip)/(xi2-xi1)*(fact3**(v2-1.d0))
        dadxi2=fact1*(fact3**v2)
        dsdxi1=fact2*((xi2-xip)**2.d0/(xi2-xi1)**3.d0)*(fact3**(v2-1.d0))
        dsdxi2=fact2*((xi2-xip)/(xi2-xi1)**2.d0)*(fact3**v2)
        dsds1=sp/s1
        dmds1=mp/(fi**2.d0-s1)
        dmdqi=m1*sigc*(2.d0*ap*fi*qi*sp*sigc+fi**(1.d0/ap)*&
             &(qi**2.d0-2.d0*ap*fi*qi*sigc-s1*sigc**2.d0))/&
             &(ap*fi*(qi**2.d0-s1*sigc**2.d0))**2.d0
        dmdm1=mp/m1
        
        fact8=(fi**2.d0-s1)
        dmda=-(fi**(1.d0/ap))*m1*log(fi)/(ap**2.d0)/fact8
        dmds=-m1/fact8
        
        dadt=dadxi1*dxi1+dadxi2*dxi2
        dsdt=dsdxi1*dxi1+dsds1*ds1+dsdxi2*dxi2
        dmdt=dmds1*ds1+dmdm1*dm1+dmdqi*dqi+dmda*dadt+dmds*dsdt
    
    !!! Post-pic 2
    else if (xip.ge.xi2) then
        
        fact1=v2*(a2-a1)/(xi2-xi1)**2.d0
        fact2=exp(-v2*(a2-a1)*(xip-xi2)/(1.d0-a2)/(xi2-xi1))
        fact3=fi**2.d0-s1
        fact4=-fi**(1.d0/ap-1.d0)*m1
        fact5=((ap-a1)*fi**2.d0+a1*s1)
        dadxi1=fact1*(xip-xi2)*fact2
        dadxi2=fact1*(xi1-xip)*fact2
        dmdm1=mp/m1
        dmds1=mp/fact3
        dmdqi=-fi**(1.d0/ap-1.d0)*m1*sigc*(2.d0*ap*fi*qi*sigc+s1*sigc**2.d0)/&
             &ap/(qi**2.d0-s1*sigc**2.d0)**2.d0
        
        fact8=(fi**2.d0-s1)
        dmda=-(fi**(1.d0/ap))*m1*log(fi)/(ap**2.d0)/fact8
        
        dadt=dadxi1*dxi1+dadxi2*dxi2
        dsdt=0.d0
        dmdt=dmds1*ds1+dmdm1*dm1+dmdqi*dqi+dmda*dadt
        
    endif
    
    !!!
    !!! Sockage
    !!!
    
    dpdt(1)=dadt
    dpdt(2)=dsdt
    dpdt(3)=dmdt

end subroutine

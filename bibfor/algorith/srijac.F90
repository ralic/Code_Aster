subroutine srijac(nmat,materf,timed,timef,&
                  yf,deps,nr,nvi,vind,&
                  vinf,yd,drdy)

!
! ======================================================================
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
! ======================================================================

!!!
!!! MODELE LKR : CALCUL DU JACOBIEN DE LKR = DRDY(DY)
!!!

! ===================================================================================
! IN  : MOD               : TYPE DE MODELISATION
!     : NMAT              : DIMENSION MATER
!     : MATERF(NMAT,2)    : COEFFICIENTS MATERIAU A T+DT
!     : YF(NDT+3)         : VARIABLES A T + DT =  ( SIGF DLAMBDA XI_P XI_VP)
!     : DEPS(6)           : INCREMENT DE DEFORMATION
!     : TIMED             : INSTANT  T
!     : TIMEF             : INSTANT  T+DT
!     : NR                : DIMENSION DECLAREE DRDY
!     : NVI               : NOMBRE DE VARIABLES INTERNES
!     : VIND(NVI)         : VARIABLE INTERNES A T
!     : VINF(NVI)         : VARIABLE INTERNES A T+DT
!     : YD(NDT+3)         : VARIABLES A T  = ( SIGD  0 XI_P XI_VP) A T
!     : DY(NDT+3)         : SOLUTION = ( DSIG  DLAMBDA  DXI_P DXI_VP )
! OUT : DRDY(NDT+3,NDT+3) : JACOBIEN DU SYSTEME NON LINEAIRE
!     : IRET              : CODE RETOUR
! ===================================================================================

    implicit none

#include "asterc/r8prem.h"
#include "asterfort/lcdevi.h"
#include "asterfort/lcdima.h"
#include "asterfort/lceqvn.h"
#include "asterfort/lcinma.h"
#include "asterfort/lcinve.h"
#include "asterfort/lcprmm.h"
#include "asterfort/lcprmv.h"
#include "asterfort/lcprsc.h"
#include "asterfort/lcprsm.h"
#include "asterfort/lcprsv.h"
#include "asterfort/lcprte.h"
#include "asterfort/lcsove.h"
#include "asterfort/srbpri.h"
#include "asterfort/srcalg.h"
#include "asterfort/srcaln.h"
#include "asterfort/srcrip.h"
#include "asterfort/srcriv.h"
#include "asterfort/srdepp.h"
#include "asterfort/srdfds.h"
#include "asterfort/srdfdx.h"
#include "asterfort/srdgde.h"
#include "asterfort/srdgds.h"
#include "asterfort/srdhds.h"
#include "asterfort/srdndx.h"
#include "asterfort/srds2h.h"
#include "asterfort/srelas.h"
#include "asterfort/srfsxi.h"
#include "asterfort/srvacp.h"
#include "asterfort/srvacv.h"
#include "asterfort/srvarp.h"
#include "asterfort/srvarv.h"
#include "asterfort/r8inir.h"
#include "asterf_types.h"

    !!!
    !!! Variables globales
    !!!
    
    integer :: nr,nmat,nvi
    real(kind=8) :: deps(6),drdy(nr,nr),yf(nr),yd(nr)
    real(kind=8) :: materf(nmat,2)
    real(kind=8) :: timed,timef,vind(nvi),vinf(nvi)
    
    !!!
    !!! Varibales locales
    !!!
    
    integer :: i,j,varv,valv,valp,retcom,ndt,ndi
    
    real(kind=8) :: sigft(6),depst(6)
    real(kind=8) :: devgii,vint(nvi),dt,devsig(6),i1
    real(kind=8) :: xi5,xi1,dsdenl(6,6),kk,mu
    real(kind=8) :: ucriv,seuilv,depsv(6),dgamv
    real(kind=8) :: seuilp,ucrip,seuivm,dhds(6),ds2hds(6)
    real(kind=8) :: paraep(3),varpl(4),dfdsp(6),bprimp
    real(kind=8) :: vecnp(6),gp(6),vetemp(6),derpar(3),dfdxip
    real(kind=8) :: depse(6),hook(6,6),mue,ke
    real(kind=8) :: dsige(6),vident(6),dhokds(6,6),patm,nelas
    real(kind=8) :: paravi(3),varavi(4),dfvdsi(6)
    real(kind=8) :: dgpds(6,6),dgvds(6,6)
    real(kind=8) :: dldgds(6,6),dlambd,hnldgp(6,6),dsgvds(6,6)
    real(kind=8) :: hnldgv(6,6),bprimv,vecnv(6),ucrim,devgiv,gv(6)
    real(kind=8) :: hnlgv(6),hnldfg(6,6),phiv,av,nv,dphiv
    real(kind=8) :: dphvds(6),dr1dy3(6),dgpdxi(6),dfsdxp(6)
    real(kind=8) :: term1,term2,term3,coupl
    real(kind=8) :: dndxip(6),dfsdxv(6),dpadxp(3)
    real(kind=8) :: dndxiv(6),dfdxiv,dpadxv(3),dphidx
    real(kind=8) :: dphdxg(6),dgvdxi(6),phdgdx(6)
    real(kind=8) :: dr1dy4(6),dgipds(6),dgivds(6),dgipdx,dgivdx
    real(kind=8) :: mident(6,6),kron(6),kron2(6,6),unstro
    real(kind=8) :: dsdsig(6,6),dgtvds(6,6),dgtpds(6,6),kron3(6,6)
    real(kind=8) :: devgp(6),devgv(6),dgtpdx(6),dgtvdx(6),dxiv
    real(kind=8) :: z,r,a0,tpp,trr,dtmp,xi2,xi20,rx2
    real(kind=8) :: xi10,xi50,rx1,rx5
    
    aster_logical :: plas
    
    common /tdim/ ndt, ndi
    
    !!!
    !!! Recup. des temperatures
    !!!
    
    tpp=materf(7,1)
    trr=materf(8,1)
    
    if ((tpp.ge.trr).and.(trr.gt.0.d0)) then
        dtmp=tpp-trr
    else
        dtmp=0.d0
    endif
    
    !!!
    !!! Passage en convention meca. des sols
    !!!
    
    do i=1,ndt
        sigft(i)=-yf(i)
        depst(i)=-deps(i)
    end do
    
    !!!
    !!! Variables locales tmp
    !!!
    
    call lcinma(0.d0,mident)
    
    do i=1, ndt
        mident(i,i)=1.d0
    end do
    
    varv=0
    devgii=0.d0
    dlambd=yf(ndt+1)
    
    !!!
    !!! Vecteur variables internes tmp
    !!!
    
    call lceqvn(nvi, vind, vint)
    
    if (yf(ndt+2).ge.vind(1)) then
        vint(1)=yf(ndt+2)
    else
        vint(1)=vind(1)
    endif
    
    if (yf(ndt+3).ge.vind(3)) then
        vint(3)=yf(ndt+3)
    else
        vint(3)=vind(3)
    endif
    
    !!! Increment de temps
    dt=timef-timed
    
    !!!
    !!! Construction deviateur des contraintes et 1er invariant
    !!!
    
    call lcdevi(sigft, devsig)
    
    i1=sigft(1)+sigft(2)+sigft(3)
    
    !!!
    !!! Recup. des para. materiau
    !!!
    
    xi10=materf(12,2)
    xi20=materf(13,2)
    xi50=materf(14,2)
    rx1=materf(24,2)
    rx2=materf(25,2)
    rx5=materf(26,2)
    xi5=xi50*exp(rx5*dtmp)
    xi1=xi10*exp(rx1*dtmp)
    xi2=xi20*exp(rx2*dtmp)
    
    !!!
    !!! Construction tenseur elastique non lineaire
    !!!
    
    call srelas(ndi,ndt,nmat,materf,sigft,dsdenl,kk,mu)
    
    !!!
    !!! 1) Calcul de la def. visco. et du para. d'ecrouissage
    !!!
    
    !!! 1-1) Indicateur angle de dilatance visco.
    valv=0
    
    !!! 1-2) Calcul seuil visco. par rapport a yf
    seuilv=0.d0
    
    call srcriv(vint(3),i1,devsig,nmat,materf,tpp,ucriv,seuilv)
    
    if (seuilv.ge.0.d0) then
        
        call srdgde(valv,vint(3),dt,seuilv,ucriv,&
                    i1,devsig,vint,nvi,nmat,materf,&
                    tpp,depsv,dgamv,retcom)
    
    else
        
        dgamv=0.d0
        
        do i=1, ndt
            depsv(i)=0.d0
        end do
        
        seuilv=0.d0
        ucriv=0.d0
        
    endif
    
    !!!
    !!! 2) Calcul de depsp et dgamp
    !!!
    
    call srdhds(nmat,materf,devsig,dhds,retcom)
    call srds2h(nmat,materf,devsig,dhds,ds2hds,retcom)
    
    !!! 2-1) Calcul fonction seuil en yf
    seuilp=0.d0
    
    call srcrip(i1,devsig,vint,nvi,nmat,materf,tpp,ucrip,seuilp)
    
    !!! 2-2) Indicateur contractance ou dilatance
    seuivm=0.d0
    
    call srcriv(xi5,i1,devsig,nmat,materf,tpp,ucrim,seuivm)
    
    if (seuivm.lt.0.d0) then
        varv=0
    else
        varv=1
    endif
    
    !!! 2-3) Si seuilp >= 0, alors plasticite
    if ((seuilp.ge.0.d0).or.(vinf(7).gt.0.d0)) then
        
        !!! 2-3-1) Indicateur angle de dilatance
        if (yf(ndt+2).lt.xi1) then
            valp=0
        else
            valp=1
        endif
        
        !!! 2-3-2) Calcul de df/dsig
        call srvarp(vint,nvi,nmat,materf,tpp,paraep)
        call srvacp(nmat,materf,paraep,varpl)
        call srdepp(vint,nvi,nmat,materf,paraep,derpar)
        call srdfds(nmat,materf,paraep,varpl,ds2hds,ucrip,dfdsp)
        
        !!! 2-3-2) Calcul de gp
        bprimp=srbpri(valp,vint,nvi,nmat,materf,paraep,i1,devsig,tpp)
        
        call lcinve(0.d0,vecnp)
        call srcaln(devsig,bprimp,vecnp,retcom)
        call srcalg(dfdsp,vecnp,gp,devgii)
        
        !!! 2-3-4) Calcul def. elastique
        do i=1,ndt
            depse(i)=depst(i)-yf(ndt+1)*gp(i)-depsv(i)
        end do
        
        !!! 2-3-5) calcul de dgp/dsigma
        call srdgds(nmat,materf,paraep,varpl,devsig,&
                    i1,valp,ds2hds,vecnp,dfdsp,&
                    bprimp,nvi,vint,dhds,tpp,dgpds)
        
        !!! 2-3-6) Produit matriciel hook*dlambda*d(gp)/d(sig)
        call lcprsm(dlambd,dgpds,dldgds)
        call lcprmm(dsdenl,dldgds,hnldgp)
        
        !!! 2-3-7) Calcul de d2(fp)/d(sig)d(xi)
        plas=.true.
        call srfsxi(nmat,materf,i1,devsig,ds2hds,&
                    plas,vint(1),paraep,varpl,tpp,dfsdxp,dpadxp)
        
        !!! 2-3-8) Calcul de d(n)/d(xi)
        call srdndx(nmat,materf,i1,devsig,bprimp,&
                    valp,paraep,vint(1),tpp,derpar,dndxip)
    
    !!! 2-4) Pas de plasticite
    else
        
        do i=1,ndt
            depse(i)=depst(i)-depsv(i)
        end do
        
        call lcinma(0.d0,hnldgp)
        call lcinma(0.d0,dgpds)
        call lcinve(0.d0,dfdsp)
        call lcinve(0.d0,gp)
        call lcinve(0.d0,vecnp)
        call lcinve(0.d0,dfsdxp)
        call lcinve(0.d0,dndxip)
        
        devgii=0.d0
        
    endif
    
    !!!
    !!! Calcul de d(r1)/d(y)
    !!!
    
    !!! Calcul de d(r1)/d(y1) : y1 == sigma
    
    !!! construction du tenseur elasticite lineaire

    mue=materf(4,1)
    ke=materf(5,1)
    
    call lcinma(0.d0, hook)
    
    do i=1, ndi
        do j=1, ndi
            hook(i,j)=ke-2.d0*mue/3.d0
        end do
    end do
    
    do i=1, ndt
        hook(i,i)=hook(i,i)+2.d0*mue
    end do
    
    !!! contrainte elastique
    call lcprmv(hook, depse, dsige)
    
    !!! produit tensoriel d(sige) x vident
    patm=materf(1,2)
    nelas=materf(2,2)
    
    call lcinve(0.d0, vident)
    
    do i=1, ndi
        vident(i)=nelas/3.d0/patm*(i1/(3.d0*patm))**(nelas-1.d0)
    end do
    
    call lcprte(dsige, vident, dhokds)
    
    !!! calcul de d(fv)/d(sig)
    call srvarv(vint(3),nmat,materf,tpp,paravi)
    call srvacv(nmat,materf,paravi,varavi)
    
    bprimv=srbpri(valv,vint,nvi,nmat,materf,paravi,i1,devsig,tpp)
    
    call srcaln(devsig,bprimv,vecnv,retcom)
    call srdfds(nmat,materf,paravi,varavi,ds2hds,ucriv,dfvdsi)
    
    !!! calcul de gvp
    call srcalg(dfvdsi,vecnv,gv,devgiv)
    
    !!! calcul de d(gvp)/d(sig)
    call srdgds(nmat,materf,paravi,varavi,devsig,&
                i1,valv,ds2hds,vecnv,dfvdsi,&
                bprimv,nvi,vint,dhds,tpp,dgvds)
    
    !!! produit matriciel de hook * phi_v * d(gv)/d(sig)
    r=8.3144621d0
    a0=materf(16,2)
    nv=materf(17,2)
    z=materf(27,2)
    
    if ((tpp.ge.trr).and.(trr.gt.0.d0)) then
        av=a0*exp(-z/r/tpp*(1.d0-tpp/trr))
    else
        av=a0
    endif
    
    phiv=av*(seuilv/patm)**nv
    
    call lcprsm(phiv,dgvds,dsgvds)
    call lcprmm(dsdenl,dsgvds,hnldgv)
    
    !!! produit matriciel hook*d(phiv)/d(sig)*gv
    dphiv=av*nv/patm*(seuilv/patm)**(nv-1.d0)
    
    call lcprsv(dphiv,dfvdsi,dphvds)
    call lcprmv(dsdenl,gv,hnlgv)
    call lcprte(hnlgv,dphvds,hnldfg)
    
    !!! assemblage
    do i=1,ndt
        do j=1,ndt
            drdy(i,j)=-(mident(i,j)-dhokds(i,j)+hnldgp(i,j)+hnldgv( i,j)*dt+&
                    & hnldfg(i,j)*dt)/mu
        end do
    end do
    
    !!! Calcul de d(r1)/d(y2) - y2 == dlambda
    
    if (vinf(7).le.0.d0) then
        do i=1,ndt
            drdy(i,ndt+1)=0.d0
        end do
    else
        call lcprmv(dsdenl,gp,vetemp)
        do i=1, ndt
            drdy(i,ndt+1)=vetemp(i)/mu
        end do
    endif
    
    !!! Calcul de d(r1)/d(y3) - y3 == xip
    
    !!! assemblage d(gp)/d(xi)
    call lcprsc(dfsdxp,vecnp,term1)
    call lcprsc(dfdsp,dndxip,term2)
    call lcprsc(dfdsp,vecnp,term3)
    
    do i=1,ndt
        dgpdxi(i)=dfsdxp(i)-term1*vecnp(i)-term2*vecnp(i)-term3*dndxip(i)
    end do
    
    !!! assemblage final
    call lcprmv(dsdenl,dgpdxi,dr1dy3)
    call lcprsv(dlambd,dr1dy3,dr1dy4)
    
    do i=1,ndt
        drdy(i,ndt+2)=dr1dy4(i)/mu
    end do
    
    !!! Calcul de d(r1)/d(y4) - y4 == xivp

    dxiv=min(dgamv,xi5-yd(ndt+3))
    if (abs(dxiv-dgamv).lt.r8prem()) then
        !!! calcul de d(fvp)/d(sig)d(xiv)
        plas=.false.
        
        call srfsxi(nmat,materf,i1,devsig,ds2hds,&
                    plas,vint(3),paravi,varavi,tpp,dfsdxv,dpadxv)
        
        !!! calcull de d(n)/d(xi)
        call srdndx(nmat,materf,i1,devsig,bprimv,&
                    valv,paravi,vint(3),tpp,dpadxv,dndxiv)
        
        !!! assemblage de d(gv)/d(xiv)
        call lcprsc(dfsdxv,vecnv,term1)
        call lcprsc(dfvdsi,dndxiv,term2)
        call lcprsc(dfvdsi,vecnv,term3)
        
        do i=1,ndt
            dgvdxi(i)=dfsdxv(i)-term1*vecnv(i)-term2*vecnv(i)-term3*dndxiv(i)
        end do
        
        !!! calcul de d(phiv)/d(xiv)
        call srdfdx(nmat, materf, ucriv, i1, devsig,&
                    paravi, varavi, dpadxv, dfdxiv)
        
        !!! assemblage d(r1)/d(y4)
        dphidx=dphiv*dfdxiv
        
        call lcprsv(dphidx,gv,dphdxg)
        call lcprsv(phiv,dgvdxi,phdgdx)
        call lcsove(dphdxg,phdgdx,vetemp)
        call lcprmv(dsdenl,vetemp,dr1dy4)
        
        do i=1,ndt
            drdy(i,ndt+3)=dr1dy4(i)/mu*dt
        end do
        
    else
        
        do i=1,ndt
            drdy(i,ndt+3)=0.d0
        end do
        
    endif
    
    !!!
    !!! Calcul de d(r2)/d(y)
    !!!
    
    !!! Application de la condition de kt sur r(ndt+1)
    
    if (vinf(7).le.0.d0) then
        
        !!! calcul de d(r2)/d(y1) - y1 == sigma
        do i=1,ndt
            drdy(ndt+1,i)=0.d0
        end do
        
        !!! calcul de d(r2)/d(y2) - y2 == dlambda
        drdy(ndt+1,ndt+1)=1.d0
        
        !!! calcul de d(r2)/d(y3) - y3 == xip
        drdy(ndt+1,ndt+2)=0.d0
        
    else
        
        !!! calcul de d(r2)/d(y1) - y1 == sigma
        do i=1,ndt
            drdy(ndt+1,i)=-dfdsp(i)/mu
        end do
        
        !!! calcul de d(r2)/d(y2) - y2 == dlambda
        drdy(ndt+1,ndt+1)=0.d0
        
        !!! calcul de d(r2)/d(y3) - y3 = xip
        call srdfdx(nmat,materf,ucrip,i1,devsig,paraep,varpl,derpar,dfdxip)
        
        drdy(ndt+1,ndt+2)=dfdxip/mu
        
    endif
    
    !!! Calcul de d(r2)/d(y4) - y4 = xivp
    drdy(ndt+1,ndt+3)=0.d0
    
    !!!
    !!! Calcul de d(r3)/d(y)
    !!!
    
    !!! Calcul de d(r3)/d(y1) - y1 == sigma
    
    call lcinve(0.d0,kron)
    
    do i=1,ndi
        kron(i)=1.d0
    end do
    
    !!! construction de d(s)/d(sig)
    unstro=1.d0/3.d0
    
    call lcprte(kron,kron,kron2)
    call lcprsm(unstro,kron2,kron3)
    call lcdima(mident,kron3,dsdsig)
    
    !!! construction de dev(g)
    call lcdevi(gv,devgv)
    call lcdevi(gp,devgp)
    
    !!! construction de d(devgii)/d(sig)  
    call lcprmm(dsdsig,dgvds,dgtvds)
    call lcprmm(dsdsig,dgpds,dgtpds)
    call lcinve(0.d0,dgivds)
    call lcinve(0.d0,dgipds)
    
    if ((seuilp.ge.0.d0).or.(vinf(7).gt.0.d0)) then
        do i=1,ndt
            do j=1,ndt
                dgivds(i)=dgivds(i)+devgv(j)/devgiv*dgtvds(j,i)
                dgipds(i)=dgipds(i)+devgp(j)/devgii*dgtpds(j,i)
            end do
        end do
    else
        do i=1,ndt
            do j=1,ndt
                dgivds(i)=dgivds(i)+devgv(j)/devgiv*dgtvds(j,i)
            end do
        end do
    endif
    
    coupl=materf(28,2)
    
    if ((varv.eq.1).and.(coupl.ge.1.d0/2.d0)) then
        do i=1,ndt
            drdy(ndt+2,i)=sqrt(2.d0/3.d0)*(dlambd*dgipds(i)+ &
                        & (dphvds(i)*devgiv+phiv*dgivds(i))*dt)
        end do
    else
        do i=1,ndt
            drdy(ndt+2,i)=dlambd*sqrt(2.d0/3.d0)*dgipds(i)
        end do
    endif
    
    !!! Calcul de d(r3)/d(y2) - y2 == dlambda
    
    !!! application de la condition de kt
    if (vinf(7).le.0.d0) then
        drdy(ndt+2,ndt+1)=0.d0
    else
        drdy(ndt+2,ndt+1)=-devgii*sqrt(2.d0/3.d0)
    endif
    
    !!! Calcul de d(r3)/d(y3) - y3 == xip
    
    call lcprmv(dsdsig,dgpdxi,dgtpdx)
    call lcprsc(devgp, dgtpdx,dgipdx)
    
    if (vinf(7).gt.0.d0) then
        drdy(ndt+2,ndt+2)=1.d0-dlambd*sqrt(2.d0/3.d0)*dgipdx/devgii
    else
        drdy(ndt+2,ndt+2)=1.d0
    endif
    
    !!! Calcul de d(r3)/d(y4) - y4 == xivp
    
    call lcprmv(dsdsig,dgvdxi,dgtvdx)
    call lcprsc(devgv,dgtvdx,dgivdx)
    
    if (abs(dxiv-dgamv).lt.r8prem()) then
        drdy(ndt+2,ndt+3)=-(dphidx*devgiv+phiv*dgivdx/devgiv)*sqrt(2.d0/3.d0)*dt
    else
        drdy(ndt+2,ndt+3)=0.d0
    endif
    
    !!!
    !!! Calcul de d(r4)/d(y)
    !!!

    if (abs(dxiv-dgamv).lt.r8prem()) then
        
        !!! Calcul de d(r4)/d(y1) - y1 == sigma
        do i=1, ndt
            drdy(ndt+3,i)=(dphvds(i)*devgiv+phiv*dgivds(i))*sqrt( 2.d0/3.d0 )*dt
        end do
        
        !!! Calcul de d(r4)/d(y2) - y2 == dlambda
        drdy(ndt+3,ndt+1)=0.d0
        
        !!! Calcul de d(r4)/d(y3) - y3 == xip
        drdy(ndt+3,ndt+2)=0.d0
        
        !!! Calcul de d(r4)/d(y4) - y4 == xivp
        drdy(ndt+3,ndt+3)=1.d0-sqrt(2.d0/3.d0)*dt*(dphidx*devgiv+phiv*dgivdx/devgiv)
        
    else
        
        !!! Calcul de d(r4)/d(y1) - y1 == sigma
        do i=1, ndt
            drdy(ndt+3,i)=0.d0
        end do
        
        !!! Calcul de d(r4)/d(y2) - y2 == dlambda
        drdy(ndt+3,ndt+1)=0.d0
        
        !!! Calcul de d(r4)/d(y3) - y3 == xip
        drdy(ndt+3,ndt+2)=0.d0
        
        !!! Calcul de d(r4)/d(y4) - y4 == xivp
        drdy(ndt+3,ndt+3)=1.d0
        
    endif

end subroutine

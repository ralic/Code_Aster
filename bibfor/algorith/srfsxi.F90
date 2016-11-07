subroutine srfsxi(nmat, materf, i1, devsig, dshds,&
                  plas, xi, para, vara, tmp, dfdsdx, dpardx)

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
!!! MODELE LKR : CALCUL DU TERME D(DF/DS)/DXI
!!!

! ===================================================================================
! IN  : NMAT           : DIMENSION TABLE DES PARAMETRES MATERIAU
!     : MATERF(NMAT,2) : TABLE DES PARAMETRES MATERIAU
!     : I1             : TRACE DU TENSEUR DES CONTRAINTES
!     : DEVISG(6)      : DEVIATEUR DU TENSEUR DES CONTRAINTES
!     : DSHDS(6)       : DERIVVE DE SII*HTHETA PAR RAPPORT A SIGMA
!     : PLAS           : BOOLEEN -> PLASTI. = TRUE - VISCOS. = FALSE
!     : XI             : VARIABLE D'EXROUISSAGE XI(P OU VP)
!     : PARA(3)        : CONTIENT VALEURS DE A(XI),S(XI),M(XI)
!     : VARA(4)        : CONTIENT AG(XI),BG(XI),DG(XI) ET K(XI)
! OUT : DFDSFX(6)      : D(DF/DS)/DXI
!     : DPARDX         : VECTEUR DE LONGUEUR 3 CONTENANT :
!            DAMDX  : DERIVEE DE A(XI) PAR RAPPORT A XI
!            DSDX   : DERIVEE DE M(XI) PAR RAPPORT A XI
!            DMDX   : DERIVEE DE S(XI) PAR RAPPORT A XI
! ===================================================================================

    implicit   none

#include "asterfort/cos3t.h"
#include "asterfort/lcinve.h"
#include "asterfort/lcprsc.h"
#include "asterfort/srhtet.h"
#include "asterf_types.h"

    !!!
    !!! Variables globales
    !!!
    
    integer :: nmat
    real(kind=8) :: i1, devsig(6), dshds(6), dfdsdx(6), materf(nmat,2)
    real(kind=8) :: para(3), xi, vara(4), dpardx(3), tmp
    aster_logical :: plas
    
    !!!
    !!! Variables locales
    !!!
    
    integer :: ndt, ndi, i
    real(kind=8) :: sigc, v1, v2, a0, a2, m0, m1, qi, xi1
    real(kind=8) :: xi2, s0, a1, s1, fi
    real(kind=8) :: fp, ffp, xi5, fact1, fact2, s5, m5, a5, pref, fact3
    real(kind=8) :: damdx, r0c, agx, sii, rtheta, bgx, dgx, amx, fact4, fact5
    real(kind=8) :: vident(6), terexp, dagdx, dbgdx, ddgdx
    real(kind=8) :: kx, dkdx, sx
    real(kind=8) :: rcos3t, dsdx, dmdx, mx
    real(kind=8) :: m00, m10, qi0, xi10, xi20, xi50, trr, dtmp
    real(kind=8) :: rq, rm, rs, rx1, rx2, rx5
    common /tdim/   ndt,ndi
    
    !!!
    !!! Recup. des para. mater.
    !!!
    
    !!! a T0
    pref=materf(1,2)
    sigc=materf(3,2)
    v1=materf(6,2)
    v2=materf(7,2)
    a0=5.0d-1
    a1=5.0d-1
    a2=materf(8,2)
    m00=materf(9,2)
    m10=materf(10,2)
    qi0=materf(11,2)
    xi10=materf(12,2)
    xi20=materf(13,2)
    xi50=materf(14,2)
    fp=materf(15,2)
    rq=materf(21,2)
    rm=materf(22,2)
    rs=materf(23,2)
    rx1=materf(24,2)
    rx2=materf(25,2)
    rx5=materf(26,2)
    
    !!! temperatures
    trr=materf(8,1)
    
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
    s1=1.d0*exp(-rs*(dtmp**2.d0))
    xi1=xi10*exp(rx1*dtmp)
    xi2=xi20*exp(rx2*dtmp)
    xi5=xi50*exp(rx5*dtmp)
    s0=(m0*1.d-1/(1.d0-1.d-1**2.d0))**2.d0
    fi=qi/sigc
    ffp=s0*(1.d0-fp)/m0+fp*s1/m1
    fact1=ffp*m1*fi**(1.d0/a2)
    fact2=fi**2.d0-s1+m1*ffp
    
    if (fp.le.0.d0) then
        s5=s0
        m5=m0
        a5=a0
    else
        s5=fact1/fact2
        m5=s5/ffp
        a5=a2
    endif
    
    !!! Recuperation des para. d'ecrouissage
    amx=para(1)
    sx=para(2)
    mx=para(3)
    agx=vara(1)
    bgx=vara(2)
    dgx=vara(3)
    kx=vara(4)
    rcos3t=cos3t(devsig,pref,1.d-8)
    
    call srhtet(nmat,materf,rcos3t,r0c,rtheta)
    
    !!!
    !!! Identite
    !!!

    call lcinve(0.d0,vident)
    
    do i=1,ndi
        vident(i)=1.d0
    end do
    
    !!!
    !!! sii
    !!!
    
    call lcprsc(devsig,devsig,sii)
    sii=sqrt(sii)
    
    !!!
    !!! Calcul de d(a)/d(x), d(m)/d(x) et d(s)/d(x)
    !!!
    
    if (plas) then
        !!! Si meca. plastique
        
        if ((xi.ge.0.d0).and.(xi.lt.xi1)) then
            
            fact3=v1*((1.d0-xi/xi1)**(v1-1.d0))/xi1
            damdx=0.d0
            dsdx=(s1-s0)*fact3
            dmdx=(m1-m0)*fact3
            
        else if ((xi.ge.xi1).and.(xi.lt.xi2)) then
            
            fact3=(xi-xi1)/(xi2-xi1)
            fact4=-m1/(fi**2.d0-s1)
            damdx=v2*(a2-a1)*(fact3**(v2-1.d0))/(xi2-xi1)
            dsdx=s1*v2*(1.d0+v2)*(fact3**(v2-1.d0))*&
                 & (xi-xi2)/((xi2-xi1)**2.d0)
            dmdx=fact4*(log(fi)*(fi**(1.d0/amx))*damdx/(amx**2.d0)+dsdx)
            
        else if (xi.ge.xi2) then
            
            fact3=(a2-a1)/(xi2-xi1)
            fact4=-m1/(fi**2.d0-s1)
            fact5=fact3*(xi-xi2)/(1.d0-a2)
            damdx=v2*fact3*exp(-v2*fact5)
            dsdx=0.d0
            dmdx=fact4*(log(fi)*(fi**(1.d0/amx))*damdx/(amx**2.d0)+dsdx)
            
        endif
        
    else
        !!! Si meca. visco.
        
        if ((xi.ge.0.d0).and.(xi.lt.xi5)) then
            
            fact3=v1*((1.d0-xi/xi5)**(v1-1.d0))/xi5
            damdx=(a5-a0)*fact3
            dsdx=(s5-s0)*fact3
            dmdx=(m5-m0)*fact3
        
        else if (xi.ge.xi5) then
            
            damdx=0.d0
            dmdx=0.d0
            dsdx=0.d0
            
        endif
        
    endif
    
    !!!
    !!! Calcul de d(k)/d(x)
    !!!
    
    dkdx=-damdx*log(2.d0/3.d0)/(2.d0*amx**2.d0)*kx
    
    !!!
    !!! Calcul de d(a)/d(x)
    !!!
    
    dagdx=1.d0/(sqrt(6.d0)*sigc*r0c)*(-dmdx*kx-mx*dkdx)
    
    !!!
    !!! Calcul de d(b)/d(x)
    !!!
    
    dbgdx=dmdx*kx/3.d0/sigc+mx/3.d0/sigc*dkdx
    
    !!!
    !!! Calcul de d(d)/d(x)
    !!!
    
    ddgdx=dsdx*kx+sx*dkdx
    
    !!!
    !!! Assemblage
    !!!
    
    terexp=agx*sii*rtheta+bgx*i1+dgx
    
    if (terexp.gt.0.d0) then
        
        do i=1,ndt
            dfdsdx(i)=-damdx*sigc*r0c*terexp**(amx-1.d0)*(agx*dshds(i)+ &
                      & bgx*vident(i))-amx*sigc*r0c*((damdx*log(terexp)+ &
                      & (amx-1.d0)/terexp*(dagdx*sii*rtheta+dbgdx*i1+ddgdx))* & 
                      & terexp**(amx-1.d0))*(agx*dshds(i)+bgx*vident(i))- &
                      & amx*sigc*r0c*terexp**(amx-1.d0)*(dagdx*dshds(i)+ &
                      & dbgdx*vident(i))
        end do
        
    else
        
        call lcinve(0.d0,dfdsdx)
        
    endif
    
    dpardx(1)=damdx
    dpardx(2)=dsdx
    dpardx(3)=dmdx

end subroutine

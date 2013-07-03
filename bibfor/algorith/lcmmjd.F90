subroutine lcmmjd(taur, materf, ifa, nmat, nbcomm,&
                  dt, ir, is, nbsys, nfs,&
                  nsg, hsr, vind, dy, dpdtau,&
                  dprdas, dhrdas, hr, dpr, sgnr,&
                  iret)
! aslint: disable=W1306,W1504
    implicit none
#include "asterfort/assert.h"
#include "asterfort/lcmmdc.h"
#include "asterfort/lcmmdh.h"
#include "asterfort/lcmmfe.h"
#include "asterfort/lcmmfi.h"
    integer :: ifa, nmat, nbcomm(nmat, 3), nfs, nsg
    real(kind=8) :: taur, materf(nmat*2), rr, dt, vind(36), dy(12)
    real(kind=8) :: dpdtau, dprdas, hsr(nsg, nsg), hr
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
! ======================================================================
!  CALCUL DES DERIVEES DES VARIABLES INTERNES DES LOIS MONOCRISTALLINES
!  POUR LA LOI D'ECOULEMENT  DD-CFC
!       IN  TAUR    :  SCISSION REDUITE
!           MATERF  :  PARAMETRES MATERIAU
!           IFA     :  NUMERO DE FAMILLE
!           NBCOMM  :  NOMBRE DE COEF MATERIAU PAR FAMILLE
!           DT      :  INCREMENT DE TEMPS
!           IR,IS   :  NUMEROS DE SYSTEMES DE GLISSEMENT
!           NBSYS   :  NOMBRE DE SYSTEMES DE GLISSEMENT (12 POUR CFC)
!           HSR     :  MATRICE D'INTERACTION
!           VIND,DY :  VARIABLES INTERNES A T ET SOLUTION A T+DT
!     OUT:
!           DPDTAU  :  dpr/dTaur
!           DPRDAS  :  dpr/dAlphas
!           DHRDAS  :  dHr/dAlphaS
!     ----------------------------------------------------------------
    real(kind=8) :: b, n, a, gamma0, r8b, dpr, dtrdas, critr, expbp(nsg)
    real(kind=8) :: ceff, dcdals, soms3, soms2, soms1
    real(kind=8) :: dhrdas, tauf, y, beta, mu, somaal, ars, sgnr, t3
    real(kind=8) :: alphap(12)
    integer :: ifl, is, nbsys, ir, iret, nuecou, iei, iu, i, is3, ir3
    character(len=16) :: k16b
    integer :: irr, decirr, nbsyst, decal, gdef
    common/polycr/irr,decirr,nbsyst,decal,gdef
!     ----------------------------------------------------------------
!
    ifl=nbcomm(ifa,1)+nmat
    nuecou=nint(materf(ifl))
    iei=nbcomm(ifa,3)+nmat
    iret=0
!
    if (nuecou .ne. 5) then
        call assert(.false.)
    endif
!             MATRICE JACOBIENNE DU SYSTEME :
!  R1 = D-1*SIGMA - (D_M-1*SIGMA_M)-(DEPS-DEPS_TH)+Somme(ms*Dps*S)=0
!  R2 = dALPHA - Dps*h(alphas)
! avec S=sgn(TAUR)
!
! ON VEUT CALCULER :
!     d(Dps)/dTAUR
    dpdtau=0.d0
!     d(R1)/dalphas
    hr=0.d0
!     d(R2)/d(alphar)
    dhrdas=0.d0
!     DPSDAR=d(Dp_s)/d(Alpha_r)
    dprdas=0.d0
!
    tauf  =materf(ifl+1)
    gamma0=materf(ifl+2)
    a     =materf(ifl+3)
    b     =materf(ifl+4)
    n     =materf(ifl+5)
    y     =materf(ifl+6)
    beta  =materf(iei+2)
    mu    =materf(iei+4)
    k16b=' '
!     CALCUL de l'écrouissage RR=TAUr_Forest
    call lcmmfi(materf(nmat+1), ifa, nmat, nbcomm, k16b,&
                ir, nbsys, vind, decal, dy,&
                nfs, nsg, hsr, 1, expbp,&
                rr)
    if (iret .gt. 0) goto 9999
!
!     CALCUL de l'écoulement dpr et du critère
    call lcmmfe(taur, materf(nmat+1), materf(1), ifa, nmat,&
                nbcomm, k16b, ir, nbsys, vind,&
                dy, rr, r8b, r8b, dt,&
                r8b, r8b, dpr, critr, sgnr,&
                nfs, nsg, hsr, iret)
    if (iret .gt. 0) goto 9999
!
!
!     1. d(Dp_s)/d(Tau_s)
    if (dpr .gt. 0.d0) then
        if (abs(taur) .gt. 0.d0) then
            dpdtau=n*(dpr+gamma0*dt)/taur
        endif
    endif
!
    do 55 iu = 1, nbsys
        alphap(iu)=vind(decal+3*(iu-1)+1)+dy(iu)
55  end do
!
    call lcmmdc(materf(nmat+1), ifa, nmat, nbcomm, alphap,&
                is, ceff, dcdals)
!
    call lcmmdh(materf(nmat+1), ifa, nmat, nbcomm, alphap,&
                nfs, nsg, hsr, nbsys, ir,&
                nuecou, hr, soms1, soms2, soms3)
!
!
    somaal=0.d0
    do 56 i = 1, 12
        if (alphap(i) .gt. 0.d0) then
            somaal=somaal+hsr(ir,i)*alphap(i)
        endif
56  end do
    somaal=sqrt(somaal)
    dtrdas=0.d0
!
    if (alphap(is) .gt. 0.d0) then
        if (somaal .gt. 0.d0) then
            dtrdas=ceff/2.d0/somaal*hsr(ir,is)
        endif
    endif
    dtrdas=dtrdas + dcdals*somaal
    dtrdas=mu*dtrdas
!
!     2. d(Dp_r)/d(Omega_s)
!
    if (dpr .gt. 0.d0) then
        dprdas=-n*(dpr+gamma0*dt)/(tauf+rr)*dtrdas
    endif
!
    ars=hsr(ir,is)
    if (ars*alphap(is) .gt. 0.d0) then
        t3=ars/2.d0/sqrt(ars*alphap(is))
    endif
!
    dhrdas=0.d0
!     IS APPARTIENT-IL A FOREST(IR) ?
!     division entiere
    is3=(is-1)/3
    ir3=(ir-1)/3
    if (is3 .ne. ir3) then
        if (alphap(is) .gt. 0.d0) then
            dhrdas=dhrdas + a*(sqrt(ars)/soms1)
        endif
    endif
!
    if (soms1 .gt. 0.d0) then
        dhrdas=dhrdas-a*t3*soms2/soms1/soms1
    endif
!     IS APPARTIENT-IL A COPLA(IR) ?
    if (is3 .eq. ir3) then
        if (ars*alphap(is) .gt. 0.d0) then
            dhrdas=dhrdas+b*t3*ceff
        endif
    endif
!
    dhrdas=dhrdas+b*soms3*dcdals
!
!     3. d(h_r)/d(Omega_s)
    if (is .eq. ir) dhrdas=dhrdas-y/beta
!
9999  continue
!
end subroutine

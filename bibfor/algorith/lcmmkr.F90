subroutine lcmmkr(taus, coeft, cisa2, ifa, nmat,&
                  nbcomm, is, nbsys, nfs, nsg,&
                  hsr, vind, dy, dt, dalpha,&
                  dgamma, dp, crit, sgns, iret)
!
    implicit none
#include "asterc/r8miem.h"
    integer :: ifa, nmat, nbcomm(nmat, 3), iret, nfs, nsg
    real(kind=8) :: taus, coeft(nmat), dgamma, dp, dt, taumu, tauv
    real(kind=8) :: sgns, hsr(nsg, nsg), dy(*)
    real(kind=8) :: vind(*), dalpha
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
! person_in_charge: jean-michel.proix at edf.fr
! ======================================================================
!  COMPORTEMENT MONOCRISTALLIN : ECOULEMENT (VISCO)PLASTIQUE
!  INTEGRATION DE LA LOIS MONOCRISTALLINES KOCKS-RAUCH. CALCUL DE DALPHA
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
!           DALPHA  :  VARIABLE dalpha pour Kocks-Rauch
!           DGAMMA  :  DEF PLAS
!           DP      :  DEF PLAS CUMULEE
!           CRIT    :  CRITERE
!           SGNS    :  SIGNE DE GAMMA
!           IRET    :  CODE RETOUR
! ======================================================================
!             RESOLUTION DU SYSTEME :
!  R1 = D-1*SIGMA - (D_M-1*SIGMA_M)-(DEPS-DEPS_TH)+Somme(ms*Gamma_s)=0
!      avec Gamma_s=g(Taus,alphas)
!  R2 = dALPHA - |g(Taus,alphas)|*h(alphas)
!     ----------------------------------------------------------------
    real(kind=8) :: p, q, k, crit, gamma0
    real(kind=8) :: tempf, tabs, deltag, ptit
    real(kind=8) :: taur, tau0, tauef, bsd, gcb, alphar, kdcs, som, deltgg
    real(kind=8) :: aux, cisa2, alphas, terme, petith, petitg
    integer :: ifl, is, iu, nbsys
!     ----------------------------------------------------------------
!     DANS VIS : 1 = ALPHA, 2=GAMMA, 3=P
!
    ifl=nbcomm(ifa,1)
    iret=0
    ptit=r8miem()
    k         =coeft(ifl+1)
    taur      =coeft(ifl+2)
    tau0      =coeft(ifl+3)
    gamma0    =coeft(ifl+4)
    deltag    =coeft(ifl+5)
    bsd       =coeft(ifl+6)
    gcb       =coeft(ifl+7)
    kdcs      =coeft(ifl+8)
    p         =coeft(ifl+9)
    q         =coeft(ifl+10)
    tempf     =coeft(ifl+11)
    tauv=abs(taus)-tau0
    crit=tauv
!
!
!      VARIABLE INTERNE PRINCIPALE : ALPHA
    iret=0
    if (abs(taus) .le. ptit) then
        sgns=1.d0
    else
        sgns=taus/abs(taus)
    endif
!
    if (tauv .gt. 0.d0) then
!
        som=0.d0
        taumu=0.d0
        alphas=vind(3*(is-1)+1)+dy(is)
        do 1 iu = 1, nbsys
            alphar=vind(3*(iu-1)+1)+dy(iu)
!           PARTIE POSITIVE DE ALPHA
            if (alphar .gt. 0.d0) then
                taumu = taumu + hsr(is,iu)*alphar
                if (iu .ne. is) som = som+alphar
            endif
 1      continue
!
        som=sqrt(som)
        taumu = cisa2 * taumu/tauv
        tauef = tauv-taumu
        crit=tauef
        if (tauef .gt. 0.d0) then
            aux= (1.d0-(tauef/taur)**p)
            if (aux .le. 0.d0) then
                iret=1
                goto 9999
            endif
!          PROTECTION DE l'EXPONENTIELLE
            tabs=tempf+273.15d0
            deltgg=deltag*(aux**q)
            terme=-deltgg/k/tabs
            if (terme .gt. 10.d0) then
                iret=1
                goto 9999
            endif
            petitg=gamma0*exp(terme)*dt
            dgamma=petitg*sgns
            dp=petitg
            petith=bsd+som/kdcs-gcb*alphas
!          LES VALEURS NEGATIVES DE PETITH
!          SONT INTERDITES (CELA DEVRAIT ETRE DANS LE MODELE)
            if (petith .lt. 0.d0) petith=0.d0
            dalpha=petitg*petith
        else
            dgamma=0.d0
            dp=0.d0
            dalpha=0.d0
        endif
    else
        dgamma=0.d0
        dp=0.d0
        dalpha=0.d0
    endif
9999  continue
end subroutine

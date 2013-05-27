subroutine lcmmlc(nmat, nbcomm, cpmono, nfs, nsg,&
                  hsr, nsfv, nsfa, ifa, nbsys,&
                  is, dt, nvi, vind, yd,&
                  dy, itmax, toler, materf, expbp,&
                  taus, dalpha, dgamma, dp, crit,&
                  sgns, rp, iret)
    implicit none
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! TOLE CRP_21
!       ----------------------------------------------------------------
!     MONOCRISTAL  : CALCUL DE L'ECOULEMENT VISCOPLASTIQUE
!     IN  NMAT   :  DIMENSION MATER
!         NBCOMM :  INCIDES DES COEF MATERIAU
!         CPMONO :  NOM DES COMPORTEMENTS
!         HSR    :  MATRICE D'INTERACTION
!         NVI    :  NOMBRE DE VARIABLES INTERNES
!         NSFV   :  DEBUT DES SYST. GLIS. DE LA FAMILLE IFA DANS VIND
!         NSFA   :  DEBUT DES SYST. GLIS. DE LA FAMILLE IFA DANS Y
!         NBSYS  :  NOMBRE DE SYSTEMES DE LA FAMILLE IFA
!         IS     :  NUMERO DU SYST. GLIS. ACTUEL
!         DT     :  ACCROISSEMENT DE TEMPS
!         NVI    :  NOMBRE DE VARIABLES INTERNES AU TOTAL
!         VIND   :  VARIABLES INTERNES A L'INSTANT PRECEDENT
!         YD     :  VARIABLES A T
!         DY     :  SOLUTION ESSAI
!         ITMAX  :  ITER_INTE_MAXI
!         TOLER  :  RESI_INTE_RELA
!         MATERF :  COEFFICIENTS MATERIAU A T+DT
!         EXPBP  :  EXPONENTIELLES POUR LE COMPORTEMENT VISC1
!         TAUS   :  SCISSION REDUITE SYSTEME IS
!
!  OUT    DALPHA :  DALPHA ENTRE T ET T+DT ITERATION COURANTE
!         DGAMMA :  DGAMMA ENTRE T ET T+DT ITERATION COURANTE
!         DP     :  NORME DE L'ACCROISSEMENT GLIS.PLAS.
!         CRIT   :  CRITERE ATTEINT POUR TAUS (SUIVANT LA LOI)
!         SGNS   :  SIGNE DE TAUS (-C.ALPHA POUR VISC1)
!         RP     :  ECROUISSAGE
!         IRET   :  CODE RETOUR
!     ----------------------------------------------------------------
    include 'asterfort/lcmmec.h'
    include 'asterfort/lcmmfc.h'
    include 'asterfort/lcmmfe.h'
    include 'asterfort/lcmmfi.h'
    integer :: nmat, nvi, nsfv, iret, ifl, nfs, nsg
    integer :: nuvi, ifa, nbsys, is, itmax, iexp, irk
    integer :: nbcomm(nmat, 3), nsfa, nuecou
    real(kind=8) :: dt, vind(nvi)
    real(kind=8) :: materf(nmat*2), dy(*), yd(*), toler
    real(kind=8) :: taus, dgamma, dalpha, dp, rp
    real(kind=8) :: hsr(nsg, nsg)
    real(kind=8) :: dgamm1, alpham
    real(kind=8) :: crit, alphap, sgns, gammap, expbp(nsg)
    character(len=24) :: cpmono(5*nmat+1)
    character(len=16) :: necoul, necris, necrci
    integer :: irr, decirr, nbsyst, decal, gdef
    common/polycr/irr,decirr,nbsyst,decal,gdef
!     ----------------------------------------------------------------
!
    ifl=nbcomm(ifa,1)
    nuecou=nint(materf(nmat+ifl))
    necoul=cpmono(5*(ifa-1)+3)
    necris=cpmono(5*(ifa-1)+4)
    necrci=cpmono(5*(ifa-1)+5)
!
!     SI IRK=1, ON EST EN EXPLICITE RUNGE_KUTTA
!
    irk=0
    if (dt .lt. 0.d0) then
        irk=1
        dt=1.d0
    endif
!
    nuvi=nsfv+3*(is-1)
!
    if (irk .eq. 0) then
!     ECROUISSAGE CINEMATIQUE - CALCUL DE DALPHA-SAUF MODELES DD
        if (nuecou .lt. 4) then
!
            dgamm1=dy(nsfa+is)
            alpham=vind(nuvi+1)
            call lcmmfc(materf(nmat+1), ifa, nmat, nbcomm, necrci,&
                        itmax, toler, alpham, dgamm1, dalpha,&
                        iret)
!
            alphap=alpham+dalpha
            gammap=yd(nsfa+is)+dgamm1
        else
!           POUR DD_*,  ALPHA est la variable principale
            alphap=yd(nsfa+is)+dy(nsfa+is)
        endif
    else
        alphap=vind(nuvi+1)
        gammap=vind(nuvi+2)
    endif
!
    if (nuecou .ne. 4) then
!        ECROUISSAGE ISOTROPE : CALCUL DE R(P)
        iexp=0
        if (is .eq. 1) iexp=1
        call lcmmfi(materf(nmat+1), ifa, nmat, nbcomm, necris,&
                    is, nbsys, vind, nsfv, dy(nsfa+1),&
                    nfs, nsg, hsr, iexp, expbp,&
                    rp)
    endif
!
!     ECOULEMENT VISCOPLASTIQUE
!     ROUTINE COMMUNE A L'IMPLICITE (PLASTI-LCPLNL)
!     ET L'EXPLICITE (NMVPRK-GERPAS-RK21CO-RDIF01)
!     CAS IMPLICITE : IL FAUT PRENDRE EN COMPTE DT
!     CAS EXPLICITE : IL NE LE FAUT PAS (C'EST FAIT PAR RDIF01)
    decal=nsfv
    call lcmmfe(taus, materf(nmat+1), materf(1), ifa, nmat,&
                nbcomm, necoul, is, nbsys, vind,&
                dy(nsfa+1), rp, alphap, gammap, dt,&
                dalpha, dgamma, dp, crit, sgns,&
                nfs, nsg, hsr, iret)
!
    if (irk .eq. 1) then
        if (nuecou .lt. 4) then
!         ECROUISSAGE CINEMATIQUE EN RUNGE-KUTTA
            call lcmmec(materf(nmat+1), ifa, nmat, nbcomm, necrci,&
                        itmax, toler, alphap, dgamma, dalpha,&
                        iret)
        endif
    endif
!
end subroutine

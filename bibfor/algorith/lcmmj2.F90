subroutine lcmmj2(taur, materf, cpmono, ifa, nmat,&
                  nbcomm, dt, nsfv, nsfa, ir,&
                  is, nbsys, nfs, nsg, hsr,&
                  vind, dy, dgsdts, dksdts, dgrdbs,&
                  dksdbr, iret)
    implicit none
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! TOLE CRP_21
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
! TOLE CRS_1404
!       MONOCRISTAL : DERIVEES DES TERMES UTILES POUR LE CALCUL
!                    DU JACOBIEN DU SYSTEME NL A RESOUDRE = DRDY
!                    cf. R5.03.11, COMPO DD_KR
!       IN
!           TAUR   :  SCISSION REDUITE SYSTEME IR
!           MATERF :  COEFFICIENTS MATERIAU A T+DT
!           CPMONO :  NOM DES COMPORTEMENTS
!           IFA    :  NUMERO FAMILLE
!           NMAT   :  DIMENSION MATER
!           NBCOMM :  INCIDES DES COEF MATERIAU
!           DT     :  ACCROISSEMENT INSTANT ACTUEL
!           NSFV   :  DEBUT DES SYST. GLIS. DE LA FAMILLE IFA DANS VIND
!           NSFA   :  DEBUT DES SYST. GLIS. DE LA FAMILLE IFA DANS Y
!           IS     :  NUMERO DU SYST. GLIS. S
!           IR     :  NUMERO DU SYST. GLIS. R
!           NBSYS  :  NOMBRE DE SYSTEMES DE GLISSEMENT FAMILLE IFA
!           HSR    :  MATRICE D'INTERACTION
!           VIND   :  VARIABLES INTERNES A L'INSTANT PRECEDENT
!           DY     :  SOLUTION           =  ( DSIG DX1 DX2 DP (DEPS3) )
!       OUT DGSDTS :  derivee dGammaS/dTauS
!       OUT DKSDTS :  dkS/dTaus
!       OUT DGRDBS :  dGammaR/dBetaS
!       OUT DKRDBS :  dkR/dBetaS
!       OUT IRET   :  CODE RETOUR
!       ----------------------------------------------------------------
    include 'asterfort/lcmmfe.h'
    include 'asterfort/lcmmjf.h'
    integer :: nmat, nuvr, nbcomm(nmat, 3), nuvi, ifa, nbsys, is, iret, nfs, nsg
    integer :: ir, nsfa, nsfv
    real(kind=8) :: vind(*), dgdtau, hsr(nsg, nsg), dgsdts, dksdts, dgrdbs
    real(kind=8) :: dksdbr
    real(kind=8) :: dhdalr, hs, taur, dp, dy(*), materf(nmat*2), dt, rp, dgdalr
    real(kind=8) :: dfdrr
    real(kind=8) :: alpham, dalpha, alphap, crit, dgamma, sgns, gammap, petith
    real(kind=8) :: sgnr
    character(len=16) :: necoul
    character(len=24) :: cpmono(5*nmat+1)
    integer :: irr, decirr, nbsyst, decal, gdef
    common/polycr/irr,decirr,nbsyst,decal,gdef
!     ----------------------------------------------------------------
!
    iret=0
!
    dgsdts=0.d0
    dksdts=0.d0
    dgrdbs=0.d0
    dksdbr=0.d0
!
    nuvr=nsfa+ir
    nuvi=nsfv+3*(ir-1)
    necoul=cpmono(5*(ifa-1)+3)(1:16)
    alpham=vind(nuvi+1)
    gammap=vind(nuvi+2)
    alphap=alpham+dy(nuvr)
    decal=nsfv
    call lcmmfe(taur, materf(nmat+1), materf(1), ifa, nmat,&
                nbcomm, necoul, ir, nbsys, vind,&
                dy(nsfa+1), rp, alphap, gammap, dt,&
                dalpha, dgamma, dp, crit, sgns,&
                nfs, nsg, hsr, iret)
    if (iret .gt. 0) goto 9999
    if (crit .gt. 0.d0) then
!        CALCUL de dF/dtau
        call lcmmjf(taur, materf(nmat+1), materf(1), ifa, nmat,&
                    nbcomm, dt, necoul, ir, is,&
                    nbsys, vind(nsfv+1), dy(nsfa+1), nfs, nsg,&
                    hsr, rp, alphap, dalpha, gammap,&
                    dgamma, sgnr, dgdtau, dgdalr, dfdrr,&
                    petith, iret)
        if (iret .gt. 0) goto 9999
        dgsdts=dgdtau
        dksdts=abs(dgdtau)*petith
        dgrdbs=dgdalr*sgnr
        dhdalr=dfdrr
        hs=petith
        dksdbr=dgdalr*hs+dp*dhdalr
    endif
!
9999  continue
end subroutine

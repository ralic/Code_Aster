subroutine lcmmjb(taur, materf, cpmono, ifa, nmat,&
                  nbcomm, dt, nuecou, nsfv, nsfa,&
                  ir, is, nbsys, nfs, nsg,&
                  hsr, vind, dy, iexp, expbp,&
                  itmax, toler, dgsdts, dksdts, dgrdbs,&
                  dkrdbs, iret)
! aslint: disable=W1504
    implicit none
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
!       ----------------------------------------------------------------
!       MONOCRISTAL : DERIVEES DES TERMES UTILES POUR LE CALCUL
!                    DU JACOBIEN DU SYSTEME NL A RESOUDRE = DRDY
!                    cf. R5.03.11
!       IN
!           TAUR   :  SCISSION REDUITE SYSTEME IR
!           MATERF :  COEFFICIENTS MATERIAU A T+DT
!           CPMONO :  NOM DES COMPORTEMENTS
!           IFA    :  NUMERO FAMILLE
!           NMAT   :  DIMENSION MATER
!           NBCOMM :  INCIDES DES COEF MATERIAU
!           DT     :  ACCROISSEMENT INSTANT ACTUEL
!           NUECOU : NUMERO DE LA LOI D'ECOULEMENT
!           NSFV   :  DEBUT DES SYST. GLIS. DE LA FAMILLE IFA DANS VIND
!           NSFA   :  DEBUT DES SYST. GLIS. DE LA FAMILLE IFA DANS Y
!           IS     :  NUMERO DU SYST. GLIS. S
!           IR     :  NUMERO DU SYST. GLIS. R
!           NBSYS  :  NOMBRE DE SYSTEMES DE GLISSEMENT FAMILLE IFA
!           HSR    :  MATRICE D'INTERACTION
!           VIND   :  VARIABLES INTERNES A L'INSTANT PRECEDENT
!           DY     :  SOLUTION           =  ( DSIG DX1 DX2 DP (DEPS3) )
!           ITMAX  :  ITER_INTE_MAXI
!           TOLER  :  RESI_INTE_RELA
!       OUT DGSDTS :  derivee dGammaS/dTauS
!       OUT DKSDTS :  dkS/dTaus
!       OUT DGRDBS :  dGammaR/dBetaS
!       OUT DKRDBS :  dkR/dBetaS
!       OUT IRET   :  CODE RETOUR
!       ----------------------------------------------------------------
    include 'asterfort/lcmmj1.h'
    include 'asterfort/lcmmj2.h'
    include 'asterfort/lcmmjd.h'
    include 'asterfort/u2mess.h'
    integer :: nmat, nbcomm(nmat, 3), ifa, nbsys, is, iret, nfs, nsg
    integer :: ir, nsfa, nsfv, nuecou, itmax, iexp
    real(kind=8) :: dgsdts, dksdts, dgrdbs, dkrdbs, vind(*), hsr(nsg, nsg)
    real(kind=8) :: materf(nmat*2), dt, sgnr, hr, dpr, dpdtau, dprdas, dhrdas
    real(kind=8) :: toler
    real(kind=8) :: taur, dy(*), expbp(nsg)
    character(len=24) :: cpmono(5*nmat+1)
    integer :: irr, decirr, nbsyst, decal, gdef
    common/polycr/irr,decirr,nbsyst,decal,gdef
!     ----------------------------------------------------------------
!
    iret=0
!
    if (nuecou .eq. 4) then
!        KOCKS-RAUCH
        call lcmmj2(taur, materf, cpmono, ifa, nmat,&
                    nbcomm, dt, nsfv, nsfa, ir,&
                    is, nbsys, nfs, nsg, hsr,&
                    vind, dy, dgsdts, dksdts, dgrdbs,&
                    dkrdbs, iret)
    else if (nuecou.eq.5) then
!        DD-CFC
        decal=nsfv
        call lcmmjd(taur, materf, ifa, nmat, nbcomm,&
                    dt, ir, is, nbsys, nfs,&
                    nsg, hsr, vind, dy(nsfa+1), dpdtau,&
                    dprdas, dhrdas, hr, dpr, sgnr,&
                    iret)
!
        dgsdts=dpdtau*sgnr
        dksdts=dpdtau*hr
        dgrdbs=dprdas*sgnr
        dkrdbs=dprdas*hr+dpr*dhrdas
    else if (nuecou.eq.6) then
!        DD-FAT
        call u2mess('F', 'COMPOR2_21')
    else if (nuecou.ge.7) then
!        DD-CC
!        matrice tangente pas encore programmee
!        mais pourquoi EXTRAPOLE appelle RIGI_MECA_TANG ?
        dgsdts=0.d0
        dksdts=0.d0
        dgrdbs=0.d0
        dkrdbs=0.d0
    else
!        AUTRES COMPORTEMENTS
        call lcmmj1(taur, materf, cpmono, ifa, nmat,&
                    nbcomm, dt, nsfv, nsfa, ir,&
                    is, nbsys, nfs, nsg, hsr,&
                    vind, dy, iexp, expbp, itmax,&
                    toler, dgsdts, dksdts, dgrdbs, dkrdbs,&
                    iret)
    endif
!
end subroutine

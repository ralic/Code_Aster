subroutine irrjpl(model, nmat, mater, sigf, vind,&
                  vinf, dsde)
!
    implicit none
    include 'asterc/r8prem.h'
    include 'asterfort/irrfss.h'
    include 'asterfort/lcdevi.h'
    include 'asterfort/lceqma.h'
    include 'asterfort/lcinma.h'
    include 'asterfort/lcnrts.h'
    include 'asterfort/lcopil.h'
    include 'asterfort/lcprsm.h'
    include 'asterfort/lcprsv.h'
    include 'asterfort/lcprte.h'
    include 'asterfort/lcsoma.h'
    include 'asterfort/mgauss.h'
    character(len=8) :: model
    integer :: nmat
    real(kind=8) :: mater(nmat, 2), dsde(6, 6), sigf(6)
    real(kind=8) :: vind(*), vinf(*)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: jean-luc.flejou at edf.fr
!       ----------------------------------------------------------------
!       IRRAD3M   :  MATRICE SYMETRIQUE DE COMPORTEMENT TANGENT
!                     ELASTO_PLASTIQUE EN VITESSE A T OU T+DT
!       ----------------------------------------------------------------
!       IN  FAMI   :  FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
!           KPG,KSP:  NUMERO DU (SOUS)POINT DE GAUSS
!           MOD    :  TYPE DE MODELISATION
!           NMAT   :  DIMENSION MATER
!           MATER  :  COEFFICIENTS MATERIAU
!           NR     :  TAILLE DE LA MATRICE JACOBIENNE
!           SIGF   :  CONTRAINTES A T+DT
!           VIND   :  VARIABLES INTERNES A T
!           VINF   :  VARIABLES INTERNES A T+DT
!       OUT DSDE   :  MATRICE DE COMPORTEMENT TANGENT = DSIG/DEPS
!       ----------------------------------------------------------------
    integer :: ndt, ndi
!     ------------------------------------------------------------------
    common /tdim/ ndt,ndi
!
    real(kind=8) :: det, mat(6, 6), i4(6, 6)
    real(kind=8) :: fkooh(6, 6), dphi, pf, etaif, dp, detai, dpi
    real(kind=8) :: ai0, etais, k, n, p0, kappa, r02, zetaf, penpe, pk, pe, spe
    real(kind=8) :: sr
    real(kind=8) :: ddfdds(6, 6), drsds(6, 6), sequiv, dev(6), dfds(6), drids
    real(kind=8) :: drpdp
!
    integer :: iret
    logical :: ldrpdp
!     ------------------------------------------------------------------
    data  i4    /1.0d0   , 0.0d0  , 0.0d0  , 0.0d0  ,0.0d0  ,0.0d0,&
     &             0.0d0   , 1.0d0  , 0.0d0  , 0.0d0  ,0.0d0  ,0.0d0,&
     &             0.0d0   , 0.0d0  , 1.0d0  , 0.0d0  ,0.0d0  ,0.0d0,&
     &             0.0d0   , 0.0d0  , 0.0d0  , 1.0d0  ,0.0d0  ,0.0d0,&
     &             0.0d0   , 0.0d0  , 0.0d0  , 0.0d0  ,1.0d0  ,0.0d0,&
     &             0.0d0   , 0.0d0  , 0.0d0  , 0.0d0  ,0.0d0  ,1.0d0/
!     ------------------------------------------------------------------
!
!     CALCUL DE LA MATRICE JACOBIENNE ==> methode b (plus sure)
!     a) Faire appel a IRRJAC
!        certaines des équations sont normées   ==> précautions
!        si le jacobien est changé              ==> répercutions
!        CALL IRRJAC (FAMI,KPG,KSP,MOD,NMAT,MATER,YF,DY,NR,DRDY)
!     b) Calcul qui ressemble a IRRJAC
!        independant de IRRJAC
!        adaptation du code de IRRJAC
!
    call lcopil('ISOTROPE', model, mater(1, 1), fkooh)
!     RECUPERATION DES VARIABLES INTERNES A t+
    pf = vinf(1)
    etaif = vinf(2)
!     RECUPERATION DES INCREMENTS DES VARIABLES INTERNES
    dp = vinf(1) - vind(1)
    detai = vinf(2) - vind(2)
    dpi = vinf(3) - vind(3)
!
!     CARACTERISTIQUES MATERIAUX
    ai0 = mater(4,2)
    etais = mater(5,2)
    k = mater(7,2)
    n = mater(8,2)
    p0 = mater(9,2)
    kappa = mater(10,2)
    r02 = mater(11,2)
    zetaf = mater(12,2)
    penpe = mater(13,2)
    pk = mater(14,2)
    pe = mater(15,2)
    spe = mater(16,2)
!     INCREMENT D'IRRADIATION
    dphi = mater(23,2)
!
!     Calcul de DRSDS : (6,6)
    call irrfss(sigf, ddfdds)
    call lcprsm((dp+dpi), ddfdds, drsds)
    call lcsoma(fkooh, drsds, drsds)
!
!     Calcul de DRPDP : scalaire
!     loi de comportement : Calcul du seuil
    if (pf .lt. pk) then
        sr = kappa*r02
    else if (pf .lt. pe) then
        sr = penpe*(pf - pe) + spe
    else
        sr = k*((pf + p0)**n)
    endif
!     Calcul de Sigma equivalent
    call lcdevi(sigf, dev)
    sequiv = lcnrts(dev)
    ldrpdp = .true.
    if (((sequiv.ge.sr).and.(dp.ge.0.d0)) .or. (dp.gt.r8prem())) then
        if (pf .lt. pk) then
            ldrpdp = .false.
        else if (pf .lt. pe) then
            drpdp = penpe
        else
            drpdp = n*k*((pf+p0)**(n-1.d0))
        endif
    else
        ldrpdp = .false.
    endif
!     Calcul de DRIDS : scalaire
    if ((etaif-detai) .gt. etais) then
        drids = ai0*dphi*zetaf
    else if (etaif .le. etais) then
        drids = 0.0d0
    else if (detai .gt. 0.0d0) then
        drids = ai0*dphi*zetaf*(etaif-etais)/detai
    else
        drids = 0.0d0
    endif
    if (sequiv .eq. 0.0d0) then
        call lcinma(0.0d0, ddfdds)
    else
        call lcprsv(1.5d0/sequiv, dev, dfds)
        call lcprte(dfds, dfds, ddfdds)
    endif
!
    if (ldrpdp) then
        call lcprsm(1.0d0/drpdp + drids, ddfdds, ddfdds)
    else
        call lcprsm(drids, ddfdds, ddfdds)
    endif
!
!     Assemblage de DRSDS et DDFDDS : (6,6)
    call lcsoma(drsds, ddfdds, mat)
!
!     Inversion de MAT : DSDE(6,6)
    call lceqma(i4, dsde)
    call mgauss('NFVP', mat, dsde, 6, ndt,&
                ndt, det, iret)
!
end subroutine

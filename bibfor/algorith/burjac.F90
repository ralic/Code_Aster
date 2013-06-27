subroutine burjac(mod, nmat, materd, materf, nvi,&
                  vind, timed, timef, yd, yf,&
                  dy, nr, drdy)
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
! person_in_charge: alexandre.foucault at edf.fr
!       ----------------------------------------------------------------
!       CALCUL DU JACOBIEN DU SYSTEME NL A RESOUDRE = DRDY(DY)
!       POUR LE MODELE BETON_BURGER_FP
!       IN  MOD    :  TYPE DE MODELISATION
!           NMAT   :  DIMENSION MATER
!           MATERD :  COEFFICIENTS MATERIAU A T
!           MATERF :  COEFFICIENTS MATERIAU A T+DT
!           YF     :  VARIABLES A T + DT =    ( SIGF  VINF  (EPS3F)  )
!           DEPS   :  INCREMENT DE DEFORMATION
!           TIMED  :  INSTANT  T
!           TIMEF  :  INSTANT  T+DT
!           NR     :  DIMENSION DECLAREE DRDY
!           NVI    :  NOMBRE DE VARIABLES INTERNES
!           VIND   :  VARIABLE INTERNES A T
!           EPSD   :  DEFORMATION A T
!           YD     :  VARIABLES A T   = ( SIGD  VARD  ) A T
!           DY     :  SOLUTION           =    ( DSIG  DVIN  (DEPS3)  )
!       OUT DRDY   :  JACOBIEN DU SYSTEME NON LINEAIRE
!       ----------------------------------------------------------------
    implicit none
!     ----------------------------------------------------------------
    include 'asterfort/burafd.h'
    include 'asterfort/burafr.h'
    include 'asterfort/lcicma.h'
    include 'asterfort/lcinma.h'
    include 'asterfort/lcinve.h'
    include 'asterfort/lcopli.h'
    include 'asterfort/lcprmm.h'
    include 'asterfort/lcprmv.h'
    include 'asterfort/lcprsc.h'
    include 'asterfort/lcprsm.h'
    include 'asterfort/lcprsv.h'
    include 'asterfort/lcprte.h'
    include 'asterfort/lcsoma.h'
    include 'asterfort/mgauss.h'
    common /tdim/   ndt  , ndi
!     ----------------------------------------------------------------
    integer :: i, ndt, ndi, nmat, nr, nvi, iret
    real(kind=8) :: drdy(nr, nr), yf(nr), dy(nr), yd(nr)
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2), vind(nvi)
    real(kind=8) :: invela(6, 6), hook(6, 6), timed, timef, damp
    real(kind=8) :: etai0(6, 6), visco(6)
    real(kind=8) :: afr(6), bfr(6, 6), cfr(6, 6)
    real(kind=8) :: afd(6), bfd(6, 6), cfd(6, 6), cft(6, 6)
    real(kind=8) :: temp(6, 6), det, dr1dy1(6, 6)
    real(kind=8) :: identi(6, 6), dr1dy2(6, 6), dr1dy3(6)
    real(kind=8) :: dt, hini, scal, dr2dy1(6, 6)
    real(kind=8) :: kappa, etas, etad, epsfif(6), depsfi(6), epsfid(6)
    real(kind=8) :: nfif, nfid, coef, cepsfi(6), ndfi, normal(6)
    real(kind=8) :: dr2dy2(6, 6), drdyt(6, 6), youm1
    real(kind=8) :: dr2dy3(6), dr3dy1(6), dr3dy2(6), dr3dy3(1)
    real(kind=8) :: d2fi1(6, 6), d2fi2(6, 6), mident(6, 6)
    character(len=8) :: mod
!
! === =================================================================
! --- INITIALISATION DES VARIABLES
! === =================================================================
    call lcinma(0.d0, invela)
    call lcinma(0.d0, identi)
! === =================================================================
! --- CONSTRUCTION TENSEUR RIGIDITE ELASTIQUE A T+DT
! === =================================================================
    call lcopli('ISOTROPE', '3D      ', materf, hook)
    damp = materf(6,2)
! === =================================================================
! --- INVERSION TENSEUR RIGIDITE ELASTIQUE
! === =================================================================
    do 1 i = 1, ndt
        invela(i,i) = 1.d0
        identi(i,i) = 1.d0
 1  end do
    call mgauss('NFVP', hook, invela, 6, ndt,&
                ndt, det, iret)
! === =================================================================
! --- RECUPERATION DU TENSEUR FLUAGE PROPRE REVERSIBLE CFR (ORDRE 4)
! === =================================================================
    call burafr(vind, nvi, materd, materf, nmat,&
                timed, timef, afr, bfr, cfr)
! === =================================================================
! --- RECUPERATION DU TENSEUR FLUAGE DESSICATION CFD (ORDRE 4)
! === =================================================================
    call burafd(materd, materf, nmat, afd, bfd,&
                cfd)
! === =================================================================
! --- ASSEMBLAGE DES TERMES CFR+CFD (TENSEUR ORDRE 4)
! === =================================================================
    call lcsoma(cfr, cfd, cft)
! === =================================================================
! --- CALCUL DE DR(1:NDT)/DY(1:NDT)= DR1DY1
! === =================================================================
    call lcsoma(invela, cft, temp)
    call lcprsm(-1.d0, temp, dr1dy1)
!
! *********************************************************************
! === =================================================================
! --- CALCUL DE DR(1:NDT)/DY(NDT+1:2*NDT)= DR1DY2
! === =================================================================
    call lcprsm(-1.d0, identi, dr1dy2)
!
! *********************************************************************
! === =================================================================
! --- CALCUL DE DR(NDT+1:NDT)/DY(1:NDT)= DR2DY1 + MISE A L'ECHELLE
! === =================================================================
    dt = timef-timed
    hini = materd(6,1)
    damp = damp/dt
    scal = -dt/2.d0*hini/damp
    call lcprsm(scal, identi, dr2dy1)
!
! *********************************************************************
! === =================================================================
! --- CALCUL DE DR(NDT+1:NDT)/DY(NDT+1:2*NDT)= DR2DY2
! === =================================================================
    etas = materd(3,2)
    etad = materd(6,2)
    kappa = materd(7,2)
! === =================================================================
! --- RECUPERATION DES DEFORMATIONS IRREVERSIBLES A T+DT
! === =================================================================
    do 2 i = 1, ndt
        epsfif(i) = yf(ndt+i)
        epsfid(i) = yd(ndt+i)
        depsfi(i) = dy(ndt+i)
 2  end do
! === =================================================================
! --- CALCUL DE LA NORME DES DEFORMATIONS IRREVERSIBLES A T+DT -> NFIF
! === =================================================================
    call lcprsc(epsfif, epsfif, nfif)
    nfif = sqrt(nfif)
    if(nfif.lt.vind(21))nfif = vind(21)
! === =================================================================
! --- CALCUL DE LA NORME DES DEFORMATIONS IRREVERSIBLES A T -> NFID
! === =================================================================
    call lcprsc(epsfid, epsfid, nfid)
    nfid = sqrt(nfid)
    if(nfid.lt.vind(21))nfid = vind(21)
! === =================================================================
! --- CALCUL DE EXP(NFIF/KAPPA)/(2*NFIF)
! === =================================================================
    if ((abs(nfif).ne.0.d0) .and. ((nfif/kappa).lt.1.d2)) then
        coef = exp(nfif/kappa)/(nfif)
    else
        coef = 0.d0
    endif
! === =================================================================
! --- CALCUL DU PRODUIT SCALAIRE*VECTEUR: COEF*EPSFIF
! === =================================================================
    call lcprsv(coef, epsfif, cepsfi)
! === =================================================================
! --- CONSTRUCTION TENSEUR ORDRE 4 DE VISCOSITE ETA_I^0
! === =================================================================
    call lcinma(0.d0, etai0)
    do 3 i = 1, ndi
        etai0(i,i) = (etas+2.d0*etad)/3.d0
        etai0(i+ndi,i+ndi) = etad
 3  end do
    etai0(1,2) = (etas-etad)/3
    etai0(2,1) = etai0(1,2)
    etai0(3,1) = etai0(1,2)
    etai0(1,3) = etai0(1,2)
    etai0(2,3) = etai0(1,2)
    etai0(3,2) = etai0(1,2)
! === =================================================================
! --- CALCUL DE LA NORME DES INCREMENTS DE DEFORMATIONS IRREVERSIBLES
! === =================================================================
    call lcprsc(depsfi, depsfi, ndfi)
    if (abs(ndfi) .ne. 0.d0) then
        ndfi = 1.d0/sqrt(ndfi)
    else
        ndfi = 0.d0
    endif
! === =================================================================
! --- CALCUL DE LA DIRECTION DES INCREMENTS DEFORMATION IRREVERSIBLE
! === =================================================================
    call lcprsv(ndfi, depsfi, normal)
! === =================================================================
! --- CALCUL DE ETAI0(ORDRE4)*NORMAL(ORDRE2)=VISCO
! === =================================================================
    call lcprmv(etai0, normal, visco)
! === =================================================================
! --- PRODUIT TENSORIEL VISCO(X)CEPSFI
! === =================================================================
    call lcprte(visco, cepsfi, drdyt)
! === =================================================================
! --- SUITE - DERIVEE PAR RAPPORT A LA NORMALE D'ECOULEMENT
! === =================================================================
    call lcprte(depsfi, depsfi, d2fi1)
    scal = -ndfi*ndfi*ndfi
    call lcprsm(scal, d2fi1, d2fi2)
    call lcprsm(ndfi, identi, mident)
    call lcsoma(mident, d2fi2, d2fi1)
    if (((nfif/kappa).lt.1.d2) .and. ((nfid/kappa).lt.1.d2)) then
        scal = kappa*(exp(nfif/kappa)-exp(nfid/kappa))
    else
        scal = 0.d0
    endif
    call lcprsm(scal, etai0, temp)
    call lcprmm(temp, d2fi1, d2fi2)
    call lcsoma(d2fi2, drdyt, d2fi1)
! === =================================================================
! --- MISE A L'ECHELLE DE DR2DY2
! === =================================================================
    youm1 = 1.d0/damp
    call lcprsm(youm1, d2fi1, dr2dy2)
!
! *********************************************************************
! === =================================================================
! --- REMPLISSAGE DU JACOBIEN
! === =================================================================
    call lcicma(dr1dy1, 6, 6, ndt, ndt,&
                1, 1, drdy, nr, nr,&
                1, 1)
    call lcicma(dr1dy2, 6, 6, ndt, ndt,&
                1, 1, drdy, nr, nr,&
                1, ndt+1)
    call lcicma(dr2dy1, 6, 6, ndt, ndt,&
                1, 1, drdy, nr, nr,&
                ndt+1, 1)
    call lcicma(dr2dy2, 6, 6, ndt, ndt,&
                1, 1, drdy, nr, nr,&
                ndt+1, ndt+1)
!
! *********************************************************************
! --- MODELISATION C_PLAN
! *********************************************************************
    if (mod(1:6) .eq. 'C_PLAN') then
! === =================================================================
! --- CALCUL DE DR(1:NDT)/DY(NR)= DR1DY3
! === =================================================================
        call lcinve(0.d0, dr1dy3)
        dr1dy3(3) = -1.d0
! === =================================================================
! --- CALCUL DE DR(NDT+1:2*NDT)/DY(NR)= DR2DY3
! === =================================================================
        call lcinve(0.d0, dr2dy3)
! === =================================================================
! --- CALCUL DE DR(NR)/DY(1:NDT)= DR3DY1
! === =================================================================
        dr3dy1(1) = hook(3,1)*cft(1,1)+hook(3,2)*cft(2,1)+ hook(3,3)* cft(3,1)+hook(3,4)*cft(4,1)
        dr3dy1(2) = hook(3,1)*cft(1,2)+hook(3,2)*cft(2,2)+ hook(3,3)* cft(3,2)+hook(3,4)*cft(4,2)
        dr3dy1(3) = hook(3,1)*cft(1,3)+hook(3,2)*cft(2,3)+ hook(3,3)* cft(3,3)+hook(3,4)*cft(4,3)
        dr3dy1(4) = hook(3,1)*cft(1,4)+hook(3,2)*cft(2,4)+ hook(3,3)* cft(3,4)+hook(3,4)*cft(4,4)
! === =================================================================
! --- CALCUL DE DR(NR)/DY(NDT+1:2*NDT)= DR3DY2=-I
! === =================================================================
        dr3dy2(1) = -hook(3,1)
        dr3dy2(2) = -hook(3,2)
        dr3dy2(3) = -hook(3,3)
        dr3dy2(4) = -hook(3,4)
! === =================================================================
! --- CALCUL DE DR(NR)/DY(NR)= HOOK(3,3)
! === =================================================================
        dr3dy3(1) = hook(3,3)
!
! === =================================================================
! --- COMPLEMENT DU JACOBIEN
! === =================================================================
        call lcicma(dr1dy3, 6, 1, ndt, 1,&
                    1, 1, drdy, nr, nr,&
                    1, 2*ndt+1)
        call lcicma(dr2dy3, 6, 1, ndt, 1,&
                    1, 1, drdy, nr, nr,&
                    ndt+1, 2*ndt+1)
        call lcicma(dr3dy1, 1, 6, 1, ndt,&
                    1, 1, drdy, nr, nr,&
                    nr, 1)
        call lcicma(dr3dy2, 1, 6, 1, ndt,&
                    1, 1, drdy, nr, nr,&
                    nr, ndt+1)
        call lcicma(dr3dy3, 1, 1, 1, 1,&
                    1, 1, drdy, nr, nr,&
                    nr, nr)
!
    endif
!
end subroutine

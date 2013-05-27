subroutine burres(typmod, nmat, materd, materf, timed,&
                  timef, nvi, vin, yd, yf,&
                  deps, dy, nr, r)
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
! person_in_charge: alexandre.foucault at edf.fr
!=====================================================================
!    BETON_BURGER_FP : CALCUL RESIDUS DU SYSTEME NL A RESOUDRE = R(DY)
!                    CF. R7.01.34
!                    DY =  DSIG (DBETA PAR SYSTEME)
!                    Y  =  SIG   (BETA  PAR SYSTEME)
!                    R  = ( R1  R2  (R3) )
!                    ATTENTION IL FAUT CALCULER -R
!
!     IN  TYPMOD :  TYPE DE MODELISATION
!         NMAT   :  DIMENSION MATER
!         MATERD :  COEFFICIENTS MATERIAU A T
!         MATERF :  COEFFICIENTS MATERIAU A T+DT
!         TIMED  :  ISTANT PRECEDENT
!         TIMEF  :  INSTANT ACTUEL
!         NVI    :  NOMBRE DE VARIABLES INTERNES
!         VIN    :  VECTEUR VARIABLES INTERNES
!         YD     :  VARIABLES A T       = ( SIGD BETAD )
!         YF     :  VARIABLES A T + DT  = ( SIGF BETAF )
!         DEPS   :  INCREMENT DE DEFORMATION
!         DY     :  SOLUTION  =  ( DSIG DBETA )
!         NR     :  DIMENSION VECTEUR INCONNUES
!     OUT R      :  RESIDU DU SYSTEME NL A T + DT
!=====================================================================
    implicit none
!     ----------------------------------------------------------------
    include 'asterc/r8prem.h'
    include 'asterfort/burafd.h'
    include 'asterfort/burafr.h'
    include 'asterfort/lcdive.h'
    include 'asterfort/lcinma.h'
    include 'asterfort/lcopli.h'
    include 'asterfort/lcprmv.h'
    include 'asterfort/lcprsc.h'
    include 'asterfort/lcprsm.h'
    include 'asterfort/lcprsv.h'
    include 'asterfort/lcsove.h'
    include 'asterfort/mgauss.h'
    common /tdim/   ndt ,ndi
!     ----------------------------------------------------------------
    integer :: i, ndt, ndi, nmat, nvi, nr, iret
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2), timed, timef, dt
    real(kind=8) :: vin(nvi), yd(nr), yf(nr), dy(nr), r(nr)
    real(kind=8) :: deps(6), epsem(6), epsef(6), hookm(6, 6)
    real(kind=8) :: hook(6, 6), det, invela(6, 6), depst(6), epsep(6)
    real(kind=8) :: afr(6), bfr(6, 6), cfr(6, 6), ident(6, 6)
    real(kind=8) :: afd(6), bfd(6, 6), cfd(6, 6)
    real(kind=8) :: bfryd(6), cfryf(6), temp(6), depsfr(6)
    real(kind=8) :: bfdyd(6), cfdyf(6), depsfd(6)
    real(kind=8) :: hini, hfin, kappa, etas, etad, etai0(6, 6)
    real(kind=8) :: psig(6), epsfid(6), epsfif(6), depsfi(6)
    real(kind=8) :: difexp, nfid, nfif, ndfi, ketai0(6, 6), normal(6)
    real(kind=8) :: visco(6), damp
    real(kind=8) :: maxi, mini
    character(len=8) :: typmod
!
! === =================================================================
! --- INITIALISATION DES VARIABLES
! === =================================================================
    call lcinma(0.d0, invela)
! === =================================================================
! --- EQUATIONS D'ETAT MECANIQUE : DSIG = E*DEPSE (6 EQUATIONS)
! === =================================================================
! === =================================================================
! --- CONSTRUCTION TENSEUR RIGIDITE ELASTIQUE A T+DT
! === =================================================================
    call lcopli('ISOTROPE', '3D      ', materf, hook)
! === =================================================================
! --- INVERSION TENSEUR RIGIDITE ELASTIQUE A T+DT
! === =================================================================
    do 1 i = 1, ndt
        invela(i,i) = 1.d0
 1  end do
    call mgauss('NFVP', hook, invela, 6, ndt,&
                6, det, iret)
! === =================================================================
! --- CONSTRUCTION EPSEP = (E)^-1 * SIGF
! === =================================================================
    call lcprmv(invela, yf, epsep)
! === =================================================================
! --- CONSTRUCTION TENSEUR RIGIDITE ELASTIQUE A T
! === =================================================================
    call lcopli('ISOTROPE', '3D      ', materd, hookm)
! === =================================================================
! --- INVERSION TENSEUR RIGIDITE ELASTIQUE A T
! === =================================================================
    call lcinma(0.d0, ident)
    do 2 i = 1, ndt
        ident(i,i) = 1.d0
 2  end do
    call mgauss('NFVP', hookm, ident, 6, ndt,&
                6, det, iret)
! === =================================================================
! --- CONSTRUCTION EPSEM = (E)^-1 * SIGD
! === =================================================================
    call lcprmv(ident, yd, epsem)
! === =================================================================
! --- CONSTRUCTION DEPST = EPSEP - EPSEM
! === =================================================================
    call lcdive(epsep, epsem, depst)
! === =================================================================
! --- CONSTRUCTION DES DEFORMATIONS FLUAGE PROPRE REVERSIBLES
! === =================================================================
    call burafr(vin, nvi, materd, materf, nmat,&
                timed, timef, afr, bfr, cfr)
    call lcprmv(bfr, yd, bfryd)
    call lcprmv(cfr, yf, cfryf)
    call lcsove(bfryd, cfryf, temp)
    call lcsove(temp, afr, depsfr)
! === =================================================================
! --- CONSTRUCTION DES DEFORMATIONS FLUAGE DESSICATION
! === =================================================================
    call burafd(materd, materf, nmat, afd, bfd,&
                cfd)
    call lcprmv(bfd, yd, bfdyd)
    call lcprmv(cfd, yf, cfdyf)
    call lcsove(bfdyd, cfdyf, depsfd)
! === =================================================================
! --- RECUPERATION DES DEFORMATIONS IRREVERSIBLES A T ET T+DT
! === =================================================================
    do 4 i = 1, ndt
        epsfid(i) = yd(ndt+i)
        epsfif(i) = yf(ndt+i)
        depsfi(i) = dy(ndt+i)
 4  end do
! === =================================================================
! --- EVALUATION DE -R(YD+DY) SUR LES (NDT) 1ERES COMPOSANTES
! === =================================================================
    do 5 i = 1, ndt
        r(i) = -deps(i)+depst(i)+depsfr(i)+depsfd(i)+depsfi(i)
 5  end do
!
!**********************************************************************
!**********************************************************************
! === =================================================================
! --- EQUATIONS LIEES AUX DEFORMATIONS FLUAGE IRREVERSIBLE
! === =================================================================
! === =================================================================
! --- RECUPERATION TERME HYDRATATION A T ET T+DT
! === =================================================================
    hini = materd(6,1)
    hfin = materf(6,1)
! === =================================================================
! --- RECUPERATION PROPRIETE VISQUEUSE IRREVERSIBLE
! === =================================================================
    etas = materd(3,2)
    etad = materd(6,2)
    kappa = materd(7,2)
! === =================================================================
! --- CALCUL DE (SIGF*HINI + SIGD*HFIN)*(DT/2)
! === =================================================================
    dt = timef - timed
    do 6 i = 1, ndt
        psig(i) = dt/2.d0*(hini*yf(i)+hfin*yd(i))
 6  end do
! === =================================================================
! --- CALCUL DE LA NORME DES DEFORMATIONS IRREVERSIBLES A T -> NFID
! === =================================================================
    call lcprsc(epsfid, epsfid, nfid)
    nfid = sqrt(nfid)
    if(nfid.lt.vin(21))nfid = vin(21)
! === =================================================================
! --- CALCUL DE LA NORME DES DEFORMATIONS IRREVERSIBLES A T+DT -> NFIF
! === =================================================================
    call lcprsc(epsfif, epsfif, nfif)
    nfif = sqrt(nfif)
    if(nfif.lt.vin(21))nfif = vin(21)
! === =================================================================
! --- CALCUL DE (EXP(NFIF/KAPPA)-EXP(NFID/KAPPA))
! === =================================================================
    if (((nfif/kappa).lt.1.d2) .and. ((nfid/kappa).lt.1.d2)) then
        difexp = exp(nfif/kappa)-exp(nfid/kappa)
    else
        difexp = 0.d0
    endif
! === =================================================================
! --- CONSTRUCTION TENSEUR ORDRE 4 DE VISCOSITE KAPPA*(ETA_I^0)*DIFEXP
! === =================================================================
    call lcinma(0.d0, etai0)
    do 7 i = 1, ndi
        etai0(i,i) = (etas+2.d0*etad)/3.d0
        etai0(i+ndi,i+ndi) = etad
 7  end do
    etai0(1,2) = (etas-etad)/3.d0
    etai0(2,1) = etai0(1,2)
    etai0(3,1) = etai0(1,2)
    etai0(1,3) = etai0(1,2)
    etai0(2,3) = etai0(1,2)
    etai0(3,2) = etai0(1,2)
    call lcprsm(kappa*difexp, etai0, ketai0)
! === =================================================================
! --- CALCUL DE LA NORME DES INCREMENTS DE DEFORMATIONS IRREVERSIBLES
! === =================================================================
    call lcprsc(depsfi, depsfi, ndfi)
! --- ON S'ASSURE QUE TOUTE DIVISION PAR ZERO EST IMPOSSIBLE
    if (ndfi .ne. 0.d0) then
        ndfi = 1.d0/sqrt(ndfi)
    else
        ndfi = 0.d0
    endif
! === =================================================================
! --- CALCUL DE LA DIRECTION DES INCREMENTS DEFORMATION IRREVERSIBLE
! === =================================================================
    call lcprsv(ndfi, depsfi, normal)
! === =================================================================
! --- CALCUL DE KETAI0(ORDRE4)*NORMAL(ORDRE2)=VISCO
! === =================================================================
    call lcprmv(ketai0, normal, visco)
! === =================================================================
! --- EVALUATION DE -R(YD+DY) SUR LES (NDT) DERNIERES COMPOSANTES
! === =================================================================
    do 8 i = ndt+1, 2*ndt
        r(i) = psig(i-ndt)-visco(i-ndt)
 8  end do
! === =================================================================
! --- MISE A L'ECHELLE DU SYSTEME D'EQUATIONS NL
! === =================================================================
    damp = materf(6,2)/dt
    do 9 i = ndt+1, 2*ndt
        r(i) = r(i)/damp
 9  end do
!**********************************************************************
!**********************************************************************
! === =================================================================
! --- MODELISATION C_PLAN: EQ(NR) SIG3 = 0
! === =================================================================
    if (typmod(1:6) .eq. 'C_PLAN') then
        call lcprmv(invela, yd, epsem)
        do 10 i = 1, ndt
            epsef(i) = epsem(i)+deps(i)-depsfd(i)-depsfr(i)-dy(ndt+i)
10      continue
        r(nr) = -hook(3,3)*epsef(3)-hook(3,1)*epsef(1) -hook(3,2)* epsef(2)-hook(3,4)*epsef(4)
    endif
! === =================================================================
! --- TRAITEMENT DU BRUIT NUMERIQUE PAR R8PREM
! === =================================================================
    maxi = 0.d0
    do 11 i = 1, nr
        if(abs(r(i)).gt.maxi)maxi = abs(r(i))
11  end do
    mini = r8prem() * maxi
    do 12 i = 1, nr
        if(abs(r(i)).lt.mini)r(i) = 0.d0
12  end do
!
end subroutine

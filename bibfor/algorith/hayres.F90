subroutine hayres(mod, nmat, materd, materf, timed,&
                  timef, yd, yf, deps, dy,&
                  res, crit, iret)
    implicit none
!       ================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
!       ----------------------------------------------------------------
!     HAYHURST :
!            CALCUL DES TERMES DU SYSTEME NL A RESOUDRE = RES(DY)
!                   DY  = ( DEPSILON_EL DP DH1 DH2 DPHI DD )
!       IN  MOD    :  TYPE DE MODELISATION
!           NMAT   :  DIMENSION MATER
!           MATERD :  COEFFICIENTS MATERIAU A T
!           MATERF :  COEFFICIENTS MATERIAU A T+DT
!           YD     :  VARIABLES A T      = ( SIGD  X1D X2D PD RD QD(..))
!           YF     :  VARIABLES A T + DT = ( SIGF  X1F X2F PF RF QF(..))
!           DY     :  SOLUTION ESSAI     = ( DSIG  DX1 DX2 DP DR DQ(..))
!           EPSD   :  DEFORMATION A T
!           DEPS   :  INCREMENT DE DEFORMATION
!       OUT RES    :  SYSTEME NL A T + DT
!       ----------------------------------------------------------------
    include 'asterc/r8miem.h'
    include 'asterc/r8prem.h'
    include 'asterfort/fgequi.h'
    include 'asterfort/lcdive.h'
    include 'asterfort/lceqvn.h'
    include 'asterfort/lcopli.h'
    include 'asterfort/lcprmv.h'
    include 'asterfort/lcprsv.h'
    include 'blas/dscal.h'
    character(len=8) :: mod
    integer :: iret, itens, ndi, nmat, nvi, ndt, ndim
    real(kind=8) :: hookf(6, 6), res(10), crit(*), theta, alphad
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2), timed, timef, deps(6), dt
    real(kind=8) :: dtot
    real(kind=8) :: yd(*), yf(*), dy(*), sigf(6), ecrou(2), gh, dmg, dmgmi
    real(kind=8) :: epsed(6)
    real(kind=8) :: depsp(6), devcum, decrou(2), ddmg, epsef(6), depsel(6), m13
    real(kind=8) :: w(6)
    real(kind=8) :: ze, td, sinn, grj0, gh1, gh2, equi(17), rmin, sequid
    real(kind=8) :: eps0, pk, ph1, ph2, delta1, delta2, h1st, h2st, pkc, sig0
    real(kind=8) :: biga
    real(kind=8) :: trsig, grj2v, grj1, epsi, terme1, shmax, sequi, dddmg, dh1
    real(kind=8) :: dh2, dp
!     ----------------------------------------------------------------
    parameter(ze=0.0d0)
    parameter(td=1.5d0)
!
    common /tdim/   ndt,    ndi
!-----------------------------------------------------------------------
    theta=crit(4)
    call lceqvn(1, yd(8), gh1)
    call lceqvn(1, yd(9), gh2)
    call lceqvn(1, dy(7), dp)
    call lceqvn(1, dy(8), dh1)
    call lceqvn(1, dy(9), dh2)
    call lceqvn(1, dy(10), dddmg)
    do 11 itens = 1, ndt
        epsef(itens)=yd(itens)+theta*dy(itens)
11  end do
    dt=timef-timed
    if (ndt .eq. 6) then
        ndim=3
    else if (ndt.eq.4) then
        ndim=2
        sigf(5)=0.d0
        sigf(6)=0.d0
        depsp(5)=0.d0
        depsp(6)=0.d0
    endif
    iret=0
    rmin=r8miem()
    shmax=50.d0
    eps0 = materf(1,2)
    pk = materf(2,2)
    ph1 = materf(3,2)
    ph2 = materf(4,2)
    delta1 = materf(5,2)
    delta2 = materf(6,2)
    h1st = materf(7,2)
    h2st = materf(8,2)
    biga = materf(9,2)
    sig0 = materf(10,2)
    pkc = materf(11,2)
    alphad = materf(12,2)
    sequid = materf(13,2)
    epsi=r8prem()*pk
    gh=gh1+theta*dh1+gh2+theta*dh2
    m13=-1.d0/3.d0
    dmgmi=1.d0-(1.d0+pkc*timef)**m13
    dmg=yd(10)+theta*dddmg
!
!----------------------------------------------------------------
    dtot=(1.d0-dmg)
    call lcopli('ISOTROPE', mod, materf(1, 1), hookf)
    call lcprmv(hookf, epsef, sigf)
    call lcprsv(dtot, sigf, sigf)
!
!------------CALCUL DES INVARIANTS DE CONTRAINTE  -------
!     attention FGEQUI ne prend pas en compte les SQRT(2)
    call dscal(3, 1.d0/sqrt(2.d0), sigf(4), 1)
    call fgequi(sigf, 'SIGM_DIR', ndim, equi)
!     on retablit le tenseur
    call dscal(3, sqrt(2.d0), sigf(4), 1)
    trsig=equi(16)
    grj0=max(equi(3),equi(4))
    grj0=max(grj0,equi(5))
    grj1= trsig
    grj2v=equi(1)
    if (sequid .eq. 0.d0) then
        sequi=grj0
    else
        sequi=grj1
    endif
!------------ CALCUL DU TENSEUR DEPSPATORIQUE DES CONTRAINTES ---
    do 10 itens = 1, ndt
        if (itens .le. 3) sigf(itens)=sigf(itens)-grj1/3.d0
10  end do
!
!----- EQUATION DONNANT LA DERIVEE DE LA DEF VISCO PLAST
!----- CUMULEE
!
    terme1=(grj2v*(1-gh))/(pk*(1-dmgmi)*dtot)
    if (grj2v .le. epsi) then
        devcum=ze
    else if (abs(terme1).lt.shmax) then
        devcum=eps0*(sinh(terme1))*dt
    else
        iret=1
        goto 9999
    endif
!
!----- EQUATION DONNANT LA DERIVEE DE GH
!
    if (grj2v .le. epsi) then
!       DIVISION PAR ZERO EVITEE
        decrou(1)=ze
        decrou(2)=ze
    else
        if (gh1 .le. (h1st-rmin)) then
            decrou(1)=(ph1/grj2v)*(h1st-(delta1*(gh1+theta*dh1)))*dp
            decrou(2)=(ph2/grj2v)*(h2st-(delta2*(gh2+theta*dh2)))*dp
        else
            iret=1
            goto 9999
        endif
    endif
!
!----- EQUATION DONNANT LA DERIVEE DE L ENDOMMAGEMENT
!
    if (sequi .ge. ze) then
        sinn=alphad*sequi+((1.d0-alphad)*grj2v)
    else
        sinn=(1.d0-alphad)*grj2v
    endif
    if ((sinn/sig0) .lt. shmax) then
        ddmg=biga*sinh(sinn/sig0)*dt
    else
        iret=1
        goto 9999
    endif
!
!------ EQUATION DONNANT LA DERIVEE DE LA DEF VISCO PLAST
!
    if (grj2v .le. epsi) then
        do 33 itens = 1, ndt
            depsp(itens)=ze
33      continue
    else
        do 12 itens = 1, ndt
            depsp(itens)=td*dp*sigf(itens)/grj2v
12      continue
    endif
    call lcdive(deps, depsp, depsel)
    call lcdive(depsel, dy, res(1))
    res(ndt+1) = devcum-dp
    res(ndt+2) = decrou(1)-dh1
    res(ndt+3) = decrou(2)-dh2
    res(ndt+4) = ddmg-dddmg
9999  continue
end subroutine

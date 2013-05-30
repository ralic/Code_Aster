subroutine lkijac(mod, nmat, materf, timed, timef,&
                  yf, deps, nr, nvi, vind,&
                  vinf, yd, dy, drdy, iret)
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
! person_in_charge: alexandre.foucault at edf.fr
! TOLE CRS_1404 CRP_20
    implicit   none
!     --------------------------------------------------------------
!     CALCUL DU JACOBIEN DE LETK = DRDY(DY)
!     IN  MOD    :  TYPE DE MODELISATION
!         NMAT   :  DIMENSION MATER
!         MATERF :  COEFFICIENTS MATERIAU A T+DT
!         YF     :  VARIABLES A T + DT =  ( SIGF DLAMBDA XI_P XI_VP)
!         DEPS   :  INCREMENT DE DEFORMATION
!         TIMED  :  INSTANT  T
!         TIMEF  :  INSTANT  T+DT
!         NR     :  DIMENSION DECLAREE DRDY
!         NVI    :  NOMBRE DE VARIABLES INTERNES
!         VIND   :  VARIABLE INTERNES A T
!         VINF   :  VARIABLE INTERNES A T+DT
!         YD     :  VARIABLES A T  = ( SIGD  0 XI_P XI_VP) A T
!         DY     :  SOLUTION = ( DSIG  DLAMBDA  DXI_P DXI_VP )
!     OUT DRDY   :  JACOBIEN DU SYSTEME NON LINEAIRE
!         IRET   :  CODE RETOUR
!     --------------------------------------------------------------
    include 'asterc/r8prem.h'
    include 'asterfort/lcdevi.h'
    include 'asterfort/lcdima.h'
    include 'asterfort/lceqvn.h'
    include 'asterfort/lcinma.h'
    include 'asterfort/lcinve.h'
    include 'asterfort/lcprmm.h'
    include 'asterfort/lcprmv.h'
    include 'asterfort/lcprsc.h'
    include 'asterfort/lcprsm.h'
    include 'asterfort/lcprsv.h'
    include 'asterfort/lcprte.h'
    include 'asterfort/lcsove.h'
    include 'asterfort/lkbpri.h'
    include 'asterfort/lkcalg.h'
    include 'asterfort/lkcaln.h'
    include 'asterfort/lkcrip.h'
    include 'asterfort/lkcriv.h'
    include 'asterfort/lkdepp.h'
    include 'asterfort/lkdfds.h'
    include 'asterfort/lkdfdx.h'
    include 'asterfort/lkdgde.h'
    include 'asterfort/lkdgds.h'
    include 'asterfort/lkdhds.h'
    include 'asterfort/lkdndx.h'
    include 'asterfort/lkds2h.h'
    include 'asterfort/lkelas.h'
    include 'asterfort/lkfsxi.h'
    include 'asterfort/lkvacp.h'
    include 'asterfort/lkvacv.h'
    include 'asterfort/lkvarp.h'
    include 'asterfort/lkvarv.h'
    integer :: nr, nmat, iret, nvi
    real(kind=8) :: deps(6), drdy(nr, nr), yf(nr), dy(nr), yd(nr)
    real(kind=8) :: materf(nmat, 2)
    real(kind=8) :: timed, timef, vind(nvi), vinf(nvi)
    character(len=8) :: mod
!
    integer :: i, j, varv, valv, valp, retcom, ndt, ndi
    real(kind=8) :: sigft(6), depst(6), zero
    real(kind=8) :: devgii, vint(nvi), dt, devsig(6), i1
    real(kind=8) :: xivmax, xippic, dsdenl(6, 6), kk, mu
    real(kind=8) :: ucriv, seuilv, depsv(6), dgamv
    real(kind=8) :: seuilp, ucrip, seuivm, dhds(6), ds2hds(6)
    real(kind=8) :: paraep(3), varpl(4), dfdsp(6), bprimp
    real(kind=8) :: vecnp(6), gp(6), vetemp(6), derpar(3), dfdxip
    real(kind=8) :: un, deux, trois, depse(6), hook(6, 6), mue, ke
    real(kind=8) :: dsige(6), vident(6), dhokds(6, 6), patm, nelas
    real(kind=8) :: paravi(3), varavi(4), dfvdsi(6)
    real(kind=8) :: dgpds(6, 6), dgvds(6, 6)
    real(kind=8) :: dldgds(6, 6), dlambd, hnldgp(6, 6), dsgvds(6, 6)
    real(kind=8) :: hnldgv(6, 6), bprimv, vecnv(6), ucrim, devgiv, gv(6)
    real(kind=8) :: hnlgv(6), hnldfg(6, 6), phiv, av, nv, dphiv
    real(kind=8) :: dphvds(6), dr1dy3(6), dgpdxi(6), dfsdxp(6)
    real(kind=8) :: term1, term2, term3
    real(kind=8) :: dndxip(6), dfsdxv(6), dpadxp(3)
    real(kind=8) :: dndxiv(6), dfdxiv, dpadxv(3), dphidx
    real(kind=8) :: dphdxg(6), dgvdxi(6), phdgdx(6)
    real(kind=8) :: dr1dy4(6), dgipds(6), dgivds(6), dgipdx, dgivdx
    real(kind=8) :: mident(6, 6), kron(6), kron2(6, 6), unstro
    real(kind=8) :: dsdsig(6, 6), dgtvds(6, 6), dgtpds(6, 6), kron3(6, 6)
    real(kind=8) :: devgp(6), devgv(6), dgtpdx(6), dgtvdx(6), dxiv
    logical :: plas
    parameter       (zero  =  0.d0 )
    parameter       (un    =  1.d0 )
    parameter       (deux  =  2.d0 )
    parameter       (trois =  3.d0 )
!     --------------------------------------------------------------
    common /tdim/   ndt  , ndi
!     --------------------------------------------------------------
! ------------------------------------------------------------------
! --- PASSAGE EN CONVENTION MECANIQUE DES SOLS
! ------------------------------------------------------------------
    do 10 i = 1, ndt
        sigft(i) = -yf(i)
        depst(i) = -deps(i)
10  end do
! ------------------------------------------------------------------
! --- VARIABLES LOCALES TEMPORAIRES
! ------------------------------------------------------------------
    call lcinma(zero, mident)
    do 20 i = 1, ndt
        mident(i,i) = un
20  end do
!
    varv = 0
    devgii = zero
    dlambd = yf(ndt+1)
! --- VECTEUR VARIABLES INTERNES TEMPORAIRES
    call lceqvn(nvi, vind, vint)
    if (yf(ndt+2) .ge. vind(1)) then
        vint(1) = yf(ndt+2)
    else
        vint(1) = vind(1)
    endif
    if (yf(ndt+3) .ge. vind(3)) then
        vint(3) = yf(ndt+3)
    else
        vint(3) = vind(3)
    endif
! --- INCREMENT DE TEMPS
    dt = timef - timed
! --- CONSTRUCTION TENSEUR DEVIATOIRE DES CONTRAINTES ET 1ER INVARIA
    call lcdevi(sigft, devsig)
    i1 = sigft(1)+sigft(2)+sigft(3)
! --- DONNEES MATERIAU : VALEUR MAX DE XIV; XI_PIC
    xivmax = materf(20,2)
    xippic = materf(18,2)
! --- CONSTRUCTION TENSEUR ELASTIQUE NON LINEAIRE DSDENL
    call lkelas(ndi, ndt, mod, nmat, materf,&
                depst, sigft, dsdenl, kk, mu)
! ------------------------------------------------------------------
! --- A) - BUT : CALCUL DE LA DEFORMATION VISQUEUSE -DEPSV- ET DU
! ---      PARAMETRE D ECROUISSAGE VISQUEUX -DGAMV-
! ------------------------------------------------------------------
! --- A-1) INDICATEUR SUR ANGLE DE DILATANCE VISQUEUX PSI -> VAL = 0
    valv = 0
! --- A-2) VARIABLE D'ECROUISSAGE VISQUEUSE VINTR = YF(NDT+3)
! --- A-3) CALCUL SEUIL VISQUEUX PAR RAPPORT A YF(1:6)=SIGF ->SEUILV
! --- A-3-1)  XIT   = YF(NDT+3)
    seuilv = zero
    call lkcriv(vint(3), i1, devsig, vint, nmat,&
                materf, ucriv, seuilv)
    if (seuilv .ge. zero) then
        call lkdgde(valv, vint(3), dt, seuilv, ucriv,&
                    i1, devsig, vint, nmat, materf,&
                    depsv, dgamv, retcom)
    else
        dgamv = zero
        do 30 i = 1, ndt
            depsv(i) = zero
30      continue
        seuilv = zero
        ucriv = zero
    endif
! ------------------------------------------------------------------
! --- B) - BUT : CALCUL DE LA DEFORMATION PLASTIQUE -DEPSP- ET DU
! ---       PARAMETRE D ECROUISSAGE PLASTIQUE -DGAMP-
! ------------------------------------------------------------------
    call lkdhds(nmat, materf, i1, devsig, dhds,&
                retcom)
    call lkds2h(nmat, materf, i1, devsig, dhds,&
                ds2hds, retcom)
! --- B-1) CALCUL FONCTION SEUIL PLASTIQUE EN YF
    seuilp = zero
    call lkcrip(i1, devsig, vint, nmat, materf,&
                ucrip, seuilp)
! --- B-1-B-2) INDICATEUR CONTRACTANCE OU DILATANCE -> VARV = 0 OU 1
! --- B-1-B-2)-1) CALCUL POSITION YF PAR RAPPORT SEUIL VISQUEUX MAX
    seuivm = zero
    call lkcriv(xivmax, i1, devsig, vint, nmat,&
                materf, ucrim, seuivm)
! --- B-1-B-2)-2) TEST SUR SEUIL >0 OU <0 POUR DEFINIR VARV
    if (seuivm .le. zero) then
        varv = 0
    else
        varv = 1
    endif
! --- B-2)SI SEUILP >= 0 ALORS PLASTICITE A PRENDRE EN COMPTE
    if ((seuilp.ge.zero) .or. (vinf(7).gt.zero)) then
! --- B-2-B-1) INDICATEUR ANGLE DE DILATANCE PLASTIQUE PSI -> 0 OU 1
        if (yf(ndt+2) .le. xippic) then
            valp = 0
        else
            valp = 1
        endif
! --- B-2-B-3) CALCUL DE DF/DSIG
        call lkvarp(vint, nmat, materf, paraep)
        call lkvacp(nmat, materf, paraep, varpl)
        call lkdepp(vint, nmat, materf, paraep, derpar)
        call lkdfds(nmat, materf, devsig, paraep, varpl,&
                    ds2hds, ucrip, dfdsp)
! --- B-2-B-4) CALCUL DE G_EP
        bprimp = lkbpri (valp,vint,nmat,materf,paraep,i1,devsig)
        call lcinve(zero, vecnp)
        call lkcaln(devsig, bprimp, vecnp, retcom)
        call lkcalg(dfdsp, vecnp, gp, devgii)
! --- CALCUL DEFORMATION ELASTIQUE
        do 40 i = 1, ndt
            depse(i) = depst(i)-yf(ndt+1)*gp(i)-depsv(i)
40      continue
! --- CALCUL DE DGP/DSIGMA
        call lkdgds(nmat, materf, paraep, varpl, devsig,&
                    i1, valp, ds2hds, vecnp, dfdsp,&
                    bprimp, nvi, vint, dhds, dgpds,&
                    iret)
! --- PRODUIT MATRICIEL HOOK_NL*D_LAMBDA*DGP/DSIGMA
        call lcprsm(dlambd, dgpds, dldgds)
        call lcprmm(dsdenl, dldgds, hnldgp)
! --- CALCUL DE D(DFPDSIG)/DXI
        plas = .true.
        call lkfsxi(nmat, materf, i1, devsig, ds2hds,&
                    plas, vint(1), paraep, varpl, dfsdxp,&
                    dpadxp)
! --- CALCUL DE DN/DXI
        call lkdndx(nmat, materf, i1, devsig, bprimp,&
                    valp, paraep, vint(1), derpar, dndxip)
! --- PAS DE PLASTICITE A GERER
    else
        do 50 i = 1, ndt
            depse(i) = depst(i)-depsv(i)
50      continue
        call lcinma(zero, hnldgp)
        call lcinma(zero, dgpds)
        call lcinve(zero, dfdsp)
        call lcinve(zero, gp)
        call lcinve(zero, vecnp)
        call lcinve(zero, dfsdxp)
        call lcinve(zero, dndxip)
        devgii = zero
    endif
! ##################################################################
! --- CALCUL DE DR1/DY
! ##################################################################
! ------------------------------------------------------------------
! --- I.1 CALCUL DE DR1DY1 -> Y1 = SIGMA
! ------------------------------------------------------------------
! --- CONSTRUCTION TENSEUR ELASTIQUE LINEAIRE
    mue = materf(4,1)
    ke = materf(5,1)
    call lcinma(zero, hook)
    do 110 i = 1, ndi
        do 120 j = 1, ndi
            hook(i,j) = ke - deux*mue/trois
120      continue
110  end do
!
    do 130 i = 1, ndt
        hook(i,i) = hook(i,i) + deux*mue
130  end do
! --- INCREMENT CONTRAINTE "ELASTIQUE"
    call lcprmv(hook, depse, dsige)
! --- PRODUIT TENSORIEL DSIGE X VECTEUR(IDENTITE) (=1 1 1 0 0 0)
    patm = materf(1,2)
    nelas = materf(2,2)
    call lcinve(zero, vident)
    do 150 i = 1, ndi
        vident(i) = nelas/trois/patm*(i1/(trois*patm))**(nelas-un)
150  end do
    call lcprte(dsige, vident, dhokds)
! --- CALCUL DE DFV/DSIGMA
    call lkvarv(vint(3), nmat, materf, paravi)
    call lkvacv(nmat, materf, paravi, varavi)
    bprimv = lkbpri (valv,vint,nmat,materf,paravi,i1,devsig)
    call lkcaln(devsig, bprimv, vecnv, retcom)
    call lkdfds(nmat, materf, devsig, paravi, varavi,&
                ds2hds, ucriv, dfvdsi)
! --- CALCUL DE G_VISQUEUX
    call lkcalg(dfvdsi, vecnv, gv, devgiv)
! --- CALCUL DE DGV/DSIGMA
    call lkdgds(nmat, materf, paravi, varavi, devsig,&
                i1, valv, ds2hds, vecnv, dfvdsi,&
                bprimv, nvi, vint, dhds, dgvds,&
                iret)
! --- PRODUIT MATRICIEL HOOK_NL*PHIV*DGV/DSIGMA
    av = materf(21,2)
    nv = materf(22,2)
    phiv = av * (seuilv/patm)**nv
    call lcprsm(phiv, dgvds, dsgvds)
    call lcprmm(dsdenl, dsgvds, hnldgv)
! --- PRODUIT MATRICIEL HOOK_NL*DPHIV/DSIG*GV
    dphiv = av*nv/patm*(seuilv/patm)**(nv-un)
    call lcprsv(dphiv, dfvdsi, dphvds)
    call lcprmv(dsdenl, gv, hnlgv)
    call lcprte(hnlgv, dphvds, hnldfg)
! --- ASSEMBLAGE FINAL
    do 160 i = 1, ndt
        do 170 j = 1, ndt
            drdy(i,j) = -(mident(i,j)-dhokds(i,j)+hnldgp(i,j) +hnldgv( i,j)*dt+hnldfg(i,j)*dt)/mu
170      continue
160  end do
! ------------------------------------------------------------------
! --- I.2 CALCUL DE DR1DY2 -> Y2 = DLAMBDA
! ------------------------------------------------------------------
    if (vinf(7) .eq. zero) then
        do 175 i = 1, ndt
            drdy(i,ndt+1) = zero
175      continue
    else
        call lcprmv(dsdenl, gp, vetemp)
        do 180 i = 1, ndt
            drdy(i,ndt+1) = vetemp(i)/mu
180      continue
    endif
! ------------------------------------------------------------------
! --- I.3 CALCUL DE DR1DY3 -> Y3 = XIP
! ------------------------------------------------------------------
! --- ASSEMBLAGE DE DGPDXI =
! D(DFPDSIG)/DXI-D(DFPDSIG)/DXI.N*N-DFPDSIG.DNDXI*N-DFPDSIG.N*DNDXI
    call lcprsc(dfsdxp, vecnp, term1)
    call lcprsc(dfdsp, dndxip, term2)
    call lcprsc(dfdsp, vecnp, term3)
    do 190 i = 1, ndt
        dgpdxi(i) = dfsdxp(i)-term1*vecnp(i)-term2*vecnp(i) -term3* dndxip(i)
190  end do
! --- ASSEMBLAGE FINAL --- DR1DY3 = DSDENL*DLAMBD*DGPDXI
    call lcprmv(dsdenl, dgpdxi, dr1dy3)
    call lcprsv(dlambd, dr1dy3, dr1dy4)
    do 200 i = 1, ndt
        drdy(i,ndt+2) = dr1dy4(i)/mu
200  end do
! ------------------------------------------------------------------
! --- I.4 CALCUL DE DR1DY4 -> Y4 = XIVP
! ------------------------------------------------------------------
    dxiv = min(dgamv,xivmax-yd(ndt+3))
    if (abs(dxiv-dgamv) .lt. r8prem()) then
! --- CALCUL DE D(DFVDSIG)/DXIV
        plas = .false.
        call lkfsxi(nmat, materf, i1, devsig, ds2hds,&
                    plas, vint(3), paravi, varavi, dfsdxv,&
                    dpadxv)
! --- CALCUL DE DN/DXI
        call lkdndx(nmat, materf, i1, devsig, bprimv,&
                    valv, paravi, vint(3), dpadxv, dndxiv)
! --- ASSEMBLAGE DE DGVDXIV =
        call lcprsc(dfsdxv, vecnv, term1)
        call lcprsc(dfvdsi, dndxiv, term2)
        call lcprsc(dfvdsi, vecnv, term3)
        do 210 i = 1, ndt
            dgvdxi(i) = dfsdxv(i)-term1*vecnv(i)-term2*vecnv(i) -term3*dndxiv(i)
210      continue
! --- CALCUL DE D(PHIV)/DXIV =
        call lkdfdx(nmat, materf, ucriv, i1, devsig,&
                    paravi, varavi, dpadxv, dfdxiv)
! --- ASSEMBLAGE DE DR1DY4
        dphidx = dphiv*dfdxiv
        call lcprsv(dphidx, gv, dphdxg)
        call lcprsv(phiv, dgvdxi, phdgdx)
        call lcsove(dphdxg, phdgdx, vetemp)
        call lcprmv(dsdenl, vetemp, dr1dy4)
        do 220 i = 1, ndt
            drdy(i,ndt+3)= dr1dy4(i)/mu*dt
220      continue
    else
        do 230 i = 1, ndt
            drdy(i,ndt+3)= zero
230      continue
    endif
! ##################################################################
! --- CALCUL DE DR2/DY
! ##################################################################
! --- APPLICATION DE LA CONDITION DE KHUN-TUCKER SUR R(NDT+1)
    if (vinf(7) .eq. zero) then
! ------------------------------------------------------------------
! --- II.1 CALCUL DE DR2DY1 -> Y1 = SIGMA
! ------------------------------------------------------------------
        do 240 i = 1, ndt
            drdy(ndt+1,i) = zero
240      continue
! ------------------------------------------------------------------
! --- II.2 CALCUL DE DR2DY2 -> Y2 = DLAMBDA
! ------------------------------------------------------------------
        drdy(ndt+1,ndt+1) = un
! ------------------------------------------------------------------
! --- II.3 CALCUL DE DR2DY3 -> Y3 = XIP
! ------------------------------------------------------------------
        drdy(ndt+1,ndt+2) = zero
    else
! ------------------------------------------------------------------
! --- II.1 CALCUL DE DR2DY1 -> Y1 = SIGMA
! ------------------------------------------------------------------
        do 250 i = 1, ndt
            drdy(ndt+1,i) = -dfdsp(i)/mu
250      continue
! ------------------------------------------------------------------
! --- II.2 CALCUL DE DR2DY2 -> Y2 = DLAMBDA
! ------------------------------------------------------------------
        drdy(ndt+1,ndt+1) = zero
! ------------------------------------------------------------------
! --- II.3 CALCUL DE DR2DY3 -> Y3 = XIP
! ------------------------------------------------------------------
! --- RECUPERATION DE DF/DXIP -------------------------------------
        call lkdfdx(nmat, materf, ucrip, i1, devsig,&
                    paraep, varpl, derpar, dfdxip)
        drdy(ndt+1,ndt+2) = dfdxip/mu
    endif
! ------------------------------------------------------------------
! --- II.4 CALCUL DE DR2DY4 -> Y4 = XIVP
! ------------------------------------------------------------------
    drdy(ndt+1,ndt+3) = zero
! ##################################################################
! --- CALCUL DE DR3/DY
! ##################################################################
! ------------------------------------------------------------------
! --- III.1 CALCUL DE DR3DY1 -> Y1 = SIGMA
! ------------------------------------------------------------------
! --- CONSTRUCTION DE KRONECKER
    call lcinve(zero, kron)
    do 300 i = 1, ndi
        kron(i) = un
300  end do
! --- CONSTRUCTION DE DS/DSIGMA
    unstro = un / trois
    call lcprte(kron, kron, kron2)
    call lcprsm(unstro, kron2, kron3)
    call lcdima(mident, kron3, dsdsig)
! --- CONSTRUCTION DE DEVG
    call lcdevi(gv, devgv)
    call lcdevi(gp, devgp)
! --- CONSTRUCTION DE D(DEVGII)/DSIGMA
    call lcprmm(dsdsig, dgvds, dgtvds)
    call lcprmm(dsdsig, dgpds, dgtpds)
    call lcinve(zero, dgivds)
    call lcinve(zero, dgipds)
    if ((seuilp.ge.zero) .or. (vinf(7).gt.zero)) then
        do 310 i = 1, ndt
            do 320 j = 1, ndt
                dgivds(i) = dgivds(i)+devgv(j)/devgiv*dgtvds(j,i)
                dgipds(i) = dgipds(i)+devgp(j)/devgii*dgtpds(j,i)
320          continue
310      continue
    else
        do 330 i = 1, ndt
            do 340 j = 1, ndt
                dgivds(i) = dgivds(i)+devgv(j)/devgiv*dgtvds(j,i)
340          continue
330      continue
    endif
    if (varv .eq. 0) then
        do 350 i = 1, ndt
            drdy(ndt+2,i) = dlambd*sqrt(deux/trois)*dgipds(i)
350      continue
    else
        do 360 i = 1, ndt
            drdy(ndt+2,i) = sqrt(deux/trois)*(dlambd*dgipds(i)+ (dphvds(i)*devgiv+phiv*dgivds(i))&
                            &*dt)
360      continue
    endif
! ------------------------------------------------------------------
! --- III.2 CALCUL DE DR3DY2 -> Y2 = DLAMBDA
! ------------------------------------------------------------------
! --- APPLICATION DE LA CONDITION DE KHUN-TUCKER SUR R(NDT+1)
    if (vinf(7) .eq. zero) then
        drdy(ndt+2,ndt+1) = zero
    else
        drdy(ndt+2,ndt+1) = -devgii*sqrt(deux/trois)
    endif
! ------------------------------------------------------------------
! --- III.3 CALCUL DE DR3DY3 -> Y3 = XIP
! ------------------------------------------------------------------
    call lcprmv(dsdsig, dgpdxi, dgtpdx)
    call lcprsc(devgp, dgtpdx, dgipdx)
    if (vinf(7) .gt. zero) then
        drdy(ndt+2,ndt+2)= un - dlambd*sqrt(deux/trois) *dgipdx/&
        devgii
    else
        drdy(ndt+2,ndt+2)= un
    endif
! ------------------------------------------------------------------
! --- III.4 CALCUL DE DR3DY4 -> Y4 = XIVP
! ------------------------------------------------------------------
! --- TEST POUR SAVOIR SI ON EST EN BUTEE SUR XIVP
    call lcprmv(dsdsig, dgvdxi, dgtvdx)
    call lcprsc(devgv, dgtvdx, dgivdx)
    if (abs(dxiv-dgamv) .lt. r8prem()) then
        drdy(ndt+2,ndt+3)= -(dphidx*devgiv+phiv*dgivdx/devgiv)&
        *sqrt(deux/trois)*dt
    else
        drdy(ndt+2,ndt+3)= zero
    endif
! ##################################################################
! --- CALCUL DE DR4/DY
! ##################################################################
! --- TEST POUR SAVOIR SI ON EST EN BUTEE SUR XIVP
    if (abs(dxiv-dgamv) .lt. r8prem()) then
! ------------------------------------------------------------------
! --- IV.1 CALCUL DE DR4DY1 -> Y1 = SIGMA
! ------------------------------------------------------------------
        do 400 i = 1, ndt
            drdy(ndt+3,i) = (dphvds(i)*devgiv+phiv*dgivds(i)) *sqrt( deux/trois )*dt
400      continue
! ------------------------------------------------------------------
! --- IV.2 CALCUL DE DR4DY2 -> Y2 = DLAMBDA
! ------------------------------------------------------------------
        drdy(ndt+3,ndt+1) = zero
! ------------------------------------------------------------------
! --- IV.3 CALCUL DE DR4DY3 -> Y3 = XIP
! ------------------------------------------------------------------
        drdy(ndt+3,ndt+2) = zero
! ------------------------------------------------------------------
! --- IV.4 CALCUL DE DR4DY4 -> Y4 = XIVP
! ------------------------------------------------------------------
        drdy(ndt+3,ndt+3) = un - sqrt(deux/trois)*dt* (dphidx*devgiv+ phiv*dgivdx/devgiv)
    else
! ------------------------------------------------------------------
! --- IV.1 CALCUL DE DR4DY1 -> Y1 = SIGMA
! ------------------------------------------------------------------
        do 410 i = 1, ndt
            drdy(ndt+3,i) = zero
410      continue
! ------------------------------------------------------------------
! --- IV.2 CALCUL DE DR4DY2 -> Y2 = DLAMBDA
! ------------------------------------------------------------------
        drdy(ndt+3,ndt+1) = zero
! ------------------------------------------------------------------
! --- IV.3 CALCUL DE DR4DY3 -> Y3 = XIP
! ------------------------------------------------------------------
        drdy(ndt+3,ndt+2) = zero
! ------------------------------------------------------------------
! --- IV.4 CALCUL DE DR4DY4 -> Y4 = XIVP
! ------------------------------------------------------------------
        drdy(ndt+3,ndt+3) = un
    endif
!
end subroutine

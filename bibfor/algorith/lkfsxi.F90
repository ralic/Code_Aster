subroutine lkfsxi(nmat, materf, i1, devsig, dshds,&
                  plas, xi, para, vara, dfdsdx,&
                  dpardx)
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
    implicit   none
!     --------------------------------------------------------------
!     CALCUL DU TERME DE LETK = D(DF/DS)/DXI
!     IN  NMAT     : DIMENSION TABLE DES PARAMETRES MATERIAU
!         MATERF   : TABLE DES PARAMETRES MATERIAU
!         I1       : TRACE DU TENSEUR DES CONTRAINTES
!         DEVISG   : DEVIATEUR DU TENSEUR DES CONTRAINTES
!         DSHDS    : DERIVVE DE SII*HTHETA PAR RAPPORT A SIGMA
!         PLAS     : BOOLEEN -> PLASTI. = TRUE - VISCOS. = FALSE
!         XI       : VARIABLE D'EXROUISSAGE XI(P OU VP)
!         PARA     : CONTIENT VALEURS DE A(XI),S(XI),M(XI)
!         VARA     : CONTIENT AG(XI),BG(XI),DG(XI) ET K(XI)
!     OUT DFDSFX   : D(DF/DS)/DXI
!         DPARDX   : VECTEUR DE LONGUEUR 3 CONTENANT :
!         DAMDX    : DERIVEE DE A(XI) PAR RAPPORT A XI
!         DSDX     : DERIVEE DE M(XI) PAR RAPPORT A XI
!         DMDX     : DERIVEE DE S(XI) PAR RAPPORT A XI
!     --------------------------------------------------------------
    include 'asterfort/cos3t.h'
    include 'asterfort/lcinve.h'
    include 'asterfort/lcprsc.h'
    include 'asterfort/lkhtet.h'
    integer :: nmat
    real(kind=8) :: i1, devsig(6), dshds(6), dfdsdx(6), materf(nmat, 2)
    real(kind=8) :: para(3), xi, vara(4), dpardx(3)
    logical :: plas
!
    integer :: ndt, ndi, i
    real(kind=8) :: damdx, sigc, h0c, agx, sii, htheta, bgx, dgx, amx, un
    real(kind=8) :: vident(6), zero, terexp, dagdx, dbgdx, ddgdx
    real(kind=8) :: kx, six, dkdx, deux, trois, apic, a0, xams
    real(kind=8) :: xipic, avmax, xivmax, mpic, m0, mvmax, xie, ae, ault
    real(kind=8) :: eta, xiult, sigp1, spic, sigp2, me, sx, s0
    real(kind=8) :: mult, rcos3t, h0e, lgleps, pref, dsdx, dmdx, mx
    parameter       (zero   = 0.d0)
    parameter       (un     = 1.d0)
    parameter       (deux   = 2.d0)
    parameter       (trois  = 3.d0)
    parameter       (six    = 6.d0)
    parameter       (lgleps = 1.0d-8 )
!     --------------------------------------------------------------
    common /tdim/   ndt,ndi
!     --------------------------------------------------------------
! -----------------------------------------
! --- RECUPERATION DES PARAMETRES MATERIAU
! -----------------------------------------
    pref = materf(1,2)
!
    a0 = materf(8,2)
    avmax = un
    m0 = materf(12,2)
    mvmax = materf(19,2)
    s0 = materf(11,2)
    spic = un
    xivmax = materf(20,2)
!
    sigc = materf(3,2)
    xams = materf(6,2)
    eta = materf(7,2)
    ae = materf(9,2)
    apic = materf(10,2)
    ault = un
    me = materf(13,2)
    mpic = materf(14,2)
    mult = materf(15,2)
    xiult = materf(16,2)
    xie = materf(17,2)
    xipic = materf(18,2)
!
    sigp1 = materf(23,2)
!
    sigp2 = ((mult*(sigc)**(ae-un))/(me**ae))**(un/(ae-un))
!
    amx = para(1)
    sx = para(2)
    mx = para(3)
!
    agx = vara(1)
    bgx = vara(2)
    dgx = vara(3)
    kx = vara(4)
!
    rcos3t = cos3t (devsig, pref, lgleps)
    call lkhtet(nmat, materf, rcos3t, h0e, h0c,&
                htheta)
! --------------------------------------
! --- CONSTRUCTION VARIABLES TEMPORAIRES
! --------------------------------------
! --- VECTEUR IDENTITE
    call lcinve(zero, vident)
    do 10 i = 1, ndi
        vident(i) = un
10  end do
!
! --- NORME DU DEVIATEUR DES CONTRAINTES
    call lcprsc(devsig, devsig, sii)
    sii = sqrt(sii)
!
! --------------------------------------
! --- CALCUL DE DAMDX, DSDX ET DMDX
! --------------------------------------
    if (plas) then
! --- SEUIL DE PLASTICITE
        if ((xi.ge.zero) .and. (xi.le.xipic)) then
! --- ENTRE SEUIL D'ENDOMMAGEMENT ET SEUIL DE PIC
            damdx = (apic-a0)/log(un+un/xams)/(xi+xams*xipic)
            dmdx = (mpic-m0)/log(un+un/xams)/(xi+xams*xipic)
            dsdx = (spic-s0)/log(un+un/xams)/(xi+xams*xipic)
        else if ((xi.gt.xipic).and.(xi.le.xie)) then
! --- ENTRE SEUIL DE PIC ET SEUIL INTERMEDIAIRE
            damdx = (ae-apic)/(xie-xipic)
            dsdx = -un/(xie-xipic)
            dmdx = sigc/sigp1*(&
                   (-apic/amx**2)*(mpic*sigp1/sigc+spic) **(apic/amx)*log(mpic*sigp1/sigc+spic)*d&
                   &amdx-dsdx&
                   )
        else if ((xi.gt.xie).and.(xi.lt.xiult)) then
! --- ENTRE SEUIL INTERMEDIAIRE ET SEUIL RESIDUEL
            damdx = (ault-ae)/log(un+un/eta)/(xi+eta*xiult- (un+eta)* xie)
            dmdx = sigc/sigp2*((-ae/amx**2)*log(me*sigp2/sigc)* (me*sigp2/sigc)**(ae/amx))*damdx
            dsdx = zero
        else if (xi.ge.xiult) then
! --- SUR SEUIL RESIDUEL
            damdx = zero
            dmdx = zero
            dsdx = zero
        endif
    else
! --- SEUIL DE VISCOSITE
        damdx = (avmax-a0)/xivmax
        dmdx = (mvmax-m0)/xivmax
        dsdx = zero
    endif
!
! --------------------------------------
! --- CALCUL DE DKDX
! --------------------------------------
    dkdx = -damdx*log(deux/trois)/(deux*amx**2)*kx
!
! --------------------------------------
! --- CALCUL DE DAGDX
! --------------------------------------
    dagdx = un/(sqrt(six)*sigc*h0c)*(-dmdx*kx-mx*dkdx)
!
! --------------------------------------
! --- CALCUL DE DBGDX
! --------------------------------------
    dbgdx = dmdx*kx/trois/sigc+mx/trois/sigc*dkdx
!
! --------------------------------------
! --- CALCUL DE DDGDX
! --------------------------------------
    ddgdx = dsdx*kx+sx*dkdx
!
! --------------------------------------
! --- ASSEMBLAGE DE DFDSDX
! --------------------------------------
    terexp = agx*sii*htheta+bgx*i1+dgx
    if (terexp .gt. zero) then
        do 20 i = 1, ndt
            dfdsdx(i) = -damdx*sigc*h0c*terexp**(amx-un)* (agx*dshds( i)+bgx*vident(i))- amx*sigc&
                        &*h0c*((damdx*log(terexp)+(amx- un)/terexp *(dagdx*sii*htheta+dbgdx*i1+dd&
                        &gdx))*terexp** (amx-un))*(agx*dshds(i)+bgx*vident(i))- amx*sigc*h0c* ter&
                        &exp**(amx-un)* (dagdx*dshds(i)+dbgdx*vident(i))
20      continue
    else
        call lcinve(zero, dfdsdx)
    endif
!
    dpardx(1) = damdx
    dpardx(2) = dsdx
    dpardx(3) = dmdx
!
end subroutine

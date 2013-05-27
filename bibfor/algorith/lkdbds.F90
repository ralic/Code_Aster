subroutine lkdbds(nmat, mater, i1, devsig, nvi,&
                  vint, para, val, dbetds, dbetdi)
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
    implicit   none
!     ------------------------------------------------------------------
!     CALCUL DE DERIVEE DE N PAR RAPPORT A SIGMA
!     IN  VAL    : ENTIER PRECISANT DILATANCE EN PRE(0) OU POST-PIC(1)
!         NMAT   : DIMENSION TABLE DES PARAMETRES MATERIAU
!         MATER  : PARAMETRES MATERIAU A T+DT
!         DEVSIG : DEVIATEUR DES CONTRAINTES
!         I1     : TRACE DES CONTRAINTES
!         NVI    : NOMBRE DE VARIABLES INTERNES
!         VINT   : VARIABLES INTERNES
!         PARA   : VECTEUR CONTENANT AXI, SXI ET MXI
!
!     OUT DBETDS :  DERIVEE DE BPRIME PAR RAPPORT A DEVSIG (DEVIATEUR)
!     OUT DBETDI :  DERIVEE DE BPRIME PAR RAPPORT A I1 (TRACE SIGMA)
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
    include 'asterc/r8pi.h'
    include 'asterfort/cos3t.h'
    include 'asterfort/lcprsc.h'
    include 'asterfort/lcprsv.h'
    include 'asterfort/lkdhds.h'
    include 'asterfort/lkhtet.h'
    integer :: nmat, nvi, iret, val
    real(kind=8) :: mater(nmat, 2), devsig(6), i1, para(3)
    real(kind=8) :: vint(nvi), dbetds(6), dbetdi
!
    integer :: i, ndt, ndi
    real(kind=8) :: pi, pref, sigc, h0ext, s0, mult, xie, mvmax
    real(kind=8) :: mu0v, xi0v, mu1, alres, rcos3t, lgleps, htheta, fact1
    real(kind=8) :: zero, un, deux, trois, c, phi, xip, troisd, tiers, fact2
    real(kind=8) :: sigmin, sigmax, sii, siglim, alpha, sigtil, sinpsi
    real(kind=8) :: six, dsinds(6), dsmids(6), dsmads(6), h0c, h0e, dhds(6)
    real(kind=8) :: dsmidi, dsmadi, dsindi, xi1, dbdsin, coefh
!     ------------------------------------------------------------------
    parameter       ( lgleps =  1.0d-8 )
    parameter       ( zero   =  0.0d0 )
    parameter       ( un     =  1.0d0 )
    parameter       ( deux   =  2.0d0 )
    parameter       ( trois  =  3.0d0 )
    parameter       ( six    =  6.0d0 )
!     ------------------------------------------------------------------
    common /tdim/   ndt,ndi
!     ------------------------------------------------------------------
!
! =================================================================
! --- CALCUL DE SII -----------------------------------------------
! =================================================================
    call lcprsc(devsig, devsig, sii)
    sii = sqrt (sii)
! =====================================================================
! --- RECUPERATION DE PARAMETRES DU MODELE ----------------------------
! =====================================================================
    pi = r8pi()
    pref = mater(1,2)
    sigc = mater(3,2)
    h0ext = mater(4,2)
    s0 = mater(11,2)
    mult = mater(15,2)
    xie = mater(17,2)
    mvmax = mater(19,2)
!
    mu0v = mater(24,2)
    xi0v = mater(25,2)
    mu1 = mater(26,2)
    xi1 = mater(27,2)
! =================================================================
! --- CALCUL DE ALPHA RES -----------------------------------------
! =================================================================
    alres = un + mult
! =================================================================
! --- CALCUL DE H(THETA), H0E ET H0C -----------------------------
! =================================================================
    rcos3t = cos3t (devsig, pref, lgleps)
    call lkhtet(nmat, mater, rcos3t, h0e, h0c,&
                htheta)
    coefh = (h0c-h0ext)/(h0c-h0e)
! =================================================================
! --- CALCUL DE C TILDE -------------------------------------------
! =================================================================
    if (para(2) .le. zero) then
        fact1 = zero
        c = zero
    else
        fact1 = un + para(1)*para(3)*para(2)**(para(1)-un)
        c = sigc*(para(2))**para(1)/deux/sqrt(fact1)
    endif
! =================================================================
! --- CALCUL DE PHI TILDE -----------------------------------------
! =================================================================
    fact1 = sqrt(fact1)
    phi = deux*atan2(fact1,un)-pi/deux
! =================================================================
! --- CALCUL DE SIGMA TILDE ---------------------------------------
! =================================================================
    xip = vint(1)
    if (xip .le. xie) sigtil = c/tan(phi)
    if (xip .gt. xie) sigtil = zero
! =================================================================
! --- CALCUL DE SIGMIN ET SIGMAX ----------------------------------
! =================================================================
    troisd = trois/deux
    tiers = un/trois
    fact2 = (deux*htheta -(h0c + h0ext))/deux/(h0c-h0ext)
    sigmin = tiers * (i1 - (troisd-fact2)*sqrt(troisd)*sii)
    sigmax = tiers * (i1 + (troisd+fact2)*sqrt(troisd)*sii)
! =================================================================
! --- CALCUL DE SIGLIM  -------------------------------------------
! =================================================================
    siglim = sigmin + sigc * (mvmax*sigmin/sigc + s0)
! =================================================================
! --- CALCUL DE ALPHA  --------------------------------------------
! =================================================================
    alpha = (sigmax+sigtil)/(sigmin+sigtil)
! =================================================================
! --- CALCUL DE SIN(PSI) ------------------------------------------
! =================================================================
    if (val .eq. 0) then
        sinpsi = mu0v*((sigmax - siglim)/(xi0v*sigmax + siglim))
    else
        sinpsi = mu1*((alpha - alres)/(xi1*alpha + alres))
    endif
! =================================================================
! --- CALCUL DE D(BP)/D(SIN(PSI)) ---------------------------------
! =================================================================
    dbdsin = -six*sqrt(six)/(trois-sinpsi)**2
! =================================================================
! --- CALCUL DE D(SINPSI)/D(DEVSIG) -------------------------------
! --- DISTINCTION DES FORMULES ET VALEURS SI PRE OU POST-PIC ------
! =================================================================
    call lkdhds(nmat, mater, i1, devsig, dhds,&
                iret)
! --- CALCUL DE D(SIGMAX)/D(DEVSIG) ET D(SIGMIN)/D(DEVSIG)
    do 10 i = 1, ndt
        dsmids(i) = tiers*(&
                    sqrt(troisd)*sii/(h0c-h0ext)*dhds(i) *coefh-(troisd-(deux*htheta-h0c-h0ext)/(&
                    &deux *(h0c-h0ext)))* sqrt(troisd)*devsig(i)/sii&
                    )
!
        dsmads(i) = tiers*(&
                    sqrt(troisd)*sii/(h0c-h0ext)*dhds(i) *coefh+(troisd+(deux*htheta-h0c-h0ext)/ &
                    &(deux*(h0c-h0ext)))* sqrt(troisd)*devsig(i)/sii&
                    )
10  end do
!
    if (val .eq. 0) then
        do 20 i = 1, ndt
            dsinds(i) = mu0v*(&
                        (&
                        siglim*(un+xi0v))/(xi0v*sigmax+ siglim)**2*dsmads(i)-dsmids(i)*(un+mvmax)&
                        & *(un+xi0v)* sigmax/ (xi0v*sigmax+siglim&
                        )**2&
                        )
20      continue
!
    else if (val.eq.1) then
        do 30 i = 1, ndt
            dsinds(i) = mu1*(un+xi1)/(xi1*alpha+alres)**2* ((-alres*( sigmax+sigtil)/(sigmin+sigt&
                        &il)**2) *dsmids(i)+(alres/( sigmin+sigtil))*dsmads(i))
30      continue
    endif
!
! =================================================================
! --- CALCUL DE D(SINPSI)/D(I1) -----------------------------------
! --- DISTINCTION DES FORMULES ET VALEURS SI PRE OU POST-PIC ------
! =================================================================
! --- CALCUL DE D(SIGMAX)/D(I1) ET D(SIGMIN)/D(I1)
    dsmidi = tiers
!
    dsmadi = tiers
!
    if (val .eq. 0) then
        dsindi = mu0v*(&
                 (&
                 siglim*(un+xi0v))/(xi0v*sigmax+ siglim)**2* dsmadi-dsmidi*(un+mvmax) *(un+xi0v)*&
                 &sigmax/ (xi0v*sigmax+ siglim&
                 )**2&
                 )
!
    else if (val.eq.1) then
        dsindi = mu1*(un+xi1)*(sigmin-sigmax)*tiers/ (xi1*alpha+alres) **2*alres/(sigmin+sigtil)*&
                 &*2
    endif
!
! =================================================================
! --- CALCUL DE D(BP)/D(DEVSIG) -----------------------------------
! =================================================================
    call lcprsv(dbdsin, dsinds, dbetds)
! =================================================================
! --- CALCUL DE D(BP)/DI1 -----------------------------------------
! =================================================================
    dbetdi = dbdsin*dsindi
!
end subroutine

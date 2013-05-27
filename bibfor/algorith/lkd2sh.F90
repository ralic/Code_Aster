subroutine lkd2sh(nmat, materf, varh, dhds, devsig,&
                  rcos3t, d2shds, iret)
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
!     CALCUL DE DERIVEE 2NDE DE SII*H PAR RAPPORT A SIGMA
!     IN  NMAT   : DIMENSION TABLE DES PARAMETRES MATERIAU
!         MATERF : PARAMETRES MATERIAU A T+DT
!         VARH   : VECTEUR CONTENANT H0E,H0C ET HTHETA
!         DHDS   : DERIVEE DE HTHETA PAR RAPPORT A SIGMA
!         DEVSIG : DEIATEUR DES CONTRAINTES
!         RCOS3T : COS(3THETA) = SQRT(54)*DET(DEVISG)/SII**3
!     OUT D2SHDS :  DERIVEE 2NDE SII*H PAR RAPPORT A SIGMA (NDT X NDT)
!         IRET   :  CODE RETOUR
!     ------------------------------------------------------------------
    include 'asterfort/lcinma.h'
    include 'asterfort/lcprmm.h'
    include 'asterfort/lcprsc.h'
    include 'asterfort/lcprsm.h'
    include 'asterfort/lcprte.h'
    include 'asterfort/lcsoma.h'
    include 'asterfort/lkd2hs.h'
    integer :: iret, nmat
    real(kind=8) :: materf(nmat, 2), varh(3), d2shds(6, 6), dhds(6)
    real(kind=8) :: devsig(6), rcos3t
!
    integer :: ndi, ndt, i, j
    real(kind=8) :: h0ext, coefh, sii, un, zero, dikdjl(6, 6), dijdkl(6, 6)
    real(kind=8) :: trois, dsiids(6), dsdsig(6, 6), mat1(6, 6), d2hds2(6, 6)
    real(kind=8) :: mat2(6, 6), mat3(6, 6), dhtds(6), mat4(6, 6), mat5(6, 6)
    real(kind=8) :: d2hdsi(6, 6)
    parameter       ( zero   = 0.0d0 )
    parameter       ( un     = 1.0d0 )
    parameter       ( trois  = 3.0d0 )
!     ------------------------------------------------------------------
    common /tdim/   ndt,ndi
!     ------------------------------------------------------------------
!
! --- RECUPERATION PROPRIETES MATERIAUX
    h0ext = materf(4,2)
!
! --- COEFFICIENT (H0C-H0EXT)/(H0C-HOE)
    coefh = (varh(2)-h0ext)/(varh(2)-varh(1))
!
! --- CONSTRUCTION DE SII
    call lcprsc(devsig, devsig, sii)
    sii = sqrt(sii)
!
! --- INITIALISATION MATRICE D_IK X D_JL
    call lcinma(zero, dikdjl)
    do 10 i = 1, ndt
        dikdjl(i,i) = un
10  end do
!
! --- INITIALISATION MATRICE D_IJ X D_KL
    call lcinma(zero, dijdkl)
    do 20 i = 1, ndi
        do 30 j = 1, ndi
            dijdkl(i,j) = un/trois
30      continue
20  end do
!
! --- CALCUL DERIVEE SII PAR RAPPORT A SIGMA =
!          SIJ/SII*(K_IK*K_KL-1/3*K_IJ*K_KL)
    do 40 i = 1, ndt
        dsiids(i) = zero
        do 50 j = 1, ndt
            dsdsig(j,i) = dikdjl(j,i)-dijdkl(j,i)
            dsiids(i) = dsiids(i) + devsig(j)*dsdsig(j,i)/sii
50      continue
40  end do
!
! --- CALCUL DE DHDS*DSIIDS
    call lcprte(dhds, dsiids, mat1)
!
! --- CALCUL DE D2HDS2
    call lkd2hs(nmat, materf, devsig, sii, rcos3t,&
                dhds, d2hds2)
!
! --- CALCUL DE D2HDSIGMA
    call lcprmm(d2hds2, dsdsig, d2hdsi)
!
! --- CONSTRUCTION DE SII*D2HDSDSIGMA = MAT2
    call lcprsm(sii, d2hdsi, mat2)
!
! --- ADDITION DE MAT2 + MAT1 = MAT3
    call lcsoma(mat1, mat2, mat3)
!
! --- CALCUL DE COEFH*MAT3 = MAT2
    call lcprsm(coefh, mat3, mat2)
!
! --- CONSTRUCTION DE DHTDSIGMA = DHTDS*DSDSIG
    call lcprsm(coefh, dsdsig, mat1)
    do 60 i = 1, ndt
        dhtds(i) = zero
        do 70 j = 1, ndt
            dhtds(i) = dhtds(i) + dhds(j)*mat1(j,i)/sii
70      continue
60  end do
!
! --- CONSTRUCTION PRODUIT TENSORIEL DE MAT1 = DEVSIG*DHTDSIGMA
    call lcprte(devsig, dhtds, mat1)
!
! --- CALCUL DE HTHETA/SII*DSDSIG = MAT3
    call lcprsm(varh(3)/sii, dsdsig, mat3)
!
! --- MAT5 = HTHETA*DEVSIG*DSIIDS/SII**2
    call lcprte(devsig, dsiids, mat4)
    call lcprsm(varh(3)/sii**2, mat4, mat5)
!
! --- MAT4 = MAT2+MAT1+MAT3-MAT5
    do 80 i = 1, ndt
        do 90 j = 1, ndt
            mat4(i,j) = mat2(i,j)+mat1(i,j)+mat3(i,j)-mat5(i,j)
90      continue
80  end do
!
! --- D2SHDS = DSDSIG.MAT4
    call lcprmm(dsdsig, mat4, d2shds)
!
end subroutine

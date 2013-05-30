subroutine lkd2fs(nmat, materf, para, vara, varh,&
                  i1, devsig, ds2hds, d2shds, d2fds2,&
                  iret)
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
!     CALCUL DE DERIVEE 2NDE DE F PAR RAPPORT A SIGMA
!     IN  NMAT   : DIMENSION TABLE DES PARAMETRES MATERIAU
!         MATERF : PARAMETRES MATERIAU A T+DT
!         I1     : TRACE DES CONTRAINTES
!         DEVSIG : DEVIATEUR DES CONTRAINTES
!         PARA   : PARAMETRES AXI, SXI, MXI
!         VARA   : PARAMTERES ADXI,BDXI,DDXI,KDXI
!         VARH   : VECTEUR CONTENANT H0E,H0C ET HTHETA
!         DS2HDS : DERIVEE DE SII*H PAR RAPPORT A SIGMA
!         D2SHDS : DERIVVE 2NDE DE SII*H PAR RAPPORT A SIGMA
!     OUT D2FDS2 :  DERIVEE 2NDE F PAR RAPPORT A SIGMA (NDT X NDT)
!         IRET   :  CODE RETOUR
!     ------------------------------------------------------------------
    include 'asterfort/lcdima.h'
    include 'asterfort/lcinve.h'
    include 'asterfort/lcprsc.h'
    include 'asterfort/lcprsm.h'
    include 'asterfort/lcprte.h'
    integer :: iret, nmat
    real(kind=8) :: d2fds2(6, 6), para(3), vara(4), materf(nmat, 2)
    real(kind=8) :: devsig(6), i1, ds2hds(6), varh(3), d2shds(6, 6)
!
    integer :: ndi, ndt, i
    real(kind=8) :: sigc, sii, coef1, coef2, vident(6), zero, un, vect1(6)
    real(kind=8) :: mat1(6, 6), mat2(6, 6), mat3(6, 6), ucri, deux
    parameter       ( zero   = 0.0d0 )
    parameter       ( un     = 1.0d0 )
    parameter       ( deux   = 2.0d0 )
!     ------------------------------------------------------------------
    common /tdim/   ndt,ndi
!     ------------------------------------------------------------------
!
! --- RECUPERATION PARAMETRES MATERIAU
    sigc = materf(3,2)
!
! --- CONSTRUCTION DE SII
    call lcprsc(devsig, devsig, sii)
    sii = sqrt(sii)
!
! --- CONSTRUCTION COEF1 = A*SIGC*H0C*(A-1)(AD*SII*H+B*I1+D)^(A-2)
    ucri = vara(1)*sii*varh(3)+vara(2)*i1+vara(3)
    if (ucri .le. zero) then
        ucri = zero
        coef1 = zero
        coef2 = un
    else
        coef1 = para(1)*sigc*varh(2)*(para(1)-un)*ucri**(para(1)-deux)
! --- CONSTRUCTION COEF2 = A*SIGC*H0C(AD*SII*H+B*I1+D)^(A-1)
        coef2 = un-(vara(1)*para(1)*sigc*varh(2)*ucri**(para(1)-un))
    endif
!
! --- CONSTRUCTION VECTEUR IDENTITE
    call lcinve(zero, vident)
    do 10 i = 1, ndi
        vident(i) = un
10  end do
!
! --- CONSTRUCTION (A*DS2HDS+B*VIDENT)
    do 20 i = 1, ndt
        vect1(i) = vara(1)*ds2hds(i)+vara(2)*vident(i)
20  end do
! --- CONSTRUCTION PRODUIT TENSORIEL COEF1*(VECT1 X VECT1)
    call lcprte(vect1, vect1, mat1)
    call lcprsm(coef1, mat1, mat2)
!
! --- CONSTRUCTION PRODUIT COEF2*D2SHDS
    call lcprsm(coef2, d2shds, mat3)
!
! --- CONSTRUCTION DIFFERENCE MAT3-MAT2 = D2FDS2
    call lcdima(mat3, mat2, d2fds2)
!
end subroutine

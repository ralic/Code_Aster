subroutine lkdnds(nmat, materf, i1, devsig, bprimp,&
                  nvi, vint, val, para, dndsig)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     IN  NMAT   : DIMENSION TABLE DES PARAMETRES MATERIAU
!         MATERF : PARAMETRES MATERIAU A T+DT
!         DEVSIG : DEVIATEUR DES CONTRAINTES
!         I1     : TRACE DES CONTRAINTES
!         BPRIMP : PARAMETRE DE DILATANCE FCTN SIGMA
!         NVI    : NOMBRE DE VARIABLES INTERNES
!         VINT   : VARIABLES INTERNES
!         VAL    : BOOLEEN PRECISANT DILATANCE EN PRE(0) OU POST-PIC(1)
!         PARA   : VECTEUR CONTENANT AXI, SXI ET MXI
!
!     OUT DNDISG :  DERIVEE DE N PAR RAPPORT A SIGMA (NDT X NDT)
!     ------------------------------------------------------------------
#include "asterfort/lcdima.h"
#include "asterfort/lcinma.h"
#include "asterfort/lcinve.h"
#include "asterfort/lcprmv.h"
#include "asterfort/lcprsc.h"
#include "asterfort/lcprsm.h"
#include "asterfort/lcprsv.h"
#include "asterfort/lcprte.h"
#include "asterfort/lkdbds.h"
    integer :: nmat, nvi, val
    real(kind=8) :: materf(nmat, 2), dndsig(6, 6), devsig(6), i1
    real(kind=8) :: bprimp, vint(nvi), para(3)
!
    integer :: ndt, ndi, i, j
    real(kind=8) :: zero, un
    real(kind=8) :: dsdsig(6, 6), di1dsi(6), sii
    real(kind=8) :: dbetds(6), dbetdi, mident(6, 6)
    real(kind=8) :: dsiids(6, 6), kron2(6, 6)
    real(kind=8) :: unstro, unssii, trois, kron(6), dbdsig(6)
    real(kind=8) :: devbds(6, 6), dsidsi(6, 6), sdsids(6, 6), didbds(6, 6)
    parameter       (zero  =  0.d0 )
    parameter       (un    =  1.d0 )
    parameter       (trois =  3.d0 )
!     ------------------------------------------------------------------
    common /tdim/   ndt,ndi
!     ------------------------------------------------------------------
!
! ----------------------------
! --- 1) CALCUL TERMES COMMUNS
! ----------------------------
! --- CONSTRUCTION DE SII
    call lcprsc(devsig, devsig, sii)
    sii = sqrt(sii)
!
! --- CONSTRUCTION DE KRONECKER
    call lcinve(zero, kron)
    do 10 i = 1, ndi
        kron(i) = un
10  end do
!
! --- CONSTRUCTION DE MATRICE IDENTITE
    call lcinma(zero, mident)
    do 20 i = 1, ndt
        mident(i,i) = un
20  end do
!
! --- CONSTRUCTION DE DI1/DSIGMA
    call lcinve(zero, di1dsi)
    do 30 i = 1, ndi
        di1dsi(i) = un
30  end do
!
! --- CONSTRUCTION DE DS/DSIGMA
    unstro = un / trois
    call lcprte(kron, kron, kron2)
    call lcprsm(unstro, kron2, kron2)
    call lcdima(mident, kron2, dsdsig)
!
! --- CONSTRUCTION DE DSII/D(DEVSIG)
    unssii = un/sii
    call lcprsv(unssii, devsig, dsiids)
!
! --- CALCUL DE D(BPRIME)/D(DEVSIG) ET D(BPRIME)/D(I1)
    call lkdbds(nmat, materf, i1, devsig, nvi,&
                vint, para, val, dbetds, dbetdi)
!
! --- CONSTRUCTION DE D(BPRIME)/DSIGMA
    do 40 i = 1, ndt
        dbdsig(i) = zero
        do 50 j = 1, ndt
            dbdsig(i) = dbdsig(i)+dbetds(j)*dsdsig(j,i)
50      continue
        dbdsig(i) = dbdsig(i)+dbetdi*di1dsi(i)
40  end do
!
! --- PRODUIT TENSORIEL DE DEVSIG*D(BPRIME)/DSIGMA
    call lcprte(devsig, dbdsig, devbds)
!
! --- CALCUL DE DEVSIG*DSII/DSIGMA
    call lcprmv(dsdsig, dsiids, dsidsi)
    call lcprte(devsig, dsidsi, sdsids)
!
! --- PRODUIT TENSORIEL DE KRON*DBPRIME/DSIGMA
    call lcprte(kron, dbdsig, didbds)
!
! ------------------------------
! --- 2) ASSEMBLAGE DE DN/DSIGMA
! ------------------------------
    do 60 i = 1, ndt
        do 70 j = 1, ndt
            dndsig(i,j) = (&
                          (&
                          devbds(i,j)/sii+bprimp/sii*dsdsig(i,j)- bprimp/sii**2*sdsids(i,j))* sqr&
                          &t(bprimp**2+trois)-bprimp/ sqrt(bprimp**2+trois)*(bprimp/sii* devbds(i&
                          &,j)-didbds(i,j) )&
                          )/(bprimp**2+trois&
                          )
70      continue
60  end do
!
end subroutine

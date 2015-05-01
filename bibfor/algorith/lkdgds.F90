subroutine lkdgds(nmat, materf, para, vara, devsig,&
                  i1, val, ds2hds, vecn, dfds,&
                  bprimp, nvi, vint, dhds, dgds,&
                  iret)
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
!     CALCUL DE DERIVEE DE G PAR RAPPORT A SIGMA
!     POUR G PLASTIQUE OU VISQUEUX
!     IN  NMAT   : DIMENSION TABLE PARAMETRES MATERIAU
!         MATERF : TABLE DES PARAMETRES MATERIAU A T+DT
!         PARA   : VECTEUR CONTENANT AXI, SXI ET MXI
!         VARA   : VECTEUR CONTENANT ADXI, BDXI, DDXI ET KDXI
!         DEVSIG : DEVIATEUR DES CONTRAINTES
!         I1     : TRACE DES CONTRAINTES
!         VAL    : BOOLEEN PRECISANT DILATANCE EN PRE(0) OU POST-PIC(1)
!         VECN   : VECTEUR UNITAIRE DE DILATANCE
!         DFDS   : DERIVEE SEUIL PAR RAPPORT A SIGMA
!         DS2HDS : DERIVEE DE SII*H PAR RAPPORT A SIGMA
!         BPRIMP : PARAMETRE DE DILATANCE FCTN SIGMA
!         NVI    : NOMBRE DE VARIABLES INTERNES
!         VINT   : VARIABLES INTERNES
!         DHDS   : DERIVEE DE H(THETA) PAR RAPPORT A SIGMA
!     OUT DGDS   : DERIVEE DU POTENTIEL G PAR RAPPORT A SIGMA (NDTXNDT)
!         IRET   : CODE RETOUR
!     ------------------------------------------------------------------
#include "asterfort/cos3t.h"
#include "asterfort/lcprsc.h"
#include "asterfort/lcprsm.h"
#include "asterfort/lcprte.h"
#include "asterfort/lkd2fs.h"
#include "asterfort/lkd2sh.h"
#include "asterfort/lkdnds.h"
#include "asterfort/lkhtet.h"
    integer :: iret, nmat, nvi, val
    real(kind=8) :: materf(nmat, 2), dgds(6, 6), vecn(6), dfds(6), vint(nvi)
    real(kind=8) :: para(3), vara(4), ds2hds(6), devsig(6), i1, bprimp
    real(kind=8) :: dhds(6)
!
    integer :: i, j, ndi, ndt
    real(kind=8) :: d2fds2(6, 6), d2fdsn(6)
    real(kind=8) :: dfdnpn(6, 6), dfpndn(6, 6)
    real(kind=8) :: zero, dfdsdn(6), dfdsvn, d2shds(6, 6), varh(3)
    real(kind=8) :: h0e, h0c, htheta, rcos3t, lgleps, patm
    real(kind=8) :: dndsig(6, 6), d2fn2(6, 6)
    parameter       (zero  =  0.d0 )
    parameter       (lgleps = 1.0d-8 )
!     ------------------------------------------------------------------
    common /tdim/   ndt,ndi
!     ------------------------------------------------------------------
!
    patm = materf(1,2)
!
! --- APPEL A HOC ET  H(THETA)
    rcos3t = cos3t(devsig,patm,lgleps)
    call lkhtet(nmat, materf, rcos3t, h0e, h0c,&
                htheta)
    varh(1) = h0e
    varh(2) = h0c
    varh(3) = htheta
!
! --- CONSTRUCTION D2FDS2
    call lkd2sh(nmat, materf, varh, dhds, devsig,&
                rcos3t, d2shds, iret)
!
    call lkd2fs(nmat, materf, para, vara, varh,&
                i1, devsig, ds2hds, d2shds, d2fds2,&
                iret)
!
! --- CONSTRUCTION DNDSIG
    call lkdnds(nmat, materf, i1, devsig, bprimp,&
                nvi, vint, val, para, dndsig)
!
! --- CONSTRUCTION DE (D2FDS2:N)*N
    do 10 i = 1, ndt
        d2fdsn(i) = zero
        do 20 j = 1, ndt
            d2fdsn(i) = d2fdsn(i) + vecn(j)*d2fds2(j,i)
20      continue
10  end do
    call lcprte(vecn, d2fdsn, d2fn2)
!
! --- CONSTRUCTION DE (DFDS*DNDSIG)*VECN
    do 30 i = 1, ndt
        dfdsdn(i) = zero
        do 40 j = 1, ndt
            dfdsdn(i) = dfdsdn(i)+dfds(j)*dndsig(j,i)
40      continue
30  end do
    call lcprte(vecn, dfdsdn, dfdnpn)
!
! --- CONSTRUCTION DE (DFDS:VECN)*DNDSIG
    call lcprsc(dfds, vecn, dfdsvn)
    call lcprsm(dfdsvn, dndsig, dfpndn)
!
! --- CONSTRUCTION DE DGDS
    do 50 i = 1, ndt
        do 60 j = 1, ndt
            dgds(i,j) = d2fds2(i,j)-d2fn2(i,j)-dfdnpn(i,j)-dfpndn(i,j)
60      continue
50  end do
!
end subroutine

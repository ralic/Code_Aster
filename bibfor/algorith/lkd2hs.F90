subroutine lkd2hs(nmat, materf, devsig, sii, rcos3t,&
                  dhds, d2hds2)
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
!     CALCUL DE DERIVEE 2NDE DE H PAR RAPPORT A DEVIATEUR SIGMA
!     IN  NMAT   : DIMENSION TABLE DES PARAMETRES MATERIAU
!         MATERF : PARAMETRES MATERIAU A T+DT
!         DEVSIG : DEVIATEUR DES CONTRAINTES
!         SII    : 2EME INVARIANT DU DEVIATEUR
!         RCOS3T : COS(3THETA) = SQRT(54)*DET(DEVSIG)/SII**3
!         DHDS   : DERIVEE DE H PAR RAPPORT A DEVSIG
!     OUT D2HDS2 :  DERIVEE 2NDE H PAR RAPPORT A SIGMA (NDT X NDT)
!     ------------------------------------------------------------------
    include 'asterfort/cjst.h'
    include 'asterfort/lcinma.h'
    include 'asterfort/lcprte.h'
    include 'asterfort/lkd2de.h'
    include 'asterfort/lkhlod.h'
    integer :: nmat
    real(kind=8) :: devsig(6), rcos3t, sii, d2hds2(6, 6)
    real(kind=8) :: materf(nmat, 2), dhds(6)
!
    integer :: ndi, ndt, i, j
    real(kind=8) :: mident(6, 6), zero, un, rhlode, gamcjs
    real(kind=8) :: coef1, deux, trois, coef3, coef4, r54, coef7, cinq
    real(kind=8) :: coef6, ddetds(6), mat1(6, 6), mat2(6, 6)
    real(kind=8) :: mat6(6, 6), mat7(6, 6), d2dets(6, 6), mat5(6, 6)
    real(kind=8) :: coef5, coef2, six
    parameter       ( zero   = 0.0d0 )
    parameter       ( un     = 1.0d0 )
    parameter       ( deux   = 2.0d0 )
    parameter       ( trois  = 3.0d0 )
    parameter       ( cinq   = 5.0d0 )
    parameter       ( six    = 6.0d0 )
    parameter       ( r54    = 5.4d1 )
!     ------------------------------------------------------------------
    common /tdim/   ndt,ndi
!     ------------------------------------------------------------------
!
! --- RECUPERATION PROPRIETES MATERIAUX
    gamcjs = materf(5,2)
!
! --- CONSTRUCTION TENSEUR IDENTITE
    call lcinma(zero, mident)
    do 10 i = 1, ndt
        mident(i,i) = un
10  end do
!
    rhlode = lkhlod (gamcjs, rcos3t)
!
    coef1 = gamcjs*sqrt(r54)/(deux*(rhlode**5)*(sii**5))
    coef2 = trois*gamcjs*rcos3t/(deux*(rhlode**5)*(sii**4))
    coef3 = rcos3t*gamcjs/(deux*rhlode**5*sii**2)
    coef4 = coef3*deux/sii**2
    coef5 = coef3/rhlode*cinq
    coef6 = gamcjs*sqrt(r54)*cinq/six/sii**3/rhlode**6
    coef7 = gamcjs*sqrt(r54)/six/rhlode**5/sii**3
!
! --- CALCUL DERIVEE DET(DEVSIG) PAR RAPPORT A DEVSIG
    call cjst(devsig, ddetds)
    call lcprte(devsig, ddetds, mat1)
!
    call lcprte(devsig, devsig, mat2)
!
    call lcprte(devsig, dhds, mat5)
!
    call lcprte(ddetds, dhds, mat6)
!
    call lcprte(ddetds, devsig, mat7)
!
    call lkd2de(devsig, d2dets)
!
    do 20 i = 1, ndt
        do 30 j = 1, ndt
            d2hds2(i,j) = coef1*mat1(i,j)-coef2*mat2(i,j)+ coef3* mident(i,j)-coef4*mat2(i,j)- co&
                          &ef5*mat5(i,j)+coef6*mat6(i, j)+ coef1*mat7(i,j)-coef7*d2dets(i,j)
30      continue
20  end do
!
end subroutine

subroutine calkce(nno, ndim, kbp, kbb, pm,&
                  dp, kce, rce)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! person_in_charge: sebastien.fayolle at edf.fr
! TOLE CRS_1404
    implicit none
!
    include 'asterfort/matinv.h'
    include 'asterfort/matmul.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/transp.h'
    integer :: nno, ndim
    real(kind=8) :: kbp(ndim, nno), kbb(ndim, ndim), rce(nno)
    real(kind=8) :: kce(nno, nno), pm(nno), dp(nno)
!-----------------------------------------------------------------------
!          CALCUL DE LA MATRICE CE POUR LA CONDENSATION STATIQUE
!          ET DU PRODUIT CEP
!-----------------------------------------------------------------------
! IN  NNO     : NOMBRE DE NOEUDS DE L'ELEMENT
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  KBP     : DISTANCE DU POINT DE GAUSS A L'AXE (EN AXISYMETRIQUE)
! IN  KBB     : COORDONEES DES NOEUDS
! IN  PM      : P A L'INSTANT PRECEDENT
! IN  DP      : INCREMENT POUR P
! OUT CE      : MATRICE DE CONDENSATION STATIQUE
! OUT RCE     : PRODUIT MATRICE DE CONDENSATION-PRESSION
!-----------------------------------------------------------------------
!
    integer :: i
    real(kind=8) :: kpb(nno, ndim), kbbi(ndim, ndim)
    real(kind=8) :: det, pp(nno, 1), prod(ndim, nno)
!-----------------------------------------------------------------------
!
    call r8inir(nno*nno, 0.d0, kce, 1)
    call r8inir(nno, 0.d0, rce, 1)
    call r8inir(ndim*nno, 0.d0, prod, 1)
!
! - TRANSPOSITION DE LA MATRICE KBP
    call transp(kbp, ndim, ndim, nno, kpb,&
                nno)
!
! - INVERSE DE LA MATRICE KBB
    call matinv('S', ndim, kbb, kbbi, det)
!
! - PRODUIT DE L'INVERSE DE LA MATRICE KBB ET DE LA MATRICE KBP
    call matmul(kbbi, kbp, ndim, ndim, nno,&
                prod)
!
! - CALCUL DE KCE
    call matmul(kpb, prod, nno, ndim, nno,&
                kce)
!
! - CALCUL DU PRODUIT RCE
    do 1 i = 1, nno
        pp(i,1)=pm(i)+dp(i)
 1  end do
!
    call matmul(kce, pp, nno, nno, 1,&
                rce)
!
end subroutine

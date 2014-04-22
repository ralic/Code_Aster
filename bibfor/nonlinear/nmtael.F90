subroutine nmtael(fami, kpg, ksp, imate, ndimsi,&
                  matm, mat, sigm, epsm, deps,&
                  epm, sigdv, sigp)
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
!
#include "asterfort/verift.h"
    integer :: kpg, ksp, ndimsi, imate
    character(len=*) :: fami
    real(kind=8) :: matm(3), mat(3)
    real(kind=8) :: sigm(ndimsi), epsm(ndimsi), deps(ndimsi)
    real(kind=8) :: sigdv(ndimsi), epm(ndimsi), sigp(ndimsi)
!
! ----------------------------------------------------------------------
! TAHERI :  CALCUL DES CONTRAINTES ELASTIQUES ET DEFORMATIONS PLASTIQUES
! ----------------------------------------------------------------------
! IN  NDIMSI DIMENSION DES TENSEURS
! IN  MATM   CARACTERISTIQUES ELASTIQUES EN T-
! IN  MAT    CARACTERISTIQUES ELASTIQUES EN T+
! IN  SIGM   CONTRAINTES EN T-
! IN  DEPS   INCREMENT DE DEFORMATION
! OUT EPM    DEFORMATIONS PLASTIQUES EN T-
! OUT SIGDV  DEVIATEUR DES CONTRAINTES ELASTIQUES
! OUT SIGP   TENSEUR DES CONTRAINTES ELASTIQUES
! ----------------------------------------------------------------------
!
    integer :: k
    real(kind=8) :: troikm, deumum
    real(kind=8) :: troisk, deuxmu
    real(kind=8) :: epmmo
    real(kind=8) :: depsth, depsme(6), depsmo, depsdv(6)
    real(kind=8) :: sigmom, sigdvm(6), sigmo
    real(kind=8) :: kron(6)
    data    kron /1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
!
    call verift(fami, kpg, ksp, 'T', imate,&
                elas_keyword = 'ELAS', epsth=depsth)
!
    troikm = matm(1)
    deumum = matm(2)
    troisk = mat(1)
    deuxmu = mat(2)
!
!    CALCUL DES DEFORMATIONS PLASTIQUES AU TEMPS -
    do k = 1, ndimsi
        epm(k) = epsm(k) - sigm(k)/deumum
    end do
    epmmo = (epm(1)+epm(2)+epm(3)) / 3.d0
    do k = 1, 3
        epm(k) = epm(k) - epmmo
    end do
!
!    PARTS HYDROSTATIQUES ET DEVIATORIQUES DE L'INCR. DEFO. MECANIQUE
    do k = 1, ndimsi
        depsme(k) = deps(k) - depsth*kron(k)
    end do
    depsmo = (depsme(1)+depsme(2)+depsme(3))/3.d0
    do k = 1, ndimsi
        depsdv(k) = depsme(k) - depsmo * kron(k)
    end do
!
!
!    PART HYDROSTATIQUE DES CONTRAINTES
    sigmom = (sigm(1)+sigm(2)+sigm(3))/3.d0
    sigmo = troisk/troikm*sigmom + troisk*depsmo
!
!
!    PART DEVIATORIQUE DES CONTRAINTES ELASTIQUES
    do k = 1, ndimsi
        sigdvm(k) = sigm(k) - sigmom*kron(k)
        sigdv(k) = deuxmu/deumum*sigdvm(k) + deuxmu*depsdv(k)
    end do
!
!
!    CONTRAINTES ELASTIQUES
    do k = 1, ndimsi
        sigp(k) = sigmo*kron(k) + sigdv(k)
    end do
!
end subroutine

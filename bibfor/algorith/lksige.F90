subroutine lksige(mod, nmat, materd, deps, sigd,&
                  sigf)
    implicit none
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
!       --------------------------------------------------------------
!       INTEGRATION ELASTIQUE NON LINEAIRE SUR DT POUR LETK
!       IN  MOD    :  MODELISATION
!           NMAT   :  DIMENSION MATER
!           MATERD :  COEFFICIENTS MATERIAU A T
!           SIGD   :  CONTRAINTE  A T
!           DEPS   :  INCREMENT DE DEFORMATION
!       OUT SIGF   :  CONTRAINTE ELASTIQUE A T+DT
!       --------------------------------------------------------------
#include "asterfort/lcdevi.h"
#include "asterfort/lkelas.h"
    integer :: nmat, ndt, ndi
    real(kind=8) :: materd(nmat, 2)
    real(kind=8) :: sigd(6), sigf(6)
    real(kind=8) :: deps(6)
    character(len=8) :: mod
! --- VARIABLES LOCALES
    integer :: i
    real(kind=8) :: dsde(6, 6), kk, mu, depsv
    real(kind=8) :: deux, trois, un, zero, kron(6)
    real(kind=8) :: i1ml, iel, devsig(6), depsd(6)
    real(kind=8) :: sigdt(6), sigft(6), depst(6)
    parameter       (trois =  3.d0 )
    parameter       (deux  =  2.d0 )
    parameter       (un    =  1.d0 )
    parameter       (zero  =  0.d0 )
!       --------------------------------------------------------------
    common /tdim/   ndt  , ndi
!       --------------------------------------------------------------
    data   kron /un , un , un , zero ,zero ,zero/
!       --------------------------------------------------------------
!
! --------------------------------------------------------------------
! --- PASSAGE DES TENSEURS CONTRAINTES ET DEFORMATIONS A LA CONVENTION
! --- MECANIQUE DES SOLS --- INVERSE DE MMC
! --- TRACTION -> NEGATIF / COMPRESSION -> POSITIF
! --------------------------------------------------------------------
    do i = 1, ndt
        sigdt(i) = -sigd(i)
        depst(i) = -deps(i)
    end do
! --------------------------------------------------------------------
! --- CONSTRUCTION TENSEUR DE RIGIDITE ELASTIQUE NON LINEAIRE
! --------------------------------------------------------------------
    call lkelas(ndi, ndt, nmat, materd, depst,&
                sigdt, dsde, kk, mu)
!
! --------------------------------------------------------------------
! --- DEFINITION DE L'INCREMENT DEFORMATION VOLUMIQUE
! --------------------------------------------------------------------
    depsv = depst(1)+depst(2)+depst(3)
!
! --------------------------------------------------------------------
! --- DEFINITION DU 1ER INVARIANT DES CONTRAINTES ELASTIQUES: IEL
! --------------------------------------------------------------------
    i1ml = sigdt(1)+sigdt(2)+sigdt(3)
    iel = i1ml + trois*kk*depsv
!
! --------------------------------------------------------------------
! --- CONSTRUCTION TENSEUR DEVIATOIRE DES CONTRAINTES A T
! --------------------------------------------------------------------
    call lcdevi(sigdt, devsig)
!
! --------------------------------------------------------------------
! --- CONSTRUCTION TENSEUR DEVIATOIRE DE L'INCREMENT DES DEFORMATIONS
! --------------------------------------------------------------------
    call lcdevi(depst, depsd)
!
! --------------------------------------------------------------------
! --- CONSTRUCTION TENSEUR DEVIATOIRE DES CONTRAINTES ELASTIQUES
! --------------------------------------------------------------------
    do i = 1, ndt
        devsig(i) = devsig(i) + deux* mu *depsd(i)
    end do
!
! --------------------------------------------------------------------
! --- CONSTRUCTION TENSEUR DES CONTRAINTES ELASTIQUES
! --------------------------------------------------------------------
    do i = 1, ndt
        sigft(i) = devsig(i) + iel/trois*kron(i)
    end do
!
! --------------------------------------------------------------------
! --- RETOUR DES CONTRAINTES ELASTIQUES A LA CONVENTION MMC
! --------------------------------------------------------------------
    do i = 1, ndt
        sigf(i) = -sigft(i)
    end do
!
end subroutine

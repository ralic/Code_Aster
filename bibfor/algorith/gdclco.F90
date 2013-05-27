subroutine gdclco(e, tau)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    implicit none
    real(kind=8) :: e(6), tau(6)
! ----------------------------------------------------------------------
!       INTEGRATION DES LOIS EN GRANDES DEFORMATIONS CANO-LORENTZ
!   CALCUL DES CONTRAINTES A PARTIR DE LA DEFORMATION THERMO-ELASTIQUE
! ----------------------------------------------------------------------
! IN  E       DEFORMATION ELASTIQUE
! OUT TAU     CONTRAINTE DE KIRCHHOFF
! ----------------------------------------------------------------------
!  COMMON GRANDES DEFORMATIONS CANO-LORENTZ
!
    integer :: ind1(6), ind2(6)
    real(kind=8) :: kr(6), rac2, rc(6)
    real(kind=8) :: lambda, mu, deuxmu, unk, troisk, cother
    real(kind=8) :: jm, dj, jp, djdf(3, 3)
    real(kind=8) :: etr(6), dvetr(6), eqetr, tretr, detrdf(6, 3, 3)
    real(kind=8) :: dtaude(6, 6)
!
    common /gdclc/&
     &          ind1,ind2,kr,rac2,rc,&
     &          lambda,mu,deuxmu,unk,troisk,cother,&
     &          jm,dj,jp,djdf,&
     &          etr,dvetr,eqetr,tretr,detrdf,&
     &          dtaude
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
    integer :: ij
    real(kind=8) :: e2(6), tre
! ----------------------------------------------------------------------
!
!
!
!    CALCUL DE E.E
    e2(1) = e(1)*e(1) + e(4)*e(4)/2.d0 + e(5)*e(5)/2.d0
    e2(2) = e(4)*e(4)/2.d0 + e(2)*e(2) + e(6)*e(6)/2.d0
    e2(3) = e(5)*e(5)/2.d0 + e(6)*e(6)/2.d0 + e(3)*e(3)
    e2(4) = e(1)*e(4) + e(4)*e(2) + e(5)*e(6)/rac2
    e2(5) = e(1)*e(5) + e(4)*e(6)/rac2 + e(5)*e(3)
    e2(6) = e(4)*e(5)/rac2 + e(2)*e(6) + e(6)*e(3)
!
!    CALCUL DE TAU
    tre = e(1)+e(2)+e(3)
    do 10 ij = 1, 6
        tau(ij) = (lambda*tre+cother)*(2*e(ij) - kr(ij)) + deuxmu *(2*e2(ij) - e(ij))
10  end do
!
end subroutine

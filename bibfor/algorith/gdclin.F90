subroutine gdclin()
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
!
! ----------------------------------------------------------------------
!        INTEGRATION DES LOIS EN GRANDES DEFORMATIONS CANO-LORENTZ
!                            INITIALISATION
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
!
    rac2 = sqrt(2.d0)
!
!    AFFECTATION DU RACINE DE 2 EN REPRESENTATION VECTORIELLE
    rc(1) = 1.d0
    rc(2) = 1.d0
    rc(3) = 1.d0
    rc(4) = rac2
    rc(5) = rac2
    rc(6) = rac2
!
!    TENSEUR DU SECOND ORDRE IDENTITE (REPRESENTATION VECTORIELLE)
    kr(1) = 1.d0
    kr(2) = 1.d0
    kr(3) = 1.d0
    kr(4) = 0.d0
    kr(5) = 0.d0
    kr(6) = 0.d0
!
!    MANIPULATION DES INDICES : IJ -> I
    ind1(1) = 1
    ind1(2) = 2
    ind1(3) = 3
    ind1(4) = 2
    ind1(5) = 3
    ind1(6) = 3
!
!    MANIPULATION DES INDICES : IJ -> J
    ind2(1) = 1
    ind2(2) = 2
    ind2(3) = 3
    ind2(4) = 1
    ind2(5) = 1
    ind2(6) = 2
!
end subroutine

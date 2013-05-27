subroutine jacbpz(kn, cn, jacobc, jacobk)
    implicit none
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
! DESCRIPTION : CALCUL DES MATRICES JACOBIENNES LIEES A LA FORCE
! -----------   NON-LINEAIRE DE CHOC F(X,DX)
!
!               CAS DE LA BUTEE PLANE SUIVANT Z LOCAL
!
!               APPELANT : CALJAC
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    real(kind=8) :: kn, cn
    real(kind=8) :: jacobc(3, *), jacobk(3, *)
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
!  1. MATRICE JACOBIENNE DE RAIDEUR
!     -----------------------------
    jacobk(1,1) = 0.0d0
    jacobk(1,2) = 0.0d0
    jacobk(1,3) = 0.0d0
!
    jacobk(3,1) = 0.0d0
    jacobk(3,2) = 0.0d0
    jacobk(3,3) = - kn
!
    jacobk(2,1) = 0.0d0
    jacobk(2,2) = 0.0d0
    jacobk(2,3) = 0.0d0
!
!  2. MATRICE JACOBIENNE D'AMORTISSEMENT
!     ----------------------------------
    jacobc(1,1) = 0.0d0
    jacobc(1,2) = 0.0d0
    jacobc(1,3) = 0.0d0
!
    jacobc(3,1) = 0.0d0
    jacobc(3,2) = 0.0d0
    jacobc(3,3) = - cn
!
    jacobc(2,1) = 0.0d0
    jacobc(2,2) = 0.0d0
    jacobc(2,3) = 0.0d0
!
! --- FIN DE JACBPZ.
end subroutine

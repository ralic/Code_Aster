subroutine jacbqu(np3, nbseg, rc, theta, xloc,&
                  kn, cn, ic, jacobc, jacobk)
    implicit none
!-----------------------------------------------------------------------
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
!               CAS DE LA BUTEE QUELCONQUE
!
!               APPELANT : CALJAC
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
#include "asterfort/disbut.h"
    integer :: np3, nbseg(*)
    real(kind=8) :: rc(np3, *), theta(np3, *), xloc(*), kn, cn
    integer :: ic
    real(kind=8) :: jacobc(3, *), jacobk(3, *)
!
! VARIABLES LOCALES
! -----------------
    integer :: nbs
    real(kind=8) :: xjeu, cost, sint, dnorm, c2, cs, s2
!
! ROUTINES EXTERNES
! -----------------
!     EXTERNAL  DISBUT
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
!  1. CALCUL DU SIN ET DU COS DE L'ANGLE DE LA NORMALE A L'OBSTACLE
!     ET DE LA DISTANCE NORMALE DE CHOC
!     ---------------------------------
    nbs = nbseg(ic)
    call disbut(np3, ic, xloc, 3, xjeu,&
                rc, theta, nbs, cost, sint,&
                dnorm)
!
!  2. MATRICE JACOBIENNE DE RAIDEUR
!     -----------------------------
    jacobk(1,1) = 0.0d0
    jacobk(1,2) = 0.0d0
    jacobk(1,3) = 0.0d0
    jacobk(2,1) = 0.0d0
    jacobk(3,1) = 0.0d0
!
    c2 = cost * cost
    cs = cost * sint
    s2 = sint * sint
!
    jacobk(2,2) = - kn * c2
    jacobk(2,3) = - kn * cs
    jacobk(3,2) = - kn * cs
    jacobk(3,3) = - kn * s2
!
!  3. MATRICE JACOBIENNE D'AMORTISSEMENT
!     ----------------------------------
    jacobc(1,1) = 0.0d0
    jacobc(1,2) = 0.0d0
    jacobc(1,3) = 0.0d0
    jacobc(2,1) = 0.0d0
    jacobc(3,1) = 0.0d0
!
    jacobc(2,2) = - cn * c2
    jacobc(2,3) = - cn * cs
    jacobc(3,2) = - cn * cs
    jacobc(3,3) = - cn * s2
!
! --- FIN DE JACBQU.
end subroutine

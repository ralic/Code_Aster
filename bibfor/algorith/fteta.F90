subroutine fteta(theta, neq, f0, f1)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
    include 'blas/daxpy.h'
    include 'blas/dscal.h'
    real(kind=8) :: theta, f0(*), f1(*)
!**********************************************************************
!
!      BUT :   POUR LA METHODE DE WILSON  , CALCUL DU SECOND MEMBRE
!      ====    DANS VEC(NEQ+1:2*NEQ)
!
!     INPUT:
!           THETA  : PARAMETRE THETA
!           NEQ    : DIMENSION DES VECTEURS FA ET VEC
!           F0     : VECTEUR CHARGEMENT AU TEMPS T
!           F1     : VECTEUR CHARGEMENT AU TEMPS T+DT
!     OUTPUT:
!           F1     : VECTEUR CHARGEMENT THETA METHODE
!
!
!----------------------------------------------------------------------
!   E.D.F DER   JACQUART G. 47-65-49-41      LE 19 JUILLET 1990
!**********************************************************************
!
    real(kind=8) :: coef
!-----------------------------------------------------------------------
    integer :: neq
!-----------------------------------------------------------------------
    coef = 1.0d0 - theta
    call dscal(neq, theta, f1, 1)
    call daxpy(neq, coef, f0, 1, f1,&
               1)
end subroutine

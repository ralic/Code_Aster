subroutine calcj0(t, sigpri, valp)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!     CALCUL LE MAXIMUM DES :
!                        . CONTRAINTES PRINCIPALES      (= 3 VALEURS)
!     AU MOYEN DE LA METHODE ITERATIVE DE JACOBI (ROUTINE JACOBI.F )
!     LE CALCUL EST EN DIM 3 (COHERENT AVEC CHOIX DE NMVPRK)
! ----------------------------------------------------------------------
!     IN     T     TENSEUR CONTRAINTE OU DEFORMATION (XX YY ZZ XY XZ YZ)
!            ON EST EN DIMENSION ESPACE = 3
!     OUT    VALP  VECTEUR DES GRANDEURS EQUIVALENTES
! ----------------------------------------------------------------------
#include "asterfort/jacobi.h"
    real(kind=8) :: t(6), tb(6), ts(6), tu(6), vecp(3, 3)
    real(kind=8) :: valp(3), jacaux(3)
    real(kind=8) :: tol, toldyn
    real(kind=8) :: sigpri
    integer :: nbvec, nperm
    integer :: i, itype, iordre
!-----------------------------------------------------------------------
    integer :: nitjac
!-----------------------------------------------------------------------
    data   nperm ,tol,toldyn    /12,1.d-10,1.d-2/
! ----------------------------------------------------------------------
!     ON RECALCULE LES TERMES REELS DU TENSEURS SACHANT
!     QUE LES TERMES NON DIAGONAUX ONT ETE MULTIPLIE PAR SQRT(2)
!
    do 30 i = 1, 6
        tb(i)=t(i)
!      IF (I.GT.3) TB(I)=T(I)/RAC2
30  end do
!
!     REANRANGEMENT POUR LA ROUTINE JACOBI EN COLONNE
!     A=(XX YY ZZ XY XZ YZ)->B=(XX XY XZ YY YZ ZZ)
!
    ts(1)=tb(1)
    ts(2)=tb(4)
    ts(3)=tb(5)
    ts(4)=tb(2)
    ts(5)=tb(6)
    ts(6)=tb(3)
!
!     MATRICE UNITE = (1 0 0 1 0 1) (POUR JACOBI)
    tu(1) = 1.d0
    tu(2) = 0.d0
    tu(3) = 0.d0
    tu(4) = 1.d0
    tu(5) = 0.d0
    tu(6) = 1.d0
!
! -    VALEURS PRINCIPALES
    nbvec = 3
    itype = 2
    iordre = 2
    call jacobi(nbvec, nperm, tol, toldyn, ts,&
                tu, vecp, valp(1), jacaux, nitjac,&
                itype, iordre)
!
    sigpri=max(valp(1),valp(2),valp(3))
!
end subroutine

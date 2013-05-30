subroutine diago3(tens, vecp, valp)
    implicit none
    include 'asterfort/jacobi.h'
    real(kind=8) :: tens(6), valp(3), vecp(3, 3)
! ======================================================================
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
!
!              DIAGONALISATION MATRICE 3x3 SYMETRIQUE
!
!  IN
!     TENS   : TENSEUR SOUS LA FORME (XX YY ZZ XY XZ YZ)
!  OUT
!     VECP   : VECTEURS PROPRES
!     VALP   : VALEURS PROPRES
!
! --- ------------------------------------------------------------------
!
    real(kind=8) :: tol, toldyn
    real(kind=8) :: tr(6), tu(6), jacaux(3)
    integer :: nperm, nitjac, ttrij, otrij, nbind
!
    data        nperm , nbind   / 12 , 3 /
    data        tol ,   toldyn  / 1.0d-10 , 1.0d-02/
!     PAS DE TRI
    data        ttrij , otrij   / 2 , 2 /
!
! --- ------------------------------------------------------------------
!
!     MATRICE  TR = (XX XY XZ YY YZ ZZ) POUR JACOBI
    tr(1) = tens(1)
    tr(2) = tens(4)
    tr(3) = tens(5)
    tr(4) = tens(2)
    tr(5) = tens(6)
    tr(6) = tens(3)
!     MATRICE UNITE = (1 0 0 1 0 1) POUR JACOBI
    tu(1) = 1.d0
    tu(2) = 0.d0
    tu(3) = 0.d0
    tu(4) = 1.d0
    tu(5) = 0.d0
    tu(6) = 1.d0
!
    call jacobi(nbind, nperm, tol, toldyn, tr,&
                tu, vecp, valp, jacaux, nitjac,&
                ttrij, otrij)
end subroutine

subroutine calatm(neq, nbddl, mu, coef, ddl,&
                  atmu)
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
!
    implicit none
!
    integer :: neq
    integer :: nbddl
    integer :: ddl(nbddl)
    real(kind=8) :: mu
    real(kind=8) :: coef(nbddl)
    real(kind=8) :: atmu(neq)
!
! ----------------------------------------------------------------------
! ROUTINE APPELEE PAR : ALGOCL/ALGOCO/ALGOCP/CFACA1/CFATMU
!                       FRO2GD/FROGDP/FROLGD/FROPGD
! ----------------------------------------------------------------------
!
! CALCUL DE LA CONTRIBUTION D'UNE LIAISON DE CONTACT AU VECTEUR AT.MU.
!
! IN  NEQ    : NOMBRE D'EQUATIONS
! IN  NBDDL  : NOMBRE DE DDLS IMPLIQUES DANS LA LIAISON UNILATERALE
! IN  MU     : MULTIPLICATEUR DE LAGRANGE ASSOCIE AU CONTACT POUR
!              LA LIAISON ETUDIEE
! IN  COEF   : COEFFICIENTS IMPLIQUES DANS LA LIAISON UNILATERALE
! IN  DDL    : NUMEROS DES DDLS IMPLIQUES DANS LA LIAISON UNILATERALE
! I/O ATMU   : VECTEUR AT.MU
!
! ----------------------------------------------------------------------
!
    integer :: j
!
! ----------------------------------------------------------------------
!
    do 10 j = 1, nbddl
        atmu(ddl(j)) = atmu(ddl(j)) + coef(j) * mu
10  end do
!
! ======================================================================
!
end subroutine

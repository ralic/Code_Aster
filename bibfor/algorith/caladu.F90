subroutine caladu(neq, nbddl, coef, ddl, depl,&
                  val)
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
!
    implicit none
!
    integer :: neq
    integer :: nbddl
    integer :: ddl(nbddl)
    real(kind=8) :: coef(nbddl)
    real(kind=8) :: depl(neq)
    real(kind=8) :: val
!
! ----------------------------------------------------------------------
! ROUTINE APPELEE PAR : ALGOCL/ALGOCO/ALGOCP/CFACA2/CFADU/FRO2GD
!                       FROGDP/FROLGD/FROPGD/REAJEU/RESUCO
! ----------------------------------------------------------------------
!
! CALCUL DU TERME II DE (A.DEPL) OU A EST LA MATRICE DE CONTACT ET
! DEPL UN VECTEUR QUELCONQUE.
!
! IN  NEQ    : NOMBRE D'EQUATIONS
! IN  NBDDL  : NOMBRE DE DDLS IMPLIQUES DANS LA LIAISON UNILATERALE
! IN  COEF   : COEFFICIENTS IMPLIQUES DANS LA LIAISON UNILATERALE
! IN  DDL    : NUMEROS DES DDLS IMPLIQUES DANS LA LIAISON UNILATERALE
! IN  DEPL   : VECTEUR A MULTIPLIER PAR LA MATRICE A
! OUT VAL    : VALEUR DU TERME OBTENU
!
! ----------------------------------------------------------------------
!
    integer :: j
!
! ----------------------------------------------------------------------
!
    val = 0.d0
    do 10 j = 1, nbddl
        val = val + coef(j) * depl(ddl(j))
10  end do
!
! ======================================================================
!
end subroutine

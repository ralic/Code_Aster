function csevl(x, cs, n)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! UTILISEE SOUS NT POUR L'EVALUATION DE LA FONCTION D'ERREUR
! ERFC (PROVIENT DE LA BIBLIOTHEQUE SLATEC)
!
!***BEGIN PROLOGUE  CSEVL
!***PURPOSE  EVALUATE A CHEBYSHEV SERIES.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C3A2
!***TYPE      DOUBLE PRECISION (CSEVL-S, DCSEVL-D)
!***KEYWORDS  CHEBYSHEV SERIES, FNLIB, SPECIAL FUNCTIONS
!***AUTHOR  FULLERTON, W., (LANL)
!***DESCRIPTION
!
!  EVALUATE THE N-TERM CHEBYSHEV SERIES CS AT X.  ADAPTED FROM
!  A METHOD PRESENTED IN THE PAPER BY BROUCKE REFERENCED BELOW.
!
!       INPUT ARGUMENTS --
!  X    VALUE AT WHICH THE SERIES IS TO BE EVALUATED.
!  CS   ARRAY OF N TERMS OF A CHEBYSHEV SERIES.  IN EVALUATING
!       CS, ONLY HALF THE FIRST COEFFICIENT IS SUMMED.
!  N    NUMBER OF TERMS IN ARRAY CS.
!
!***REFERENCES  R. BROUCKE, TEN SUBROUTINES FOR THE MANIPULATION OF
!                 CHEBYSHEV SERIES, ALGORITHM 446, COMMUNICATIONS OF
!                 THE A.C.M. 16, (1973) PP. 254-256.
!               L. FOX AND I. B. PARKER, CHEBYSHEV POLYNOMIALS IN
!                 NUMERICAL ANALYSIS, OXFORD UNIVERSITY PRESS, 1968,
!                 PAGE 56.
!***END PROLOGUE  DCSEVL
    implicit none
    real(kind=8) :: csevl
    include 'asterc/r8prem.h'
    include 'asterfort/assert.h'
    real(kind=8) :: b0, b1, b2, cs(*), onepl, twox, x
    logical :: first
    save first, onepl
!-----------------------------------------------------------------------
    integer :: i, n, ni
!-----------------------------------------------------------------------
    data first /.true./
!***FIRST EXECUTABLE STATEMENT  DCSEVL
    if (first) onepl = 1.0d0 + r8prem()
    first = .false.
    call assert(n .ge. 1)
    call assert(n .le. 1000)
    call assert(abs(x) .le. onepl)
!
    b1 = 0.0d0
    b0 = 0.0d0
    twox = 2.0d0*x
    do 10 i = 1, n
        b2 = b1
        b1 = b0
        ni = n + 1 - i
        b0 = twox*b1 - b2 + cs(ni)
10  end do
!
    csevl = 0.5d0*(b0-b2)
!
end function

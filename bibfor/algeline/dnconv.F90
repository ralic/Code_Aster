subroutine dnconv(n, ritzr, ritzi, bounds, tol,&
                  nconv)
!---------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) LAPACK
! ======================================================================
!
!     SUBROUTINE ARPACK EFFECTUANT LES TESTS DE CONVERGENCE.
!-----------------------------------------------------------------------
! BEGINDOC
!
! DESCRIPTION:
!  CONVERGENCE TESTING FOR THE NONSYMMETRIC ARNOLDI EIGENVALUE ROUTINE.
!
! ARGUMENTS
!  N       INTEGER.  (INPUT)
!          NUMBER OF RITZ VALUES TO CHECK FOR CONVERGENCE.
!
!  RITZR,  REAL*8 ARRAYS OF LENGTH N.  (INPUT)
!  RITZI   REAL AND IMAGINARY PARTS OF THE RITZ VALUES TO BE CHECKED
!          FOR CONVERGENCE.
!
!  BOUNDS  REAL*8 ARRAY OF LENGTH N.  (INPUT)
!          RITZ ESTIMATES FOR THE RITZ VALUES IN RITZR AND RITZI.
!
!  TOL     REAL*8 SCALAR.  (INPUT)
!          DESIRED BACKWARD ERROR FOR A RITZ VALUE TO BE CONSIDERED
!          "CONVERGED".
!
!  NCONV   INTEGER SCALAR.  (OUTPUT)
!          NUMBER OF "CONVERGED" RITZ VALUES.
!
! ENDDOC
!-----------------------------------------------------------------------
! BEGINLIB
!
! ROUTINES CALLED:
!   DLAPY2  LAPACK ROUTINE TO COMPUTE SQRT(X**2+Y**2) CAREFULLY.
!
! INTRINSIC FUNCTIONS:
!   MAX.
!
! AUTHOR
!     DANNY SORENSEN               PHUONG VU
!     RICHARD LEHOUCQ              CRPC / RICE UNIVERSITY
!     DEPT. OF COMPUTATIONAL &     HOUSTON, TEXAS
!     APPLIED MATHEMATICS
!     RICE UNIVERSITY
!     HOUSTON, TEXAS
!
! REVISION HISTORY:
!     XX/XX/92: VERSION ' 2.1'
!
! FILE: NCONV.F   SID: 2.3   DATE OF SID: 4/20/96   RELEASE: 2
!
! ASTER INFORMATION
! 07/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
!            DISPARITION DE SECOND ET DLAMCH,
!            DISPARITION DU COMMON TIMING ET DEBUG,
!            UTILISATION DE R8PREM(),
!            IMPLICIT NONE.
!
! ENDLIB
!-----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
!     %------------------%
!     | SCALAR ARGUMENTS |
!     %------------------%
!
    include 'asterc/matfpe.h'
    include 'asterc/r8prem.h'
    include 'blas/dlapy2.h'
    integer :: n, nconv
    real(kind=8) :: tol
!
!     %-----------------%
!     | ARRAY ARGUMENTS |
!     %-----------------%
!
    real(kind=8) :: ritzr(n), ritzi(n), bounds(n)
!
!     %---------------%
!     | LOCAL SCALARS |
!     %---------------%
!
    integer :: i
    real(kind=8) :: temp, eps23
!
!     %-----------%
!     | FUNCTIONS |
!     %-----------%
!
!
!     %-----------------------%
!     | EXECUTABLE STATEMENTS |
!     %-----------------------%
!
    call matfpe(-1)
!
!     %-------------------------------------------------------------%
!     | CONVERGENCE TEST: UNLIKE IN THE SYMMETRIC CODE, I AM NOT    |
!     | USING THINGS LIKE REFINED ERROR BOUNDS AND GAP CONDITION    |
!     | BECAUSE I DON'T KNOW THE EXACT EQUIVALENT CONCEPT.          |
!     |                                                             |
!     | INSTEAD THE I-TH RITZ VALUE IS CONSIDERED "CONVERGED" WHEN: |
!     |                                                             |
!     |     BOUNDS(I) .LE. ( TOL * | RITZ | )                       |
!     |                                                             |
!     | FOR SOME APPROPRIATE CHOICE OF NORM.                        |
!     %-------------------------------------------------------------%
!
!     %---------------------------------%
!     | GET MACHINE DEPENDENT CONSTANT. |
!     %---------------------------------%
!
    eps23 = (r8prem()*0.5d0)**(2.0d+0 / 3.0d+0)
!
    nconv = 0
    do 20 i = 1, n
        temp = max( eps23, dlapy2( ritzr(i), ritzi(i) ) )
        if (bounds(i) .le. tol*temp) nconv = nconv + 1
20  end do
!
    call matfpe(1)
!
!     %---------------%
!     | END OF DNCONV |
!     %---------------%
!
end subroutine

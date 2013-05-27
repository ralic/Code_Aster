subroutine xerbla(srname, info)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
!----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) LAPACK / BLAS
! ======================================================================
!
!     SUBROUTINE LAPACK / BLAS DE TRAITEMENT DES ERREURS.
!-----------------------------------------------------------------------
!  -- LAPACK AUXILIARY ROUTINE (VERSION 2.0) --
!     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
!     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
!     SEPTEMBER 30, 1994
!
!  PURPOSE
!  =======
!
!  XERBLA  IS AN ERROR HANDLER FOR THE LAPACK ROUTINES.
!  IT IS CALLED BY AN LAPACK ROUTINE IF AN INPUT PARAMETER HAS AN
!  INVALID VALUE.  A MESSAGE IS PRINTED AND EXECUTION STOPS.
!
!  INSTALLERS MAY CONSIDER MODIFYING THE STOP STATEMENT IN ORDER TO
!  CALL SYSTEM-SPECIFIC EXCEPTION-HANDLING FACILITIES.
!
!  ARGUMENTS
!  =========
!
!  SRNAME  (INPUT) CHARACTER*6
!          THE NAME OF THE ROUTINE WHICH CALLED XERBLA.
!
!  INFO    (INPUT) INTEGER
!          THE POSITION OF THE INVALID PARAMETER IN THE PARAMETER LIST
!          OF THE CALLING ROUTINE.
!
! ASTER INFORMATION
! 11/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
!            AU LIEU DE WRITE(*,FMT=9999)SRNAME,INFO
!            IMPLICIT NONE
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
!     .. SCALAR ARGUMENTS ..
    include 'asterfort/u2mesg.h'
    character(len=6) :: srname
    character(len=24) :: valk
    integer :: info
    integer :: vali(2)
!     ..
!     .. EXECUTABLE STATEMENTS ..
!
    valk = srname
    vali (1) = info
    vali (2) = info
    call u2mesg('F', 'ALGELINE5_4', 1, valk, 2,&
                vali, 0, 0.d0)
!
!     END OF XERBLA
!
end subroutine

subroutine dngets(ishift, which, kev, np, ritzr,&
                  ritzi, bounds, shiftr, shifti)
!---------------------------------------------------------------------
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
!     SUBROUTINE ARPACK CALCULANT NP SHIFTS DU RESTART DE IRAM.
!---------------------------------------------------------------------
! BEGINDOC
!
! NAME: DNGETS
!
! DESCRIPTION:
!  GIVEN THE EIGENVALUES OF THE UPPER HESSENBERG MATRIX H,
!  COMPUTES THE NP SHIFTS AMU THAT ARE ZEROS OF THE POLYNOMIAL OF
!  DEGREE NP WHICH FILTERS OUT COMPONENTS OF THE UNWANTED EIGENVECTORS
!  CORRESPONDING TO THE AMU'S BASED ON SOME GIVEN CRITERIA.
!
!  NOTE: CALL THIS EVEN IN THE CASE OF USER SPECIFIED SHIFTS IN ORDER
!  TO SORT THE EIGENVALUES, AND ERROR BOUNDS OF H FOR LATER USE.
!
!
! ARGUMENTS
!  ISHIFT  INTEGER.  (INPUT)
!          METHOD FOR SELECTING THE IMPLICIT SHIFTS AT EACH ITERATION.
!          ISHIFT = 0: USER SPECIFIED SHIFTS
!          ISHIFT = 1: EXACT SHIFT WITH RESPECT TO THE MATRIX H.
!
!  WHICH   CHARACTER*2.  (INPUT)
!          SHIFT SELECTION CRITERIA.
!          'LM' -> WANT THE KEV EIGENVALUES OF LARGEST MAGNITUDE.
!          'SM' -> WANT THE KEV EIGENVALUES OF SMALLEST MAGNITUDE.
!          'LR' -> WANT THE KEV EIGENVALUES OF LARGEST REAL PART.
!          'SR' -> WANT THE KEV EIGENVALUES OF SMALLEST REAL PART.
!          'LI' -> WANT THE KEV EIGENVALUES OF LARGEST IMAGINARY PART.
!          'SI' -> WANT THE KEV EIGENVALUES OF SMALLEST IMAGINARY PART.
!
!  KEV   INTEGER.  (INPUT/OUTPUT)
!     INPUT: KEV+NP IS THE SIZE OF THE MATRIX H.
!     OUTPUT: POSSIBLY INCREASES KEV BY ONE TO KEEP COMPLEX CONJUGATE
!     PAIRS TOGETHER.
!
!  NP   INTEGER.  (INPUT/OUTPUT)
!       NUMBER OF IMPLICIT SHIFTS TO BE COMPUTED.
!       OUTPUT: POSSIBLY DECREASES NP BY ONE TO KEEP COMPLEX CONJUGATE
!       PAIRS TOGETHER.
!
!  RITZR,  REAL*8 ARRAY OF LENGTH KEV+NP.  (INPUT/OUTPUT)
!  RITZI   ON INPUT, RITZR AND RITZI CONTAIN THE REAL AND IMAGINARY
!       PARTS OF THE EIGENVALUES OF H.
!       ON OUTPUT, RITZR AND RITZI ARE SORTED SO THAT THE UNWANTED
!       EIGENVALUES ARE IN THE FIRST NP LOCATIONS AND THE WANTED
!       PORTION IS IN THE LAST KEV LOCATIONS.  WHEN EXACT SHIFTS ARE
!       SELECTED, THE UNWANTED PART CORRESPONDS TO THE SHIFTS TO
!       BE APPLIED. ALSO, IF ISHIFT .EQ. 1, THE UNWANTED EIGENVALUES
!       ARE FURTHER SORTED SO THAT THE ONES WITH LARGEST RITZ VALUES
!       ARE FIRST.
!
!  BOUNDS  REAL*8 ARRAY OF LENGTH KEV+NP.  (INPUT/OUTPUT)
!          ERROR BOUNDS CORRESPONDING TO THE ORDERING IN RITZ.
!
!  SHIFTR, SHIFTI  *** USE DEPRECATED AS OF VERSION 2.1. ***
!
!
! ENDDOC
!-----------------------------------------------------------------------
! BEGINLIB
!
! ROUTINES CALLED:
!     DSORTC  ARPACK SORTING ROUTINE.
!
! INTRINSIC FUNCTIONS
!     ABS
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
! FILE: NGETS.F   SID: 2.3   DATE OF SID: 4/20/96   RELEASE: 2
!
! ASTER INFORMATION
! 07/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
!            DISPARITION DE SECOND,
!            COMMON TIMING REMPLACE PAR COMMON INFOR,
!            IMPLICIT NONE.
!
! ENDLIB
!-----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
!     %-----------------------------%
!     | INCLUDE FILES FOR DEBUGGING |
!     %-----------------------------%
!
#include "asterfort/dsortc.h"
#include "asterfort/dvout.h"
#include "asterfort/ivout.h"
    integer :: logfil, ndigit, mgetv0, mnaupd, mnaup2, mnaitr, mneigh, mnapps
    integer :: mngets, mneupd
    common /debug/&
     &  logfil, ndigit, mgetv0,&
     &  mnaupd, mnaup2, mnaitr, mneigh, mnapps, mngets, mneupd
!
!     %------------------%
!     | SCALAR ARGUMENTS |
!     %------------------%
!
    character(len=2) :: which
    integer :: ishift, kev, np
!
!     %-----------------%
!     | ARRAY ARGUMENTS |
!     %-----------------%
!
    real(kind=8) :: bounds(kev+np), ritzr(kev+np), ritzi(kev+np), shiftr(1)
    real(kind=8) :: shifti(1)
!
!     %------------%
!     | PARAMETERS |
!     %------------%
!
    real(kind=8) :: zero
    parameter (zero = 0.0d+0)
!
!     %---------------%
!     | LOCAL SCALARS |
!     %---------------%
!
    integer :: msglvl
!
!     %-----------------------%
!     | EXECUTABLE STATEMENTS |
!     %-----------------------%
!
!     %-------------------------------%
!     | INITIALIZE TIMING STATISTICS  |
!     | & MESSAGE LEVEL FOR DEBUGGING |
!     %-------------------------------%
!
    msglvl = mngets
!
!     %----------------------------------------------------%
!     | LM, SM, LR, SR, LI, SI CASE.                       |
!     | SORT THE EIGENVALUES OF H INTO THE DESIRED ORDER   |
!     | AND APPLY THE RESULTING ORDER TO BOUNDS.           |
!     | THE EIGENVALUES ARE SORTED SO THAT THE WANTED PART |
!     | ARE ALWAYS IN THE LAST KEV LOCATIONS.              |
!     | WE FIRST DO A PRE-PROCESSING SORT IN ORDER TO KEEP |
!     | COMPLEX CONJUGATE PAIRS TOGETHER                   |
!     %----------------------------------------------------%
!
    if (which .eq. 'LM') then
        call dsortc('LR', .true._1, kev+np, ritzr, ritzi,&
                    bounds)
    else if (which .eq. 'SM') then
        call dsortc('SR', .true._1, kev+np, ritzr, ritzi,&
                    bounds)
    else if (which .eq. 'LR') then
        call dsortc('LM', .true._1, kev+np, ritzr, ritzi,&
                    bounds)
    else if (which .eq. 'SR') then
        call dsortc('SM', .true._1, kev+np, ritzr, ritzi,&
                    bounds)
    else if (which .eq. 'LI') then
        call dsortc('LM', .true._1, kev+np, ritzr, ritzi,&
                    bounds)
    else if (which .eq. 'SI') then
        call dsortc('SM', .true._1, kev+np, ritzr, ritzi,&
                    bounds)
    endif
!
    call dsortc(which, .true._1, kev+np, ritzr, ritzi,&
                bounds)
!
!     %-------------------------------------------------------%
!     | INCREASE KEV BY ONE IF THE ( RITZR(NP),RITZI(NP) )    |
!     | = ( RITZR(NP+1),-RITZI(NP+1) ) AND RITZ(NP) .NE. ZERO |
!     | ACCORDINGLY DECREASE NP BY ONE. IN OTHER WORDS KEEP   |
!     | COMPLEX CONJUGATE PAIRS TOGETHER.                     |
!     %-------------------------------------------------------%
!
    if (( ritzr(np+1) - ritzr(np) ) .eq. zero .and. ( ritzi(np+1) + ritzi(np) ) .eq. zero) then
        np = np - 1
        kev = kev + 1
    endif
!
    if (ishift .eq. 1) then
!
!        %-------------------------------------------------------%
!        | SORT THE UNWANTED RITZ VALUES USED AS SHIFTS SO THAT  |
!        | THE ONES WITH LARGEST RITZ ESTIMATES ARE FIRST        |
!        | THIS WILL TEND TO MINIMIZE THE EFFECTS OF THE         |
!        | FORWARD INSTABILITY OF THE ITERATION WHEN THEY SHIFTS |
!        | ARE APPLIED IN SUBROUTINE DNAPPS.                     |
!        | BE CAREFUL AND USE 'SR' SINCE WE WANT TO SORT BOUNDS! |
!        %-------------------------------------------------------%
!
        call dsortc('SR', .true._1, np, bounds, ritzr,&
                    ritzi)
    endif
!
    if (msglvl .gt. 0) then
        call ivout(logfil, 1, [kev], ndigit, '_NGETS: KEV IS')
        call ivout(logfil, 1, [np], ndigit, '_NGETS: NP IS')
        call dvout(logfil, kev+np, ritzr, ndigit,&
                   '_NGETS: EIGENVALUES OF CURRENT H MATRIX -- REAL PART')
        call dvout(logfil, kev+np, ritzi, ndigit,&
                   '_NGETS: EIGENVALUES OF CURRENT H MATRIX -- IMAG PART')
        call dvout(logfil, kev+np, bounds, ndigit,&
                   '_NGETS: RITZ ESTIMATES OF THE CURRENT KEV+NP RITZ VALUES')
    endif
!
!     %---------------%
!     | END OF DNGETS |
!     %---------------%
!
end subroutine

! Define some constant values shared by several subroutines
! (C or Fortran 90)
!
! Constant values to check status in parallel
!
! ST_OK and ST_ER can be used for all boolean tests.
!
! ST_OK and others ST_xxx values allow binary operations to be more
! precise.
!
! ST_ER and ST_xxx constants must not be used together.
!
! ST_ER_PR0 : error on processor #0
! ST_ER_OTH : error on another processor
! ST_UN_OTH : undefined status for another processor
! ST_EXCEPT : not a fatal error, an exception
      INTEGER     ST_OK, ST_ER
      INTEGER     ST_ER_PR0, ST_ER_OTH, ST_UN_OTH
      INTEGER     ST_EXCEPT
      PARAMETER ( ST_ER     =  1,
     &            ST_OK     =  0,
     &
     &            ST_ER_PR0 =  4,
     &            ST_ER_OTH =  8,
     &            ST_UN_OTH = 16,
     &            ST_EXCEPT = 32 )
!
! ST_TAG_CHK : mpi communication tag for the check step of the status
! ST_TAG_CNT : mpi communication tag for the continue or stop
! ST_TAG_ALR : mpi communication tag for the alarm check
      INTEGER     ST_TAG_CHK, ST_TAG_CNT, ST_TAG_ALR
      PARAMETER ( ST_TAG_CHK = 123111,
     &            ST_TAG_CNT = 123222,
     &            ST_TAG_ALR = 123333 )
!

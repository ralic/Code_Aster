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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
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
    integer :: st_ok, st_er
    integer :: st_er_pr0, st_er_oth, st_un_oth
    integer :: st_except
    parameter ( st_er     =  1,&
     &          st_ok     =  0,&
     &          st_er_pr0 =  4,&
     &          st_er_oth =  8,&
     &          st_un_oth = 16,&
     &          st_except = 32 )
!
! ST_TAG_CHK : mpi communication tag for the check step of the status
! ST_TAG_CNT : mpi communication tag for the continue or stop
! ST_TAG_ALR : mpi communication tag for the alarm check
    integer :: st_tag_chk, st_tag_cnt, st_tag_alr
    parameter ( st_tag_chk = 123111,&
     &          st_tag_cnt = 123222,&
     &          st_tag_alr = 123333 )

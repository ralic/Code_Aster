!   COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
!   THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
!   IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
!   THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
!   (AT YOUR OPTION) ANY LATER VERSION.
!  
!   THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
!   WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
!   MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
!   GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!  
!   YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
!   ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!      1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!  
!   Define some constant values shared by several subroutines
!   (C or Fortran 90)
!  
!   Constant values to check status in parallel
!  
!   ST_OK and ST_ER can be used for all boolean tests.
!  
!   ST_OK and others ST_xxx values allow binary operations to be more
!   precise.
!  
!   ST_ER and ST_xxx constants must not be used together.
!  
!   ST_ER_PR0 : error on processor #0
!   ST_ER_OTH : error on another processor
!   ST_UN_OTH : undefined status for another processor
!   ST_EXCEPT : not a fatal error, an exception
!
!   ST_TAG_CHK : mpi communication tag for the check step of the status
!   ST_TAG_CNT : mpi communication tag for the continue or stop
!   ST_TAG_ALR : mpi communication tag for the alarm check
!
#ifndef ASTER_CONSTANT_H
#define ASTER_CONSTANT_H
!
#define ST_ER       1
#define ST_OK       0
#define ST_ER_PR0   4
#define ST_ER_OTH   8
#define ST_UN_OTH  16
#define ST_EXCEPT  32
!
#define ST_TAG_CHK  123111
#define ST_TAG_CNT  123222
#define ST_TAG_ALR  123333
!
#endif

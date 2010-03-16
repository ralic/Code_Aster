!
!  This file is part of MUMPS 4.9.2, built on Thu Nov  5 07:05:08 UTC 2009
!
!
!  This version of MUMPS is provided to you free of charge. It is public
!  domain, based on public domain software developed during the Esprit IV
!  European project PARASOL (1996-1999) by CERFACS, ENSEEIHT-IRIT and RAL.
!  Since this first public domain version in 1999, the developments are
!  supported by the following institutions: CERFACS, CNRS, INPT(ENSEEIHT)-
!  IRIT, and INRIA.
!
!  Current development team includes Patrick Amestoy, Alfredo Buttari,
!  Abdou Guermouche, Jean-Yves L'Excellent, Bora Ucar.
!
!  Up-to-date copies of the MUMPS package can be obtained
!  from the Web pages:
!  http://mumps.enseeiht.fr/  or  http://graal.ens-lyon.fr/MUMPS
!
!
!   THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY
!   EXPRESSED OR IMPLIED. ANY USE IS AT YOUR OWN RISK.
!
!
!  User documentation of any code that uses this software can
!  include this complete notice. You can acknowledge (using
!  references [1] and [2]) the contribution of this package
!  in any scientific publication dependent upon the use of the
!  package. You shall use reasonable endeavours to notify
!  the authors of the package of this publication.
!
!   [1] P. R. Amestoy, I. S. Duff, J. Koster and  J.-Y. L'Excellent,
!   A fully asynchronous multifrontal solver using distributed dynamic
!   scheduling, SIAM Journal of Matrix Analysis and Applications,
!   Vol 23, No 1, pp 15-41 (2001).
!
!   [2] P. R. Amestoy and A. Guermouche and J.-Y. L'Excellent and
!   S. Pralet, Hybrid scheduling for the parallel solution of linear
!   systems. Parallel Computing Vol 32 (2), pp 136-156 (2006).
!
!
!      Dummy mpif.h file including symbols used by MUMPS.
!
      INTEGER*4 MPI_2DOUBLE_PRECISION
      INTEGER*4 MPI_2INTEGER
      INTEGER*4 MPI_2REAL
      INTEGER*4 MPI_ANY_SOURCE
      INTEGER*4 MPI_ANY_TAG
      INTEGER*4 MPI_BYTE
      INTEGER*4 MPI_CHARACTER
      INTEGER*4 MPI_COMM_NULL
      INTEGER*4 MPI_COMM_WORLD
      INTEGER*4 MPI_COMPLEX
      INTEGER*4 MPI_DOUBLE_COMPLEX
      INTEGER*4 MPI_DOUBLE_PRECISION
      INTEGER*4 MPI_INTEGER
      INTEGER*4 MPI_LOGICAL
      INTEGER*4 MPI_MAX
      INTEGER*4 MPI_MAX_PROCESSOR_NAME
      INTEGER*4 MPI_MAXLOC
      INTEGER*4 MPI_MIN
      INTEGER*4 MPI_MINLOC
      INTEGER*4 MPI_PACKED
      INTEGER*4 MPI_PROD
      INTEGER*4 MPI_REAL
      INTEGER*4 MPI_REPLACE
      INTEGER*4 MPI_REQUEST_NULL
      INTEGER*4 MPI_SOURCE
      INTEGER*4 MPI_STATUS_SIZE
      INTEGER*4 MPI_SUM
      INTEGER*4 MPI_TAG
      INTEGER*4 MPI_UNDEFINED
      INTEGER*4 MPI_WTIME_IS_GLOBAL
      INTEGER*4 MPI_LOR
      INTEGER*4 MPI_LAND
      INTEGER*4 MPI_INTEGER8
      INTEGER*4 MPI_REAL8
      INTEGER*4 MPI_BSEND_OVERHEAD
      PARAMETER (MPI_2DOUBLE_PRECISION=2)
      PARAMETER (MPI_2INTEGER=0)
      PARAMETER (MPI_2REAL=1)
      PARAMETER (MPI_ANY_SOURCE=4)
      PARAMETER (MPI_ANY_TAG=5)
      PARAMETER (MPI_BYTE=6)
      PARAMETER (MPI_CHARACTER=4)
      PARAMETER (MPI_COMM_NULL=0)
      PARAMETER (MPI_COMM_WORLD=1)
      PARAMETER (MPI_COMPLEX=3)
      PARAMETER (MPI_DOUBLE_COMPLEX=5)
      PARAMETER (MPI_DOUBLE_PRECISION=2)
      PARAMETER (MPI_INTEGER=0)
      PARAMETER (MPI_LOGICAL=0)
      PARAMETER (MPI_MAX=15)
      PARAMETER (MPI_MAX_PROCESSOR_NAME=31)
      PARAMETER (MPI_MAXLOC=16)
      PARAMETER (MPI_MIN=17)
      PARAMETER (MPI_MINLOC=18)
      PARAMETER (MPI_PACKED=19)
      PARAMETER (MPI_PROD=20)
      PARAMETER (MPI_REAL=1)
      PARAMETER (MPI_REAL8=2)
      PARAMETER (MPI_REPLACE=22)
      PARAMETER (MPI_REQUEST_NULL=23)
      PARAMETER (MPI_SOURCE=2)
      PARAMETER (MPI_STATUS_SIZE=3)
      PARAMETER (MPI_SUM=0)
      PARAMETER (MPI_TAG=3)
      PARAMETER (MPI_UNDEFINED=-32766)
      PARAMETER (MPI_WTIME_IS_GLOBAL=30)
      PARAMETER (MPI_LOR=31)
      PARAMETER (MPI_LAND=32)
      PARAMETER (MPI_INTEGER8=2)
      PARAMETER (MPI_BSEND_OVERHEAD=0)
      DOUBLE PRECISION MPI_WTIME
      EXTERNAL MPI_WTIME

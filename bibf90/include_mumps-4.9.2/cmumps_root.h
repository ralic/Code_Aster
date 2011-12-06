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
      TYPE CMUMPS_ROOT_STRUC
        SEQUENCE
        INTEGER*4 MBLOCK, NBLOCK, NPROW, NPCOL
        INTEGER*4 MYROW, MYCOL
        INTEGER*4 ROOT_SIZE, TOT_ROOT_SIZE
        INTEGER*4 :: CNTXT_BLACS
        INTEGER*4, DIMENSION(:), POINTER :: RG2L_ROW
        INTEGER*4, DIMENSION(:), POINTER :: RG2L_COL
        INTEGER*4 , DIMENSION(:), POINTER :: IPIV
        INTEGER*4, DIMENSION( 9 ) :: DESCRIPTOR, DESCB
        LOGICAL*4 yes, gridinit_done
        INTEGER*4 LPIV
!       Used to access Schur easily from root structure
        COMPLEX*8, DIMENSION(:), POINTER :: SCHUR_POINTER
        INTEGER*4 SCHUR_MLOC, SCHUR_NLOC, SCHUR_LLD
!
!      Data for nullspace/QR
!
        COMPLEX*8, DIMENSION(:), POINTER :: QR_TAU
        REAL*4   QR_RCOND
!
!      Givens rotations
!
        INTEGER*4 MAXG, GIND
        COMPLEX*8, DIMENSION(:),POINTER::GROW, GCOS, GSIN
!
!      RRRLU data
!
        INTEGER*4 ELG_MAX,NULL_MAX
        INTEGER*4 ELIND,EUIND,NLUPDATE,NUUPDATE
        INTEGER*4,DIMENSION(:),POINTER::PERM_ROW,PERM_COL
        INTEGER*4,DIMENSION(:),POINTER::ELROW, EUROW, PTREL, PTREU
        COMPLEX*8, DIMENSION(:), POINTER :: ELELG, EUELG, DL
!
      END TYPE CMUMPS_ROOT_STRUC

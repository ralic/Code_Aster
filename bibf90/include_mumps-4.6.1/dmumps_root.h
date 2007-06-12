!
!   THIS FILE IS PART OF DMUMPS VERSION 4.6.1
!   This Version was built on Fri Feb 17 14:27:51 2006
!
!
!  This version of DMUMPS is provided to you free of charge. It is public
!  domain, based on public domain software developed during the Esprit IV
!  European project PARASOL (1996-1999) by CERFACS, ENSEEIHT-IRIT and RAL.
!  Since this first public domain version in 1999, the developments are
!  supported by the following institutions: CERFACS, ENSEEIHT-IRIT, and
!  INRIA.
!
!  Main contributors are Patrick Amestoy, Iain Duff, Abdou Guermouche,
!  Jacko Koster, Jean-Yves L'Excellent, and Stephane Pralet.
!
!  Up-to-date copies of the DMUMPS package can be obtained
!  from the Web pages http://www.enseeiht.fr/apo/DMUMPS/
!  or http://graal.ens-lyon.fr/DMUMPS
!
!
!   THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY
!   EXPRESSED OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
!
!
!  User documentation of any code that uses this software can
!  include this complete notice. You can acknowledge (using
!  references [1], [2], and [3] the contribution of this package
!  in any scientific publication dependent upon the use of the
!  package. You shall use reasonable endeavours to notify
!  the authors of the package of this publication.
!
!   [1] P. R. Amestoy, I. S. Duff and  J.-Y. L'Excellent (1998),
!   Multifrontal parallel distributed symmetric and unsymmetric solvers,
!   in Comput. Methods in Appl. Mech. Eng., 184,  501-520 (2000).
!
!   [2] P. R. Amestoy, I. S. Duff, J. Koster and  J.-Y. L'Excellent,
!   A fully asynchronous multifrontal solver using distributed dynamic
!   scheduling, SIAM Journal of Matrix Analysis and Applications,
!   Vol 23, No 1, pp 15-41 (2001).
!
!   [3] P. R. Amestoy and A. Guermouche and J.-Y. L'Excellent and
!   S. Pralet (2005), Hybrid scheduling for the parallel solution
!   of linear systems. Accepted to Parallel Computing.
!
!     $Id: dmumps_root.h,v 1.9 2005/04/18 10:58:44 jylexcel Exp $
      TYPE DMUMPS_ROOT_STRUC
        SEQUENCE
        INTEGER*4 MBLOCK, NBLOCK, NPROW, NPCOL
        INTEGER*4 MYROW, MYCOL
        INTEGER*4 ROOT_SIZE, TOT_ROOT_SIZE
        INTEGER*4 :: CNTXT_BLACS
        INTEGER*4, DIMENSION(:), POINTER :: RG2L_ROW
        INTEGER*4, DIMENSION(:), POINTER :: RG2L_COL
        INTEGER*4 , DIMENSION(:), POINTER :: IPIV
        INTEGER*4, DIMENSION( 9 ) :: DESCRIPTOR, DESCB
        LOGICAL yes, gridinit_done
        INTEGER*4 LPIV
!       Used to access Schur easily from root structure
        DOUBLE PRECISION, DIMENSION(:), POINTER :: SCHUR_POINTER
        INTEGER*4 SCHUR_MLOC, SCHUR_NLOC, SCHUR_LLD
!
!      Data for nullspace/QR
!
        DOUBLE PRECISION, DIMENSION(:), POINTER :: QR_TAU
!
!      Givens rotations
!
        INTEGER*4 MAXG, GIND
        DOUBLE PRECISION, DIMENSION(:),POINTER::GROW, GCOS, GSIN
!
!      RRRLU data
!
        INTEGER*4 ELG_MAX,NULL_MAX
        INTEGER*4 ELIND,EUIND,NLUPDATE,NUUPDATE
        INTEGER*4,DIMENSION(:),POINTER::PERM_ROW,PERM_COL
        INTEGER*4,DIMENSION(:),POINTER::ELROW, EUROW, PTREL, PTREU
        DOUBLE PRECISION, DIMENSION(:), POINTER :: ELELG, EUELG, DL
!
      END TYPE DMUMPS_ROOT_STRUC

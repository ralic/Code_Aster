!
!   THIS FILE IS PART OF DMUMPS VERSION 4.3.2
!   This Version was built on Wed Nov 12 16:57:09 2003
!
! COPYRIGHT (c) 1996-2003 P. R. Amestoy, I. S. Duff, J. Koster,
!                       J.-Y. L'Excellent
!
!  CERFACS      , Toulouse    (France)  (http://www.cerfacs.fr)
!  ENSEEIHT-IRIT, Toulouse    (France)  (http://www.enseeiht.fr)
!  INRIA                      (France)  (http://www.inria.fr)
!  PARALLAB     , Bergen      (Norway)  (http://www.parallab.uib.no)
!
! All rights reserved.
!
!  Your use or distribution of the package implies that you agree
!  with this License. Up-to-date copies of the DMUMPS package can be
!  obtained from the Web page http://www.enseeiht.fr/apo/DMUMPS/
!
!  This package is provided to you free of charge. It was
!  initially based on public domain software developed during
!  the European Esprit IV project PARASOL (1996-1999).
!
!  THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY
!  EXPRESSED OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
!
!  Permission is hereby granted to use or copy this
!  package provided that the Copyright and this License is
!  retained on all copies and that the package is used
!  under the same terms and conditions. User documentation
!  of any code that uses this software should include this
!  complete Copyright notice and this License.
!
!  You can modify this code but, at no time shall the right
!  or title to all or any part of this package pass to you.
!  All information relating to any alteration or addition
!  made to this package for the purposes of extending the
!  capabilities or enhancing the performance of this package
!  shall be made available free of charge to the authors for
!  any purpose.
!
!  You shall acknowledge (using references [1] and [2])
!  the contribution of this package in any publication
!  of material dependent upon the use of the package.
!  You shall use reasonable endeavours to notify
!  the authors of the package of this publication.
!
!
!  [1] P. R. Amestoy, I. S. Duff and  J.-Y. L'Excellent (1998),
!  Multifrontal parallel distributed symmetric and unsymmetric solvers,
!  in Comput. Methods in Appl. Mech. Eng., 184,  501-520 (2000).
!  An early version appeared as a Technical Report ENSEEIHT-IRIT (1998)
!  and is available at http://www.enseeiht.fr/apo/DMUMPS/.
!
!  [2] P. R. Amestoy, I. S. Duff, J. Koster and  J.-Y. L'Excellent,
!  A fully asynchronous multifrontal solver using distributed dynamic
!  scheduling, SIAM Journal of Matrix Analysis and Applications,
!  Vol 23, No 1, pp 15-41 (2001).
!  An  early version appeared as a Technical Report ENSEEIHT-IRIT,
!  RT/APO/99/2 (1999)  and is available at http://www.enseeiht.fr/apo/DMUMPS/.
!
!  None of the text from the Copyright notice up to and
!  including this line shall be removed or altered in any way.
!
!     $Id: dmumps_struc.h,v 1.30 2003/11/12 15:56:08 jylexcel Exp $
      INCLUDE 'dmumps_root.h'
      TYPE DMUMPS_STRUC
        SEQUENCE
!
! This structure contains all parameters 
! for the interface to the user, plus internal
! information
!
! *****************
! INPUT PARAMETERS
! *****************
!    ------------------
!    Problem definition
!    ------------------
!    Solver (SYM=0 unsymmetric,SYM=1 symmetric Positive Definite, 
!         SYM=2 general symmetric)
!    Type of parallelism (PAR=1 host working, PAR=0 host not working)
         INTEGER*4 SYM, PAR
         INTEGER*4 JOB 
!    --------------------
!    Order of Input matrix 
!    --------------------
         INTEGER*4 N
!
!    ----------------------------------------
!    Assembled input matrix : User interface
!    ----------------------------------------
         INTEGER*4 NZ
         DOUBLE PRECISION, DIMENSION(:), POINTER :: A
         INTEGER*4, DIMENSION(:), POINTER :: IRN, JCN
         DOUBLE PRECISION, DIMENSION(:), POINTER :: COLSCA, ROWSCA
!
!        ------------------------------------
!        Case of distributed assembled matrix
!        matrix on entry:
!        ------------------------------------
         INTEGER*4 NZ_loc
         INTEGER*4, DIMENSION(:), POINTER :: IRN_loc, JCN_loc
         DOUBLE PRECISION, DIMENSION(:), POINTER :: A_loc
!
!    ----------------------------------------
!    Unassembled input matrix: User interface
!    ----------------------------------------
         INTEGER*4 NELT
         INTEGER*4, DIMENSION(:), POINTER :: ELTPTR
         INTEGER*4, DIMENSION(:), POINTER :: ELTVAR
         DOUBLE PRECISION, DIMENSION(:), POINTER :: A_ELT
!
!    -----------------
!    MPI Communicator
!    -----------------
         INTEGER*4 COMM
!
!    ---------------------------------------------
!    Symmetric permutation : 
!               PERM_IN if given by user (optional)
!    ---------------------------------------------
         INTEGER*4, DIMENSION(:), POINTER :: PERM_IN
!
!
! ******************
! INPUT/OUTPUT data 
! ******************
!    --------------------------------------------------------
!    RHS 
!    ---
!       on input it holds the right hand side 
!       on ouput : always hold the assembled solution
!    -------------------------------------------------------
         DOUBLE PRECISION, DIMENSION(:), POINTER :: RHS
         INTEGER*4 PADDING
!    ----------------------------
!    Control parameters,
!    statistics and output data
!    ---------------------------
         INTEGER*4 ICNTL(40)
         INTEGER*4 INFO(40) 
         INTEGER*4 INFOG(40)
         DOUBLE PRECISION CNTL(5)
         DOUBLE PRECISION RINFO(20)
         DOUBLE PRECISION RINFOG(20)
!        Cost (flops) of subtrees on local process
         DOUBLE PRECISION COST_SUBTREES
!    ---------------------------------------------------------
!    Permutations computed during analysis:
!        SYM_PERM: Symmetric permutation 
!        UNS_PERM: Column permutations (optionnal)
!    ---------------------------------------------------------
         INTEGER*4, DIMENSION(:), POINTER :: SYM_PERM, UNS_PERM
! 
!    -------------------------------------
!    Case of distributed matrix on entry:
!    DMUMPS potentially provides mapping
!    -------------------------------------
         INTEGER*4, DIMENSION(:), POINTER :: MAPPING
!
!    -------------------------------
!    Deficiency and null space basis
!    -------------------------------
         INTEGER*4 Deficiency
         DOUBLE PRECISION, DIMENSION(:,:), POINTER :: NULL_SPACE
!    -----
!    Schur
!    -----
         INTEGER*4 SIZE_SCHUR
         INTEGER*4, DIMENSION(:), POINTER :: LISTVAR_SCHUR
         DOUBLE PRECISION, DIMENSION(:), POINTER :: SCHUR
!
!
! **********************
! INTERNAL Working data
! **********************
!        For MPI
         INTEGER*4 COMM_NODES, MYID_NODES, COMM_LOAD
         INTEGER*4  MYID, NPROCS, NSLAVES
         INTEGER*4 ASS_IRECV
         INTEGER*4, DIMENSION(:), POINTER :: POIDS
         INTEGER*4 LBUFR
         INTEGER*4 LBUFR_BYTES
         INTEGER*4, DIMENSION(:), POINTER ::  BUFR
         INTEGER*4 INST_Number
!        for anlaysis/facto/solve phases
         INTEGER*4 MAXIS, MAXS
         INTEGER*4 MAXIS1
         INTEGER*4 KEEP(150)
!        IS is used for the factors + workspace for contrib. blocks
         INTEGER*4, DIMENSION(:), POINTER :: IS
!        is1 (maxis1) contains working arrays computed 
!        and used only during analysis
         INTEGER*4, DIMENSION(:), POINTER :: IS1
!        The following data/arrays are computed during the analysis
!        phase and used during the factorization and solve phases.
         INTEGER*4 LNA
         INTEGER*4,POINTER,DIMENSION(:)::STEP, NE_STEPS, ND_STEPS
         INTEGER*4,POINTER,DIMENSION(:)::FRERE_STEPS
         INTEGER*4,POINTER,DIMENSION(:)::FILS, PTRAR, FRTPTR, FRTELT
         INTEGER*4,POINTER,DIMENSION(:)::NA, PROCNODE_STEPS
!        The only array computed in facto and used by the solve
!           (except the factors) is PTLUST_S. 
         INTEGER*4, DIMENSION(:), POINTER :: PTLUST_S
!        main real working arrays for factorization/solve phases
         DOUBLE PRECISION, DIMENSION(:), POINTER :: S
!        Information on mapping
         INTEGER*4, DIMENSION(:), POINTER :: PROCNODE
         INTEGER*4 nbsa
!        Input matrix ready for numerical assembly 
!            -arrowhead format in case of assembled matrix
!            -element format otherwise
         INTEGER*4, DIMENSION(:), POINTER :: INTARR
         DOUBLE PRECISION, DIMENSION(:), POINTER :: DBLARR
!         Element entry: internal data
         INTEGER*4 NELT_LOC, LELTVAR, NA_ELT
         INTEGER*4, DIMENSION(:), POINTER :: ELTPROC
!         Candidates and node partitionning
         INTEGER*4, DIMENSION(:,:), POINTER :: CANDIDATES
         INTEGER*4, DIMENSION(:),   POINTER :: ISTEP_TO_INIV2
         INTEGER*4, DIMENSION(:),   POINTER :: FUTURE_NIV2
         INTEGER*4, DIMENSION(:,:), POINTER :: TAB_POS_IN_PERE 
!        For heterogeneous architecture
         INTEGER*4, DIMENSION(:), POINTER :: MEM_DIST
!   ------------------------
!   Root structure(internal)
!   ------------------------
         TYPE (DMUMPS_ROOT_STRUC) :: root
!        For C interface
!   Instance number used/managed by the C/F77 interface
         INTEGER*4 INSTANCE_NUMBER
!   Info on the subtrees to be used during facto
         INTEGER*4, DIMENSION(:),   POINTER :: MEM_SUBTREE
         INTEGER*4 NBSA_LOCAL
      END TYPE DMUMPS_STRUC

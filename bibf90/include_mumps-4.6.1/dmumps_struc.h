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
!     $Id: dmumps_struc.h,v 1.60 2005/12/08 11:16:42 jylexcel Exp $
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
!    RHS / SOL_LOC
!    -------------
!       right-hand side and solution
!    -------------------------------------------------------
         DOUBLE PRECISION, DIMENSION(:), POINTER :: RHS
         DOUBLE PRECISION, DIMENSION(:), POINTER :: RHS_SPARSE
         DOUBLE PRECISION, DIMENSION(:), POINTER :: SOL_LOC
         INTEGER*4, DIMENSION(:), POINTER :: IRHS_SPARSE
         INTEGER*4, DIMENSION(:), POINTER :: IRHS_PTR
         INTEGER*4, DIMENSION(:), POINTER :: ISOL_LOC
         INTEGER*4 LRHS, NRHS, NZ_RHS, LSOL_LOC
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
         INTEGER*4 NPROW, NPCOL, MBLOCK, NBLOCK
	 INTEGER*4 SCHUR_MLOC, SCHUR_NLOC, SCHUR_LLD
         INTEGER*4 SIZE_SCHUR
         INTEGER*4, DIMENSION(:), POINTER :: LISTVAR_SCHUR
         DOUBLE PRECISION, DIMENSION(:), POINTER :: SCHUR
         DOUBLE PRECISION, DIMENSION(:), POINTER :: SCHUR_CINTERFACE
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
!        for analysis/facto/solve phases
         INTEGER*4 MAXIS, MAXS
         INTEGER*4 MAXIS1
         INTEGER*4 KEEP(500)
	 INTEGER*8 KEEP8(150)
!        IS is used for the factors + workspace for contrib. blocks
         INTEGER*4, DIMENSION(:), POINTER :: IS
!        is1 (maxis1) contains working arrays computed
!        and used only during analysis
         INTEGER*4, DIMENSION(:), POINTER :: IS1
!        The following data/arrays are computed during the analysis
!        phase and used during the factorization and solve phases.
         INTEGER*4 LNA
         INTEGER*4,POINTER,DIMENSION(:)::STEP, NE_STEPS, ND_STEPS
         INTEGER*4,POINTER,DIMENSION(:)::FRERE_STEPS, DAD_STEPS
         INTEGER*4,POINTER,DIMENSION(:)::FILS, PTRAR, FRTPTR, FRTELT
         INTEGER*4,POINTER,DIMENSION(:)::NA, PROCNODE_STEPS
!        The two pointer arrays computed in facto and used by the solve
!           (except the factors) are PTLUST_S and PTRFAC.
         INTEGER*4, DIMENSION(:), POINTER :: PTLUST_S, PTRFAC
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
!        To save the matrix in a simple format
         CHARACTER(LEN=80) WRITE_PROBLEM
!   ------------------------
!   Root structure(internal)
!   ------------------------
         TYPE (DMUMPS_ROOT_STRUC) :: root
!        For C interface
!   Instance number used/managed by the C/F77 interface
         INTEGER*4 INSTANCE_NUMBER
!   Info on the subtrees to be used during facto
         INTEGER*4, DIMENSION(:),   POINTER :: MEM_SUBTREE
         INTEGER*4, DIMENSION(:),   POINTER :: MY_ROOT_SBTR
         INTEGER*4, DIMENSION(:),   POINTER :: MY_FIRST_LEAF
         INTEGER*4, DIMENSION(:),   POINTER :: MY_NB_LEAF
         INTEGER*4, DIMENSION(:),   POINTER :: DEPTH_FIRST
         DOUBLE PRECISION, DIMENSION(:),   POINTER :: COST_TRAV
         INTEGER*4 NBSA_LOCAL
         INTEGER*4 MEMORY_MD
         INTEGER*4 MAX_SURF_MASTER
!    For simulating parallel out-of-core stack.
         INTEGER*4, DIMENSION(:),POINTER ::CB_SON_SIZE
!    OOC management data that must persist from factorization to solve.
         INTEGER*4, DIMENSION(:),   POINTER :: OOC_INODE_SEQUENCE
         INTEGER*4, DIMENSION(:),   POINTER :: OOC_NUM_FILE
         INTEGER*4, DIMENSION(:),   POINTER :: OOC_POS_IN_FILE
         INTEGER*4, DIMENSION(:),   POINTER :: OOC_SIZE_OF_BLOCK
         INTEGER*4 OOC_MAX_NB_NODES_FOR_ZONE,OOC_TOTAL_NB_NODES
         INTEGER*4 OOC_NB_FILES
         CHARACTER,DIMENSION(:,:), POINTER :: OOC_FILE_NAMES
         INTEGER*4,DIMENSION(:), POINTER :: OOC_FILE_NAME_LENGTH
         CHARACTER(LEN=150) DMUMPS_OOC_TMPDIR,DMUMPS_OOC_PREFIX
!    Other internal data
         LOGICAL PB_ALLOC_ON_EXIT
      END TYPE DMUMPS_STRUC

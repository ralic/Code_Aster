!
!  This file is part of MUMPS 4.10.0, built on Tue May 10 12:56:32 UTC 2011
!
!
!  This version of MUMPS is provided to you free of charge. It is public
!  domain, based on public domain software developed during the Esprit IV
!  European project PARASOL (1996-1999). Since this first public domain
!  version in 1999, research and developments have been supported by the
!  following institutions: CERFACS, CNRS, ENS Lyon, INPT(ENSEEIHT)-IRIT,
!  INRIA, and University of Bordeaux.
!
!  The MUMPS team at the moment of releasing this version includes
!  Patrick Amestoy, Maurice Bremond, Alfredo Buttari, Abdou Guermouche,
!  Guillaume Joslin, Jean-Yves L'Excellent, Francois-Henry Rouet, Bora
!  Ucar and Clement Weisbecker.
!
!  We are also grateful to Emmanuel Agullo, Caroline Bousquet, Indranil
!  Chowdhury, Philippe Combes, Christophe Daniel, Iain Duff, Vincent Espirat,
!  Aurelia Fevre, Jacko Koster, Stephane Pralet, Chiara Puglisi, Gregoire
!  Richard, Tzvetomila Slavova, Miroslav Tuma and Christophe Voemel who
!  have been contributing to this project.
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
! aslint: disable=C1302
!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
      INCLUDE 'smumps_root.h'
      TYPE SMUMPS_STRUC
        SEQUENCE
!
! This structure contains all parameters 
! for the interface to the user, plus internal
! information from the solver
!
! *****************
! INPUT PARAMETERS
! *****************
!    -----------------
!    MPI Communicator
!    -----------------
        INTEGER*4 COMM
!    ------------------
!    Problem definition
!    ------------------
!    Solver (SYM=0 unsymmetric,SYM=1 symmetric Positive Definite, 
!        SYM=2 general symmetric)
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
        REAL*4, DIMENSION(:), POINTER :: A
        INTEGER*4, DIMENSION(:), POINTER :: IRN, JCN
        REAL*4, DIMENSION(:), POINTER :: COLSCA, ROWSCA, pad0
!
!       ------------------------------------
!       Case of distributed assembled matrix
!       matrix on entry:
!       ------------------------------------
        INTEGER*4 NZ_loc, pad1
        INTEGER*4, DIMENSION(:), POINTER :: IRN_loc, JCN_loc
        REAL*4, DIMENSION(:), POINTER :: A_loc, pad2
!
!    ----------------------------------------
!    Unassembled input matrix: User interface
!    ----------------------------------------
        INTEGER*4 NELT, pad3
        INTEGER*4, DIMENSION(:), POINTER :: ELTPTR
        INTEGER*4, DIMENSION(:), POINTER :: ELTVAR
        REAL*4, DIMENSION(:), POINTER :: A_ELT, pad4
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
!    RHS / SOL_loc
!    -------------
!       right-hand side and solution
!    -------------------------------------------------------
        REAL*4, DIMENSION(:), POINTER :: RHS, REDRHS
        REAL*4, DIMENSION(:), POINTER :: RHS_SPARSE
        REAL*4, DIMENSION(:), POINTER :: SOL_loc
        INTEGER*4, DIMENSION(:), POINTER :: IRHS_SPARSE
        INTEGER*4, DIMENSION(:), POINTER :: IRHS_PTR
        INTEGER*4, DIMENSION(:), POINTER :: ISOL_loc
        INTEGER*4 LRHS, NRHS, NZ_RHS, LSOL_loc, LREDRHS
        INTEGER*4 pad5
!    ----------------------------
!    Control parameters,
!    statistics and output data
!    ---------------------------
        INTEGER*4 ICNTL(40)
        INTEGER*4 INFO(40) 
        INTEGER*4 INFOG(40)
        REAL*4 COST_SUBTREES
        REAL*4 CNTL(15)
        REAL*4 RINFO(40)
        REAL*4 RINFOG(40)
!    ---------------------------------------------------------
!    Permutations computed during analysis:
!       SYM_PERM: Symmetric permutation 
!       UNS_PERM: Column permutations (optionnal)
!    ---------------------------------------------------------
        INTEGER*4, DIMENSION(:), POINTER :: SYM_PERM, UNS_PERM
! 
!    -----
!    Schur
!    -----
        INTEGER*4 NPROW, NPCOL, MBLOCK, NBLOCK
        INTEGER*4 SCHUR_MLOC, SCHUR_NLOC, SCHUR_LLD
        INTEGER*4 SIZE_SCHUR
        REAL*4, DIMENSION(:), POINTER :: SCHUR
        REAL*4, DIMENSION(:), POINTER :: SCHUR_CINTERFACE
        INTEGER*4, DIMENSION(:), POINTER :: LISTVAR_SCHUR
!    -------------------------------------
!    Case of distributed matrix on entry:
!    SMUMPS potentially provides mapping
!    -------------------------------------
        INTEGER*4, DIMENSION(:), POINTER :: MAPPING
!    --------------
!    Version number
!    --------------
        CHARACTER(LEN=14) VERSION_NUMBER
!    -----------
!    Out-of-core
!    -----------
        CHARACTER(LEN=255) :: OOC_TMPDIR
        CHARACTER(LEN=63) :: OOC_PREFIX
!    ------------------------------------------
!    To save the matrix in matrix market format
!    ------------------------------------------
        CHARACTER(LEN=255) WRITE_PROBLEM
        CHARACTER(LEN=5) :: pad8
!
!
! **********************
! INTERNAL Working data
! *********************
        INTEGER(8) :: KEEP8(150), MAX_SURF_MASTER
        INTEGER*4 INST_Number
!       For MPI
        INTEGER*4 COMM_NODES, MYID_NODES, COMM_LOAD
        INTEGER*4  MYID, NPROCS, NSLAVES
        INTEGER*4 ASS_IRECV
        INTEGER*4 LBUFR
        INTEGER*4 LBUFR_BYTES
        INTEGER*4, DIMENSION(:), POINTER :: POIDS
        INTEGER*4, DIMENSION(:), POINTER ::  BUFR
!       IS is used for the factors + workspace for contrib. blocks
        INTEGER*4, DIMENSION(:), POINTER :: IS
!       IS1 (maxis1) contains working arrays computed 
!       and used only during analysis
        INTEGER*4, DIMENSION(:), POINTER :: IS1
!       For analysis/facto/solve phases
        INTEGER*4 MAXIS1, Deficiency
        INTEGER*4 KEEP(500)
!       The following data/arrays are computed during the analysis
!       phase and used during the factorization and solve phases.
        INTEGER*4 LNA
        INTEGER*4 NBSA
        INTEGER*4,POINTER,DIMENSION(:)::STEP, NE_STEPS, ND_STEPS
!  Info for pruning tree 
        INTEGER*4,POINTER,DIMENSION(:)::Step2node
!  ---------------------
        INTEGER*4,POINTER,DIMENSION(:)::FRERE_STEPS, DAD_STEPS
        INTEGER*4,POINTER,DIMENSION(:)::FILS, PTRAR, FRTPTR, FRTELT
        INTEGER*4,POINTER,DIMENSION(:)::NA, PROCNODE_STEPS
!       The two pointer arrays computed in facto and used by the solve
!          (except the factors) are PTLUST_S and PTRFAC. 
        INTEGER*4, DIMENSION(:), POINTER :: PTLUST_S
        INTEGER(8), DIMENSION(:), POINTER :: PTRFAC
!       main REAL*4 working arrays for factorization/solve phases
        REAL*4, DIMENSION(:), POINTER :: S
!       Information on mapping
        INTEGER*4, DIMENSION(:), POINTER :: PROCNODE
!       Input matrix ready for numerical assembly 
!           -arrowhead format in case of assembled matrix
!           -element format otherwise
        INTEGER*4, DIMENSION(:), POINTER :: INTARR
        REAL*4, DIMENSION(:), POINTER :: DBLARR
!       Element entry: internal data
        INTEGER*4 NELT_loc, LELTVAR, NA_ELT, pad11
        INTEGER*4, DIMENSION(:), POINTER :: ELTPROC
!       Candidates and node partitionning
        INTEGER*4, DIMENSION(:,:), POINTER :: CANDIDATES
        INTEGER*4, DIMENSION(:),   POINTER :: ISTEP_TO_INIV2
        INTEGER*4, DIMENSION(:),   POINTER :: FUTURE_NIV2
        INTEGER*4, DIMENSION(:,:), POINTER :: TAB_POS_IN_PERE 
        LOGICAL*4, DIMENSION(:),   POINTER :: I_AM_CAND
!       For heterogeneous architecture
        INTEGER*4, DIMENSION(:), POINTER :: MEM_DIST
!       Compressed RHS
        INTEGER*4, DIMENSION(:),   POINTER :: POSINRHSCOMP
        REAL*4, DIMENSION(:),   POINTER :: RHSCOMP
!       Info on the subtrees to be used during factorization
        DOUBLE PRECISION, DIMENSION(:), POINTER :: MEM_SUBTREE
        DOUBLE PRECISION, DIMENSION(:), POINTER :: COST_TRAV
        INTEGER*4, DIMENSION(:),   POINTER :: MY_ROOT_SBTR
        INTEGER*4, DIMENSION(:),   POINTER :: MY_FIRST_LEAF
        INTEGER*4, DIMENSION(:),   POINTER :: MY_NB_LEAF
        INTEGER*4, DIMENSION(:),   POINTER :: DEPTH_FIRST
        INTEGER*4, DIMENSION(:),   POINTER :: DEPTH_FIRST_SEQ
        INTEGER*4, DIMENSION(:),   POINTER :: SBTR_ID
        REAL*4, DIMENSION(:), POINTER :: WK_USER
        INTEGER*4 :: NBSA_LOCAL
        INTEGER*4 :: LWK_USER
!    Internal control array
        REAL*4 DKEEP(30)
!    For simulating parallel out-of-core stack.
        DOUBLE PRECISION, DIMENSION(:),POINTER ::CB_SON_SIZE, pad12
!    Instance number used/managed by the C/F77 interface
        INTEGER*4 INSTANCE_NUMBER
!    OOC management data that must persist from factorization to solve.
        INTEGER*4 OOC_MAX_NB_NODES_FOR_ZONE
        INTEGER*4, DIMENSION(:,:),  POINTER :: OOC_INODE_SEQUENCE, pad13
        INTEGER(8),DIMENSION(:,:), POINTER :: OOC_SIZE_OF_BLOCK
        INTEGER(8), DIMENSION(:,:),   POINTER :: OOC_VADDR
        INTEGER*4,DIMENSION(:), POINTER :: OOC_TOTAL_NB_NODES
        INTEGER*4,DIMENSION(:), POINTER :: OOC_NB_FILES
        CHARACTER,DIMENSION(:,:), POINTER :: OOC_FILE_NAMES  
        INTEGER*4,DIMENSION(:), POINTER :: OOC_FILE_NAME_LENGTH
!    Indices of nul pivots
        INTEGER*4,DIMENSION(:), POINTER :: PIVNUL_LIST
!    Array needed to manage additionnal candidate processor 
        INTEGER*4, DIMENSION(:,:), POINTER :: SUP_PROC, pad14
!   ------------------------
!   Root structure(internal)
!   ------------------------
        TYPE (SMUMPS_ROOT_STRUC) :: root
      END TYPE SMUMPS_STRUC

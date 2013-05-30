!
! COPYRIGHT (C) 2007 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
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
interface
    subroutine dmumps(cmpsk)
        type DMUMPS_ROOT_STRUC
        sequence
        integer(kind=4) :: mblock
        integer(kind=4) :: nblock
        integer(kind=4) :: nprow
        integer(kind=4) :: npcol
        integer(kind=4) :: myrow
        integer(kind=4) :: mycol
        integer(kind=4) :: SCHUR_MLOC
        integer(kind=4) :: SCHUR_NLOC
        integer(kind=4) :: SCHUR_LLD
        integer(kind=4) :: RHS_NLOC
        integer(kind=4) :: ROOT_SIZE
        integer(kind=4) :: TOT_ROOT_SIZE
        integer(kind=4) :: DESCRIPTOR(9)
        integer(kind=4) :: CNTXT_BLACS
        integer(kind=4) :: lpiv
        integer(kind=4) :: ROOTPAD0
        integer(kind=4) :: pointer :: RG2L_ROW(:)
        integer(kind=4) :: pointer :: RG2L_COL(:)
        integer(kind=4) :: pointer :: ipiv(:)
        integer(kind=4) :: pointer :: ROOTPAD1(:)
        real(kind=8) :: pointer :: RHS_CNTR_MASTER_ROOT(:)
        real(kind=8) :: pointer :: SCHUR_POINTER(:)
        real(kind=8) :: pointer :: qr_tau(:)
        real(kind=8) :: pointer :: ROOTPAD2(:)
        real(kind=8) :: pointer :: RHS_ROOT(:, :)
        real(kind=8) :: pointer :: ROOTPAD(:, :)
        real(kind=8) :: QR_RCOND
        real(kind=8) :: ROOTPAD3
        logical(kind=4) :: yes
        logical(kind=4) :: GRIDINIT_DONE
        end type DMUMPS_ROOT_STRUC
        type DMUMPS_STRUC
        sequence
        integer(kind=4) :: comm
        integer(kind=4) :: sym
        integer(kind=4) :: par
        integer(kind=4) :: job
        integer(kind=4) :: n
        integer(kind=4) :: nz
        real(kind=8) :: pointer :: a(:)
        integer(kind=4) :: pointer :: irn(:)
        integer(kind=4) :: pointer :: jcn(:)
        real(kind=8) :: pointer :: colsca(:)
        real(kind=8) :: pointer :: rowsca(:)
        real(kind=8) :: pointer :: pad0(:)
        integer(kind=4) :: nz_loc
        integer(kind=4) :: pad1
        integer(kind=4) :: pointer :: IRN_LOC(:)
        integer(kind=4) :: pointer :: JCN_LOC(:)
        real(kind=8) :: pointer :: a_loc(:)
        real(kind=8) :: pointer :: pad2(:)
        integer(kind=4) :: nelt
        integer(kind=4) :: pad3
        integer(kind=4) :: pointer :: eltptr(:)
        integer(kind=4) :: pointer :: eltvar(:)
        real(kind=8) :: pointer :: a_elt(:)
        real(kind=8) :: pointer :: pad4(:)
        integer(kind=4) :: pointer :: PERM_IN(:)
        real(kind=8) :: pointer :: rhs(:)
        real(kind=8) :: pointer :: redrhs(:)
        real(kind=8) :: pointer :: RHS_SPARSE(:)
        real(kind=8) :: pointer :: SOL_LOC(:)
        integer(kind=4) :: pointer :: IRHS_SPARSE(:)
        integer(kind=4) :: pointer :: IRHS_PTR(:)
        integer(kind=4) :: pointer :: ISOL_LOC(:)
        integer(kind=4) :: lrhs
        integer(kind=4) :: nrhs
        integer(kind=4) :: nz_rhs
        integer(kind=4) :: LSOL_LOC
        integer(kind=4) :: LREDRHS
        integer(kind=4) :: pad5
        integer(kind=4) :: icntl(40)
        integer(kind=4) :: info(40)
        integer(kind=4) :: infog(40)
        real(kind=8) :: COST_SUBTREES
        real(kind=8) :: cntl(15)
        real(kind=8) :: rinfo(40)
        real(kind=8) :: rinfog(40)
        integer(kind=4) :: pointer :: SYM_PERM(:)
        integer(kind=4) :: pointer :: UNS_PERM(:)
        integer(kind=4) :: nprow
        integer(kind=4) :: npcol
        integer(kind=4) :: mblock
        integer(kind=4) :: nblock
        integer(kind=4) :: SCHUR_MLOC
        integer(kind=4) :: SCHUR_NLOC
        integer(kind=4) :: SCHUR_LLD
        integer(kind=4) :: SIZE_SCHUR
        real(kind=8) :: pointer :: schur(:)
        real(kind=8) :: pointer :: SCHUR_CINTERFACE(:)
        integer(kind=4) :: pointer :: LISTVAR_SCHUR(:)
        integer(kind=4) :: pointer :: MAPPING(:)
        character(len=14) :: VERSION_NUMBER
        character(len=255) :: OOC_TMPDIR
        character(len=63) :: OOC_PREFIX
        character(len=255) :: WRITE_PROBLEM
        character(len=5) :: pad8
        integer :: keep8(150)
        integer :: MAX_SURF_MASTER
        integer(kind=4) :: INST_NUMBER
        integer(kind=4) :: COMM_NODES
        integer(kind=4) :: MYID_NODES
        integer(kind=4) :: COMM_LOAD
        integer(kind=4) :: myid
        integer(kind=4) :: nprocs
        integer(kind=4) :: NSLAVES
        integer(kind=4) :: ASS_IRECV
        integer(kind=4) :: lbufr
        integer(kind=4) :: LBUFR_BYTES
        integer(kind=4) :: pointer :: poids(:)
        integer(kind=4) :: pointer :: bufr(:)
        integer(kind=4) :: pointer :: is(:)
        integer(kind=4) :: pointer :: is1(:)
        integer(kind=4) :: maxis1
        integer(kind=4) :: DEFICIENCY
        integer(kind=4) :: keep(500)
        integer(kind=4) :: lna
        integer(kind=4) :: nbsa
        integer(kind=4) :: pointer :: step(:)
        integer(kind=4) :: pointer :: NE_STEPS(:)
        integer(kind=4) :: pointer :: ND_STEPS(:)
        integer(kind=4) :: pointer :: STEP2NODE(:)
        integer(kind=4) :: pointer :: FRERE_STEPS(:)
        integer(kind=4) :: pointer :: DAD_STEPS(:)
        integer(kind=4) :: pointer :: fils(:)
        integer(kind=4) :: pointer :: ptrar(:)
        integer(kind=4) :: pointer :: frtptr(:)
        integer(kind=4) :: pointer :: frtelt(:)
        integer(kind=4) :: pointer :: na(:)
        integer(kind=4) :: pointer :: PROCNODE_STEPS(:)
        integer(kind=4) :: pointer :: PTLUST_S(:)
        integer,pointer :: ptrfac(:)
        real(kind=8) :: pointer :: s(:)
        integer(kind=4) :: pointer :: PROCNODE(:)
        integer(kind=4) :: pointer :: intarr(:)
        real(kind=8) :: pointer :: dblarr(:)
        integer(kind=4) :: NELT_LOC
        integer(kind=4) :: LELTVAR
        integer(kind=4) :: na_elt
        integer(kind=4) :: pad11
        integer(kind=4) :: pointer :: ELTPROC(:)
        integer(kind=4) :: pointer :: CANDIDATES(:, :)
        integer(kind=4) :: pointer :: ISTEP_TO_INIV2(:)
        integer(kind=4) :: pointer :: FUTURE_NIV2(:)
        integer(kind=4) :: pointer :: TAB_POS_IN_PERE(:, :)
        logical(kind=4) :: pointer :: I_AM_CAND(:)
        integer(kind=4) :: pointer :: MEM_DIST(:)
        integer(kind=4) :: pointer :: POSINRHSCOMP(:)
        real(kind=8) :: pointer :: RHSCOMP(:)
        real(kind=8) :: pointer :: MEM_SUBTREE(:)
        real(kind=8) :: pointer :: COST_TRAV(:)
        integer(kind=4) :: pointer :: MY_ROOT_SBTR(:)
        integer(kind=4) :: pointer :: MY_FIRST_LEAF(:)
        integer(kind=4) :: pointer :: MY_NB_LEAF(:)
        integer(kind=4) :: pointer :: DEPTH_FIRST(:)
        integer(kind=4) :: pointer :: DEPTH_FIRST_SEQ(:)
        integer(kind=4) :: pointer :: SBTR_ID(:)
        real(kind=8) :: pointer :: WK_USER(:)
        integer(kind=4) :: NBSA_LOCAL
        integer(kind=4) :: LWK_USER
        real(kind=8) :: dkeep(30)
        real(kind=8) :: pointer :: CB_SON_SIZE(:)
        real(kind=8) :: pointer :: pad12(:)
        integer(kind=4) :: INSTANCE_NUMBER
        integer(kind=4) :: OOC_MAX_NB_NODES_FOR_ZONE
        integer(kind=4) :: pointer :: OOC_INODE_SEQUENCE(:, :)
        integer(kind=4) :: pointer :: pad13(:, :)
        integer,pointer :: OOC_SIZE_OF_BLOCK(:, :)
        integer,pointer :: OOC_VADDR(:, :)
        integer(kind=4) :: pointer :: OOC_TOTAL_NB_NODES(:)
        integer(kind=4) :: pointer :: OOC_NB_FILES(:)
        character(len=1) :: pointer :: OOC_FILE_NAMES(:, :)
        integer(kind=4) :: pointer :: OOC_FILE_NAME_LENGTH(:)
        integer(kind=4) :: pointer :: PIVNUL_LIST(:)
        integer(kind=4) :: pointer :: SUP_PROC(:, :)
        integer(kind=4) :: pointer :: pad14(:, :)
        type (DMUMPS_ROOT_STRUC) :: root
        end type DMUMPS_STRUC
        type (DMUMPS_STRUC) :: cmpsk
    end subroutine dmumps
end interface

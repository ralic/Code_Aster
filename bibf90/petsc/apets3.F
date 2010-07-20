      SUBROUTINE APETS3(ACTION,KPTSC)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF APETS3 PETSC  DATE 19/07/2010   AUTEUR TARDIEU N.TARDIEU 
! ================================================================== 
! COPYRIGHT (C) 1991 - 2008  EDF R&D              WWW.CODE-ASTER.ORG 
!                                                                    
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR      
! MODIFY IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS     
! PUBLISHED BY THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE 
! LICENSE, OR (AT YOUR OPTION) ANY LATER VERSION.                    
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL,    
! BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF     
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU   
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.                           
!                                                                    
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE  
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO : EDF R&D CODE_ASTER,    
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.     
! ================================================================== 
      CHARACTER*(*) ACTION
      INTEGER      KPTSC
!--------------------------------------------------------------
!
!---------------------------------------------------------------

#ifdef _HAVE_PETSC

!================================================================
#include "include/finclude/petsc.h"
#include "include/finclude/petscvec.h"
#include "include/finclude/petscmat.h"
#include "include/finclude/petscpc.h"
#include "include/finclude/petscksp.h"
!================================================================
! --- DECLARATIONS NORMALISEES JEVEUX ----------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
!================================================================
! DECLARATIONS DE VARIABLES
!
      INTEGER NMXINS
      PARAMETER (NMXINS=5)

      INTEGER JSLVK,JSLVR,JSLVI
      INTEGER NIREMP,NSMDI

      CHARACTER*24 PRECON,SOLVBD
      CHARACTER*19 NOMAT,NOSOLV
      CHARACTER*14 NONU
      CHARACTER*4  KBID

      REAL*8 FILLIN

!     -- ON STOCKE AU PLUS NMXINS INSTANCES DE PETSC
!     -- PLUS PRECISEMENT LES MATRICES, SOLVEURS, ETC
      CHARACTER*19 NOMATS(NMXINS),NOSOLS(NMXINS)
      CHARACTER*14 NONUS(NMXINS)
      Mat      Ap(NMXINS)
      KSP      Kp(NMXINS)
      COMMON /SPETSC/ Ap,Kp,NOMATS,NOSOLS,NONUS
      
      
!      -- SUBROUTINES EXTERNES POUR L'UTILISATION DU 
!      -- PRECONDITIONNEUR SIMPLE PRECISION LDLT_SP 
       external LDSP1
       external LDSP2
!     -- COMMON POUR L'UTILISATION DU PRECONDITIONNEUR
!     -- SIMPLE PRECISION LDLT_SP
      COMMON       /LDLTSP/xtemp,SOLVBD,NOMAT,NOSOLV
      
!=================================================================
!  Variables PETSc
!
!  A       - matrix that defines linear system
!  ksp    - KSP context
!  x, b - approx solution, RHS vectors
!
      PetscMPIInt rang,size,ier
      PetscInt fill,ierr,NEQ
      PetscReal fillp
      Vec xtemp
      Mat A
      KSP ksp,kspp
      PC  pc,pcp
!=================================================================
      CALL JEMARQ()

!     -- ON RECUPERE DANS LE COMMON L'INSTANCE COURANTE
      NOMAT=NOMATS(KPTSC)
      NOSOLV=NOSOLS(KPTSC)
      NONU=NONUS(KPTSC)
      A=Ap(KPTSC)
      ksp=Kp(KPTSC)

      CALL JEVEUO(NOSOLV//'.SLVK','L',JSLVK)
      CALL JEVEUO(NOSOLV//'.SLVR','L',JSLVR)
      CALL JEVEUO(NOSOLV//'.SLVI','L',JSLVI)
      PRECON=ZK24(JSLVK-1+2)
      FILLIN=ZR(JSLVR-1+3)
      NIREMP=ZI(JSLVI-1+3)

      fill=NIREMP
      fillp=FILLIN

!     ----- RECUPERE LE RANG DU PROCESSUS
      CALL MPI_COMM_RANK(PETSC_COMM_WORLD, rang, ier)
      CALL MPI_COMM_SIZE(PETSC_COMM_WORLD, size, ier)
      
      IF (size.GT.1) THEN
         IF (ACTION.NE.'RESOUD') GOTO 999
! EN PARALLELE, ON NE PREPARE PAS LE PRECONDITIONNEUR TT DE SUITE
! CAR ON NE VEUT PAS ETRE OBLIGE D'APPELER KSPSetUp
         kspp=ksp
         call KSPGetPC(kspp,pcp,ierr)
         call PCSetType(pcp,PCBJACOBI,ierr)
         call KSPSetUp(kspp,ierr)
         call PCBJacobiGetSubKSP(pcp,PETSC_NULL_INTEGER,
     &                               PETSC_NULL_INTEGER,ksp,ierr)
      ELSE
! size.LE.1 i.e. SEQUENTIEL
         IF (ACTION.NE.'PRERES') GOTO 999
      END IF

!     -- choix du preconditionneur :
!     -------------------------------
! ("LDLT_INC","JACOBI","SOR"),defaut="LDLT_INC" ),
      call KSPGetPC(ksp,pc,ierr)
      IF (PRECON.eq.'LDLT_INC') THEN
         call PCSetType(pc,PCILU,ierr)
         call PCFactorSetLevels(pc,fill,ierr)
         call PCFactorSetFill(pc,fillp,ierr)
         call PCFactorSetMatOrderingType(pc,MATORDERING_NATURAL,ierr)
      ELSEIF (PRECON.eq.'JACOBI') THEN
         call PCSetType(pc,PCJACOBI,ierr)
      ELSEIF (PRECON.eq.'SOR') THEN
         call PCSetType(pc,PCSOR,ierr)
      ELSEIF (PRECON.eq.'LDLT_SP') THEN
        call PCSetType(pc,PCSHELL,ierr)
        call PCShellSetSetUp(pc,LDSP1,ierr)
        call PCShellSetApply(pc,LDSP2,ierr)
      ELSE
         CALL ASSERT(.FALSE.)
      ENDIF

      call PCSetUp(pc,ierr)
      CALL ASSERT(ierr.EQ.0)

      
      
999   CONTINUE


C     ON DOIT CREER CE SOLVEUR BIDON POUR LE PRECONDITIONNEUR LDLT_SP
C     SI ACTION='PRERES' *ET* ACTION='RESOUD' 
      IF (PRECON.EQ.'LDLT_SP') THEN
C       CREATION D'UN VECTEUR TEMPORAIRE UTILISE DANS PREC_SP
        CALL JELIRA(NONU//'.SMOS.SMDI','LONMAX',NSMDI,KBID)
        NEQ=NSMDI
        call VecCreateMPI(PETSC_COMM_WORLD,PETSC_DECIDE,NEQ,xtemp,ierr)
        CALL ASSERT(ierr.EQ.0)
C       CREATION SOLVEUR BIDON
        SOLVBD=ZK24(JSLVK-1+3)
        CALL CRSMSP(SOLVBD,NOMAT)
      ENDIF

      CALL JEDEMA()

#endif

      END

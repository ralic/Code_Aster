      SUBROUTINE APETS3(ACTION,KPTSC)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION
!& MODIF APETS3 PETSC  DATE 26/07/2011   AUTEUR DESOZA T.DESOZA 
! ==================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D              WWW.CODE-ASTER.ORG
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
!----------------------------------------------------------------
!
!  CREATION DU PRECONDITIONNEUR PETSC (INSTANCE NUMERO KPTSC)
!
!----------------------------------------------------------------

#ifdef _HAVE_PETSC

!================================================================
#include "include/finclude/petsc.h"
#include "include/finclude/petscvec.h"
#include "include/finclude/petscmat.h"
#include "include/finclude/petscksp.h"
#include "include/finclude/petscpc.h"
!================================================================
! --- DECLARATIONS NORMALISEES JEVEUX ---------------------------
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
!----------------------------------------------------------------
!     AU PLUS 5 MATRICES PETSC SONT GEREES A LA FOIS
      INTEGER      NMXINS
      PARAMETER   (NMXINS=5)
!
!     VARIABLES LOCALES
      INTEGER JSLVK,JSLVR,JSLVI
      INTEGER NIREMP,NSMDI
!
      CHARACTER*24 PRECON
      CHARACTER*19 NOMAT,NOSOLV
      CHARACTER*14 NONU
      CHARACTER*4  KBID
!
      REAL*8 FILLIN
!
!     COMMUN DE SAUVEGARDE DES INSTANCES
      CHARACTER*19 NOMATS(NMXINS),NOSOLS(NMXINS)
      CHARACTER*14  NONUS(NMXINS)
      Mat              Ap(NMXINS)
      KSP              Kp(NMXINS)
      COMMON /SPETSC/ Ap,Kp,NOMATS,NOSOLS,NONUS
!
!     SUBROUTINES EXTERNES POUR L'UTILISATION DU
!     PRECONDITIONNEUR SIMPLE PRECISION LDLT_SP
      external LDSP1
      external LDSP2
!
!     COMMUN POUR LE PRECONDITIONNEUR SIMPLE PRECISION LDLT_SP
      CHARACTER*19 SPSOMU,SPMAT,SPSOLV
      Vec          xlocal,xglobal
      VecScatter   xscatt
      COMMON /LDLTSP/xlocal,xscatt,xglobal,SPSOMU,SPMAT,SPSOLV
!----------------------------------------------------------------
!     Variables PETSc
      PetscMPIInt rang,size,ier
      PetscInt fill,ierr,neq
      PetscReal fillp
      Mat A
      KSP ksp,kspp
      PC  pc,pcp
!----------------------------------------------------------------
      CALL JEMARQ()

!     -- LECTURE DU COMMUN
      NOMAT  = NOMATS(KPTSC)
      NOSOLV = NOSOLS(KPTSC)
      NONU   =  NONUS(KPTSC)
      A      =     Ap(KPTSC)
      ksp    =     Kp(KPTSC)

      CALL JEVEUO(NOSOLV//'.SLVK','L',JSLVK)
      CALL JEVEUO(NOSOLV//'.SLVR','L',JSLVR)
      CALL JEVEUO(NOSOLV//'.SLVI','L',JSLVI)
      PRECON = ZK24(JSLVK-1+2)
      FILLIN =   ZR(JSLVR-1+3)
      NIREMP =   ZI(JSLVI-1+4)

      fill  = NIREMP
      fillp = FILLIN

!     ----- RECUPERE LE RANG DU PROCESSUS
      CALL MPI_COMM_RANK(PETSC_COMM_WORLD, rang, ier)
      CALL MPI_COMM_SIZE(PETSC_COMM_WORLD, size, ier)

!     -- TRAITEMENT PARTICULIER DU PRECONDITIONNEUR LDLT_SP
!     -----------------------------------------------------
      IF (PRECON.EQ.'LDLT_SP') THEN
         IF (ACTION.EQ.'PRERES') THEN
!           LDLT_SP FAIT APPEL A DEUX ROUTINES EXTERNES
            call KSPGetPC(ksp,pc,ierr)
            CALL ASSERT(ierr.EQ.0)
            call PCSetType(pc,PCSHELL,ierr)
            CALL ASSERT(ierr.EQ.0)
            call PCShellSetSetUp(pc,LDSP1,ierr)
            CALL ASSERT(ierr.EQ.0)
            call PCShellSetApply(pc,LDSP2,ierr)
            CALL ASSERT(ierr.EQ.0)
!
            CALL ASSERT(SPMAT.EQ.' ')
            SPMAT  = NOMAT
            CALL ASSERT(SPSOLV.EQ.' ')
            SPSOLV = NOSOLV
!           INSTANCIATION DU PRECONDITIONNEUR
            call PCSetUp(pc,ierr)
!           ERREUR : PCENT_PIVOT PAS SUFFISANT
            IF (ierr.NE.0) THEN
              CALL U2MESS('F','PETSC_15')
            ENDIF
         ELSE IF (ACTION.EQ.'RESOUD') THEN
!           CREATION SOLVEUR BIDON SIMPLE PRECISION
            SPSOMU = ZK24(JSLVK-1+3)
            CALL CRSMSP(SPSOMU,NOMAT,0)
!           CREATION DES VECTEURS TEMPORAIRES UTILISES DANS LDLT_SP
            CALL JELIRA(NONU//'.SMOS.SMDI','LONMAX',NSMDI,KBID)
            neq=NSMDI
!
            CALL ASSERT(xlocal.EQ.0)
            call VecCreateMPI(PETSC_COMM_WORLD,PETSC_DECIDE,neq,xlocal,
     &                        ierr)
            CALL ASSERT(ierr.EQ.0)
!
            CALL ASSERT(xscatt.EQ.0)
            CALL ASSERT(xglobal.EQ.0)
            call VecScatterCreateToAll(xlocal,xscatt,xglobal,ierr)
            CALL ASSERT(ierr.EQ.0)
         ELSE
            CALL ASSERT(.FALSE.)
         ENDIF
         GOTO 999
      ENDIF


!     -- TRAITEMENT DES AUTRES PRECONDITIONNEURS
!     ------------------------------------------
      IF (size.GT.1) THEN
         IF (ACTION.NE.'RESOUD') GOTO 999
! EN PARALLELE, ON NE PREPARE PAS LE PRECONDITIONNEUR TT DE SUITE
! CAR ON NE VEUT PAS ETRE OBLIGE D'APPELER KSPSetUp
         kspp=ksp
         call KSPGetPC(kspp,pcp,ierr)
         CALL ASSERT(ierr.EQ.0)
         call PCSetType(pcp,PCBJACOBI,ierr)
         CALL ASSERT(ierr.EQ.0)
         call KSPSetUp(kspp,ierr)
         CALL ASSERT(ierr.EQ.0)
         call PCBJacobiGetSubKSP(pcp,PETSC_NULL_INTEGER,
     &                               PETSC_NULL_INTEGER,ksp,ierr)
         CALL ASSERT(ierr.EQ.0)
      ELSE
! size.LE.1 i.e. SEQUENTIEL
         IF (ACTION.NE.'PRERES') GOTO 999
      END IF

!     -- choix du preconditionneur :
!     -------------------------------
! ("LDLT_INC","JACOBI","SOR"),defaut="LDLT_INC" ),
      call KSPGetPC(ksp,pc,ierr)
      CALL ASSERT(ierr.EQ.0)
      IF (PRECON.eq.'LDLT_INC') THEN
         call PCSetType(pc,PCILU,ierr)
         CALL ASSERT(ierr.EQ.0)
         call PCFactorSetLevels(pc,fill,ierr)
         CALL ASSERT(ierr.EQ.0)
         call PCFactorSetFill(pc,fillp,ierr)
         CALL ASSERT(ierr.EQ.0)
         call PCFactorSetMatOrderingType(pc,MATORDERING_NATURAL,ierr)
         CALL ASSERT(ierr.EQ.0)
      ELSEIF (PRECON.eq.'JACOBI') THEN
         call PCSetType(pc,PCJACOBI,ierr)
         CALL ASSERT(ierr.EQ.0)
      ELSEIF (PRECON.eq.'SOR') THEN
         call PCSetType(pc,PCSOR,ierr)
         CALL ASSERT(ierr.EQ.0)
      ELSE
         CALL ASSERT(.FALSE.)
      ENDIF

      call PCSetUp(pc,ierr)
      IF (ierr.NE.0) THEN
        CALL U2MESS('F','PETSC_14')
      ENDIF



999   CONTINUE

      CALL JEDEMA()

#endif

      END

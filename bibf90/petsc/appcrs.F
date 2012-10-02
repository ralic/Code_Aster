      SUBROUTINE APPCRS(KPTSC)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF APPCRS PETSC  DATE 02/10/2012   AUTEUR DESOZA T.DESOZA 
! ================================================================== 
! COPYRIGHT (C) 1991 - 2012  EDF R&D              WWW.CODE-ASTER.ORG 
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
C RESPONSABLE DESOZA T.DESOZA
      INCLUDE 'jeveux.h'
      INTEGER KPTSC
!----------------------------------------------------------------
!
!  CREATION DU PRECONDITIONNEUR PETSC (INSTANCE NUMERO KPTSC)
!  PHASE DE RESOLUTION (RESOUD)
!
!----------------------------------------------------------------

#ifdef _HAVE_PETSC

!================================================================
#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"
#include "finclude/petscksp.h"
#include "finclude/petscpc.h"
!================================================================
!----------------------------------------------------------------
!     AU PLUS 5 MATRICES PETSC SONT GEREES A LA FOIS
      INTEGER      NMXINS
      PARAMETER   (NMXINS=5)
!
!     VARIABLES LOCALES
      INTEGER RANG  ,NBPROC
      INTEGER JSLVK ,JSLVR ,JSLVI
      INTEGER NIREMP,NSMDI
!
      CHARACTER*24 PRECON
      CHARACTER*19 NOMAT ,NOSOLV
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
!
      CALL JEVEUO(NOSOLV//'.SLVK','L',JSLVK)
      CALL JEVEUO(NOSOLV//'.SLVR','L',JSLVR)
      CALL JEVEUO(NOSOLV//'.SLVI','L',JSLVI)
      PRECON = ZK24(JSLVK-1+2)
      FILLIN =   ZR(JSLVR-1+3)
      NIREMP =   ZI(JSLVI-1+4)
!
      fill  = NIREMP
      fillp = FILLIN

!     -- RECUPERE LE RANG DU PROCESSUS ET LE NB DE PROCS
      CALL MPICM0(RANG  ,NBPROC)

!     -- CAS PARTICULIER (LDLT_INC/SOR)
!     -- CES PC NE SONT PAS PARALLELISES
!     -- ON UTILISE DONC DES VERSIONS PAR BLOC
!     ----------------------------------------
      IF ((PRECON.EQ.'LDLT_INC').OR.(PRECON.EQ.'SOR')) THEN
         IF (NBPROC.GT.1) THEN
            kspp=ksp
            call KSPGetPC(kspp,pcp,ierr)
            CALL ASSERT(ierr.EQ.0)
            call PCSetType(pcp,PCBJACOBI,ierr)
            CALL ASSERT(ierr.EQ.0)
            call KSPSetUp(kspp,ierr)
            CALL ASSERT(ierr.EQ.0)
            call PCBJacobiGetSubKSP(pcp,PETSC_NULL_INTEGER,
     &                                  PETSC_NULL_INTEGER,ksp,ierr)
            CALL ASSERT(ierr.EQ.0)
         ELSE
            GOTO 999
         END IF
      END IF

!     -- choix du preconditionneur :
!     -------------------------------
      call KSPGetPC(ksp,pc,ierr)
      CALL ASSERT(ierr.EQ.0)
!-----------------------------------------------------------------------
      IF (PRECON.EQ.'LDLT_INC') THEN
         call PCSetType(pc,PCILU,ierr)
         CALL ASSERT(ierr.EQ.0)
         call PCFactorSetLevels(pc,fill,ierr)
         CALL ASSERT(ierr.EQ.0)
         call PCFactorSetFill(pc,fillp,ierr)
         CALL ASSERT(ierr.EQ.0)
         call PCFactorSetMatOrderingType(pc,MATORDERINGNATURAL,ierr)
         CALL ASSERT(ierr.EQ.0)
!-----------------------------------------------------------------------
      ELSEIF (PRECON.EQ.'LDLT_SP') THEN
!        CREATION SOLVEUR BIDON SIMPLE PRECISION
         SPSOMU = ZK24(JSLVK-1+3)
         CALL CRSMSP(SPSOMU,NOMAT,0)
!        CREATION DES VECTEURS TEMPORAIRES UTILISES DANS LDLT_SP
         CALL JELIRA(NONU//'.SMOS.SMDI','LONMAX',NSMDI,KBID)
         neq=NSMDI
!
         CALL ASSERT(xlocal.EQ.0)
         call VecCreateMPI(PETSC_COMM_WORLD,PETSC_DECIDE,neq,xlocal,
     &                     ierr)
         CALL ASSERT(ierr.EQ.0)
!
         CALL ASSERT(xscatt.EQ.0)
         CALL ASSERT(xglobal.EQ.0)
         call VecScatterCreateToAll(xlocal,xscatt,xglobal,ierr)
         CALL ASSERT(ierr.EQ.0)
!-----------------------------------------------------------------------
      ELSEIF (PRECON.EQ.'SOR') THEN
         call PCSetType(pc,PCSOR,ierr)
         CALL ASSERT(ierr.EQ.0)
      ENDIF
!-----------------------------------------------------------------------

!     CREATION EFFECTIVE DES PRECONDITIONNEURS RETARDES
      IF ((PRECON.EQ.'LDLT_INC').OR.(PRECON.EQ.'SOR')) THEN
         call PCSetUp(pc,ierr)
         IF (ierr.NE.0) THEN
           CALL U2MESS('F','PETSC_14')
         ENDIF
      ENDIF

999   CONTINUE

      CALL JEDEMA()

#endif

      END

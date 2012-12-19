      SUBROUTINE APPCPR(KPTSC)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION
!& MODIF APPCPR PETSC  DATE 19/12/2012   AUTEUR DESOZA T.DESOZA 
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
! RESPONSABLE DESOZA T.DESOZA
      INCLUDE 'jeveux.h'
      INTEGER KPTSC
!----------------------------------------------------------------
!
!  CREATION DU PRECONDITIONNEUR PETSC (INSTANCE NUMERO KPTSC)
!  PHASE DE PRE-TRAITEMENT (PRERES)
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
      INTEGER TBLOC ,NIREMP
      INTEGER IBID  ,IRET
!
      CHARACTER*24 PRECON
      CHARACTER*19 NOMAT ,NOSOLV
      CHARACTER*14 NONU
      CHARACTER*4  EXILAG
!
      REAL*8 FILLIN
!
!     COMMUN DE SAUVEGARDE DES INSTANCES
      CHARACTER*19 NOMATS(NMXINS),NOSOLS(NMXINS)
      CHARACTER*14  NONUS(NMXINS)
      Mat              Ap(NMXINS)
      KSP              Kp(NMXINS)
      Vec              Bp(NMXINS),x
      COMMON /SPETSC/ Ap,Kp,Bp,x,NOMATS,NOSOLS,NONUS
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
      PetscInt fill,ierr
      PetscReal fillp
      Mat A
      KSP ksp
      PC  pc
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
!     -- QUE L'ON CREERA AU MOMENT DE LA RESOLUTION (DANS APPCPO)
!     -----------------------------------------------------------
      IF ((PRECON.EQ.'LDLT_INC').OR.(PRECON.EQ.'SOR')) THEN
         IF (NBPROC.GT.1) THEN
!           EN PARALLELE, ON NE PREPARE PAS LE PRECONDITIONNEUR
!           TOUT DE SUITE CAR ON NE VEUT PAS ETRE OBLIGE
!           D'APPELER KSPSetUp
            GOTO 999
         END IF
      END IF

!     -- CHOIX DU PRECONDITIONNEUR :
!     ------------------------------
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
         call PCSetType(pc,PCSHELL,ierr)
         CALL ASSERT(ierr.EQ.0)
!        LDLT_SP FAIT APPEL A DEUX ROUTINES EXTERNES
         call PCShellSetSetUp(pc,LDSP1,ierr)
         CALL ASSERT(ierr.EQ.0)
         call PCShellSetApply(pc,LDSP2,ierr)
         CALL ASSERT(ierr.EQ.0)
!
         CALL ASSERT(SPMAT.EQ.' ')
         SPMAT  = NOMAT
         CALL ASSERT(SPSOLV.EQ.' ')
         SPSOLV = NOSOLV
!-----------------------------------------------------------------------
      ELSEIF (PRECON.EQ.'JACOBI') THEN
         call PCSetType(pc,PCJACOBI,ierr)
         CALL ASSERT(ierr.EQ.0)
!-----------------------------------------------------------------------
      ELSEIF (PRECON.EQ.'SOR') THEN
         call PCSetType(pc,PCSOR,ierr)
         CALL ASSERT(ierr.EQ.0)
!-----------------------------------------------------------------------
      ELSEIF (PRECON.EQ.'ML') THEN
         call PCSetType(pc,PCML,ierr)
         CALL ASSERT(ierr.EQ.0)
         call PetscOptionsSetValue('-pc_type','ml',ierr)
         CALL ASSERT(ierr.EQ.0)
!        CHOIX DE LA RESTRICTION (UNCOUPLED UNIQUEMENT ACTUELLEMENT)
         call PetscOptionsSetValue('-pc_ml_CoarsenScheme','Uncoupled',
     &                             ierr)
         CALL ASSERT(ierr.EQ.0)
         call PetscOptionsSetValue('-pc_ml_PrintLevel', '0',ierr)
         CALL ASSERT(ierr.EQ.0)
!        APPEL OBLIGATOIRE POUR PRENDRE EN COMPTE LES AJOUTS CI-DESSUS
         call PCSetFromOptions(pc,ierr)
         CALL ASSERT(ierr.EQ.0)
!-----------------------------------------------------------------------
      ELSEIF (PRECON.EQ.'BOOMER') THEN
         call PCSetType(pc,PCHYPRE,ierr)
         CALL ASSERT(ierr.EQ.0)
         call PetscOptionsSetValue('-pc_hypre_type','boomeramg',ierr)
         CALL ASSERT(ierr.EQ.0)
!        CHOIX DE LA RESTRICTION (PMIS UNIQUEMENT ACTUELLEMENT)
         call PetscOptionsSetValue('-pc_hypre_boomeramg_coarsen_type',
     &                             'PMIS',ierr)
         CALL ASSERT(ierr.EQ.0)
!        CHOIX DU LISSAGE (SOR UNIQUEMENT POUR LE MOMENT)
         call PetscOptionsSetValue('-pc_hypre_boomeramg_relax_type_all',
     &                             'SOR/Jacobi',ierr)
         CALL ASSERT(ierr.EQ.0)
         call PetscOptionsSetValue(
     &                  '-pc_hypre_boomeramg_print_statistics','0',ierr)
         CALL ASSERT(ierr.EQ.0)
!        APPEL OBLIGATOIRE POUR PRENDRE EN COMPTE LES AJOUTS CI-DESSUS
         call PCSetFromOptions(pc,ierr)
         CALL ASSERT(ierr.EQ.0)
!-----------------------------------------------------------------------
      ELSEIF (PRECON.eq.'SANS') THEN
         call PCSetType(pc,PCNONE,ierr)
         CALL ASSERT(ierr.EQ.0)
!-----------------------------------------------------------------------
      ELSE
         CALL ASSERT(.FALSE.)
      ENDIF
!-----------------------------------------------------------------------

!     VERIFICATION DU DOMAINE D'APPLICATION
      IF (PRECON.EQ.'ML'.OR.PRECON.EQ.'BOOMER') THEN
         CALL DISMOI('C','EXIS_LAGR',NOMAT,'MATR_ASSE',IBID,EXILAG,IRET)
         IF (IRET.EQ.0) THEN
            IF (EXILAG.EQ.'OUI') THEN
               CALL U2MESS('F','PETSC_17')
            ENDIF
         ENDIF
         CALL APBLOC(NOMAT ,NOSOLV,TBLOC )
         IF (TBLOC.LE.0) THEN
            CALL U2MESS('F','PETSC_18')
         ENDIF
      ENDIF

!     CREATION EFFECTIVE DU PRECONDITIONNEUR
      call PCSetUp(pc,ierr)
!     ANALYSE DU CODE RETOUR
      IF (ierr.NE.0) THEN
         IF (PRECON.EQ.'LDLT_SP') THEN
!           ERREUR : PCENT_PIVOT PAS SUFFISANT
            CALL U2MESS('F','PETSC_15')
         ELSE
            CALL U2MESS('F','PETSC_14')
         ENDIF
      ENDIF

999   CONTINUE

      CALL JEDEMA()

#endif

      END

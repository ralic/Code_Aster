      SUBROUTINE APMAIN(ACTION,KPTSC ,RSOLU ,VCINE ,ISTOP ,
     &                  IRET  )
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF APMAIN PETSC  DATE 18/12/2012   AUTEUR SELLENET N.SELLENET 
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
      CHARACTER*(*) ACTION
      INTEGER       KPTSC
      REAL*8        RSOLU(*)
      CHARACTER*19  VCINE
      INTEGER       ISTOP ,IRET
!--------------------------------------------------------------
!
! IN  : ACTION :
!     /'DETR_MAT': POUR DETRUIRE L'INSTANCE PETSC ASSOCIEE A UNE MATRICE
!     /'PRERES'  : POUR CONSTRUIRE LE PRECONDITIONNEUR
!                 (ATTENTION EN // LA CONSTRUCTION DE CERTAINS PC EST
!                  RETARDEE)
!     /'RESOUD'  : POUR RESOUDRE LE SYSTEME LINEAIRE
!
! IN  : KPTSC (I): INDICE DES INSTANCES PETSC DANS Ap,Kp
! I/O : RSOLU (R): EN ENTREE : VECTEUR SECOND MEMBRE (REEL)
!                  EN SORTIE : VECTEUR SOLUTION      (REEL)
!                 (SI ACTION=RESOUD)
! IN  : VCINE (K19): NOM DU CHAM_NO DE CHARGEMENT CINEMATIQUE
!                   (SI ACTION=RESOUD)
! IN  : ISTOP (I)  : COMPORTEMENT EN CAS D'ERREUR
! OUT : IRET  (I)  : CODE RETOUR
!---------------------------------------------------------------

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
      INTEGER      IFM,NIV,IERD,IBID,NMAXIT,PTSERR,JNEQU
      INTEGER      LMAT,IDVALC,JSLVI,JSLVK,JSLVR
!
      CHARACTER*24 PRECON
      CHARACTER*19 NOMAT,NOSOLV
      CHARACTER*14 NONU
      CHARACTER*1  ROUC
!
      REAL*8       DIVTOL,RESIPC
      COMPLEX*16   CBID
!
      LOGICAL      LMD
!
!     COMMUN DE SAUVEGARDE DES INSTANCES
      CHARACTER*19 NOMATS(NMXINS),NOSOLS(NMXINS)
      CHARACTER*14  NONUS(NMXINS)
      Mat              Ap(NMXINS)
      KSP              Kp(NMXINS)
      Vec              Bp(NMXINS),x
      COMMON /SPETSC/ Ap,Kp,Bp,x,NOMATS,NOSOLS,NONUS
!
!     COMMUN POUR LE PRECONDITIONNEUR SIMPLE PRECISION LDLT_SP
      CHARACTER*19 SPSOMU,SPMAT,SPSOLV
      Vec          xlocal,xglobal
      VecScatter   xscatt
      COMMON /LDLTSP/xlocal,xscatt,xglobal,SPSOMU,SPMAT,SPSOLV
!----------------------------------------------------------------
!     Variables PETSc
!
      PetscInt           ierr,neq,i
      PetscInt           maxits
      PetscReal          rtol,atol,dtol
      Vec                r
      PetscScalar        ires,fres
      VecScatter         ctx
      KSPConvergedReason indic
      Mat                A
      KSP                ksp
!----------------------------------------------------------------
      CALL JEMARQ()

!     -- ON DESACTIVE LA LEVEE D'EXCEPTION FPE DANS LES MKL
      CALL MATFPE(-1)

      CALL INFNIV(IFM,NIV)

!     -- LECTURE DU COMMUN
      NOMAT  = NOMATS(KPTSC)
      NOSOLV = NOSOLS(KPTSC)
      NONU   =  NONUS(KPTSC)

      CALL JEVEUO(NOSOLV//'.SLVK','L',JSLVK)
      LMD = ZK24(JSLVK-1+10)(1:3).EQ.'OUI'

      IF (ACTION.EQ.'PRERES') THEN
!     ----------------------------

!        1.1 CREATION ET PREALLOCATION DE LA MATRICE PETSc :
!        ---------------------------------------------------

         IF ( LMD ) THEN
           CALL APALMD(KPTSC)
         ELSE
           CALL APALMC(KPTSC)
         ENDIF

!        1.2 COPIE DE LA MATRICE ASTER VERS LA MATRICE PETSc :
!        -----------------------------------------------------

         IF ( LMD ) THEN
           CALL APMAMD(KPTSC)
         ELSE
           CALL APMAMC(KPTSC)
         ENDIF

!        1.3 ASSEMBLAGE DE LA MATRICE PETSc :
!        ------------------------------------

         call MatAssemblyBegin(Ap(KPTSC),MAT_FINAL_ASSEMBLY,ierr)
         CALL ASSERT(ierr.EQ.0)
         call MatAssemblyEnd  (Ap(KPTSC),MAT_FINAL_ASSEMBLY,ierr)
         CALL ASSERT(ierr.EQ.0)

!        1.4 CREATION DU PRECONDITIONNEUR PETSc (EXTRAIT DU KSP) :
!        ---------------------------------------------------------

         call KSPCreate(PETSC_COMM_WORLD,Kp(KPTSC),ierr)
         CALL ASSERT(ierr.EQ.0)
!
         call KSPSetOperators(Kp(KPTSC),Ap(KPTSC),Ap(KPTSC),
     &                        DIFFERENT_NONZERO_PATTERN,ierr)
         CALL ASSERT(ierr.EQ.0)
!
         CALL APPCPR(KPTSC)

      ELSE IF (ACTION.EQ.'RESOUD') THEN
!     ---------------------------------

!        2.0 RECUPERATION DES POINTEURS DANS LE COMMUN :
!        -----------------------------------------------

         A   = Ap(KPTSC)
         ksp = Kp(KPTSC)

!        2.1 PRETRAITEMENT DU SECOND MEMBRE :
!        ------------------------------------

!        -- MISE A L'ECHELLE DES LAGRANGES DANS LE SECOND MEMBRE
         CALL MTDSCR(NOMAT)
         CALL JEVEUO(NOMAT//'.&INT','L',LMAT)
         CALL MRCONL('MULT',LMAT,0,'R',RSOLU,1)

!        -- PRISE EN COMPTE DES CHARGES CINEMATIQUES :
         CALL JEEXIN(VCINE//'.VALE',IERD)
         IF (IERD.NE.0) THEN
            CALL JEVEUO(VCINE//'.VALE','L',IDVALC)
            CALL JELIRA(VCINE//'.VALE','TYPE',IBID,ROUC)
            CALL ASSERT(ROUC.EQ.'R')
            CALL CSMBGG(LMAT,RSOLU,ZR(IDVALC),CBID,CBID,'R')
         END IF

!        2.2 CREATION DU VECTEUR PETSc :
!        -------------------------------

         CALL APVSMB(KPTSC,LMD,RSOLU)

!        2.3 PARAMETRES DU KSP :
!        -----------------------

         CALL APKSP (KPTSC)

!        2.3b CREATION DES PRECONDITIONNEURS RETARDES :
!        ----------------------------------------------

         CALL APPCRS(KPTSC,LMD)

!        2.4 RESOLUTION :
!        ----------------

         call VecDuplicate(Bp(KPTSC),x,ierr)
         CALL ASSERT(ierr.EQ.0)
         call KSPSolve(ksp,Bp(KPTSC),x,ierr)

!        2.5 DIAGNOSTIC :
!        ----------------

!        ARRET ANORMAL DU KSP
         IF (ierr.GT.0) then
            CALL U2MESS('F','PETSC_13')
         ENDIF

!        ANALYSE DE LA CONVERGENCE DU KSP
         call KSPGetConvergedReason(ksp, indic, ierr)
         CALL ASSERT(ierr.EQ.0)

!        ANALYSE DES CAUSES ET EMISSION EVENTUELLE D'UN MESSAGE
!        EN CAS DE DIVERGENCE
         IF (indic.LT.0) THEN
            call KSPGetTolerances(ksp,rtol,atol,dtol,maxits,ierr)
            CALL ASSERT(ierr.EQ.0)
!           -- PRECONDITIONNEUR UTILISE
            CALL JEVEUO(NOSOLV//'.SLVK','L',JSLVK)
            PRECON = ZK24(JSLVK-1+2)
            IF (indic.EQ.KSP_DIVERGED_ITS) THEN
!              NOMBRE MAX D'ITERATIONS
               IF ((ISTOP.EQ.0).OR.(PRECON.NE.'LDLT_SP')) THEN
                  NMAXIT=maxits
                  CALL U2MESI('F','PETSC_5',1,NMAXIT)
               ELSE
                  IRET = 1
                  GOTO 999
               ENDIF
            ELSE IF (indic.EQ.KSP_DIVERGED_DTOL) THEN
!              DIVERGENCE
               DIVTOL = dtol
               CALL U2MESR('F','PETSC_6',1,DIVTOL)
            ELSE IF (indic.EQ.KSP_DIVERGED_BREAKDOWN) THEN
!              BREAKDOWN
               CALL U2MESS('F','PETSC_7')
            ELSE IF (indic.EQ.KSP_DIVERGED_BREAKDOWN_BICG) THEN
!              BREAKDOWN BiCG
               CALL U2MESS('F','PETSC_8')
            ELSE IF (indic.EQ.KSP_DIVERGED_NONSYMMETRIC) THEN
!              MATRICE NON SYMETRIQUE
               CALL U2MESS('F','PETSC_9')
            ELSE IF (indic.EQ.KSP_DIVERGED_INDEFINITE_PC) THEN
!              PRECONDITIONNEUR NON DEFINI
               CALL U2MESS('F','PETSC_10')
            ELSE IF (indic.EQ.KSP_DIVERGED_INDEFINITE_MAT) THEN
!              MATRICE NON DEFINIE
               CALL U2MESS('F','PETSC_11')
            ELSE
!              AUTRE ERREUR
               PTSERR = indic
               CALL U2MESI('F','PETSC_12',1,PTSERR)
            ENDIF
         ENDIF

!        2.5b VERIFICATION DE LA SOLUTION :
!        ----------------------------------

!        -- DOIT-ON VERIFIER LE CRITERE EN NORME NON PRECONDITIONNEE ?
         CALL JEVEUO(NOSOLV//'.SLVR','L',JSLVR)
         RESIPC = ZR(JSLVR-1+4)

         IF (RESIPC.GE.0.D0) THEN
            call VecDuplicate(x,r,ierr)
            CALL ASSERT(ierr.EQ.0)
!           r = Ax
            call MatMult(A,x,r,ierr)
            CALL ASSERT(ierr.EQ.0)
!           r = b - Ax
            call VecAYPX(r,-1.D0,Bp(KPTSC),ierr)
            CALL ASSERT(ierr.EQ.0)
!           fres = ||r||_2
            call VecNorm(r,NORM_2,fres,ierr)
            CALL ASSERT(ierr.EQ.0)
!           ires = ||b||_2
            call VecNorm(Bp(KPTSC),NORM_2,ires,ierr)
            CALL ASSERT(ierr.EQ.0)
!
            call VecDestroy(r,ierr)
            CALL ASSERT(ierr.EQ.0)
!
            call KSPGetTolerances(ksp,rtol,atol,dtol,maxits,ierr)
            CALL ASSERT(ierr.EQ.0)
!
            IF (fres.GT.SQRT(rtol)*ires) THEN
               CALL U2MESS('F','PETSC_16')
            ENDIF
         ENDIF

!        2.6 RECOPIE DE LA SOLUTION :
!        ----------------------------

         CALL APSOLU(KPTSC,LMD,RSOLU)

!         2.7 NETTOYAGE PETSc (VECTEURS) :
!         --------------------------------

!        -- EN CAS D'ERREUR DANS LES ITERATIONS DE KRYLOV ON SAUTE ICI
999      CONTINUE
         call VecDestroy(Bp(KPTSC),ierr)
         CALL ASSERT(ierr.EQ.0)
         call VecDestroy(x,ierr)
         CALL ASSERT(ierr.EQ.0)

!        -- PRECONDITIONNEUR UTILISE
         CALL JEVEUO(NOSOLV//'.SLVK','L',JSLVK)
         PRECON = ZK24(JSLVK-1+2)

!        -- TRAITEMENT PARTICULIER DU PRECONDITIONNEUR LDLT_SP
         IF (PRECON .EQ. 'LDLT_SP') THEN
!           MENAGE
            SPSOMU = ZK24(JSLVK-1+3)
            CALL DETRSD('SOLVEUR',SPSOMU)
            SPSOMU = ' '
!
            call VecDestroy(xlocal,ierr)
            CALL ASSERT(ierr.EQ.0)
            call VecDestroy(xglobal,ierr)
            CALL ASSERT(ierr.EQ.0)
            call VecScatterDestroy(xscatt,ierr)
            CALL ASSERT(ierr.EQ.0)
            xlocal  = 0
            xglobal = 0
            xscatt  = 0
!           ON STOCKE LE NOMBRE D'ITERATIONS DU KSP
            call KSPGetIterationNumber(ksp,maxits,ierr)
            CALL ASSERT(ierr.EQ.0)
            NMAXIT = maxits
            CALL JEVEUO(NOSOLV//'.SLVI','E',JSLVI)
            ZI(JSLVI-1+5) = NMAXIT
         ENDIF

      ELSE IF (ACTION.EQ.'DETR_MAT') THEN
!     -----------------------------------

!        3.0 RECUPERATION DES POINTEURS :
!        --------------------------------

         A   = Ap(KPTSC)
         ksp = Kp(KPTSC)

!        3.1 NETTOYAGE PETSc :
!        ---------------------

!        -- DESTRUCTION DES OBJETS PETSC GENERAUX
         call MatDestroy(A,ierr)
         CALL ASSERT(ierr.EQ.0)
         call KSPDestroy(ksp,ierr)
         CALL ASSERT(ierr.EQ.0)

!        -- SUPRESSION DE L'INSTANCE PETSC
         NOMATS(KPTSC) = ' '
         NOSOLS(KPTSC) = ' '
          NONUS(KPTSC) = ' '
             Ap(KPTSC) =  0
             Kp(KPTSC) =  0

!        -- PRECONDITIONNEUR UTILISE
         CALL JEVEUO(NOSOLV//'.SLVK','L',JSLVK)
         PRECON = ZK24(JSLVK-1+2)

         IF (PRECON .EQ. 'LDLT_SP') THEN
!           MENAGE
            SPMAT  = ' '
            SPSOLV = ' '
         ENDIF

      ELSE
          CALL ASSERT(.FALSE.)
      END IF

!     -- ON REACTIVE LA LEVEE D'EXCEPTION
      CALL MATFPE(1)

      CALL JEDEMA()

#endif

      END


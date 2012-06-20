      SUBROUTINE APETSC(ACTION,SOLVEZ,MATASZ,RSOLU ,VCINEZ,
     &                  NBSOL ,IRET  )
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION
!& MODIF APETSC PETSC  DATE 18/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
      CHARACTER*(*) ACTION,SOLVEZ,MATASZ,VCINEZ
      REAL*8        RSOLU(*)
      INTEGER       NBSOL,IRET
!-----------------------------------------------------------------------
! BUT : ROUTINE D'INTERFACE ENTRE CODE_ASTER ET LA BIBLIOTHEQUE PETSC
!       DE RESOLUTION DE SYSTEMES LINEAIRES.

! IN  : ACTION
!     /'DETR_MAT': POUR DEMANDER LA DESTRUCTION DE L'INSTANCE PETSC
!                  ASSOCIEE A UNE MATRICE
!     /'PRERES'  : POUR DEMANDER LA FACTORISATION DU PRECONDITIONNEUR
!     /'RESOUD'  : POUR DEMANDER LA RESOLUTION ITERATIVE

! IN  : SOLVEU   (K19) : NOM DE LA SD SOLVEUR
!                       (SI ACTION=PRERES/RESOUD)
! IN  : MATASS   (K19) : NOM DE LA MATR_ASSE
!                       (SI ACTION=PRERES/RESOUD)
! I/O : RSOLU      (R) : EN ENTREE : VECTEUR SECOND MEMBRE (REEL)
!                        EN SORTIE : VECTEUR SOLUTION      (REEL)
!                       (SI ACTION=RESOUD)
! IN  : VCINE    (K19) : NOM DU CHAM_NO DE CHARGEMENT CINEMATIQUE
!                       (SI ACTION=RESOUD)
! IN  : NBSOL      (I) : NOMRE DE SYSTEMES A RESOUDRE
! OUT : IRET       (I) : CODE_RETOUR
!                      /  0 : OK
!                      / !0 : NOOK
!-----------------------------------------------------------------------

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
      INTEGER      IPREM,K,IBID,IERD,NSMDI,KDEB
      INTEGER      JREFA,JTRAV,KPTSC
      INTEGER*4    N4
!
      CHARACTER*19 SOLVEU,MATAS,VCINE
      CHARACTER*14 NU
      CHARACTER*4  ETAMAT,KBID
      CHARACTER*1  ROUC
!
!     COMMUN DE SAUVEGARDE DES INSTANCES
      CHARACTER*19 NOMATS(NMXINS),NOSOLS(NMXINS)
      CHARACTER*14 NONUS(NMXINS)
      Mat      Ap(NMXINS)
      KSP      Kp(NMXINS)
      COMMON /SPETSC/ Ap,Kp,NOMATS,NOSOLS,NONUS
!
!     COMMUN POUR LE PRECONDITIONNEUR SIMPLE PRECISION LDLT_SP
      CHARACTER*19 SPSOMU,SPMAT,SPSOLV
      Vec          xlocal,xglobal
      VecScatter   xscatt
      COMMON /LDLTSP/xlocal,xscatt,xglobal,SPSOMU,SPMAT,SPSOLV
!----------------------------------------------------------------
!     Variables PETSc
      PetscInt m,n,ierr
!----------------------------------------------------------------
!     INITIALISATION DE PETSC A FAIRE AU PREMIER APPEL
      SAVE IPREM
      DATA IPREM /0/
!----------------------------------------------------------------
      CALL JEMARQ()

      SOLVEU = SOLVEZ
      MATAS = MATASZ
      VCINE = VCINEZ

      IF (IPREM.EQ.0) THEN
!     --------------------
         call PetscInitialize(PETSC_NULL_CHARACTER,ierr)
         IF (ierr.NE.0) THEN
            CALL U2MESS('F','PETSC_1')
         END IF
         DO K = 1,NMXINS
                Ap(K) =  0
                Kp(K) =  0
            NOMATS(K) = ' '
            NOSOLS(K) = ' '
             NONUS(K) = ' '
         ENDDO
         xlocal  = 0
         xglobal = 0
         xscatt  = 0
         SPSOMU  = ' '
         SPMAT   = ' '
         SPSOLV  = ' '
         IPREM = 1
      END IF

      CALL ASSERT(MATAS.NE.' ')
      CALL JELIRA(MATAS//'.VALM','TYPE',IBID,ROUC)

!     1. INFORMATIONS SUR LA MATRICE :
!     --------------------------------
      IF (ROUC.NE.'R') THEN
         CALL U2MESS('F','PETSC_2')
      END IF

!     2. ON CHERCHE UNE PLACE EN MEMOIRE :
!     ------------------------------------
!     ON TESTE LE NOM DE LA MATRICE, CELUI DU NUME_DDL,
!     LA TAILLE DES MATRICES ASTER ET PETSC
      CALL DISMOI('F','NOM_NUME_DDL',MATAS,'MATR_ASSE',IBID,NU,IERD)
      CALL JELIRA(NU//'.SMOS.SMDI','LONMAX',NSMDI,KBID)

      KPTSC=1
      DO K=1,NMXINS
         IF ((NOMATS(K).EQ.MATAS) .AND.
     &       (NONUS (K).EQ.NU   ))
     &   THEN
            CALL ASSERT(Ap(K).NE.0)
            call MatGetSize(Ap(K),m,n,ierr)
            CALL ASSERT(ierr.EQ.0)
            IF (m.EQ.n .AND. NSMDI.EQ.n) THEN
               KPTSC = K
               GOTO 1
            ENDIF
         ENDIF
      ENDDO

      IF (ACTION.EQ.'RESOUD') THEN
         CALL ASSERT(.FALSE.)
      ENDIF

      IF (ACTION.EQ.'DETR_MAT') THEN
         GOTO 9999
      ENDIF

!     Y-A-T-IL ENCORE UNE PLACE LIBRE ?
      DO K=1,NMXINS
         IF (NOMATS(K).EQ.' ') THEN
            KPTSC = K
            GOTO 1
         ENDIF
      END DO

      CALL U2MESS('F','PETSC_3')

1     CONTINUE

!     3. QUELQUES VERIFICATIONS ET PETITES ACTIONS :
!     ----------------------------------------------
      IF (ACTION.EQ.'PRERES') THEN
!        -- REMPLISSAGE DU COMMUN
         CALL ASSERT(NOMATS(KPTSC).EQ.' ')
         CALL ASSERT(NOSOLS(KPTSC).EQ.' ')
         CALL ASSERT( NONUS(KPTSC).EQ.' ')
         CALL ASSERT(MATAS .NE.' ')
         CALL ASSERT(SOLVEU.NE.' ')
         CALL ASSERT(NU    .NE.' ')
         NOMATS(KPTSC) = MATAS
         NOSOLS(KPTSC) = SOLVEU
          NONUS(KPTSC) = NU

!        -- VERIFICATION QUE LA MATRICE N'A PAS ETE FACTORISEE
         CALL JEVEUO(MATAS//'.REFA','E',JREFA)
         ETAMAT = ZK24(JREFA-1+8)
         IF (ETAMAT.EQ.'DECT') THEN
            CALL U2MESS('A','PETSC_4')
            GOTO 9999
         ELSE
            ZK24(JREFA-1+8) = 'DECT'
         ENDIF

!        -- ELIMINATION DES DDLS (AFFE_CHAR_CINE)
         CALL ASSERT(ZK24(JREFA-1+3).NE.'ELIMF')
         IF (ZK24(JREFA-1+3).EQ.'ELIML') CALL MTMCHC(MATAS,'ELIMF')
         CALL ASSERT(ZK24(JREFA-1+3).NE.'ELIML')

      ELSE IF (ACTION.EQ.'RESOUD') THEN
         CALL ASSERT(NBSOL.GE.1)
      ELSE IF (ACTION.EQ.'DETR_MAT') THEN
!        RIEN A VERIFIER
      ELSE
        CALL ASSERT(.FALSE.)
      END IF

!     4. APPEL DE PETSC :
!     -------------------

      IF (ACTION.EQ.'RESOUD') THEN
         CALL WKVECT('&&APETSC.TRAVAIL','V V R',NSMDI,JTRAV)
         N4 = NSMDI
         DO K = 1,NBSOL
            KDEB = (K-1)*NSMDI+1
            CALL DCOPY(N4,RSOLU(KDEB),1,ZR(JTRAV),1)
            CALL APETSR(ACTION,KPTSC,ZR(JTRAV),VCINE)
            CALL DCOPY(N4,ZR(JTRAV),1,RSOLU(KDEB),1)
         END DO
         CALL JEDETR('&&APETSC.TRAVAIL')
      ELSE
         CALL APETSR(ACTION,KPTSC,RSOLU,VCINE)
      END IF

9999  CONTINUE

      IRET = 0

      CALL JEDEMA()

#else

      CALL U2MESS('F','FERMETUR_10')

#endif

      END

      SUBROUTINE APETSC(ACTION,SOLVEZ,MATASZ,RSOLU,VCINEZ,NBSOL,IRET)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF APETSC PETSC  DATE 08/08/2008   AUTEUR DESOZA T.DESOZA 
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
      CHARACTER*(*) ACTION,SOLVEZ,MATASZ,VCINEZ
      REAL*8        RSOLU(*)
      INTEGER       NBSOL,IRET
!--------------------------------------------------------------
! BUT : ROUTINE D'INTERFACE ENTRE LE CODE_ASTER ET LA BIBLIOTHEQUE
!       PETSC DE RESOLUTION DE SYSTEMES LINEAIRES.

! IN : ACTION :
!     /'PRERES'  : POUR DEMANDER LA FACTORISATION DU PRECONDITIONNEUR
!     /'RESOUD'  : POUR DEMANDER LA RESOLUTION ITERATIVE

! IN : SOLVEZ   (K19) : NOM DE LA SD SOLVEUR
!             (SI ACTION=PRERES/RESOUD)
! IN : MATAS (K19) : NOM DE LA MATR_ASSE
!             (SI ACTION=PRERES/RESOUD)
! VAR: RSOLU (R)   : EN ENTREE : VECTEUR SECOND MEMBRE (REEL)
!                    EN SORTIE : VECTEUR SOLUTION (REEL)
!             (SI ACTION=RESOUD)
! IN : VCINE (K19) : NOM DU CHAM_NO DE CHARGEMENT CINEMATIQUE
!            (SI ACTION=RESOUD)
! IN : NBSOL (I) : NOMRE DE SYSTEMES A RESOUDRE
! OUT : IRET (I) : CODE_RETOUR :
!            0 : OK
!---------------------------------------------------------------

#ifdef _HAVE_PETSC

!================================================================
#include "include/finclude/petsc.h"
#include "include/finclude/petscvec.h"
#include "include/finclude/petscmat.h"
#include "include/finclude/petscksp.h"
#include "include/finclude/petscpc.h"
!================================================================
! --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
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
      INTEGER NMXINS
      PARAMETER (NMXINS=5)

      INTEGER IPREM,K,IBID,IERD,NSMDI,KDEB
      INTEGER JREFA,JTRAV,KPTSC
      CHARACTER*19 SOLVEU,MATAS,VCINE,NOMAT,NOSOLV
      CHARACTER*14 NU,NONU
      CHARACTER*4  ETAMAT,KBID
      CHARACTER*1  ROUC

      INTEGER*4 N4

!     -- ON STOCKE AU PLUS NMXINS INSTANCES DE PETSC
!     -- PLUS PRECISEMENT LES MATRICES, SOLVEURS, ETC
      CHARACTER*19 NOMATS(NMXINS),NOSOLS(NMXINS)
      CHARACTER*14 NONUS(NMXINS)
      Mat      Ap(NMXINS)
      KSP      Kp(NMXINS)
      COMMON /SPETSC/ Ap,Kp,NOMATS,NOSOLS,NONUS
!----------------------------------------------------------------
!  Variables PETSc
      PetscInt m,n,ierr
!----------------------------------------------------------------
      SAVE IPREM
      DATA IPREM /0/
!----------------------------------------------------------------
      CALL JEMARQ()
      SOLVEU = SOLVEZ
      MATAS = MATASZ
      VCINE = VCINEZ

      IF (IPREM.EQ.0) THEN
!     --------------------------------
         call PetscInitialize(PETSC_NULL_CHARACTER,ierr)
         IF (ierr.NE.0) THEN
           CALL U2MESS('F','PETSC_1')
         END IF
         DO K=1,NMXINS
           NOMATS(K)=' '
           NOSOLS(K)=' '
           NONUS(K)=' '
         ENDDO
         IPREM=1
      END IF

      CALL ASSERT(MATAS.NE.' ')
      CALL JELIRA(MATAS//'.VALM','TYPE',IBID,ROUC)

!     1. INFORMATIONS SUR LA MATRICE :
!     ----------------------------------------------
      IF (ROUC.NE.'R') THEN
         CALL U2MESS('F','PETSC_2')
      END IF

!     2. ON CHERCHE UNE PLACE EN MEMOIRE :
!     ----------------------------------------------

!     ON TEST LE NOM DE LA MATRICE, CELUI DU NUME_DDL,
!     LA TAILLE DU PB ASTER ET DU PB MUMPS ASSOCIE
      CALL DISMOI('F','NOM_NUME_DDL',MATAS,'MATR_ASSE',IBID,NU,IERD)
      CALL JELIRA(NU//'.SMOS.SMDI','LONMAX',NSMDI,KBID)

      KPTSC=1
      DO K=1,NMXINS
        IF ((NOMATS(K).EQ.MATAS).AND.(NONUS(K).EQ.NU)) THEN
          call MatGetSize(Ap(K),m,n,ierr)
          IF (m.EQ.n .AND. NSMDI.EQ.n) THEN
            KPTSC=K
            GOTO 1
          ENDIF
        ENDIF
      END DO
      IF (ACTION.EQ.'DETR_MAT') GO TO 9999

!     Y-A-T-IL ENCORE UNE PLACE LIBRE ?
      DO K=1,NMXINS
        IF (NOMATS(K).EQ.' ') THEN
          KPTSC=K
          GOTO 1
        ENDIF
      END DO
      CALL U2MESS('F','PETSC_3')
1     CONTINUE

!     3. QUELQUES VERIFICATIONS ET PETITES ACTIONS :
!     ----------------------------------------------
      IF (ACTION.EQ.'PRERES') THEN

        CALL ASSERT(SOLVEU.NE.' ')

        CALL ASSERT(NOMATS(KPTSC).EQ.' ')
        CALL ASSERT(NOSOLS(KPTSC).EQ.' ')
        CALL ASSERT(NONUS(KPTSC).EQ.' ')
        NOMAT=MATAS
        NOSOLV=SOLVEU
        NONU=NU
        NOMATS(KPTSC)=NOMAT
        NOSOLS(KPTSC)=NOSOLV
        NONUS(KPTSC)=NONU

        CALL JEVEUO(MATAS//'.REFA','E',JREFA)
        ETAMAT=ZK24(JREFA-1+8)
        IF (ETAMAT.EQ.'DECT') THEN
           CALL U2MESS('A','PETSC_4')
           GOTO 9999
        ELSE
          ZK24(JREFA-1+8)='DECT'
        ENDIF

!       -- DDLS ELIMINES :
        CALL ASSERT(ZK24(JREFA-1+3).NE.'ELIMF')
        IF (ZK24(JREFA-1+3).EQ.'ELIML') CALL MTMCHC(MATAS,'ELIMF')
        CALL ASSERT(ZK24(JREFA-1+3).NE.'ELIML')

      ELSE IF (ACTION.EQ.'RESOUD') THEN
        
        CALL ASSERT(NBSOL.GE.1)
        NOMAT=NOMATS(KPTSC)
        NOSOLV=NOSOLS(KPTSC)
        NONU=NONUS(KPTSC)
        
        CALL ASSERT(SOLVEU.NE.' ')
        CALL ASSERT(SOLVEU.EQ.NOSOLV)
        CALL ASSERT(NONU.EQ.NU)
      ELSE IF (ACTION.EQ.'DETR_MAT') THEN
         NOMAT=NOMATS(KPTSC)
         CALL ASSERT(MATAS.NE.' ')
      ELSE
        CALL ASSERT(.FALSE.)
      END IF

!     4. APPEL A APETSR :
!     ----------------------------------------------

      IF (ACTION.EQ.'RESOUD') THEN
        CALL WKVECT('&&APETSC.TRAVAIL','V V R',NSMDI,JTRAV)
        N4=NSMDI
        DO K=1,NBSOL
          KDEB=(K-1)*NSMDI+1
!           DO I=1,NSMDI
!             ZR(JTRAV-1+I)=RSOLU(KDEB-1+I)
!           END DO
          CALL DCOPY(N4,RSOLU(KDEB),1,ZR(JTRAV),1)
          CALL APETSR(ACTION,KPTSC,ZR(JTRAV),VCINE)
!           DO I=1,NSMDI
!             RSOLU(KDEB-1+I)=ZR(JTRAV-1+I)
!           END DO
          CALL DCOPY(N4,ZR(JTRAV),1,RSOLU(KDEB),1)
        END DO
        CALL JEDETR('&&APETSC.TRAVAIL')
      ELSE
        CALL APETSR(ACTION,KPTSC,RSOLU,VCINE)
      END IF

      IRET = 0

9999  CONTINUE
      CALL JEDEMA()

#else
      CALL U2MESS('F','FERMETUR_10')

#endif

      END

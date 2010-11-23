      SUBROUTINE APETS2(KPTSC)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF APETS2 PETSC  DATE 23/11/2010   AUTEUR DESOZA T.DESOZA 
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
      INTEGER      KPTSC
!--------------------------------------------------------------
!
!---------------------------------------------------------------

#ifdef _HAVE_PETSC

!================================================================
#include "include/finclude/petsc.h"
#include "include/finclude/petscvec.h"
#include "include/finclude/petscmat.h"
#include "include/finclude/petscksp.h"
#include "include/finclude/petscpc.h"
!================================================================
! --- DECLARATIONS NORMALISEES JEVEUX ----------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      INTEGER*4 ZI4
      COMMON /I4VAJE/ZI4(1)
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

      INTEGER   NSMDI,NSMHC,N,NZ,NVALM,NLONG
      INTEGER   JSMDI,JSMHC,JDXI1,JDXI2,JDVAL1,JDVAL2,JVALM,JVALM2
      INTEGER   K,ILIG,JCOL,NZDEB,NZFIN
      INTEGER   LTERM,ITERM,JTERM

      CHARACTER*19 NOMAT,NOSOLV
      CHARACTER*14 NONU
      CHARACTER*4  KBID

      LOGICAL LMNSY

      REAL*8 VALM

      CHARACTER*16 IDXI1,IDXI2,TRANS1,TRANS2
      PARAMETER (IDXI1 ='&&APETS2.IDXI1__')
      PARAMETER (IDXI2 ='&&APETS2.IDXI2__')
      PARAMETER (TRANS1='&&APETS2.TRANS1_')
      PARAMETER (TRANS2='&&APETS2.TRANS2_')

      CHARACTER*32 JEXNUM

!
!     -- ON STOCKE AU PLUS NMXINS INSTANCES DE PETSC
!     -- PLUS PRECISEMENT LES MATRICES, SOLVEURS, ETC
      CHARACTER*19 NOMATS(NMXINS),NOSOLS(NMXINS)
      CHARACTER*14 NONUS(NMXINS)
      Mat      Ap(NMXINS)
      KSP      Kp(NMXINS)
      COMMON /SPETSC/ Ap,Kp,NOMATS,NOSOLS,NONUS
!=================================================================
!  Variables PETSc
!
!  A       - matrix that defines linear system
!  ksp    - KSP context
!  x, b - approx solution, RHS vectors
!
      PetscInt low,high,NEQ,dlow,ierr
      Mat A
!=================================================================
      CALL JEMARQ()

!     -- ON RECUPERE DANS LE COMMON L'INSTANCE COURANTE
      NOMAT=NOMATS(KPTSC)
      NOSOLV=NOSOLS(KPTSC)
      NONU=NONUS(KPTSC)
      A=Ap(KPTSC)

      CALL JEVEUO(NONU//'.SMOS.SMDI','L',JSMDI)
      CALL JELIRA(NONU//'.SMOS.SMDI','LONMAX',NSMDI,KBID)
      CALL JEVEUO(NONU//'.SMOS.SMHC','L',JSMHC)
      CALL JELIRA(NONU//'.SMOS.SMHC','LONMAX',NSMHC,KBID)
      N=NSMDI
      NEQ=N

!     DANS PETSc, LES MATRICES SONT STOCKEES NON SYMETRIQUES
      NZ=ZI(JSMDI-1+N)
      CALL ASSERT(NZ.LE.NSMHC)


      CALL JELIRA(NOMAT//'.VALM','NMAXOC',NVALM,KBID)
      IF (NVALM.EQ.1) THEN
         LMNSY=.FALSE.
      ELSE IF (NVALM.EQ.2) THEN
         LMNSY=.TRUE.
      ELSE
         CALL ASSERT(.FALSE.)
      END IF

      CALL JEVEUO(JEXNUM(NOMAT//'.VALM',1),'L',JVALM)
      CALL JELIRA(JEXNUM(NOMAT//'.VALM',1),'LONMAX',
     &             NLONG,KBID)
      CALL ASSERT(NLONG.EQ.NZ)
      IF (LMNSY) THEN
         CALL JEVEUO(JEXNUM(NOMAT//'.VALM',2),'L',JVALM2)
         CALL JELIRA(JEXNUM(NOMAT//'.VALM',2),'LONMAX',
     &             NLONG,KBID)
         CALL ASSERT(NLONG.EQ.NZ)
      END IF

!     low donne la premiere ligne stockee localement
!     high donne la premiere ligne stockee par le processus (rang+1)
!     ATTENTION ces indices commencent a zero (convention C de PETSc)
      call MatGetOwnershipRange(A,low,high,ierr)

      CALL WKVECT(IDXI1,'V V S',N,JDXI1)
      CALL WKVECT(IDXI2,'V V S',N,JDXI2)
      CALL WKVECT(TRANS1,'V V R',N,JDVAL1)
      CALL WKVECT(TRANS2,'V V R',N,JDVAL2)

      LTERM=0
      ITERM=0
      JTERM=0

C cas ou on possede le premier bloc de lignes
      IF (low.EQ.0) THEN
         LTERM=LTERM+1
         call MatSetValue(A,0,0,ZR(JVALM),INSERT_VALUES,ierr)
         dlow=1
      ELSE
         dlow=0
      END IF

C on commence par s'occuper du nombres de NZ par ligne dans le bloc diagonal
      DO JCOL = low+dlow, high-1
         NZDEB = ZI(JSMDI+JCOL-1) + 1
         NZFIN = ZI(JSMDI+JCOL)
         DO K = NZDEB, NZFIN
            ILIG = ZI4(JSMHC-1+K)
            JTERM=JTERM+1
            LTERM=LTERM+1
            IF (LMNSY) THEN
               VALM=ZR(JVALM2-1+K)
               ZR(JDVAL2+JTERM-1)=VALM
               ZI4(JDXI2+JTERM-1)=ILIG-1
            ELSE
               VALM=ZR(JVALM-1+K)
               ZR(JDVAL2+JTERM-1)=VALM
               ZI4(JDXI2+JTERM-1)=ILIG-1
            END IF
            IF (ILIG.GE.(low+1)) THEN
               ITERM=ITERM+1
               LTERM=LTERM+1
               VALM=ZR(JVALM-1+K)
               ZR(JDVAL1+ITERM-1)=VALM
               ZI4(JDXI1+ITERM-1)=ILIG-1
            END IF
         END DO
         JTERM=JTERM-1
         LTERM=LTERM-1
         call MatSetValues(A,ITERM,ZI4(JDXI1),1,JCOL,
     &                     ZR(JDVAL1),INSERT_VALUES,ierr)
         call MatSetValues(A,1,JCOL,JTERM,ZI4(JDXI2),
     &                     ZR(JDVAL2),INSERT_VALUES,ierr)
         ITERM=0
         JTERM=0
      END DO

C ensuite on complete le tableau du bloc hors diagonal         
      DO JCOL = high, NEQ-1
         NZDEB = ZI(JSMDI+JCOL-1) + 1
         NZFIN = ZI(JSMDI+JCOL)
         DO K = NZDEB, NZFIN
            ILIG = ZI4(JSMHC-1+K)
            IF (ILIG.LT.(low+1)) THEN
               CONTINUE
            ELSE IF (ILIG.LE.high) THEN
               ITERM=ITERM+1
               LTERM=LTERM+1
               VALM=ZR(JVALM-1+K)
               ZR(JDVAL1+ITERM-1)=VALM
               ZI4(JDXI1+ITERM-1)=ILIG-1
            ELSE
               EXIT
            END IF
         END DO
         call MatSetValues(A,ITERM,ZI4(JDXI1),1,JCOL,
     &                     ZR(JDVAL1),INSERT_VALUES,ierr)
         ITERM=0
      END DO
         
      CALL JELIBE(NONU//'.SMOS.SMDI')
      CALL JELIBE(NONU//'.SMOS.SMHC')
      CALL JELIBE(JEXNUM(NOMAT//'.VALM',1))
      IF (LMNSY) CALL JELIBE(JEXNUM(NOMAT//'.VALM',2))

C on n'oublie pas de detruire les tableaux apres avoir alloue correctement
      CALL JEDETR(IDXI1)
      CALL JEDETR(IDXI2)
      CALL JEDETR(TRANS1)
      CALL JEDETR(TRANS2)

      CALL JEDEMA()

#endif

      END

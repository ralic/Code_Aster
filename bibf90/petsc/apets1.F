      SUBROUTINE APETS1(KPTSC)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF APETS1 PETSC  DATE 08/08/2008   AUTEUR DESOZA T.DESOZA 
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

      INTEGER   NSMDI,NSMHC,NZ,NBLLOC
      INTEGER   JSMDI,JSMHC,JIDXD,JIDXO
      INTEGER   I,K,ILIG,JCOL,NBO,NBD,NZDEB,NZFIN

      CHARACTER*19 NOMAT,NOSOLV
      CHARACTER*14 NONU
      CHARACTER*4  KBID

      CHARACTER*16 IDXO, IDXD
      PARAMETER (IDXO  ='&&APETS1.IDXO___')
      PARAMETER (IDXD  ='&&APETS1.IDXD___')

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
      PetscInt low,high,NEQ,ierr
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
      NEQ=NSMDI

!     DANS PETSc, LES MATRICES SONT STOCKEES NON SYMETRIQUES
      NZ=ZI(JSMDI-1+NSMDI)
      CALL ASSERT(NZ.LE.NSMHC)

!     low donne la premiere ligne stockee localement
!     high donne la premiere ligne stockee par le processus (rang+1)
!     ATTENTION ces indices commencent a zero (convention C de PETSc)
      call MatGetOwnershipRange(A,low,high,ierr)

! nb de lignes que l'on stocke localement
      NBLLOC = high - low

! ces deux vecteurs sont les d_nnz et o_nnz a passer a PETSc
      CALL WKVECT(IDXO,'V V S',NBLLOC,JIDXO)
      CALL WKVECT(IDXD,'V V S',NBLLOC,JIDXD)

! initialisation des deux vecteurs         
      DO I = 1, NBLLOC
         ZI4(JIDXO-1+I) = 0
         ZI4(JIDXD-1+I) = 0
      END DO

      IF (low.EQ.0) THEN
         ZI4(JIDXO-1+1) = 0
         ZI4(JIDXD-1+1) = 1
      ELSE
         ZI4(JIDXO-1+1) = ZI(JSMDI+low) - ZI(JSMDI+low-1) - 1
         ZI4(JIDXD-1+1) = 1
      END IF

      NBO = 0
      NBD = 0

! on commence par s'occuper du nombre de NZ par ligne dans le bloc diagonal
      DO JCOL = low+1, high-1
         NZDEB = ZI(JSMDI+JCOL-1) + 1
         NZFIN = ZI(JSMDI+JCOL)
         DO K = NZDEB, NZFIN
            ILIG = ZI(JSMHC-1+K)
            IF (ILIG.LT.(low+1)) THEN
               NBO = NBO + 1
            ELSE
               NBD = NBD + 1
               ZI4(JIDXD-1+(ILIG-low)) = ZI4(JIDXD-1+(ILIG-low)) + 1
            END IF
         END DO
         ZI4(JIDXD-1+(JCOL+1-low)) = ZI4(JIDXD-1+(JCOL+1-low)) + NBD 
     &                                                         - 1
         ZI4(JIDXO-1+(JCOL+1-low)) = ZI4(JIDXO-1+(JCOL+1-low)) + NBO
         NBD = 0
         NBO = 0
      END DO

! ensuite on complete le tableau du bloc hors diagonal         
      DO JCOL = high, NEQ-1
         NZDEB = ZI(JSMDI+JCOL-1) + 1
         NZFIN = ZI(JSMDI+JCOL)
         DO K = NZDEB, NZFIN
            ILIG = ZI(JSMHC-1+K)
            IF (ILIG.LT.(low+1)) THEN
               CONTINUE
            ELSE IF (ILIG.LE.high) THEN
               ZI4(JIDXO-1+(ILIG-low)) = ZI4(JIDXO-1+(ILIG-low)) + 1
            ELSE
               EXIT
            END IF
         END DO
      END DO

      call MatDestroy(A,ierr)
      CALL ASSERT(ierr.EQ.0)
      call MatCreateMPIAIJ(PETSC_COMM_WORLD,PETSC_DECIDE,PETSC_DECIDE
     &                                    ,NEQ,NEQ,
     &                                    PETSC_NULL_INTEGER,ZI4(JIDXD),
     &                                    PETSC_NULL_INTEGER,ZI4(JIDXO),
     &                                    A,ierr)

      CALL ASSERT(ierr.EQ.0)
      Ap(KPTSC)=A
!       call MatMPIAIJSetPreallocation(A,PETSC_NULL_INTEGER,ZI4(JIDXD),
!      &                                 PETSC_NULL_INTEGER,ZI4(JIDXO),
!      &                                 ierr)
!       CALL ASSERT(ierr.EQ.0)

      CALL JELIBE(NONU//'.SMOS.SMDI')
      CALL JELIBE(NONU//'.SMOS.SMHC')

! on n'oublie pas de detruire les tableaux apres avoir alloue correctement
      CALL JEDETR(IDXO)
      CALL JEDETR(IDXD)

! permet de dire a PETSc que les blocs fournis sont "tries" (plus rapide)
      call MatSetOption(A,MAT_ROWS_SORTED,ierr)
      call MatSetOption(A,MAT_COLUMNS_SORTED,ierr)
      
      CALL JEDEMA()

#endif

      END

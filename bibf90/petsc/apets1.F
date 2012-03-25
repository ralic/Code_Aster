      SUBROUTINE APETS1(KPTSC)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF APETS1 PETSC  DATE 26/03/2012   AUTEUR DESOZA T.DESOZA 
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
      INTEGER      KPTSC
!----------------------------------------------------------------
!
!  CREATION DE LA MATRICE PETSC (INSTANCE NUMERO KPTSC)
!  PREALLOCATION
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
! --- DECLARATIONS NORMALISEES JEVEUX ---------------------------
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
!----------------------------------------------------------------
!     AU PLUS 5 MATRICES PETSC SONT GEREES A LA FOIS
      INTEGER      NMXINS
      PARAMETER   (NMXINS=5)
!
!     VARIABLES LOCALES
      INTEGER      NSMDI,NSMHC,NZ,NBLLOC
      INTEGER      JSMDI,JSMHC,JIDXD,JIDXO
      INTEGER      I,K,ILIG,JCOL,NBO,NBD,NZDEB,NZFIN
!
      CHARACTER*19 NOMAT,NOSOLV
      CHARACTER*16 IDXO, IDXD
      CHARACTER*14 NONU
      CHARACTER*4  KBID
!
      PARAMETER   (IDXO  ='&&APETS1.IDXO___')
      PARAMETER   (IDXD  ='&&APETS1.IDXD___')
!
!     COMMUN DE SAUVEGARDE DES INSTANCES
      CHARACTER*19 NOMATS(NMXINS),NOSOLS(NMXINS)
      CHARACTER*14  NONUS(NMXINS)
      Mat              Ap(NMXINS)
      KSP              Kp(NMXINS)
      COMMON /SPETSC/ Ap,Kp,NOMATS,NOSOLS,NONUS
!----------------------------------------------------------------
!     Variables PETSc
      PetscInt low,high,neq,ierr
      Vec tmp
!----------------------------------------------------------------
      CALL JEMARQ()

!     -- LECTURE DU COMMUN
      NOMAT  = NOMATS(KPTSC)
      NOSOLV = NOSOLS(KPTSC)
      NONU   =  NONUS(KPTSC)
!
      CALL JEVEUO(NONU//'.SMOS.SMDI','L',JSMDI)
      CALL JELIRA(NONU//'.SMOS.SMDI','LONMAX',NSMDI,KBID)
      CALL JEVEUO(NONU//'.SMOS.SMHC','L',JSMHC)
      CALL JELIRA(NONU//'.SMOS.SMHC','LONMAX',NSMHC,KBID)
      neq = NSMDI
      NZ = ZI(JSMDI-1+NSMDI)

!     low donne la premiere ligne stockee localement
!     high donne la premiere ligne stockee par le processus (rang+1)
!     ATTENTION ces indices commencent a zero (convention C de PETSc)
      call VecCreateMPI(PETSC_COMM_WORLD,PETSC_DECIDE,neq,tmp,ierr)
      CALL ASSERT(ierr.EQ.0)
      call VecGetOwnershipRange(tmp,low,high,ierr)
      CALL ASSERT(ierr.EQ.0)
      call VecDestroy(tmp,ierr)
      CALL ASSERT(ierr.EQ.0)

!     nb de lignes que l'on stocke localement
      NBLLOC = high - low

!     ces deux vecteurs sont les d_nnz et o_nnz a passer a PETSc
      CALL WKVECT(IDXO,'V V S',NBLLOC,JIDXO)
      CALL WKVECT(IDXD,'V V S',NBLLOC,JIDXD)

!     initialisation des deux vecteurs         
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

!     on commence par s'occuper du nombre de NZ par ligne
!     dans le bloc diagonal
      DO JCOL = low+1, high-1
         NZDEB = ZI(JSMDI+JCOL-1) + 1
         NZFIN = ZI(JSMDI+JCOL)
         DO K = NZDEB, NZFIN
            ILIG = ZI4(JSMHC-1+K)
            IF (ILIG.LT.(low+1)) THEN
               NBO = NBO + 1
            ELSE
               NBD = NBD + 1
               ZI4(JIDXD-1+(ILIG-low)) = ZI4(JIDXD-1+(ILIG-low)) + 1
            END IF
         END DO
         ZI4(JIDXD-1+(JCOL+1-low)) = ZI4(JIDXD-1+(JCOL+1-low)) + NBD - 1
         ZI4(JIDXO-1+(JCOL+1-low)) = ZI4(JIDXO-1+(JCOL+1-low)) + NBO
         NBD = 0
         NBO = 0
      END DO

!     ensuite on complete le tableau du bloc hors diagonal         
      DO JCOL = high, neq-1
         NZDEB = ZI(JSMDI+JCOL-1) + 1
         NZFIN = ZI(JSMDI+JCOL)
         DO K = NZDEB, NZFIN
            ILIG = ZI4(JSMHC-1+K)
            IF (ILIG.LT.(low+1)) THEN
               CONTINUE
            ELSE IF (ILIG.LE.high) THEN
               ZI4(JIDXO-1+(ILIG-low)) = ZI4(JIDXO-1+(ILIG-low)) + 1
            ELSE
               EXIT
            END IF
         END DO
      END DO

      call MatCreateMPIAIJ(PETSC_COMM_WORLD,
     &                     PETSC_DECIDE,PETSC_DECIDE,neq,neq,
     &                     PETSC_NULL_INTEGER,ZI4(JIDXD),
     &                     PETSC_NULL_INTEGER,ZI4(JIDXO),
     &                     Ap(KPTSC),ierr)
      CALL ASSERT(ierr.EQ.0)

!     on n'oublie pas de detruire les tableaux
!     apres avoir alloue correctement
      CALL JEDETR(IDXO)
      CALL JEDETR(IDXD)
      
      CALL JEDEMA()

#endif

      END

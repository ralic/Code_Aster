      SUBROUTINE APALLC(KPTSC)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF APALLC PETSC  DATE 02/10/2012   AUTEUR DESOZA T.DESOZA 
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
!----------------------------------------------------------------
!     AU PLUS 5 MATRICES PETSC SONT GEREES A LA FOIS
      INTEGER      NMXINS
      PARAMETER   (NMXINS=5)
!
!     VARIABLES LOCALES
      INTEGER      RANG,NBPROC
      INTEGER      NSMDI,NSMHC,NZ,TBLOC,NBLLOC
      INTEGER      JSMDI,JSMHC,JIDXD,JIDXO
      INTEGER      I,K,ILIG,JCOL,NBO,NBD,NZDEB,NZFIN
!
      CHARACTER*19 NOMAT,NOSOLV
      CHARACTER*16 IDXO, IDXD
      CHARACTER*14 NONU
      CHARACTER*4  KBID
!
      PARAMETER   (IDXO  ='&&APALLC.IDXO___')
      PARAMETER   (IDXD  ='&&APALLC.IDXD___')
!
!     COMMUN DE SAUVEGARDE DES INSTANCES
      CHARACTER*19 NOMATS(NMXINS),NOSOLS(NMXINS)
      CHARACTER*14  NONUS(NMXINS)
      Mat              Ap(NMXINS)
      KSP              Kp(NMXINS)
      COMMON /SPETSC/ Ap,Kp,NOMATS,NOSOLS,NONUS
!----------------------------------------------------------------
!     Variables PETSc
      PetscInt bs,low,high,neq,ierr
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
!
      CALL APBLOC(NOMAT ,NOSOLV,TBLOC)
      bs = ABS(TBLOC)

!     -- RECUPERE LE RANG DU PROCESSUS ET LE NB DE PROCS
      CALL MPICM0(RANG  ,NBPROC)
!
!     low DONNE LA PREMIERE LIGNE STOCKEE LOCALEMENT
!     high DONNE LA PREMIERE LIGNE STOCKEE PAR LE PROCESSUS DE (RANG+1)
!     *ATTENTION* CES INDICES COMMENCENT A ZERO (CONVENTION C DE PETSc)

!     ON EST OBLIGE DE PASSER PAR UN VECTEUR TEMPORAIRE CONSTRUIT
!     PAR MORCEAUX POUR OBTENIR LE BON DECOUPAGE PAR BLOC
      call VecCreate(PETSC_COMM_WORLD,tmp,ierr)
      CALL ASSERT(ierr.EQ.0)
      call VecSetBlockSize(tmp,bs,ierr)
      CALL ASSERT(ierr.EQ.0)
      call VecSetSizes(tmp,PETSC_DECIDE,neq,ierr)
      CALL ASSERT(ierr.EQ.0)
      call VecSetType(tmp, VECMPI, ierr)
      CALL ASSERT(ierr.EQ.0)

      call VecGetOwnershipRange(tmp,low,high,ierr)
      CALL ASSERT(ierr.EQ.0)
      call VecDestroy(tmp,ierr)
      CALL ASSERT(ierr.EQ.0)

!     NB DE LIGNES QUE L'ON STOCKE LOCALEMENT
      NBLLOC = high - low

!     CES DEUX VECTEURS SONT LES D_NNZ ET O_NNZ A PASSER A PETSc
      CALL WKVECT(IDXO,'V V S',NBLLOC,JIDXO)
      CALL WKVECT(IDXD,'V V S',NBLLOC,JIDXD)

!     INITIALISATION DES DEUX VECTEURS
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

!     ON COMMENCE PAR S'OCCUPER DU NOMBRE DE NZ PAR LIGNE
!     DANS LE BLOC DIAGONAL
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

!     ENSUITE ON COMPLETE LE TABLEAU DU BLOC HORS DIAGONAL
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

      CALL MatCreate(PETSC_COMM_WORLD,Ap(KPTSC),ierr)
      CALL ASSERT(ierr.EQ.0)
      CALL MatSetSizes(Ap(KPTSC),NBLLOC,NBLLOC,neq,neq,ierr)
      CALL ASSERT(ierr.EQ.0)
      IF (NBPROC.EQ.1) THEN
         CALL MatSetType(Ap(KPTSC),MATSEQAIJ,ierr)
         CALL ASSERT(ierr.EQ.0)
         CALL MatSEQAIJSetPreallocation(Ap(KPTSC),
     &                    PETSC_NULL_INTEGER,ZI4(JIDXD),ierr)
         CALL ASSERT(ierr.EQ.0)
      ELSE
         CALL MatSetType(Ap(KPTSC),MATMPIAIJ,ierr)
         CALL ASSERT(ierr.EQ.0)
         CALL MatMPIAIJSetPreallocation(Ap(KPTSC),
     &                    PETSC_NULL_INTEGER,ZI4(JIDXD),
     &                    PETSC_NULL_INTEGER,ZI4(JIDXO),ierr)
         CALL ASSERT(ierr.EQ.0)
      ENDIF

!     AVEC PETSc <= 3.2
!     LE BS DOIT ABSOLUMENT ETRE DEFINI ICI, NE PAS DEPLACER
      call MatSetBlockSize(Ap(KPTSC),bs,ierr)
      CALL ASSERT(ierr.EQ.0)

!     ON N'OUBLIE PAS DE DETRUIRE LES TABLEAUX
!     APRES AVOIR ALLOUE CORRECTEMENT
      CALL JEDETR(IDXO)
      CALL JEDETR(IDXD)

      CALL JEDEMA()

#endif

      END

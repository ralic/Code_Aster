      SUBROUTINE APMATR(KPTSC)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF APMATR PETSC  DATE 02/10/2012   AUTEUR DESOZA T.DESOZA 
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
!  REMPLISSAGE DE LA MATRICE PETSC (INSTANCE NUMERO KPTSC)
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
      INTEGER   NSMDI,NSMHC,N,NZ,NVALM,NLONG
      INTEGER   JSMDI,JSMHC,JDXI1,JDXI2,JDVAL1,JDVAL2,JVALM,JVALM2
      INTEGER   K,ILIG,JCOL,NZDEB,NZFIN
      INTEGER   ITERM,JTERM
!
      CHARACTER*19 NOMAT,NOSOLV
      CHARACTER*16 IDXI1,IDXI2,TRANS1,TRANS2
      CHARACTER*14 NONU
      CHARACTER*4  KBID
!
      LOGICAL LMNSY
!
      REAL*8 VALM
!
      PARAMETER (IDXI1 ='&&APMATR.IDXI1__')
      PARAMETER (IDXI2 ='&&APMATR.IDXI2__')
      PARAMETER (TRANS1='&&APMATR.TRANS1_')
      PARAMETER (TRANS2='&&APMATR.TRANS2_')
!
!     COMMUN DE SAUVEGARDE DES INSTANCES
      CHARACTER*19 NOMATS(NMXINS),NOSOLS(NMXINS)
      CHARACTER*14  NONUS(NMXINS)
      Mat              Ap(NMXINS)
      KSP              Kp(NMXINS)
      COMMON /SPETSC/ Ap,Kp,NOMATS,NOSOLS,NONUS
!----------------------------------------------------------------
!     Variables PETSc
      PetscInt low,high,neq,dlow,ierr
      Mat A
!----------------------------------------------------------------
      CALL JEMARQ()

!     -- LECTURE DU COMMUN
      NOMAT  = NOMATS(KPTSC)
      NOSOLV = NOSOLS(KPTSC)
      NONU   =  NONUS(KPTSC)
      A      =     Ap(KPTSC)
!
      CALL JEVEUO(NONU//'.SMOS.SMDI','L',JSMDI)
      CALL JELIRA(NONU//'.SMOS.SMDI','LONMAX',NSMDI,KBID)
      CALL JEVEUO(NONU//'.SMOS.SMHC','L',JSMHC)
      CALL JELIRA(NONU//'.SMOS.SMHC','LONMAX',NSMHC,KBID)
      N=NSMDI
      neq=N
      NZ=ZI(JSMDI-1+N)

      CALL JELIRA(NOMAT//'.VALM','NMAXOC',NVALM,KBID)
      IF (NVALM.EQ.1) THEN
         LMNSY=.FALSE.
      ELSE IF (NVALM.EQ.2) THEN
         LMNSY=.TRUE.
      ELSE
         CALL ASSERT(.FALSE.)
      END IF

      CALL JEVEUO(JEXNUM(NOMAT//'.VALM',1),'L',JVALM)
      CALL JELIRA(JEXNUM(NOMAT//'.VALM',1),'LONMAX',NLONG,KBID)
      CALL ASSERT(NLONG.EQ.NZ)
      IF (LMNSY) THEN
         CALL JEVEUO(JEXNUM(NOMAT//'.VALM',2),'L',JVALM2)
         CALL JELIRA(JEXNUM(NOMAT//'.VALM',2),'LONMAX',NLONG,KBID)
         CALL ASSERT(NLONG.EQ.NZ)
      END IF

!     low DONNE LA PREMIERE LIGNE STOCKEE LOCALEMENT
!     high DONNE LA PREMIERE LIGNE STOCKEE PAR LE PROCESSUS DE (RANG+1)
!     *ATTENTION* CES INDICES COMMENCENT A ZERO (CONVENTION C DE PETSc)
      call MatGetOwnershipRange(A,low,high,ierr)
      CALL ASSERT(ierr.EQ.0)

      CALL WKVECT(IDXI1,'V V S',N,JDXI1)
      CALL WKVECT(IDXI2,'V V S',N,JDXI2)
      CALL WKVECT(TRANS1,'V V R',N,JDVAL1)
      CALL WKVECT(TRANS2,'V V R',N,JDVAL2)

      ITERM=0
      JTERM=0

!     CAS OU ON POSSEDE LE PREMIER BLOC DE LIGNES
      IF (low.EQ.0) THEN
         call MatSetValue(A,0,0,ZR(JVALM),INSERT_VALUES,ierr)
         dlow=1
      ELSE
         dlow=0
      END IF

!     ON COMMENCE PAR S'OCCUPER DU BLOC DIAGONAL
      DO JCOL = low+dlow, high-1
         NZDEB = ZI(JSMDI+JCOL-1) + 1
         NZFIN = ZI(JSMDI+JCOL)
         DO K = NZDEB, NZFIN
            ILIG = ZI4(JSMHC-1+K)
            JTERM=JTERM+1
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
               VALM=ZR(JVALM-1+K)
               ZR(JDVAL1+ITERM-1)=VALM
               ZI4(JDXI1+ITERM-1)=ILIG-1
            END IF
         END DO
         JTERM=JTERM-1
         call MatSetValues(A,ITERM,ZI4(JDXI1),1,JCOL,
     &                     ZR(JDVAL1),INSERT_VALUES,ierr)
         call MatSetValues(A,1,JCOL,JTERM,ZI4(JDXI2),
     &                     ZR(JDVAL2),INSERT_VALUES,ierr)
         ITERM=0
         JTERM=0
      END DO

!     ENSUITE ON FINIT PAR LE BLOC HORS DIAGONAL
      DO JCOL = high, neq-1
         NZDEB = ZI(JSMDI+JCOL-1) + 1
         NZFIN = ZI(JSMDI+JCOL)
         DO K = NZDEB, NZFIN
            ILIG = ZI4(JSMHC-1+K)
            IF (ILIG.LT.(low+1)) THEN
               CONTINUE
            ELSE IF (ILIG.LE.high) THEN
               ITERM=ITERM+1
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

!     ON N'OUBLIE PAS DE DETRUIRE LES TABLEAUX
!     APRES AVOIR ALLOUE CORRECTEMENT
      CALL JEDETR(IDXI1)
      CALL JEDETR(IDXI2)
      CALL JEDETR(TRANS1)
      CALL JEDETR(TRANS2)

      CALL JEDEMA()

#endif

      END

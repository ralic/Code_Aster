      SUBROUTINE APMAMD(KPTSC)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION
!& MODIF APMAMD PETSC  DATE 21/01/2013   AUTEUR DESOZA T.DESOZA 
! ==================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D              WWW.CODE-ASTER.ORG
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
! RESPONSABLE SELLENET N.SELLENET
      INCLUDE 'jeveux.h'
      INTEGER      KPTSC
!----------------------------------------------------------------
!
!  REMPLISSAGE DE LA MATRICE PETSC (INSTANCE NUMERO KPTSC)
!  DANS LE CAS MATR_DISTRIBUEE
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
      INTEGER   NSMDI,NSMHC,NZ,NVALM,NLONG
      INTEGER   JSMDI,JSMHC,JDXI1,JDXI2,JDVAL1,JDVAL2,JVALM,JVALM2
      INTEGER   K,ILIGL,JCOLL,NZDEB,NZFIN,NUNO1,NUCMP1,NUNO2,NBPROC
      INTEGER   ITERM,JTERM,JCOLG,ILIGG,JNUGLL,NUCMP2,PROCOL,JPRDDL
      INTEGER   JNEQU,NLOC,NGLO,PROLIG,RANG,JNEQUL
!
      CHARACTER*32 JEXNUM
      CHARACTER*19 NOMAT,NOSOLV
      CHARACTER*16 IDXI1,IDXI2,TRANS1,TRANS2
      CHARACTER*14 NONU
      CHARACTER*4  KBID
!
      LOGICAL LMNSY
!
      REAL*8 VALM
!
      PARAMETER (IDXI1 ='&&APETS2.IDXI1__')
      PARAMETER (IDXI2 ='&&APETS2.IDXI2__')
      PARAMETER (TRANS1='&&APETS2.TRANS1_')
      PARAMETER (TRANS2='&&APETS2.TRANS2_')
!
!     COMMUN DE SAUVEGARDE DES INSTANCES
      CHARACTER*19 NOMATS(NMXINS),NOSOLS(NMXINS)
      CHARACTER*14  NONUS(NMXINS)
      Mat              Ap(NMXINS)
      KSP              Kp(NMXINS)
      Vec              b,x
      COMMON /SPETSC/ Ap,Kp,b,x,NOMATS,NOSOLS,NONUS
!----------------------------------------------------------------
!     Variables PETSc
      PetscInt low,high,neql,neqg,ierr
      Mat A
!----------------------------------------------------------------
      CALL JEMARQ()
!
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
      CALL JEVEUO(NONU//'.NUME.NEQU','L',JNEQU)
      CALL JEVEUO(NONU//'.NUML.NEQU','L',JNEQUL)
      CALL JEVEUO(NONU//'.NUML.NLGP','L',JNUGLL)
      CALL JEVEUO(NONU//'.NUML.PDDL','L',JPRDDL)
      CALL MPICM0(RANG,NBPROC)
      NLOC = ZI(JNEQUL)
      NGLO = ZI(JNEQU)
      neql = NLOC
      neqg = NGLO
      NZ=ZI(JSMDI-1+NLOC)
!
      CALL JELIRA(NOMAT//'.VALM','NMAXOC',NVALM,KBID)
      IF (NVALM.EQ.1) THEN
         LMNSY=.FALSE.
      ELSE IF (NVALM.EQ.2) THEN
         CALL ASSERT(.FALSE.)
         LMNSY=.TRUE.
      ELSE
         CALL ASSERT(.FALSE.)
      END IF
!
      CALL JEVEUO(JEXNUM(NOMAT//'.VALM',1),'L',JVALM)
      CALL JELIRA(JEXNUM(NOMAT//'.VALM',1),'LONMAX',NLONG,KBID)
      CALL ASSERT(NLONG.EQ.NZ)
      IF (LMNSY) THEN
         CALL JEVEUO(JEXNUM(NOMAT//'.VALM',2),'L',JVALM2)
         CALL JELIRA(JEXNUM(NOMAT//'.VALM',2),'LONMAX',NLONG,KBID)
         CALL ASSERT(NLONG.EQ.NZ)
      END IF
!
!     low donne la premiere ligne stockee localement
!     high donne la premiere ligne stockee par le processus (rang+1)
!     ATTENTION ces indices commencent a zero (convention C de PETSc)
      call MatGetOwnershipRange(A,low,high,ierr)
      CALL ASSERT(ierr.EQ.0)
!
      CALL WKVECT(IDXI1,'V V S',NLOC,JDXI1)
      CALL WKVECT(IDXI2,'V V S',NLOC,JDXI2)
      CALL WKVECT(TRANS1,'V V R',NLOC,JDVAL1)
      CALL WKVECT(TRANS2,'V V R',NLOC,JDVAL2)
!
      ITERM=0
      JTERM=0
!
!     Recopie de la matrice
!     C'est PETSc qui s'occupe de la recopie des termes vers
!     le bon processeur
      call MatSetValue(A,ZI(JNUGLL)-1,ZI(JNUGLL)-1,ZR(JVALM),
     &                 ADD_VALUES,ierr)
!
      DO JCOLL = 2, NLOC
         NZDEB = ZI(JSMDI+JCOLL-2) + 1
         NZFIN = ZI(JSMDI+JCOLL-1)
         PROCOL = ZI(JPRDDL+JCOLL-1)
         JCOLG = ZI(JNUGLL+JCOLL-1)
         DO K = NZDEB, NZFIN
            ILIGL = ZI4(JSMHC-1+K)
            ILIGG = ZI(JNUGLL-1+ILIGL)
            PROLIG = ZI(JPRDDL-1+ILIGL)
            JTERM=JTERM+1
            VALM=ZR(JVALM-1+K)
            ZR(JDVAL2+JTERM-1)=VALM
            ZI4(JDXI2+JTERM-1)=ILIGG-1
            IF ( ILIGG.NE.JCOLG ) THEN
               ITERM=ITERM+1
               VALM=ZR(JVALM-1+K)
               ZR(JDVAL1+ITERM-1)=VALM
               ZI4(JDXI1+ITERM-1)=ILIGG-1
            ENDIF
         END DO
         call MatSetValues(A,JTERM,ZI4(JDXI2),1,JCOLG-1,
     &                     ZR(JDVAL2),ADD_VALUES,ierr)
         call MatSetValues(A,1,JCOLG-1,ITERM,ZI4(JDXI1),
     &                     ZR(JDVAL1),ADD_VALUES,ierr)
         ITERM=0
         JTERM=0
      END DO
!
      CALL JELIBE(NONU//'.SMOS.SMDI')
      CALL JELIBE(NONU//'.SMOS.SMHC')
      CALL JELIBE(JEXNUM(NOMAT//'.VALM',1))
      IF (LMNSY) CALL JELIBE(JEXNUM(NOMAT//'.VALM',2))
!
      CALL JEDETR(IDXI1)
      CALL JEDETR(IDXI2)
      CALL JEDETR(TRANS1)
      CALL JEDETR(TRANS2)
!
      CALL JEDEMA()
!
#endif
!
      END

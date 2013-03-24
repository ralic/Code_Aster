      SUBROUTINE APSOLU(KPTSC,LMD,RSOLU)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION
!& MODIF APSOLU PETSC  DATE 18/03/2013   AUTEUR BOITEAU O.BOITEAU 
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
! RESPONSABLE DESOZA T.DESOZA
      INCLUDE 'jeveux.h'
      INTEGER  KPTSC
      LOGICAL  LMD
      REAL*8   RSOLU(*)
!----------------------------------------------------------------
!
!  RECOPIE DE LA SOLUTION
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
!     AU PLUS 5 MATRICES PETSC SONT GEREES A LA FOIS
      INTEGER      NMXINS
      PARAMETER   (NMXINS=5)
!
!     VARIABLES LOCALES
      INTEGER      JNEQU,JNEQUL,JNUGLP,JNUGL,JPRDDL,NLOC,NGLO,RANG
      INTEGER      NBPROC,IAUX,NUMGLO,IBID,LMAT
!
      CHARACTER*14 NONU
      CHARACTER*19 NOMAT
      COMPLEX*16   CBID
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
      PetscInt    i,neqg,neql,nuglpe,high,low,ierr
      PetscScalar XX(1)
      PetscOffset xidx
      VecScatter  ctx
      Vec         xgth
!----------------------------------------------------------------
      CALL JEMARQ()
!
!     -- LECTURE DU COMMUN
      NOMAT  = NOMATS(KPTSC)
      NONU   =  NONUS(KPTSC)
!
      IF ( LMD ) THEN
!
        CALL MPICM0(RANG,NBPROC)
!
        CALL JEVEUO(NONU//'.NUML.NLGP','L',JNUGLP)
        CALL JEVEUO(NONU//'.NUML.NULG','L',JNUGL)
        CALL JEVEUO(NONU//'.NUME.NEQU','L',JNEQU)
        CALL JEVEUO(NONU//'.NUML.NEQU','L',JNEQUL)
        CALL JEVEUO(NONU//'.NUML.PDDL','L',JPRDDL)
!
        NLOC = ZI(JNEQUL)
        NGLO = ZI(JNEQU)
        neqg = NGLO
        neql = NLOC
!
        DO IAUX=1,NGLO
          RSOLU(IAUX)=0.D0
        ENDDO
!
        call VecGetOwnershipRange(x,low,high,ierr)
        CALL ASSERT(ierr.EQ.0)
!
!       -- RECOPIE DE DANS RSOLU
        call VecGetArray(x,XX,xidx,ierr)
        CALL ASSERT(ierr.EQ.0)
!
        DO IAUX=1,NLOC
          IF ( ZI(JPRDDL-1+IAUX).EQ.RANG ) THEN
            nuglpe=ZI(JNUGLP-1+IAUX)
            NUMGLO=ZI(JNUGL-1+IAUX)
            RSOLU(NUMGLO)=XX(xidx+nuglpe-low)
          ENDIF
        ENDDO
!
        CALL MPICM1('MPI_SUM','R',NGLO,IBID,IBID,RSOLU,CBID)
!
        call VecRestoreArray(x,XX,xidx,ierr)
        CALL ASSERT(ierr.EQ.0)

      ELSE
!
!       -- RECONSTRUCTION DE LA LA SOLUTION SUR CHAQUE PROC
        call VecScatterCreateToAll(x,ctx,xgth,ierr)
        CALL ASSERT(ierr.EQ.0)
        call VecScatterBegin(ctx,x,xgth,INSERT_VALUES,SCATTER_FORWARD,
     &                       ierr)
        CALL ASSERT(ierr.EQ.0)
        call VecScatterEnd(ctx,x,xgth,INSERT_VALUES,SCATTER_FORWARD,
     &                     ierr)
        CALL ASSERT(ierr.EQ.0)
        call VecScatterDestroy(ctx,ierr)
        CALL ASSERT(ierr.EQ.0)
!
!       -- RECOPIE DE DANS RSOLU
        call VecGetArray(xgth,XX,xidx,ierr)
        CALL ASSERT(ierr.EQ.0)
        CALL JEVEUO(NONU//'.NUME.NEQU','L',JNEQU)
        neqg = ZI(JNEQU)
        DO i=1,neqg
          RSOLU(i)=XX(xidx+i)
        END DO

        call VecRestoreArray(xgth,XX,xidx,ierr)
        CALL ASSERT(ierr.EQ.0)
!
!       -- NETTOYAGE
        call VecDestroy(xgth,ierr)
        CALL ASSERT(ierr.EQ.0)

      ENDIF
!
      CALL JEVEUO(NOMAT//'.&INT','L',LMAT)
!
!     -- REMISE A L'ECHELLE DES LAGRANGES DANS LA SOLUTION
      CALL MRCONL('MULT',LMAT,0,'R',RSOLU,1)
!
      CALL JEDEMA()
!
#endif
!
      END

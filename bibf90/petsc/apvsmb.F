      SUBROUTINE APVSMB(KPTSC,LMD,RSOLU)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION
!& MODIF APVSMB PETSC  DATE 26/02/2013   AUTEUR BOITEAU O.BOITEAU 
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
!  CREATION ET REMPLISSAGE DU SECOND MEMBRE
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
      INTEGER      NSMDI,TBLOC,RANG,NBPROC,JNEQU,JNEQUL,JNUGL,JNUGLP
      INTEGER      JPRDDL,NLOC,NGLO,NDPROP,JCOLL,JINDIC,JVALEU,NUMGLO
      INTEGER*4 COMCOU,MPICOU
!
      CHARACTER*4  KBID
      CHARACTER*14 NONU
      CHARACTER*19 NOMAT,NOSOLV
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
      PetscInt    low,high,bs,i,neq,ierr
      PetscScalar XX(1)
      PetscOffset xidx
!----------------------------------------------------------------
      CALL JEMARQ()
!---- COMMUNICATEUR MPI DE TRAVAIL
      MPICOU=COMCOU(1)
!
!     -- LECTURE DU COMMUN
      NOMAT  = NOMATS(KPTSC)
      NOSOLV = NOSOLS(KPTSC)
      NONU   =  NONUS(KPTSC)
!
!     -- TAILLE DES BLOCS
      CALL APBLOC(NOMAT,NOSOLV,TBLOC)
      bs = ABS(TBLOC)
!
      IF ( LMD ) THEN
        CALL MPICM0(RANG,NBPROC)
        CALL JEVEUO(NONU//'.NUME.NEQU','L',JNEQU)
        CALL JEVEUO(NONU//'.NUML.NEQU','L',JNEQUL)
        CALL JEVEUO(NONU//'.NUML.NULG','L',JNUGL)
        CALL JEVEUO(NONU//'.NUML.NLGP','L',JNUGLP)
        CALL JEVEUO(NONU//'.NUML.PDDL','L',JPRDDL)
        NLOC = ZI(JNEQUL)
        NGLO = ZI(JNEQU)
!
        NDPROP = 0
        DO JCOLL = 0, NLOC-1
          IF ( ZI(JPRDDL+JCOLL).EQ.RANG ) NDPROP = NDPROP+1
        END DO
!
        call VecCreate(MPICOU,b,ierr)
        CALL ASSERT(ierr.EQ.0)
        call VecSetBlockSize(b,bs,ierr)
        CALL ASSERT(ierr.EQ.0)
        call VecSetSizes(b,NDPROP,NGLO,ierr)
        CALL ASSERT(ierr.EQ.0)
        call VecSetType(b,VECMPI,ierr)
        CALL ASSERT(ierr.EQ.0)
!
        CALL WKVECT('&&APVSMB.INDICES','V V S',NLOC,JINDIC)
        CALL WKVECT('&&APVSMB.VALEURS','V V R',NLOC,JVALEU)
        DO JCOLL = 0, NLOC-1
          ZI4(JINDIC+JCOLL) = ZI(JNUGLP+JCOLL)-1
          NUMGLO = ZI(JNUGL+JCOLL)
          ZR(JVALEU+JCOLL) = RSOLU(NUMGLO)
        END DO
        call VecSetValues(b,NLOC,ZI4(JINDIC),
     &                    ZR(JVALEU),ADD_VALUES,ierr)
        CALL JEDETR('&&APVSMB.INDICES')
        CALL JEDETR('&&APVSMB.VALEURS')
        call VecAssemblyBegin(b,ierr)
        CALL ASSERT(ierr.EQ.0)
        call VecAssemblyEnd(b,ierr)
        CALL ASSERT(ierr.EQ.0)
      ELSE
        CALL JELIRA(NONU//'.SMOS.SMDI','LONMAX',NSMDI,KBID)
        neq=NSMDI
!
        call VecCreate(MPICOU,b,ierr)
        CALL ASSERT(ierr.EQ.0)
        call VecSetBlockSize(b,bs,ierr)
        CALL ASSERT(ierr.EQ.0)
        call VecSetSizes(b,PETSC_DECIDE,neq,ierr)
        CALL ASSERT(ierr.EQ.0)
        call VecSetType(b,VECMPI,ierr)
        CALL ASSERT(ierr.EQ.0)
!
        call VecGetOwnershipRange(b,low,high,ierr)
        CALL ASSERT(ierr.EQ.0)
        call VecGetArray(b,XX,xidx,ierr)
        CALL ASSERT(ierr.EQ.0)
!
        DO i=1,high-low
           XX(xidx+i)=RSOLU(low+i)
        END DO
!
        call VecRestoreArray(b,XX,xidx,ierr)
        CALL ASSERT(ierr.EQ.0)
      ENDIF
!
      CALL JEDEMA()
!
#endif
!
      END

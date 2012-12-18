      SUBROUTINE APALMD(KPTSC)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF APALMD PETSC  DATE 18/12/2012   AUTEUR SELLENET N.SELLENET 
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
C RESPONSABLE SELLENET N.SELLENET
      INCLUDE 'jeveux.h'
      INTEGER KPTSC
!----------------------------------------------------------------
!
!  CREATION DE LA MATRICE PETSC (INSTANCE NUMERO KPTSC)
!  PREALLOCATION DANS LE CAS MATR_DISTRIBUEE
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
      INTEGER      RANG,NBPROC,JNBJOI,NBJOIN,JNEQU,JNEQUL,JNUGLL
      INTEGER      NSMDI,NSMHC,NZ,TBLOC,NBLLOC,JPRDDL,NLOC,NGLO,JCOLL
      INTEGER      JSMDI,JSMHC,JIDXD,JIDXO,NDPROP,PROCOL,JIDXDC,JIDXOC
      INTEGER      I,K,ILIG,JCOL,NBO,NBD,NZDEB,NZFIN,JCOLG,ILIGL,JAUX
      INTEGER      PROLIG,ILIGG,IAUX,NUMPRO,JJOINT,JVALEU,NUMLOC
      INTEGER      LGENVO,NUMGLO,IBID,COMPLE
!
      REAL*8       RBID
!
      CHARACTER*4  KBID,CHNBJO
      CHARACTER*8  K8BID
      CHARACTER*14 NONU
      CHARACTER*16 IDXO,IDXD,IDXOC,IDXDC
      CHARACTER*19 NOMAT,NOSOLV
      CHARACTER*24 NOJOIN
!
      PARAMETER   (IDXO  ='&&APALLC.IDXO___')
      PARAMETER   (IDXD  ='&&APALLC.IDXD___')
      PARAMETER   (IDXOC ='&&APALLC.IDXOC__')
      PARAMETER   (IDXDC ='&&APALLC.IDXDC__')
!
!     COMMUN DE SAUVEGARDE DES INSTANCES
      CHARACTER*19 NOMATS(NMXINS),NOSOLS(NMXINS)
      CHARACTER*14  NONUS(NMXINS)
      Mat              Ap(NMXINS)
      KSP              Kp(NMXINS)
      Vec              Bp(NMXINS),x
      COMMON /SPETSC/ Ap,Kp,Bp,x,NOMATS,NOSOLS,NONUS
!----------------------------------------------------------------
!     Variables PETSc
      PetscInt bs,low,high,neql,neqg,ierr
      Vec tmp
!----------------------------------------------------------------
      CALL JEMARQ()
!
      CALL MPICM0(RANG,NBPROC)
!
!     -- LECTURE DU COMMUN
      NOMAT  = NOMATS(KPTSC)
      NOSOLV = NOSOLS(KPTSC)
      NONU   =  NONUS(KPTSC)
!
      CALL JEVEUO(NONU//'.NUML.JOIN','L',JNBJOI)
      CALL JELIRA(NONU//'.NUML.JOIN','LONMAX',NBJOIN,KBID)
!
      CALL JEVEUO(NONU//'.SMOS.SMDI','L',JSMDI)
      CALL JELIRA(NONU//'.SMOS.SMDI','LONMAX',NSMDI,KBID)
      CALL JEVEUO(NONU//'.SMOS.SMHC','L',JSMHC)
      CALL JELIRA(NONU//'.SMOS.SMHC','LONMAX',NSMHC,KBID)
      NZ = ZI(JSMDI-1+NSMDI)
!
      CALL APBLOC(NOMAT ,NOSOLV,TBLOC)
      bs = ABS(TBLOC)
!
      CALL JEVEUO(NONU//'.NUME.NEQU','L',JNEQU)
      CALL JEVEUO(NONU//'.NUML.NEQU','L',JNEQUL)
      CALL JEVEUO(NONU//'.NUML.NLGP','L',JNUGLL)
      CALL JEVEUO(NONU//'.NUML.PDDL','L',JPRDDL)
!
      NLOC = ZI(JNEQUL)
      NGLO = ZI(JNEQU)
      neqg = NGLO
      neql = NLOC
!
      NDPROP = 0
!
      DO JCOLL = 1, NLOC
        PROCOL = ZI(JPRDDL-1+JCOLL)
        IF ( PROCOL.EQ.RANG ) NDPROP = NDPROP+1
      END DO
      call VecCreateMPI(PETSC_COMM_WORLD,NDPROP,neqg,tmp,ierr)
      CALL ASSERT(ierr.EQ.0)
      call VecGetOwnershipRange(tmp,low,high,ierr)
      CALL ASSERT(ierr.EQ.0)
!
      CALL WKVECT(IDXD,'V V S',NDPROP,JIDXD)
      CALL WKVECT(IDXO,'V V S',NDPROP,JIDXO)
      CALL WKVECT(IDXDC,'V V S',NLOC,JIDXDC)
      CALL WKVECT(IDXOC,'V V S',NLOC,JIDXOC)
!
      JCOLG = ZI(JNUGLL)
      IF ( ZI(JPRDDL).EQ.RANG ) THEN
        ZI4(JIDXD+JCOLG-low-1) = ZI4(JIDXD+JCOLG-low-1)+1
      ELSE
        ZI4(JIDXDC) = ZI4(JIDXDC)+1
      ENDIF
!
!     ON COMMENCE PAR NOTER DDL PAR DDL LE NOMBRE DE TERMES POSSEDES
!     ET CEUX QU'IL FAUDRA ENVOYER AUX AUTRES PROCESSEURS
      DO JCOLL = 2, NLOC
        NZDEB = ZI(JSMDI+JCOLL-2) + 1
        NZFIN = ZI(JSMDI+JCOLL-1)
        PROCOL = ZI(JPRDDL+JCOLL-1)
        JCOLG = ZI(JNUGLL+JCOLL-1)
        DO K = NZDEB, NZFIN
          ILIGL = ZI4(JSMHC-1+K)
          PROLIG = ZI(JPRDDL-1+ILIGL)
          ILIGG = ZI(JNUGLL-1+ILIGL)
!         SOIT LA COLONNE ET LA LIGNE APPARTIENNENT AU PROC COURANT
!         AUQUEL CAS, ON S'EN PREOCCUPE POUR L'ALLOCATION
          IF ( PROCOL.EQ.RANG.AND.PROLIG.EQ.RANG ) THEN
            ZI4(JIDXD+ILIGG-low-1) = ZI4(JIDXD+ILIGG-low-1)+1
            IF ( ILIGG.NE.JCOLG ) THEN
              ZI4(JIDXD+JCOLG-low-1) = ZI4(JIDXD+JCOLG-low-1)+1
            ENDIF
!         SOIT ILS N'APPARTIENNENT PAS AU PROC COURANT TOUT LES
!         DEUX, DANS CE CAS ON LES OUBLIE
          ELSEIF ( PROCOL.NE.RANG.AND.PROLIG.NE.RANG ) THEN
            IF ( PROCOL.EQ.PROLIG ) THEN
              ZI4(JIDXDC+ILIGL-1) = ZI4(JIDXDC+ILIGL-1)+1
              IF ( ILIGG.NE.JCOLG ) THEN
                ZI4(JIDXDC+JCOLL-1) = ZI4(JIDXDC+JCOLL-1)+1
              ENDIF
            ELSE
              ZI4(JIDXOC+ILIGL-1) = ZI4(JIDXOC+ILIGL-1)+1
              IF ( ILIGG.NE.JCOLG ) THEN
                ZI4(JIDXOC+JCOLL-1) = ZI4(JIDXOC+JCOLL-1)+1
              ENDIF
            ENDIF
!         SOIT L'UN DES DEUX APPARTIENT AU PROC COURANT
!         DANS CE CAS, ON LE COMPTE POUR L'ALLOCATION
!         OU ON PREVIENT L'AUTRE PROC
          ELSE
            IF ( PROCOL.EQ.RANG ) THEN
              ZI4(JIDXO+JCOLG-low-1) = ZI4(JIDXO+JCOLG-low-1)+1
              ZI4(JIDXOC+ILIGL-1) = ZI4(JIDXOC+ILIGL-1)+1
            ELSE
              ZI4(JIDXO+ILIGG-low-1) = ZI4(JIDXO+ILIGG-low-1)+1
              ZI4(JIDXOC+JCOLL-1) = ZI4(JIDXOC+JCOLL-1)+1
            ENDIF
          END IF
        END DO
      END DO
!
      DO IAUX=1,NBJOIN
        NUMPRO=ZI(JNBJOI+IAUX-1)
        IF ( NUMPRO.NE.-1 ) THEN
        CALL CODENT(IAUX,'G',CHNBJO)
!
        NOJOIN=NONU//'.NUML.'//CHNBJO
        CALL JEVEUO(NOJOIN,'L',JJOINT)
        CALL JELIRA(NOJOIN,'LONMAX',LGENVO,K8BID)
        IF ( LGENVO.GT.0 ) THEN
          CALL WKVECT('&&CPYSOL.TMP1','V V S',LGENVO,JVALEU)
!
          IF ( NUMPRO.GT.RANG ) THEN
            CALL MPIPPV('MPI_RECV','I4',LGENVO,IBID,
     &                  ZI4(JVALEU),RBID,NUMPRO,IAUX)
            DO JAUX=1,LGENVO
              NUMLOC=ZI(JJOINT+JAUX-1)
              NUMGLO=ZI(JNUGLL+NUMLOC-1)
              ZI4(JIDXO+NUMGLO-low-1)=ZI4(JIDXO+NUMGLO-low-1)+
     &                                ZI4(JVALEU+JAUX-1)
            ENDDO
!
            CALL MPIPPV('MPI_RECV','I4',LGENVO,IBID,
     &                  ZI4(JVALEU),RBID,NUMPRO,IAUX)
            DO JAUX=1,LGENVO
              NUMLOC=ZI(JJOINT+JAUX-1)
              NUMGLO=ZI(JNUGLL+NUMLOC-1)
              ZI4(JIDXD+NUMGLO-low-1)=ZI4(JIDXD+NUMGLO-low-1)+
     &                                ZI4(JVALEU+JAUX-1)
            ENDDO
          ELSEIF ( NUMPRO.LT.RANG ) THEN
            DO JAUX=1,LGENVO
              NUMLOC=ZI(JJOINT+JAUX-1)
              NUMGLO=ZI(JNUGLL+NUMLOC-1)
              ZI4(JVALEU+JAUX-1)=ZI4(JIDXOC+NUMLOC-1)
            ENDDO
            CALL MPIPPV('MPI_SEND','I4',LGENVO,IBID,
     &                  ZI4(JVALEU),RBID,NUMPRO,IAUX)
!
            DO JAUX=1,LGENVO
              NUMLOC=ZI(JJOINT+JAUX-1)
              NUMGLO=ZI(JNUGLL+NUMLOC-1)
              ZI4(JVALEU+JAUX-1)=ZI4(JIDXDC+NUMLOC-1)
            ENDDO
            CALL MPIPPV('MPI_SEND','I4',LGENVO,IBID,
     &                  ZI4(JVALEU),RBID,NUMPRO,IAUX)
          ELSE
            CALL ASSERT(.FALSE.)
          ENDIF
          CALL JEDETR('&&CPYSOL.TMP1')
        ENDIF
        ENDIF
      ENDDO
!
      COMPLE=NGLO-NDPROP
      DO IAUX=1,NDPROP
        ZI4(JIDXD+IAUX-1)=MIN(ZI4(JIDXD+IAUX-1),NDPROP)
        ZI4(JIDXO+IAUX-1)=MIN(ZI4(JIDXO+IAUX-1),COMPLE)
      ENDDO
!
      CALL MatCreate(PETSC_COMM_WORLD,Ap(KPTSC),ierr)
      CALL ASSERT(ierr.EQ.0)
      CALL MatSetSizes(Ap(KPTSC),NDPROP,NDPROP,neqg,neqg,ierr)
      CALL ASSERT(ierr.EQ.0)
      CALL MatSetType(Ap(KPTSC),MATMPIAIJ,ierr)
      CALL ASSERT(ierr.EQ.0)
      CALL MatMPIAIJSetPreallocation(Ap(KPTSC),
     &                    PETSC_NULL_INTEGER,ZI4(JIDXD),
     &                    PETSC_NULL_INTEGER,ZI4(JIDXO),ierr)
      CALL ASSERT(ierr.EQ.0)
      call MatSetBlockSize(Ap(KPTSC),bs,ierr)
      CALL ASSERT(ierr.EQ.0)
!
      CALL JEDETR(IDXD)
      CALL JEDETR(IDXO)
      CALL JEDETR(IDXDC)
      CALL JEDETR(IDXOC)
!
      CALL JEDEMA()
!
#endif
!
      END

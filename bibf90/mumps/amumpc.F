      SUBROUTINE AMUMPC(ACTION,KXMPS,CSOLU,VCINE,NBSOL,IRET)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION
!& MODIF AMUMPC MUMPS  DATE 30/09/2008   AUTEUR REZETTE C.REZETTE 
! ==================================================================
! COPYRIGHT (C) 1991 - 2005  EDF R&D              WWW.CODE-ASTER.ORG
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
C--------------------------------------------------------------
C OBJET: DRIVER EN MODE COMPLEXE DE LA RESOLUTION DE SYSTEMES LINEAIRES
C        VIA MUMPS
C
C IN : ACTION :
C     /'PRERES'  : POUR DEMANDER LA FACTORISATION
C     /'RESOUD'  : POUR DEMANDER LA DESCENTE/REMONTEE
C     /'DETR_MAT': POUR DEMANDER LA DESTRUCTION DE L'INSTANCE MUMPS
C                  ASSOCIEE A UNE MATRICE
C
C IN : KXMPS (I)   : INDICE DE L'INSTANCE MUMPS DANS DMPS
C VAR: CSOLU (C)   : EN ENTREE : VECTEUR SECOND MEMBRE (COMPLEXE)
C                    EN SORTIE : VECTEUR SOLUTION (COMPLEXE)
C            (SI ACTION=RESOUD)
C IN : VCINE (K19) : NOM DU CHAM_NO DE CHARGEMENT CINEMATIQUE
C            (SI ACTION=RESOUD)
C OUT : IRET (I) : CODE_RETOUR :
C            0 : OK
C            2 : MATRICE NUMERIQUEMENT SINGULIERE
C IN  : NBSOL  : NBRE DE SYSTEMES A RESOUDRE
C---------------------------------------------------------------
      CHARACTER*(*) ACTION
      CHARACTER*19  VCINE,NOSOLV
      INTEGER       IRET,NBSOL,KXMPS
      COMPLEX*16 CSOLU(*)

#ifdef _HAVE_MUMPS

C================================================================
      INCLUDE 'mpif.h'
      INCLUDE 'dmumps_struc.h'
      INCLUDE 'zmumps_struc.h'
C================================================================
C --- DECLARATIONS NORMALISEES JEVEUX ----------------------------
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
C================================================================
      INTEGER    NMXINS
      PARAMETER (NMXINS=5)
      TYPE (DMUMPS_STRUC) , TARGET  :: DMPS(NMXINS)
      TYPE (ZMUMPS_STRUC) , TARGET  :: ZMPS(NMXINS)
      TYPE (ZMUMPS_STRUC) , POINTER :: XMPSK
      INTEGER      JSLVK,JSLVR,RANG,IFMUMP,NBPROC,NIV,IFM,
     &             MONIT(12)
      CHARACTER*1  ROUCS(NMXINS),ROUC,TYPE
      CHARACTER*4  ETAMS(NMXINS),IMPR,ETAM
      CHARACTER*14 NONUS(NMXINS),NONU
      CHARACTER*19 NOMATS(NMXINS),NOMAT,NOSOLS(NMXINS)
      CHARACTER*24 KMONIT(12)
      REAL*8       EPSMAX,VALR(2),TEMPS(6),RDESQ,RBID
      LOGICAL      LQUALI,ELI2LG,LDIST,LDEBUG
      COMMON /SMUMPS/ DMPS,ZMPS,NONUS,NOMATS,NOSOLS,ETAMS,ROUCS
C=================================================================
      CALL JEMARQ()

C       ------------------------------------------------
C        INITS
C       ------------------------------------------------
C --- ON DESACTIVE LA LEVEE D'EXCEPTION FPE DANS LA BIBLIOTHEQUE MKL
C --  CAR CES EXCEPTIONS NE SONT PAS JUSTIFIEES
      CALL MATFPE(-1)
      CALL INFNIV(IFM,NIV)
C      WRITE(IFM,*)'DEBUT AMUMPC'
C      CALL JXVERI('MESSAGE','DEBUT AMUMPC')
C --- PARAMETRE POUR DEBUGGAGE
      LDEBUG=.TRUE.
      LDEBUG=.FALSE.

C --- TYPE DE SYSTEME: REEL OU COMPLEXE
      TYPE='C'
      CALL ASSERT(KXMPS.GT.0)
      CALL ASSERT(KXMPS.LE.NMXINS)
      NOMAT=NOMATS(KXMPS)
      NOSOLV=NOSOLS(KXMPS)
      NONU=NONUS(KXMPS)
      ETAM=ETAMS(KXMPS)
      ROUC=ROUCS(KXMPS)
      CALL ASSERT(ROUC.EQ.TYPE)
      XMPSK=>ZMPS(KXMPS)
      IRET=0

C --- IMPR : PARAMETRE POUR IMPRIMER LA MATRICE OU LE SECOND MEMBRE
C            SUR L'UNITE IFMUMP (POUR RESOLUTION EXTERNE)
      IMPR='OUI'
      IMPR='NON'
      IFMUMP=17

C --- RDESQ: TAUX DE DESEQUILIBRAGE MAX. POUR PROFILING ACTIVE
C --- AVEC INFO=2
      RDESQ=10.D0

C --- CNTL(2)=1.D-13 POUR ETRE PLUS SUR DE FAIRE DES ITERATIONS
C     DE RAFINEMENT
      XMPSK%CNTL(2) = 1.D-14

C --- L'UTILISATEUR VEUT-IL UNE ESTIMATION DE LA QUALITE DE LA SOL ?
C --- => LQUALI
      CALL JEVEUO(NOSOLV//'.SLVR','L',JSLVR)
      EPSMAX=ZR(JSLVR-1+2)
      LQUALI=(EPSMAX.GT.0.D0)

      CALL JEVEUO(NOSOLV//'.SLVK','L',JSLVK)
C --- POUR "ELIMINER" LE 2EME LAGRANGE :
      ELI2LG=ZK24(JSLVK-1+6)(1:3).EQ.'OUI'

C --- MUMPS PARALLELE DISTRIBUE ?
      LDIST=.FALSE.
      LDIST=ZK24(JSLVK-1+7)(1:10).EQ.'DISTRIBUE_'
      RANG=XMPSK%MYID
      NBPROC=XMPSK%NPROCS

C --- POUR MONITORING
      CALL AMUMPT(0,KMONIT,MONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,RDESQ,
     &            TYPE)

C     ------------------------------------------------
C     ------------------------------------------------
      IF (ACTION(1:6).EQ.'PRERES') THEN
C     ------------------------------------------------
C     ------------------------------------------------

C       ------------------------------------------------
C        INITIALISATION DE L'OCCURENCE MUMPS KXMPS:
C       ------------------------------------------------
        CALL AMUMPI(0,LQUALI,LDIST,KXMPS,TYPE)
        CALL ZMUMPS(XMPSK)
        RANG=XMPSK%MYID
        NBPROC=XMPSK%NPROCS

C       ------------------------------------------------
C        CHOIX ICNTL VECTEUR DE PARAMETRES POUR MUMPS :
C       ------------------------------------------------
        CALL AMUMPI(2,LQUALI,LDIST,KXMPS,TYPE)

C       ------------------------------------------------
C        REMPLISSAGE DE LA MATRICE MUMPS :
C       ------------------------------------------------
        CALL AMUMPM(RANG,LDIST,KXMPS,MONIT,IMPR,IFMUMP,ELI2LG,TYPE)

C       ------------------------------------------------
C        ANALYSE MUMPS:
C       ------------------------------------------------
        IF (LDEBUG) WRITE(IFM,*)'AVANT ANALYSE MUMPS'
        CALL AMUMPT(2,KMONIT,MONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,
     &              RDESQ,TYPE)
        XMPSK%JOB = 1
        CALL ZMUMPS(XMPSK)
        CALL AMUMPT(4,KMONIT,MONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,
     &                RDESQ,TYPE)
        IF (LDEBUG) WRITE(IFM,*)'APRES ANALYSE MUMPS'

C       ------------------------------------------------
C        GESTION ERREURS ET MENAGE ASTER:
C       ------------------------------------------------
        IF (XMPSK%INFOG(1).EQ.0) THEN
C              -- C'EST OK
        ELSE IF ((XMPSK%INFOG(1).EQ.-5).OR.(XMPSK%INFOG(1).EQ.-7)) THEN
          CALL U2MESS('F','FACTOR_64')
        ELSE
          IF (XMPSK%INFOG(1).LT.0) THEN
            CALL U2MESI('F','FACTOR_55',1,XMPSK%INFOG(1))
          ELSE
            CALL U2MESI('A','FACTOR_55',1,XMPSK%INFOG(1))
          ENDIF
        ENDIF
C        IF (RANG.EQ.0) CALL ASSERT(XMPSK%INFOG(1).EQ.0)
C --- VERIFICATION DU RENUMEROTEUR EFFECTIVEMENT UTILISE
        IF(ZK24(JSLVK-1+4).NE.'AUTO' .AND.
     &     XMPSK%ICNTL(7).NE.XMPSK%INFOG(7))
     &     CALL U2MESK('A','FACTOR_50',1,ZK24(JSLVK-1+4))

C       ------------------------------------------------
C        FACTORISATION NUMERIQUE MUMPS:
C       ------------------------------------------------
        IF (LDEBUG) WRITE(IFM,*)'AVANT FACTORISATION MUMPS'
        XMPSK%JOB = 2
        CALL ZMUMPS(XMPSK)
        CALL AMUMPT(6,KMONIT,MONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,
     &              RDESQ,TYPE)
        IF (LDEBUG) WRITE(IFM,*)'APRES FACTORISATION MUMPS'

C       ------------------------------------------------
C        GESTION ERREURS ET MENAGE ASTER:
C       ------------------------------------------------
        IF (XMPSK%INFOG(1).EQ.0) THEN
C              -- C'EST OK
        ELSE IF (XMPSK%INFOG(1).EQ.-9) THEN
          CALL U2MESS('F','FACTOR_53')
        ELSE IF (XMPSK%INFOG(1).EQ.-10) THEN
          IRET=2
        ELSE IF (XMPSK%INFOG(1).EQ.-13) THEN
          CALL U2MESS('F','FACTOR_54')
        ELSE IF (XMPSK%INFOG(1).EQ.-37) THEN
          CALL U2MESS('F','FACTOR_65')
       ELSE IF (XMPSK%INFOG(1).EQ.-90) THEN
C     PROBLEME DU TRAITEMENT OUT-OF-CORE DE MUMPS             
          CALL U2MESS('F','FACTOR_66')
        ELSE
          IF (XMPSK%INFOG(1).LT.0) THEN
            CALL U2MESI('F','FACTOR_55',1,XMPSK%INFOG(1))
          ELSE
            CALL U2MESI('A','FACTOR_55',1,XMPSK%INFOG(1))
          ENDIF
        ENDIF
        IF ((( RANG.EQ.0).AND.(.NOT.LDIST)).OR.(LDIST)) THEN
          IF (.NOT.LQUALI) THEN
            IF (LDIST) THEN
              DEALLOCATE(XMPSK%A_loc)
              DEALLOCATE(XMPSK%IRN_loc)
              DEALLOCATE(XMPSK%JCN_loc)
            ELSE
              DEALLOCATE(XMPSK%A)
              DEALLOCATE(XMPSK%IRN)
              DEALLOCATE(XMPSK%JCN)
            ENDIF
          ENDIF
        ENDIF

C     ------------------------------------------------
C     ------------------------------------------------
      ELSE IF (ACTION(1:6).EQ.'RESOUD') THEN
C     ------------------------------------------------
C     ------------------------------------------------

C       ------------------------------------------------
C        PRETRAITEMENTS ASTER DU/DES SECOND(S) MEMBRE(S) :
C       ------------------------------------------------
        IF (LDEBUG) WRITE(IFM,*)'AVANT PRETRAITEMENT SM'
        CALL AMUMPP(0,NBSOL,KXMPS,LDIST,TYPE,IMPR,IFMUMP,ELI2LG,
     &              RBID,CSOLU,VCINE)

C       ------------------------------------------------
C        RESOLUTION MUMPS :
C       ------------------------------------------------
        IF (LDEBUG) WRITE(IFM,*)'AVANT RESOLUTION MUMPS'
        CALL AMUMPT(8,KMONIT,MONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,
     &              RDESQ,TYPE)
        XMPSK%JOB = 3
        CALL ZMUMPS(XMPSK)
        CALL AMUMPT(10,KMONIT,MONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,
     &              RDESQ,TYPE)
        IF (LDEBUG) WRITE(IFM,*)'APRES RESOLUTION MUMPS'

C       ------------------------------------------------
C        GESTION ERREURS ET MENAGE ASTER:
C       ------------------------------------------------
        IF (XMPSK%INFOG(1).EQ.0) THEN
C              -- C'EST OK
        ELSE IF ((XMPSK%INFOG(1).EQ.8).AND.(LQUALI)) THEN
          CALL U2MESI('A','FACTOR_62',1,XMPSK%ICNTL(10))
        ELSE IF (XMPSK%INFOG(1).LT.0) THEN
          CALL U2MESI('F','FACTOR_55',1,XMPSK%INFOG(1))
        ELSE IF (XMPSK%INFOG(1).EQ.4) THEN
C          -- PERMUTATION DE COLONNES, XMPSK%JCN MODIFIE VOLONTAIREMENT
C          -- PAR MUMPS. IL NE FAUT DONC PAS LE MANIPULER TEL QUE
C          -- PAS GRAVE POUR ASTER.
        ELSE
          CALL U2MESI('A','FACTOR_55',1,XMPSK%INFOG(1))
        ENDIF
C --- CONTROLE DE L'ERREUR SUR LA SOLUTION :
        IF (LQUALI) THEN
          IF (XMPSK%RINFOG(9).GT.EPSMAX) THEN
            VALR(1)=XMPSK%RINFOG(9)
            VALR(2)=EPSMAX
            CALL U2MESR('F','FACTOR_57',2,VALR)
          ENDIF
        ENDIF

C       ------------------------------------------------
C        POST-TRAITEMENTS ASTER DE/DES (LA) SOLUTION(S) :
C       ------------------------------------------------
        CALL AMUMPP(2,NBSOL,KXMPS,LDIST,TYPE,IMPR,IFMUMP,ELI2LG,
     &              RBID,CSOLU,VCINE)
        IF (LDEBUG) WRITE(IFM,*)'APRES POST-TRAITEMENTS SOL'

C       ------------------------------------------------
C        AFFICHAGE DU MONITORING :
C       ------------------------------------------------
        CALL AMUMPT(12,KMONIT,MONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,
     &              RDESQ,TYPE)

C     ------------------------------------------------
C     ------------------------------------------------
      ELSE IF (ACTION.EQ.'DETR_MAT') THEN
C     ------------------------------------------------
C     ------------------------------------------------

C       ------------------------------------------------
C        MENAGE ASTER ET MUMPS:
C       ------------------------------------------------
        IF (NOMATS(KXMPS).NE.' ') THEN
          IF ((( RANG.EQ.0).AND.(.NOT.LDIST)).OR.(LDIST)) THEN
            CALL JEVEUO(NOSOLS(KXMPS)//'.SLVR','L',JSLVR)
            EPSMAX=ZR(JSLVR-1+2)
            LQUALI=(EPSMAX.GT.0.D0)
            IF (LQUALI) THEN
              IF (LDIST) THEN
                DEALLOCATE(XMPSK%A_loc)
                DEALLOCATE(XMPSK%IRN_loc)
                DEALLOCATE(XMPSK%JCN_loc)
              ELSE
                DEALLOCATE(XMPSK%A)
                DEALLOCATE(XMPSK%IRN)
                DEALLOCATE(XMPSK%JCN)
              ENDIF
            ENDIF
          ENDIF
          ETAMS(KXMPS)=' '
          NONUS(KXMPS)=' '
          NOMATS(KXMPS)=' '
          NOSOLS(KXMPS)=' '
          ROUCS(KXMPS)=' '
          XMPSK%JOB = -2
          CALL ZMUMPS(XMPSK)
        ENDIF
      ENDIF

C     -- ON REACTIVE LA LEVEE D'EXCEPTION
      CALL MATFPE(1)
C      WRITE(IFM,*)'FIN AMUMPC'
C      CALL JXVERI('MESSAGE','FIN AMUMPC')
      CALL JEDEMA()

#endif
      END

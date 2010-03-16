      SUBROUTINE AMUMPZ(ACTION,KXMPS,CSOLU,VCINE,NBSOL,IRET,
     &                  IMPR,IFMUMP)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION
!&           CONFIGURATION MANAGEMENT OF EDF VERSION                   
!& MODIF AMUMPZ MUMPS  DATE 15/03/2010   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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
C        VIA MUMPS (EN DOUBLE PRECISION AUSSI POUR MUMPS)
C
C IN : ACTION :
C     /'PRERES'  : POUR DEMANDER LA FACTORISATION
C     /'RESOUD'  : POUR DEMANDER LA DESCENTE/REMONTEE
C     /'DETR_MAT/OCC': POUR DEMANDER LA DESTRUCTION DE L'INSTANCE MUMPS
C                  ASSOCIEE A UNE MATRICE
C
C IN : KXMPS (I)   : INDICE DE L'INSTANCE MUMPS DANS ZMPS
C VAR: CSOLU (C)   : EN ENTREE : VECTEUR SECOND MEMBRE (COMPLEXE)
C                    EN SORTIE : VECTEUR SOLUTION (COMPLEXE)
C            (SI ACTION=RESOUD)
C IN : VCINE (K19) : NOM DU CHAM_NO DE CHARGEMENT CINEMATIQUE
C            (SI ACTION=RESOUD)
C OUT : IRET (I) : CODE_RETOUR :
C            0 : OK
C            2 : MATRICE NUMERIQUEMENT SINGULIERE
C IN  : NBSOL  : NBRE DE SYSTEMES A RESOUDRE
C IN  : IMPR,IFMUMP : PARAMETRES POUR SORTIE FICHIER MATRICE CF AMUMPH
C---------------------------------------------------------------
C RESPONSABLE BOITEAU

      CHARACTER*(*) ACTION
      CHARACTER*14  IMPR
      CHARACTER*19  VCINE,NOSOLV
      INTEGER       IRET,NBSOL,KXMPS,IFMUMP
      COMPLEX*16    CSOLU(*)

#ifdef _HAVE_MUMPS

C================================================================
      INCLUDE 'mpif.h'
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
      TYPE (ZMUMPS_STRUC) , TARGET  :: ZMPS(NMXINS)
      TYPE (ZMUMPS_STRUC) , POINTER :: ZMPSK
      INTEGER      JSLVK,JSLVR,RANG,NBPROC,NIV,IFM,IBID,IETDEB,
     &             IETRAT,JREFA,NPREC,JSLVI,N,IPIV,I
      CHARACTER*1  ROUCS(NMXINS),ROUC,TYPE,PREC,PRECS(NMXINS)
      CHARACTER*4  ETAMS(NMXINS),ETAM
      CHARACTER*14 NONUS(NMXINS),NONU
      CHARACTER*19 NOMATS(NMXINS),NOMAT,NOSOLS(NMXINS)
      CHARACTER*24 KMONIT(12),KPIV
      REAL*8       EPSMAX,VALR(2),RDESQ,RCTDEB,RBID(1),TEMPS(6),EPSMAT
      LOGICAL      LQUALI,ELI2LG,LDIST,LRESOL,LMD
      COMMON /MUMPSZ/ ZMPS
      COMMON /MUMPSH/ NONUS,NOMATS,NOSOLS,ETAMS,ROUCS,PRECS
C=================================================================
      CALL JEMARQ()

C       ------------------------------------------------
C        INITS
C       ------------------------------------------------
C --- ON DESACTIVE LA LEVEE D'EXCEPTION FPE DANS LA BIBLIOTHEQUE MKL
C --  CAR CES EXCEPTIONS NE SONT PAS JUSTIFIEES
      CALL MATFPE(-1)
      CALL INFNIV(IFM,NIV)

C --- PARAMETRE POUR IMPRESSION FICHIER
      LRESOL=((IMPR(1:3).EQ.'NON').OR.(IMPR(1:9).EQ.'OUI_SOLVE'))

C --- TYPE DE SYSTEME: REEL OU COMPLEXE
      TYPE='Z'
      CALL ASSERT(KXMPS.GT.0)
      CALL ASSERT(KXMPS.LE.NMXINS)
      NOMAT=NOMATS(KXMPS)
      NOSOLV=NOSOLS(KXMPS)
      NONU=NONUS(KXMPS)
      ETAM=ETAMS(KXMPS)
      ROUC=ROUCS(KXMPS)
      PREC=PRECS(KXMPS)
      CALL ASSERT((ROUC.EQ.'C').AND.(PREC.EQ.'D'))
      ZMPSK=>ZMPS(KXMPS)
      IRET=0

C --- RDESQ: TAUX DE DESEQUILIBRAGE MAX. POUR PROFILING ACTIVE
C --- AVEC INFO=2
      RDESQ=10.D0

C --- L'UTILISATEUR VEUT-IL UNE ESTIMATION DE LA QUALITE DE LA SOL ?
C --- => LQUALI
      CALL JEVEUO(NOSOLV//'.SLVR','L',JSLVR)
      EPSMAX=ZR(JSLVR-1+2)
      LQUALI=(EPSMAX.GT.0.D0)

      CALL JEVEUO(NOSOLV//'.SLVK','L',JSLVK)
C --- POUR "ELIMINER" LE 2EME LAGRANGE :
      ELI2LG=ZK24(JSLVK-1+6)(1:3).EQ.'OUI'

C --- PARAMETRE NPREC
      CALL JEVEUO(NOSOLV//'.SLVI','L',JSLVI)
      NPREC=ZI(JSLVI)
      
C --- MUMPS PARALLELE DISTRIBUE ?
      CALL JEVEUO(NOMAT//'.REFA','L',JREFA)
      LDIST=(ZK24(JREFA-1+11).NE.'MPI_COMPLET')
      RANG=ZMPSK%MYID
      NBPROC=ZMPSK%NPROCS

C --- MATRICE ASTER DISTRIBUEE ?
      LMD = .FALSE.
      LMD = ZK24(JSLVK-1+10)(1:3).EQ.'OUI'

C --- FILTRAGE DE LA MATRICE DONNEE A MUMPS (UNIQUEMENT NON LINEAIRE)
      EPSMAT=ZR(JSLVR-1+1)
      
C --- POUR MONITORING
      CALL AMUMPT(0,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,RDESQ,TYPE,
     &            IETDEB,IETRAT,RCTDEB,LDIST)


C     ------------------------------------------------
C     ------------------------------------------------
      IF (ACTION(1:6).EQ.'PRERES') THEN
C     ------------------------------------------------
C     ------------------------------------------------

C       ------------------------------------------------
C        INITIALISATION DE L'OCCURENCE MUMPS KXMPS:
C       ------------------------------------------------
        CALL AMUMPI(0,LQUALI,LDIST,KXMPS,TYPE)
        CALL ZMUMPS(ZMPSK)
        RANG=ZMPSK%MYID
        NBPROC=ZMPSK%NPROCS

C       ------------------------------------------------
C        CHOIX ICNTL VECTEUR DE PARAMETRES POUR MUMPS :
C       ------------------------------------------------
        CALL AMUMPI(2,LQUALI,LDIST,KXMPS,TYPE)

C       ------------------------------------------------
C        REMPLISSAGE DE LA MATRICE MUMPS :
C       ------------------------------------------------
        CALL AMUMPT(1,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,RDESQ,TYPE,
     &              IETDEB,IETRAT,RCTDEB,LDIST)
        CALL AMUMPM(RANG,LDIST,KXMPS,KMONIT,IMPR,IFMUMP,ELI2LG,TYPE,LMD,
     &              EPSMAT)

         CALL JJLDYN(0,-1,IBID)

C       ------------------------------------------------
C        ANALYSE MUMPS:
C       ------------------------------------------------
        CALL AMUMPT(2,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,RDESQ,TYPE,
     &              IETDEB,IETRAT,RCTDEB,LDIST)
        ZMPSK%JOB = 1
        CALL ZMUMPS(ZMPSK)
        CALL AMUMPT(4,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,RDESQ,TYPE,
     &              IETDEB,IETRAT,RCTDEB,LDIST)

C       ------------------------------------------------
C        GESTION ERREURS ET MENAGE ASTER:
C       ------------------------------------------------
        IF (ZMPSK%INFOG(1).EQ.0) THEN
C              -- C'EST OK
        ELSE IF ((ZMPSK%INFOG(1).EQ.-5).OR.(ZMPSK%INFOG(1).EQ.-7)) THEN
          CALL U2MESS('F','FACTOR_64')
        ELSE IF (ZMPSK%INFOG(1).EQ.-6) THEN
	  IRET=2
	  GOTO 99
        ELSE
          IF (ZMPSK%INFOG(1).LT.0) THEN
            CALL U2MESI('F','FACTOR_55',1,ZMPSK%INFOG(1))
          ELSE
            CALL U2MESI('A','FACTOR_55',1,ZMPSK%INFOG(1))
          ENDIF
        ENDIF
C        IF (RANG.EQ.0) CALL ASSERT(ZMPSK%INFOG(1).EQ.0)
        IF(ZK24(JSLVK-1+4).NE.'AUTO' .AND.
     &     ZMPSK%ICNTL(7).NE.ZMPSK%INFOG(7))
     &     CALL U2MESK('A','FACTOR_50',1,ZK24(JSLVK-1+4))

C       ------------------------------------------------
C        FACTORISATION NUMERIQUE MUMPS:
C       ------------------------------------------------
        ZMPSK%JOB = 2
        IF (LRESOL) CALL ZMUMPS(ZMPSK)
        CALL AMUMPT(6,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,RDESQ,TYPE,
     &              IETDEB,IETRAT,RCTDEB,LDIST)

C       ------------------------------------------------
C        GESTION ERREURS ET MENAGE ASTER:
C       ------------------------------------------------
        VALR(1)=(ZMPSK%INFOG(13)*100.D0)/ZMPSK%N
        VALR(2)=ZMPSK%ICNTL(14)*1.D0
        IF (VALR(1).GE.VALR(2)) THEN
          IF (NIV.GE.2) CALL U2MESR('I','FACTOR_73',2,VALR)
        ENDIF
        IF (ZMPSK%INFOG(1).EQ.0) THEN
C              -- C'EST OK
        ELSE IF ((ZMPSK%INFOG(1).EQ.-8).OR.(ZMPSK%INFOG(1).EQ.-9).OR.
     &         (ZMPSK%INFOG(1).EQ.-14).OR.(ZMPSK%INFOG(1).EQ.-15).OR.
     &         (ZMPSK%INFOG(1).EQ.-17).OR.(ZMPSK%INFOG(1).EQ.-20)) THEN
          CALL U2MESS('F','FACTOR_53')
        ELSE IF (ZMPSK%INFOG(1).EQ.-10) THEN
          IRET=2
	  GOTO 99
        ELSE IF (ZMPSK%INFOG(1).EQ.-13) THEN
          CALL U2MESS('F','FACTOR_54')
        ELSE IF (ZMPSK%INFOG(1).EQ.-37) THEN
          CALL U2MESS('F','FACTOR_65')
       ELSE IF (ZMPSK%INFOG(1).EQ.-90) THEN
          CALL U2MESS('F','FACTOR_66')
        ELSE
          IF (ZMPSK%INFOG(1).LT.0) THEN
            CALL U2MESI('F','FACTOR_55',1,ZMPSK%INFOG(1))
          ELSE
            CALL U2MESI('A','FACTOR_55',1,ZMPSK%INFOG(1))
          ENDIF
        ENDIF
        
C       ------------------------------------------------
C        DETECTION DE SINGULARITE:
C       ------------------------------------------------
        IF ((NPREC.GE.0).AND.(LRESOL)) THEN
          KPIV='&&AMUMP.PIVNUL'
          CALL JEEXIN(KPIV,IBID)
          IF (IBID.NE.0) THEN
            CALL ASSERT(.FALSE.)
          ELSE
            N=ZMPSK%N
            CALL WKVECT(KPIV,'V V I',N+2,IPIV)
C --- KPIV(1)= NOMBRE DE PIVOTS QUASI NULS
            IF (ZMPSK%INFOG(28).GT.N) CALL ASSERT(.FALSE.)
            ZI(IPIV)=ZMPSK%INFOG(28)
C --- KPIV(2)= NOMBRE DE PIVOTS NEGATIFS
            IF (ZMPSK%INFO(12).GT.N) CALL ASSERT(.FALSE.)
            ZI(IPIV+1)=ZMPSK%INFO(12)
C --- KPIV(3..) LES PIVOTS QUASI NULS
            DO I=1,ZMPSK%INFOG(28)
              ZI(IPIV+1+I)=ZMPSK%PIVNUL_LIST(I)
            ENDDO
          ENDIF
        ENDIF
        
        IF ((( RANG.EQ.0).AND.(.NOT.LDIST)).OR.(LDIST)) THEN
          IF (.NOT.LQUALI) THEN
            IF (LDIST) THEN
              DEALLOCATE(ZMPSK%A_loc)
              DEALLOCATE(ZMPSK%IRN_loc)
              DEALLOCATE(ZMPSK%JCN_loc)
            ELSE
              DEALLOCATE(ZMPSK%A)
              DEALLOCATE(ZMPSK%IRN)
              DEALLOCATE(ZMPSK%JCN)
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
        CALL AMUMPT(7,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,RDESQ,TYPE,
     &              IETDEB,IETRAT,RCTDEB,LDIST)
        CALL AMUMPP(0,NBSOL,KXMPS,LDIST,TYPE,IMPR,IFMUMP,ELI2LG,
     &              RBID,CSOLU,VCINE)

C       ------------------------------------------------
C        RESOLUTION MUMPS :
C       ------------------------------------------------
        CALL AMUMPT(8,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,RDESQ,TYPE,
     &              IETDEB,IETRAT,RCTDEB,LDIST)
        ZMPSK%JOB = 3
        IF (LRESOL) CALL ZMUMPS(ZMPSK)
        CALL AMUMPT(10,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,RDESQ,TYPE,
     &              IETDEB,IETRAT,RCTDEB,LDIST)

C       ------------------------------------------------
C        GESTION ERREURS ET MENAGE ASTER:
C       ------------------------------------------------
        IF (ZMPSK%INFOG(1).EQ.0) THEN
C              -- C'EST OK
        ELSE IF ((ZMPSK%INFOG(1).EQ.8).AND.(LQUALI)) THEN
          CALL U2MESI('A','FACTOR_62',1,ZMPSK%ICNTL(10))
        ELSE IF (ZMPSK%INFOG(1).LT.0) THEN
          CALL U2MESI('F','FACTOR_55',1,ZMPSK%INFOG(1))
        ELSE IF (ZMPSK%INFOG(1).EQ.4) THEN
C          -- PERMUTATION DE COLONNES, ZMPSK%JCN MODIFIE VOLONTAIREMENT
C          -- PAR MUMPS. IL NE FAUT DONC PAS LE MANIPULER TEL QUE
C          -- PAS GRAVE POUR ASTER.
        ELSE
          CALL U2MESI('A','FACTOR_55',1,ZMPSK%INFOG(1))
        ENDIF
C --- CONTROLE DE L'ERREUR SUR LA SOLUTION :
        IF (LQUALI) THEN
          IF (ZMPSK%RINFOG(9).GT.EPSMAX) THEN
            VALR(1)=ZMPSK%RINFOG(9)
            VALR(2)=EPSMAX
            CALL U2MESR('F','FACTOR_57',2,VALR)
          ENDIF
        ENDIF

C       ------------------------------------------------
C        POST-TRAITEMENTS ASTER DE/DES (LA) SOLUTION(S) :
C       ------------------------------------------------
        CALL AMUMPP(2,NBSOL,KXMPS,LDIST,TYPE,IMPR,IFMUMP,ELI2LG,
     &              RBID,CSOLU,VCINE)

C       ------------------------------------------------
C        AFFICHAGE DU MONITORING :
C       ------------------------------------------------
        CALL AMUMPT(12,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,RDESQ,TYPE,
     &              IETDEB,IETRAT,RCTDEB,LDIST)

C     ------------------------------------------------
C     ------------------------------------------------
      ELSE IF (ACTION(1:5).EQ.'DETR_') THEN
C     ------------------------------------------------
C     ------------------------------------------------


C       ------------------------------------------------
C        MENAGE ASTER ET MUMPS:
C       ------------------------------------------------
        IF (NOMATS(KXMPS).NE.' ') THEN
          IF ((( RANG.EQ.0).AND.(.NOT.LDIST)).OR.(LDIST)) THEN
            IF (LQUALI) THEN
              IF (LDIST) THEN
                DEALLOCATE(ZMPSK%A_loc)
                DEALLOCATE(ZMPSK%IRN_loc)
                DEALLOCATE(ZMPSK%JCN_loc)
              ELSE
                DEALLOCATE(ZMPSK%A)
                DEALLOCATE(ZMPSK%IRN)
                DEALLOCATE(ZMPSK%JCN)
              ENDIF
            ENDIF
          ENDIF
          ETAMS(KXMPS)=' '
          NONUS(KXMPS)=' '
          NOMATS(KXMPS)=' '
          NOSOLS(KXMPS)=' '
          ROUCS(KXMPS)=' '
          PRECS(KXMPS)=' '
          ZMPSK%JOB = -2
          CALL ZMUMPS(ZMPSK)
        ENDIF
      ENDIF

C     -- ON REACTIVE LA LEVEE D'EXCEPTION
   99 CALL MATFPE(1)
      CALL JEDEMA()

#endif
      END

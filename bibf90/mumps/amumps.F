      SUBROUTINE AMUMPS(ACTION,KXMPS,RSOLU,VCINE,NBSOL,IRET,
     &                  IMPR,IFMUMP,PREPOS,PCENTP)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION
!& MODIF AMUMPS MUMPS  DATE 29/10/2012   AUTEUR BOITEAU O.BOITEAU 
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
C--------------------------------------------------------------
C OBJET: DRIVER EN MODE REEL DE LA RESOLUTION DE SYSTEMES LINEAIRES
C        VIA MUMPS (EN SIMPLE PRECISION POUR MUMPS UNIQUEMENT)
C
C IN : ACTION :
C     /'PRERES'  : POUR DEMANDER LA FACTORISATION
C     /'RESOUD'  : POUR DEMANDER LA DESCENTE/REMONTEE
C     /'DETR_MAT/OCC': POUR DEMANDER LA DESTRUCTION DE L'INSTANCE MUMPS
C                  ASSOCIEE A UNE MATRICE
C
C IN : KXMPS (I)   : INDICE DE L'INSTANCE MUMPS DANS SMPS
C VAR: RSOLU (R)   : EN ENTREE : VECTEUR SECOND MEMBRE (REEL)
C                    EN SORTIE : VECTEUR SOLUTION (REEL)
C            (SI ACTION=RESOUD)
C IN : VCINE (K19) : NOM DU CHAM_NO DE CHARGEMENT CINEMATIQUE
C            (SI ACTION=RESOUD)
C OUT : IRET (I) : CODE_RETOUR :
C            0 : OK
C            1 : ERREUR (DANS LE CAS OU MUMPS EST UTILISE EN PRE_COND)
C            2 : MATRICE NUMERIQUEMENT SINGULIERE
C IN  : NBSOL  : NBRE DE SYSTEMES A RESOUDRE
C IN  : IMPR,IFMUMP : PARAMETRES POUR SORTIE FICHIER MATRICE CF AMUMPH
C IN : PREPOS (LOG) : SI .TRUE. ON FAIT LES PRE ET POSTTRAITEMENTS DE
C           MISE A L'ECHELLE DU RHS ET DE LA SOLUTION (MRCONL) ET DE LA
C           PRISE EN COMPTE DES AFFE_CHAR_CINE (CSMBGG).
C           SI .FALSE. ON NE LES FAIT PAS (PAR EXEMPLE EN MODAL).
C IN  : PCENTP VECTEUR D'ENTIER GERE PAR AMUMPH POUR PARAMETRER LES
C                STRATEGIES D'ADAPTATION EN CAS DE PB PCENT_PIVOT
C---------------------------------------------------------------
! RESPONSABLE BOITEAU O.BOITEAU

      CHARACTER*(*) ACTION
      CHARACTER*14  IMPR
      CHARACTER*19  VCINE,NOSOLV
      INTEGER       IRET,NBSOL,KXMPS,IFMUMP,PCENTP(2)
      REAL*8        RSOLU(*)
      LOGICAL       PREPOS

#ifdef _HAVE_MUMPS

C================================================================
      INCLUDE 'mpif.h'
      INCLUDE 'smumps_struc.h'
      INCLUDE 'jeveux.h'
C================================================================
      INTEGER    NMXINS
      PARAMETER (NMXINS=5)
      TYPE (SMUMPS_STRUC) , TARGET  :: SMPS(NMXINS)
      TYPE (SMUMPS_STRUC) , POINTER :: SMPSK
      INTEGER      JSLVK,JSLVR,RANG,NBPROC,NIV,IFM,IBID,IETDEB,IFACTM,
     &             IETRAT,JREFA,NPREC,JSLVI,IFACT,IAUX,VALI(4),PCPI
      CHARACTER*1  ROUCS(NMXINS),ROUC,TYPE,PREC,PRECS(NMXINS)
      CHARACTER*4  ETAMS(NMXINS),ETAM,KLAG2
      CHARACTER*8  KTYPR,K8BID
      CHARACTER*12 USERSM,K12BID
      CHARACTER*14 NONUS(NMXINS),NONU
      CHARACTER*19 NOMATS(NMXINS),NOMAT,NOSOLS(NMXINS)
      CHARACTER*24 KMONIT(12),K24AUX,KVERS
      REAL*8       EPSMAX,VALR(2),RCTDEB,TEMPS(6),EPSMAT
      COMPLEX*16   CBID(1)
      LOGICAL      LQUALI,LDIST,LRESOL,LMD,LBID,LPRECO,LBIS,LPB13,
     &             LDET
      COMMON /MUMPSS/ SMPS
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
      TYPE='S'
      CALL ASSERT(KXMPS.GT.0)
      CALL ASSERT(KXMPS.LE.NMXINS)
      NOMAT=NOMATS(KXMPS)
      NOSOLV=NOSOLS(KXMPS)
      NONU=NONUS(KXMPS)
      ETAM=ETAMS(KXMPS)
      ROUC=ROUCS(KXMPS)
      PREC=PRECS(KXMPS)
      CALL ASSERT((ROUC.EQ.'R').AND.(PREC.EQ.'S'))
      SMPSK=>SMPS(KXMPS)
      IRET=0

C --- L'UTILISATEUR VEUT-IL UNE ESTIMATION DE LA QUALITE DE LA SOL ?
C --- => LQUALI
      CALL JEVEUO(NOSOLV//'.SLVR','L',JSLVR)
      EPSMAX=ZR(JSLVR-1+2)
      LQUALI=(EPSMAX.GT.0.D0)

      CALL JEVEUO(NOSOLV//'.SLVK','E',JSLVK)
C --- POUR "ELIMINER" LE 2EME LAGRANGE :
C --- OPTION DEBRANCHEE SI CALCUL DE DETERMINANT
      KLAG2=ZK24(JSLVK-1+6)
      LBIS=KLAG2(1:3).EQ.'OUI'

C --- TYPE DE RESOLUTION
      KTYPR=ZK24(JSLVK-1+3)      

C --- PARAMETRE NPREC
      CALL JEVEUO(NOSOLV//'.SLVI','E',JSLVI)
      NPREC=ZI(JSLVI)
      
C --- MUMPS PARALLELE DISTRIBUE ?
      CALL JEVEUO(NOMAT//'.REFA','L',JREFA)
      LDIST=(ZK24(JREFA-1+11).NE.'MPI_COMPLET')
      RANG=SMPSK%MYID
      NBPROC=SMPSK%NPROCS

C --- MATRICE ASTER DISTRIBUEE ?
      LMD = ZK24(JSLVK-1+10)(1:3).EQ.'OUI'

C --- MUMPS EST-IL UTILISE COMME PRECONDITIONNEUR ?
      LPRECO = ZK24(JSLVK-1+8)(1:3).EQ.'OUI'
      
C --- FILTRAGE DE LA MATRICE DONNEE A MUMPS (UNIQUEMENT NON LINEAIRE)
      EPSMAT=ZR(JSLVR-1+1)

C --- STRATEGIE MEMOIRE POUR MUMPS
      USERSM=ZK24(JSLVK+8)

C --- POUR MONITORING
      CALL AMUMPT(0,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,TYPE,
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
        CALL SMUMPS(SMPSK)
        RANG=SMPSK%MYID
        NBPROC=SMPSK%NPROCS

C       --------------------------------------------------------------
C        CHOIX ICNTL VECTEUR DE PARAMETRES POUR MUMPS (ANALYSE+FACTO):
C       --------------------------------------------------------------
        CALL AMUMPI(2,LQUALI,LDIST,KXMPS,TYPE)

C       ----------------------------------------------------------
C        ON RECUPERE ET STOCKE DS SD_SOLVEUR LE NUMERO DE VERSION
C        LICITE
C       ----------------------------------------------------------
        CALL AMUMPU(3,TYPE,KXMPS,K12BID,IBID,LBID,KVERS)
        ZK24(JSLVK-1+12)=KVERS

C       -----------------------------------------------------
C       CALCUL DU DETERMINANT PART I ?
C       -----------------------------------------------------
        LDET=.FALSE.
        IF (ZI(JSLVI-1+5).EQ.1) THEN
          SELECT CASE(KVERS)
          CASE('4.10.0')
C --- ON DEBRANCHE ELIM_LAGR2='NON' CAR CELA FAUSSE LA VALEUR DU DETER
C --- MINANT PAR RAPPORT AUX AUTRES SOLVEURS DIRECTS
            IF ((NIV.GE.2).AND.(LBIS)) CALL U2MESS('I','FACTOR_88')
            ZK24(JSLVK-1+6)='NON'
            KLAG2='NON'
            LBIS=.FALSE.
            LDET=.TRUE.
          CASE('4.9.2')
            CALL U2MESK('F','FACTOR_87',1,KVERS)
          END SELECT
        ENDIF

C       ------------------------------------------------
C        REMPLISSAGE DE LA MATRICE MUMPS :
C       ------------------------------------------------
        CALL AMUMPT(1,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,TYPE,
     &              IETDEB,IETRAT,RCTDEB,LDIST)
        CALL AMUMPM(LDIST,KXMPS,KMONIT,IMPR,IFMUMP,KLAG2,
     &              TYPE,LMD,EPSMAT,KTYPR)

C       -----------------------------------------------------
C       CONSERVE-T-ON LES FACTEURS OU NON ?
C       -----------------------------------------------------
        IF (ZI(JSLVI-1+4).EQ.1) THEN
          SELECT CASE(KVERS)
          CASE('4.10.0')
            SMPSK%ICNTL(31)=1
          CASE('4.9.2')
            IF (NIV.GE.2) CALL U2MESK('I','FACTOR_86',1,KVERS)
          END SELECT
        ENDIF

C       ------------------------------------------------
C        ANALYSE MUMPS:
C       ------------------------------------------------
        CALL AMUMPT(2,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,TYPE,
     &              IETDEB,IETRAT,RCTDEB,LDIST)
        SMPSK%JOB = 1
        CALL SMUMPS(SMPSK)
        CALL AMUMPT(4,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,TYPE,
     &              IETDEB,IETRAT,RCTDEB,LDIST)

C       ------------------------------------------------
C        GESTION ERREURS ET MENAGE ASTER:
C       ------------------------------------------------
        IF (SMPSK%INFOG(1).EQ.0) THEN
C              -- C'EST OK
        ELSE IF ((SMPSK%INFOG(1).EQ.-5).OR.(SMPSK%INFOG(1).EQ.-7)) THEN
          CALL U2MESS('F','FACTOR_64')
        ELSE IF (SMPSK%INFOG(1).EQ.-6) THEN
          IRET=2
          GOTO 99
        ELSE
          IAUX=SMPSK%INFOG(1)
          IF (IAUX.LT.0) THEN
            CALL U2MESI('F','FACTOR_55',1,IAUX)
          ELSE
            CALL U2MESI('A','FACTOR_55',1,IAUX)
          ENDIF
        ENDIF
        IF(ZK24(JSLVK-1+4).NE.'AUTO' .AND.
     &     SMPSK%ICNTL(7).NE.SMPSK%INFOG(7))
     &     CALL U2MESK('A','FACTOR_50',1,ZK24(JSLVK-1+4))

C       -----------------------------------------------------
C        CHOIX DE LA STRATEGIE MUMPS POUR LA GESTION MEMOIRE
C       -----------------------------------------------------
        CALL AMUMPU(1,'S',KXMPS,USERSM,IBID,LBID,K12BID)
C ---   ON SORT POUR REVENIR A AMUMPH ET DETRUIRE L'OCCURENCE MUMPS
C ---   ASSOCIEE
        IF (USERSM(1:4).EQ.'EVAL') GOTO 99

C       -----------------------------------------------------
C       CALCUL DU DETERMINANT PART II ?
C       -----------------------------------------------------
        IF (LDET) SMPSK%ICNTL(33)=1

C       ------------------------------------------------
C        FACTORISATION NUMERIQUE MUMPS:
C       ------------------------------------------------

C --- SI GESTION_MEMOIRE='AUTO'
C --- ON TENTE PLUSIEURS (PCENTP(1)) FACTORISATIONS NUMERIQUES EN 
C --- MULTIPLIANT, A CHAQUE ECHEC, L'ANCIEN PCENT_PIVOT PAR PCENTP(2)
C --- VOIRE EN PASSANT EN OOC (EN DERNIER RESSORT).
C --- AUTO-ADAPTATION DU PARAMETRAGE SOLVEUR/PCENT_PIVOT:
C --- ON MODIFIE LE PARAMETRE DANS LA SD_SOLVEUR A LA VOLEE POUR NE
C --- PAS PERDRE DE TEMPS LA PROCHAINE FOIS. CETTE VALEUR N'EST VALABLE
C --- QUE DANS L'OPERATEUR CONSIDERE.
C --- ON FAIT LA MEME CHOSE EN CAS DE PB D'ALLOCATION MEMOIRE (INFOG=-13)
C --- CELA PEUT ETRE DU A UN ICNTL(23) MAL ESTIME
C       
        SMPSK%JOB = 2
        IFACT=0
        LPB13=.FALSE.
        IF (USERSM(1:4).EQ.'AUTO') THEN
          IFACTM=PCENTP(1)
        ELSE
          IFACTM=1
        ENDIF
        IF (LRESOL) THEN
          PCPI=SMPSK%ICNTL(14)
          DO IFACT=1,IFACTM
            CALL SMUMPS(SMPSK)
            IAUX=SMPSK%INFOG(1)

C --- TRAITEMENT CORRECTIF ICNTL(14)
            IF ((IAUX.EQ.-8).OR.(IAUX.EQ.-9).OR.(IAUX.EQ.-14).OR.
     &          (IAUX.EQ.-15).OR.(IAUX.EQ.-17).OR.(IAUX.EQ.-20)) THEN
              IF (IFACT.EQ.IFACTM) THEN
C ---  ICNTL(14): PLUS DE NOUVELLE TENTATIVE POSSIBLE
                IF (LPRECO) THEN
C                 -- MUMPS EST APPELE COMME PRECONDITIONNEUR
C                 -- ON SORT AVEC UN CODE RETOUR NON NUL
                  IRET = 1
                  GOTO 99
                ELSE
                  VALI(1)=IFACTM
                  VALI(2)=PCPI
                  VALI(3)=SMPSK%ICNTL(14)
                  CALL U2MESI('F','FACTOR_53',3,VALI)
                ENDIF
              ELSE
C ---  ICNTL(14): ON MODIFIE DES PARAMETRES POUR LA NOUVELLE TENTATIVE
                SMPSK%ICNTL(14)=SMPSK%ICNTL(14)*PCENTP(2)
                ZI(JSLVI-1+2)=SMPSK%ICNTL(14)
                IF ((IAUX.EQ.-9).OR.(IFACT.EQ.(IFACTM-1)))
     &            SMPSK%ICNTL(22)=1
                IF (NIV.GE.2) THEN
                  VALI(1)=SMPSK%ICNTL(14)/PCENTP(2)
                  VALI(2)=SMPSK%ICNTL(14)
                  VALI(3)=IFACT
                  VALI(4)=IFACTM
                  CALL U2MESI('I','FACTOR_58',4,VALI)
                ENDIF
              ENDIF

C --- TRAITEMENT CORRECTIF ICNTL(23)
C --- CE N'EST UTILE QU' UNE FOIS D'OU LE CONTROLE DE LPB13
            ELSE IF ((IAUX.EQ.-13).AND.(.NOT.LPB13)) THEN
C ---  ICNTL(23): ON MODIFIE DES PARAMETRES POUR LA NOUVELLE TENTATIVE
              IF (NIV.GE.2) THEN
                VALI(1)=SMPSK%ICNTL(23)
                CALL U2MESI('I','FACTOR_85',1,VALI)
              ENDIF
              LPB13=.TRUE.
              SMPSK%ICNTL(23)=0       
            ELSE
C ---  SORTIE STANDARD SANS ERREUR
              EXIT
            ENDIF
          ENDDO
        ENDIF

C ---  AFFICHAGE DE CONTROLE
        IF (NIV.GE.2) THEN
          WRITE(IFM,*)
          WRITE(IFM,*)
     &      '<AMUMPS> FACTO. NUM. - NBRE TENTATIVES/MAX: ',IFACT,IFACTM
        ENDIF
        CALL AMUMPT(6,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,TYPE,
     &              IETDEB,IETRAT,RCTDEB,LDIST)

C       ------------------------------------------------
C        GESTION ERREURS ET MENAGE ASTER (SAUF ERREUR ICNTL(14/23)
C           TRAITEE EN AMONT):
C       ------------------------------------------------
        VALR(1)=(SMPSK%INFOG(13)*100.D0)/SMPSK%N
        VALR(2)=SMPSK%ICNTL(14)*1.D0
        IF (VALR(1).GE.VALR(2)) THEN
          IF (NIV.GE.2) CALL U2MESR('I','FACTOR_73',2,VALR)
        ENDIF
        IF (SMPSK%INFOG(1).EQ.0) THEN
C              -- C'EST OK
        ELSE IF (SMPSK%INFOG(1).EQ.-10) THEN
          IRET=2
          GOTO 99
        ELSE IF (SMPSK%INFOG(1).EQ.-13) THEN
          CALL U2MESS('F','FACTOR_54')
        ELSE IF (SMPSK%INFOG(1).EQ.-37) THEN
          CALL U2MESS('F','FACTOR_65')
       ELSE IF (SMPSK%INFOG(1).EQ.-90) THEN
          CALL U2MESS('F','FACTOR_66'  )
       ELSE
          IAUX=SMPSK%INFOG(1)
          IF (IAUX.LT.0) THEN
            CALL U2MESI('F','FACTOR_55',1,IAUX)
          ELSE
            CALL U2MESI('A','FACTOR_55',1,IAUX)
          ENDIF
        ENDIF

C       ------------------------------------------------
C        DETECTION DE SINGULARITE SI NECESSAIRE:
C       ------------------------------------------------
        CALL AMUMPU(2,'S',KXMPS,K8BID,NPREC,LRESOL,K12BID)

C       ------------------------------------------------
C        RECUPERATION DU DETERMINANT SI NECESSAIRE:
C       ------------------------------------------------
        CALL AMUMPU(4,'S',KXMPS,K8BID,IBID,LBID,K12BID)

C       ON SOULAGE LA MEMOIRE JEVEUX DES QUE POSSIBLE D'OBJETS MUMPS
C       INUTILES
        IF ((( RANG.EQ.0).AND.(.NOT.LDIST)).OR.(LDIST)) THEN
          IF (.NOT.LQUALI) THEN
            IF (LDIST) THEN
              DEALLOCATE(SMPSK%A_loc)
              DEALLOCATE(SMPSK%IRN_loc)
              DEALLOCATE(SMPSK%JCN_loc)
            ELSE
              DEALLOCATE(SMPSK%A)
              DEALLOCATE(SMPSK%IRN)
              DEALLOCATE(SMPSK%JCN)
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
        CALL AMUMPT(7,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,TYPE,
     &              IETDEB,IETRAT,RCTDEB,LDIST)
        CALL AMUMPP(0,NBSOL,KXMPS,LDIST,TYPE,IMPR,IFMUMP,LBIS,
     &              RSOLU,CBID,VCINE,PREPOS,LPRECO)

C       --------------------------------------------------------------
C        CHOIX ICNTL VECTEUR DE PARAMETRES POUR MUMPS (SOLVE):
C       --------------------------------------------------------------
        CALL AMUMPI(3,LQUALI,LDIST,KXMPS,TYPE)

C       ------------------------------------------------
C        RESOLUTION MUMPS :
C       ------------------------------------------------
        CALL AMUMPT(8,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,TYPE,
     &              IETDEB,IETRAT,RCTDEB,LDIST)
        SMPSK%JOB = 3
        IF (LRESOL) CALL SMUMPS(SMPSK)
        CALL AMUMPT(10,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,TYPE,
     &              IETDEB,IETRAT,RCTDEB,LDIST)

C       ------------------------------------------------
C        GESTION ERREURS ET MENAGE ASTER:
C       ------------------------------------------------
        IF (SMPSK%INFOG(1).EQ.0) THEN
C              -- C'EST OK
        ELSE IF ((SMPSK%INFOG(1).EQ.8).AND.(LQUALI)) THEN
          IAUX=SMPSK%INFOG(10)    
          CALL U2MESI('A','FACTOR_62',1,IAUX)
        ELSE IF (SMPSK%INFOG(1).LT.0) THEN
          IAUX=SMPSK%INFOG(1)      
          CALL U2MESI('F','FACTOR_55',1,IAUX)
        ELSE IF (SMPSK%INFOG(1).EQ.4) THEN
C          -- PERMUTATION DE COLONNES, SMPSK%JCN MODIFIE VOLONTAIREMENT
C          -- PAR MUMPS. IL NE FAUT DONC PAS LE MANIPULER TEL QUE
C          -- PAS GRAVE POUR ASTER.
        ELSE
          IAUX=SMPSK%INFOG(1)     
          CALL U2MESI('A','FACTOR_55',1,IAUX)
        ENDIF
C --- CONTROLE DE L'ERREUR SUR LA SOLUTION :
        IF (LQUALI) THEN
          IF (SMPSK%RINFOG(9).GT.EPSMAX) THEN
            VALR(1)=SMPSK%RINFOG(9)
            VALR(2)=EPSMAX
            CALL U2MESR('F','FACTOR_57',2,VALR)
          ENDIF
        ENDIF

C       ------------------------------------------------
C        POST-TRAITEMENTS ASTER DE/DES (LA) SOLUTION(S) :
C       ------------------------------------------------
        CALL AMUMPP(2,NBSOL,KXMPS,LDIST,TYPE,IMPR,IFMUMP,LBIS,
     &              RSOLU,CBID,VCINE,PREPOS,LPRECO)

C       ------------------------------------------------
C        AFFICHAGE DU MONITORING :
C       ------------------------------------------------
        CALL AMUMPT(12,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,TYPE,
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
                DEALLOCATE(SMPSK%A_loc)
                DEALLOCATE(SMPSK%IRN_loc)
                DEALLOCATE(SMPSK%JCN_loc)
              ELSE
                DEALLOCATE(SMPSK%A)
                DEALLOCATE(SMPSK%IRN)
                DEALLOCATE(SMPSK%JCN)
              ENDIF
            ENDIF
          ENDIF
          ETAMS(KXMPS)=' '
          NONUS(KXMPS)=' '
          NOMATS(KXMPS)=' '
          NOSOLS(KXMPS)=' '
          ROUCS(KXMPS)=' '
          PRECS(KXMPS)=' '
          SMPSK%JOB = -2
          CALL SMUMPS(SMPSK)
C NETTOYAGE OBJETS AUXILIAIRES AU CAS OU
          K24AUX='&&TAILLE_OBJ_MUMPS'
          CALL JEEXIN(K24AUX,IBID)
          IF (IBID.NE.0) CALL JEDETR(K24AUX)
C
          K24AUX='&&AMUMP.PIVNUL'
          CALL JEEXIN(K24AUX,IBID)
          IF (IBID.NE.0) CALL JEDETR(K24AUX)
C
          K24AUX='&&AMUMP.DETERMINANT'
          CALL JEEXIN(K24AUX,IBID)
          IF (IBID.NE.0) CALL JEDETR(K24AUX)
        ENDIF
      ENDIF

C     -- ON REACTIVE LA LEVEE D'EXCEPTION
   99 CALL MATFPE(1)
      CALL JEDEMA()

#endif
      END

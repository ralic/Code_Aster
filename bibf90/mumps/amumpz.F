      SUBROUTINE AMUMPZ(ACTION,KXMPS,CSOLU,VCINE,NBSOL,IRET,
     &                  IMPR,IFMUMP,PREPOS,PCENTP)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION
!& MODIF AMUMPZ MUMPS  DATE 20/06/2012   AUTEUR BOITEAU O.BOITEAU 
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
C            1 : ERREUR (DANS LE CAS OU MUMPS EST UTILISE EN PRE_COND)
C            2 : MATRICE NUMERIQUEMENT SINGULIERE
C IN  : NBSOL  : NBRE DE SYSTEMES A RESOUDRE
C IN  : IMPR,IFMUMP : PARAMETRES POUR SORTIE FICHIER MATRICE CF AMUMPH
C IN : PREPOS (LOG) : SI .TRUE. ON FAIT LES PRE ET POSTTRAITEMENTS DE
C           MISE A L'ECHELLE DU RHS ET DE LA SOLUTION (MRCONL) ET DE LA
C           PRISE EN COMPTE DES AFFE_CHAR_CINE (CSMBGG).
C           SI .FALSE. ON NE LES FAIT PAS (PAR EXEMPLE EN MODAL).
C IN  : PCENTP  VECTEUR D'ENTIER GERE PAR AMUMPH POUR PARAMETRER LES
C                STRATEGIES D'ADAPTATION EN CAS DE PB PCENT_PIVOT
C---------------------------------------------------------------
! RESPONSABLE BOITEAU O.BOITEAU

      CHARACTER*(*) ACTION
      CHARACTER*14  IMPR
      CHARACTER*19  VCINE,NOSOLV
      INTEGER       IRET,NBSOL,KXMPS,IFMUMP,PCENTP(2)
      COMPLEX*16    CSOLU(*)
      LOGICAL       PREPOS

#ifdef _HAVE_MUMPS

C================================================================
      INCLUDE 'mpif.h'
      INCLUDE 'zmumps_struc.h'
      INCLUDE 'jeveux.h'
C================================================================
      INTEGER    NMXINS
      PARAMETER (NMXINS=5)
      TYPE (ZMUMPS_STRUC) , TARGET  :: ZMPS(NMXINS)
      TYPE (ZMUMPS_STRUC) , POINTER :: ZMPSK
      INTEGER      JSLVK,JSLVR,RANG,NBPROC,NIV,IFM,IBID,IETDEB,IFACTM,
     &             IETRAT,JREFA,NPREC,JSLVI,N,IFACT,IAUX,VALI(4),PCPI
      CHARACTER*1  ROUCS(NMXINS),ROUC,TYPE,PREC,PRECS(NMXINS)
      CHARACTER*4  ETAMS(NMXINS),ETAM,KLAG2
      CHARACTER*8  KTYPR,K8BID
      CHARACTER*12 USERSM
      CHARACTER*14 NONUS(NMXINS),NONU
      CHARACTER*19 NOMATS(NMXINS),NOMAT,NOSOLS(NMXINS)
      CHARACTER*24 KMONIT(12)
      REAL*8       EPSMAX,VALR(2),RCTDEB,RBID(1),TEMPS(6),EPSMAT
      LOGICAL      LQUALI,LDIST,LRESOL,LMD,LBID,LMIXP,LPRECO,LBIS
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

C --- L'UTILISATEUR VEUT-IL UNE ESTIMATION DE LA QUALITE DE LA SOL ?
C --- => LQUALI
      CALL JEVEUO(NOSOLV//'.SLVR','L',JSLVR)
      EPSMAX=ZR(JSLVR-1+2)
      LQUALI=(EPSMAX.GT.0.D0)

      CALL JEVEUO(NOSOLV//'.SLVK','L',JSLVK)
C --- POUR "ELIMINER" LE 2EME LAGRANGE :
      KLAG2=ZK24(JSLVK-1+6)
      LBIS=KLAG2(1:3).EQ.'OUI'

C --- TYPE DE RESOLUTION
      KTYPR=ZK24(JSLVK-1+3)
      IF (KTYPR(1:6).EQ.'SYMDEF') CALL U2MESS('F','FACTOR_80')

C --- PARAMETRE NPREC
      CALL JEVEUO(NOSOLV//'.SLVI','L',JSLVI)
      NPREC=ZI(JSLVI)
      
C --- MUMPS PARALLELE DISTRIBUE ?
      CALL JEVEUO(NOMAT//'.REFA','L',JREFA)
      LDIST=(ZK24(JREFA-1+11).NE.'MPI_COMPLET')
      RANG=ZMPSK%MYID
      NBPROC=ZMPSK%NPROCS

C --- MATRICE ASTER DISTRIBUEE ?
      LMD = ZK24(JSLVK-1+10)(1:3).EQ.'OUI'

C --- MUMPS EST-IL UTILISE COMME PRECONDITIONNEUR ?
      LPRECO = ZK24(JSLVK-1+8)(1:3).EQ.'OUI'

C --- MUMPS EST-IL UTILISE EN MIXTE-PRECISION ?
      LMIXP = ZK24(JSLVK-1+7)(1:3).EQ.'OUI'

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
        CALL AMUMPT(1,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,TYPE,
     &              IETDEB,IETRAT,RCTDEB,LDIST)
        CALL AMUMPM(RANG,LDIST,KXMPS,KMONIT,IMPR,IFMUMP,KLAG2,TYPE,LMD,
     &              EPSMAT,KTYPR,LPRECO.OR.LMIXP)

C       ------------------------------------------------
C        ANALYSE MUMPS:
C       ------------------------------------------------
        CALL AMUMPT(2,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,TYPE,
     &              IETDEB,IETRAT,RCTDEB,LDIST)
        ZMPSK%JOB = 1
        CALL ZMUMPS(ZMPSK)
        CALL AMUMPT(4,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,TYPE,
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
          IAUX=ZMPSK%INFOG(1)
          IF (IAUX.LT.0) THEN
            CALL U2MESI('F','FACTOR_55',1,IAUX)
          ELSE
            CALL U2MESI('A','FACTOR_55',1,IAUX)
          ENDIF
        ENDIF
        IF(ZK24(JSLVK-1+4).NE.'AUTO' .AND.
     &     ZMPSK%ICNTL(7).NE.ZMPSK%INFOG(7))
     &     CALL U2MESK('A','FACTOR_50',1,ZK24(JSLVK-1+4))

C       -----------------------------------------------------
C        CHOIX DE LA STRATEGIE MUMPS POUR LA GESTION MEMOIRE
C       -----------------------------------------------------
        CALL AMUMPU(1,'Z',KXMPS,USERSM,IBID,LBID)
C ---   ON SORT POUR REVENIR A AMUMPH ET DETRUIRE L'OCCURENCE MUMPS
C ---   ASSOCIEE
        IF (USERSM(1:4).EQ.'EVAL') GOTO 99

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
       
        ZMPSK%JOB = 2
        IFACT=0
        IF (USERSM(1:4).EQ.'AUTO') THEN
          IFACTM=PCENTP(1)
        ELSE
          IFACTM=1
        ENDIF
        IF (LRESOL) THEN
          PCPI=ZMPSK%ICNTL(14)
          DO IFACT=1,IFACTM
            CALL ZMUMPS(ZMPSK)
            IAUX=ZMPSK%INFOG(1)
            IF ((IAUX.EQ.-8).OR.(IAUX.EQ.-9).OR.(IAUX.EQ.-14).OR.
     &          (IAUX.EQ.-15).OR.(IAUX.EQ.-17).OR.(IAUX.EQ.-20)) THEN
              IF (IFACT.EQ.IFACTM) THEN
C ---  ERREUR: PLUS DE NOUVELLE TENTATIVE POSSIBLE
                IF (LPRECO) THEN
C                 -- MUMPS EST APPELE COMME PRECONDITIONNEUR
C                 -- ON SORT AVEC UN CODE RETOUR NON NUL
                  IRET = 1
                  GOTO 99
                ELSE
                  VALI(1)=IFACTM
                  VALI(2)=PCPI
                  VALI(3)=ZMPSK%ICNTL(14)
                  CALL U2MESI('F','FACTOR_53',3,VALI)
                ENDIF
              ELSE
C ---  ERREUR: ON MODIFIE DES PARAMETRES POUR LA NOUVELLE TENTATIVE
                ZMPSK%ICNTL(14)=ZMPSK%ICNTL(14)*PCENTP(2)
                ZI(JSLVI-1+2)=ZMPSK%ICNTL(14)
                IF ((IAUX.EQ.-9).OR.(IFACT.EQ.(IFACTM-1)))
     &            ZMPSK%ICNTL(22)=1
                IF (NIV.GE.2) THEN
                  VALI(1)=ZMPSK%ICNTL(14)/PCENTP(2)
                  VALI(2)=ZMPSK%ICNTL(14)
                  VALI(3)=IFACT
                  VALI(4)=IFACTM
                  CALL U2MESI('I','FACTOR_58',4,VALI)
                ENDIF
              ENDIF
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
     &      '<AMUMPZ> FACTO. NUM. - NBRE TENTATIVES/MAX: ',IFACT,IFACTM
        ENDIF
        CALL AMUMPT(6,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,TYPE,
     &              IETDEB,IETRAT,RCTDEB,LDIST)

C       ------------------------------------------------
C        GESTION ERREURS ET MENAGE ASTER (SAUF ERREUR PCENT_PIVOT
C           TRAITEE EN AMONT):
C       ------------------------------------------------
        VALR(1)=(ZMPSK%INFOG(13)*100.D0)/ZMPSK%N
        VALR(2)=ZMPSK%ICNTL(14)*1.D0
        IF (VALR(1).GE.VALR(2)) THEN
          IF (NIV.GE.2) CALL U2MESR('I','FACTOR_73',2,VALR)
        ENDIF
        IF (ZMPSK%INFOG(1).EQ.0) THEN
C              -- C'EST OK
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
          IAUX=ZMPSK%INFOG(1)     
          IF (IAUX.LT.0) THEN
            CALL U2MESI('F','FACTOR_55',1,IAUX)
          ELSE
            CALL U2MESI('A','FACTOR_55',1,IAUX)
          ENDIF
        ENDIF
        
C       ------------------------------------------------
C        DETECTION DE SINGULARITE:
C       ------------------------------------------------
        CALL AMUMPU(2,'Z',KXMPS,K8BID,NPREC,LRESOL)

C       ON SOULAGE LA MEMOIRE JEVEUX DES QUE POSSIBLE D'OBJETS MUMPS
C       INUTILES
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
        CALL AMUMPT(7,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,TYPE,
     &              IETDEB,IETRAT,RCTDEB,LDIST)
        CALL AMUMPP(0,NBSOL,KXMPS,LDIST,TYPE,IMPR,IFMUMP,LBIS,
     &              RBID,CSOLU,VCINE,PREPOS,LPRECO,LMIXP)

C       ------------------------------------------------
C        RESOLUTION MUMPS :
C       ------------------------------------------------
        CALL AMUMPT(8,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,TYPE,
     &              IETDEB,IETRAT,RCTDEB,LDIST)
        ZMPSK%JOB = 3
        IF (LRESOL) CALL ZMUMPS(ZMPSK)
        CALL AMUMPT(10,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,TYPE,
     &              IETDEB,IETRAT,RCTDEB,LDIST)

C       ------------------------------------------------
C        GESTION ERREURS ET MENAGE ASTER:
C       ------------------------------------------------
        IF (ZMPSK%INFOG(1).EQ.0) THEN
C              -- C'EST OK
        ELSE IF ((ZMPSK%INFOG(1).EQ.8).AND.(LQUALI)) THEN
          IAUX=ZMPSK%ICNTL(10)    
          CALL U2MESI('A','FACTOR_62',1,IAUX)
        ELSE IF (ZMPSK%INFOG(1).LT.0) THEN
          IAUX=ZMPSK%ICNTL(1)     
          CALL U2MESI('F','FACTOR_55',1,IAUX)
        ELSE IF (ZMPSK%INFOG(1).EQ.4) THEN
C          -- PERMUTATION DE COLONNES, ZMPSK%JCN MODIFIE VOLONTAIREMENT
C          -- PAR MUMPS. IL NE FAUT DONC PAS LE MANIPULER TEL QUE
C          -- PAS GRAVE POUR ASTER.
        ELSE
          IAUX=ZMPSK%INFOG(1)     
          CALL U2MESI('A','FACTOR_55',1,IAUX)
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
        CALL AMUMPP(2,NBSOL,KXMPS,LDIST,TYPE,IMPR,IFMUMP,LBIS,
     &              RBID,CSOLU,VCINE,PREPOS,LPRECO,LMIXP)

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

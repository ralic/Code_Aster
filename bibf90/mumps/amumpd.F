      SUBROUTINE AMUMPD(ACTION,KXMPS,RSOLU,VCINE,NBSOL,IRET,
     &                  IMPR,IFMUMP,PREPOS,PCENTP)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION
!& MODIF AMUMPD MUMPS  DATE 22/04/2013   AUTEUR BOITEAU O.BOITEAU 
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
!--------------------------------------------------------------
! OBJET: DRIVER EN MODE REEL DE LA RESOLUTION DE SYSTEMES LINEAIRES
!        VIA MUMPS (EN DOUBLE PRECISION AUSSI POUR MUMPS)
!
! IN : ACTION :
!     /'PRERES'  : POUR DEMANDER LA FACTORISATION
!     /'RESOUD'  : POUR DEMANDER LA DESCENTE/REMONTEE
!     /'DETR_MAT/OCC': POUR DEMANDER LA DESTRUCTION DE L'INSTANCE MUMPS
!                  ASSOCIEE A UNE MATRICE
!
! IN : KXMPS (I)   : INDICE DE L'INSTANCE MUMPS DANS DMPS
! VAR: RSOLU (R)   : EN ENTREE : VECTEUR SECOND MEMBRE (REEL)
!                    EN SORTIE : VECTEUR SOLUTION (REEL)
!            (SI ACTION=RESOUD)
! IN : VCINE (K19) : NOM DU CHAM_NO DE CHARGEMENT CINEMATIQUE
!            (SI ACTION=RESOUD)
! OUT : IRET (I) : CODE_RETOUR :
!            0 : OK
!            1 : ERREUR (DANS LE CAS OU MUMPS EST UTILISE EN PRE_COND)
!            2 : MATRICE NUMERIQUEMENT SINGULIERE
! IN  : NBSOL  : NBRE DE SYSTEMES A RESOUDRE
! IN  : IMPR,IFMUMP : PARAMETRES POUR SORTIE FICHIER MATRICE CF AMUMPH
! IN  : PREPOS (LOG) : SI .TRUE. ON FAIT LES PRE ET POSTTRAITEMENTS DE
!           MISE A L'ECHELLE DU RHS ET DE LA SOLUTION (MRCONL) ET DE LA
!           PRISE EN COMPTE DES AFFE_CHAR_CINE (CSMBGG).
!           SI .FALSE. ON NE LES FAIT PAS (PAR EXEMPLE EN MODAL).
! IN  : PCENTP  VECTEUR D'ENTIER GERE PAR AMUMPH POUR PARAMETRER LES
!                STRATEGIES D'ADAPTATION EN CAS DE PB PCENT_PIVOT
!---------------------------------------------------------------
! RESPONSABLE BOITEAU O.BOITEAU

      CHARACTER*(*) ACTION
      CHARACTER*14  IMPR
      CHARACTER*19  VCINE,NOSOLV
      INTEGER       IRET,NBSOL,KXMPS,IFMUMP,PCENTP(2)
      REAL*8        RSOLU(*)
      LOGICAL       PREPOS

#ifdef _HAVE_MUMPS

!================================================================
      INCLUDE 'mpif.h'
      INCLUDE 'dmumps_struc.h'
      INCLUDE 'jeveux.h'
!================================================================
      INTEGER    NMXINS
      PARAMETER (NMXINS=5)
      TYPE (DMUMPS_STRUC) , TARGET  :: DMPS(NMXINS)
      TYPE (DMUMPS_STRUC) , POINTER :: DMPSK
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
     &             LDET,LOPFAC
      COMMON /MUMPSD/ DMPS
      COMMON /MUMPSH/ NONUS,NOMATS,NOSOLS,ETAMS,ROUCS,PRECS
!=================================================================
      CALL JEMARQ()

!       ------------------------------------------------
!        INITS
!       ------------------------------------------------
! --- ON DESACTIVE LA LEVEE D'EXCEPTION FPE DANS LA BIBLIOTHEQUE MKL
! --  CAR CES EXCEPTIONS NE SONT PAS JUSTIFIEES
      CALL MATFPE(-1)
      CALL INFNIV(IFM,NIV)

! --- PARAMETRE POUR IMPRESSION FICHIER
      LRESOL=((IMPR(1:3).EQ.'NON').OR.(IMPR(1:9).EQ.'OUI_SOLVE'))

! --- TYPE DE SYSTEME: REEL OU COMPLEXE
      TYPE='D'
      CALL ASSERT(KXMPS.GT.0)
      CALL ASSERT(KXMPS.LE.NMXINS)
      NOMAT=NOMATS(KXMPS)
      NOSOLV=NOSOLS(KXMPS)
      NONU=NONUS(KXMPS)
      ETAM=ETAMS(KXMPS)
      ROUC=ROUCS(KXMPS)
      PREC=PRECS(KXMPS)
      CALL ASSERT((ROUC.EQ.'R').AND.(PREC.EQ.'D'))
      DMPSK=>DMPS(KXMPS)
      IRET=0

! --- L'UTILISATEUR VEUT-IL UNE ESTIMATION DE LA QUALITE DE LA SOL ?
! --- => LQUALI
      CALL JEVEUO(NOSOLV//'.SLVR','L',JSLVR)
      EPSMAX=ZR(JSLVR-1+2)
      LQUALI=(EPSMAX.GT.0.D0)

      CALL JEVEUO(NOSOLV//'.SLVK','E',JSLVK)
! --- POUR "ELIMINER" LE 2EME LAGRANGE:
! --- OPTION DEBRANCHEE SI CALCUL DE DETERMINANT
      KLAG2=ZK24(JSLVK-1+6)
      LBIS=KLAG2(1:3).EQ.'OUI'

! --- TRES PROBABLEMENT COMMANDE FACTORISER (POSTTRAITEMENTS
! --- INITIALISE A 'XXXX'). ON NE DETRUIRA RIEN A L'ISSU DE LA
! --- FACTO, AU CAS OU UN OP. RESOUDRE + RESI_RELA>0 SUIVRAIT
      IF (ZK24(JSLVK-1+11)(1:4).EQ.'XXXX') THEN
        LOPFAC=.TRUE.
      ELSE
        LOPFAC=.FALSE.
      ENDIF

! --- TYPE DE RESOLUTION
      KTYPR=ZK24(JSLVK-1+3)

! --- PARAMETRE NPREC
      CALL JEVEUO(NOSOLV//'.SLVI','E',JSLVI)
      NPREC=ZI(JSLVI)

! --- MUMPS PARALLELE DISTRIBUE ?
      CALL JEVEUO(NOMAT//'.REFA','L',JREFA)
      LDIST=(ZK24(JREFA-1+11).NE.'MPI_COMPLET')
      RANG=DMPSK%MYID
      NBPROC=DMPSK%NPROCS

! --- MATRICE ASTER DISTRIBUEE ?
      LMD = ZK24(JSLVK-1+10)(1:3).EQ.'OUI'

! --- MUMPS EST-IL UTILISE COMME PRECONDITIONNEUR ?
! --- SI OUI, ON DEBRANCHE LES ALARMES ET INFO (PAS LES UTMESS_F)
      LPRECO = ZK24(JSLVK-1+8)(1:3).EQ.'OUI'

! --- FILTRAGE DE LA MATRICE DONNEE A MUMPS (UNIQUEMENT NON LINEAIRE)
      EPSMAT=ZR(JSLVR-1+1)

! --- STRATEGIE MEMOIRE POUR MUMPS
      USERSM=ZK24(JSLVK+8)

! --- POUR MONITORING
      CALL AMUMPT(0,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,TYPE,
     &            IETDEB,IETRAT,RCTDEB,LDIST)

!     ------------------------------------------------
!     ------------------------------------------------
      IF (ACTION(1:6).EQ.'PRERES') THEN
!     ------------------------------------------------
!     ------------------------------------------------

!       ------------------------------------------------
!        INITIALISATION DE L'OCCURENCE MUMPS KXMPS:
!       ------------------------------------------------
        CALL AMUMPI(0,LQUALI,LDIST,KXMPS,TYPE)
        CALL DMUMPS(DMPSK)
        RANG=DMPSK%MYID
        NBPROC=DMPSK%NPROCS

!       --------------------------------------------------------------
!        CHOIX ICNTL VECTEUR DE PARAMETRES POUR MUMPS (ANALYSE+FACTO):
!       --------------------------------------------------------------
        CALL AMUMPI(2,LQUALI,LDIST,KXMPS,TYPE)

!       ----------------------------------------------------------
!        ON RECUPERE ET STOCKE DS SD_SOLVEUR LE NUMERO DE VERSION
!        LICITE
!       ----------------------------------------------------------
        CALL AMUMPU(3,TYPE,KXMPS,K12BID,IBID,LBID,KVERS)
        ZK24(JSLVK-1+12)=KVERS

!       -----------------------------------------------------
!       CALCUL DU DETERMINANT PART I ?
!       -----------------------------------------------------
        LDET=.FALSE.
        IF (ZI(JSLVI-1+5).EQ.1) THEN
          SELECT CASE(KVERS)
          CASE('4.10.0')
! --- ON DEBRANCHE ELIM_LAGR2='NON' CAR CELA FAUSSE LA VALEUR DU DETER
! --- MINANT PAR RAPPORT AUX AUTRES SOLVEURS DIRECTS
            IF ((NIV.GE.2).AND.(LBIS).AND.(.NOT.LPRECO))
     &        CALL U2MESS('I','FACTOR_88')
            ZK24(JSLVK-1+6)='NON'
            KLAG2='NON'
            LBIS=.FALSE.
            LDET=.TRUE.
          CASE('4.9.2')
            CALL U2MESK('F','FACTOR_87',1,KVERS)
          END SELECT
        ENDIF

!       ------------------------------------------------
!        REMPLISSAGE DE LA MATRICE MUMPS :
!       ------------------------------------------------
        CALL AMUMPT(1,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,TYPE,
     &              IETDEB,IETRAT,RCTDEB,LDIST)
        CALL AMUMPM(LDIST,KXMPS,KMONIT,IMPR,IFMUMP,KLAG2,
     &              TYPE,LMD,EPSMAT,KTYPR,LPRECO)

!       -----------------------------------------------------
!       CONSERVE-T-ON LES FACTEURS OU NON ?
!       -----------------------------------------------------
        IF (ZI(JSLVI-1+4).EQ.1) THEN
          SELECT CASE(KVERS)
          CASE('4.10.0')
            DMPSK%ICNTL(31)=1
          CASE('4.9.2')
            IF ((NIV.GE.2).AND.(.NOT.LPRECO))
     &        CALL U2MESK('I','FACTOR_86',1,KVERS)
          END SELECT
        ENDIF

!       ------------------------------------------------
!        ANALYSE MUMPS:
!       ------------------------------------------------
        CALL AMUMPT(2,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,TYPE,
     &              IETDEB,IETRAT,RCTDEB,LDIST)
        DMPSK%JOB = 1
        CALL DMUMPS(DMPSK)
        CALL AMUMPT(4,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,TYPE,
     &              IETDEB,IETRAT,RCTDEB,LDIST)

!       ------------------------------------------------
!        GESTION ERREURS ET MENAGE ASTER:
!       ------------------------------------------------
        IF (DMPSK%INFOG(1).EQ.0) THEN
!              -- C'EST OK
        ELSE IF ((DMPSK%INFOG(1).EQ.-5).OR.(DMPSK%INFOG(1).EQ.-7)) THEN
          CALL U2MESS('F','FACTOR_64')
        ELSE IF (DMPSK%INFOG(1).EQ.-6) THEN
          IRET=2
          GOTO 99
        ELSE
          IAUX=DMPSK%INFOG(1)
          IF (IAUX.LT.0) THEN
            CALL U2MESI('F','FACTOR_55',1,IAUX)
          ELSE
            IF (.NOT.LPRECO) CALL U2MESI('A','FACTOR_55',1,IAUX)
          ENDIF
        ENDIF
        IF ((ZK24(JSLVK-1+4).NE.'AUTO').AND.
     &      (DMPSK%ICNTL(7).NE.DMPSK%INFOG(7)).AND.(.NOT.LPRECO))
     &    CALL U2MESK('A','FACTOR_50',1,ZK24(JSLVK-1+4))

!       -----------------------------------------------------
!        CHOIX DE LA STRATEGIE MUMPS POUR LA GESTION MEMOIRE
!       -----------------------------------------------------
        CALL AMUMPU(1,'D',KXMPS,USERSM,IBID,LBID,K12BID)
! ---   ON SORT POUR REVENIR A AMUMPH ET DETRUIRE L'OCCURENCE MUMPS
! ---   ASSOCIEE
        IF (USERSM(1:4).EQ.'EVAL') GOTO 99

!       -----------------------------------------------------
!       CALCUL DU DETERMINANT PART II ?
!       -----------------------------------------------------
        IF (LDET) DMPSK%ICNTL(33)=1

!       ------------------------------------------------
!        FACTORISATION NUMERIQUE MUMPS:
!       ------------------------------------------------

! --- SI GESTION_MEMOIRE='AUTO'
! --- ON TENTE PLUSIEURS (PCENTP(1)) FACTORISATIONS NUMERIQUES EN
! --- MULTIPLIANT, A CHAQUE ECHEC, L'ANCIEN PCENT_PIVOT PAR PCENTP(2)
! --- VOIRE EN PASSANT EN OOC (EN DERNIER RESSORT).
! --- AUTO-ADAPTATION DU PARAMETRAGE SOLVEUR/PCENT_PIVOT:
! --- ON MODIFIE LE PARAMETRE DANS LA SD_SOLVEUR A LA VOLEE POUR NE
! --- PAS PERDRE DE TEMPS LA PROCHAINE FOIS. CETTE VALEUR N'EST VALABLE
! --- QUE DANS L'OPERATEUR CONSIDERE.
! --- ON FAIT LA MEME CHOSE EN CAS DE PB D'ALLOCATION MEMOIRE (INFOG=-13)
! --- CELA PEUT ETRE DU A UN ICNTL(23) MAL ESTIME
!
        DMPSK%JOB = 2
        IFACT=0
        LPB13=.FALSE.
        IF (USERSM(1:4).EQ.'AUTO') THEN
          IFACTM=PCENTP(1)
        ELSE
          IFACTM=1
        ENDIF
        IF (LRESOL) THEN
          PCPI=DMPSK%ICNTL(14)
          DO IFACT=1,IFACTM
            CALL DMUMPS(DMPSK)
            IAUX=DMPSK%INFOG(1)

! --- TRAITEMENT CORRECTIF ICNTL(14)
            IF ((IAUX.EQ.-8).OR.(IAUX.EQ.-9).OR.(IAUX.EQ.-14).OR.
     &          (IAUX.EQ.-15).OR.(IAUX.EQ.-17).OR.(IAUX.EQ.-20)) THEN
              IF (IFACT.EQ.IFACTM) THEN
! ---  ICNTL(14): PLUS DE NOUVELLE TENTATIVE POSSIBLE
                IF (LPRECO) THEN
!                 -- MUMPS EST APPELE COMME PRECONDITIONNEUR
!                 -- ON SORT AVEC UN CODE RETOUR NON NUL
                  IRET = 1
                  GOTO 99
                ELSE
                  VALI(1)=IFACTM
                  VALI(2)=PCPI
                  VALI(3)=DMPSK%ICNTL(14)
                  CALL U2MESI('F','FACTOR_53',3,VALI)
                ENDIF
              ELSE
! ---  ICNTL(14): ON MODIFIE DES PARAMETRES POUR LA NOUVELLE TENTATIVE
                DMPSK%ICNTL(14)=DMPSK%ICNTL(14)*PCENTP(2)
                ZI(JSLVI-1+2)=DMPSK%ICNTL(14)
                IF ((IAUX.EQ.-9).OR.(IFACT.EQ.(IFACTM-1)))
     &            DMPSK%ICNTL(22)=1
                IF ((NIV.GE.2).AND.(.NOT.LPRECO)) THEN
                  VALI(1)=DMPSK%ICNTL(14)/PCENTP(2)
                  VALI(2)=DMPSK%ICNTL(14)
                  VALI(3)=IFACT
                  VALI(4)=IFACTM
                  CALL U2MESI('I','FACTOR_58',4,VALI)
                ENDIF
              ENDIF

! --- TRAITEMENT CORRECTIF ICNTL(23)
! --- CE N'EST UTILE QU' UNE FOIS D'OU LE CONTROLE DE LPB13
            ELSE IF ((IAUX.EQ.-13).AND.(.NOT.LPB13)) THEN
! ---  ICNTL(23): ON MODIFIE DES PARAMETRES POUR LA NOUVELLE TENTATIVE
              IF ((NIV.GE.2).AND.(.NOT.LPRECO)) THEN
                VALI(1)=DMPSK%ICNTL(23)
                CALL U2MESI('I','FACTOR_85',1,VALI)
              ENDIF
              LPB13=.TRUE.
              DMPSK%ICNTL(23)=0
            ELSE
! ---  SORTIE STANDARD SANS ERREUR
              EXIT
            ENDIF
          ENDDO
        ENDIF

! ---  AFFICHAGE DE CONTROLE
        IF (NIV.GE.2) THEN
          WRITE(IFM,*)
          WRITE(IFM,*)
     &      '<AMUMPD> FACTO. NUM. - NBRE TENTATIVES/MAX: ',IFACT,IFACTM
        ENDIF
        CALL AMUMPT(6,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,TYPE,
     &              IETDEB,IETRAT,RCTDEB,LDIST)

!       ------------------------------------------------
!        GESTION ERREURS ET MENAGE ASTER (SAUF ERREUR ICNTL(14/23)
!           TRAITEE EN AMONT):
!       ------------------------------------------------
        VALR(1)=(DMPSK%INFOG(13)*100.D0)/DMPSK%N
        VALR(2)=DMPSK%ICNTL(14)*1.D0
        IF (VALR(1).GE.VALR(2)) THEN
          IF ((NIV.GE.2).AND.(.NOT.LPRECO))
     &      CALL U2MESR('I','FACTOR_73',2,VALR)
        ENDIF
        IF (DMPSK%INFOG(1).EQ.0) THEN
!              -- C'EST OK
        ELSE IF (DMPSK%INFOG(1).EQ.-10) THEN
          IRET=2
          GOTO 99
        ELSE IF (DMPSK%INFOG(1).EQ.-13) THEN
          CALL U2MESS('F','FACTOR_54')
        ELSE IF (DMPSK%INFOG(1).EQ.-37) THEN
          CALL U2MESS('F','FACTOR_65')
       ELSE IF (DMPSK%INFOG(1).EQ.-90) THEN
          CALL U2MESS('F','FACTOR_66'  )
       ELSE
          IAUX=DMPSK%INFOG(1)
          IF (IAUX.LT.0) THEN
            CALL U2MESI('F','FACTOR_55',1,IAUX)
          ELSE
            IF (.NOT.LPRECO) CALL U2MESI('A','FACTOR_55',1,IAUX)
          ENDIF
        ENDIF

!       ------------------------------------------------
!        DETECTION DE SINGULARITE SI NECESSAIRE:
!       ------------------------------------------------
        CALL AMUMPU(2,'D',KXMPS,K8BID,NPREC,LRESOL,K12BID)

!       ------------------------------------------------
!        RECUPERATION DU DETERMINANT SI NECESSAIRE:
!       ------------------------------------------------
        CALL AMUMPU(4,'D',KXMPS,K8BID,IBID,LBID,K12BID)

!       ON SOULAGE LA MEMOIRE JEVEUX DES QUE POSSIBLE D'OBJETS MUMPS
!       INUTILES
        IF ((( RANG.EQ.0).AND.(.NOT.LDIST)).OR.(LDIST)) THEN
          IF (.NOT.LQUALI.AND..NOT.LOPFAC) THEN
            IF (LDIST) THEN
              DEALLOCATE(DMPSK%A_loc,STAT=IBID)
              DEALLOCATE(DMPSK%IRN_loc,STAT=IBID)
              DEALLOCATE(DMPSK%JCN_loc,STAT=IBID)
            ELSE
              DEALLOCATE(DMPSK%A,STAT=IBID)
              DEALLOCATE(DMPSK%IRN,STAT=IBID)
              DEALLOCATE(DMPSK%JCN,STAT=IBID)
            ENDIF
          ENDIF
        ENDIF

!     ------------------------------------------------
!     ------------------------------------------------
      ELSE IF (ACTION(1:6).EQ.'RESOUD') THEN
!     ------------------------------------------------
!     ------------------------------------------------

!       ------------------------------------------------
!        PRETRAITEMENTS ASTER DU/DES SECOND(S) MEMBRE(S) :
!       ------------------------------------------------
        CALL AMUMPT(7,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,TYPE,
     &              IETDEB,IETRAT,RCTDEB,LDIST)
        CALL AMUMPP(0,NBSOL,KXMPS,LDIST,TYPE,IMPR,IFMUMP,LBIS,
     &              RSOLU,CBID,VCINE,PREPOS,LPRECO)

!       --------------------------------------------------------------
!        CHOIX ICNTL VECTEUR DE PARAMETRES POUR MUMPS (SOLVE):
!       --------------------------------------------------------------
        CALL AMUMPI(3,LQUALI,LDIST,KXMPS,TYPE)

!       ------------------------------------------------
!        RESOLUTION MUMPS :
!       ------------------------------------------------
        CALL AMUMPT(8,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,TYPE,
     &              IETDEB,IETRAT,RCTDEB,LDIST)

        DMPSK%JOB = 3
        IF (LRESOL) CALL DMUMPS(DMPSK)
        CALL AMUMPT(10,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,TYPE,
     &              IETDEB,IETRAT,RCTDEB,LDIST)

!       ------------------------------------------------
!        GESTION ERREURS ET MENAGE ASTER:
!       ------------------------------------------------
        IF (DMPSK%INFOG(1).EQ.0) THEN
!              -- C'EST OK
        ELSE IF ((DMPSK%INFOG(1).EQ.8).AND.(LQUALI)) THEN
          IAUX=DMPSK%INFOG(10)
          IF (.NOT.LPRECO) CALL U2MESI('A','FACTOR_62',1,IAUX)
        ELSE IF (DMPSK%INFOG(1).LT.0) THEN
          IAUX=DMPSK%INFOG(1)
          CALL U2MESI('F','FACTOR_55',1,IAUX)
        ELSE IF (DMPSK%INFOG(1).EQ.4) THEN
!          -- PERMUTATION DE COLONNES, DMPSK%JCN MODIFIE VOLONTAIREMENT
!          -- PAR MUMPS. IL NE FAUT DONC PAS LE MANIPULER TEL QUE
!          -- PAS GRAVE POUR ASTER.
        ELSE
          IAUX=DMPSK%INFOG(1)
          IF (.NOT.LPRECO) CALL U2MESI('A','FACTOR_55',1,IAUX)
        ENDIF
! --- CONTROLE DE L'ERREUR SUR LA SOLUTION :
        IF (LQUALI) THEN
          IF (DMPSK%RINFOG(9).GT.EPSMAX) THEN
            VALR(1)=DMPSK%RINFOG(9)
            VALR(2)=EPSMAX
            CALL U2MESR('F','FACTOR_57',2,VALR)
          ENDIF
        ENDIF

!       ------------------------------------------------
!        POST-TRAITEMENTS ASTER DE/DES (LA) SOLUTION(S) :
!       ------------------------------------------------
        CALL AMUMPP(2,NBSOL,KXMPS,LDIST,TYPE,IMPR,IFMUMP,LBIS,
     &              RSOLU,CBID,VCINE,PREPOS,LPRECO)

!       ------------------------------------------------
!        AFFICHAGE DU MONITORING :
!       ------------------------------------------------
        CALL AMUMPT(12,KMONIT,TEMPS,RANG,NBPROC,KXMPS,LQUALI,TYPE,
     &              IETDEB,IETRAT,RCTDEB,LDIST)

!     ------------------------------------------------
!     ------------------------------------------------
      ELSE IF (ACTION(1:5).EQ.'DETR_') THEN
!     ------------------------------------------------
!     ------------------------------------------------

!       ------------------------------------------------
!        MENAGE ASTER ET MUMPS:
!       ------------------------------------------------
        IF (NOMATS(KXMPS).NE.' ') THEN
          IF ((( RANG.EQ.0).AND.(.NOT.LDIST)).OR.(LDIST)) THEN
            IF (LDIST) THEN
              DEALLOCATE(DMPSK%A_loc,STAT=IBID)
              DEALLOCATE(DMPSK%IRN_loc,STAT=IBID)
              DEALLOCATE(DMPSK%JCN_loc,STAT=IBID)
            ELSE
              DEALLOCATE(DMPSK%A,STAT=IBID)
              DEALLOCATE(DMPSK%IRN,STAT=IBID)
              DEALLOCATE(DMPSK%JCN,STAT=IBID)
            ENDIF
          ENDIF
          ETAMS(KXMPS)=' '
          NONUS(KXMPS)=' '
          NOMATS(KXMPS)=' '
          NOSOLS(KXMPS)=' '
          ROUCS(KXMPS)=' '
          PRECS(KXMPS)=' '
          DMPSK%JOB = -2
          CALL DMUMPS(DMPSK)
! NETTOYAGE OBJETS AUXILIAIRES AU CAS OU
          K24AUX='&&TAILLE_OBJ_MUMPS'
          CALL JEEXIN(K24AUX,IBID)
          IF (IBID.NE.0) CALL JEDETR(K24AUX)
!
          K24AUX='&&AMUMP.PIVNUL'
          CALL JEEXIN(K24AUX,IBID)
          IF (IBID.NE.0) CALL JEDETR(K24AUX)
!
          K24AUX='&&AMUMP.DETERMINANT'
          CALL JEEXIN(K24AUX,IBID)
          IF (IBID.NE.0) CALL JEDETR(K24AUX)
        ENDIF
      ENDIF

!     -- ON REACTIVE LA LEVEE D'EXCEPTION
   99 CALL MATFPE(1)
      CALL JEDEMA()

#endif
      END

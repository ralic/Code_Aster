      SUBROUTINE OP0026(IER   )
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION 
C MODIF CALCULEL  DATE 23/09/2008   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY  
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY  
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR     
C (AT YOUR OPTION) ANY LATER VERSION.                                   
C                                                                       
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT   
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF            
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU      
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.                              
C                                                                       
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE     
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,         
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      INTEGER  IER

C-----------------------------------------------------------------------
C
C           O P E R A T E U R    C A L C U L
C           ================================
C
C-----------------------------------------------------------------------
C

C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
      INTEGER ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER         NBPAR
      PARAMETER       (NBPAR=3)
      CHARACTER*19    NOMPAR(NBPAR),TYPPAR(NBPAR)
      CHARACTER*24    VK(NBPAR)   
C
      INTEGER         ZFON  
      PARAMETER      (ZFON    = 22 )        
C-----------------------------------------------------------------------
      INTEGER         N1,NBOPT,ITERAT,NUMEOR,I,IBID
      INTEGER         NIV,IFM
      INTEGER         IRET,KNINDI,NUORD,NBPASE,JLINST
      REAL*8          RBID,INSTAM,INSTAP,CONST(2)
      COMPLEX*16      CBID
      CHARACTER*1     TYPCST(2),TYPECH(2),TYPRES
      CHARACTER*2     CODRET
      CHARACTER*8     RESULT,TABLE
      CHARACTER*14    NUMMOI,NUMDEL
      CHARACTER*16    LOPT(3),OPTION
      CHARACTER*19    LISCHA,K19BLA
      CHARACTER*19    LINST
      CHARACTER*19    MEDIRI,MERIGI,VEFINT
      CHARACTER*24    DEPDEL,MATE,CARELE,COMPOR,CARCRI,INPSCO
      CHARACTER*24    DEPPLU,SIGPLU,VARPLU,COMMOI,MODELE,DEPMOI
      CHARACTER*24    COMPLU, CODERE
      CHARACTER*24    VALMOI(8),VALPLU(8)
      CHARACTER*24    SIGMOI,VARMOI,NOMCH(2),CHPRES,DEPMO1
      CHARACTER*24    K24BLA,K24BID
      CHARACTER*19    SOLALG(30)   
      CHARACTER*19    MEELEM(20),VEELEM(30)   
      LOGICAL         TABRET(0:10),FONACT(ZFON)
C-----------------------------------------------------------------------
      DATA LISCHA     /'&&OP0026.LISCHA'/
      DATA CARELE     /'&&OP0026.CARELE'/
      DATA CARCRI     /'&&OP0026.CARCRI'/
      DATA DEPPLU     /'&&OP0026.DEPPLU'/
      DATA DEPMO1     /'&&OP0026.DEPMO1'/
      DATA VARMOI     /'&&OP0026.VARMOI'/
      DATA SIGMOI     /'&&OP0026.SIGMOI'/
      DATA COMMOI     /'&&OP0026.COMMOI'/
      DATA COMPLU     /'&&OP0026.COMPLU'/
C      
C ----------------------------------------------------------------------
C      
      CALL JEMARQ()


C ======================================================================
C --- RECUPERATION DES ARGUMENTS  DE LA COMMANDE
C ======================================================================

      CALL INFMAJ
      CALL INFNIV(IFM,NIV)

C     RECUPERATION DU NOM DE LA TABLE PRODUITE
C     ----------------------------------------
      CALL GETRES(TABLE,K24BID,K24BID)

C     DEFINITION DES NOMS DES SD
C     --------------------------
      CALL GCNCON('_',SIGPLU)
      CALL GCNCON('_',VARPLU)
      CALL GCNCON('_',MERIGI)
      CALL GCNCON('_',VEFINT)
      CALL GCNCON('_',MEDIRI)
      CALL GCNCON('_',CODERE)

C     RECUPERATION DES OPTIONS DEMANDEES
C     ----------------------------------
      CALL GETVTX(' ','OPTION',0,1,3,LOPT,NBOPT)

      IF (KNINDI(16,'MATR_TANG_ELEM',LOPT,NBOPT).GT.0) THEN
         OPTION='FULL_MECA'
      ELSEIF  (KNINDI(16,'MATR_TANG_ELEM',LOPT,NBOPT).EQ.0) THEN
         OPTION='RAPH_MECA'
      ENDIF

C     RECUPERATION DU MODELE, DU MATERIAU, DES CHARGES
C     ------------------------------------------------
      NBPASE = 0
      NUORD  = 0
      MODELE = ' '
      K19BLA = ' '
      K24BLA = ' '
      DO 2 I = 1,ZFON
        FONACT(I) = .FALSE.
  2   CONTINUE
      CALL NMDOME(MODELE,MATE,CARELE,LISCHA,NBPASE,INPSCO,
     &            RESULT, NUORD)


C     RECUPERATION DES DEPLACEMENTS
C     -----------------------------
      CALL GETVID(' ','DEPL',0,1,1,DEPMOI,N1)
      CALL GETVID(' ','INCR_DEPL',0,1,1,DEPDEL,N1)
C     SI LES CHAMPS N'ONT PAS LA MEME NUMEROTATION, ON TRANSFERT DEPMOI
C     DANS LA NUMEROTATION DE DEPDEL
      CALL DISMOI('F','NOM_NUME_DDL',DEPMOI,'CHAM_NO',IBID,NUMMOI,IBID)
      CALL DISMOI('F','NOM_NUME_DDL',DEPDEL,'CHAM_NO',IBID,NUMDEL,IBID)
      IF (NUMMOI.NE.NUMDEL) THEN
        CALL COPISD('CHAMP_GD','V',DEPDEL,DEPMO1)
        CALL VTCOPY(DEPMOI,DEPMO1,IER)
      ELSE
        DEPMO1=DEPMOI
      ENDIF
C     ON CALCULE LE CHAMP DEPPLU=DEPMO1+DEPDEL
      TYPCST(1)='R'
      TYPCST(2)='R'
      CONST(1)=1.D0
      CONST(2)=1.D0
      TYPECH(1)='R'
      TYPECH(2)='R'
      NOMCH(1)=DEPMO1
      NOMCH(2)=DEPDEL
      TYPRES='R'
      CHPRES=DEPPLU
      CALL VTCMBL(2,TYPCST,CONST,TYPECH,NOMCH,TYPRES,CHPRES)


C     RECUPERATION DES CONTRAINTES ET VARIABLES INTERNES
C     --------------------------------------------------
      CALL GETVID(' ','SIGM',0,1,1,SIGMOI,N1)
      CALL GETVID(' ','VARI',0,1,1,VARMOI,N1)
C     VERIFICATION DE LA NATURE DES CHAMPS
      CALL CHPVER('F',SIGMOI,'ELGA','SIEF_R',IRET)
      CALL CHPVER('F',VARMOI,'ELGA','VARI_R',IRET)


C     RECUPERATION DU COMPORTEMENT
C     ----------------------------
      CALL NMDORC(MODELE,COMPOR,CARCRI)

C     RECUPERATION DU NUMERO D'ORDRE ET DE L'INSTANT COURANTS
C     -------------------------------------------------------
      LINST=' '
      CALL GETVIS('INCREMENT','NUME_ORDRE',1,1,1,NUMEOR,N1)
      CALL GETVID('INCREMENT','LIST_INST',1,1,1,LINST,N1)
      CALL JEVEUO(LINST//'.VALE','L',JLINST)
      INSTAP=ZR(JLINST-1+NUMEOR)
      INSTAM=ZR(JLINST-1+NUMEOR-1)

C     LECTURE DES VARIABLES DE COMMANDE A L'INSTANT COURANT
C     -----------------------------------------------------
      CALL NMVCLE(MODELE,MATE,CARELE,LISCHA,INSTAP,COMPLU,CODRET)
      CALL NMVCLE(MODELE,MATE,CARELE,LISCHA,INSTAM,COMMOI,CODRET)


C ======================================================================
C --- PREPARATION DES ARGUMENTS DE CALCUL
C ======================================================================

C
C --- VARIABLES CHAPEAUX
C     A NOTER : MEDIRI EST ICI UN OBJET BIDON, TOUTES LES MATRICES 
C     -------   DE "LAGRANGE" SONT EN FAIT AJOUTÉES DANS L'OBJET MERIGI
C               DANS L'APPEL À MEDIME AVEC OPTION 'CUMUL'
C
      CALL AGGLOM(DEPMOI,SIGMOI,VARMOI,COMMOI,K24BLA, 
     &            K24BLA,K24BLA,K24BLA,4     ,VALMOI)
      CALL AGGLOM(DEPPLU,SIGPLU,VARPLU,COMPLU,K24BLA, 
     &            K24BLA,K24BLA,K24BLA,4     ,VALPLU) 
      SOLALG(2) = DEPDEL     
      MEELEM(1) = MERIGI 
      MEELEM(2) = MEDIRI       
      VEELEM(1) = VEFINT   
C
C --- CALCUL DE LA CONTRIBUTION DES DDL PHYSIQUES (MERIGI ET VEFINT)
C       
      ITERAT=1
     
      CALL MERIMO('G'   ,MODELE,CARELE,MATE  ,K24BID,
     &            COMPOR,LISCHA,CARCRI,ITERAT,FONACT,
     &            K19BLA,VALMOI,VALPLU,K24BID,SOLALG,
     &            MEELEM,VEELEM,OPTION,TABRET,CODERE)

C
C --- CALCUL DE LA CONTRIBUTION DES "LAGRANGE"
C 
      CALL MEDIME('G','CUMU',MODELE,LISCHA,MERIGI)


C ======================================================================
C --- ECRITURE DES RESULTATS DANS UNE TABLE
C ======================================================================

      CALL DETRSD('TABLE_CONTAINER',TABLE)
      CALL TBCRSD(TABLE,'G')

      NOMPAR(1)='NOM_OBJET'
      TYPPAR(1)='K16'

      NOMPAR(2)='TYPE_OBJET'
      TYPPAR(2)='K16'

      NOMPAR(3)='NOM_SD'
      TYPPAR(3)='K24'

      CALL TBAJPA(TABLE,NBPAR,NOMPAR,TYPPAR)
      VK(1)='MATR_ELEM'
      VK(2)='MATR_ELEM_DEPL_R'
      VK(3)=MERIGI
      CALL TBAJLI(TABLE,NBPAR,NOMPAR,IBID,RBID,CBID,VK,0)
      VK(1)='SIEF_ELGA'
      VK(2)='CHAM_ELEM'
      VK(3)=SIGPLU
      CALL TBAJLI(TABLE,NBPAR,NOMPAR,IBID,RBID,CBID,VK,0)
      VK(1)='VARI_ELGA'
      VK(2)='CHAM_ELEM'
      VK(3)=VARPLU
      CALL TBAJLI(TABLE,NBPAR,NOMPAR,IBID,RBID,CBID,VK,0)
      VK(1)='VECT_ELEM'
      VK(2)='VECT_ELEM_DEPL_R'
      VK(3)=VEFINT
      CALL TBAJLI(TABLE,NBPAR,NOMPAR,IBID,RBID,CBID,VK,0)
      VK(1)='CODE_RETOUR'
      VK(2)='CHAM_ELEM'
      VK(3)=CODERE
      CALL TBAJLI(TABLE,NBPAR,NOMPAR,IBID,RBID,CBID,VK,0)


C ======================================================================
C --- SORTIE
C ======================================================================

      CALL JEDEMA()

      END

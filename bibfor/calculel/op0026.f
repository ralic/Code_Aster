      SUBROUTINE OP0026 ( IER )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 12/11/2007   AUTEUR COURTOIS M.COURTOIS 
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
C-----------------------------------------------------------------------
C
C           O P E R A T E U R    C A L C U L
C           ================================
C
C-----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INTEGER  IER
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
      CHARACTER*32       JEXNOM, JEXNUM,JEXATR
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C
C ----- DECLARATIONS
C
C-----------------------------------------------------------------------
      INTEGER         NBPAR
      PARAMETER       (NBPAR=5)
C-----------------------------------------------------------------------
      INTEGER         N1,NIV,IFM,NBOPT,ITERAT,NUMEOR,I
      INTEGER         IRET,KNINDI,IBID,NUORD,NBPASE,VI(1),JLINST
      REAL*8          RBID,INSTAM,INSTAP,CONST(2)
      COMPLEX*16      CBID
      CHARACTER*1     TYPCST(2),TYPECH(2),TYPRES
      CHARACTER*8     RESULT,TABLE
      CHARACTER*14    NUMMOI,NUMDEL
      CHARACTER*16    LOPT(3),OPTION
      CHARACTER*19    LISCHA
      CHARACTER*19    NOMPAR(NBPAR),TYPPAR(NBPAR),LINST
      CHARACTER*24    DEPDEL,MATE,CARELE,COMPOR,CARCRI,MERIGI,INPSCO
      CHARACTER*24    DEPPLU,SIGPLU,VARPLU,VERESI,COMMOI,MODELE,DEPMOI
      CHARACTER*24    COMPLU,K24BID,VALMOI(8),VALPLU(8),VEDIRI,VK(NBPAR)
      CHARACTER*24    SIGMOI,VARMOI,NOMCH(2),CHPRES,DEPMO1
      LOGICAL         TABRET(0:10)
C-----------------------------------------------------------------------
      DATA LISCHA     /'&&OP0026.LISCHA'/
      DATA CARELE     /'&&OP0026.CARELE'/
      DATA CARCRI     /'&&OP0026.CARCRI'/
      DATA DEPPLU     /'&&OP0026.DEPPLU'/
      DATA DEPMO1     /'&&OP0026.DEPMO1'/
      DATA VARMOI     /'&&OP0026.VARMOI'/
      DATA VARPLU     /'&&OP0026.VARPLU'/
      DATA SIGMOI     /'&&OP0026.SIGMOI'/
      DATA SIGPLU     /'&&OP0026.SIGPLU'/
      DATA COMMOI     /'&&OP0026.COMMOI'/
      DATA COMPLU     /'&&OP0026.COMPLU'/
C-----------------------------------------------------------------------

      CALL JEMARQ()


C ======================================================================
C --- RECUPERATION DES ARGUMENTS  DE LA COMMANDE
C ======================================================================

      CALL INFMAJ
      CALL INFNIV(IFM,NIV)

C     RECUPERATION DU NOM DE LA TABLE PRODUITE
C     ----------------------------------------
      CALL GETRES(TABLE,K24BID,K24BID)


C     RECUPERATION DES OPTIONS DEMANDEES
C     ----------------------------------
      CALL GETVTX(' ','OPTION',0,1,3,LOPT,NBOPT)

      IF (KNINDI(16,'MATR_TANG_ELEM',LOPT,NBOPT).GT.0) THEN
         OPTION='FULL_MECA'
      ELSEIF  (KNINDI(16,'MATR_TANG_ELEM',LOPT,NBOPT).EQ.0) THEN
         OPTION='RAPH_MECA'
      ENDIF

      IF (NIV.GE.2) THEN
        WRITE(IFM,*) 'CALCUL DE L''OPTION '//OPTION
      ENDIF


C     RECUPERATION DU MODELE, DU MATERIAU, DES CHARGES
C     ------------------------------------------------
      NBPASE=0
      NUORD=0
      MODELE=' '
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
        CALL COPISD('CHAMP_GD','G',DEPDEL,DEPMO1)
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
      CALL NMVCLE(MODELE,MATE,CARELE,LISCHA,INSTAP,COMPLU)
      CALL NMVCLE(MODELE,MATE,CARELE,LISCHA,INSTAM,COMMOI)


C ======================================================================
C --- PREPARATION DES ARGUMENTS DE CALCUL
C ======================================================================

C     CALCUL DE LA CONTRIBUTION "PHYSIQUE"
C     ------------------------------------
      CALL AGGLOM(DEPMOI, SIGMOI, VARMOI, COMMOI, K24BID, K24BID,
     &            K24BID, K24BID, 4,      VALMOI)
      CALL AGGLOM(DEPPLU, SIGPLU, VARPLU, COMPLU, K24BID, K24BID,
     &            K24BID, K24BID, 4,      VALPLU)

      ITERAT=1
      MERIGI = ' '
      VERESI = ' '
      VEDIRI = ' '
      CALL MERIMO('G',    MODELE, CARELE, MATE,   K24BID, COMPOR,
     &            LISCHA, CARCRI, DEPDEL, K24BID, K24BID, K24BID,
     &            K24BID, VALMOI, VALPLU, OPTION, MERIGI, VERESI,
     &            VEDIRI, ITERAT, TABRET)

      CALL DESAGG(VALPLU, DEPPLU, SIGPLU, VARPLU, COMPLU, K24BID,
     &            K24BID, K24BID, K24BID)


C     CALCUL DE LA CONTRIBUTION DES "LAGRANGE"
C     ----------------------------------------
      CALL MEDIME(MODELE,LISCHA,MERIGI)


C ======================================================================
C --- ECRITURE DES RESULTATS DANS UNE TABLE
C ======================================================================

      CALL DETRSD('TABLE',TABLE)
      CALL TBCRSD(TABLE,'G')
      NOMPAR(1)='MATR_ELEM'
      TYPPAR(1)='K24'
      NOMPAR(2)='SIEF_ELGA'
      TYPPAR(2)='K24'
      NOMPAR(3)='VARI_ELGA'
      TYPPAR(3)='K24'
      NOMPAR(4)='VECT_ELEM'
      TYPPAR(4)='K24'
      NOMPAR(5)='CODRET'
      TYPPAR(5)='I'
      CALL TBAJPA(TABLE,NBPAR,NOMPAR,TYPPAR)
      VK(1)=MERIGI
      VK(2)=SIGPLU
      VK(3)=VARPLU
      VK(4)=VERESI
      IF (TABRET(0)) THEN
        VI(1)=1
      ELSE
        VI(1)=0
      ENDIF
      CALL TBAJLI(TABLE,NBPAR,NOMPAR,VI,RBID,CBID,VK,0)


C ======================================================================
C --- SORTIE
C ======================================================================

      CALL JEDEMA()

      END

      SUBROUTINE OP0026()
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 30/06/2010   AUTEUR DELMAS J.DELMAS 
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
      INTEGER         NBPAR,NBLIBL
      PARAMETER       (NBPAR=5,NBLIBL=5)
      CHARACTER*19    NOMPAR(NBPAR),TYPPAR(NBPAR)
      CHARACTER*24    VK(NBPAR)
      CHARACTER*24    KNOOBJ(NBLIBL),VKK(NBLIBL),KNOTYP(NBLIBL)
      INTEGER         ZSOLAL,ZVALIN
      PARAMETER       (ZSOLAL=17,ZVALIN=18)
C-----------------------------------------------------------------------
      INTEGER         N1,NBOPT,ITERAT,NUMEOR,I,IBID,NBPA
      INTEGER         NIV,IFM,VI(1),JTBNP
      INTEGER         IRET,KNINDI,NUORD,NBPASE,LONG
      INTEGER         INOOBJ, INOMSD, INUORD, IINST, JNOBJ, JNOSD
      INTEGER         JNUOR, JRINS, JLINS, NBLI, J, JTBLP, IER
      REAL*8          INSTAM,INSTAP,CONST(2),VR(1),DIINST
      COMPLEX*16      CBID
      CHARACTER*1     TYPCST(2),TYPECH(2),TYPRES
      CHARACTER*2     CODRET
      CHARACTER*8     RESULT,TABLE,TABLU,K8B
      CHARACTER*19    PFCHN1,PFCHN2
      CHARACTER*16    LOPT(3),OPTION
      CHARACTER*19    LISCHA,K19BLA
      CHARACTER*19    LINST
      CHARACTER*19    MEDIRI,MERIGI,VEFINT
      CHARACTER*24    DEPDEL,MATE,CARELE,COMPOR,CARCRI,INPSCO
      CHARACTER*24    DEPPLU,SIGPLU,VARPLU,COMMOI,MODELE,DEPMOI
      CHARACTER*24    COMPLU, CODERE
      CHARACTER*24    SIGMOI,VARMOI,NOMCH(2),CHPRES,DEPMO1
      CHARACTER*24    K24BID
      CHARACTER*19    NOMTAB
      CHARACTER*19    VALINC(ZVALIN),SOLALG(ZSOLAL)      
      LOGICAL         TABRET(0:10)
      INTEGER         FONACT(100)
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
      DATA KNOOBJ     /'MATR_ELEM               ',
     &                 'SIEF_ELGA               ',
     &                 'VARI_ELGA               ',
     &                 'VECT_ELEM               ',
     &                 'CODE_RETOUR             '/
      DATA KNOTYP     /'MATR_ELEM_DEPL_R        ',
     &                 'CHAM_ELEM               ',
     &                 'CHAM_ELEM               ',
     &                 'VECT_ELEM_DEPL_R        ',
     &                 'CHAM_ELEM               '/
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
      DO 2 I = 1,100
        FONACT(I) = 0
  2   CONTINUE
      CALL NMDOME(MODELE,MATE,CARELE,LISCHA,NBPASE,INPSCO,
     &            RESULT, NUORD)


C     RECUPERATION DES DEPLACEMENTS
C     -----------------------------
      CALL GETVID(' ','DEPL',0,1,1,DEPMOI,N1)
      CALL GETVID(' ','INCR_DEPL',0,1,1,DEPDEL,N1)
C     SI LES CHAMPS N'ONT PAS LA MEME NUMEROTATION, ON TRANSFERT DEPMOI
C     DANS LA NUMEROTATION DE DEPDEL
      CALL DISMOI('F','PROF_CHNO',DEPMOI,'CHAM_NO',IBID,PFCHN1,IBID)
      CALL DISMOI('F','PROF_CHNO',DEPDEL,'CHAM_NO',IBID,PFCHN2,IBID)
      IF (PFCHN1.NE.PFCHN2) THEN
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
      INSTAP=DIINST(LINST,NUMEOR-1)
      INSTAM=DIINST(LINST,NUMEOR)

C     LECTURE DES VARIABLES DE COMMANDE A L'INSTANT COURANT
C     -----------------------------------------------------
      CALL NMVCLE(MODELE,MATE,CARELE,LISCHA,INSTAP,COMPLU,CODRET)
      CALL NMVCLE(MODELE,MATE,CARELE,LISCHA,INSTAM,COMMOI,CODRET)


C ======================================================================
C --- PREPARATION DES ARGUMENTS DE CALCUL
C ======================================================================

C
C --- VARIABLES CHAPEAUX
C
      CALL NMCHAI('SOLALG','LONMAX',LONG  )
      CALL ASSERT(LONG.EQ.ZSOLAL)
      CALL NMCHAI('VALINC','LONMAX',LONG  )
      CALL ASSERT(LONG.EQ.ZVALIN)
      CALL NMCHA0('VALINC','ALLINI',' ',VALINC) 
      CALL NMCHA0('VALINC','DEPMOI',DEPMOI,VALINC)
      CALL NMCHA0('VALINC','SIGMOI',SIGMOI,VALINC)
      CALL NMCHA0('VALINC','COMMOI',COMMOI,VALINC)
      CALL NMCHA0('VALINC','VARMOI',VARMOI,VALINC)
      CALL NMCHA0('VALINC','DEPPLU',DEPPLU,VALINC)
      CALL NMCHA0('VALINC','SIGPLU',SIGPLU,VALINC)
      CALL NMCHA0('VALINC','COMPLU',COMPLU,VALINC)
      CALL NMCHA0('VALINC','VARPLU',VARPLU,VALINC) 
      CALL NMCHA0('SOLALG','ALLINI',' ',SOLALG) 
      CALL NMCHA0('SOLALG','DEPDEL',DEPDEL,SOLALG)
C
C --- CALCUL DE LA CONTRIBUTION DES DDL PHYSIQUES (MERIGI ET VEFINT)
C
      ITERAT=1

      CALL MERIMO('G'   ,MODELE,CARELE,MATE  ,K24BID,
     &            COMPOR,LISCHA,CARCRI,ITERAT,FONACT,
     &            K19BLA,VALINC,SOLALG,MERIGI,VEFINT,
     &            OPTION,TABRET,CODERE)
C
C --- CALCUL DE LA CONTRIBUTION DES "LAGRANGE"
C
      CALL MEDIME('G','CUMU',MODELE,LISCHA,MERIGI)


C ======================================================================
C --- ECRITURE DES RESULTATS DANS UNE TABLE
C ======================================================================
      NOMPAR(1)='NOM_OBJET'
      TYPPAR(1)='K16'

      NOMPAR(2)='TYPE_OBJET'
      TYPPAR(2)='K16'

      NOMPAR(3)='NOM_SD'
      TYPPAR(3)='K24'

      NOMPAR(4)='NUME_ORDRE'
      TYPPAR(4)='I'

      NOMPAR(5)='INST'
      TYPPAR(5)='R8'

      VR(1)=INSTAP
      VI(1)=NUMEOR

      VKK(1)=MERIGI
      VKK(2)=SIGPLU
      VKK(3)=VARPLU
      VKK(4)=VEFINT
      VKK(5)=CODERE

      CALL GETVID(' ','TABLE',0,1,0,K8B,N1)

C     ---------------------------------------------
C     CAS 1 - ON CREE UNE NOUVELLE TABLE CONTAINER
C     ---------------------------------------------
      IF(N1.EQ.0)THEN

        CALL DETRSD('TABLE_CONTAINER',TABLE)
        CALL TBCRSD(TABLE,'G')

        CALL TBAJPA(TABLE,NBPAR,NOMPAR,TYPPAR)
        VK(1)=KNOOBJ(1)
        VK(2)=KNOTYP(1)
        VK(3)=VKK(1)
        CALL TBAJLI(TABLE,NBPAR,NOMPAR,VI,VR,CBID,VK,0)
        VK(1)=KNOOBJ(2)
        VK(2)=KNOTYP(2)
        VK(3)=VKK(2)
        CALL TBAJLI(TABLE,NBPAR,NOMPAR,VI,VR,CBID,VK,0)
        VK(1)=KNOOBJ(3)
        VK(2)=KNOTYP(3)
        VK(3)=VKK(3)
        CALL TBAJLI(TABLE,NBPAR,NOMPAR,VI,VR,CBID,VK,0)
        VK(1)=KNOOBJ(4)
        VK(2)=KNOTYP(4)
        VK(3)=VKK(4)
        CALL TBAJLI(TABLE,NBPAR,NOMPAR,VI,VR,CBID,VK,0)
        VK(1)=KNOOBJ(5)
        VK(2)=KNOTYP(5)
        VK(3)=VKK(5)
        CALL TBAJLI(TABLE,NBPAR,NOMPAR,VI,VR,CBID,VK,0)

      ELSE

C     -----------------------------------------------------
C     CAS 2 - ON ENRICHIT UNE TABLE CONTAINER  OU
C             ON EN CREE UNE NOUVELLE A PARTIR D'UNE AUTRE
C     ----------------------------------------------------

        CALL GETVID(' ','TABLE',0,1,1,TABLU,N1)

        IF(TABLU.NE.TABLE)THEN
          CALL DETRSD('TABLE_CONTAINER',TABLE)
          CALL COPISD('TABLE','G',TABLU,TABLE)
        ENDIF

        NOMTAB=TABLE
        CALL JEVEUO(NOMTAB//'.TBNP','L',JTBNP)
        NBPA=ZI(JTBNP)
        NBLI=ZI(JTBNP+1)

C       RECHERCHE DES PARAMETRES CONCERNES DANS LA TABLE FOURNIE
        INOOBJ=0
        INOMSD=0
        INUORD=0
        IINST=0
        CALL JEVEUO(NOMTAB//'.TBLP','L',JTBLP)
        DO 5 I=1,NBPA
          IF(ZK24(JTBLP+(I-1)*4)(1:9).EQ.'NOM_OBJET')THEN
               INOOBJ=I
          ELSEIF(ZK24(JTBLP+(I-1)*4)(1:6).EQ.'NOM_SD')THEN
               INOMSD=I
          ELSEIF(ZK24(JTBLP+(I-1)*4)(1:10).EQ.'NUME_ORDRE')THEN
               INUORD=I
          ELSEIF(ZK24(JTBLP+(I-1)*4)(1:4).EQ.'INST')THEN
               IINST=I
          ENDIF
 5      CONTINUE

        CALL ASSERT(INOOBJ.NE.0)
        CALL ASSERT(INOMSD.NE.0)
        CALL ASSERT(INUORD.NE.0)
        CALL ASSERT(IINST .NE.0)

C       RECUPERATION DES POINTEURS POUR LIRE ET MODIFIER LA TABLE
        CALL JEVEUO(ZK24(JTBLP+(INOOBJ-1)*4+2),'L',JNOBJ)
        CALL JEVEUO(ZK24(JTBLP+(INOMSD-1)*4+2),'E',JNOSD)
        CALL JEVEUO(ZK24(JTBLP+(INUORD-1)*4+2),'E',JNUOR)
        CALL JEVEUO(ZK24(JTBLP+(IINST -1)*4+2),'E',JRINS)
        CALL JEVEUO(ZK24(JTBLP+(IINST -1)*4+3),'L',JLINS)

C       POUR LES NBLIBL(=5) NOM_OBJET DE CALCUL
        DO 10 I=1,NBLIBL
C         ON PARCOURT LES LIGNES DE LA TABLE POUR:
          DO 20 J=1,NBLI
C          - IDENTIFIER LES LIGNES OU L'ON TROUVE LE NOM_OBJET DE CALCUL
            IF(ZK16(JNOBJ+J-1).EQ.KNOOBJ(I)(1:16))THEN
              IF(ZI(JLINS+J-1).EQ.1)THEN
C          - LIRE L'INSTANT PRESENT DANS LA TABLE:
C            SI CELUI-CI EST IDENTIQUE A L'INSTANT DU NOM_SD A STOCKER
                   IF(ZR(JRINS+J-1).EQ.VR(1))THEN
C            ALORS, ON ECRASE LE CONCEPT NOM_SD ET ON LE REMPLACE PAR
C            LE NOUVEAU (ON MET A JOUR AUSSI NUME_ORDRE ET INST)
                       VK(1)=KNOOBJ(I)
                       VK(2)=TABLU
                       CALL U2MESG('A','TABLE0_16',2,VK,0,IBID,1,VR)
                       CALL JEDETR(ZK24(JNOSD+J-1))
                       ZK24(JNOSD+J-1)=VKK(I)
                       ZI(JNUOR+J-1)=VI(1)
                       ZR(JRINS+J-1)=VR(1)
                       GOTO 10
                    ENDIF
               ENDIF
             ENDIF
 20       CONTINUE
C         SI LE NOM_OBJET ET L'INSTANT N'ONT PAS ETE TROUVES, ALORS ON
C         AJOUTE UNE NOUVELLE LIGNE A LA TABLE:
          VK(1)=KNOOBJ(I)
          VK(2)=KNOTYP(I)
          VK(3)=VKK(I)
          CALL TBAJLI(TABLE,NBPAR,NOMPAR,VI,VR,CBID,VK,0)
 10     CONTINUE

      ENDIF


C ======================================================================
C --- SORTIE
C ======================================================================

      CALL JEDEMA()

      END

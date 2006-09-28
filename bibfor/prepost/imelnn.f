      SUBROUTINE IMELNN ( IFM, NOMSDZ, NBCMP, LISCMZ,
     &                    NBELEM, LISMAZ, NBCHIF )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE CRP_20
C.======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
C      IMELNN -- IMPRESSION DU MATR_ELEM DE NOM NOMSD
C                (SYMETRIQUE OU NON-SYMETRIQUE)
C                DANS LE FICHIER DE NOM FICHIER.
C                LE GRAIN DE L'IMPRESSION EST 'NOEUD'
C                (I.E. ON IMPRIME LES SOUS_MATRICES DONT LES TERMES
C                      COUPLENT 2 NOEUDS)
C
C
C   ARGUMENT        E/S  TYPE         ROLE
C    IFM             IN    I     UNITE LOGIQUE D'IMPRESSION DU
C                                MATR_ELEM,
C    NOMSDZ          IN    K*    NOM DU MATR_ELEM
C    NBCMP           IN    I     NOMBRE DE COMPOSANTES DE LA LISTE
C                                LISCMZ DES COMPOSANTES
C    LISCMZ          IN    K*    LISTE DES COMPOSANTES POUR-LESQUELLES
C                                ON DESIRE L'IMPRESSION DES VALEURS
C                                DU MATR_ELEM
C    NBELEM          IN    I     NOMBRE DE TERMES DE LA LISTE LISMAZ
C    LISMAZ          IN    K*    LISTE DES MAILLES POUR-LESQUELLES
C                                ON DESIRE L'IMPRESSION DES VALEURS
C                                DU MATR_ELEM
C    NBCHIF          IN    I     NOMBRE DE CHIFFRES SIGNICATIFS
C.========================= DEBUT DES DECLARATIONS ====================
C ----- COMMUNS NORMALISES  JEVEUX
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNUM, JEXNOM, JEXATR
C -----  ARGUMENTS
      CHARACTER*(*) NOMSDZ, LISMAZ, LISCMZ
C -----  VARIABLES LOCALES
      PARAMETER     (NBNOMX = 27)
      PARAMETER     (NBVAIM = 1323)
      PARAMETER     (NBECMX = 10)
      CHARACTER*1   K1BID, TYMAT, TYPVAL
      CHARACTER*2   KLONLG , KLONLS, KLONLO
      CHARACTER*7   SYM
      CHARACTER*8   K8BID, NOMA, NOMGD, NOMNO1, NOMCM1
      CHARACTER*8   M8BLAN, MATEL, NOMNOE, NOMAIL
      CHARACTER*8   NONO(NBVAIM), NOCMP(NBVAIM)
      CHARACTER*8   SCAL, SCALAI
      CHARACTER*15  LISBI1, LISBI2
      CHARACTER*15  LISBI3
      CHARACTER*16  OPTION
      CHARACTER*19  NOMSD
      CHARACTER*24  LISCMP, LINBCM
      CHARACTER*24  LISINO
      CHARACTER*24  NOEUMA
      CHARACTER*24  RESU, NOLI, NOMOPT, LIEL, NEMA, LISMAI
      CHARACTER*72  FORM1, FORM2, FORM3
      INTEGER       IUNIFI, LXLGUT
      INTEGER       NOLIG(NBVAIM), INDCM(NBVAIM)
      INTEGER       ADMODL,LCMODL,GD
      INTEGER       DIGDEL, NBNO, NBEC, DG(NBECMX), ENTCOD
      COMPLEX*16    ZEROC
      LOGICAL       EXISDG, IMPLIG
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
      CALL JEMARQ ( )
C
C --- INITIALISATIONS :
C     ---------------
      MATEL  = NOMSDZ
      LISMAI = LISMAZ
      LISCMP = LISCMZ
C
      ZERO   = 0.0D0
      ZEROC  = (0.0D0,0.0D0)
      M8BLAN = '        '
      OPTION = '                '
      NBNOE  = 0
      DO 1 I =1, NBECMX
        DG(I) = 0
 1    CONTINUE
C
      CALL JEVEUO(JEXATR('&CATA.TE.MODELOC','LONCUM'),'L',LCMODL)
      CALL JEVEUO(JEXNUM('&CATA.TE.MODELOC',1),'L',ADMODL)
C
C --- RECUPERATION DES NOEUDS POUR-LESQUELS ON VEUT L'IMPRESSION
C --- DE LA MATRICE :
C     -------------
      IF (NBNOE.EQ.0) THEN
C
C ---    ON FABRIQUE UN VECTEUR BIDON DE K8 D'1 SEUL TERME SI LA
C ---    EST VIDE :
C        --------
         LISBI1 = '&&IMELNN.BID1'
         CALL WKVECT(LISBI1,'V V K8',1,IDLINO)
      ENDIF
C
C --- RECUPERATION DES MAILLES POUR-LESQUELLES ON VEUT L'IMPRESSION
C --- DE LA MATRICE :
C     -------------
      IF (NBELEM.EQ.0) THEN
C
C ---    ON FABRIQUE UN VECTEUR BIDON DE K8 D'1 SEUL TERME SI LA
C ---    EST VIDE :
C        --------
         LISBI2 = '&&IMELNN.BID2'
         CALL WKVECT(LISBI2,'V V K8',1,IDLIMA)
      ELSE
         CALL JEVEUO(LISMAI,'L',IDLIMA)
      ENDIF
C
C --- RECUPERATION DES COMPOSANTES POUR-LESQUELLES ON VEUT L'IMPRESSION
C --- DE LA MATRICE :
C     -------------
      IF (NBCMP.EQ.0) THEN
C
C ---    ON FABRIQUE UN VECTEUR BIDON DE K8 D'1 SEUL TERME SI LA
C ---    EST VIDE :
C        --------
         LISBI3 = '&&IMELNN.BID3'
         CALL WKVECT(LISBI3,'V V K8',1,IDLICM)
      ELSE
         CALL JEVEUO(LISCMP,'L',IDLICM)
      ENDIF
C
C --- CREATION DU TABLEAU DU NOMBRE DE COMPOSANTES PAR NOEUD :
C     ------------------------------------------------------
      LINBCM = '&&IMELNN.NBCOMP'
      CALL WKVECT(LINBCM,'V V I',NBNOMX,IDNBCN)
C
C --- CREATION DU TABLEAU INDIQUANT SI LES VALEURS RELATIVES
C --- A UNE CONNECTIVITE DE L'ELEMENT ONT ETE TRAITEES OU NON :
C     -------------------------------------------------------
      LISINO = '&&IMELNN.IDINO'
      CALL WKVECT(LISINO,'V V I',NBNOMX,IDIDNO)
C
C --- RECUPERATION DE LA LISTE DES RESU_ELEM DU MATR_ELEM :
C     ---------------------------------------------------
      CALL JEVEUO(MATEL//'.LISTE_RESU','L',IDLRES)
C
C --- RECUPERATION DU NOMBRE DE RESU_ELEM DU MATR_ELEM :
C     ------------------------------------------------
      CALL JELIRA(MATEL//'.LISTE_RESU','LONUTI',NBRESU,K1BID)
C
      NCMPMM = 0
      TYPVAL = 'R'
      ICMPLX = 0
C================================================================
C --- RECUPERATION DU NOMBRE MAX DE COMPOSANTES ASSOCIEES A UN  =
C --- NOEUD POUR TOUS LES RESU_ELEM                             =
C================================================================
      DO 10 IRESU = 1, NBRESU
C
C ---   NOM DU RESU_ELEM COURANT :
C       ------------------------
         RESU = ZK24(IDLRES+IRESU-1)
C
         CALL JEEXIN(RESU(1:19)//'.DESC',IER)
         IF (IER.EQ.0) GO TO 10
C
C ---   RECUPERATION DU DESCRIPTEUR DU RESU_ELEM :
C       ----------------------------------------
         CALL JEVEUO(RESU(1:19)//'.DESC','L',IDDESC)
C
C ---   RECUPERATION DE LA GRANDEUR ASSOCIEE AU MATR_ELEM :
C       -------------------------------------------------
         GD = ZI(IDDESC+1-1)
C
C ---   TYPE (REEL OU COMPLEXE) ASSOCIE AUX VALEURS DU RESU_ELEM :
C       --------------------------------------------------------
        SCAL = SCALAI(GD)
        TYPVAL = SCAL(1:1)
        IF (TYPVAL.EQ.'C') THEN
          ICMPLX = 1
        ENDIF
C
C ---   NOM DE LA GRANDEUR ASSOCIEE AU MATR_ELEM :
C       ----------------------------------------
         CALL JENUNO(JEXNUM('&CATA.GD.NOMCMP',GD),NOMGD)
C
C ---   GRANDEUR SIMPLE ASSOCIEE AU MATR_ELEM :
C       -------------------------------------
         CALL DISMOI('F','NUM_GD_SI',NOMGD,'GRANDEUR',NUMGD,K8BID,IER)
C
C ---   NOMBRE DE COMPOSANTES ASSOCIEES A LA GRANDEUR :
C       ---------------------------------------------
         CALL JELIRA(JEXNUM('&CATA.GD.NOMCMP',NUMGD),'LONMAX',NCMPMX,
     &               K1BID)
         CALL JEVEUO(JEXNUM('&CATA.GD.NOMCMP',NUMGD),'L',INOCMP)
         NCMPMM = MAX(NCMPMM, NCMPMX)
C
  10  CONTINUE
C
C===================================================================
C --- CREATION DES TABLEAUX DE TRAVAIL NECESSAIRES A L'IMPRESSION  =
C --- ET DEPENDANT DU NOMBRE DE COMPOSANTES PAR NOEUD              =
C===================================================================
C
      NCMPM2 = NCMPMM*NCMPMM
C
C --- CREATION DU TABLEAU DES NOMS DES COMPOSANTES ASSOCIEES
C --- A LA GRANDEUR :
C        -------------
      CALL WKVECT('&&IMELNN.NOMCMP','V V K8',NCMPM2*NBNOMX,IDNOMC)
C
C --- CREATION DU TABLEAU DES VALEURS DE LA LIGNE COURANTE
C --- DU MATR_ELEM OU DU VECT_ELEM A IMPRIMER :
C     ---------------------------------------
      IF (ICMPLX.EQ.0) THEN
        CALL WKVECT('&&IMELNN.VALE','V V R',NCMPM2*NBNOMX,IDVALE)
      ELSEIF (ICMPLX.EQ.1) THEN
        CALL WKVECT('&&IMELNN.VALE','V V C',NCMPM2*NBNOMX,IDVALE)
      ENDIF
C
C --- CREATION DU TABLEAU DES NOMS NOEUDS, A CHAQUE INCONNUE
C --- DE LA MATRICE ELEMENTAIRE, ON ASSOCIE LE NOM DU NOEUD
C --- SUR-LAQUELLE ELLE PORTE :
C     -----------------------
      CALL WKVECT('&&IMELNN.NOMNOE','V V K8',NCMPM2*NBNOMX,IDNONO)
C
C --- CREATION DU TABLEAU D'INDICATEURS DISANT SI UNE COMPOSANTE
C --- EST A PRENDRE EN COMPTE OU NON :
C     -----------------------------
      CALL WKVECT('&&IMELNN.INDIC','V V I',NCMPM2*NBNOMX,IDINCM)
C
C --- CREATION DU TABLEAU D'INDICATEURS DISANT A QUELLE LIGNE
C --- APPARTIENT UNE COMPOSANTE A IMPRIMER :
C       ------------------------------------
      CALL WKVECT('&&IMELNN.INDLICM','V V I',NCMPM2*NBNOMX,IDIDCM)
C
C --- CREATION DU TABLEAU D'INDICATEURS DISANT A QUEL NOEUD
C --- APPARTIENT UNE COMPOSANTE A IMPRIMER :
C       -----------------------------------
      CALL WKVECT('&&IMELNN.INDNUNO','V V I',NCMPM2*NBNOMX,IDNUNO)
C
C --- CREATION DU TABLEAU DES NOMS DES COMPOSANTES DU NOEUD COURANT :
C     -------------------------------------------------------------
      CALL WKVECT('&&IMELNN.NOMCM1','V V K8',NCMPM2,IDNCM1)
C
C --- CREATION DU TABLEAU DES NOMS DES COMPOSANTES DES LIGNES A
C --- IMPRIMER :
C     --------
      CALL WKVECT('&&IMELNN.NOMCM2','V V K8',NCMPM2,IDNCM2)
C
C --- CREATION DU TABLEAU DES VALEURS DES COMPOSANTES DES LIGNES A
C --- IMPRIMER :
C     --------
      IF (ICMPLX.EQ.0) THEN
        CALL WKVECT('&&IMELNN.NOVALE','V V R',NCMPM2,IDNVAL)
      ELSEIF (ICMPLX.EQ.1) THEN
        CALL WKVECT('&&IMELNN.NOVALE','V V C',NCMPM2,IDNVAL)
      ENDIF
C
C --- CREATION DU TABLEAU DU NOMBRE DE COMPOSANTES PAR LIGNE
C --- DU NOEUD COURANT A IMPRIMER :
C     ---------------------------
      CALL WKVECT('&&IMELNN.NBCMPX','V V I',NCMPMM,IDNBCM)
C
C======================================================================
C --- AFFECTATION DES TABLEAUX DE TRAVAIL NECESSAIRES A L'IMPRESSION  =
C======================================================================
      NUMLAG = 0
C
C --- BOUCLE SUR LES RESU_ELEM DU MATR_ELEM :
C     -------------------------------------
      DO 20 IRESU = 1, NBRESU
C
         IMLIGR = 0
C
C ---   NOM DU RESU_ELEM COURANT :
C       ------------------------
         RESU = ZK24(IDLRES+IRESU-1)
C
         CALL JEEXIN(RESU(1:19)//'.DESC',IER)
         IF (IER.EQ.0) GO TO 20
C
C ---   RECUPERATION DU DESCRIPTEUR DU RESU_ELEM :
C       ----------------------------------------
         CALL JEVEUO(RESU(1:19)//'.DESC','L',IDDESC)
C
C ---   DETERMINATION DU TYPE 'SYMETRIQUE' OU 'NON-SYMETRIQUE'
C ---   DU RESU_ELEM :
C       ------------
         TYMAT = 'S'
         CALL DISMOI('F','TYPE_MATRICE',RESU,'RESUELEM',IBID,SYM,IER)
         IF (SYM.EQ.'NON_SYM') TYMAT = 'N'
C
C ---   RECUPERATION DE LA GRANDEUR ASSOCIEE AU MATR_ELEM :
C       -------------------------------------------------
         GD = ZI(IDDESC+1-1)
C
C ---   NOM DE LA GRANDEUR ASSOCIEE AU MATR_ELEM :
C       ----------------------------------------
         CALL JENUNO(JEXNUM('&CATA.GD.NOMCMP',GD),NOMGD)
C
C ---   GRANDEUR SIMPLE ASSOCIEE AU MATR_ELEM :
C       -------------------------------------
         CALL DISMOI('F','NUM_GD_SI',NOMGD,'GRANDEUR',NUMGD,K8BID,IER)
C
C ---   RECUPERATION DU NOMBRE D'ENTIERS CODES ASSOCIE A LA GRANDEUR :
C       ------------------------------------------------------------
         NEC = NBEC(NUMGD)
C
C ---   NOMBRE DE COMPOSANTES ASSOCIEES A LA GRANDEUR :
C       ---------------------------------------------
         CALL JELIRA(JEXNUM('&CATA.GD.NOMCMP',NUMGD),'LONMAX',NCMPMX,
     &               K1BID)
         CALL JEVEUO(JEXNUM('&CATA.GD.NOMCMP',NUMGD),'L',INOCMP)
C
C ---   RECUPERATION DU .NOLI DU RESU_ELEM :
C       ----------------------------------
         CALL JEVEUO(RESU(1:19)//'.NOLI','L',IDNOLI)
C
C ---   NOM DU LIGREL AUQUEL EST ASSOCIE LE RESU_ELEM :
C       ---------------------------------------------
         NOLI = ZK24(IDNOLI)
         LIEL = NOLI(1:19)//'.LIEL'
         NEMA = NOLI(1:19)//'.NEMA'
C
C ---   NOM DE L'OPTION ASSOCIEE AU MATR_ELEM :
C       -------------------------------------
         NOMOPT = ZK24(IDNOLI+1)
C
C ---   LONGUEUR UTILE DES CHAINES DE CARACTERES NOLI ET NOMOPT :
C       -------------------------------------------------------
         LGNOMS = LXLGUT(NOLI)
         LGNOMO = LXLGUT(NOMOPT)
         LGTOT = 26 + LGNOMS + LGNOMO
C
         CALL CODENT(LGNOMS,'G',KLONLS)
         CALL CODENT(LGNOMO,'G',KLONLO)
         CALL CODENT(LGTOT ,'G',KLONLG)
C
C ---   FORMATS D'IMPRESSION DU NOM DU LIGREL ET DE L'OPTION :
C       ----------------------------------------------------
         FORM1 = '('//'//,2X,'//KLONLG//'("_")'//')'
         FORM3 = '(2X,'//KLONLG//'("_")'//',//'//')'
         FORM2 = '(2X,'//'"!",X,"LIGREL : "A'//KLONLS//
     &           ',4X,"OPTION : "A'//KLONLO//',X,"!"'//')'
C
C ---   RECUPERATION DU MAILLAGE ASSOCIE AU LIGREL :
C       ------------------------------------------
         CALL DISMOI('F','NOM_MAILLA',NOLI,'LIGREL',IBID,NOMA,IER)
C
C ---   COLLECTION DES NOMS DES NOEUDS DU MAILLAGE  :
C       ------------------------------------------
         NOEUMA = NOMA//'.NOMNOE'
C
C ---   RECUPERATION DU NOMBRE DE GROUPES D'ELEMENTS DU LIGREL :
C       ------------------------------------------------------
         CALL DISMOI('F','NB_GREL',NOLI,'LIGREL',NBGREL,K8BID,IER)
C
C ---   TYPE (REEL OU COMPLEXE) ASSOCIE AUX VALEURS DU RESU_ELEM :
C       --------------------------------------------------------
        SCAL = SCALAI(GD)
        TYPVAL = SCAL(1:1)
C
C ---   CAS D'UN RESU_ELEM REEL :
C       -----------------------
        IF (TYPVAL.EQ.'R') THEN
C
C ---   BOUCLE SUR LES GROUPES D'ELEMENTS DU LIGREL :
C       ------------------------------------------
         DO 30 IGR = 1, NBGREL
C
C ---      INITIALISATION DE LA LISTE DES NOMS DES COMPOSANTES ASSOCIEES
C ---      AUX NOEUDS DES ELEMENTS :
C          -----------------------
            DO 40 ICMP = 1, NCMPM2*NBNOMX
              ZK8(IDNOMC+ICMP-1) = M8BLAN
  40        CONTINUE
C
C ---    RECUPERATION DU MODE ASSOCIE AU GROUPE D'ELEMENTS COURANT :
C        ---------------------------------------------------------
          MODE = ZI(IDDESC+2+IGR-1)
C
          IF (MODE.GT.0) THEN
C
C ---      RECUPERATION DU NOMBRE DE CONNECTIVITES DES ELEMENTS DU GREL:
C          ------------------------------------------------------------
            NNOE = NBNO(MODE)
C
C ---      CONSTITUTION DE LA LISTE DES NOMS DES COMPOSANTES ASSOCIEES
C ---      AUX NOEUDS DES ELEMENTS :
C          -----------------------
            ICO = 0
            DO 50 INO = 1, NNOE
              NBCMNO = 0
              DO 60 ICMP = 1, NCMPMX
                DO 70 IEC = 1, NEC
                  DG(IEC) = ENTCOD(ADMODL,LCMODL,NEC,MODE,INO,IEC)
  70            CONTINUE
                IF (EXISDG(DG,ICMP)) THEN
                    NBCMNO = NBCMNO + 1
                    ICO  = ICO + 1
                    ZK8(IDNOMC+ICO-1) = ZK8(INOCMP+ICMP-1)
                    ZI(IDIDCM+ICO-1)  = 1
                    NOCMP(ICO)        = ZK8(INOCMP+ICMP-1)
                    NOLIG(ICO)        = 1
                ENDIF
  60          CONTINUE
              ZI(IDNBCN+INO-1) = NBCMNO
  50        CONTINUE
C
C ---      NOMBRE DE DDL ASSOCIE AU MATR_ELEM OU AU VECT_ELEM :
C          --------------------------------------------------
            NBINCO = ICO
C
C ---      ON DUPLIQUE LE TABLEAU DES NOMS DE COMPOSANTES PAR LIGNE
C ---      POUR LES NCMPMM LIGNES MAXIMUM RELATIVES A UN NOEUD ET
C ---      ON AFFECTE A CHAQUE TERME L'INDICE DE LIGNE SUR-LEQUEL
C ---      IL SE TROUVE :
C          ------------
            DO 80 ICMP = 2, NCMPMM
              DO 90 ICO = 1, NBINCO
                 ZK8(IDNOMC+(ICMP-1)*NBINCO+ICO-1) = ZK8(IDNOMC+ICO-1)
                 ZI(IDIDCM+(ICMP-1)*NBINCO+ICO-1)  = ICMP
                 NOCMP((ICMP-1)*NBINCO+ICO)        = ZK8(IDNOMC+ICO-1)
                 NOLIG((ICMP-1)*NBINCO+ICO)        = ICMP
  90          CONTINUE
  80        CONTINUE
C
C ---      NOMBRE DE COMPOSANTES DU VECTEUR OU DE LA
C ---      MATRICE ELEMENTAIRE :
C          -------------------
            NCMPEL = DIGDEL(MODE)
C
C ---      RECUPERATION DU NOMBRE D'ELEMENTS DU GREL :
C          -----------------------------------------
            CALL JELIRA(JEXNUM(LIEL,IGR),'LONMAX',NBEL1,K1BID)
            NBEL = NBEL1 - 1
C
C ---      RECUPERATION DU GREL :
C          --------------------
            CALL JEVEUO(JEXNUM(LIEL,IGR),'L',IDGREL)
C
C ---      RECUPERATION DU VECTEUR DES VECTEURS OU DES MATRICES
C ---      ELEMENTAIRES DU GREL :
C          --------------------
            CALL JEVEUO(JEXNUM(RESU(1:19)//'.RESL',IGR),'L',IDRESL)
C
C ---      BOUCLE SUR LES ELEMENTS DU GREL :
C          -------------------------------
            DO 100 IEL = 1, NBEL
C
C ---        RECUPERATION DU NUMERO DE LA MAILLE :
C            -----------------------------------
             NUMEL = ZI(IDGREL+IEL-1)
C
C ---        CAS D'UNE MAILLE PHYSIQUE (NUMEL > 0) :
C            -------------------------------------
             IF (NUMEL.GT.0) THEN
C
C ---         NOM DE LA MAILLE :
C             ----------------
               CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',NUMEL),NOMAIL)
C
               IF (.NOT.IMPLIG(NOMAIL,NBELEM,ZK8(IDLIMA))) GOTO 100
C
C ---         RECUPERATION DES CONNECTIVITES DE LA MAILLE :
C             -------------------------------------------
               CALL JEVEUO(JEXNUM(NOMA//'.CONNEX',NUMEL),'L',IDNOEU)
C
C ---         BOUCLE SUR LES CONNECTIVITES DE LA MAILLE :
C             -----------------------------------------
               K    = 0
               DO 110 INO = 1, NNOE
C
C ---          NOM DE LA CONNECTIVITE :
C              ---------------------
                  CALL JENUNO(JEXNUM(NOEUMA,ZI(IDNOEU+INO-1)),NOMNOE)
C
C ---          NOMBRE DE COMPOSANTES DU NOEUD INO :
C              ----------------------------------
                   NBCM = ZI(IDNBCN+INO-1)
C
C ---          BOUCLE SUR LES COMPOSANTES DU NOEUD INO :
C              ---------------------------------------
                   DO 120 ICMP = 1, NBCM
                     K = K + 1
C
C ---            AFFECTATION DU TABLEAU DU NOM DES NOEUDS :
C                ----------------------------------------
                     ZK8(IDNONO+K-1) = NOMNOE
                     ZI(IDNUNO+K-1)  = INO
                     NONO(K)         = NOMNOE
                     INDCM(K)        = INO
  120              CONTINUE
  110           CONTINUE
C
C ---        CAS D'UNE MAILLE TARDIVE (NUMEL < 0) :
C            ------------------------------------
             ELSE
C
               NUMLAG = NUMLAG - 1
C
C ---         CODAGE DU NUMERO DE LA MAILLE SOUS FORME D'UNE CHAINE
C ---         DE CARACTERES :
C             -------------
               CALL CODENT(NUMLAG,'G',NOMAIL)
C
               IF (.NOT.IMPLIG(NOMAIL,NBELEM,ZK8(IDLIMA))) GOTO 100
C
               K    = 0
C
C ---         RECUPERATION DU DESCRIPTEUR DE LA MAILLE DANS LE .NEMA :
C             ------------------------------------------------------
               IMA = -NUMEL
               CALL JEVEUO(JEXNUM(NEMA,IMA),'L',IDNEMA)
C
C ---         BOUCLE SUR LES CONNECTIVITES  DE LA MAILLE :
C             ------------------------------------------
               DO 130 INO = 1, NNOE
C
C ---          CAS D'UN NOEUD PHYSIQUE :
C              -----------------------
                 IF (ZI(IDNEMA+INO-1).GT.0) THEN
                    NUMNOE = ZI(IDNEMA+INO-1)
C
C ---            NOM DE LA CONNECTIVITE :
C                ---------------------
                    CALL JENUNO(JEXNUM(NOEUMA,NUMNOE),NOMNOE)
C
C ---          CAS D'UN NOEUD TARDIF :
C              -----------------------
                 ELSEIF (ZI(IDNEMA+INO-1).LT.0) THEN
                    NOELAG = ZI(IDNEMA+INO-1)
C
C ---            CODAGE DU NUMERO DU NOEUD SOUS FORME D'UNE CHAINE
C ---            DE CARACTERES :
C                -------------
                   CALL CODENT(NOELAG,'G',NOMNOE)
C
                 ENDIF
C
C ---          NOMBRE DE COMPOSANTES DU NOEUD INO :
C              ----------------------------------
                   NBCM = ZI(IDNBCN+INO-1)
C
C ---          BOUCLE SUR LES COMPOSANTES DU NOEUD INO :
C              ---------------------------------------
                   DO 140 ICMP = 1, NBCM
                     K = K + 1
C
C ---            AFFECTATION DU TABLEAU DU NOM DES NOEUDS :
C                ----------------------------------------
                     ZK8(IDNONO+K-1) = NOMNOE
                     ZI(IDNUNO+K-1)  = INO
                     NONO(K)         = NOMNOE
                     INDCM(K)        = INO
  140               CONTINUE
  130           CONTINUE
             ENDIF
C
C ---         ON DUPLIQUE LE TABLEAU DES NOMS DES NOEUDS PAR LIGNE
C ---         POUR LES NCMPMM LIGNES MAXIMUM RELATIVES A UN NOEUD :
C             ---------------------------------------------------
             DO 150 ICMP = 2, NCMPMM
               DO 160 ICO = 1, NBINCO
                  ZK8(IDNONO+(ICMP-1)*NBINCO+ICO-1) = ZK8(IDNONO+ICO-1)
                  ZI(IDNUNO+(ICMP-1)*NBINCO+ICO-1)  = ZI(IDNUNO+ICO-1)
                  NONO((ICMP-1)*NBINCO+ICO)       = ZK8(IDNONO+ICO-1)
                  INDCM((ICMP-1)*NBINCO+ICO)      = ZI(IDNUNO+ICO-1)
  160          CONTINUE
  150        CONTINUE
C
C ---        IMPRESSION DU NOM DU LIGREL ET DU NOM DE L'OPTION :
C            -------------------------------------------------
             IMLIGR = IMLIGR + 1
             IF (IMLIGR.EQ.1) THEN
               WRITE(IFM,FORM1)
               WRITE(IFM,FORM2) NOLI(1:LGNOMS), NOMOPT(1:LGNOMO)
               WRITE(IFM,FORM3)
             ENDIF
C
C=================================================================
C ---        IMPRESSION PAR NOEUD :                              =
C ---        RECONSTITUTION DES LIGNES DE LA MATRICE ELEMENTAIRE =
C ---        RELATIVES AU NOEUD COURANT                          =
C=================================================================
C
C ---        CAS D'UNEMATRICE ELEMENTAIRE NON-SYMETRIQUE :
C            -------------------------------------------
            IF (TYMAT.EQ.'N') THEN
             I = 0
C
C ---         BOUCLE SUR LES CONNECTIVITES DE LA MAILLE :
C             -----------------------------------------
             DO 170 INO = 1, NNOE
C
C ---         INITIALISATION A 0 DU TABLEAU DES INDICATEURS DISANT
C ---         SI UNE COMPOSANTE EST A PRENDRE EN COMPTE OU NON
C ---         ET DU TABLEAU DES VALEURS :
C             -------------------------
               K1 = 0
               DO 180 ICMP = 1, NCMPMM
                 DO 190 ICO = 1, NBINCO
                    K1 = K1 + 1
                    ZI(IDINCM+K1-1) = 0
                    ZR(IDVALE+K1-1) = ZERO
  190            CONTINUE
  180          CONTINUE
C
C ---          CAS D'UNE MAILLE PHYSIQUE :
C              ---------------------
                IF (NUMEL.GT.0) THEN
C
C ---            NOM DE LA CONNECTIVITE :
C                ---------------------
                    CALL JENUNO(JEXNUM(NOEUMA,ZI(IDNOEU+INO-1)),NOMNO1)
C
C ---          CAS D'UNE MAILLE TARDIVE :
C              -----------------------
                 ELSEIF (NUMEL.LT.0) THEN
C
C ---          CAS D'UN NOEUD PHYSIQUE :
C              -----------------------
                   IF (ZI(IDNEMA+INO-1).GT.0) THEN
                      NUMNOE = ZI(IDNEMA+INO-1)
C
C ---            NOM DE LA CONNECTIVITE :
C                ---------------------
                      CALL JENUNO(JEXNUM(NOEUMA,NUMNOE),NOMNO1)
C
C ---          CAS D'UN NOEUD TARDIF :
C              -----------------------
                   ELSEIF (ZI(IDNEMA+INO-1).LT.0) THEN
                      NOELAG = ZI(IDNEMA+INO-1)
C
C ---            CODAGE DU NUMERO DU NOEUD SOUS FORME D'UNE CHAINE
C ---            DE CARACTERES :
C                -------------
                     CALL CODENT(NOELAG,'G',NOMNO1)
C
                   ENDIF
                ENDIF
C
                K2 = 0
                DO 200 ICM1 = 1, NCMPMM
                 DO 210 ICO = 1, NBINCO
                  K2 = K2 + 1
                  ZK8(IDNONO+K2-1) = NONO(K2)
                  ZK8(IDNOMC+K2-1) = NOCMP(K2)
                  ZI(IDNUNO +K2-1) = INDCM(K2)
                  ZI(IDIDCM +K2-1) = NOLIG(K2)
  210           CONTINUE
  200          CONTINUE
C
                K = 0
C
C ---          NOMBRE DE COMPOSANTES DU NOEUD INO :
C              ----------------------------------
                NBCM = ZI(IDNBCN+INO-1)
C
C ---          BOUCLE SUR LES COMPOSANTES DU NOEUD INO :
C              ---------------------------------------
                 DO 220 ICMP = 1, NBCM
C
C ---            I DESIGNE L'INDICE DE LA LIGNE COURANTE DE LA
C ---            ELEMENTAIRE :
C                -----------
                     I = I + 1
C
C ---            AFFECTATION DU TABLEAU DES NOMS DES COMPOSANTES
C ---            DU NOEUD 1 :
C                ----------
                     ZK8(IDNCM1+ICMP-1) = ZK8(IDNOMC+I-1)
C
C ---            POINTEUR DU DERNIER TERME TRAITE DE LA MATRICE :
C                ----------------------------------------------
                     NBTERM = NBINCO*(I-1)
C
C ---            BOUCLE SUR LES TERMES DE LA LIGNE COURANTE DE LA
C ---            MATRICE  :
C                -------
                     DO 230 J = 1, NBINCO
                      K = K + 1
                      ZR(IDVALE+K-1) = ZR(IDRESL+NCMPEL*(IEL-1)
     &                                     + NBTERM + J -1)
 230                 CONTINUE
 220             CONTINUE
C
C ---          RECONSTRUCTION DES TABLEAUX DE TRAVAIL  PAR FILTRAGE
C ---          EN FONCTION DES NOEUDS ET DES COMPOSANTES SPECIFIES
C ---          PAR L'UTILISATEUR :
C              -----------------
                   CALL TRNOR2 (NBNOE, ZK8(IDLINO), NBCMP,
     &                          ZK8(IDLICM), OPTION, NBINCO*NBCM,
     &                          ZK8(IDNONO), ZK8(IDNOMC), ZR(IDVALE),
     &                          ZI(IDINCM), ZI(IDNUNO), ZI(IDIDCM),
     &                          LONLI2)
C
C ---         INITIALISATION DU TABLEAU DES INDICES DE NOEUD DEJA
C ---         TRAITES :
C             -------
                   DO 240 INOEU = 1, NBNOMX
                     ZI(IDIDNO+INOEU-1) = 0
 240               CONTINUE
C
C ---          IMPRESSION DES VALEURS NODALES :
C              ------------------------------
                   CALL IMPFR4 (NOMAIL,NOMNO1, LONLI2,
     &                          ZK8(IDNONO), ZK8(IDNOMC),
     &                          ZR(IDVALE), ZI(IDIDCM),
     &                          ZI(IDIDNO),ZI(IDNUNO),ZK8(IDNCM2),
     &                          ZR(IDNVAL),ZI(IDNBCM), NCMPMM, NBCM,
     &                          ZK8(IDNCM1), NBCMP, ZK8(IDLICM),
     &                          NBCHIF, IFM)
C
 170         CONTINUE
C
C ---        CAS D'UNE MATRICE ELEMENTAIRE SYMETRIQUE :
C            ----------------------------------------
            ELSEIF (TYMAT.EQ.'S') THEN
             I = 0
C
C ---         BOUCLE SUR LES CONNECTIVITES DE LA MAILLE :
C             -----------------------------------------
             DO 250 INO = 1, NNOE
C
C ---         INITIALISATION A 0 DU TABLEAU DES INDICATEURS DISANT
C ---         SI UNE COMPOSANTE EST A PRENDRE EN COMPTE OU NON
C ---         ET DU TABLEAU DES VALEURS :
C             -------------------------
             K1 = 0
             DO 260 ICMP = 1, NCMPMM
               DO 270 ICO = 1, NBINCO
                  K1 = K1 + 1
                  ZI(IDINCM+K1-1) = 0
                  ZR(IDVALE+K1-1) = ZERO
  270          CONTINUE
  260        CONTINUE
C
C ---        CAS D'UNE MAILLE PHYSIQUE :
C            ---------------------
              IF (NUMEL.GT.0) THEN
C
C ---            NOM DE LA CONNECTIVITE :
C                ---------------------
                    CALL JENUNO(JEXNUM(NOEUMA,ZI(IDNOEU+INO-1)),NOMNO1)
C
C ---         CAS D'UNE MAILLE TARDIVE :
C             ------------------------
               ELSEIF (NUMEL.LT.0) THEN
C
C ---           CAS D'UN NOEUD PHYSIQUE :
C               -----------------------
                   IF (ZI(IDNEMA+INO-1).GT.0) THEN
                      NUMNOE = ZI(IDNEMA+INO-1)
C
C ---            NOM DE LA CONNECTIVITE :
C                ---------------------
                      CALL JENUNO(JEXNUM(NOEUMA,NUMNOE),NOMNO1)
C
C ---           CAS D'UN NOEUD TARDIF :
C               -----------------------
                   ELSEIF (ZI(IDNEMA+INO-1).LT.0) THEN
                      NOELAG = ZI(IDNEMA+INO-1)
C
C ---             CODAGE DU NUMERO DU NOEUD SOUS FORME D'UNE CHAINE
C ---             DE CARACTERES :
C                 -------------
                     CALL CODENT(NOELAG,'G',NOMNO1)
C
                   ENDIF
                  ENDIF
C
             K2 = 0
             DO 280 ICM1 = 1, NCMPMM
               DO 290 ICO = 1, NBINCO
                  K2 = K2 + 1
                  ZK8(IDNONO+K2-1) = NONO(K2)
                  ZK8(IDNOMC+K2-1) = NOCMP(K2)
                  ZI(IDNUNO +K2-1) = INDCM(K2)
                  ZI(IDIDCM +K2-1) = NOLIG(K2)
  290          CONTINUE
  280        CONTINUE
C
              K = 0
C
C ---          NOMBRE DE COMPOSANTES DU NOEUD INO :
C              ----------------------------------
               NBCM = ZI(IDNBCN+INO-1)
C
C ---          BOUCLE SUR LES COMPOSANTES DU NOEUD INO :
C              ---------------------------------------
                DO 300 ICMP = 1, NBCM
C
C ---            I DESIGNE L'INDICE DE LA LIGNE COURANTE DE LA
C ---            ELEMENTAIRE :
C                -----------
                     I = I + 1
C
C ---            AFFECTATION DU TABLEAU DES NOMS DES COMPOSANTES
C ---            DU NOEUD 1 :
C                ----------
                     ZK8(IDNCM1+ICMP-1) = ZK8(IDNOMC+I-1)
C
C ---            POINTEUR DU DERNIER TERME TRAITE DE LA DEMI-MATRICE :
C                ---------------------------------------------------
                     NBTERM = (I*(I-1))/2
C
C ---            BOUCLE SUR LES TERMES DE LA LIGNE COURANTE DE LA
C ---            DEMI-MATRICE INFERIEURE :
C                -----------------------
C
                     DO 310 J = 1, I
                      K = K + 1
                      ZR(IDVALE+K-1) = ZR(IDRESL+NCMPEL*(IEL-1)
     &                                     + NBTERM + J -1)
 310                 CONTINUE
C
C ---            BOUCLE SUR LES TERMES DE LA COLONNE I SITUES
C ---            EN DESSOUS DU TERME DIAGONAL :
C                ----------------------------
                     DO 320 J = I+1, NBINCO
C
C ---              POINTEUR DU TERME DIAGONAL PRECEDANT LA LIGNE
C ---              COURANTE DANS LA DEMI-MATRICE :
C                  -----------------------------
                      NBTERM = (J*(J-1))/2
C
                      K = K + 1
                      ZR(IDVALE+K-1) = ZR(IDRESL+NCMPEL*(IEL-1)
     &                                     + NBTERM + I -1)
 320                 CONTINUE
 300             CONTINUE
C
C ---          RECONSTRUCTION DES TABLEAUX DE TRAVAIL  PAR FILTRAGE
C ---          EN FONCTION DES NOEUDS ET DES COMPOSANTES SPECIFIES
C ---          PAR L'UTILISATEUR :
C              -----------------
                  CALL TRNOR2 (NBNOE, ZK8(IDLINO), NBCMP,
     &                          ZK8(IDLICM), OPTION, NBINCO*NBCM,
     &                          ZK8(IDNONO), ZK8(IDNOMC), ZR(IDVALE),
     &                          ZI(IDINCM), ZI(IDNUNO), ZI(IDIDCM),
     &                          LONLI2)
C
C ---         INITIALISATION DU TABLEAU DES INDICES DE NOEUD DEJA
C ---         TRAITES :
C             -------
                   DO 330 INOEU = 1, NBNOMX
                     ZI(IDIDNO+INOEU-1) = 0
 330               CONTINUE
C
C ---          IMPRESSION DES VALEURS NODALES :
C              ------------------------------
                   CALL IMPFR4 (NOMAIL,NOMNO1, LONLI2,
     &                          ZK8(IDNONO), ZK8(IDNOMC),
     &                          ZR(IDVALE), ZI(IDIDCM),
     &                          ZI(IDIDNO),ZI(IDNUNO),ZK8(IDNCM2),
     &                          ZR(IDNVAL),ZI(IDNBCM), NCMPMM, NBCM,
     &                          ZK8(IDNCM1), NBCMP, ZK8(IDLICM),
     &                          NBCHIF, IFM)
C
 250         CONTINUE
            ENDIF
 100       CONTINUE
          ENDIF
 30      CONTINUE
C
C ---   CAS D'UN RESU_ELEM COMPLEXE :
C       ---------------------------
        ELSEIF (TYPVAL.EQ.'C') THEN
C
C ---   BOUCLE SUR LES GROUPES D'ELEMENTS DU LIGREL :
C       ------------------------------------------
         DO 340 IGR = 1, NBGREL
C
C ---      INITIALISATION DE LA LISTE DES NOMS DES COMPOSANTES ASSOCIEES
C ---      AUX NOEUDS DES ELEMENTS :
C          -----------------------
            DO 350 ICMP = 1, NCMPM2*NBNOMX
              ZK8(IDNOMC+ICMP-1) = M8BLAN
 350        CONTINUE
C
C ---    RECUPERATION DU MODE ASSOCIE AU GROUPE D'ELEMENTS COURANT :
C        ---------------------------------------------------------
          MODE = ZI(IDDESC+2+IGR-1)
C
          IF (MODE.GT.0) THEN
C
C ---      RECUPERATION DU NOMBRE DE CONNECTIVITES DES ELEMENTS DU GREL:
C          ------------------------------------------------------------
            NNOE = NBNO(MODE)
C
C ---      CONSTITUTION DE LA LISTE DES NOMS DES COMPOSANTES ASSOCIEES
C ---      AUX NOEUDS DES ELEMENTS :
C          -----------------------
            ICO = 0
            DO 360 INO = 1, NNOE
              NBCMNO = 0
              DO 370 ICMP = 1, NCMPMX
                DO 380 IEC = 1, NEC
                  DG(IEC) = ENTCOD(ADMODL,LCMODL,NEC,MODE,INO,IEC)
 380            CONTINUE
                IF (EXISDG(DG,ICMP)) THEN
                    NBCMNO = NBCMNO + 1
                    ICO  = ICO + 1
                    ZK8(IDNOMC+ICO-1) = ZK8(INOCMP+ICMP-1)
                    ZI(IDIDCM+ICO-1)  = 1
                    NOCMP(ICO)        = ZK8(INOCMP+ICMP-1)
                    NOLIG(ICO)        = 1
                ENDIF
 370          CONTINUE
              ZI(IDNBCN+INO-1) = NBCMNO
 360        CONTINUE
C
C ---      NOMBRE DE DDL ASSOCIE AU MATR_ELEM OU AU VECT_ELEM :
C          --------------------------------------------------
            NBINCO = ICO
C
C ---      ON DUPLIQUE LE TABLEAU DES NOMS DE COMPOSANTES PAR LIGNE
C ---      POUR LES NCMPMM LIGNES MAXIMUM RELATIVES A UN NOEUD ET
C ---      ON AFFECTE A CHAQUE TERME L'INDICE DE LIGNE SUR-LEQUEL
C ---      IL SE TROUVE :
C          ------------
            DO 390 ICMP = 2, NCMPMM
              DO 400 ICO = 1, NBINCO
                 ZK8(IDNOMC+(ICMP-1)*NBINCO+ICO-1) = ZK8(IDNOMC+ICO-1)
                 ZI(IDIDCM+(ICMP-1)*NBINCO+ICO-1)  = ICMP
                 NOCMP((ICMP-1)*NBINCO+ICO)        = ZK8(IDNOMC+ICO-1)
                 NOLIG((ICMP-1)*NBINCO+ICO)        = ICMP
 400          CONTINUE
 390        CONTINUE
C
C ---      NOMBRE DE COMPOSANTES DU VECTEUR OU DE LA
C ---      MATRICE ELEMENTAIRE :
C          -------------------
            NCMPEL = DIGDEL(MODE)
C
C ---      RECUPERATION DU NOMBRE D'ELEMENTS DU GREL :
C          -----------------------------------------
            CALL JELIRA(JEXNUM(LIEL,IGR),'LONMAX',NBEL1,K1BID)
            NBEL = NBEL1 - 1
C
C ---      RECUPERATION DU GREL :
C          --------------------
            CALL JEVEUO(JEXNUM(LIEL,IGR),'L',IDGREL)
C
C ---      RECUPERATION DU VECTEUR DES VECTEURS OU DES MATRICES
C ---      ELEMENTAIRES DU GREL :
C          --------------------
            CALL JEVEUO(JEXNUM(RESU(1:19)//'.RESL',IGR),'L',IDRESL)
C
C ---      BOUCLE SUR LES ELEMENTS DU GREL :
C          -------------------------------
            DO 410 IEL = 1, NBEL
C
C ---        RECUPERATION DU NUMERO DE LA MAILLE :
C            -----------------------------------
             NUMEL = ZI(IDGREL+IEL-1)
C
C ---        CAS D'UNE MAILLE PHYSIQUE (NUMEL > 0) :
C            -------------------------------------
             IF (NUMEL.GT.0) THEN
C
C ---         NOM DE LA MAILLE :
C             ----------------
               CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',NUMEL),NOMAIL)
C
               IF (.NOT.IMPLIG(NOMAIL,NBELEM,ZK8(IDLIMA))) GOTO 410
C
C ---         RECUPERATION DES CONNECTIVITES DE LA MAILLE :
C             -------------------------------------------
               CALL JEVEUO(JEXNUM(NOMA//'.CONNEX',NUMEL),'L',IDNOEU)
C
C ---         BOUCLE SUR LES CONNECTIVITES DE LA MAILLE :
C             -----------------------------------------
               K    = 0
               DO 420 INO = 1, NNOE
C
C ---          NOM DE LA CONNECTIVITE :
C              ---------------------
                  CALL JENUNO(JEXNUM(NOEUMA,ZI(IDNOEU+INO-1)),NOMNOE)
C
C ---          NOMBRE DE COMPOSANTES DU NOEUD INO :
C              ----------------------------------
                   NBCM = ZI(IDNBCN+INO-1)
C
C ---          BOUCLE SUR LES COMPOSANTES DU NOEUD INO :
C              ---------------------------------------
                   DO 430 ICMP = 1, NBCM
                     K = K + 1
C
C ---            AFFECTATION DU TABLEAU DU NOM DES NOEUDS :
C                ----------------------------------------
                     ZK8(IDNONO+K-1) = NOMNOE
                     ZI(IDNUNO+K-1)  = INO
                     NONO(K)         = NOMNOE
                     INDCM(K)        = INO
  430              CONTINUE
  420           CONTINUE
C
C ---        CAS D'UNE MAILLE TARDIVE (NUMEL < 0) :
C            ------------------------------------
             ELSE
C
               NUMLAG = NUMLAG - 1
C
C ---         CODAGE DU NUMERO DE LA MAILLE SOUS FORME D'UNE CHAINE
C ---         DE CARACTERES :
C             -------------
               CALL CODENT(NUMLAG,'G',NOMAIL)
C
               IF (.NOT.IMPLIG(NOMAIL,NBELEM,ZK8(IDLIMA))) GOTO 410
C
               K    = 0
C
C ---         RECUPERATION DU DESCRIPTEUR DE LA MAILLE DANS LE .NEMA :
C             ------------------------------------------------------
               IMA = -NUMEL
               CALL JEVEUO(JEXNUM(NEMA,IMA),'L',IDNEMA)
C
C ---         BOUCLE SUR LES CONNECTIVITES  DE LA MAILLE :
C             ------------------------------------------
               DO 440 INO = 1, NNOE
C
C ---          CAS D'UN NOEUD PHYSIQUE :
C              -----------------------
                 IF (ZI(IDNEMA+INO-1).GT.0) THEN
                    NUMNOE = ZI(IDNEMA+INO-1)
C
C ---            NOM DE LA CONNECTIVITE :
C                ---------------------
                    CALL JENUNO(JEXNUM(NOEUMA,NUMNOE),NOMNOE)
C
C ---          CAS D'UN NOEUD TARDIF :
C              -----------------------
                 ELSEIF (ZI(IDNEMA+INO-1).LT.0) THEN
                    NOELAG = ZI(IDNEMA+INO-1)
C
C ---            CODAGE DU NUMERO DU NOEUD SOUS FORME D'UNE CHAINE
C ---            DE CARACTERES :
C                -------------
                   CALL CODENT(NOELAG,'G',NOMNOE)
C
                 ENDIF
C
C ---          NOMBRE DE COMPOSANTES DU NOEUD INO :
C              ----------------------------------
                   NBCM = ZI(IDNBCN+INO-1)
C
C ---          BOUCLE SUR LES COMPOSANTES DU NOEUD INO :
C              ---------------------------------------
                   DO 450 ICMP = 1, NBCM
                     K = K + 1
C
C ---            AFFECTATION DU TABLEAU DU NOM DES NOEUDS :
C                ----------------------------------------
                     ZK8(IDNONO+K-1) = NOMNOE
                     ZI(IDNUNO+K-1)  = INO
                     NONO(K)         = NOMNOE
                     INDCM(K)        = INO
  450               CONTINUE
  440           CONTINUE
             ENDIF
C
C ---         ON DUPLIQUE LE TABLEAU DES NOMS DES NOEUDS PAR LIGNE
C ---         POUR LES NCMPMM LIGNES MAXIMUM RELATIVES A UN NOEUD :
C             ---------------------------------------------------
             DO 460 ICMP = 2, NCMPMM
               DO 470 ICO = 1, NBINCO
                  ZK8(IDNONO+(ICMP-1)*NBINCO+ICO-1)=ZK8(IDNONO+ICO-1)
                  ZI(IDNUNO+(ICMP-1)*NBINCO+ICO-1)= ZI(IDNUNO+ICO-1)
                  NONO((ICMP-1)*NBINCO+ICO)       = ZK8(IDNONO+ICO-1)
                  INDCM((ICMP-1)*NBINCO+ICO)      = ZI(IDNUNO+ICO-1)
  470          CONTINUE
  460        CONTINUE
C
C ---        IMPRESSION DU NOM DU LIGREL ET DU NOM DE L'OPTION :
C            -------------------------------------------------
             IMLIGR = IMLIGR + 1
             IF (IMLIGR.EQ.1) THEN
               WRITE(IFM,FORM1)
               WRITE(IFM,FORM2) NOLI(1:LGNOMS), NOMOPT(1:LGNOMO)
               WRITE(IFM,FORM3)
             ENDIF
C
C=================================================================
C ---        IMPRESSION PAR NOEUD :                              =
C ---        RECONSTITUTION DES LIGNES DE LA MATRICE ELEMENTAIRE =
C ---        RELATIVES AU NOEUD COURANT                          =
C=================================================================
C
C ---        CAS D'UNE MATRICE ELEMENTAIRE NON-SYMETRIQUE :
C            -------------------------------------------
            IF (TYMAT.EQ.'N') THEN
             I = 0
C
C ---         BOUCLE SUR LES CONNECTIVITES DE LA MAILLE :
C             -----------------------------------------
             DO 480 INO = 1, NNOE
C
C ---         INITIALISATION A 0 DU TABLEAU DES INDICATEURS DISANT
C ---         SI UNE COMPOSANTE EST A PRENDRE EN COMPTE OU NON
C ---         ET DU TABLEAU DES VALEURS :
C             -------------------------
               K1 = 0
               DO 490 ICMP = 1, NCMPMM
                 DO 500 ICO = 1, NBINCO
                    K1 = K1 + 1
                    ZI(IDINCM+K1-1) = 0
                    ZC(IDVALE+K1-1) = ZEROC
  500            CONTINUE
  490          CONTINUE
C
C ---          CAS D'UNE MAILLE PHYSIQUE :
C              ---------------------
                IF (NUMEL.GT.0) THEN
C
C ---            NOM DE LA CONNECTIVITE :
C                ---------------------
                    CALL JENUNO(JEXNUM(NOEUMA,ZI(IDNOEU+INO-1)),NOMNO1)
C
C ---          CAS D'UNE MAILLE TARDIVE :
C              -----------------------
                 ELSEIF (NUMEL.LT.0) THEN
C
C ---          CAS D'UN NOEUD PHYSIQUE :
C              -----------------------
                   IF (ZI(IDNEMA+INO-1).GT.0) THEN
                      NUMNOE = ZI(IDNEMA+INO-1)
C
C ---            NOM DE LA CONNECTIVITE :
C                ---------------------
                      CALL JENUNO(JEXNUM(NOEUMA,NUMNOE),NOMNO1)
C
C ---          CAS D'UN NOEUD TARDIF :
C              -----------------------
                   ELSEIF (ZI(IDNEMA+INO-1).LT.0) THEN
                      NOELAG = ZI(IDNEMA+INO-1)
C
C ---            CODAGE DU NUMERO DU NOEUD SOUS FORME D'UNE CHAINE
C ---            DE CARACTERES :
C                -------------
                     CALL CODENT(NOELAG,'G',NOMNO1)
C
                   ENDIF
                ENDIF
C
                K2 = 0
                DO 510 ICM1 = 1, NCMPMM
                 DO 520 ICO = 1, NBINCO
                  K2 = K2 + 1
                  ZK8(IDNONO+K2-1) = NONO(K2)
                  ZK8(IDNOMC+K2-1) = NOCMP(K2)
                  ZI(IDNUNO +K2-1) = INDCM(K2)
                  ZI(IDIDCM +K2-1) = NOLIG(K2)
  520           CONTINUE
  510          CONTINUE
C
                K = 0
C
C ---          NOMBRE DE COMPOSANTES DU NOEUD INO :
C              ----------------------------------
                NBCM = ZI(IDNBCN+INO-1)
C
C ---          BOUCLE SUR LES COMPOSANTES DU NOEUD INO :
C              ---------------------------------------
                 DO 530 ICMP = 1, NBCM
C
C ---            I DESIGNE L'INDICE DE LA LIGNE COURANTE DE LA
C ---            ELEMENTAIRE :
C                -----------
                     I = I + 1
C
C ---            AFFECTATION DU TABLEAU DES NOMS DES COMPOSANTES
C ---            DU NOEUD 1 :
C                ----------
                     ZK8(IDNCM1+ICMP-1) = ZK8(IDNOMC+I-1)
C
C ---            POINTEUR DU DERNIER TERME TRAITE DE LA MATRICE :
C                ----------------------------------------------
                     NBTERM = NBINCO*(I-1)
C
C ---            BOUCLE SUR LES TERMES DE LA LIGNE COURANTE DE LA
C ---            MATRICE  :
C                -------
                     DO 540 J = 1, NBINCO
                      K = K + 1
                      ZC(IDVALE+K-1) = ZC(IDRESL+NCMPEL*(IEL-1)
     &                                     + NBTERM + J -1)
 540                 CONTINUE
 530             CONTINUE
C
C ---          RECONSTRUCTION DES TABLEAUX DE TRAVAIL  PAR FILTRAGE
C ---          EN FONCTION DES NOEUDS ET DES COMPOSANTES SPECIFIES
C ---          PAR L'UTILISATEUR :
C              -----------------
                   CALL TRNOC2 (NBNOE, ZK8(IDLINO), NBCMP,
     &                          ZK8(IDLICM), OPTION, NBINCO*NBCM,
     &                          ZK8(IDNONO), ZK8(IDNOMC), ZC(IDVALE),
     &                          ZI(IDINCM), ZI(IDNUNO), ZI(IDIDCM),
     &                          LONLI2)
C
C ---         INITIALISATION DU TABLEAU DES INDICES DE NOEUD DEJA
C ---         TRAITES :
C             -------
                   DO 550 INOEU = 1, NBNOMX
                     ZI(IDIDNO+INOEU-1) = 0
 550               CONTINUE
C
C ---          IMPRESSION DES VALEURS NODALES :
C              ------------------------------
                   CALL IMPFC4 (NOMAIL,NOMNO1, LONLI2,
     &                          ZK8(IDNONO), ZK8(IDNOMC),
     &                          ZC(IDVALE), ZI(IDIDCM),
     &                          ZI(IDIDNO),ZI(IDNUNO),ZK8(IDNCM2),
     &                          ZC(IDNVAL),ZI(IDNBCM), NCMPMM, NBCM,
     &                          ZK8(IDNCM1), NBCMP, ZK8(IDLICM),
     &                          NBCHIF, IFM)
C
 480         CONTINUE
C
C ---        CAS D'UNE MATRICE ELEMENTAIRE SYMETRIQUE :
C            ----------------------------------------
            ELSEIF (TYMAT.EQ.'S') THEN
             I = 0
C
C ---         BOUCLE SUR LES CONNECTIVITES DE LA MAILLE :
C             -----------------------------------------
             DO 560 INO = 1, NNOE
C
C ---         INITIALISATION A 0 DU TABLEAU DES INDICATEURS DISANT
C ---         SI UNE COMPOSANTE EST A PRENDRE EN COMPTE OU NON
C ---         ET DU TABLEAU DES VALEURS :
C             -------------------------
             K1 = 0
             DO 570 ICMP = 1, NCMPMM
               DO 580 ICO = 1, NBINCO
                  K1 = K1 + 1
                  ZI(IDINCM+K1-1) = 0
                  ZC(IDVALE+K1-1) = ZEROC
  580          CONTINUE
  570        CONTINUE
C
C ---        CAS D'UNE MAILLE PHYSIQUE :
C            ---------------------
              IF (NUMEL.GT.0) THEN
C
C ---            NOM DE LA CONNECTIVITE :
C                ---------------------
                    CALL JENUNO(JEXNUM(NOEUMA,ZI(IDNOEU+INO-1)),NOMNO1)
C
C ---         CAS D'UNE MAILLE TARDIVE :
C             ------------------------
               ELSEIF (NUMEL.LT.0) THEN
C
C ---           CAS D'UN NOEUD PHYSIQUE :
C               -----------------------
                   IF (ZI(IDNEMA+INO-1).GT.0) THEN
                      NUMNOE = ZI(IDNEMA+INO-1)
C
C ---            NOM DE LA CONNECTIVITE :
C                ---------------------
                      CALL JENUNO(JEXNUM(NOEUMA,NUMNOE),NOMNO1)
C
C ---           CAS D'UN NOEUD TARDIF :
C               -----------------------
                   ELSEIF (ZI(IDNEMA+INO-1).LT.0) THEN
                      NOELAG = ZI(IDNEMA+INO-1)
C
C ---             CODAGE DU NUMERO DU NOEUD SOUS FORME D'UNE CHAINE
C ---             DE CARACTERES :
C                 -------------
                     CALL CODENT(NOELAG,'G',NOMNO1)
C
                   ENDIF
                  ENDIF
C
             K2 = 0
             DO 590 ICM1 = 1, NCMPMM
               DO 600 ICO = 1, NBINCO
                  K2 = K2 + 1
                  ZK8(IDNONO+K2-1) = NONO(K2)
                  ZK8(IDNOMC+K2-1) = NOCMP(K2)
                  ZI(IDNUNO +K2-1) = INDCM(K2)
                  ZI(IDIDCM +K2-1) = NOLIG(K2)
  600          CONTINUE
  590        CONTINUE
C
              K = 0
C
C ---          NOMBRE DE COMPOSANTES DU NOEUD INO :
C              ----------------------------------
               NBCM = ZI(IDNBCN+INO-1)
C
C ---          BOUCLE SUR LES COMPOSANTES DU NOEUD INO :
C              ---------------------------------------
                DO 610 ICMP = 1, NBCM
C
C ---            I DESIGNE L'INDICE DE LA LIGNE COURANTE DE LA
C ---            ELEMENTAIRE :
C                -----------
                     I = I + 1
C
C ---            AFFECTATION DU TABLEAU DES NOMS DES COMPOSANTES
C ---            DU NOEUD 1 :
C                ----------
                     ZK8(IDNCM1+ICMP-1) = ZK8(IDNOMC+I-1)
C
C ---            POINTEUR DU DERNIER TERME TRAITE DE LA DEMI-MATRICE :
C                ---------------------------------------------------
                     NBTERM = (I*(I-1))/2
C
C ---            BOUCLE SUR LES TERMES DE LA LIGNE COURANTE DE LA
C ---            DEMI-MATRICE INFERIEURE :
C                -----------------------
C
                     DO 620 J = 1, I
                      K = K + 1
                      ZC(IDVALE+K-1) = ZC(IDRESL+NCMPEL*(IEL-1)
     &                                     + NBTERM + J -1)
 620                 CONTINUE
C
C ---            BOUCLE SUR LES TERMES DE LA COLONNE I SITUES
C ---            EN DESSOUS DU TERME DIAGONAL :
C                ----------------------------
                     DO 630 J = I+1, NBINCO
C
C ---              POINTEUR DU TERME DIAGONAL PRECEDANT LA LIGNE
C ---              COURANTE DANS LA DEMI-MATRICE :
C                  -----------------------------
                      NBTERM = (J*(J-1))/2
C
                      K = K + 1
                      ZC(IDVALE+K-1) = ZC(IDRESL+NCMPEL*(IEL-1)
     &                                     + NBTERM + I -1)
 630                 CONTINUE
 610             CONTINUE
C
C ---          RECONSTRUCTION DES TABLEAUX DE TRAVAIL  PAR FILTRAGE
C ---          EN FONCTION DES NOEUDS ET DES COMPOSANTES SPECIFIES
C ---          PAR L'UTILISATEUR :
C              -----------------
                  CALL TRNOC2 (NBNOE, ZK8(IDLINO), NBCMP,
     &                          ZK8(IDLICM), OPTION, NBINCO*NBCM,
     &                          ZK8(IDNONO), ZK8(IDNOMC), ZC(IDVALE),
     &                          ZI(IDINCM), ZI(IDNUNO), ZI(IDIDCM),
     &                          LONLI2)
C
C ---         INITIALISATION DU TABLEAU DES INDICES DE NOEUD DEJA
C ---         TRAITES :
C             -------
                   DO 640 INOEU = 1, NBNOMX
                     ZI(IDIDNO+INOEU-1) = 0
 640               CONTINUE
C
C ---          IMPRESSION DES VALEURS NODALES :
C              ------------------------------
                   CALL IMPFC4 (NOMAIL,NOMNO1, LONLI2,
     &                          ZK8(IDNONO), ZK8(IDNOMC),
     &                          ZC(IDVALE), ZI(IDIDCM),
     &                          ZI(IDIDNO),ZI(IDNUNO),ZK8(IDNCM2),
     &                          ZC(IDNVAL),ZI(IDNBCM), NCMPMM, NBCM,
     &                          ZK8(IDNCM1), NBCMP, ZK8(IDLICM),
     &                          NBCHIF, IFM)
C
 560         CONTINUE
            ENDIF
 410       CONTINUE
          ENDIF
 340     CONTINUE
        ELSE
          CALL U2MESK('F','PREPOST_63',1,TYPVAL)
        ENDIF
 20   CONTINUE
C
      CALL JEDETC('V','&&IMELNN',1)
C
      CALL JEDEMA()
C
      END

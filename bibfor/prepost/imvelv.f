      SUBROUTINE IMVELV ( IFM, NOMSDZ, NBCMP, LISCMZ,
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
C      IMVELV -- IMPRESSION DU VECT_ELEM  DE NOM NOMSD
C                DANS LE FICHIER DE NOM FICHIER.
C                LE GRAIN DE L'IMPRESSION EST 'VALEUR'
C                (I.E. ON IMPRIME UNE VALEUR PAR LIGNE)
C
C
C   ARGUMENT        E/S  TYPE         ROLE
C    IFM             IN    I     UNITE LOGIQUE D'IMPRESSION
C                                 LE VECT_ELEM,
C    NOMSDZ          IN    K*     NOM DU VECT_ELEM
C    NBCMP           IN    I     NOMBRE DE COMPOSANTES DE LA LISTE
C                                LISCMZ DES COMPOSANTES
C    LISCMZ          IN    K*    LISTE DES COMPOSANTES POUR-LESQUELLES
C                                ON DESIRE L'IMPRESSION DES VALEURS
C                                DU VECT_ELEM
C    NBELEM          IN    I     NOMBRE DE TERMES DE LA LISTE LISMAZ
C    LISMAZ          IN    K*    LISTE DES MAILLES POUR-LESQUELLES
C                                ON DESIRE L'IMPRESSION DES VALEURS
C                                DU VECT_ELEM
C    NBCHIF          IN    I     NOMBRE DE CHIFFRES SIGNIFICATIFS
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
      PARAMETER     (NBECMX = 10)
      CHARACTER*1   SLACH
      CHARACTER*1   K1BID, TYPVAL
      CHARACTER*2   KLONLG , KLONLS, KLONLO
      CHARACTER*2   KNBCH, KLVALR
      CHARACTER*8   K8BID, NOMA, NOMGD, NOMNO1, NOMCM1
      CHARACTER*8   M8BLAN, MATEL, NOMNOE, NOMAIL
      CHARACTER*8    FORVAR
      CHARACTER*8   SCAL, SCALAI
      CHARACTER*15  LISBI1, LISBI2
      CHARACTER*15  LISBI3
      CHARACTER*20  FORM4
      CHARACTER*24  LISCMP, LINOCM, LINBCM, LINONO
      CHARACTER*24  LIVALE, LINDCM, LISMAI
      CHARACTER*24  NOEUMA
      CHARACTER*24  RESU, NOLI, NOMOPT, LIEL, NEMA
      CHARACTER*72  FORM1, FORM2, FORM3, FORMAV
      CHARACTER*80  FORVAC
      CHARACTER*120 FORMAC
      INTEGER       IUNIFI, LXLGUT
      INTEGER       ADMODL,LCMODL,GD
      INTEGER       DIGDEL, NBNO, NBEC, DG(NBECMX), ENTCOD
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
      M8BLAN = '        '
      SLACH  = '/'
      NBNOE  = 0
      DO 1 I =1, NBECMX
        DG(I) = 0
 1    CONTINUE
C
      CALL JEVEUO(JEXATR('&CATA.TE.MODELOC','LONCUM'),'L',LCMODL)
      CALL JEVEUO(JEXNUM('&CATA.TE.MODELOC',1),'L',ADMODL)
C
C --- RECUPERATION DES NOEUDS POUR-LESQUELS ON VEUT L'IMPRESSION
C --- DE LA VECTEUR :
C     -------------
      IF (NBNOE.EQ.0) THEN
C
C ---    ON FABRIQUE UN VECTEUR BIDON DE K8 D'1 SEUL TERME SI LA
C ---    EST VIDE :
C        --------
         LISBI1 = '&&IMVELV.BID1'
         CALL WKVECT(LISBI1,'V V K8',1,IDLINO)
      ENDIF
C
C --- RECUPERATION DES MAILLES POUR-LESQUELLES ON VEUT L'IMPRESSION
C --- DE LA VECTEUR :
C     -------------
      IF (NBELEM.EQ.0) THEN
C
C ---    ON FABRIQUE UN VECTEUR BIDON DE K8 D'1 SEUL TERME SI LA
C ---    EST VIDE :
C        --------
         LISBI2 = '&&IMVELV.BID2'
         CALL WKVECT(LISBI2,'V V K8',1,IDLIMA)
      ELSE
         CALL JEVEUO(LISMAI,'L',IDLIMA)
      ENDIF
C
C --- RECUPERATION DES COMPOSANTES POUR-LESQUELLES ON VEUT L'IMPRESSION
C --- DE LA VECTEUR :
C     -------------
      IF (NBCMP.EQ.0) THEN
C
C ---    ON FABRIQUE UN VECTEUR BIDON DE K8 D'1 SEUL TERME SI LA
C ---    EST VIDE :
C        --------
         LISBI3 = '&&IMVELV.BID3'
         CALL WKVECT(LISBI3,'V V K8',1,IDLICM)
      ELSE
         CALL JEVEUO(LISCMP,'L',IDLICM)
      ENDIF
C
C --- NOMBRE DE CHIFFRES A METTRE APRES LA VIRGULE
C --- ON RAPPELLE QUE LA FONCTION NDRSEM() RENVOIE LE NOMBRE
C --- DE CHIFFRES EN DECIMAL D'UN REEL :
C     --------------------------------
C      NBCH = MIN (NDRSEM( ), NBCHIF)
      NBCH = MIN (14, NBCHIF)
      NBCH1 = NBCH - 1
C
C --- LONGUEUR PRISE POUR REPRESENTER UNE VALEUR REELLE :
C     -------------------------------------------------
      LVALRE = NBCH + 6
C
C --- FORMAT D'ECRITURE DES VALEURS REELLES :
C     -------------------------------------
      CALL CODENT(NBCH1,'G',KNBCH)
      CALL CODENT(LVALRE,'D',KLVALR)
C
      FORVAR = '1PD'//KLVALR//'.'//KNBCH
      FORVAC = '"("'//'1PD'//KLVALR//'.'//KNBCH//'","'//
     &                '1PD'//KLVALR//'.'//KNBCH//'")"'
C
C --- FORMAT D'ECRITURE D'UNE LIGNE :
C     -----------------------------
      FORM4  = '2X,A8,A1,A8,A1,A8,2X'
      FORMAV = '('//FORM4//','//FORVAR//')'
      FORMAC = '('//FORM4//','//FORVAC//')'
C
C --- CREATION DU TABLEAU DU NOMBRE DE COMPOSANTES PAR NOEUD :
C     ------------------------------------------------------
      LINBCM = '&&IMVELV.NBCOMP'
      CALL WKVECT(LINBCM,'V V I',NBNOMX,IDNBCM)
C
C --- RECUPERATION DE LA LISTE DES RESU_ELEM DU VECT_ELEM :
C     ---------------------------------------------------
      CALL JEVEUO(MATEL//'.LISTE_RESU','L',IDLRES)
C
C --- RECUPERATION DU NOMBRE DE RESU_ELEM DU VECT_ELEM :
C     ------------------------------------------------
      CALL JELIRA(MATEL//'.LISTE_RESU','LONUTI',NBRESU,K1BID)
C
      NUMLAG = 0
      IMLIGR = 0
C
C --- BOUCLE SUR LES RESU_ELEM DU VECT_ELEM :
C     -------------------------------------
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
C ---   RECUPERATION DE LA GRANDEUR ASSOCIEE AU VECT_ELEM :
C       -------------------------------------------------
         GD = ZI(IDDESC+1-1)
C
C ---   NOM DE LA GRANDEUR ASSOCIEE AU VECT_ELEM :
C       ----------------------------------------
         CALL JENUNO(JEXNUM('&CATA.GD.NOMCMP',GD),NOMGD)
C
C ---   GRANDEUR SIMPLE ASSOCIEE AU VECT_ELEM :
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
C ---   TYPE (REEL OU COMPLEXE) ASSOCIE AUX VALEURS DU RESU_ELEM :
C       --------------------------------------------------------
        SCAL = SCALAI(GD)
        TYPVAL = SCAL(1:1)
C
C ---   CAS D'UN RESU_ELEM REEL :
C       -----------------------
        IF (TYPVAL.EQ.'R') THEN
C
C ---   CREATION DU TABLEAU DES NOMS DES COMPOSANTES ASSOCIEES
C ---    A LA GRANDEUR :
C        -------------
         LINOCM = '&&IMVELV1.NOMCMP'
         CALL WKVECT(LINOCM,'V V K8',NCMPMX*NBNOMX,IDNOMC)
C
C ---   CREATION DU TABLEAU DES VALEURS DE LA LIGNE COURANTE
C ---   DU VECT_ELEM OU DU VECT_ELEM A IMPRIMER :
C        --------------------------------------
         LIVALE = '&&IMVELV1.VALE'
         CALL WKVECT(LIVALE,'V V R',NCMPMX*NBNOMX,IDVALE)
C
C ---   CREATION DU TABLEAU DES NOMS NOEUDS, A CHAQUE INCONNUE
C ---   DE LA VECTEUR ELEMENTAIRE, ON ASSOCIE LE NOM DU NOEUD
C ---   SUR-LAQUELLE ELLE PORTE :
C        ----------------------
         LINONO = '&&IMVELV1.NOMNOE'
         CALL WKVECT(LINONO,'V V K8',NCMPMX*NBNOMX,IDNONO)
C
C ---   CREATION DU TABLEAU D'INDICATEURS DISANT SI UNE COMPOSANTE
C ---   EST A PRENDRE EN COMPTE OU NON :
C        -----------------------------
         LINDCM = '&&IMVELV1.INDIC'
         CALL WKVECT(LINDCM,'V V I',NCMPMX*NBNOMX,IDINCM)
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
C ---   NOM DE L'OPTION ASSOCIEE AU VECT_ELEM :
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
C ---   BOUCLE SUR LES GROUPES D'ELEMENTS DU LIGREL :
C       ------------------------------------------
         DO 20 IGR = 1, NBGREL
C
C ---      INITIALISATION DE LA LISTE DES NOMS DES COMPOSANTES ASSOCIEES
C ---      AUX NOEUDS DES ELEMENTS :
C          -----------------------
            DO 30 ICMP = 1, NCMPMX*NBNOMX
              ZK8(IDNOMC+ICMP-1) = M8BLAN
  30        CONTINUE
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
            DO 40 INO = 1, NNOE
              NBCMNO = 0
              DO 50 ICMP = 1, NCMPMX
                DO 60 IEC = 1, NEC
                  DG(IEC) = ENTCOD(ADMODL,LCMODL,NEC,MODE,INO,IEC)
  60            CONTINUE
                IF (EXISDG(DG,ICMP)) THEN
                    NBCMNO = NBCMNO + 1
                    ICO  = ICO + 1
                    ZK8(IDNOMC+ICO-1) = ZK8(INOCMP+ICMP-1)
                ENDIF
  50          CONTINUE
              ZI(IDNBCM+INO-1) = NBCMNO
  40        CONTINUE
C
C ---      NOMBRE DE DDL ASSOCIE AU VECT_ELEM  :
C          ----------------------------------
            NBINCO = ICO
C
            DO 70 I = 1, NBINCO
 70        CONTINUE
C
C ---      NOMBRE DE COMPOSANTES DU VECTEUR :
C          --------------------------------
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
C ---      RECUPERATION  DES VECTEURS ELEMENTAIRES DU GREL :
C          -----------------------------------------------
            CALL JEVEUO(JEXNUM(RESU(1:19)//'.RESL',IGR),'L',IDRESL)
C
C ---      BOUCLE SUR LES ELEMENTS DU GREL :
C          -------------------------------
            DO 80 IEL = 1, NBEL
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
               IF (.NOT.IMPLIG(NOMAIL,NBELEM,ZK8(IDLIMA))) GOTO 80
C
C ---         RECUPERATION DES CONNECTIVITES DE LA MAILLE :
C             -------------------------------------------
               CALL JEVEUO(JEXNUM(NOMA//'.CONNEX',NUMEL),'L',IDNOEU)
C
C ---         BOUCLE SUR LES CONNECTIVITES DE LA MAILLE :
C             -----------------------------------------
               K    = 0
               DO 90 INO = 1, NNOE
C
C ---          NOM DE LA CONNECTIVITE :
C              ---------------------
                  CALL JENUNO(JEXNUM(NOEUMA,ZI(IDNOEU+INO-1)),NOMNOE)
C
C ---          NOMBRE DE COMPOSANTES DU NOEUD INO :
C              ----------------------------------
                   NBCM = ZI(IDNBCM+INO-1)
C
C ---          BOUCLE SUR LES COMPOSANTES DU NOEUD INO :
C              ---------------------------------------
                   DO 100 ICMP = 1, NBCM
                     K = K + 1
C
C ---            AFFECTATION DU TABLEAU DU NOM DES NOEUDS :
C                ----------------------------------------
                     ZK8(IDNONO+K-1) = NOMNOE
 100               CONTINUE
  90           CONTINUE
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
               IF (.NOT.IMPLIG(NOMAIL,NBELEM,ZK8(IDLIMA))) GOTO 80
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
               DO 110 INO = 1, NNOE
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
                   NBCM = ZI(IDNBCM+INO-1)
C
C ---          BOUCLE SUR LES COMPOSANTES DU NOEUD INO :
C              ---------------------------------------
                   DO 120 ICMP = 1, NBCM
                     K = K + 1
C
C ---            AFFECTATION DU TABLEAU DU NOM DES NOEUDS :
C                ----------------------------------------
                     ZK8(IDNONO+K-1) = NOMNOE
  120               CONTINUE
  110           CONTINUE
             ENDIF
C
             DO 130 I = 1, NBINCO
 130        CONTINUE
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
C=======================================
C ---        IMPRESSION PAR VALEUR     =
C=======================================
C
C ---         BOUCLE SUR LE NOMBRE DE LIGNES :
C             ------------------------------
               DO 140 I = 1, NBINCO
C
               NOMNO1 = ZK8(IDNONO+I-1)
               NOMCM1 = ZK8(IDNOMC+I-1)
C
               IF (.NOT.IMPLIG(NOMCM1,NBCMP,ZK8(IDLICM))) GOTO 140
C
               WRITE(IFM,FORMAV) NOMAIL,SLACH,NOMNO1,SLACH,
     &                      NOMCM1,ZR(IDRESL+NCMPEL*(IEL-1)+I-1)

 140         CONTINUE
 80         CONTINUE
          ENDIF
 20      CONTINUE
         CALL JEDETC('V','&&IMVELV1',1)
C
C ---   CAS D'UN RESU_ELEM COMPLEXE :
C       --------------------------
        ELSEIF (TYPVAL.EQ.'C') THEN
C
C ---   CREATION DU TABLEAU DES NOMS DES COMPOSANTES ASSOCIEES
C ---    A LA GRANDEUR :
C        -------------
         LINOCM = '&&IMVELV1.NOMCMP'
         CALL WKVECT(LINOCM,'V V K8',NCMPMX*NBNOMX,IDNOMC)
C
C ---   CREATION DU TABLEAU DES VALEURS DE LA LIGNE COURANTE
C ---   DU VECT_ELEM OU DU VECT_ELEM A IMPRIMER :
C        --------------------------------------
         LIVALE = '&&IMVELV1.VALE'
         CALL WKVECT(LIVALE,'V V C',NCMPMX*NBNOMX,IDVALE)
C
C ---   CREATION DU TABLEAU DES NOMS NOEUDS, A CHAQUE INCONNUE
C ---   DE LA VECTEUR ELEMENTAIRE, ON ASSOCIE LE NOM DU NOEUD
C ---   SUR-LAQUELLE ELLE PORTE :
C        ----------------------
         LINONO = '&&IMVELV1.NOMNOE'
         CALL WKVECT(LINONO,'V V K8',NCMPMX*NBNOMX,IDNONO)
C
C ---   CREATION DU TABLEAU D'INDICATEURS DISANT SI UNE COMPOSANTE
C ---   EST A PRENDRE EN COMPTE OU NON :
C        -----------------------------
         LINDCM = '&&IMVELV1.INDIC'
         CALL WKVECT(LINDCM,'V V I',NCMPMX*NBNOMX,IDINCM)
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
C ---   NOM DE L'OPTION ASSOCIEE AU VECT_ELEM :
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
C ---   BOUCLE SUR LES GROUPES D'ELEMENTS DU LIGREL :
C       ------------------------------------------
         DO 150 IGR = 1, NBGREL
C
C ---      INITIALISATION DE LA LISTE DES NOMS DES COMPOSANTES ASSOCIEES
C ---      AUX NOEUDS DES ELEMENTS :
C          -----------------------
            DO 160 ICMP = 1, NCMPMX*NBNOMX
              ZK8(IDNOMC+ICMP-1) = M8BLAN
 160        CONTINUE
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
            DO 170 INO = 1, NNOE
              NBCMNO = 0
              DO 180 ICMP = 1, NCMPMX
                DO 190 IEC = 1, NEC
                  DG(IEC) = ENTCOD(ADMODL,LCMODL,NEC,MODE,INO,IEC)
 190            CONTINUE
                  IF (EXISDG(DG,ICMP)) THEN
                    NBCMNO = NBCMNO + 1
                    ICO  = ICO + 1
                    ZK8(IDNOMC+ICO-1) = ZK8(INOCMP+ICMP-1)
                  ENDIF
 180          CONTINUE
              ZI(IDNBCM+INO-1) = NBCMNO
 170        CONTINUE
C
C ---      NOMBRE DE DDL ASSOCIE AU VECT_ELEM  :
C          ----------------------------------
            NBINCO = ICO
C
            DO 200 I = 1, NBINCO
 200        CONTINUE
C
C ---      NOMBRE DE COMPOSANTES DU VECTEUR :
C          --------------------------------
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
C ---      RECUPERATION  DES VECTEURS ELEMENTAIRES DU GREL :
C          -----------------------------------------------
            CALL JEVEUO(JEXNUM(RESU(1:19)//'.RESL',IGR),'L',IDRESL)
C
C ---      BOUCLE SUR LES ELEMENTS DU GREL :
C          -------------------------------
            DO 210 IEL = 1, NBEL
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
               IF (.NOT.IMPLIG(NOMAIL,NBELEM,ZK8(IDLIMA))) GOTO 210
C
C ---         RECUPERATION DES CONNECTIVITES DE LA MAILLE :
C             -------------------------------------------
               CALL JEVEUO(JEXNUM(NOMA//'.CONNEX',NUMEL),'L',IDNOEU)
C
C ---         BOUCLE SUR LES CONNECTIVITES DE LA MAILLE :
C             -----------------------------------------
               K    = 0
               DO 220 INO = 1, NNOE
C
C ---          NOM DE LA CONNECTIVITE :
C              ---------------------
                  CALL JENUNO(JEXNUM(NOEUMA,ZI(IDNOEU+INO-1)),NOMNOE)
C
C ---          NOMBRE DE COMPOSANTES DU NOEUD INO :
C              ----------------------------------
                   NBCM = ZI(IDNBCM+INO-1)
C
C ---          BOUCLE SUR LES COMPOSANTES DU NOEUD INO :
C              ---------------------------------------
                   DO 230 ICMP = 1, NBCM
                     K = K + 1
C
C ---            AFFECTATION DU TABLEAU DU NOM DES NOEUDS :
C                ----------------------------------------
                     ZK8(IDNONO+K-1) = NOMNOE
 230               CONTINUE
 220           CONTINUE
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
               IF (.NOT.IMPLIG(NOMAIL,NBELEM,ZK8(IDLIMA))) GOTO 210
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
               DO 240 INO = 1, NNOE
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
                   NBCM = ZI(IDNBCM+INO-1)
C
C ---          BOUCLE SUR LES COMPOSANTES DU NOEUD INO :
C              ---------------------------------------
                   DO 250 ICMP = 1, NBCM
                     K = K + 1
C
C ---            AFFECTATION DU TABLEAU DU NOM DES NOEUDS :
C                ----------------------------------------
                     ZK8(IDNONO+K-1) = NOMNOE
  250               CONTINUE
  240           CONTINUE
             ENDIF
C
             DO 260 I = 1, NBINCO
 260        CONTINUE
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
C=======================================
C ---        IMPRESSION PAR VALEUR     =
C=======================================
C
C ---         BOUCLE SUR LE NOMBRE DE LIGNES :
C             ------------------------------
               DO 270 I = 1, NBINCO
C
               NOMNO1 = ZK8(IDNONO+I-1)
               NOMCM1 = ZK8(IDNOMC+I-1)
C
               IF (.NOT.IMPLIG(NOMCM1,NBCMP,ZK8(IDLICM))) GOTO 270
C
               WRITE(IFM,FORMAC) NOMAIL,SLACH,NOMNO1,SLACH,
     &                      NOMCM1,ZC(IDRESL+NCMPEL*(IEL-1)+I-1)

 270         CONTINUE
 210        CONTINUE
          ENDIF
 150     CONTINUE
         CALL JEDETC('V','&&IMVELV1',1)
        ELSE
          CALL U2MESK('F','PREPOST_63',1,TYPVAL)
        ENDIF
 10   CONTINUE
C
      CALL JEDETC('V','&&IMVELV',1)
C
      CALL JEDEMA()
C
      END

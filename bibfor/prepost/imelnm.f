      SUBROUTINE IMELNM ( IFM, NOMSDZ, NBCMP, LISCMZ, NBELEM,
     +                    LISMAZ, NBCHIF )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 11/01/2005   AUTEUR CIBHHLV L.VIVAN 
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
C      IMELNM -- IMPRESSION DU MATR_ELEM DE NOM NOMSD
C                (SYMETRIQUE OU NON-SYMETRIQUE)
C                DANS LE FICHIER DE NOM FICHIER.
C                LE GRAIN DE L'IMPRESSION EST 'MAILLE'
C                (I.E. ON IMPRIME TOUS LES TERMES DES MATRICES
C                      ELEMENTAIRES)
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
C    NBCHIF          IN    I     NOMBRE DE CHIFFRES SIGNICATIFS DANS
C                                LE FORMAT D'IMPRESSION
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
      CHARACTER*32     JEXNUM, JEXNOM
C -----  ARGUMENTS
      CHARACTER*(*) NOMSDZ, LISCMZ, LISMAZ
C -----  VARIABLES LOCALES
      PARAMETER     (NBNOMX = 27)
      PARAMETER     (NBTELI = 810)
      PARAMETER     (NBECMX = 10)
      CHARACTER*1   SLACH
      CHARACTER*1   EXCLAM
      CHARACTER*1   K1BID , TYMAT, TYPVAL
      CHARACTER*2   KBLAN1, KLONLI, KLONG
      CHARACTER*2   KNBCH, KLVALR, KLONM
      CHARACTER*2   KLONLF , KLONLS, KLONLO
      CHARACTER*4   KLONLG
      CHARACTER*7   SYM
      CHARACTER*8   NONOEU(NBTELI), NOCMP(NBTELI)
      CHARACTER*8   K8BID, NOMA, NOMGD
      CHARACTER*8   M8BLAN, MATEL, NOMNOE, NOMAIL
      CHARACTER*8   FORVAR
      CHARACTER*8   SCAL, SCALAI
      CHARACTER*15  LISBI1, LISBI2
      CHARACTER*15  LISBI3
      CHARACTER*16  OPTION
      CHARACTER*17  NOMNCI(NBTELI)
      CHARACTER*19  NOMSD
      CHARACTER*24  LISCMP, LINOCM, LINBCM, LINONO
      CHARACTER*24  LIVALE, LINDCM
      CHARACTER*24  NOEUMA
      CHARACTER*24  RESU, NOLI, NOMOPT, LIEL, NEMA, LISMAI
      CHARACTER*72  FORM1, FORM2, FORM3, FORM4
      CHARACTER*72  FORMA, FORMB, FORMC
      CHARACTER*80  FORVAC
      CHARACTER*120 FORMC1, FORMC2, FORMC3, FORMC4
      INTEGER       GD
      INTEGER       DIGDEL, NBNO, NBEC, DG(NBECMX), ENTCOD
      INTEGER       LXLGUT
      COMPLEX*16    ZEROC
      LOGICAL       EXISDG, IMPLIG
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
      CALL JEMARQ ( )
C
C --- INITIALISATIONS :
C     ---------------
      MATEL  = NOMSDZ
      LISCMP = LISCMZ
      LISMAI = LISMAZ
C
      ZERO   = 0.0D0
      ZEROC  = (0.0D0,0.0D0)
      M8BLAN = '        '
      OPTION = '                '
      SLACH  = '/'
      EXCLAM = '!'
      NBNOE  = 0
      NUMLAG = 0
      DO 1 I =1, NBECMX
        DG(I) = 0
 1    CONTINUE
C
C --- RECUPERATION DES NOEUDS POUR-LESQUELS ON VEUT L'IMPRESSION
C --- DE LA MATRICE :
C     -------------
         LISBI1 = '&&IMELNM.BID1'
         CALL WKVECT(LISBI1,'V V K8',1,IDLINO)
C
C --- RECUPERATION DES MAILLES POUR-LESQUELLES ON VEUT L'IMPRESSION
C --- DE LA MATRICE :
C     -------------
      IF (NBELEM.EQ.0) THEN
C
C ---    ON FABRIQUE UN VECTEUR BIDON DE K8 D'1 SEUL TERME SI LA
C ---    EST VIDE :
C        --------
         LISBI2 = '&&IMELNM.BID2'
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
         LISBI3 = '&&IMELNM.BID3'
         CALL WKVECT(LISBI3,'V V K8',1,IDLICM)
      ELSE
         CALL JEVEUO(LISCMP,'L',IDLICM)
      ENDIF
C
C --- CREATION DU TABLEAU DU NOMBRE DE COMPOSANTES PAR NOEUD :
C     ------------------------------------------------------
      LINBCM = '&&IMELNM.NBCOMP'
      CALL WKVECT(LINBCM,'V V I',NBNOMX,IDNBCM)
C
C --- RECUPERATION DE LA LISTE DES RESU_ELEM DU MATR_ELEM :
C     ---------------------------------------------------
      CALL JEVEUO(MATEL//'.LISTE_RESU','L',IDLRES)
C
C --- RECUPERATION DU NOMBRE DE RESU_ELEM DU MATR_ELEM :
C     ------------------------------------------------
      CALL JELIRA(MATEL//'.LISTE_RESU','LONUTI',NBRESU,K1BID)
C
C --- BOUCLE SUR LES RESU_ELEM DU MATR_ELEM :
C     -------------------------------------
      DO 10 IRESU = 1, NBRESU
C
         IMLIGR = 0
C
C ---   NOM DU RESU_ELEM COURANT :
C       ------------------------
         RESU = ZK24(IDLRES+IRESU-1)
C
         CALL JEEXIN(RESU(1:19)//'.DESC',IER)
         IF (IER.EQ.0) GO TO 10
C
C ---   DETERMINATION DU TYPE 'SYMETRIQUE' OU 'NON-SYMETRIQUE'
C ---   DU RESU_ELEM :
C       ------------
         TYMAT = 'S'
         CALL DISMOI('F','TYPE_MATRICE',RESU,'RESUELEM',IBID,SYM,IER)
         IF (SYM.EQ.'NON_SYM') TYMAT = 'N'
C
C ---   RECUPERATION DU DESCRIPTEUR DU RESU_ELEM :
C       ----------------------------------------
         CALL JEVEUO(RESU(1:19)//'.DESC','L',IDDESC)
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
     +               K1BID)
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
         LINOCM = '&&IMELNM1.NOMCMP'
         CALL WKVECT(LINOCM,'V V K8',NCMPMX*NBNOMX*NEC,IDNOMC)
C
C ---   CREATION DU TABLEAU DES VALEURS DE LA LIGNE COURANTE
C ---   DU MATR_ELEM OU DU VECT_ELEM A IMPRIMER :
C        --------------------------------------
         LIVALE = '&&IMELNM1.VALE'
         CALL WKVECT(LIVALE,'V V R',NCMPMX*NBNOMX,IDVALE)
C
C ---   CREATION DU TABLEAU DES NOMS NOEUDS, A CHAQUE INCONNUE
C ---   DE LA MATRICE ELEMENTAIRE, ON ASSOCIE LE NOM DU NOEUD
C ---   SUR-LAQUELLE ELLE PORTE :
C        ----------------------
         LINONO = '&&IMELNM1.NOMNOE'
         CALL WKVECT(LINONO,'V V K8',NCMPMX*NBNOMX,IDNONO)
C
C ---   CREATION DU TABLEAU D'INDICATEURS DISANT SI UNE COMPOSANTE
C ---   EST A PRENDRE EN COMPTE OU NON :
C        -----------------------------
         LINDCM = '&&IMELNM1.INDIC'
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
         CALL CODENT(LGTOT ,'G',KLONLF)
C
C ---   FORMATS D'IMPRESSION DU NOM DU LIGREL ET DE L'OPTION :
C       ----------------------------------------------------
         FORMA = '('//'//,2X,'//KLONLF//'("_")'//')'
         FORMC = '(2X,'//KLONLF//'("_")'//',//'//')'
         FORMB = '(2X,'//'"!",X,"LIGREL : "A'//KLONLS//
     +           ',4X,"OPTION : "A'//KLONLO//',X,"!"'//')'
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
           CALL ENTCO0 ( MODE, IADM, M1 )
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
                  DG(IEC) = ENTCOD(NEC,IADM,M1,INO,IEC)
  60            CONTINUE
                IF (EXISDG(DG,ICMP)) THEN
                  NBCMNO = NBCMNO + 1
                  ICO  = ICO + 1
                  ZK8(IDNOMC+ICO-1) = ZK8(INOCMP+ICMP-1)
                  NOCMP(ICO)        = ZK8(INOCMP+ICMP-1)
                ENDIF
  50          CONTINUE
              ZI(IDNBCM+INO-1) = NBCMNO
  40        CONTINUE
C
C ---      NOMBRE DE DDL ASSOCIE AU MATR_ELEM OU AU VECT_ELEM :
C          --------------------------------------------------
            NBINCO = ICO
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
            DO 70 IEL = 1, NBEL
C
                 DO 80 J = 1, NBINCO
                    ZK8(IDNOMC+J-1) = NOCMP(J)
 80             CONTINUE
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
               IF (.NOT.IMPLIG(NOMAIL,NBELEM,ZK8(IDLIMA))) GOTO 70
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
                     NONOEU(K)       = NOMNOE
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
               IF (.NOT.IMPLIG(NOMAIL,NBELEM,ZK8(IDLIMA))) GOTO 70
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
                    ZK8(IDNONO+1-1) = NOMNOE
                    NONOEU(K)       = NOMNOE
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
                     NONOEU(K)       = NOMNOE
  120               CONTINUE
  110           CONTINUE
             ENDIF
C
C ---        RECONSTRUCTION DES TABLEAUX DE TRAVAIL  PAR FILTRAGE
C ---        EN FONCTION DES NOEUDS ET DES COMPOSANTES SPECIFIES
C ---        PAR L'UTILISATEUR :
C            -----------------
             CALL TRNOR1 (NBNOE, ZK8(IDLINO), NBCMP,
     +                    ZK8(IDLICM), OPTION, NBINCO, ZK8(IDNONO),
     +                    ZK8(IDNOMC), ZR(IDVALE), ZI(IDINCM), LONLI2)
C
             IF (LONLI2.EQ.0) GOTO 70
C
C ---        CONCATENATION DES NOMS DES NOEUDS ET DES NOMS DES
C ---        COMPOSANTES :
C            -----------
             LNOMCM = 0
             DO 130 I = 1, LONLI2
                NOMNCI(I) = ZK8(IDNONO+I-1)(1:LXLGUT(ZK8(IDNONO+I-1)))
     +                    //SLACH//ZK8(IDNOMC+I-1)
                LNOMCM = MAX(LNOMCM,LXLGUT(NOMNCI(I)))
 130         CONTINUE
C
C ---        NOMBRE DE CHIFFRES SIGNIFICATIFS :
C            --------------------------------
             NBCH  = MIN (14, NBCHIF)
             NBCH1 = NBCH - 1
C
C ---        LONGUEUR PRISE POUR REPRESENTER UNE VALEUR REELLE :
C            -------------------------------------------------
             LVALRE = NBCH + 6
C
C ---        FORMAT D'ECRITURE DES VALEURS REELLES :
C            -------------------------------------
             CALL CODENT(NBCH1,'G',KNBCH)
             CALL CODENT(LVALRE,'D',KLVALR)
C
             FORVAR  = '1PD'//KLVALR//'.'//KNBCH
C
C ---        LONGUEUR D'UNE 'CASE' :
C            --------------------
             LONG = MAX(17,LVALRE)
             LONG = MAX(LNOMCM,LVALRE)
             CALL CODENT(LONG,'D',KLONG)
C
C ---        LONGUEUR D'UNE LIGNE :
C            --------------------
             LONLIG = LONLI2*(LONG+3)+19
             CALL CODENT(LONLIG,'D',KLONLG)
C
C ---        CODAGE DU NOMBRE DE TERMES DE LA LIGNE SOUS FORME D'UNE
C ---        CHAINE DE CARACTERES :
C            --------------------
             CALL CODENT(LONLI2,'D',KLONLI)
C
C ---        DEFINITION DES FORMATS SELON QUE LA LONGUEUR D'UN
C ---        NOMBRE EST PLUS OU MOINS GRANDE QUE LA CHAINE
C ---        DE CARACTERES (NOMNOE+NOMCMP) LA DESIGNANT :
C            ------------------------------------------
             IF (LONG.EQ.LNOMCM) THEN
C
C ---          NOMBRE DE BLANCS DEVANT COMPLETER L'IMPRESSION
C ---          D'UN NOMBRE :
C              -----------
                NBLAN2 = LNOMCM - LVALRE + 1
                CALL CODENT(NBLAN2,'D',KBLAN1)
C
               FORM3 = '(2X,'//KLONLG//'("_")'//')'
               FORM4 = '(2X,A1,A17,A1,'//
     +                 KLONLI//'(X,'//FORVAR//','//KBLAN1//',X,A1))'
               FORM2 = '(2X,A1,A8,9X,A1,'//KLONLI//'(X,A'//KLONG//
     +                 'X,A1))'
               FORM1 = '('//'2X,'//KLONLG//'("_")'//')'

             ELSEIF (LONG.EQ.LVALRE) THEN
C
C ---          NOMBRE DE BLANCS DEVANT COMPLETER L'IMPRESSION
C ---          DE LA CHAINE DE CARACTERES (NOMNOE+NOMCMP) :
C              ------------------------------------------
                NBLAN2 = LVALRE -LNOMCM +1
                CALL CODENT(NBLAN2,'D',KBLAN1)
                CALL CODENT(LNOMCM,'D',KLONM)
C
                FORM3 = '(2X,'//KLONLG//'("_")'//')'
                FORM4 = '(2X,A1,A17,A1,'//
     +                   KLONLI//'(X,'//FORVAR//',X,A1))'
                FORM2 = '(2X,A1,A8,9X,A1,'//KLONLI//'(X,A'//KLONM//','
     +                    //KBLAN1//'X,A1))'
                FORM1 = '('//'2X,'//KLONLG//'("_")'//')'
             ENDIF
C
C ---        IMPRESSION DU NOM DU LIGREL ET DU NOM DE L'OPTION :
C            -------------------------------------------------
             IMLIGR = IMLIGR + 1
             IF (IMLIGR.EQ.1) THEN
               WRITE(IFM,FORMA)
               WRITE(IFM,FORMB) NOLI(1:LGNOMS), NOMOPT(1:LGNOMO)
               WRITE(IFM,FORMC)
             ENDIF
C
C ---          IMPRESSION DU CADRE ET DU DESCRIPTIF
C ---          (NOMNOE+NOMCMP) DE L'ELEMENT :
C              ----------------------------
             WRITE(IFM,FORM1)
             WRITE(IFM,FORM2) EXCLAM, NOMAIL,EXCLAM,
     +                        (NOMNCI(I),EXCLAM,I=1,LONLI2)
             WRITE(IFM,FORM3)
C
C================================================================
C ---        IMPRESSION PAR VALEUR                              =
C ---        RECONSTITUTION DE LA LIGNE COURANTE DU MATR_ELEM   =
C================================================================
C
C ---          CAS DES MATRICES NON_SYMETRIQUES :
C              --------------------------------
              IF (TYMAT.EQ.'N') THEN
C
C ---         BOUCLE SUR LE NOMBRE DE LIGNES :
C             ------------------------------
               DO 140 I = 1, NBINCO
C
                IF (IMPLIG(NOCMP(I),NBCMP,ZK8(IDLICM))) THEN
C
C ---           RECONSTITUTION DES LISTES DES NOMS DES COMPOSANTES
C ---           ASSOCIEES AUX NOEUDS DES ELEMENTS :
C               ---------------------------------
C
                 DO 150 J = 1, NBINCO
                    ZK8(IDNONO+J-1) = NONOEU(J)
                    ZK8(IDNOMC+J-1) = NOCMP(J)
 150             CONTINUE
C
C ---          INITIALISATION DU TABLEAU DES VALEURS DE LA LIGNE :
C              -------------------------------------------------
                 DO 160 J = 1, NBINCO
                   ZR(IDVALE+J-1) = ZERO
 160             CONTINUE
C
C ---          POINTEUR DU DERNIER TERME TRAITE DE LA MATRICE :
C              ----------------------------------------------
                 NBTERM = (I-1)*NBINCO
C
C ---          BOUCLE SUR LES TERMES DE LA LIGNE COURANTE DE LA
C ---          MATRICE  :
C              -------
                 DO 170 J = 1, NBINCO
                    ZR(IDVALE+J-1) = ZR(IDRESL+NCMPEL*(IEL-1)
     +                                  + NBTERM + J -1)
 170             CONTINUE
C
C ---          RECONSTRUCTION DES TABLEAUX DE TRAVAIL  PAR FILTRAGE
C ---          EN FONCTION DES NOEUDS ET DES COMPOSANTES SPECIFIES
C ---          PAR L'UTILISATEUR :
C              -----------------
                 CALL TRNOR1 (NBNOE, ZK8(IDLINO), NBCMP,
     +                        ZK8(IDLICM), OPTION, NBINCO, ZK8(IDNONO),
     +                        ZK8(IDNOMC), ZR(IDVALE), ZI(IDINCM),
     +                        LONLI2)
C
C ---          IMPRESSION DE LA LIGNE COURANTE SELON LE GRAIN 'MAILLE':
C              ------------------------------------------------------
                 CALL IMPFR5 (NONOEU(I), NOCMP(I),
     +                        LONLI2, ZR(IDVALE), FORM3, FORM4,
     +                        IFM)
C
                ENDIF
 140           CONTINUE
C
C ---          CAS DES MATRICES SYMETRIQUES :
C              ----------------------------
             ELSEIF (TYMAT.EQ.'S') THEN
C
C ---         BOUCLE SUR LE NOMBRE DE LIGNES :
C             ------------------------------
               DO 180 I = 1, NBINCO
C
                IF (IMPLIG(NOCMP(I),NBCMP,ZK8(IDLICM))) THEN
C
C ---           RECONSTITUTION DES LISTES DES NOMS DES COMPOSANTES
C ---           ASSOCIEES AUX NOEUDS DES ELEMENTS :
C               ---------------------------------
C
                 DO 190 J = 1, NBINCO
                    ZK8(IDNONO+J-1) = NONOEU(J)
                    ZK8(IDNOMC+J-1) = NOCMP(J)
 190             CONTINUE
C
C ---          INITIALISATION DU TABLEAU DES VALEURS DE LA LIGNE :
C              -------------------------------------------------
                 DO 200 J = 1, NBINCO
                   ZR(IDVALE+J-1) = ZERO
 200             CONTINUE
C
C ---          POINTEUR DU DERNIER TERME TRAITE DE LA DEMI-MATRICE :
C              ---------------------------------------------------
                 NBTERM = (I*(I-1))/2
C
C ---          BOUCLE SUR LES TERMES DE LA LIGNE COURANTE DE LA
C ---          DEMI-MATRICE INFERIEURE :
C              -----------------------
                 DO 210 J = 1, I
                    ZR(IDVALE+J-1) = ZR(IDRESL+NCMPEL*(IEL-1)
     +                                  + NBTERM + J -1)
 210             CONTINUE
C
C ---          BOUCLE SUR LES TERMES DE LA COLONNE I SITUES
C ---          EN DESSOUS DU TERME DIAGONAL :
C              ----------------------------
                 DO 220 J = I+1, NBINCO
C
C ---            POINTEUR DU TERME DIAGONAL PRECEDANT LA LIGNE
C ---            COURANTE DANS LA DEMI-MATRICE :
C                -----------------------------
                    NBTERM = (J*(J-1))/2
C
                    ZR(IDVALE+J-1) = ZR(IDRESL+NCMPEL*(IEL-1)
     +                                  + NBTERM + I -1)
 220             CONTINUE
C
C ---          RECONSTRUCTION DES TABLEAUX DE TRAVAIL  PAR FILTRAGE
C ---          EN FONCTION DES NOEUDS ET DES COMPOSANTES SPECIFIES
C ---          PAR L'UTILISATEUR :
C              -----------------
                 CALL TRNOR1 (NBNOE, ZK8(IDLINO), NBCMP,
     +                        ZK8(IDLICM), OPTION, NBINCO, ZK8(IDNONO),
     +                        ZK8(IDNOMC), ZR(IDVALE), ZI(IDINCM),
     +                        LONLI2)
C
C ---          IMPRESSION DE LA LIGNE COURANTE SELON LE GRAIN 'MAILLE':
C              ------------------------------------------------------
                 CALL IMPFR5 (NONOEU(I), NOCMP(I),
     +                        LONLI2, ZR(IDVALE), FORM3, FORM4,
     +                        IFM)
C
                ENDIF
 180          CONTINUE
             ENDIF
 70         CONTINUE
          ENDIF
 20      CONTINUE
         CALL JEDETC('V','&&IMELNM1',1)
C
C ---   CAS D'UN RESU_ELEM COMPLEXE :
C       ---------------------------
        ELSEIF (TYPVAL.EQ.'C') THEN
C
C ---   CREATION DU TABLEAU DES NOMS DES COMPOSANTES ASSOCIEES
C ---    A LA GRANDEUR :
C        -------------
         LINOCM = '&&IMELNM1.NOMCMP'
         CALL WKVECT(LINOCM,'V V K8',NCMPMX*NBNOMX,IDNOMC)
C
C ---   CREATION DU TABLEAU DES VALEURS DE LA LIGNE COURANTE
C ---   DU MATR_ELEM OU DU VECT_ELEM A IMPRIMER :
C        --------------------------------------
         LIVALE = '&&IMELNM1.VALE'
         CALL WKVECT(LIVALE,'V V C',NCMPMX*NBNOMX,IDVALE)
C
C ---   CREATION DU TABLEAU DES NOMS NOEUDS, A CHAQUE INCONNUE
C ---   DE LA MATRICE ELEMENTAIRE, ON ASSOCIE LE NOM DU NOEUD
C ---   SUR-LAQUELLE ELLE PORTE :
C        ----------------------
         LINONO = '&&IMELNM1.NOMNOE'
         CALL WKVECT(LINONO,'V V K8',NCMPMX*NBNOMX,IDNONO)
C
C ---   CREATION DU TABLEAU D'INDICATEURS DISANT SI UNE COMPOSANTE
C ---   EST A PRENDRE EN COMPTE OU NON :
C        -----------------------------
         LINDCM = '&&IMELNM1.INDIC'
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
         CALL CODENT(LGTOT ,'G',KLONLF)
C
C ---   FORMATS D'IMPRESSION DU NOM DU LIGREL ET DE L'OPTION :
C       ----------------------------------------------------
         FORMA = '('//'//,2X,'//KLONLF//'("_")'//')'
         FORMC = '(2X,'//KLONLF//'("_")'//',//'//')'
         FORMB = '(2X,'//'"!",X,"LIGREL : "A'//KLONLS//
     +           ',4X,"OPTION : "A'//KLONLO//',X,"!"'//')'
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
         DO 230 IGR = 1, NBGREL
C
C ---      INITIALISATION DE LA LISTE DES NOMS DES COMPOSANTES ASSOCIEES
C ---      AUX NOEUDS DES ELEMENTS :
C          -----------------------
            DO 240 ICMP = 1, NCMPMX*NBNOMX
              ZK8(IDNOMC+ICMP-1) = M8BLAN
 240        CONTINUE
C
C ---    RECUPERATION DU MODE ASSOCIE AU GROUPE D'ELEMENTS COURANT :
C        ---------------------------------------------------------
          MODE = ZI(IDDESC+2+IGR-1)
C
          IF (MODE.GT.0) THEN
C
           CALL ENTCO0 ( MODE, IADM, M1 )
C
C ---      RECUPERATION DU NOMBRE DE CONNECTIVITES DES ELEMENTS DU GREL:
C          ------------------------------------------------------------
            NNOE = NBNO(MODE)
C
C ---      CONSTITUTION DE LA LISTE DES NOMS DES COMPOSANTES ASSOCIEES
C ---      AUX NOEUDS DES ELEMENTS :
C          -----------------------
            ICO = 0
            DO 250 INO = 1, NNOE
              NBCMNO = 0
              DO 260 ICMP = 1, NCMPMX
                DO 270 IEC = 1, NEC
                  DG(IEC) = ENTCOD(NEC,IADM,M1,INO,IEC)
 270            CONTINUE
                  IF (EXISDG(DG,ICMP)) THEN
                    NBCMNO = NBCMNO + 1
                    ICO  = ICO + 1
                    ZK8(IDNOMC+ICO-1) = ZK8(INOCMP+ICMP-1)
                    NOCMP(ICO)        = ZK8(INOCMP+ICMP-1)
                  ENDIF
 260          CONTINUE
              ZI(IDNBCM+INO-1) = NBCMNO
 250        CONTINUE
C
C ---      NOMBRE DE DDL ASSOCIE AU MATR_ELEM OU AU VECT_ELEM :
C          --------------------------------------------------
            NBINCO = ICO
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
            DO 280 IEL = 1, NBEL
C
                 DO 290 J = 1, NBINCO
                    ZK8(IDNOMC+J-1) = NOCMP(J)
 290             CONTINUE
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
               IF (.NOT.IMPLIG(NOMAIL,NBELEM,ZK8(IDLIMA))) GOTO 280
C
C ---         RECUPERATION DES CONNECTIVITES DE LA MAILLE :
C             -------------------------------------------
               CALL JEVEUO(JEXNUM(NOMA//'.CONNEX',NUMEL),'L',IDNOEU)
C
C ---         BOUCLE SUR LES CONNECTIVITES DE LA MAILLE :
C             -----------------------------------------
               K    = 0
               DO 300 INO = 1, NNOE
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
                   DO 310 ICMP = 1, NBCM
                     K = K + 1
C
C ---            AFFECTATION DU TABLEAU DU NOM DES NOEUDS :
C                ----------------------------------------
                     ZK8(IDNONO+K-1) = NOMNOE
                     NONOEU(K)       = NOMNOE
 310               CONTINUE
 300           CONTINUE
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
               IF (.NOT.IMPLIG(NOMAIL,NBELEM,ZK8(IDLIMA))) GOTO 280
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
               DO 320 INO = 1, NNOE
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
                    ZK8(IDNONO+1-1) = NOMNOE
                    NONOEU(K)       = NOMNOE
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
                   DO 330 ICMP = 1, NBCM
                     K = K + 1
C
C ---            AFFECTATION DU TABLEAU DU NOM DES NOEUDS :
C                ----------------------------------------
                     ZK8(IDNONO+K-1) = NOMNOE
                     NONOEU(K)       = NOMNOE
  330               CONTINUE
  320           CONTINUE
             ENDIF
C
C ---        RECONSTRUCTION DES TABLEAUX DE TRAVAIL  PAR FILTRAGE
C ---        EN FONCTION DES NOEUDS ET DES COMPOSANTES SPECIFIES
C ---        PAR L'UTILISATEUR :
C            -----------------
             CALL TRNOC1 (NBNOE, ZK8(IDLINO), NBCMP,
     +                    ZK8(IDLICM), OPTION, NBINCO, ZK8(IDNONO),
     +                    ZK8(IDNOMC), ZC(IDVALE), ZI(IDINCM), LONLI2)
C
             IF (LONLI2.EQ.0) GOTO 280
C
C ---        CONCATENATION DES NOMS DES NOEUDS ET DES NOMS DES
C ---        COMPOSANTES :
C            -----------
             LNOMCM = 0
             DO 340 I = 1, LONLI2
                NOMNCI(I) = ZK8(IDNONO+I-1)(1:LXLGUT(ZK8(IDNONO+I-1)))
     +                    //SLACH//ZK8(IDNOMC+I-1)
                LNOMCM = MAX(LNOMCM,LXLGUT(NOMNCI(I)))
340         CONTINUE
C
C ---        NOMBRE DE CHIFFRES SIGNIFICATIFS :
C            --------------------------------
             NBCH  = MIN (14, NBCHIF)
             NBCH1 = NBCH - 1
C
C ---        LONGUEUR PRISE POUR REPRESENTER UNE VALEUR REELLE :
C            -------------------------------------------------
             LVALRE = NBCH + 6
C
C ---        LONGUEUR PRISE POUR REPRESENTER UNE VALEUR COMPLEXE :
C            ---------------------------------------------------
             LVALCO = 2*LVALRE+3
C
C ---        FORMAT D'ECRITURE DES VALEURS REELLES :
C            -------------------------------------
             CALL CODENT(NBCH1,'G',KNBCH)
             CALL CODENT(LVALRE,'D',KLVALR)
C
             FORVAC = '"("'//'1PD'//KLVALR//'.'//KNBCH//'","'//
     +                '1PD'//KLVALR//'.'//KNBCH//'")"'
C
C ---        LONGUEUR D'UNE 'CASE' :
C            --------------------
             LONG = LVALCO
             CALL CODENT(LONG,'D',KLONG)
C
C ---        LONGUEUR D'UNE LIGNE :
C            --------------------
             LONLIG = LONLI2*(LONG+3)+19
             CALL CODENT(LONLIG,'D',KLONLG)
C
C ---        CODAGE DU NOMBRE DE TERMES DE LA LIGNE SOUS FORME D'UNE
C ---        CHAINE DE CARACTERES :
C            --------------------
             CALL CODENT(LONLI2,'D',KLONLI)
C
C ---        NOMBRE DE BLANCS DEVANT COMPLETER L'IMPRESSION
C ---        DE LA CHAINE DE CARACTERES (NOMNOE+NOMCMP) :
C            ------------------------------------------
             NBLAN2 = LVALCO -LNOMCM +1
             CALL CODENT(NBLAN2,'D',KBLAN1)
             CALL CODENT(LNOMCM,'D',KLONM)
C
             FORMC3 = '(2X,'//KLONLG//'("_")'//')'
             FORMC4 = '(2X,A1,A17,A1,'//
     +                   KLONLI//'(X,'//FORVAC//',X,A1))'
             FORMC2 = '(2X,A1,A8,9X,A1,'//KLONLI//'(X,A'//KLONM//','
     +                    //KBLAN1//'X,A1))'
             FORMC1 = '('//'2X,'//KLONLG//'("_")'//')'
C
C ---        IMPRESSION DU NOM DU LIGREL ET DU NOM DE L'OPTION :
C            -------------------------------------------------
             IMLIGR = IMLIGR + 1
             IF (IMLIGR.EQ.1) THEN
               WRITE(IFM,FORMA)
               WRITE(IFM,FORMB) NOLI(1:LGNOMS), NOMOPT(1:LGNOMO)
               WRITE(IFM,FORMC)
             ENDIF
C
C ---          IMPRESSION DU CADRE ET DU DESCRIPTIF
C ---          (NOMNOE+NOMCMP) DE L'ELEMENT :
C              ----------------------------
             WRITE(IFM,FORMC1)
             WRITE(IFM,FORMC2) EXCLAM, NOMAIL,EXCLAM,
     +                        (NOMNCI(I),EXCLAM,I=1,LONLI2)
             WRITE(IFM,FORMC3)
C
C================================================================
C ---        IMPRESSION PAR VALEUR                              =
C ---        RECONSTITUTION DE LA LIGNE COURANTE DU MATR_ELEM   =
C================================================================
C
C ---          CAS DES MATRICES NON_SYMETRIQUES :
C              --------------------------------
              IF (TYMAT.EQ.'N') THEN
C
C ---         BOUCLE SUR LE NOMBRE DE LIGNES :
C             ------------------------------
               DO 350 I = 1, NBINCO
C
                IF (IMPLIG(NOCMP(I),NBCMP,ZK8(IDLICM))) THEN
C
C ---           RECONSTITUTION DES LISTES DES NOMS DES COMPOSANTES
C ---           ASSOCIEES AUX NOEUDS DES ELEMENTS :
C               ---------------------------------
C
                 DO 360 J = 1, NBINCO
                    ZK8(IDNONO+J-1) = NONOEU(J)
                    ZK8(IDNOMC+J-1) = NOCMP(J)
 360             CONTINUE
C
C ---          INITIALISATION DU TABLEAU DES VALEURS DE LA LIGNE :
C              -------------------------------------------------
                 DO 370 J = 1, NBINCO
                   ZC(IDVALE+J-1) = ZEROC
 370             CONTINUE
C
C ---          POINTEUR DU DERNIER TERME TRAITE DE LA MATRICE :
C              ----------------------------------------------
                 NBTERM = (I-1)*NBINCO
C
C ---          BOUCLE SUR LES TERMES DE LA LIGNE COURANTE DE LA
C ---          MATRICE  :
C              -------
                 DO 380 J = 1, NBINCO
                    ZC(IDVALE+J-1) = ZC(IDRESL+NCMPEL*(IEL-1)
     +                                  + NBTERM + J -1)
 380             CONTINUE
C
C ---          RECONSTRUCTION DES TABLEAUX DE TRAVAIL  PAR FILTRAGE
C ---          EN FONCTION DES NOEUDS ET DES COMPOSANTES SPECIFIES
C ---          PAR L'UTILISATEUR :
C              -----------------
                 CALL TRNOC1 (NBNOE, ZK8(IDLINO), NBCMP,
     +                        ZK8(IDLICM), OPTION, NBINCO, ZK8(IDNONO),
     +                        ZK8(IDNOMC), ZC(IDVALE), ZI(IDINCM),
     +                        LONLI2)
C
C ---          IMPRESSION DE LA LIGNE COURANTE SELON LE GRAIN 'MAILLE':
C              ------------------------------------------------------
                 CALL IMPFC5 (NONOEU(I), NOCMP(I),
     +                        LONLI2, ZC(IDVALE), FORMC3, FORMC4,
     +                        IFM)
C
                ENDIF
 350          CONTINUE
C
C ---          CAS DES MATRICES SYMETRIQUES :
C              ----------------------------
             ELSEIF (TYMAT.EQ.'S') THEN
C
C ---         BOUCLE SUR LE NOMBRE DE LIGNES :
C             ------------------------------
               DO 390 I = 1, NBINCO
C
                IF (IMPLIG(NOCMP(I),NBCMP,ZK8(IDLICM))) THEN
C
C ---           RECONSTITUTION DES LISTES DES NOMS DES COMPOSANTES
C ---           ASSOCIEES AUX NOEUDS DES ELEMENTS :
C               ---------------------------------
C
                 DO 400 J = 1, NBINCO
                    ZK8(IDNONO+J-1) = NONOEU(J)
                    ZK8(IDNOMC+J-1) = NOCMP(J)
 400             CONTINUE
C
C ---          INITIALISATION DU TABLEAU DES VALEURS DE LA LIGNE :
C              -------------------------------------------------
                 DO 410 J = 1, NBINCO
                   ZC(IDVALE+J-1) = ZEROC
 410             CONTINUE
C
C ---          POINTEUR DU DERNIER TERME TRAITE DE LA DEMI-MATRICE :
C              ---------------------------------------------------
                 NBTERM = (I*(I-1))/2
C
C ---          BOUCLE SUR LES TERMES DE LA LIGNE COURANTE DE LA
C ---          DEMI-MATRICE INFERIEURE :
C              -----------------------
                 DO 420 J = 1, I
                    ZC(IDVALE+J-1) = ZC(IDRESL+NCMPEL*(IEL-1)
     +                                  + NBTERM + J -1)
 420             CONTINUE
C
C ---          BOUCLE SUR LES TERMES DE LA COLONNE I SITUES
C ---          EN DESSOUS DU TERME DIAGONAL :
C              ----------------------------
                 DO 430 J = I+1, NBINCO
C
C ---            POINTEUR DU TERME DIAGONAL PRECEDANT LA LIGNE
C ---            COURANTE DANS LA DEMI-MATRICE :
C                -----------------------------
                    NBTERM = (J*(J-1))/2
C
                    ZC(IDVALE+J-1) = ZC(IDRESL+NCMPEL*(IEL-1)
     +                                  + NBTERM + I -1)
 430            CONTINUE
C
C ---          RECONSTRUCTION DES TABLEAUX DE TRAVAIL  PAR FILTRAGE
C ---          EN FONCTION DES NOEUDS ET DES COMPOSANTES SPECIFIES
C ---          PAR L'UTILISATEUR :
C              -----------------
                 CALL TRNOC1 (NBNOE, ZK8(IDLINO), NBCMP,
     +                        ZK8(IDLICM), OPTION, NBINCO, ZK8(IDNONO),
     +                        ZK8(IDNOMC), ZC(IDVALE), ZI(IDINCM),
     +                        LONLI2)
C
C ---          IMPRESSION DE LA LIGNE COURANTE SELON LE GRAIN 'MAILLE':
C              ------------------------------------------------------
                 CALL IMPFC5 (NONOEU(I), NOCMP(I),
     +                        LONLI2, ZC(IDVALE), FORMC3, FORMC4,
     +                        IFM)
C
                ENDIF
 390          CONTINUE
             ENDIF
 280        CONTINUE
          ENDIF
 230     CONTINUE
         CALL JEDETC('V','&&IMELNM1',1)
        ELSE
          CALL UTMESS('F','IMELNM','LES SEULS TYPES DE VALEURS ACCEPTES'
     +                //' POUR LES RESU_ELEM SONT LES REELS ET LES'
     +                //' COMPLEXES, LE DESCRIPTEUR DE TYPE '//TYPVAL
     +                //' EST INADEQUAT.')
        ENDIF
 10   CONTINUE
C
      CALL JEDETC('V','&&IMELNM',1)
C
      CALL JEDEMA()
C
      END

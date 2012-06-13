      SUBROUTINE JNI080(ELREFE,NMAXOB,LIOBJ,NBOBJ)
      IMPLICIT REAL*8 (A-H,O-Z)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE VABHHTS J.PELLET
      INCLUDE 'jeveux.h'
      CHARACTER*8 ELREFE
      INTEGER NMAXOB,NBOBJ
      CHARACTER*24 LIOBJ(NMAXOB)

C TOLE CRP_20
C.......................................................................

C BUT :  ROUTINE D'INITIALISATION DES ELEMENTS COQUE (MEC3QU9H,MEC3TR7H)
C.......................................................................

C    ON CONSTRUIT 3 TABLEAUX POUR CHAQUE ELEMENT (MEC3QU9H,MEC3TR7H)

C    DESI='&INEL.'//ELREFE//'.DESI' CONTIENT :
C                MEC3QU9H    MEC3TR7H
C    NBN1        8           6
C    NBN2        9           7
C    NPGSR       4           3
C    NPGSN       9           7

C   DESR='&INEL.'//ELREFE//'.DESR' (LONGUEUR 2100) CONTIENT :
C ======================================================================
C OBJET  DESR                     NBVAL    POINTEUR   NBVAL    POINTEUR
C                               MEC3QU9H   Q9        MEC3TR7H     T7
C ======================================================================
C INTEGRATION REDUITE
C COORDONNEE X POINTS DE GAUSS      4                 3
C COORDONNEE Y POINTS DE GAUSS      4                 3
C POIDS DE GAUSS                    4                 3
C ----------------------------------------------------------------------
C FONCTIONS DE FORME SERENDIP (NBN1) PT GAUSS REDUITS (NPSGR)
C POINTEURS CI-DESSOUS UTILISEES DANS : BTDMSR, JM1DN1, JM1DN2 (VALFOR),
C VECTGT,
C                                          12                  12
C ----------------------------------------------------------------------
C NI (PG1) I=1,NBN1                 8                 6
C ...                               ...               ...
C NI (PGN) I=1,NBN1                 8                 6
C                                         44                    44
C DNI/DKSI I=1,NBN1                 8                 6
C ...                               ...               ...
C DNI/DKSI I=1,NBN1                 8                 6
C                                         76                    76
C DNI/DETA I=1,NBN1                 8                 6
C ...                               ...               ...
C DNI/DETA I=1,NBN1                 8                 6
C      NOMBRE TOTAL 3*NPGSR*NBN1   96     108        54         63
C ----------------------------------------------------------------------
C DEFINITION POINTS DE GAUSS COMPLETS (NPGSN)
C POINTEURS CI-DESSOUS UTILISEES DANS : FORNGR, FORCEN, FORPES, FORSRG
C HSJ1F TE0402 TE0406 TE0417 TE0486 VDGNLR, VDPNLR
C                                         108                  108
C ----------------------------------------------------------------------
C INTEGRATION COMPLETE
C COORDONNEE X POINTS DE GAUSS      9                 7
C COORDONNEE Y POINTS DE GAUSS      9                 7
C                                         126                  126
C POIDS DE GAUSS                    9                 7
C                                  27     135        21        130
C ----------------------------------------------------------------------
C FONCTIONS DE FORME SERENDIP (NBN1) PT GAUSS COMPLETS (NPSGN)
C POINTEURS CI-DESSOUS UTILISEES DANS : FORCEN, FORPES, FORSRG, MATRN
C  TE0417, JM1DN1, JM1DN2 (VALFOR), VECTGT, VECTCI
C                                         135                  135
C ----------------------------------------------------------------------
C NI (PG1) I=1,NBN1                 8                 6
C ...                               ...               ...
C NI (PGN) I=1,NBN1                 8                 6
C                                         207                  207
C DNI/DKSI I=1,NBN1                 8                 6
C ...                               ...               ...
C DNI/DKSI I=1,NBN1                 8                 6
C                                         279                  279
C DNI/DETA I=1,NBN1                 8                 6
C ...                               ...               ...
C DNI/DETA I=1,NBN1                 8                 6
C      NOMBRE TOTAL 3*NPGSN*NBN1    216   351        126       261
C ----------------------------------------------------------------------
C FONCTIONS DE FORME LAGRANGE (NBN2) PT GAUSS REDUITS (NPSGR)
C POINTEURS CI-DESSOUS UTILISEES DANS : JM1DN1, JM1DN2 (VALFOR)
C                                         351                  351
C ----------------------------------------------------------------------
C NI (PG1) I=1,NBN2                 9                 7
C ...                               ...               ...
C NI (PGN) I=1,NBN2                 9                 7
C                                         387                  387
C DNI/DKSI I=1,NBN2                 9                 7
C ...                               ...               ...
C DNI/DKSI I=1,NBN2                 9                 7
C                                         423                  423
C DNI/DETA I=1,NBN2                 9                 7
C ...                               ...               ...
C DNI/DETA I=1,NBN2                 9                 7
C  NOMBRE TOTAL 3*NPGSR*NBN2      108    459         63        414
C ----------------------------------------------------------------------
C FONCTIONS DE FORME LAGRANGE (NBN2) PT GAUSS COMPLETS (NPSGN)
C POINTEURS CI-DESSOUS UTILISEES DANS : FORSRG, TE0486,
C JM1DN1, JM1DN2 (VALFOR), VDXNLR, MATRN, JM1DN3, VDGNLLR, VDPNLR
C                                        459                  459
C ----------------------------------------------------------------------
C NI (PG1) I=1,NBN2                 9                 7
C ...                               ...               ...
C NI (PGN) I=1,NBN2                 9  NPGSN*NBN2
C                                         540                  540
C DNI/DKSI I=1,NBN2                 9
C ...                               ...               ...
C DNI/DKSI I=1,NBN2                 9  NPGSN*NBN2
C                                         621                  621
C DNI/DETA I=1,NBN2                 9
C ...                               ...               ...
C DNI/DETA I=1,NBN2                 9  NPGSN*NBN2
C   NOMBRE TOTAL 3*NPGSN*NBN2      243     702        147      606
C ----------------------------------------------------------------------
C FONCTIONS DE FORME REDUITES (LINEAIRES SUR ELEMENT DONT LES SOMMETS
C SONT LES POINTS DE GAUSS REDUITS) EVALUEES AUX PT GAUSS COMPLETS
C POINTEURS CI-DESSOUS UTILISEES DANS :  MATBSU, BTDMSN, VDGNLR, VDPNLR
C                                          702                  702
C ----------------------------------------------------------------------
C NI (PG1) I=1,NPGSR                4
C ...                               ...               ...
C NI (PGN) I=1,NPGSR                4
C DNI/DKSI I=1,NPGSR                 4
C ...                               ...               ...
C DNI/DKSI I=1,NPGSR                 4
C DNI/DETA I=1,NPGSR                 4
C ...                               ...               ...
C DNI/DETA I=1,NPGSR                 4
C   NOMBRE TOTAL 3*NPGSN*NBN2      108     810
C ----------------------------------------------------------------------
C COORDONNEES DES NBN2 NOEUDS
C POINTEURS CI-DESSOUS UTILISEES DANS :
C                                          810                 810
C ----------------------------------------------------------------------
C COORDONNES DES NOEUDS X           9                  7
C COORDONNES DES NOEUDS Y           9                  7
C   NOMBRE TOTAL 3*NPGSN*NBN2      18     828          14      824
C ----------------------------------------------------------------------
C DERIVEES DES NBN1 FONCTIONS DE FORMES AUX NBN2 NOEUDS
C POINTEURS CI-DESSOUS UTILISEES DANS : VECTAN
C                                         828                 828
C ----------------------------------------------------------------------
C DNI/DKSI I=1,NBN2                 9                 7
C ...                               ...               ...
C DNI/DKSI I=1,NBN2                 9                 7
C                                         900                  900
C DNI/DETA I=1,NBN2                 9                 7
C ...                               ...               ...
C DNI/DETA I=1,NBN2                 9                 7
C   NOMBRE TOTAL 2*NBN1 *NBN2     144     972        84        912
C ----------------------------------------------------------------------
C ZONE DE TRAVAIL LONGUEUR 10*NB1 POINTEUR             1000
C UTILISEE DANS FPRES, FSURF
C                                                      1080
C ----------------------------------------------------------------------
C ZONE DE TRAVAIL LONGUEUR 9*NB2 POINTEUR             1090
C UTILISEE DANS VDREPE, VECTAN
C ----------------------------------------------------------------------
C ZONE DE TRAVAIL LONGUEUR 9     POINTEUR             1180
C UTILISEE DANS HSJ1F
C ----------------------------------------------------------------------
C ESPACE VIDE DANS DESR JUSQU'A :        1250                 1250
C ----------------------------------------------------------------------
C COORDONNES DES 3 POINTS DANS L'EPAISSEUR  1253              1253
C ----------------------------------------------------------------------
C POUR CHAQUE POINT DANS L'EPAISSEUR
C VALEURS DES NPGSN-1 FONCTIONS D'INTERPOLATION AUX NBN1 NOEUDS
C CES FONCTIONS SONT DEFINIES SUR UN ELEMENT DE REFERENCE
C CONSTRUIT SUR LES NPGSN -1 POINTS DE GAUSS
C POINTEURS CI-DESSOUS UTILISEES DANS : VDESNG
C                                         1260                 1260
C ----------------------------------------------------------------------
C  NOMBRE TOTAL 3*NBN1*(NPGSN-1)  192     1452     108        1368
C ----------------------------------------------------------------------
C VALEURS DES NPGSR FONCTIONS D'INTERPOLATION AUX NBN1 NOEUDS
C CES FONCTIONS SONT DEFINIES SUR UN ELEMENT DE REFERENCE
C CONSTRUIT SUR LES NPGSR POINTS DE GAUSS      NBN1*(NPGSR)
C POINTEURS CI-DESSOUS UTILISEES DANS : VDEFGE
C                                        1452                 1452
C ----------------------------------------------------------------------
C  NOMBRE TOTAL 3*NBN1*(NPGSR)    32     1484     18          1470
C ----------------------------------------------------------------------
C DANS FCEPAIS, POIDS ET POINTS DE GAUSS DANS L'EPAISSEUR
C POINTEURS CI-DESSOUS UTILISEES DANS :
C                                        1500                 1500
C ----------------------------------------------------------------------
C COORDONNES ET POIDS                     6                    6
C 3 VALEURS POUR CHACUN DES 3 POINTS      9                    9
C ESPACE VIDE DANS DESR A PARTIR DE      1515                 1515
C ----------------------------------------------------------------------
C ZONE DE TRAVAIL LONGUEUR 1 POINTEUR             1550
C UTILISEE DANS CAURTG,FORNRG, PK2GAU,VDGNLR, VDPNLR, VDXRIG, VDXNLR
C ----------------------------------------------------------------------
C ZONE DE TRAVAIL LONGUEUR 9*NPGSR  POINTEUR        2000
C UTILISEE DANS TE0415, VDXSIG
C ----------------------------------------------------------------------
C ESPACE VIDE DANS DESR JUSQU'A :        2100                 2100
C ----------------------------------------------------------------------

C ======================================================================
C LISTE DES ROUTINES COQUE3D UTILISANT LES .DESR
C=======================================================================

C BSTHCO UTILISE ZR(459)
C BTDFN  IND=1 L=459 + IND=0 L=351 FF LAGRANGE PG REDUIT
C BTDMSN INTERGRATION REDUITE L=702 FF REDUITES SUR PT GAUSS NORMAUX
C BTDMSR DERIVEES FF SERENDIP PG REDUITS L=44,76 FF ET
C BTDMSR DERIVEES LAGRANGE PG REDUITS L=351, 387, 423
C BTLDTH INDIC=0 L=351 FF LAGRANGE PG REDUITS
C BTLDTH INDIC=1 L=459 FF LAGRANGE PG COMPLETS
C CAURTG ZONE TRAVAIL ZR(1550) L=1
C FORCEN L=127 POIDS PG COMPLETS  L=135 FF SERENDIP PG COMPLETS
C FORNGR ZONE TRAVAIL ZR(1550) BTSIG(ZR(127...=POIDS)
C FORPES L=127 POIDS PG COMPLETS  L=135 FF SERENDIP PG COMPLETS
C FORSRG L=127 POIDS PG COMPLETS  L=135 FF SERENDIP PG COMPLETS
C FORSRG L=459 FF LAGRANGE PG COMPLET
C FPRES  ZONE TRAVAIL DE 1000 A 1000+10*NB1 = 1080
C FSURF  ZONE TRAVAIL DE 1000 A 1000+10*NB1 1080
C HSJ1F  L=127 POIDS PG COMPLETS ZONE TRAVAIL XR(1180) LONGUEUR 9
C JM1DN1 UTILISE VALFOR
C JM1DN2   UTILISE VALFOR
C JM1DN3  L=459, 540, 621 FF ET DERIVEES LAGRANGE PG COMPLETS
C MATBSU L=702 L=702 FF REDUITES SUR PT GAUSS NORMAUX
C MATRN  L=135 FF SERENDIP PG COMPLET + L=459 FF LAGRANGE PG COMPLET
C PK2CAU ZONE TRAVAIL ZR(1550)
C TE0402 RIGI_MECA_GE UTILISE ZR(127...=POIDS) BTSIG(ZR(127...=POIDS)
C TE0406 INTEGRATION NUMERIQUE ZR(127)
C TE0415 SIEF_ELNO ZONE TRAVAIL ZR(2000) LONGUEUR 9*NBGSR
C TE0417 UTILISE ZR(127...) ZR(135..)
C TE0419 CHAR_MECA_TEMP_R
C TE0486 UTILISE ZR(459) B1TDB2(ZR(127)
C VALFOR INDN=0 => LT1=44  LT2=76 DERIVEES DES FF SERENDIP PG RESDUITS
C VALFOR INDN=1 => LT1=207 LT2=279 DERIVEES DES FF SERENDIP PG COMPLETS
C VALFOR L1=351 L2=387 L3=423, FF ET DERIVEES LAGRANGE PG REDUITS
C VALFOR L1=459 L2=540 L3=621, FF ET DERIVEES LAGRANGE PG COMPLETS
C VDEFGE L=1452 FF BASEES SUR NPGSR EVALUEES AUX NPG1 NOEUDS
C VDESND Q9 L=1260 FF BASEES SUR NPGSN EVALUEES AUX NPG1 NOEUDS
C VDESND T7 L=1260 FF BASEES SUR NPGSN EVALUEES AUX NPG1 NOEUDS
C VDGNLR ZONE TRAVAIL ZR(1550)  ZR(459) BTSIG(ZR(127)) ZR(702)
C VDPNLR ZONE TRAVAIL ZR(1550)  ZR(459) BTSIG(ZR(127)) ZR(702)
C VDREPE ZONE TRAVAIL ZR(1090) MATRICE PASSAGE  LONGUEUR 9*NB2
C VDXNLR UTILISE ZR(459) ZR(LZR-1+1550) = COEF
C VDXRIG ZR(LZR-1+1550) = COEF
C VDXSIG ZONE TRAVAIL  ZR(2000) LONGUEUR 9*NPGSR
C VECTAN L=1090 ZONE TRAVAIL  MATRICES DE PASSAGE LONGUEUR 9*NB2
C VECTAN L=828, 900 DERIVEES DES NBN1 FF EVALUEES AUX NBN2 NOEUDS
C VECTCI L=207, 279 DERIVEESS FF SERENDIP PG COMPLETS
C VECTGT IND=0 L=12, 44, 76  FF SERENDIP ET DERIVESS PG REDUITS
C VECTGT IND=1 L=135, 207, 279  FF SERENDIP ET DERIVESS PG COMPLETS
C ======================================================================

C CHANTIER ELREFE : QUE FAIRE ?
C  - IL FAUDRAIT VIRER FCEPAI, QUI APPAREMMENT N'EST PAS UTILISE
C  - SI ON VEUT DIMINUER LE NOMBRE DE FF, SUPPRIMER LES FF 1260 ET 1452
C    REMPLACER DANS VDEFGE ET VDESNR L'EXTRAPOLATION AUX NOEUDS ACTUELLE
C  PAR UN APPEL A PPGANO. LE PB C'EST QUE LA MATRICE DE PASSAGE ACTUELLE
C  EST CONSTRUITE SUR LES 3 POINTS D'INTÈGRATION PAR COUCHE UTILISÈS EN
C  NON LINÈAIRE, ALORS QU'EN LINÈAIRE, IL N'Y A QUE 2 POINTS.
C  IL FAUDRAIT DONC SOIT METTRE 3 POINTS EN LINÈAIRE, SOIT CHANGER LE
C  PASSAGE GAUSS NOEUDS EN NON LINÈAIRE ET EN LINÈAIRE.
C  - IL FAUDRAIT REMPLACER LES ZONES DE TRAVAIL PAR DES PASSAGES
C  D'ARGUMENTS
C  - ENSUITE ON POURRAIT PEUT ETE UTILISER DES ELREFE...
C ======================================================================
      CHARACTER*16 ELREFL
      CHARACTER*24 DESI,DESR
      INTEGER NNO,IRET,NDIM,NBPG,NPGCOU,NPGSN,NPGSR,NSO
      INTEGER LZI,LZR,I1,I2,I3,I4,I5,K,L,LL,M,NBN1,NBN2,KOMPT
      INTEGER JMAS,LDESI,LDESR,LJMAS
      REAL*8 A,B,AA
      REAL*8 VFESND(45)
      REAL*8 XPG(81),POIPG(27),X(2),FF(9),DFF(3,9),XI1,XI2,XI3

C DEB ------------------------------------------------------------------

      NBOBJ = 3
      CALL ASSERT(NMAXOB.GT.NBOBJ)
      LIOBJ(1) = '&INEL.'//ELREFE//'.DESI'
      LIOBJ(2) = '&INEL.'//ELREFE//'.DESR'
      ELREFL=ELREFE
      LIOBJ(3) = '&INEL.'//ELREFL//'.B'

      DESI = LIOBJ(1)
      DESR = LIOBJ(2)

      NPGCOU = 3
      IF (ELREFE.EQ.'MEC3QU9H') THEN
        NBN1 = 8
        NBN2 = 9
        NPGSR = 4
        NPGSN = 9
        NSO = 4
      ELSE IF (ELREFE.EQ.'MEC3TR7H'  ) THEN
        NBN1 = 6
        NBN2 = 7
        NPGSR = 3
        NPGSN = 7
        NSO = 3
      ELSE
        CALL U2MESS('F','ELEMENTS2_31')
      END IF

      CALL JEEXIN(DESI,IRET)
      IF (IRET.NE.0) GO TO 350

      LDESI = 4
      CALL WKVECT(DESI,'V V I',LDESI,LZI)
      ZI(LZI-1+1) = NBN1
      ZI(LZI-1+2) = NBN2
      ZI(LZI-1+3) = NPGSR
      ZI(LZI-1+4) = NPGSN

      LDESR = 2100
      CALL WKVECT(DESR,'V V R',LDESR,LZR)
      ZR(LZR-1+1550) = R8NNEM()
C
      IF (ELREFE.EQ.'MEC3QU9H') THEN

C     DEFINITION DES 4=2*2 PTS DE GAUSS REDUIT ET DES POIDS
C     CORRESPONDANTS
        CALL ELRAGA('QU9','FPG4    ',NDIM,NBPG,XPG,POIPG)

        ZR(LZR-1+1) = XPG(1)
        ZR(LZR-1+2) = XPG(3)
        ZR(LZR-1+3) = XPG(5)
        ZR(LZR-1+4) = XPG(7)

        ZR(LZR-1+5) = XPG(2)
        ZR(LZR-1+6) = XPG(4)
        ZR(LZR-1+7) = XPG(6)
        ZR(LZR-1+8) = XPG(8)

        ZR(LZR-1+9) = POIPG(1)
        ZR(LZR-1+10) = POIPG(2)
        ZR(LZR-1+11) = POIPG(3)
        ZR(LZR-1+12) = POIPG(4)

C     FONCTIONS SERENDIP - POUR LES TERMES DE
C     TRANSLATION
C
C     VALEURS DES FONCTIONS DE FORME ET DE
C     LEURS DERIVEES AUX 4 PTS DE GAUSS REDUITS

        DO 20 L = 1,4
          I1 = L
          I2 = 4 + L
          X(1) = ZR(LZR-1+I1)
          X(2) = ZR(LZR-1+I2)
          CALL ELRFVF('QU8',X,9,FF,NNO)
          CALL ELRFDF('QU8',X,18,DFF,NNO,NDIM)
          LL = 8* (L-1)
          ZR(LZR-1+ 12 + LL + 1) = FF(4)
          ZR(LZR-1+ 44 + LL + 1) = DFF(1,4)
          ZR(LZR-1+ 76 + LL + 1) = DFF(2,4)
          ZR(LZR-1+ 12 + LL + 2) = FF(1)
          ZR(LZR-1+ 44 + LL + 2) = DFF(1,1)
          ZR(LZR-1+ 76 + LL + 2) = DFF(2,1)
          ZR(LZR-1+ 12 + LL + 3) = FF(2)
          ZR(LZR-1+ 44 + LL + 3) = DFF(1,2)
          ZR(LZR-1+ 76 + LL + 3) = DFF(2,2)
          ZR(LZR-1+ 12 + LL + 4) = FF(3)
          ZR(LZR-1+ 44 + LL + 4) = DFF(1,3)
          ZR(LZR-1+ 76 + LL + 4) = DFF(2,3)
          ZR(LZR-1+ 12 + LL + 5) = FF(8)
          ZR(LZR-1+ 44 + LL + 5) = DFF(1,8)
          ZR(LZR-1+ 76 + LL + 5) = DFF(2,8)
          ZR(LZR-1+ 12 + LL + 6) = FF(5)
          ZR(LZR-1+ 44 + LL + 6) = DFF(1,5)
          ZR(LZR-1+ 76 + LL + 6) = DFF(2,5)
          ZR(LZR-1+ 12 + LL + 7) = FF(6)
          ZR(LZR-1+ 44 + LL + 7) = DFF(1,6)
          ZR(LZR-1+ 76 + LL + 7) = DFF(2,6)
          ZR(LZR-1+ 12 + LL + 8) = FF(7)
          ZR(LZR-1+ 44 + LL + 8) = DFF(1,7)
          ZR(LZR-1+ 76 + LL + 8) = DFF(2,7)
   20   CONTINUE

C     DEFINITION DES 9=3*3 PTS DE GAUSS NORMAL
C     POSITION DES POINTS DE GAUSS DANS LE PLAN
C     ET VALEUR DES POIDS ASSOCIES
        CALL ELRAGA('QU8','FPG9    ',NDIM,NBPG,XPG,POIPG)

        ZR(LZR-1+109) = XPG(1)
        ZR(LZR-1+110) = XPG(9)
        ZR(LZR-1+111) = XPG(3)
        ZR(LZR-1+112) = XPG(11)
        ZR(LZR-1+113) = XPG(5)
        ZR(LZR-1+114) = XPG(13)
        ZR(LZR-1+115) = XPG(7)
        ZR(LZR-1+116) = XPG(15)
        ZR(LZR-1+117) = XPG(17)

        ZR(LZR-1+118) = XPG(2)
        ZR(LZR-1+119) = XPG(10)
        ZR(LZR-1+120) = XPG(4)
        ZR(LZR-1+121) = XPG(12)
        ZR(LZR-1+122) = XPG(6)
        ZR(LZR-1+123) = XPG(14)
        ZR(LZR-1+124) = XPG(8)
        ZR(LZR-1+125) = XPG(16)
        ZR(LZR-1+126) = XPG(18)

C     VALEUR DES POIDS ASSOCIES

        ZR(LZR-1+127) = POIPG(1)
        ZR(LZR-1+128) = POIPG(5)
        ZR(LZR-1+129) = POIPG(2)
        ZR(LZR-1+130) = POIPG(6)
        ZR(LZR-1+131) = POIPG(3)
        ZR(LZR-1+132) = POIPG(7)
        ZR(LZR-1+133) = POIPG(4)
        ZR(LZR-1+134) = POIPG(8)
        ZR(LZR-1+135) = POIPG(9)

C     VALEURS DES FONCTIONS DE FORME ET
C     DERVIVEES AUX 9 PTS DE GAUSS NORMAL

        DO 40 L = 1,9
          I1 = 108 + L
          I2 = 108 + 9 + L
          X(1) = ZR(LZR-1+I1)
          X(2) = ZR(LZR-1+I2)
          CALL ELRFVF('QU8',X,9,FF,NNO)
          CALL ELRFDF('QU8',X,18,DFF,NNO,NDIM)
          LL = 8* (L-1)
          ZR(LZR-1+ 135 + LL + 1) = FF(4)
          ZR(LZR-1+ 207 + LL + 1) = DFF(1,4)
          ZR(LZR-1+ 279 + LL + 1) = DFF(2,4)
          ZR(LZR-1+ 135 + LL + 2) = FF(1)
          ZR(LZR-1+ 207 + LL + 2) = DFF(1,1)
          ZR(LZR-1+ 279 + LL + 2) = DFF(2,1)
          ZR(LZR-1+ 135 + LL + 3) = FF(2)
          ZR(LZR-1+ 207 + LL + 3) = DFF(1,2)
          ZR(LZR-1+ 279 + LL + 3) = DFF(2,2)
          ZR(LZR-1+ 135 + LL + 4) = FF(3)
          ZR(LZR-1+ 207 + LL + 4) = DFF(1,3)
          ZR(LZR-1+ 279 + LL + 4) = DFF(2,3)
          ZR(LZR-1+ 135 + LL + 5) = FF(8)
          ZR(LZR-1+ 207 + LL + 5) = DFF(1,8)
          ZR(LZR-1+ 279 + LL + 5) = DFF(2,8)
          ZR(LZR-1+ 135 + LL + 6) = FF(5)
          ZR(LZR-1+ 207 + LL + 6) = DFF(1,5)
          ZR(LZR-1+ 279 + LL + 6) = DFF(2,5)
          ZR(LZR-1+ 135 + LL + 7) = FF(6)
          ZR(LZR-1+ 207 + LL + 7) = DFF(1,6)
          ZR(LZR-1+ 279 + LL + 7) = DFF(2,6)
          ZR(LZR-1+ 135 + LL + 8) = FF(7)
          ZR(LZR-1+ 207 + LL + 8) = DFF(1,7)
          ZR(LZR-1+ 279 + LL + 8) = DFF(2,7)
   40   CONTINUE

C     FONCTIONS DE LAGRANGE - POUR LES TERMES
C     DE ROTATION
C
C     VALEURS AUX 4 PTS DE GAUSS REDUITS
C     DES FONCTIONS DE FORME ET DE LEURS
C     DERIVEES

        DO 60 L = 1,4
          I1 = L
          I2 = 4 + L
          X(1) = ZR(LZR-1+I1)
          X(2) = ZR(LZR-1+I2)
          CALL ELRFVF('QU9',X,9,FF,NNO)
          CALL ELRFDF('QU9',X,18,DFF,NNO,NDIM)
          LL = 9* (L-1)
          ZR(LZR-1+ 351 + LL + 1) = FF(4)
          ZR(LZR-1+ 387 + LL + 1) = DFF(1,4)
          ZR(LZR-1+ 423 + LL + 1) = DFF(2,4)
          ZR(LZR-1+ 351 + LL + 2) = FF(1)
          ZR(LZR-1+ 387 + LL + 2) = DFF(1,1)
          ZR(LZR-1+ 423 + LL + 2) = DFF(2,1)
          ZR(LZR-1+ 351 + LL + 3) = FF(2)
          ZR(LZR-1+ 387 + LL + 3) = DFF(1,2)
          ZR(LZR-1+ 423 + LL + 3) = DFF(2,2)
          ZR(LZR-1+ 351 + LL + 4) = FF(3)
          ZR(LZR-1+ 387 + LL + 4) = DFF(1,3)
          ZR(LZR-1+ 423 + LL + 4) = DFF(2,3)
          ZR(LZR-1+ 351 + LL + 5) = FF(8)
          ZR(LZR-1+ 387 + LL + 5) = DFF(1,8)
          ZR(LZR-1+ 423 + LL + 5) = DFF(2,8)
          ZR(LZR-1+ 351 + LL + 6) = FF(5)
          ZR(LZR-1+ 387 + LL + 6) = DFF(1,5)
          ZR(LZR-1+ 423 + LL + 6) = DFF(2,5)
          ZR(LZR-1+ 351 + LL + 7) = FF(6)
          ZR(LZR-1+ 387 + LL + 7) = DFF(1,6)
          ZR(LZR-1+ 423 + LL + 7) = DFF(2,6)
          ZR(LZR-1+ 351 + LL + 8) = FF(7)
          ZR(LZR-1+ 387 + LL + 8) = DFF(1,7)
          ZR(LZR-1+ 423 + LL + 8) = DFF(2,7)
          ZR(LZR-1+ 351 + LL + 9) = FF(9)
          ZR(LZR-1+ 387 + LL + 9) = DFF(1,9)
          ZR(LZR-1+ 423 + LL + 9) = DFF(2,9)
   60   CONTINUE

C    VALEURS AUX 9 PTS DE GAUSS NORMAL
C    DES FONCTIONS DE FORME ET DE LEURS
C    DERIVEES

        DO 80 L = 1,9
          I1 = 108 + L
          I2 = 108 + 9 + L
          X(1) = ZR(LZR-1+I1)
          X(2) = ZR(LZR-1+I2)
          CALL ELRFVF('QU9',X,9,FF,NNO)
          CALL ELRFDF('QU9',X,18,DFF,NNO,NDIM)
          LL = 9* (L-1)
          ZR(LZR-1+ 459 + LL + 1) = FF(4)
          ZR(LZR-1+ 540 + LL + 1) = DFF(1,4)
          ZR(LZR-1+ 621 + LL + 1) = DFF(2,4)
          ZR(LZR-1+ 459 + LL + 2) = FF(1)
          ZR(LZR-1+ 540 + LL + 2) = DFF(1,1)
          ZR(LZR-1+ 621 + LL + 2) = DFF(2,1)
          ZR(LZR-1+ 459 + LL + 3) = FF(2)
          ZR(LZR-1+ 540 + LL + 3) = DFF(1,2)
          ZR(LZR-1+ 621 + LL + 3) = DFF(2,2)
          ZR(LZR-1+ 459 + LL + 4) = FF(3)
          ZR(LZR-1+ 540 + LL + 4) = DFF(1,3)
          ZR(LZR-1+ 621 + LL + 4) = DFF(2,3)
          ZR(LZR-1+ 459 + LL + 5) = FF(8)
          ZR(LZR-1+ 540 + LL + 5) = DFF(1,8)
          ZR(LZR-1+ 621 + LL + 5) = DFF(2,8)
          ZR(LZR-1+ 459 + LL + 6) = FF(5)
          ZR(LZR-1+ 540 + LL + 6) = DFF(1,5)
          ZR(LZR-1+ 621 + LL + 6) = DFF(2,5)
          ZR(LZR-1+ 459 + LL + 7) = FF(6)
          ZR(LZR-1+ 540 + LL + 7) = DFF(1,6)
          ZR(LZR-1+ 621 + LL + 7) = DFF(2,6)
          ZR(LZR-1+ 459 + LL + 8) = FF(7)
          ZR(LZR-1+ 540 + LL + 8) = DFF(1,7)
          ZR(LZR-1+ 621 + LL + 8) = DFF(2,7)
          ZR(LZR-1+ 459 + LL + 9) = FF(9)
          ZR(LZR-1+ 540 + LL + 9) = DFF(1,9)
          ZR(LZR-1+ 621 + LL + 9) = DFF(2,9)
   80   CONTINUE

C    FONCTIONS ASSOCIEES AUX 4 PTS DE GAUSS REDUITS

C    VALEURS AUX 9 PTS DE GAUSS NORMAL

        DO 100 L = 1,9
          I1 = 108 + L
          I2 = 108 + 9 + L
          X(1) = ZR(LZR-1+I1)
          X(2) = ZR(LZR-1+I2)
          AA=0.577350269189626D0
          FF(1)= 0.75D0*(AA-X(1))*(AA-X(2))
          FF(2)= 0.75D0*(AA+X(1))*(AA-X(2))
          FF(3)= 0.75D0*(AA+X(1))*(AA+X(2))
          FF(4)= 0.75D0*(AA-X(1))*(AA+X(2))
          DFF(1,1)=-0.75D0*(AA-X(2))
          DFF(1,2)= 0.75D0*(AA-X(2))
          DFF(1,3)= 0.75D0*(AA+X(2))
          DFF(1,4)=-0.75D0*(AA+X(2))
          DFF(2,1)=-0.75D0*(AA-X(1))
          DFF(2,2)=-0.75D0*(AA+X(1))
          DFF(2,3)= 0.75D0*(AA+X(1))
          DFF(2,4)= 0.75D0*(AA-X(1))
          LL = 4* (L-1)
          DO 90 L1 = 1,4
            I3 = 702 + LL + L1
            I4 = 738 + LL + L1
            I5 = 774 + LL + L1
            ZR(LZR-1+I3) = FF(L1)
            ZR(LZR-1+I4) = DFF(1,L1)
            ZR(LZR-1+I5) = DFF(2,L1)
   90     CONTINUE
  100   CONTINUE

C     VALEURS DES FONCTIONS DE SERENDIP AUX 9 NOEUDS
C     ON DONNE LA POSITION DES NOEUDS

        ZR(LZR-1+811) = -1.D0
        ZR(LZR-1+812) = -1.D0
        ZR(LZR-1+813) = 1.D0
        ZR(LZR-1+814) = 1.D0
        ZR(LZR-1+815) = -1.D0
        ZR(LZR-1+816) = 0.D0
        ZR(LZR-1+817) = 1.D0
        ZR(LZR-1+818) = 0.D0
        ZR(LZR-1+819) = 0.D0

        ZR(LZR-1+820) = 1.D0
        ZR(LZR-1+821) = -1.D0
        ZR(LZR-1+822) = -1.D0
        ZR(LZR-1+823) = 1.D0
        ZR(LZR-1+824) = 0.D0
        ZR(LZR-1+825) = -1.D0
        ZR(LZR-1+826) = 0.D0
        ZR(LZR-1+827) = 1.D0
        ZR(LZR-1+828) = 0.D0

C     VALEURS DES FONCTIONS DE SERENDIP
C     ET DE LEURS DERIVEES AUX 9 NOEUDS

        DO 120 L = 1,9
          I1 = 810 + L
          I2 = 810 + 9 + L
          X(1) = ZR(LZR-1+I1)
          X(2) = ZR(LZR-1+I2)
          CALL ELRFDF('QU8',X,18,DFF,NNO,NDIM)
          LL = 8* (L-1)
          ZR(LZR-1+ 828 + LL + 1) = DFF(1,4)
          ZR(LZR-1+ 900 + LL + 1) = DFF(2,4)
          ZR(LZR-1+ 828 + LL + 2) = DFF(1,1)
          ZR(LZR-1+ 900 + LL + 2) = DFF(2,1)
          ZR(LZR-1+ 828 + LL + 3) = DFF(1,2)
          ZR(LZR-1+ 900 + LL + 3) = DFF(2,2)
          ZR(LZR-1+ 828 + LL + 4) = DFF(1,3)
          ZR(LZR-1+ 900 + LL + 4) = DFF(2,3)
          ZR(LZR-1+ 828 + LL + 5) = DFF(1,8)
          ZR(LZR-1+ 900 + LL + 5) = DFF(2,8)
          ZR(LZR-1+ 828 + LL + 6) = DFF(1,5)
          ZR(LZR-1+ 900 + LL + 6) = DFF(2,5)
          ZR(LZR-1+ 828 + LL + 7) = DFF(1,6)
          ZR(LZR-1+ 900 + LL + 7) = DFF(2,6)
          ZR(LZR-1+ 828 + LL + 8) = DFF(1,7)
          ZR(LZR-1+ 900 + LL + 8) = DFF(2,7)
  120   CONTINUE

C     DEFINITION DES 8 FONCTIONS D'INTERPOLATION QUI PERMETTENT
C     D'EXTRAPOLER LES DEFORMATIONS OU CONTRAINTES AUX NOEUDS
C     PARTIR DE LEURS VALEURS AUX POINTS DE GAUSS

        ZR(LZR-1+1251) = -1.D0
        ZR(LZR-1+1252) = 0.D0
        ZR(LZR-1+1253) = 1.D0

        KOMPT = 0

        DO 150 M = 1,3
          I3 = 1250 + M
          XI3 = ZR(LZR-1+I3)
          DO 140 L = 1,8
            I1 = 810 + L
            I2 = 810 + 9 + L
            XI1 = ZR(LZR-1+I1)
            XI2 = ZR(LZR-1+I2)
            KOMPT = KOMPT + 1
            LL = 8* (KOMPT-1)
            CALL FCESND(ELREFE,1,XI1,XI2,XI3,'LI',VFESND)
            DO 130 K = 1,8
              I4 = 1260 + LL + K
              ZR(LZR-1+I4) = VFESND(K)
  130       CONTINUE
  140     CONTINUE
  150   CONTINUE

        XI3 = 0.D0
        DO 170 L = 1,8
          I1 = 810 + L
          I2 = 810 + 9 + L
          XI1 = ZR(LZR-1+I1)
          XI2 = ZR(LZR-1+I2)
          CALL FCESND(ELREFE,0,XI1,XI2,XI3,'LI',VFESND)
          LL = 4* (L-1)
          DO 160 K = 1,4
            I3 = 1452 + LL + K
            ZR(LZR-1+I3) = VFESND(K)
  160     CONTINUE
  170   CONTINUE

C     EN NON LINEAIRE, CREATION DE LA MATRICE MAGIQUE DE PASSAGE
C     PTS DE GAUSS AUX NOEUDS PAR MOINDRES CARRES

        LJMAS = NPGCOU*NSO*NPGCOU*NPGSN
        CALL WKVECT('&INEL.'//ELREFL//'.B','V V R',LJMAS,JMAS)

        CALL MAMAGI(ELREFE,ZR(LZR),ZR(JMAS))

C     DEFINITION DES PTS DE GAUSS DANS L'EPAISSEUR, DE
C     LEURS POIDS

        CALL FCEPAI(ZR(LZR))

      ELSE IF (ELREFE.EQ.'MEC3TR7H' ) THEN

C     POUR LE TRIANGLE

C     DEFINITION DES 3 PTS DE HAMMER REDUIT ET DES POIDS CORRESPONDANT

        CALL ELRAGA('TR7','FPG3    ',NDIM,NBPG,XPG,POIPG)
        ZR(LZR-1+1) = XPG(1)
        ZR(LZR-1+2) = XPG(3)
        ZR(LZR-1+3) = XPG(5)

        ZR(LZR-1+5) = XPG(2)
        ZR(LZR-1+6) = XPG(4)
        ZR(LZR-1+7) = XPG(6)

        ZR(LZR-1+9) = POIPG(1)
        ZR(LZR-1+10) = POIPG(2)
        ZR(LZR-1+11) = POIPG(3)

C     FONCTIONS DE LAGRANGE (6 FONCTIONS)
C
C     VALEURS AUX 3 PTS DE HAMMER REDUITS
C     DES FONCTIONS DE LAGRANGE ET DE LEURS
C     DERIVEES

        DO 190 L = 1,3
          I1 = L
          I2 = 4 + L
          XI1 = ZR(LZR-1+I1)
          XI2 = ZR(LZR-1+I2)
          LL = 8* (L-1)
C
          FF(1)= XI2*(2.D0*XI2-1.D0)
          FF(2)=(1.D0-XI1-XI2)*(2.D0*(1.D0-XI1-XI2)-1.D0)
          FF(3)= XI1*(2.D0*XI1-1.D0)
          FF(4)= 4.D0*XI2*(1.D0-XI1-XI2)
          FF(5)= 4.D0*XI1*(1.D0-XI1-XI2)
          FF(6)= 4.D0*XI1*XI2
          DFF(1,1)= 0.D0
          DFF(1,2)=-4.D0*(1.D0-XI1-XI2)+1.D0
          DFF(1,3)= 4.D0*XI1-1.D0
          DFF(1,4)=-4.D0*XI2
          DFF(1,5)= 4.D0*(1.D0-XI1-XI2)-4.D0*XI1
          DFF(1,6)= 4.D0*XI2
          DFF(2,1)= 4.D0*XI2-1.D0
          DFF(2,2)=-4.D0*(1.D0-XI1-XI2)+1.D0
          DFF(2,3)= 0.D0
          DFF(2,4)= 4.D0*(1.D0-XI1-XI2)-4.D0*XI2
          DFF(2,5)=-4.D0*XI1
          DFF(2,6)= 4.D0*XI1
C
          DO 180 L1 = 1,6
            I3 = 12 + LL + L1
            I4 = 44 + LL + L1
            I5 = 76 + LL + L1
            ZR(LZR-1+I3) = FF(L1)
            ZR(LZR-1+I4) = DFF(1,L1)
            ZR(LZR-1+I5) = DFF(2,L1)
  180     CONTINUE
  190   CONTINUE

C     DEFINITION DES 7 PTS DE HAMMER NORMAL ET DES POIDS CORRESPONDANTS

        CALL ELRAGA('TR7','FPG7    ',NDIM,NBPG,XPG,POIPG)

        ZR(LZR-1+109) = XPG(1)
        ZR(LZR-1+110) = XPG(3)
        ZR(LZR-1+111) = XPG(5)
        ZR(LZR-1+112) = XPG(7)
        ZR(LZR-1+113) = XPG(9)
        ZR(LZR-1+114) = XPG(11)
        ZR(LZR-1+115) = XPG(13)

        ZR(LZR-1+118) = XPG(2)
        ZR(LZR-1+119) = XPG(4)
        ZR(LZR-1+120) = XPG(6)
        ZR(LZR-1+121) = XPG(8)
        ZR(LZR-1+122) = XPG(10)
        ZR(LZR-1+123) = XPG(12)
        ZR(LZR-1+124) = XPG(14)

        ZR(LZR-1+127) = POIPG(1)
        ZR(LZR-1+128) = POIPG(2)
        ZR(LZR-1+129) = POIPG(3)
        ZR(LZR-1+130) = POIPG(4)
        ZR(LZR-1+131) = POIPG(5)
        ZR(LZR-1+132) = POIPG(6)
        ZR(LZR-1+133) = POIPG(7)

C     VALEURS AUX 7 PTS DE HAMMER NORMAL
C     DES FONCTIONS DE LAGRANGE ET DE LEURS
C     DERVIVEEES

        DO 210 L = 1,7
          I1 = 108 + L
          I2 = 108 + 9 + L
          XI1 = ZR(LZR-1+I1)
          XI2 = ZR(LZR-1+I2)
          LL = 8* (L-1)
C
          FF(1)= XI2*(2.D0*XI2-1.D0)
          FF(2)=(1.D0-XI1-XI2)*(2.D0*(1.D0-XI1-XI2)-1.D0)
          FF(3)= XI1*(2.D0*XI1-1.D0)
          FF(4)= 4.D0*XI2*(1.D0-XI1-XI2)
          FF(5)= 4.D0*XI1*(1.D0-XI1-XI2)
          FF(6)= 4.D0*XI1*XI2
          DFF(1,1)= 0.D0
          DFF(1,2)=-4.D0*(1.D0-XI1-XI2)+1.D0
          DFF(1,3)= 4.D0*XI1-1.D0
          DFF(1,4)=-4.D0*XI2
          DFF(1,5)= 4.D0*(1.D0-XI1-XI2)-4.D0*XI1
          DFF(1,6)= 4.D0*XI2
          DFF(2,1)= 4.D0*XI2-1.D0
          DFF(2,2)=-4.D0*(1.D0-XI1-XI2)+1.D0
          DFF(2,3)= 0.D0
          DFF(2,4)= 4.D0*(1.D0-XI1-XI2)-4.D0*XI2
          DFF(2,5)=-4.D0*XI1
          DFF(2,6)= 4.D0*XI1
C
          DO 200 L1 = 1,6
            I3 = 135 + LL + L1
            I4 = 207 + LL + L1
            I5 = 279 + LL + L1
            ZR(LZR-1+I3) = FF(L1)
            ZR(LZR-1+I4) = DFF(1,L1)
            ZR(LZR-1+I5) = DFF(2,L1)
  200     CONTINUE
  210   CONTINUE

C     7 FONCTIONS CUBIQUES (6 + 1 :  LA DERNIERE EST LA 10EME
C                                    FONCTION DE P3)
C
C     VALEURS AUX 3 PTS DE HAMMER REDUITS

        DO 230 L = 1,3
          I1 = L
          I2 = 4 + L
          XI1 = ZR(LZR-1+I1)
          XI2 = ZR(LZR-1+I2)
          LL = 9* (L-1)
C
          FF(1)= XI2*(2.D0*XI2-1.D0)+3.D0*XI1*XI2*(1.D0-XI1-XI2)
          FF(2)=(1.D0-XI1-XI2)*(2.D0*(1.D0-XI1-XI2)-1.D0)
     &                               +3.D0*XI1*XI2*(1.D0-XI1-XI2)
          FF(3)= XI1*(2.D0*XI1-1.D0)+3.D0*XI1*XI2*(1.D0-XI1-XI2)
          FF(4)= 4.D0*XI2*(1.D0-XI1-XI2)-12.D0*XI1*XI2*(1.D0-XI1-XI2)
          FF(5)= 4.D0*XI1*(1.D0-XI1-XI2)-12.D0*XI1*XI2*(1.D0-XI1-XI2)
          FF(6)= 4.D0*XI1*XI2-12.D0*XI1*XI2*(1.D0-XI1-XI2)
          FF(7)= 27.D0*XI1*XI2*(1.D0-XI1-XI2)
          DFF(1,1)=3.D0*XI2*(1.D0-XI1-XI2)-3.D0*XI1*XI2
          DFF(1,2)=-4.D0*(1.D0-XI1-XI2)+1.D0
     &                         +3.D0*XI2*(1.D0-XI1-XI2)-3.D0*XI1*XI2
          DFF(1,3)= 4.D0*XI1-1.D0+3.D0*XI2*(1.D0-XI1-XI2)-3.D0*XI1*XI2
          DFF(1,4)=-4.D0*XI2-12.D0*XI2*(1.D0-XI1-XI2)+12.D0*XI1*XI2
          DFF(1,5)= 4.D0*(1.D0-XI1-XI2)-4.D0*XI1
     &                    -12.D0*XI2*(1.D0-XI1-XI2)+12.D0*XI1*XI2
          DFF(1,6)= 4.D0*XI2-12.D0*XI2*(1.D0-XI1-XI2)+12.D0*XI1*XI2
          DFF(1,7)= 27.D0*XI2*(1.D0-XI1-XI2)-27.D0*XI1*XI2
          DFF(2,1)= 4.D0*XI2-1.D0+3.D0*XI1*(1.D0-XI1-XI2)-3.D0*XI1*XI2
          DFF(2,2)=-4.D0*(1.D0-XI1-XI2)+1.D0
     &                         +3.D0*XI1*(1.D0-XI1-XI2)-3.D0*XI1*XI2
          DFF(2,3)= 3.D0*XI1*(1.D0-XI1-XI2)-3.D0*XI1*XI2
          DFF(2,4)= 4.D0*(1.D0-XI1-XI2)-4.D0*XI2
     &                    -12.D0*XI1*(1.D0-XI1-XI2)+12.D0*XI1*XI2
          DFF(2,5)=-4.D0*XI1-12.D0*XI1*(1.D0-XI1-XI2)+12.D0*XI1*XI2
          DFF(2,6)= 4.D0*XI1-12.D0*XI1*(1.D0-XI1-XI2)+12.D0*XI1*XI2
          DFF(2,7)= 27.D0*XI1*(1.D0-XI1-XI2)-27.D0*XI1*XI2
C
          DO 220 L1 = 1,7
            I3 = 351 + LL + L1
            I4 = 387 + LL + L1
            I5 = 423 + LL + L1
            ZR(LZR-1+I3) = FF(L1)
            ZR(LZR-1+I4) = DFF(1,L1)
            ZR(LZR-1+I5) = DFF(2,L1)
  220     CONTINUE
  230   CONTINUE

C     VALEURS AUX 7 PTS DE HAMMER NORMAL

        DO 250 L = 1,7
          I1 = 108 + L
          I2 = 108 + 9 + L
          XI1 = ZR(LZR-1+I1)
          XI2 = ZR(LZR-1+I2)
          LL = 9* (L-1)
C
          FF(1)= XI2*(2.D0*XI2-1.D0)+3.D0*XI1*XI2*(1.D0-XI1-XI2)
          FF(2)=(1.D0-XI1-XI2)*(2.D0*(1.D0-XI1-XI2)-1.D0)
     &                               +3.D0*XI1*XI2*(1.D0-XI1-XI2)
          FF(3)= XI1*(2.D0*XI1-1.D0)+3.D0*XI1*XI2*(1.D0-XI1-XI2)
          FF(4)= 4.D0*XI2*(1.D0-XI1-XI2)-12.D0*XI1*XI2*(1.D0-XI1-XI2)
          FF(5)= 4.D0*XI1*(1.D0-XI1-XI2)-12.D0*XI1*XI2*(1.D0-XI1-XI2)
          FF(6)= 4.D0*XI1*XI2-12.D0*XI1*XI2*(1.D0-XI1-XI2)
          FF(7)= 27.D0*XI1*XI2*(1.D0-XI1-XI2)
          DFF(1,1)=3.D0*XI2*(1.D0-XI1-XI2)-3.D0*XI1*XI2
          DFF(1,2)=-4.D0*(1.D0-XI1-XI2)+1.D0
     &                         +3.D0*XI2*(1.D0-XI1-XI2)-3.D0*XI1*XI2
          DFF(1,3)= 4.D0*XI1-1.D0+3.D0*XI2*(1.D0-XI1-XI2)-3.D0*XI1*XI2
          DFF(1,4)=-4.D0*XI2-12.D0*XI2*(1.D0-XI1-XI2)+12.D0*XI1*XI2
          DFF(1,5)= 4.D0*(1.D0-XI1-XI2)-4.D0*XI1
     &                    -12.D0*XI2*(1.D0-XI1-XI2)+12.D0*XI1*XI2
          DFF(1,6)= 4.D0*XI2-12.D0*XI2*(1.D0-XI1-XI2)+12.D0*XI1*XI2
          DFF(1,7)= 27.D0*XI2*(1.D0-XI1-XI2)-27.D0*XI1*XI2
          DFF(2,1)= 4.D0*XI2-1.D0+3.D0*XI1*(1.D0-XI1-XI2)-3.D0*XI1*XI2
          DFF(2,2)=-4.D0*(1.D0-XI1-XI2)+1.D0
     &                         +3.D0*XI1*(1.D0-XI1-XI2)-3.D0*XI1*XI2
          DFF(2,3)= 3.D0*XI1*(1.D0-XI1-XI2)-3.D0*XI1*XI2
          DFF(2,4)= 4.D0*(1.D0-XI1-XI2)-4.D0*XI2
     &                    -12.D0*XI1*(1.D0-XI1-XI2)+12.D0*XI1*XI2
          DFF(2,5)=-4.D0*XI1-12.D0*XI1*(1.D0-XI1-XI2)+12.D0*XI1*XI2
          DFF(2,6)= 4.D0*XI1-12.D0*XI1*(1.D0-XI1-XI2)+12.D0*XI1*XI2
          DFF(2,7)= 27.D0*XI1*(1.D0-XI1-XI2)-27.D0*XI1*XI2
C
          DO 240 L1 = 1,7
            I3 = 459 + LL + L1
            I4 = 540 + LL + L1
            I5 = 621 + LL + L1
            ZR(LZR-1+I3) = FF(L1)
            ZR(LZR-1+I4) = DFF(1,L1)
            ZR(LZR-1+I5) = DFF(2,L1)
  240     CONTINUE
  250   CONTINUE

C    FONCTIONS ASSOCIEES AUX 3 PTS DE HAMMER REDUITS

C    VALEURS AUX 7 PTS DE HAMMER NORMAL

        DO 270 L = 1,7
          I1 = 108 + L
          I2 = 108 + 9 + L
          X(1) = ZR(LZR-1+I1)
          X(2) = ZR(LZR-1+I2)
          LL = 4* (L-1)
C
          A=0.166666666666667D0
          B=0.666666666666667D0
          FF(1)= 2.D0*(-(X(1)-B)-(X(2)-A))
          FF(2)= 2.D0*(X(1)-A)
          FF(3)= 2.D0*(X(2)-A)
          DFF(1,1)=-2.D0
          DFF(1,2)= 2.D0
          DFF(1,3)= 0.D0
          DFF(2,1)=-2.D0
          DFF(2,2)= 0.D0
          DFF(2,3)= 2.D0
          DO 260 L1 = 1,3
            I3 = 702 + LL + L1
            I4 = 738 + LL + L1
            I5 = 774 + LL + L1
            ZR(LZR-1+I3) = FF(L1)
            ZR(LZR-1+I4) = DFF(1,L1)
            ZR(LZR-1+I5) = DFF(2,L1)
  260     CONTINUE
  270   CONTINUE

C     VALEURS DES FONCTIONS DE LAGRANGE AUX 7 NOEUDS
C     POSITION DES 7 NOEUDS

        ZR(LZR-1+811) = 0.000000000000000D0
        ZR(LZR-1+812) = 0.000000000000000D0
        ZR(LZR-1+813) = 1.000000000000000D0
        ZR(LZR-1+814) = 0.000000000000000D0
        ZR(LZR-1+815) = 0.500000000000000D0
        ZR(LZR-1+816) = 0.500000000000000D0
        ZR(LZR-1+817) = 0.333333333333333D0

        ZR(LZR-1+820) = 1.000000000000000D0
        ZR(LZR-1+821) = 0.000000000000000D0
        ZR(LZR-1+822) = 0.000000000000000D0
        ZR(LZR-1+823) = 0.500000000000000D0
        ZR(LZR-1+824) = 0.000000000000000D0
        ZR(LZR-1+825) = 0.500000000000000D0
        ZR(LZR-1+826) = 0.333333333333333D0

        DO 290 L = 1,7
          I1 = 810 + L
          I2 = 810 + 9 + L
          XI1 = ZR(LZR-1+I1)
          XI2 = ZR(LZR-1+I2)
          LL = 8* (L-1)
C
          DFF(1,1)= 0.D0
          DFF(1,2)=-4.D0*(1.D0-XI1-XI2)+1.D0
          DFF(1,3)= 4.D0*XI1-1.D0
          DFF(1,4)=-4.D0*XI2
          DFF(1,5)= 4.D0*(1.D0-XI1-XI2)-4.D0*XI1
          DFF(1,6)= 4.D0*XI2
          DFF(2,1)= 4.D0*XI2-1.D0
          DFF(2,2)=-4.D0*(1.D0-XI1-XI2)+1.D0
          DFF(2,3)= 0.D0
          DFF(2,4)= 4.D0*(1.D0-XI1-XI2)-4.D0*XI2
          DFF(2,5)=-4.D0*XI1
          DFF(2,6)= 4.D0*XI1
C
          DO 280 L1 = 1,6
            I3 = 828 + LL + L1
            I4 = 900 + LL + L1
            ZR(LZR-1+I3) = DFF(1,L1)
            ZR(LZR-1+I4) = DFF(2,L1)
  280     CONTINUE
  290   CONTINUE

C     DEFINITION DES 6 FONCTIONS D'INTERPOLATION QUI PERMETTENT
C     D'EXTRAPOLER
C     LES DEFORMATIONS OU CONTRAINTES AUX NOEUDS A PARTIR DE
C     LEURS VALEURS AUX POINTS DE HAMMER

        ZR(LZR-1+1251) = -1.D0
        ZR(LZR-1+1252) = 0.D0
        ZR(LZR-1+1253) = 1.D0

        KOMPT = 0
        DO 320 M = 1,3
          I3 = 1250 + M
          XI3 = ZR(LZR-1+I3)
          DO 310 L = 1,6
            I1 = 810 + L
            I2 = 810 + 9 + L
            XI1 = ZR(LZR-1+I1)
            XI2 = ZR(LZR-1+I2)
            KOMPT = KOMPT + 1
            LL = 6* (KOMPT-1)
            CALL FCESND(ELREFE,1,XI1,XI2,XI3,'LI',VFESND)
            DO 300 K = 1,6
              I4 = 1260 + LL + K
              ZR(LZR-1+I4) = VFESND(K)
  300       CONTINUE
  310     CONTINUE
  320   CONTINUE

        XI3 = 0.D0
        DO 340 L = 1,6
          I1 = 810 + L
          I2 = 810 + 9 + L
          XI1 = ZR(LZR-1+I1)
          XI2 = ZR(LZR-1+I2)
          CALL FCESND(ELREFE,0,XI1,XI2,XI3,'LI',VFESND)
          LL = 4* (L-1)
          DO 330 K = 1,3
            I3 = 1452 + LL + K
            ZR(LZR-1+I3) = VFESND(K)
  330     CONTINUE
  340   CONTINUE

C     EN NON LINEAIRE, CREATION DE LA MATRICE MAGIQUE DE PASSAGE
C     PTS DE HAMMER AUX NOEUDS PAR MOINDRES CARRES

        LJMAS = NPGCOU*NSO*NPGCOU*NPGSN
        CALL WKVECT('&INEL.'//ELREFL//'.B','V V R',LJMAS,JMAS)

        CALL MAMAGI(ELREFE,ZR(LZR),ZR(JMAS))

C     DEFINITION DES PTS DE GAUSS DANS L'EPAISSEUR, DE
C     LEURS POIDS

        CALL FCEPAI(ZR(LZR))

      END IF

  350 CONTINUE
      END

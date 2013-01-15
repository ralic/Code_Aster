      SUBROUTINE OP0019()

C MODIF MODELISA  DATE 14/01/2013   AUTEUR FLEJOU J-L.FLEJOU 
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C
C                O P E R A T E U R    AFFE_CARA_ELEM
C
C ----------------------------------------------------------------------
      IMPLICIT  NONE
C     NBEPO  13 : NOMBRE D'ELEMENTS DE TYPE "POUTRE"
C     NBEDI   8 : NOMBRE D'ELEMENTS DE TYPE "DISCRET"
C     NBECO  29 : NOMBRE D'ELEMENTS DE TYPE "COQUE"
C     NBECA   2 : NOMBRE D'ELEMENTS DE TYPE "CABLE"
C     NBEBA   2 : NOMBRE D'ELEMENTS DE TYPE "BARRE"
C     NBEMA  53 : NOMBRE D'ELEMENTS DE TYPE "MASSIF"
C     NBEGB   6 : NOMBRE D'ELEMENTS DE TYPE "GRILLE"
C     NBEMB   4 : NOMBRE D'ELEMENTS DE TYPE "MEMBRANE"
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNOM
      INTEGER     NBEPO,NBEDI,NBECO,NBECA,NBEBA,NBEMA,NBEGB,NBEMB
      INTEGER     NBTEL,NBMCF,NBEL1,NBEL2,NBEL3
      PARAMETER  (      NBEPO=13,NBEDI=8,NBECO=29,NBECA=2,NBEBA=2)
      PARAMETER  (NBEL1=NBEPO   +NBEDI  +NBECO   +NBECA  +NBEBA)
      PARAMETER  (      NBEMA=54,NBEGB=6,NBEMB=4)
      PARAMETER  (NBEL2=NBEMA)
      PARAMETER  (NBEL3=NBEGB   +NBEMB)
      PARAMETER  (NBTEL=NBEL1+NBEL2+NBEL3)
C     NBMCF  : NOMBRE DE MOTS CLES FACTEUR DE L'OPERATEUR
      PARAMETER  (NBMCF=15)
C ----------------------------------------------------------------------
      INTEGER        NBMCLE(NBMCF),NBOCC(NBMCF),IVR(3),NBCART,IRET,I
      INTEGER        NTYELE(NBTEL),NBVER,NLM,NLG,LXC,LXO,NLN,NLJ,LXA
      INTEGER        LXK,LXB,LXM,LXPF,LXGB,LXMB,LMAX,IFM,NIV,LXP,NBVM
      INTEGER        LXD,NBOCCD,LXRP,NOEMAF,LXRM,NOEMF2,NBMAIL,NBMTRD
      INTEGER        LXMR,NOEMF3
      INTEGER        NPOUTR,NDISCR,NCOQUE,NCABLE,NBARRE,NMASSI,NGRILL
      INTEGER        NGRIBT,NMEMBR,ICLF,IOC,ICLE,NG,IARG
      INTEGER        DEPART,JDNM,IXNW,JDLN,JDLM,JDLS
      LOGICAL        LOCACO, LOCAGB, LOCAMB
      CHARACTER*1    K1BID
      CHARACTER*6    KIOC
      CHARACTER*8    VER(3),NOMU,NOMO,NOMA,LPAIN(3),LPAOUT(1)
      CHARACTER*16   CONCEP,CMD,MCLF(NBMCF),MCLE(4),K16BID
      CHARACTER*16   NOMELE(NBTEL),NOMEL1(NBEL1),NOMEL2(NBEL2)
      CHARACTER*16   NOMEL3(NBEL3)
      CHARACTER*19   CARTCF,LIGRMO,LCHIN(3),LCHOUT(1)
      CHARACTER*24   MLGNMA,MODNOM,MODNEM,TMPLST,TMPLMA,TMPLNO,TMPNCF
      DATA MCLE   /  'GROUP_MA        ','MAILLE          ',
     &               'GROUP_NO        ','NOEUD           '/

      DATA MCLF   /  'POUTRE          ','COQUE           ',
     &               'DISCRET         ','ORIENTATION     ',
     &               'DEFI_ARC        ','CABLE           ',
     &               'BARRE           ','MASSIF          ',
     &               'POUTRE_FLUI     ','RIGI_PARASOL    ',
     &               'GRILLE          ','RIGI_MISS_3D    ',
     &               'DISCRET_2D      ','MEMBRANE        ',
     &               'MASS_AJOU       '/
C     !!!! A L'ORDRE DE STOCKAGE
C        NOMEL1 : POUTRE(13) DISCRET(8) COQUE(28) CABLE(2) BARRE(2)
C        NOMEL2 : MASSIF(53)
C        NOMEL3 : GRILLE(6) MEMBRANE(4)
      DATA NOMEL1 /         'MECA_POU_D_T    ','MECA_POU_D_E    ',
     &   'MECA_POU_D_T_GD ','MECA_POU_C_T    ','MEFS_POU_D_T    ',
     &   'MECA_POU_D_TG   ','MECA_POHO_HEXA8 ','MECA_POHO_HEXA20',
     &   'MET3SEG3        ','MET6SEG3        ','MET3SEG4        ',
     &   'MECA_POU_D_EM   ','MECA_POU_D_TGM  ','MECA_DIS_T_N    ',
     &   'MECA_DIS_T_L    ','MECA_DIS_TR_N   ','MECA_DIS_TR_L   ',
     &   'MECA_2D_DIS_T_N ','MECA_2D_DIS_T_L ','MECA_2D_DIS_TR_N',
     &   'MECA_2D_DIS_TR_L','THCOTR3         ','THCOTR6         ',
     &   'THCOQU4         ','THCOQU8         ','THCOTR7         ',
     &   'THCOQU9         ','MEDKTR3         ','MEDSTR3         ',
     &   'MET3TR3         ',
     &   'MEDKQU4         ','MEDSQU4         ','MEQ4QU4         ',
     &   'MECXSE3         ','MEDKTG3         ','MEDKQG4         ',
     &   'MEQ4GG4         ','MET3GG3         ',
     &   'METCSE3         ','METDSE3         ','THCASE3         ',
     &   'THCPSE3         ','MEC3QU9H        ','MEC3TR7H        ',
     &   'MEBODKT         ','MEBODST         ','MEBOQ4G         ',
     &   'MEBOCQ3         ','THCOSE3         ','THCOSE2         ',
     &   'MECABL2         ','MEPOULI         ','MECA_BARRE      ',
     &   'MECA_2D_BARRE   '/

      DATA NOMEL2 /         'MECA_HEXA8      ','MECA_PENTA6     ',
     &   'MECA_PENTA18    ','MECA_TETRA4     ','MECA_HEXA27     ',
     &   'MECA_HEXA20     ','MECA_PENTA15    ','MECA_TETRA10    ',
     &   'MECA_TETRS10    ',
     &   'MECA_PYRAM5     ','MECA_PYRAM13    ','MECA_HEXS8      ',
     &   'MECA_HEXS20     ','MEAXTR3         ','MEAXQU4         ',
     &   'MEAXTR6         ','MEAXQU8         ','MEAXQU9         ',
     &   'MEDPTR3         ','MEDPQU4         ','MEDPTR6         ',
     &   'MEDPQU8         ','MEDPQU9         ','MECPTR3         ',
     &   'MECPQU4         ','MECPTR6         ','MECPQU8         ',
     &   'MECPQU9         ','THER_HEXA8      ','THER_PENTA6     ',
     &   'THER_TETRA4     ','THER_PYRAM5     ','THER_HEXA27     ',
     &   'THER_HEXA20     ','THER_PENTA15    ','THER_TETRA10    ',
     &   'THER_PYRAM13    ','THAXTR3         ','THAXQU4         ',
     &   'THAXTR6         ','THAXQU8         ','THAXQU9         ',
     &   'THPLTR3         ','THPLQU4         ','THPLTR6         ',
     &   'THPLQU8         ','THPLQU9         ','MET3SEG3        ',
     &   'MET6SEG3        ','MET3SEG4        ','HM_DPQ8S        ',
     &   'HM_AXIS_QU8S    ','HM_DPTR6S       ','HM_AXIS_TR6S    '/

      DATA NOMEL3 /         'MEGCQU4         ','MEGMTR3         ',
     &   'MEGMQU4         ','MEGMTR6         ','MEGMQU8         ',
     &   'MEGCTR3         ','MEMBTR3         ','MEMBTR6         ',
     &   'MEMBQU4         ','MEMBQU8         '/

      DATA NBMCLE /  2,2,4,4,2,2,2,2,2,1,2,0,4,2,1/
C --- ------------------------------------------------------------------
      CALL JEMARQ()
C     CALL ONERRF('ABORT', K16BID, IRET)

      IRET=0
C --- ------------------------------------------------------------------
C --- INITIALISATION DE  NOMELE
      DO 10 I = 1,NBEL1
         NOMELE(I) = NOMEL1(I)
   10 CONTINUE
      DO 20 I = 1,NBEL2
         NOMELE(I+NBEL1) = NOMEL2(I)
   20 CONTINUE
      DO 25 I = 1,NBEL3
         NOMELE(I+NBEL1+NBEL2) = NOMEL3(I)
   25 CONTINUE
C --- ------------------------------------------------------------------
C --- RECUPERATION DES ARGUMENTS  DE LA COMMANDE
      CALL GETRES(NOMU,CONCEP,CMD)

C --- ------------------------------------------------------------------
C --- VERIFICATIONS SUPPLEMENTAIRES DE SYNTAXE
      CALL GETVTX(' ','VERIF',1,IARG,2,VER,NBVER)
      DO 30 I = 1,3
         IVR(I) = 0
   30 CONTINUE
      IF (NBVER.GT.0) THEN
         DO 40 I = 1,NBVER
            IF (VER(I).EQ.'MAILLE  ') IVR(1) = 1
            IF (VER(I).EQ.'NOEUD   ') IVR(2) = 1
   40    CONTINUE
      ELSE IF (NBVER.LT.0) THEN
         CALL U2MESS('F','MODELISA5_55')
      END IF

      DO 50 I = 1,NBMCF
         CALL GETFAC(MCLF(I),NBOCC(I))
   50 CONTINUE

C --- ------------------------------------------------------------------
C --- VERIFICATION DE LA SYNTAXE DES ELEMENTS POUTRE
      LXP = 0
      IF (NBOCC(1).NE.0) THEN
         CALL ACEVPO(NBOCC(1),NLM,NLG,IRET)
         LXP = MAX(NLM,NLG)
      END IF

C --- ------------------------------------------------------------------
C --- VERIFICATION DE LA SYNTAXE DES ELEMENTS COQUE
      LXC = 0
      IF (NBOCC(2).NE.0) THEN
         CALL ACEVCO(NBOCC(2),NLM,NLG,IRET)
         LXC = MAX(NLM,NLG)
      END IF

C --- ------------------------------------------------------------------
C --- VERIFICATION DE LA SYNTAXE DES ORIENTATIONS DES ELEMENTS
      LXO = 0
      IF (NBOCC(4).NE.0) THEN
         CALL ACEVOR(NBOCC(4),NLM,NLG,NLN,NLJ,IRET)
         LXO = MAX(NLM,NLN,NLJ,NLG)
      END IF

C --- ------------------------------------------------------------------
C --- VERIFICATION DE LA SYNTAXE DES POUTRES COURBES
      LXA = 0
      IF (NBOCC(5).NE.0) THEN
         CALL ACEVPC(NBOCC(5),NLM,NLG,IRET)
         LXA = MAX(NLM,NLG)
      END IF

C --- ------------------------------------------------------------------
C --- VERIFICATION DE LA SYNTAXE DES ELEMENTS CABLE
      LXK = 0
      IF (NBOCC(6).NE.0) THEN
         CALL ACEVCA(NBOCC(6),NLM,NLG,IRET)
         LXK = MAX(NLM,NLG)
      END IF

C --- ------------------------------------------------------------------
C --- VERIFICATION DE LA SYNTAXE DES ELEMENTS BARRE
      LXB = 0
      IF (NBOCC(7).NE.0) THEN
         CALL ACEVBA(NBOCC(7),NLM,NLG,IRET)
         LXB = MAX(NLM,NLG)
      END IF

C --- ------------------------------------------------------------------
C --- VERIFICATION DE LA SYNTAXE DES ELEMENTS MASSIF :
      LXM = 0
      IF (NBOCC(8).NE.0) THEN
         CALL ACEVMA(NBOCC(8),NLM,NLG)
         LXM = MAX(NLM,NLG)
      END IF

C --- ------------------------------------------------------------------
C --- VERIFICATION DE LA SYNTAXE DES ELEMENTS POUTRE_FLUI
      LXPF = 0
      IF (NBOCC(9).NE.0) THEN
         IF (NBOCC(1).EQ.0) THEN
            CALL U2MESS('F','MODELISA5_56')
         END IF
         CALL ACEVPF(NBOCC(9),NLM,NLG)
         LXPF = MAX(NLM,NLG)
      END IF

C --- ------------------------------------------------------------------
C --- VERIFICATION DE LA SYNTAXE DES ELEMENTS "GRILLE"
      LXGB = 0
      IF (NBOCC(11).NE.0) THEN
         CALL ACEVGB(NBOCC(11),NLM,NLG)
         LXGB = MAX(NLM,NLG)
      END IF

C --- ------------------------------------------------------------------
C --- VERIFICATION DE LA SYNTAXE DES ELEMENTS "MEMBRANE"
      LXMB = 0
      IF (NBOCC(14).NE.0) THEN
         CALL ACEVMB(NBOCC(14),NLM,NLG)
         LXMB = MAX(NLM,NLG)
      END IF

C --- ------------------------------------------------------------------
C --- LONGUEUR MAXIMUM D UNE LISTE DE MAILLE/NOEUD/GROUP_MA/GROUP_NO
      LMAX = MAX(1,LXP,LXC,LXO,LXA,LXK,LXB,LXM,LXPF,LXGB,LXMB)

C --- ------------------------------------------------------------------
C --- RECUPERATION DU NIVEAU D'IMPRESSION
      CALL INFMAJ
      CALL INFNIV(IFM,NIV)
      IF (NIV.EQ.2) IVR(3) = 1

C --- ------------------------------------------------------------------
C --- RECONSTRUCTION DES NOMS JEVEUX DU CONCEPT MODELE
      CALL GETVID(' ','MODELE',1,IARG,1,NOMO,NBVM)
      MODNOM = NOMO//'.MODELE    .LGRF'
      MODNEM = NOMO//'.MODELE    .NEMA'

C --- ------------------------------------------------------------------
C --- RECUPERATION DU NOM DU MAILLAGE ASSOCIE
      CALL JEVEUO(MODNOM,'L',JDNM)
      NOMA = ZK8(JDNM)

C --- ------------------------------------------------------------------
C --- RECONSTRUCTION DES NOMS JEVEUX DU CONCEPT MAILLAGE ASSOCIE
      MLGNMA = NOMA//'.NOMMAI'

C --- ------------------------------------------------------------------
C --- VERIFICATION DE LA SYNTAXE DES ELEMENTS DISCRET
      LXD = 0
      IF (NBOCC(3).NE.0 .OR. NBOCC(13).NE.0) THEN
         NBOCCD = NBOCC(3) + NBOCC(13)
         IF ( NBOCC(3) .NE.0 ) K16BID = MCLF(3)
         IF ( NBOCC(13).NE.0 ) K16BID = MCLF(13)
         CALL ACEVDI(NBOCCD,NOMA,NOMO,K16BID,NLM,NLG,NLN,NLJ,IRET)
         LXD  = MAX(NLM,NLN,NLG,NLJ)
         LMAX = MAX(LMAX,LXD)
      END IF

C --- ------------------------------------------------------------------
C --- VERIFICATION DE LA DIMENSION DES RAIDEURS REPARTIES
      LXRP = 0
      IF (NBOCC(10).NE.0) THEN
         CALL ACEVRP(NBOCC(10),NOMA,LXRP,NOEMAF)
         LMAX = MAX(LMAX,LXRP)
      END IF

C --- ------------------------------------------------------------------
C --- VERIFICATION DE LA DIMENSION DES RAIDEURS MISS
      LXRM = 0
      IF (NBOCC(12).NE.0) THEN
         CALL ACEVRM(NBOCC(12),NOMA,LXRM,NOEMF2)
         LMAX = MAX(LMAX,LXRM)
      END IF

C --- ------------------------------------------------------------------
C --- VERIFICATION DE LA DIMENSION DES MASSES REPARTIES
      LXMR = 0
      IF (NBOCC(15).NE.0) THEN
         CALL ACEVMR(NBOCC(15),NOMA,LXMR,NOEMF3)
         LMAX = MAX(LMAX,LXMR)
      END IF

C --- ------------------------------------------------------------------
C --- RECUPERATION DU NB DE MAILLES INITIALES (MAILLAGE)
      CALL JELIRA(MLGNMA,'NOMMAX',NBMAIL,K1BID)

C --- ------------------------------------------------------------------
C --- RECUPERATION DU NB DE MAILLES TARDIVES  (MODELE)
      TMPLST = NOMU//'.LISTE'
      TMPLMA = NOMU//'.AFFEMAI'
      TMPLNO = NOMU//'.AFFENOE'
      NBMTRD = 0
      CALL JEEXIN(MODNEM,IXNW)
      IF (IXNW.NE.0) THEN
         CALL JELIRA(MODNEM,'NMAXOC',NBMTRD,K1BID)
         CALL WKVECT(TMPLNO,'V V I',NBMTRD,JDLN)
      END IF
      CALL WKVECT(TMPLMA,'V V I',NBMAIL,JDLM)
      CALL WKVECT(TMPLST,'V V K24',LMAX,JDLS)

C --- ------------------------------------------------------------------
C --- RECUPERATION DES NUMEROS DES TYPES ELEMENTS
      DO 60 I = 1,NBTEL
         CALL JENONU(JEXNOM('&CATA.TE.NOMTE',NOMELE(I)),NTYELE(I))
   60 CONTINUE

C --- ------------------------------------------------------------------
C --- COMPTEUR D'ELEMENTS ET VERIFICATION COHERENCE DES AFFECTATIONS
      CALL ACECEL(NOMA,NOMO,NBOCC,NBEPO,NBEDI,NBECO,NBECA,NBEBA,NBEMA,
     &            NBEGB,NBTEL,NTYELE,NPOUTR,NDISCR,NCOQUE,NCABLE,
     &            NBARRE,NMASSI,NGRILL,NGRIBT,NMEMBR,JDLM,JDLN,IRET)
      IF (IRET.NE.0) THEN
         CALL U2MESS('F','MODELISA5_57')
      END IF

C --- ------------------------------------------------------------------
C --- VERIFICATION DE L'EXISTENCE DES MAILLES/NOEUDS/GROUPES DECLARES
      DO 100 ICLF = 1,NBMCF
         DO 90 IOC = 1,NBOCC(ICLF)
            CALL CODENT(IOC,'G',KIOC)
            DO 80 ICLE = 1,NBMCLE(ICLF)
               CALL GETVTX(MCLF(ICLF),MCLE(ICLE),IOC,IARG,LMAX,
     &                     ZK24(JDLS),NG)
               CALL VERIMA(NOMA, ZK24(JDLS), NG, MCLE(ICLE))
   80       CONTINUE
   90    CONTINUE
  100 CONTINUE

C --- ------------------------------------------------------------------
C --- VERIFICATION DE LA BONNE  AFFECTATION  DES  CARACTERISTIQUES
C     POUR TOUTES LES MAILLES ET NOEUDS AFFECTES , IMPR SI DEMANDE
C     INCREMENTATION DES COMPTEURS D APPELS A NOCART (DISCRET,COQUE,
C     DEFI_ARC,CABLE,POUTRE,BARRE)
      IRET=0
      CALL ACEINC(NOMA,NOMO,NBMCF,MCLF,NTYELE,NBOCC,IVR,NBEPO,
     &            NBEDI,NBECO,NBECA,NBEBA,NBEMA,NBEGB,NBEMB,NBTEL,
     &            LOCACO,LOCAGB,LOCAMB,JDLM,JDLN,LMAX,IRET)
      IF (IRET.NE.0) THEN
         CALL U2MESS('F','MODELISA5_59')
      END IF
C     FABRICATION DE LA CARTE COMMUNE A TOUS LES ELEMENTS LINEIQUE
C     S'IL Y EN A D'AFFECTE
      NBCART = 0
      IF ( NBOCC(1) + NBOCC(7) + NBOCC(6) .NE. 0 ) THEN
         NBCART = NPOUTR + NBARRE + NCABLE
         IF ( NBCART.GT.0 ) THEN
            CARTCF = NOMU//'.CVENTCXF'
            CALL ALCART('G',CARTCF,NOMA,'VENTCX_F')
         ENDIF
      ENDIF
      IF ( (NBOCC(1).EQ.0).AND.(NPOUTR.NE.0) )THEN
         CALL U2MESS('A','MODELISA5_60')
      ENDIF
      IF ( (NBOCC(7).EQ.0).AND.(NBARRE.NE.0) )THEN
         CALL U2MESS('A','MODELISA5_61')
      ENDIF
      IF ( (NBOCC(6).EQ.0).AND.(NCABLE.NE.0) )THEN
         CALL U2MESS('A','MODELISA5_62')
      ENDIF

C --- ------------------------------------------------------------------
C --- AFFECTATION DES ORIENTATIONS AUX ELEMENTS POUTRES ET DISCRETS  ET
C     BARRES ET AFFECTATION DE LA CARTE ORIENTATION
      IF (NBOCC(1).NE.0 .OR. NBOCC(3).NE.0 .OR. NBOCC(13).NE.0 .OR.
     &    NBOCC(7).NE.0 .OR. NBOCC(10).NE.0) THEN
         CALL ACEAOR(NOMA,NOMO,LMAX,NBEPO,NBEDI,NBTEL,NTYELE,NOMELE,
     &               IVR,IFM,NBOCC)
      END IF

C --- ------------------------------------------------------------------
C --- AFFECTATION DES CARACTERISTIQUES AUX ELEMENTS POUTRES
      IF (NBOCC(1).NE.0) THEN
C        NBEPO + NBEDI + NBECO + NBECA + NBEBA + NBEMA + NBEGB
         DEPART = 1
         CALL ACEAPO(NOMA,NOMO,LMAX,NPOUTR,NBOCC(1),MCLF(1),
     &               NBEPO,NTYELE(DEPART),IVR,IFM,JDLM)
      END IF

C --- ------------------------------------------------------------------
C --- AFFECTATION DES EPAISSEURS/COURBURES/ANGLES AUX ELEMENTS COQUES
      IF (NBOCC(2).NE.0) THEN
        CALL ACEACO(NOMU,NOMA,LMAX,LOCAGB,LOCAMB,NBOCC(2))
      END IF

C --- ------------------------------------------------------------------
C --- AFFECTATION DES MATRICES AUX ELEMENTS DISCRETS
      IF (NBOCC(3).NE.0 .OR. NBOCC(13).NE.0) THEN
         NBOCCD = NBOCC(3) + NBOCC(13)
         IF ( NBOCC(3) .NE.0 ) K16BID = MCLF(3)
         IF ( NBOCC(13).NE.0 ) K16BID = MCLF(13)
         CALL ACEADI(NOMA,NOMO,K16BID,LMAX,NBOCCD,IVR,IFM)
      END IF

C --- ------------------------------------------------------------------
C --- AFFECTATION DES COURBURES AUX ELEMENTS POUTRES COURBES
      IF (NBOCC(5).NE.0) THEN
         CALL ACEAPC(NOMU,NOMA,LMAX,NBOCC(5))
      END IF

C --- ------------------------------------------------------------------
C --- AFFECTATION DES SECTIONS AUX ELEMENTS CABLE :
      IF (NBOCC(6).NE.0) THEN
         CALL ACEACA(NOMU,NOMA,LMAX,NBOCC(6))
      END IF

C --- ------------------------------------------------------------------
C --- AFFECTATION DES CARACTERISTIQUES AUX ELEMENTS BARRE
      IF (NBOCC(7).NE.0) THEN
C        NBEPO + NBEDI + NBECO + NBECA + NBEBA + NBEMA + NBEGB
         DEPART = NBEPO + NBEDI + NBECO + NBECA + 1
         CALL ACEABA(NOMA,NOMO,LMAX,NBARRE,NBOCC(7),MCLF(7),
     &               NBEBA,NTYELE(DEPART),IVR,IFM,JDLM)
      END IF

C --- ------------------------------------------------------------------
C --- AFFECTATION DES REPERES AUX ELEMENTS THERMIQUES ET MECANIQUES
      IF (NBOCC(8).NE.0) THEN
         CALL ACEAMA(NOMU,NOMA,LMAX,NBOCC(8))
      END IF

C --- ------------------------------------------------------------------
C --- AFFECTATION DES REPERES AUX ELEMENTS POUTRE_FLUI
      IF (NBOCC(9).NE.0) THEN
         CALL ACEAPF(NOMU,NOMA,LMAX,NBOCC(9))
      END IF

C --- ------------------------------------------------------------------
C --- AFFECTATION DES MATRICES AUX RAIDEURS REPARTIES
      IF (NBOCC(10).NE.0) THEN
         CALL ACEARP(NOMA,NOMO,LMAX,NOEMAF,NBOCC(10),IVR,IFM)
      END IF

C --- ------------------------------------------------------------------
C --- AFFECTATION DES CARACTERISTIQUES POUR L'ELEMENT "GRILLE"
      IF (NBOCC(11).NE.0) THEN
         CALL ACEAGB(NOMU,NOMA,LMAX,LOCAMB,NBOCC(11))
      END IF

C --- ------------------------------------------------------------------
C --- AFFECTATION DES MATRICES AUX RAIDEURS MISS
      IF (NBOCC(12).NE.0) THEN
         CALL ACEARM(NOMA,NOMO,LMAX,NOEMF2,NBOCC(12),IVR,IFM)
      END IF

C --- ------------------------------------------------------------------
C --- AFFECTATION DES CARACTERISTIQUES POUR L'ELEMENT "MEMBRANE"
      IF (NBOCC(14).NE.0) THEN
         CALL ACEAMB(NOMU,NOMA,LMAX,NBOCC(14))
      END IF
C --- ------------------------------------------------------------------
C --- AFFECTATION DES MATRICES AUX MASSES REPARTIES
      IF (NBOCC(15).NE.0) THEN
         CALL ACEAMR(NOMA,NOMO,LMAX,NOEMF3,NBOCC(15),IVR,IFM)
      END IF

C --- ------------------------------------------------------------------
C --- COMPACTAGE DE LA CARTE : '.CVENTCXF'
      IF (NBCART.GT.0) THEN
C        PAS APPELE POUR UNE SURCHARGE "FINE" MAIS POUR LE COMPACTAGE
         CALL TECART(CARTCF)
C        DESTRUCTION DES CHAMPS
         TMPNCF = CARTCF//'.NCMP'
         CALL JEDETR(TMPNCF)
         TMPNCF = CARTCF//'.VALV'
         CALL JEDETR(TMPNCF)
      END IF

C     POUR LES COQUES, GRILLES IL PEUT EXISTER UNE CARTE FONCTION
C     IL FAUT L'EVALUER ET METTRE LE RESULTAT DANS LA CARTE DES REELS
      IF ( (NBOCC(2).NE.0).OR.(NBOCC(11).NE.0) ) THEN
         CALL COQUCF(NOMU)
      ENDIF
C --- ------------------------------------------------------------------
C --- TRAITEMENT DES MOTS CLES
C           MULTIFIBRE  /  GEOM_FIBRE
C           COQUE       /  COQUE_NCOU
C           GRILLE      /  COQUE_NCOU
C           MEMBRANE    /  COQUE_NCOU
C           POUTRE      /  TUYAU_NCOU
C           POUTRE      /  TUYAU_NSEC
C     ----------------------------------------------------------
      CALL PMFD00()

C     -- APPEL DE L'OPTION DE VERIFICATION VERI_CARA_ELEM :
C     -------------------------------------------------------
      LPAIN(1)='PCACOQU'
      LCHIN(1)=NOMU//'.CARCOQUE'
      LPAOUT(1)='PBIDON'
      LCHOUT(1)='&&OP0019.BIDON'
      LIGRMO=NOMO//'.MODELE'
      CALL CALCUL('C','VERI_CARA_ELEM',LIGRMO,1,LCHIN,LPAIN,1,
     &            LCHOUT,LPAOUT,'V','OUI')

      CALL JEDEMA()
      END

      SUBROUTINE OP0019(IER)
C MODIF MODELISA  DATE 05/09/2006   AUTEUR JOUMANA J.EL-GHARIB 
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C ----------------------------------------------------------------------
C TOLE CRP_20

C                O P E R A T E U R    AFFE_CARA_ELEM

C ----------------------------------------------------------------------
C     NBEPO  : NOMBRE D'ELEMENTS DE TYPE "POUTRE"
C     NBEDI  : NOMBRE D'ELEMENTS DE TYPE "DISCRET"
C     NBECO  : NOMBRE D'ELEMENTS DE TYPE "COQUE"
C     NBECA  : NOMBRE D'ELEMENTS DE TYPE "CABLE"
C     NBEBA  : NOMBRE D'ELEMENTS DE TYPE "BARRE"
C     NBEMA  : NOMBRE D'ELEMENTS DE TYPE "MASSIF"
C     NBEGB  : NOMBRE D'ELEMENTS DE TYPE "GRILLE"
C     NBMCF  : NOMBRE DE MOTS CLES FACTEUR DE L'OPERATEUR
C ----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32 JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------


      PARAMETER (NBEPO=13,NBEDI=8,NBECO=26,NBECA=2)
      PARAMETER (NBEBA=2,NBEMA=46,NBEGB=5)
      PARAMETER (NBTEL=NBEPO+NBEDI+NBECO+NBECA+NBEBA+NBEMA+NBEGB)
      PARAMETER (NBMCF=13,NBEL1=51,NBEL2=51)

      INTEGER NBMCLE(NBMCF),NBOCC(NBMCF),IVR(3)
      INTEGER NTYELE(NBTEL),NOCADI(3),NMTGDI(3)
      CHARACTER*1 K1BID
      CHARACTER*6 KIOC
      CHARACTER*8 VER(3),NOMU,NOMO,NOMA
      CHARACTER*16 CONCEP,CMD,MCLF(NBMCF),MCLE(4)
      CHARACTER*16 NOMELE(NBTEL),NOMEL1(NBEL1),NOMEL2(NBEL2)
      CHARACTER*24 MLGNMA,MLGNNO,MLGGNO,MLGGMA
      CHARACTER*24 MODNOM,MODNEM,NOMOBJ,TMPLST,TMPLMA,TMPLNO
      CHARACTER*19 CARTCF
      CHARACTER*24 TMPNCF
      INTEGER NBCART

      DATA MCLE/'GROUP_MA        ','MAILLE          ',
     &     'GROUP_NO        ','NOEUD           '/

      DATA MCLF/'POUTRE          ','COQUE           ',
     &     'DISCRET         ','ORIENTATION     ','DEFI_ARC        ',
     &     'CABLE           ','BARRE           ','MASSIF          ',
     &     'POUTRE_FLUI     ','RIGI_PARASOL    ',
     &     'GRILLE          ','RIGI_MISS_3D    ','DISCRET_2D      '/

      DATA NOMEL1/'MECA_POU_D_T    ','MECA_POU_D_E    ',
     &     'MECA_POU_D_T_GD ','MECA_POU_C_T    ',
     &     'MEFS_POU_D_T    ','MECA_POU_D_TG   ','MECA_POHO_HEXA8 ',
     &     'MECA_POHO_HEXA20','MET3SEG3        ','MET6SEG3        ',
     &     'MET3SEG4','MECA_POU_D_EM   ','MECA_POU_D_TGM  ',
     &     'MECA_DIS_T_N    ','MECA_DIS_T_L    ',
     &     'MECA_DIS_TR_N   ','MECA_DIS_TR_L   ','MECA_2D_DIS_T_N ',
     &     'MECA_2D_DIS_T_L ','MECA_2D_DIS_TR_N','MECA_2D_DIS_TR_L',
     &     'THCOTR3         ','THCOTR6         ','THCOQU4         ',
     &     'THCOQU8         ','THCOTR7         ','THCOQU9         ',
     &     'MEDKTR3         ','MEDSTR3         ','MEDKQU4         ',
     &     'MEDSQU4         ','MEQ4QU4         ','MECXSE3         ',
     &     'MEDKTG3         ','MEDKQG4         ',
     &     'METCSE3         ','METDSE3         ','THCASE3         ',
     &     'THCPSE3         ','MEC3QU9H        ','MEC3TR7H        ',
     &     'MEBODKT         ','MEBODST         ','MEBOQ4G         ',
     &     'MEBOCQ3         ','THCOSE3         ','THCOSE2         ',
     &     'MECABL2         ','MEPOULI         ','MECA_BARRE      ',
     &     'MECA_2D_BARRE   '/

      DATA NOMEL2/'MECA_HEXA8      ','MECA_PENTA6     ',
     &     'MECA_TETRA4     ','MECA_HEXA27     ','MECA_HEXA20     ',
     &     'MECA_PENTA15    ','MECA_TETRA10    ','MECA_PYRAM5     ',
     &     'MECA_PYRAM13    ','MEAXTR3         ','MEAXQU4         ',
     &     'MEAXTR6         ','MEAXQU8         ','MEAXQU9         ',
     &     'MEDPTR3         ','MEDPQU4         ','MEDPTR6         ',
     &     'MEDPQU8         ','MEDPQU9         ','MECPTR3         ',
     &     'MECPQU4         ','MECPTR6         ','MECPQU8         ',
     &     'MECPQU9         ','THER_HEXA8      ','THER_PENTA6     ',
     &     'THER_TETRA4     ','THER_PYRAM5     ','THER_HEXA27     ',
     &     'THER_HEXA20     ','THER_PENTA15    ','THER_TETRA10    ',
     &     'THER_PYRAM13    ','THAXTR3         ','THAXQU4         ',
     &     'THAXTR6         ','THAXQU8         ','THAXQU9         ',
     &     'THPLTR3         ','THPLQU4         ','THPLTR6         ',
     &     'THPLQU8         ','THPLQU9         ','MET3SEG3        ',
     &     'MET6SEG3        ','MET3SEG4        ',
     &     'MEGRDKT         ','MEGMTR3         ','MEGMQU4         ',
     &     'MEGMTR6         ','MEGMQU8         '/
      DATA NBMCLE/2,2,4,4,2,2,2,2,2,1,2,0,4/
C     ------------------------------------------------------------------

      CALL JEMARQ()

      IRET=0
C --- INITIALISATION DE  NOMELE
      DO 10 I = 1,NBEL1
        NOMELE(I) = NOMEL1(I)
   10 CONTINUE
      DO 20 I = 1,NBEL2
        NOMELE(I+NBEL1) = NOMEL2(I)
   20 CONTINUE

C --- RECUPERATION DES ARGUMENTS  DE LA COMMANDE
      CALL GETRES(NOMU,CONCEP,CMD)

C --- VERIFICATIONS SUPPLEMENTAIRES DE SYNTAXE
      CALL GETVTX(' ','VERIF',1,1,2,VER,NBVER)
      DO 30 I = 1,3
        IVR(I) = 0
   30 CONTINUE
      IF (NBVER.GT.0) THEN
        DO 40 I = 1,NBVER
          IF (VER(I).EQ.'MAILLE  ') IVR(1) = 1
          IF (VER(I).EQ.'NOEUD   ') IVR(2) = 1
   40   CONTINUE
      ELSE IF (NBVER.LT.0) THEN
        CALL UTMESS('F',CMD,'VERIF : 2 ARGUMENTS MAXI')
      END IF

      DO 50 I = 1,NBMCF
        CALL GETFAC(MCLF(I),NBOCC(I))
   50 CONTINUE

C     VERIFICATION DE LA SYNTAXE DES ELEMENTS POUTRE :
C     ------------------------------------------------
      LXP = 0
      IF (NBOCC(1).NE.0) THEN
        CALL ACEVPO(NBOCC(1),NLM,NLG,IRET)
        LXP = MAX(NLM,NLG)
      END IF

C     VERIFICATION DE LA SYNTAXE DES ELEMENTS COQUE :
C     ------------------------------------------------
      LXC = 0
      IF (NBOCC(2).NE.0) THEN
        CALL ACEVCO(NBOCC(2),NLM,NLG,IRET)
        LXC = MAX(NLM,NLG)
      END IF

C     VERIFICATION DE LA SYNTAXE DES ORIENTATIONS DES ELEMENTS :
C     ----------------------------------------------------------
      LXO = 0
      IF (NBOCC(4).NE.0) THEN
        CALL ACEVOR(NBOCC(4),NLM,NLG,NLN,NLJ,IRET)
        LXO = MAX(NLM,NLN,NLJ,NLG)
      END IF

C     VERIFICATION DE LA SYNTAXE DES POUTRES COURBES :
C     ------------------------------------------------
      LXA = 0
      IF (NBOCC(5).NE.0) THEN
        CALL ACEVPC(NBOCC(5),NLM,NLG,IRET)
        LXA = MAX(NLM,NLG)
      END IF

C     VERIFICATION DE LA SYNTAXE DES ELEMENTS CABLE :
C     -----------------------------------------------
      LXK = 0
      IF (NBOCC(6).NE.0) THEN
        CALL ACEVCA(NBOCC(6),NLM,NLG,IRET)
        LXK = MAX(NLM,NLG)
      END IF

C     VERIFICATION DE LA SYNTAXE DES ELEMENTS BARRE :
C     ------------------------------------------------
      LXB = 0
      IF (NBOCC(7).NE.0) THEN
        CALL ACEVBA(NBOCC(7),NLM,NLG,IRET)
        LXB = MAX(NLM,NLG)
      END IF

C     VERIFICATION DE LA SYNTAXE DES ELEMENTS MASSIF :
C     ------------------------------------------------
      LXM = 0
      IF (NBOCC(8).NE.0) THEN
        CALL ACEVMA(NBOCC(8),NLM,NLG)
        LXM = MAX(NLM,NLG)
      END IF

C     VERIFICATION DE LA SYNTAXE DES ELEMENTS POUTRE_FLUI :
C     -----------------------------------------------------
      LXPF = 0
      IF (NBOCC(9).NE.0) THEN
        IF (NBOCC(1).EQ.0) THEN
          CALL UTMESS('F',CMD,'IL MANQUE LE MOT CLE FACTEUR'//'POUTRE.')
        END IF
        CALL ACEVPF(NBOCC(9),NLM,NLG)
        LXPF = MAX(NLM,NLG)
      END IF

C     VERIFICATION DE LA SYNTAXE DES ELEMENTS "GRILLE" :
C     --------------------------------------------------
      LXGB = 0
      IF (NBOCC(11).NE.0) THEN
        CALL ACEVGB(NBOCC(11),NLM,NLG)
        LXGB = MAX(NLM,NLG)
      END IF


C --- LONGUEUR MAXIMUM D UNE LISTE DE MAILLE/NOEUD/GROUP_MA/GROUP_NO :
C     ----------------------------------------------------------------
      LMAX = MAX(1,LXP,LXC,LXO,LXA,LXK,LXB,LXM,LXPF,LXGB)



C     RECUPERATION DU NIVEAU D'IMPRESSION
C     -----------------------------------
      CALL INFMAJ
      CALL INFNIV(IFM,NIV)
      IF (NIV.EQ.2) IVR(3) = 1

C --- RECONSTRUCTION DES NOMS JEVEUX DU CONCEPT MODELE
      CALL GETVID(' ','MODELE',1,1,1,NOMO,NBVM)
      MODNOM = NOMO//'.MODELE    .NOMA'
      MODNEM = NOMO//'.MODELE    .NEMA'

C --- RECUPERATION DU NOM DU MAILLAGE ASSOCIE
      CALL JEVEUO(MODNOM,'L',JDNM)
      NOMA = ZK8(JDNM)

C --- RECONSTRUCTION DES NOMS JEVEUX DU CONCEPT MAILLAGE ASSOCIE
      MLGNMA = NOMA//'.NOMMAI'
      MLGNNO = NOMA//'.NOMNOE'
      MLGGNO = NOMA//'.GROUPENO'
      MLGGMA = NOMA//'.GROUPEMA'

C     VERIFICATION DE LA SYNTAXE DES ELEMENTS DISCRET :
C     -------------------------------------------------
      LXD = 0
      IF (NBOCC(3).NE.0 .OR. NBOCC(13).NE.0) THEN
        NBOCCD = NBOCC(3) + NBOCC(13)
        CALL ACEVDI(NBOCCD,NOMA,NOMO,NLM,NLG,NLN,NLJ,IRET)
        LXD = MAX(NLM,NLN,NLG,NLJ)
        LMAX = MAX(LMAX,LXD)
      END IF

C     VERIFICATION DE LA DIMENSION DES RAIDEURS REPARTIES :
C     -------------------------------------------------
      LXRP = 0
      IF (NBOCC(10).NE.0) THEN
        CALL ACEVRP(NBOCC(10),NOMA,LXRP,NOEMAF,IRET)
        LMAX = MAX(LMAX,LXRP)
      END IF

C     VERIFICATION DE LA DIMENSION DES RAIDEURS MISS :
C     -------------------------------------------------
      LXRM = 0
      IF (NBOCC(12).NE.0) THEN
        CALL ACEVRM(NBOCC(12),NOMA,LXRM,NOEMF2,IRET)
        LMAX = MAX(LMAX,LXRM)
      END IF

C --- RECUPERATION DU NB DE MAILLES INITIALES (MAILLAGE)
      CALL JELIRA(MLGNMA,'NOMMAX',NBMAIL,K1BID)

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
      CALL WKVECT(TMPLST,'V V K8',LMAX,JDLS)

C --- RECUPERATION DES NUMEROS DES TYPES ELEMENTS
      DO 60 I = 1,NBTEL
        CALL JENONU(JEXNOM('&CATA.TE.NOMTE',NOMELE(I)),NTYELE(I))
   60 CONTINUE

C --- COMPTEUR D'ELEMENTS ET VERIFICATION COHERENCE DES AFFECTATIONS :
C     ----------------------------------------------------------------
      CALL ACECEL(NOMA,NOMO,NBOCC,NBEPO,NBEDI,NBECO,NBECA,NBEBA,NBEMA,
     &            NBTEL,NTYELE,NPOUTR,NDISCR,NCOQUE,NCABLE,
     &            NBARRE,NMASSI,NGRILL,NGRIBT,JDLM,JDLN,IRET)
      IF (IRET.NE.0) THEN
        CALL UTMESS('F',CMD,'ERREUR(S) RENCONTREE(S) LORS DE LA '//
     &              'VERIFICATION DES AFFECTATIONS.')
      END IF

C --- VERIFICATION DE L'EXISTENCE DES MAILLES/NOEUDS/GROUPES DECLARES
C     ---------------------------------------------------------------
      DO 100 ICLF = 1,NBMCF
        DO 90 IOC = 1,NBOCC(ICLF)
          CALL CODENT(IOC,'G',KIOC)
          DO 80 ICLE = 1,NBMCLE(ICLF)
            IF (ICLE.EQ.1) NOMOBJ = MLGGMA
            IF (ICLE.EQ.2) NOMOBJ = MLGNMA
            IF (ICLE.EQ.3) NOMOBJ = MLGGNO
            IF (ICLE.EQ.4) NOMOBJ = MLGNNO
            CALL GETVID(MCLF(ICLF),MCLE(ICLE),IOC,1,LMAX,ZK8(JDLS),NG)
            DO 70 J = 1,NG
              CALL JENONU(JEXNOM(NOMOBJ,ZK8(JDLS+J-1)),IRET)
              IF (IRET.EQ.0) THEN
                CALL UTMESS('A',CMD,MCLF(ICLF)//'OCCURENCE '//KIOC//
     &                      ' : LE '//MCLE(ICLE)//' "'//ZK8(JDLS+J-1)//
     &                      '" NE FAIT PAS PARTIE DU MAILLAGE "'//NOMA//
     &                      '"')
              END IF
   70       CONTINUE
   80     CONTINUE
   90   CONTINUE
  100 CONTINUE

C --- VERIFICATION DE LA BONNE  AFFECTATION  DES  CARACTERISTIQUES
C     POUR TOUTES LES MAILLES ET NOEUDS AFFECTES , IMPR SI DEMANDE
C     INCREMENTATION DES COMPTEURS D APPELS A NOCART(DISCRET,COQUE,
C     DEFI_ARC,CABLE,POUTRE,BARRE) :
C     -----------------
      IRET=0
      CALL ACEINC(NOMA,NOMO,NBMCF,MCLF,NTYELE,NBOCC,IVR,NBEPO,
     &            NBEDI,NBECO,NBECA,NBEBA,NBEMA,NBEGB,NBTEL,
     &            NOCACO,NOCAGB,JDLM,JDLN,LMAX,IRET)
      IF (IRET.NE.0) THEN
        CALL UTMESS('F',CMD,'UNE ERREUR D AFFECTATION A ETE '//
     &            'DETECTEE : CERTAINES MAILLES DEMANDEES POSSEDENT UN '
     &              //
     &           'TYPE ELEMENT INCOMPATIBLE AVEC LES DONNEES A AFFECTER'
     &              )
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
         CALL UTMESS('A','AFFE_CARA_ELEM',
     &                   'DES POUTRES NE SONT PAS AFFECTEES')
      ENDIF
      IF ( (NBOCC(7).EQ.0).AND.(NBARRE.NE.0) )THEN
         CALL UTMESS('A','AFFE_CARA_ELEM',
     &                   'DES BARRES NE SONT PAS AFFECTEES')
      ENDIF
      IF ( (NBOCC(6).EQ.0).AND.(NCABLE.NE.0) )THEN
         CALL UTMESS('A','AFFE_CARA_ELEM',
     &                   'DES CABLES NE SONT PAS AFFECTES')
      ENDIF

C --- AFFECTATION DES ORIENTATIONS AUX ELEMENTS POUTRES ET DISCRETS  ET
C     BARRES ET AFFECTATION DE LA CARTE ORIENTATION :
C     -----------------------------------------------
      IF (NBOCC(1).NE.0 .OR. NBOCC(3).NE.0 .OR. NBOCC(13).NE.0 .OR.
     &    NBOCC(7).NE.0 .OR. NBOCC(10).NE.0) THEN
        CALL ACEAOR(NOMA,NOMO,LMAX,NBEPO,NBEDI,NBTEL,NTYELE,NOMELE,IVR,
     &              IFM,NBOCC)
      END IF

C --- AFFECTATION DES CARACTERISTIQUES AUX ELEMENTS POUTRES :
C     -------------------------------------------------------
      IF (NBOCC(1).NE.0) THEN
        CALL ACEAPO(NOMA,NOMO,LMAX,NPOUTR,NBOCC(1),NBEPO,NTYELE,IVR,IFM,
     &              JDLM)
      END IF

C --- AFFECTATION DES EPAISSEURS/COURBURES/ANGLES AUX ELEMENTS COQUES :
C     -----------------------------------------------------------------
      IF (NBOCC(2).NE.0) THEN
        CALL ACEACO(NOMU,NOMA,LMAX,NOCAGB,NBOCC(2))
      END IF

C --- AFFECTATION DES MATRICES AUX ELEMENTS DISCRETS :
C     ------------------------------------------------
      IF (NBOCC(3).NE.0 .OR. NBOCC(13).NE.0) THEN
        NBOCCD = NBOCC(3) + NBOCC(13)
        CALL ACEADI(NOMA,NOMO,LMAX,NBOCCD,IVR,IFM)
      END IF

C --- AFFECTATION DES COURBURES AUX ELEMENTS POUTRES COURBES :
C     --------------------------------------------------------
      IF (NBOCC(5).NE.0) THEN
        CALL ACEAPC(NOMU,NOMA,LMAX,NBOCC(5))
      END IF

C --- AFFECTATION DES SECTIONS AUX ELEMENTS CABLE :
C     ---------------------------------------------
      IF (NBOCC(6).NE.0) THEN
        CALL ACEACA(NOMU,NOMA,LMAX,NBOCC(6))
      END IF

C --- AFFECTATION DES CARACTERISTIQUES AUX ELEMENTS BARRE :
C     -----------------------------------------------------
      IF (NBOCC(7).NE.0) THEN
        CALL ACEABA(NOMA,NOMO,LMAX,NBARRE,NBOCC(7),NBTEL,NTYELE,IVR,IFM,
     &              JDLM)
      END IF

C --- AFFECTATION DES REPERES AUX ELEMENTS THERMIQUES ET MECANIQUES  :
C     -----------------------------------------------------------------
      IF (NBOCC(8).NE.0) THEN
        CALL ACEAMA(NOMU,NOMA,LMAX,NBOCC(8))
      END IF

C --- AFFECTATION DES REPERES AUX ELEMENTS POUTRE_FLUI  :
C     ---------------------------------------------------
      IF (NBOCC(9).NE.0) THEN
        CALL ACEAPF(NOMU,NOMA,LMAX,NBOCC(9))
      END IF

C --- AFFECTATION DES MATRICES AUX RAIDEURS REPARTIES :
C     ------------------------------------------------
      IF (NBOCC(10).NE.0) THEN
        CALL ACEARP(NOMA,NOMO,LMAX,NOEMAF,NBOCC(10),IVR,IFM)
      END IF

C --- AFFECTATION DES CARACTERISTIQUES POUR L'ELEMENT "GRILLE"
C     --------------------------------------------------------
      IF (NBOCC(11).NE.0) THEN
        CALL ACEAGB(NOMU,NOMA,LMAX,NOCACO,NBOCC(11))
      END IF

C --- AFFECTATION DES MATRICES AUX RAIDEURS MISS :
C     ------------------------------------------------
      IF (NBOCC(12).NE.0) THEN
        CALL ACEARM(NOMA,NOMO,LMAX,NOEMF2,NBOCC(12),IVR,IFM)
      END IF

C     COMPACTAGE DE LA CARTE : '.CVENTCXF'
      IF (NBCART.GT.0) THEN
        CALL TECART(CARTCF)
C     DESTRUCTION DES CHAMPS
        TMPNCF = CARTCF//'.NCMP'
        CALL JEDETR(TMPNCF)
        TMPNCF = CARTCF//'.VALV'
        CALL JEDETR(TMPNCF)
      END IF

C --- TRAITEMENT DES MOTS CLES AFFE_SECT ET AFFE_FIBRE
C     + TRAITEMENT DES MOTS CLES :
C           COQUE  /COQUE_NCOU
C           GRILLE /COQUE_NCOU
C           POUTRE /TUYAU_NCOU
C           POUTRE /TUYAU_NSEC
C     ----------------------------------------------------------
      CALL PMFD00()

      CALL JEDEMA()
      END

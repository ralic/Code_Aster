      SUBROUTINE ACEVDI(NBOCC,NOMAZ,NOMOZ,NLM,NLG,NLN,NLJ,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           NBOCC,            NLM,NLG,NLN,NLJ,IER
      CHARACTER*(*)           NOMAZ,NOMOZ
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 19/10/2010   AUTEUR DELMAS J.DELMAS 
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
C ----------------------------------------------------------------------
C     AFFE_CARA_ELEM
C     VERIFICATION DES MOTS CLES POUR L'ELEMENT DISCRET
C ----------------------------------------------------------------------
C IN  : NBOCC  : NOMBRE D'OCCURENCE
C OUT : NLM    : NOMBRE TOTAL DE MAILLE
C OUT : NLG    : NOMBRE TOTAL DE GROUPE DE MAILLE
C ----------------------------------------------------------------------
C     NCD    : NOMBRE D'ARGUMENTS ADMIS POUR "CARA"
C     NRD    : NOMBRE D'ARGUMENTS ADMIS POUR "REPERE"
C     NID    : DIMENSION DU TABLEAU DES ARGUMENTS INCOMPATIBLES
C               SOUS  UN  MEME  MOT  CLE  "CARA"
C     COMDIS : TABLEAU DES ARGUMENTS INCOMPATIBLES SOUS "CARA" :
C               ( A M K N S T TR )
C ----------------------------------------------------------------------
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
C
      CHARACTER*4   TYPE
      CHARACTER*8   K8B, NOMU, NOMA, NOMO, NOMAIL, TYPEL, NOGRM
      CHARACTER*16  CONCEP, CMD, MCF
      CHARACTER*24  GRMAMA, MAILMA, CARA
      CHARACTER*24 VALK(4)
C     ------------------------------------------------------------------
      CALL GETRES(NOMU,CONCEP,CMD)
C
      NOMA = NOMAZ
      NOMO = NOMOZ
      NLM = 0
      NLG = 0
      NLN = 0
      NLJ = 0
      I3D = 0
      I2D = 0
      MCF = ' '
      GRMAMA = NOMA//'.GROUPEMA'
      MAILMA = NOMA//'.NOMMAI'
C
C --- VECTEUR DU TYPE DES MAILLES DU MAILLAGE :
C     ---------------------------------------
      CALL JEVEUO(NOMA//'.TYPMAIL','L',IDTYMA)
C
C --- RECUPERATION DE LA DIMENSION DU MAILLAGE :
C     ----------------------------------------
      NDIM = 3
      CALL DISMOI('F','Z_CST',NOMOZ,'MODELE',IBID,K8B,IER)
      IF ( K8B(1:3) .EQ. 'OUI' )  NDIM = 2
C
C --- ON REGARDE SI LE MODELE COMPORTE DES ELEMENTS DISCRETS 3D :
C     ---------------------------------------------------------
      CALL MODEXI(NOMOZ,'DIS_',I3D)
C
C --- ON REGARDE SI LE MODELE COMPORTE DES ELEMENTS DISCRETS 2D :
C     ---------------------------------------------------------
      CALL MODEXI(NOMOZ,'2D_DIS_',I2D)
C
C --- ON INTERDIT SUR UN MAILLAGE 2D D'AVOIR DES ELEMENTS DISCRETS
C --- 2D ET 3D :
C     --------
      IF (I2D.EQ.1.AND.I3D.EQ.1.AND.NDIM.EQ.2) THEN
          CALL U2MESS('E','MODELISA_8')
          IER = IER + 1
      ENDIF
      IF (I2D.EQ.1) MCF = 'DISCRET_2D'
      IF (I3D.EQ.1) MCF = 'DISCRET'
C
C --- ON INTERDIT SUR UN MAILLAGE 3D D'AVOIR DES ELEMENTS DISCRETS 2D :
C     ---------------------------------------------------------------
      IF (I2D.EQ.1.AND.NDIM.EQ.3) THEN
          CALL U2MESS('E','MODELISA_9')
          IER = IER + 1
      ENDIF
C
      IF (I2D.EQ.0.AND.I3D.EQ.0) THEN
          CALL U2MESS('E','MODELISA_55')
          IER = IER + 1
      ENDIF
C
C --- BOUCLE SUR LES OCCURENCES :
C     -------------------------
      DO 10 IOC = 1,NBOCC
C
         CALL GETVTX(MCF,'CARA',IOC,1,1,CARA,NC)
C
         CALL GETVEM(NOMA,'GROUP_MA',MCF,'GROUP_MA',IOC,1,0,K8B,NG)
         CALL GETVEM(NOMA,'MAILLE'  ,MCF,'MAILLE'  ,IOC,1,0,K8B,NM)
         CALL GETVEM(NOMA,'GROUP_NO',MCF,'GROUP_NO',IOC,1,0,K8B,NJ)
         CALL GETVEM(NOMA,'NOEUD'   ,MCF,'NOEUD'   ,IOC,1,0,K8B,NN)
C
         NSOM = NG + NM + NJ + NN
         IF (NSOM.EQ.NG .OR. NSOM.EQ.NM .OR. NSOM.EQ.NJ
     &                                  .OR. NSOM.EQ.NN) THEN
            NLM = MAX(NLM,-NM)
            NLG = MAX(NLG,-NG)
            NLN = MAX(NLN,-NN)
            NLJ = MAX(NLJ,-NJ)
         ENDIF
C
C ------ VERIFICATION DU BON TYPE DE MAILLE EN FONCTION DE CARA :
C        ------------------------------------------------------
         IF ( CARA(2:7) .EQ. '_T_D_N'   .OR.
     &        CARA(2:8) .EQ. '_TR_D_N'  .OR.
     &        CARA(2:5) .EQ. '_T_N'     .OR.
     &        CARA(2:6) .EQ. '_TR_N'   ) THEN
            TYPE = 'POI1'
         ELSE
            TYPE = 'SEG2'
         ENDIF
C
         IF ( NM .NE. 0 ) THEN
            NBMAIL = -NM
            CALL WKVECT ( '&&ACEVDI.MAILLE', 'V V K8', NBMAIL, JMAIL )
            CALL GETVTX ( MCF, 'MAILLE', IOC,1,NBMAIL, ZK8(JMAIL), N1 )
            DO 12 IMA = 1, NBMAIL
               NOMAIL = ZK8(JMAIL+IMA-1)
               CALL JENONU(JEXNOM(MAILMA,NOMAIL),NUMA)
               NUTYMA = ZI(IDTYMA+NUMA-1)
               CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',NUTYMA),TYPEL)
               IF (TYPEL(1:4).NE.TYPE) THEN
                   VALK(1) = NOMAIL
                   VALK(2) = TYPE
                   VALK(3) = TYPEL
                   VALK(4) = CARA
                   CALL U2MESK('F','MODELISA_56', 4 ,VALK)
               ENDIF
 12         CONTINUE
            CALL JEDETR ( '&&ACEVDI.MAILLE' )
         ENDIF
C
         IF ( NG .NE. 0 ) THEN
            NBGRM = -NG
            CALL WKVECT ( '&&ACEVDI.GROUP_MA', 'V V K8', NBGRM, JGRM )
            CALL GETVTX ( MCF, 'GROUP_MA', IOC,1,NBGRM, ZK8(JGRM), N1 )
            DO 14 IG = 1, NBGRM
               NOGRM = ZK8(JGRM+IG-1)
               CALL JELIRA(JEXNOM(GRMAMA,NOGRM),'LONUTI',NBMAIL,K8B)
               CALL JEVEUO(JEXNOM(GRMAMA,NOGRM),'L',JMAIL)
               DO 16 IMA = 1 , NBMAIL
                  NUMA = ZI(JMAIL+IMA-1)
                  NUTYMA = ZI(IDTYMA+NUMA-1)
                  CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',NUTYMA),TYPEL)
                  IF (TYPEL(1:4).NE.TYPE) THEN
                     CALL JENUNO(JEXNUM(MAILMA,NUMA),NOMAIL)
                   VALK(1) = NOMAIL
                   VALK(2) = TYPE
                   VALK(3) = TYPEL
                   VALK(4) = CARA
                   CALL U2MESK('F','MODELISA_56', 4 ,VALK)
                  ENDIF
 16            CONTINUE
 14         CONTINUE
            CALL JEDETR ( '&&ACEVDI.GROUP_MA' )
         ENDIF
C
 10   CONTINUE
C
      LMAX2 = MAX(1,NLM,NLG,NLN,NLJ)
      CALL ACEVD2 ( NOMA, NOMO, LMAX2, NBOCC )
C
      END

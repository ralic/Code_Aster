      SUBROUTINE ACNOCE(NOMA,TYPE,LISTE,NB,COOR,RC,XCEN,TOLE,V1,ISPV)
      IMPLICIT  REAL*8  ( A-H , O-Z )
      REAL*8 COOR(*), XCEN(3), RC, TOLE, V1(3)
      CHARACTER*8         NOMA, LISTE(*)
      CHARACTER*4         TYPE
      INTEGER NB, ISPV
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 29/03/2010   AUTEUR PELLET J.PELLET 
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
C    VERIFICATION QUE LES MAILLES SEG2 DE LA LISTE SONT TOUTES ORIENTEES
C    DANS LE MEME SENS SUR LE CERCLE. ON TESTE PAR RAPPORT A V1
C     UTILISE PAR DEFI_ARC
C ----------------------------------------------------------------------
C IN  : NOMA   : NOM DU MAILLAGE
C IN  : TYPE   : 'TOUT', 'GRMA', 'LIMA'
C IN  : LISTE  : VECTEUR DE K8( NB) : LISTE DES MAILLES OU GROUPES
C IN  : NB     : DIMENSION DE LISTE
C IN  : COOR   : NOEUD EXTREMITE DE L'ENSEMBLE DES MAILLES
C IN  : RC     : RAYON DU CERCLE
C IN  : XCEN   : COORDONNES DU CENTRE DU CERCLE
C IN  : TOLE   : PRECISION DE LA VERIF
C IN  : V1     : VECTEUR DE REFERENCE
C OUT : ISPV   : SIGNE DU PRODUIT MIXTE  (XCEN-N1).(XCEN-N2),V1
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      INTEGER VALI
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      CHARACTER*8 C8
      CHARACTER*24 MLGGMA, MLGNMA, MLGCNX,C24
      CHARACTER*24 VALK
      REAL*8 X1(3), X2(3), XC1(3), XC2(3), PVEC(3)
C     ------------------------------------------------------------------
      CALL JEMARQ()
      MLGGMA = NOMA//'.GROUPEMA'
      MLGNMA = NOMA//'.NOMMAI'
      MLGCNX = NOMA//'.CONNEX'
      ISPV = 0
      IF (TYPE.EQ.'TOUT') THEN
         CALL DISMOI('C','NB_MA_MAILLA',NOMA,'MAILLAGE',NBM,C24,IR)
         DO 52 IM = 1,NBM
            CALL JEVEUO(JEXNUM(MLGCNX,IM),'L',JDNO)
            NN1 = ZI(JDNO)
            NN2 = ZI(JDNO+1)
            DO 42 I = 1 , 3
               X1(I) = COOR((NN1-1)*3+I)
               X2(I) = COOR((NN2-1)*3+I)
               XC1(I) = X1(I)-XCEN(I)
               XC2(I) = X2(I)-XCEN(I)
 42         CONTINUE
            XRC1 = PADIST( 3, X1, XCEN )
            XRC2 = PADIST( 3, X2, XCEN )
            IF ((ABS(XRC1-RC).GT.TOLE).OR.(ABS(XRC2-RC).GT.TOLE)) THEN
               VALI = IM
               VALK = ' '
               CALL U2MESG('E', 'MODELISA8_13',1,VALK,1,VALI,0,0.D0)
            ENDIF
            CALL PROVEC(XC1,XC2,PVEC)
            PS=DDOT(3,PVEC,1,V1,1)
            IF (PS.GT.0.D0) THEN
               IF(ISPV.EQ.0) THEN
                  ISPV = 1
               ELSE IF (ISPV.NE.1) THEN
               VALI = IM
               VALK = ' '
                  CALL U2MESG('E', 'MODELISA8_14',1,VALK,1,VALI,0,0.D0)
               ENDIF
            ELSE IF (PS.LT.0.D0) THEN
               IF(ISPV.EQ.0) THEN
                  ISPV = -1
               ELSE IF (ISPV.NE.-1) THEN
               VALI = IM
               VALK = ' '
                  CALL U2MESG('E', 'MODELISA8_14',1,VALK,1,VALI,0,0.D0)
               ENDIF
            ELSE
               VALI = IM
               VALK = ' '
               CALL U2MESG('E', 'MODELISA8_16',1,VALK,1,VALI,0,0.D0)
            ENDIF
  52     CONTINUE
      ELSE IF (TYPE.EQ.'GRMA') THEN
         DO 53 IG = 1,NB
            CALL JEVEUO(JEXNOM(MLGGMA,LISTE(IG)),'L',JDGM)
            CALL JELIRA(JEXNOM(MLGGMA,LISTE(IG)),'LONMAX',NM,C8)
            DO 54 IM = 1,NM
               IMG = ZI(JDGM+IM-1)
               CALL JEVEUO(JEXNUM(MLGCNX,IMG),'L',JDNO)
               NN1 = ZI(JDNO)
               NN2 = ZI(JDNO+1)
            DO 44 I = 1 , 3
               X1(I) = COOR((NN1-1)*3+I)
               X2(I) = COOR((NN2-1)*3+I)
               XC1(I) = X1(I)-XCEN(I)
               XC2(I) = X2(I)-XCEN(I)
 44         CONTINUE
            XRC1 = PADIST( 3, X1, XCEN )
            XRC2 = PADIST( 3, X2, XCEN )
            IF ((ABS(XRC1-RC).GT.TOLE).OR.(ABS(XRC2-RC).GT.TOLE)) THEN
               VALI = IM
               VALK = ' '
               CALL U2MESG('E', 'MODELISA8_13',1,VALK,1,VALI,0,0.D0)
            ENDIF
            CALL PROVEC(XC1,XC2,PVEC)
            PS=DDOT(3,PVEC,1,V1,1)
            IF (PS.GT.0.D0) THEN
               IF(ISPV.EQ.0) THEN
                  ISPV = 1
               ELSE IF (ISPV.NE.1) THEN
               VALI = IM
               VALK = ' '
                  CALL U2MESG('E', 'MODELISA8_14',1,VALK,1,VALI,0,0.D0)
               ENDIF
            ELSE IF (PS.LT.0.D0) THEN
               IF(ISPV.EQ.0) THEN
                  ISPV = -1
               ELSE IF (ISPV.NE.-1) THEN
               VALI = IM
               VALK = ' '
                  CALL U2MESG('E', 'MODELISA8_14',1,VALK,1,VALI,0,0.D0)
               ENDIF
            ELSE
               VALI = IM
               VALK = ' '
               CALL U2MESG('E', 'MODELISA8_16',1,VALK,1,VALI,0,0.D0)
            ENDIF
  54        CONTINUE
  53     CONTINUE
      ELSE IF (TYPE.EQ.'LIMA') THEN
         DO 55 IM = 1,NB
            CALL JENONU(JEXNOM(MLGNMA,LISTE(IM)),NUMMAI)
            CALL JEVEUO(JEXNUM(MLGCNX,NUMMAI),'L',JDNO)
            NN1 = ZI(JDNO)
            NN2 = ZI(JDNO+1)
            DO 45 I = 1 , 3
               X1(I) = COOR((NN1-1)*3+I)
               X2(I) = COOR((NN2-1)*3+I)
               XC1(I) = X1(I)-XCEN(I)
               XC2(I) = X2(I)-XCEN(I)
 45         CONTINUE
            XRC1 = PADIST( 3, X1, XCEN )
            XRC2 = PADIST( 3, X2, XCEN )
            IF ((ABS(XRC1-RC).GT.TOLE).OR.(ABS(XRC2-RC).GT.TOLE)) THEN
               VALI = IM
               VALK = ' '
               CALL U2MESG('E', 'MODELISA8_13',1,VALK,1,VALI,0,0.D0)
            ENDIF
            CALL PROVEC(XC1,XC2,PVEC)
            PS=DDOT(3,PVEC,1,V1,1)
            IF (PS.GT.0.D0) THEN
               IF(ISPV.EQ.0) THEN
                  ISPV = 1
               ELSE IF (ISPV.NE.1) THEN
               VALI = IM
               VALK = ' '
                  CALL U2MESG('E', 'MODELISA8_14',1,VALK,1,VALI,0,0.D0)
               ENDIF
            ELSE IF (PS.LT.0.D0) THEN
               IF(ISPV.EQ.0) THEN
                  ISPV = -1
               ELSE IF (ISPV.NE.-1) THEN
               VALI = IM
               VALK = ' '
                  CALL U2MESG('E', 'MODELISA8_14',1,VALK,1,VALI,0,0.D0)
               ENDIF
            ELSE
               VALI = IM
               VALK = ' '
               CALL U2MESG('E', 'MODELISA8_16',1,VALK,1,VALI,0,0.D0)
            ENDIF
  55     CONTINUE
      ENDIF
      CALL JEDEMA()
      END

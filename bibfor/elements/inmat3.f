      SUBROUTINE INMAT3(NDIM,NNO,NNOS,NBFPG,ELREFE,X,NBPG)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 09/10/2002   AUTEUR VABHHTS J.PELLET 
C RESPONSABLE VABHHTS J.PELLET
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.


C ======================================================================
C     ASTER INFORMATIONS:
C       11/09/02 (OB): CORRECTION INIT LUMPE SI NON THM.
C----------------------------------------------------------------------
C TOLE CRP_20
      IMPLICIT NONE

C BUT: ROUTINE D'INITIALISATION DES MATRICES DE PASSAGE DES POINTS
C      DE GAUSS AUX NOEUDS SOMMETS
C           &INEL.//ELREFE//.A  : MATRICE CARREE
C           &INEL.//ELREFE//.B  : MATRICE RECTANGULAIRE

C ENTREES:
C        ---> NDIM        : DIMENSION DE L'ESPACE DE REFERENCE
C        ---> NNO         : NOMBRE DE NOEUDS DE L'ELEMENT
C        ---> NNOS        : NOMBRE DE SOMMETS DE L'ELEMENT
C        ---> NBFPG       : NOMBRE DE FAMILLE DE POINTS DE GAUSS
C        ---> ELREFE       : NOM DE L'ELEMENT
C        ---> XSO,YSO,ZSO : COORDONNEES DES SOMMETS
C        ---> NBPG        : TABLEAU DES NOMBRES DE PTS DE GAUSS

      CHARACTER*8 ELREFE
      CHARACTER*16 ELREFL
      CHARACTER*24 CHCTE,CHVAL,CHMAT1,CHMAT2

      REAL*8 X(3),XG(3),COOPG(3*27),POIPG(27)
      REAL*8 FF(27),AA,BB,CC,DD,ZERO
      REAL*8 M(64),N(216),PP(6,6)
      INTEGER NBPG(10),NBPG1,IRET1,IRET2,DIMA,DIMB
      INTEGER NDIM,NNO,NNOS,NBFPG

C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------

      CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL,FAUX
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)

C --- FIN DECLARATIONS NORMALISEES JEVEUX ----------------------------

      INTEGER JMAT,JMATSI,I,KP,LF,J,LM,L,LN,NBPG3,JIN,NNO1
      INTEGER IPOIDS,IVF,NPG3,JVAL,JM,JN
C DEB ------------------------------------------------------------------

      CALL JEMARQ()
      FAUX=.FALSE.

      ZERO = 0.D0

      ELREFL = ELREFE
      CHMAT1 = '&INEL.'//ELREFL//'.A'
      CHMAT2 = '&INEL.'//ELREFL//'.B'

      DIMA = NNOS*NNOS + 2


      CALL JEEXIN('&INEL.'//ELREFL//'.A',IRET1)
      IF (IRET1.EQ.0) THEN
        CALL WKVECT(CHMAT1,'G V R',DIMA,JMAT)
      ELSE
        CALL JEVEUO(CHMAT1,'E',JMAT)
      END IF
      ZR(JMAT-1+1) = NNOS




C     ------------------------------------------------------------------
      IF ((ELREFE.EQ.'TETRA4'.OR.ELREFE.EQ.'TETRA10'.OR.
     &    ELREFE.EQ.'TETRI4'.OR.ELREFE.EQ.'TETRI10')) THEN

        NBPG1 = NBPG(1)
        DIMB = NNOS*NBPG1 + 2
        CALL JEEXIN('&INEL.'//ELREFL//'.B',IRET2)
        IF (IRET2.EQ.0) THEN
          CALL WKVECT(CHMAT2,'G V R',DIMB,JMATSI)
        ELSE
          CALL JEVEUO(CHMAT2,'E',JMATSI)
        END IF
        ZR(JMATSI-1+1) = NNOS
        ZR(JMAT-1+2) = NNOS
        JMAT = JMAT + 2
        ZR(JMATSI-1+2) = NBPG1
        JMATSI = JMATSI + 2

        AA = (1.D0-SQRT(5.D0))/4.D0
        BB = (1.D0+3.D0*SQRT(5.D0))/4.D0
        DO 10 I = 1,16
          ZR(JMAT-1+I) = AA
   10   CONTINUE
        ZR(JMAT+2) = BB
        ZR(JMAT+5) = BB
        ZR(JMAT+8) = BB
        ZR(JMAT+15) = BB

        IF (ELREFE.EQ.'TETRA10' .OR. ELREFE.EQ.'TETRI10') THEN
          CALL ELRFGF(ELREFE,1,NBPG,3,COOPG,27,POIPG)
          DO 20 I = 1,NNOS*NNOS
            M(I) = 0.D0
   20     CONTINUE
          DO 50 KP = 0,NBPG(1) - 1
            XG(1) = COOPG(3*KP+1)
            XG(2) = COOPG(3*KP+2)
            XG(3) = COOPG(3*KP+3)
            CALL ELRFVF('TETRA4  ',XG,27,FF,NNO)
            LN = KP*NNOS
            DO 40 I = 1,NNOS
              N(LN+I) = FF(I)
              DO 30 J = 1,NNOS
                LM = NNOS* (I-1) + J
                M(LM) = M(LM) + FF(I)*FF(J)
   30         CONTINUE
   40       CONTINUE
   50     CONTINUE

          CALL MGAUSS(M,N,NNOS,NNOS,NBPG(1),ZERO,FAUX)
          DO 70 I = 1,NNOS
            L = (I-1)*NBPG(1)
            DO 60 KP = 1,NBPG(1)
              LN = (KP-1)*NNOS
              ZR(JMATSI-1+L+KP) = N(LN+I)
   60       CONTINUE
   70     CONTINUE

        END IF

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'PENTA6') THEN

        NBPG1 = NBPG(1)
        DIMB = NNOS*NBPG1 + 2
        CALL JEEXIN('&INEL.'//ELREFL//'.B',IRET2)
        IF (IRET2.EQ.0) THEN
          CALL WKVECT(CHMAT2,'G V R',DIMB,JMATSI)
        ELSE
          CALL JEVEUO(CHMAT2,'E',JMATSI)
        END IF
        ZR(JMATSI-1+1) = NNOS
        ZR(JMAT-1+2) = NNOS
        ZR(JMATSI-1+2) = NBPG1
        JMAT = JMAT + 2
        JMATSI = JMATSI + 2

        AA = (1.D0+SQRT(3.D0))/2.D0
        BB = 1.D0 - AA
        DO 90 I = 1,3
          DO 80 J = 1,3
            PP(I,J) = AA
   80     CONTINUE
   90   CONTINUE
        PP(1,2) = -AA
        PP(2,3) = -AA
        PP(3,1) = -AA
        DO 110 I = 4,6
          DO 100 J = 1,3
            PP(I,J) = BB
  100     CONTINUE
  110   CONTINUE
        PP(4,2) = -BB
        PP(5,3) = -BB
        PP(6,1) = -BB
        DO 130 I = 1,3
          DO 120 J = 4,6
            PP(I,J) = PP(I+3,J-3)
  120     CONTINUE
  130   CONTINUE
        DO 150 I = 4,6
          DO 140 J = 4,6
            PP(I,J) = PP(I-3,J-3)
  140     CONTINUE
  150   CONTINUE

        DO 170 I = 1,NNOS
          L = (I-1)*NNOS
          DO 160 J = 1,NNOS
            ZR(JMAT-1+L+J) = PP(I,J)
  160     CONTINUE
  170   CONTINUE


C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'PENTA15') THEN

        NBPG3 = NBPG(3)
        DIMB = NNOS*NBPG3 + 2
        CALL JEEXIN('&INEL.'//ELREFL//'.B',IRET2)
        IF (IRET2.EQ.0) THEN
          CALL WKVECT(CHMAT2,'G V R',DIMB,JMATSI)
        ELSE
          CALL JEVEUO(CHMAT2,'E',JMATSI)
        END IF
        ZR(JMATSI-1+1) = NNOS
        ZR(JMAT-1+2) = NBPG3
        ZR(JMATSI-1+2) = NBPG3
        JMAT = JMAT + 2
        JMATSI = JMATSI + 2

        CHCTE = '&INEL.'//ELREFE//'.CARACTE'
        CALL JEVEUO(CHCTE,'L',JIN)

        NDIM = ZI(JIN+1-1)
        NNO1 = ZI(JIN+2-1)
        NBFPG = ZI(JIN+3-1)
        DO 180 I = 1,NBFPG
          NBPG(I) = ZI(JIN+3-1+I)
  180   CONTINUE
        NNOS = ZI(JIN+3-1+NBFPG+1)
        NPG3 = NBPG(3)

        CHVAL = '&INEL.'//ELREFE//'.FFORMES'
        CALL JEVEUO(CHVAL,'L',JVAL)


        IPOIDS = JVAL + (NDIM+1)*NNO1*NNO1
        IPOIDS = IPOIDS + NBPG(1) + (NDIM+1)*NNO1*NBPG(1) + NBPG(2) +
     &           (NDIM+1)*NNO1*NBPG(2)
        IVF = IPOIDS + NPG3

C   CALCUL DE LA MATRICE M

        DO 190 I = 1,NNOS*NNOS
          M(I) = 0.D0
  190   CONTINUE

C    BOUCLE   SUR LES POINTS DE GAUSS

        DO 220 KP = 1,NPG3
          LF = (KP-1)*NNOS
          DO 210 I = 1,NNOS
            N(LF+I) = ZR(IVF+LF+I-1)
            DO 200 J = 1,NNOS
              LM = NNOS* (I-1) + J
              M(LM) = M(LM) + ZR(IVF+LF+I-1)*ZR(IVF+LF+J-1)
  200       CONTINUE
  210     CONTINUE
  220   CONTINUE

        CALL MGAUSS(M,N,NNOS,NNOS,NPG3,ZERO,FAUX)

        DO 240 I = 1,NNOS
          L = (I-1)*NPG3
          DO 230 KP = 1,NPG3
            LN = (KP-1)*NNOS
            ZR(JMATSI-1+L+KP) = N(LN+I)
  230     CONTINUE
  240   CONTINUE


C     ------------------------------------------------------------------
      ELSE IF ((ELREFE.EQ.'HEXA8'.OR.ELREFE.EQ.'HEXA20'.OR.
     &         ELREFE.EQ.'HEXS20'.OR.ELREFE.EQ.'HEXI20'.OR.
     &         ELREFE.EQ.'HEXI8'.OR.ELREFE.EQ.'HEXA27')) THEN

        NBPG1 = NBPG(1)
        DIMB = NNOS*NBPG1 + 2
        CALL JEEXIN('&INEL.'//ELREFL//'.B',IRET2)
        IF (IRET2.EQ.0) THEN
          CALL WKVECT(CHMAT2,'G V R',DIMB,JMATSI)
        ELSE
          CALL JEVEUO(CHMAT2,'E',JMATSI)
        END IF
        ZR(JMATSI-1+1) = NNOS
        ZR(JMAT-1+2) = NNOS
        JMAT = JMAT + 2
        ZR(JMATSI-1+2) = NBPG1
        JMATSI = JMATSI + 2

        AA = (5.D0+3.D0*SQRT(3.D0))/4.D0
        BB = (-1.D0-SQRT(3.D0))/4.D0
        CC = (-1.D0+SQRT(3.D0))/4.D0
        DD = (5.D0-3.D0*SQRT(3.D0))/4.D0

        ZR(JMAT-1+1) = AA
        ZR(JMAT-1+2) = BB
        ZR(JMAT-1+3) = BB
        ZR(JMAT-1+4) = CC
        ZR(JMAT-1+5) = BB
        ZR(JMAT-1+6) = CC
        ZR(JMAT-1+7) = CC
        ZR(JMAT-1+8) = DD
        ZR(JMAT-1+9) = BB
        ZR(JMAT-1+10) = CC
        ZR(JMAT-1+11) = CC
        ZR(JMAT-1+12) = DD
        ZR(JMAT-1+13) = AA
        ZR(JMAT-1+14) = BB
        ZR(JMAT-1+15) = BB
        ZR(JMAT-1+16) = CC
        ZR(JMAT-1+17) = CC
        ZR(JMAT-1+18) = DD
        ZR(JMAT-1+19) = BB
        ZR(JMAT-1+20) = CC
        ZR(JMAT-1+21) = BB
        ZR(JMAT-1+22) = CC
        ZR(JMAT-1+23) = AA
        ZR(JMAT-1+24) = BB
        ZR(JMAT-1+25) = BB
        ZR(JMAT-1+26) = CC
        ZR(JMAT-1+27) = AA
        ZR(JMAT-1+28) = BB
        ZR(JMAT-1+29) = CC
        ZR(JMAT-1+30) = DD
        ZR(JMAT-1+31) = BB
        ZR(JMAT-1+32) = CC
        ZR(JMAT-1+33) = BB
        ZR(JMAT-1+34) = AA
        ZR(JMAT-1+35) = CC
        ZR(JMAT-1+36) = BB
        ZR(JMAT-1+37) = CC
        ZR(JMAT-1+38) = BB
        ZR(JMAT-1+39) = DD
        ZR(JMAT-1+40) = CC
        ZR(JMAT-1+41) = CC
        ZR(JMAT-1+42) = BB
        ZR(JMAT-1+43) = DD
        ZR(JMAT-1+44) = CC
        ZR(JMAT-1+45) = BB
        ZR(JMAT-1+46) = AA
        ZR(JMAT-1+47) = CC
        ZR(JMAT-1+48) = BB
        ZR(JMAT-1+49) = DD
        ZR(JMAT-1+50) = CC
        ZR(JMAT-1+51) = CC
        ZR(JMAT-1+52) = BB
        ZR(JMAT-1+53) = CC
        ZR(JMAT-1+54) = BB
        ZR(JMAT-1+55) = BB
        ZR(JMAT-1+56) = AA
        ZR(JMAT-1+57) = CC
        ZR(JMAT-1+58) = BB
        ZR(JMAT-1+59) = BB
        ZR(JMAT-1+60) = AA
        ZR(JMAT-1+61) = DD
        ZR(JMAT-1+62) = CC
        ZR(JMAT-1+63) = CC
        ZR(JMAT-1+64) = BB

        IF ((ELREFE.EQ.'HEXA20'.OR.ELREFE.EQ.'HEXA27'.OR.
     &      ELREFE.EQ.'HEXI20')) THEN

          CALL ELRFGF(ELREFE,1,NBPG,3,COOPG,27,POIPG)
          DO 250 I = 1,NNOS*NNOS
            M(I) = 0.D0
  250     CONTINUE
          DO 280 KP = 0,NBPG(1) - 1
            XG(1) = COOPG(3*KP+1)
            XG(2) = COOPG(3*KP+2)
            XG(3) = COOPG(3*KP+3)
            CALL ELRFVF('HEXA8   ',XG,27,FF,NNO)
            LN = KP*NNOS
            DO 270 I = 1,NNOS
              N(LN+I) = FF(I)
              DO 260 J = 1,NNOS
                LM = NNOS* (I-1) + J
                M(LM) = M(LM) + FF(I)*FF(J)
  260         CONTINUE
  270       CONTINUE
  280     CONTINUE

          CALL MGAUSS(M,N,NNOS,NNOS,NBPG(1),ZERO,FAUX)
          DO 300 I = 1,NNOS
            L = (I-1)*NBPG(1)
            DO 290 KP = 1,NBPG(1)
              LN = (KP-1)*NNOS
              ZR(JMATSI-1+L+KP) = N(LN+I)
  290       CONTINUE
  300     CONTINUE
        END IF


C     ------------------------------------------------------------------
      ELSE IF ((ELREFE.EQ.'PYRAM5'.OR.ELREFE.EQ.'PYRAM13')) THEN

        NBPG1 = NBPG(1)
        DIMB = NNOS*NBPG1 + 2
        CALL JEEXIN('&INEL.'//ELREFL//'.B',IRET2)
        IF (IRET2.EQ.0) THEN
          CALL WKVECT(CHMAT2,'G V R',DIMB,JMATSI)
        ELSE
          CALL JEVEUO(CHMAT2,'E',JMATSI)
        END IF
        ZR(JMATSI-1+1) = NNOS
        ZR(JMAT-1+2) = NBPG1
        ZR(JMATSI-1+2) = NBPG1
        JMAT = JMAT + 2
        JMATSI = JMATSI + 2

        CALL ELRFGF(ELREFE,1,NBPG,3,COOPG,27,POIPG)
        DO 310 I = 1,NNOS*NNOS
          M(I) = 0.D0
  310   CONTINUE
        DO 340 KP = 0,NBPG(1) - 1
          XG(1) = COOPG(3*KP+1)
          XG(2) = COOPG(3*KP+2)
          XG(3) = COOPG(3*KP+3)
          CALL ELRFVF('PYRAM5  ',XG,27,FF,NNO)
          LN = KP*NNOS
          DO 330 I = 1,NNOS
            N(LN+I) = FF(I)
            DO 320 J = 1,NNOS
              LM = NNOS* (I-1) + J
              M(LM) = M(LM) + FF(I)*FF(J)
  320       CONTINUE
  330     CONTINUE
  340   CONTINUE

        CALL MGAUSS(M,N,NNOS,NNOS,NBPG(1),ZERO,FAUX)

        IF (ELREFE.EQ.'PYRAM5') THEN
          DO 360 I = 1,NNOS
            L = (I-1)*NBPG(1)
            DO 350 KP = 1,NBPG(1)
              LN = (KP-1)*NNOS
              ZR(JMAT-1+L+KP) = N(LN+I)
  350       CONTINUE
  360     CONTINUE
        ELSE
          DO 380 I = 1,NNOS
            L = (I-1)*NBPG(1)
            DO 370 KP = 1,NBPG(1)
              LN = (KP-1)*NNOS
              ZR(JMATSI-1+L+KP) = N(LN+I)
  370       CONTINUE
  380     CONTINUE
        END IF


C     ------------------------------------------------------------
      ELSE IF ((ELREFE.EQ.'HEXD20') .OR. (ELREFE.EQ.'PENTA15D') .OR.
     &         (ELREFE.EQ.'PYRAM13D') .OR. (ELREFE.EQ.'TETRA10D')) THEN
C     ELEMENTS LUMPES : MATRICE A = IDENTITE
        ZR(JMAT-1+2) = NNOS
        JMAT = JMAT + 2
        DO 390 I = 1,NNOS*NNOS
          ZR(JMAT-1+I) = 0.D0
  390   CONTINUE
        DO 400 I = 1,NNOS
          ZR(JMAT+ (NNOS+1)* (I-1)) = 1.D0
  400   CONTINUE


C     ------------------------------------------------------------
      ELSE
        CALL ASSERT(.FALSE.)
      END IF

      CALL JEDEMA()

      END

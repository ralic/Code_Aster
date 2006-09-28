      SUBROUTINE NUADRF(NUAG1,NUAG2,IC1,IC2,DREF)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
      CHARACTER*19 NUAG1,NUAG2
      INTEGER IC1,IC2
      REAL*8 DREF(*)

C  BUT : CALCULER POUR TOUS LES POINTS DE NUAG2 UNE DISTANCE
C        DE REFERENCE POUR QU L'INTERPOLATION NE "CAPTE"
C        QU'UN NOMBRE LIMITE DE POINTS NE NUAG1 :
C        ON CHERCHE UNE INTERPOLATION LA PLUS LOCALE POSSIBLE
C
C        EN UN POINT DONNE IP2 DE NUAG2, ON ASSOCIE LA DISTANCE DREF
C        DREF EST TELLE QUE :
C                LA BOULE (IP2,SQRT(DREF)) CONTIENNE :
C                 . 2 POINTS EN 1D (NON CONFONDUS)
C                 . 3 POINTS EN 2D (NON ALIGNES)
C                 . 4 POINTS EN 3D (NON COPLANAIRES)
C
C IN/JXIN  NUAG1   : NUAGE A PROJETER
C IN/JXIN  NUAG2   : NUAGE A EVALUER
C IN       IC1     : NUMERO DE LA CMP DANS NUAG1
C IN       IC2     : NUMERO DE LA CMP DANS NUAG2
C OU       DREF    : VECTEUR QUI CONTIENDRA LES DISTANCE**2 CHERCHEES
C                    DIMENSION : NP2 = NOMBRE DE POINTS DE NUAG2
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C---------------- FIN COMMUNS NORMALISES  JEVEUX  ----------------------
C VARIABLES LOCALES :
      INTEGER INUAI1,INUAI2,INUAL1,INUAL2,INUAX1,INUAX2,IADM0
      INTEGER NP1,NP2,NX1,NX2,NC1,NC2,IP1,IP2,IM1,IM2,IM3,IM4
      REAL*8 X2,Y2,Z2,X1,Y1,Z1,XM1,YM1,ZM1
      REAL*8 D,DM0,DM,R8GAEM,L2,S,S2,V,L
      REAL*8 M1M2(3),M1M3(3),M1P1(3),N2(3),N(3),EPSABS
C
C DEB-------------------------------------------------------------------
      CALL JEMARQ()

      EPSABS=SQRT(1.D0/R8GAEM())

      CALL JEVEUO(NUAG1//'.NUAI','L',INUAI1)
      CALL JEVEUO(NUAG2//'.NUAI','L',INUAI2)
      CALL JEVEUO(NUAG1//'.NUAX','L',INUAX1)
      CALL JEVEUO(NUAG2//'.NUAX','L',INUAX2)
      CALL JEVEUO(NUAG1//'.NUAL','L',INUAL1)
      CALL JEVEUO(NUAG2//'.NUAL','L',INUAL2)

      NP1 = ZI(INUAI1-1+1)
      NP2 = ZI(INUAI2-1+1)
      NX1 = ZI(INUAI1-1+2)
      NX2 = ZI(INUAI2-1+2)
      NC1 = ZI(INUAI1-1+3)
      NC2 = ZI(INUAI2-1+3)


C     RECHERCHE DE LA PLUS GRANDE DISTANCE**2 ENTRE CHAQUE IP2
C     ET L'ENSEMBLE DES IP1 :
C     ------------------------------------------------------
      CALL WKVECT('&&NUADRF.DM0','V V R',NP2,IADM0)
      DO 1 ,IP2 = 1,NP2
        IF (.NOT.ZL(INUAL2-1+ (IP2-1)*NC2+IC2)) GO TO 1

C       -- DM0 EST LA PLUS GRANDE DISTANCE**2 ENTRE IP2 ET
C          L'ENSEMBLE DES IP1
        DM0 = 0.D0

        IF (NX1.EQ.1) THEN
          X2 = ZR(INUAX2-1+ (IP2-1)*NX2+1)
          DO 2,IP1 = 1,NP1
            IF (.NOT.ZL(INUAL1-1+ (IP1-1)*NC1+IC1)) GO TO 2
            X1 = ZR(INUAX1-1+ (IP1-1)*NX1+1)
            DM0 = MAX(DM0,(X2-X1)**2)
   2      CONTINUE
        ELSE IF (NX1.EQ.2) THEN
          X2 = ZR(INUAX2-1+ (IP2-1)*NX2+1)
          Y2 = ZR(INUAX2-1+ (IP2-1)*NX2+2)
          DO 3,IP1 = 1,NP1
            IF (.NOT.ZL(INUAL1-1+ (IP1-1)*NC1+IC1)) GO TO 3
            X1 = ZR(INUAX1-1+ (IP1-1)*NX1+1)
            Y1 = ZR(INUAX1-1+ (IP1-1)*NX1+2)
            DM0 = MAX(DM0,(X2-X1)**2+(Y2-Y1)**2)
   3      CONTINUE
        ELSE IF (NX1.EQ.3) THEN
          X2 = ZR(INUAX2-1+ (IP2-1)*NX2+1)
          Y2 = ZR(INUAX2-1+ (IP2-1)*NX2+2)
          Z2 = ZR(INUAX2-1+ (IP2-1)*NX2+3)
          DO 4,IP1 = 1,NP1
            IF (.NOT.ZL(INUAL1-1+ (IP1-1)*NC1+IC1)) GO TO 4
            X1 = ZR(INUAX1-1+ (IP1-1)*NX1+1)
            Y1 = ZR(INUAX1-1+ (IP1-1)*NX1+2)
            Z1 = ZR(INUAX1-1+ (IP1-1)*NX1+3)
            DM0 = MAX(DM0,(X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2)
   4      CONTINUE
        END IF

        IF (DM0.EQ.0.D0) GO TO 9994
        ZR(IADM0-1+IP2)=DM0
1     CONTINUE


      IF (NX1.EQ.1) THEN
C     ------------
        DO 10,IP2 = 1,NP2
          IF (.NOT.ZL(INUAL2-1+ (IP2-1)*NC2+IC2)) GO TO 10
          X2 = ZR(INUAX2-1+ (IP2-1)*NX2+1)

C         -- IM1 EST L'INDICE DU POINT LE + PROCHE DE IP2
          IM1 = 0
          DM = ZR(IADM0-1+IP2)
          DO 12,IP1 = 1,NP1
            IF (.NOT.ZL(INUAL1-1+ (IP1-1)*NC1+IC1)) GO TO 12
            X1 = ZR(INUAX1-1+ (IP1-1)*NX1+1)
            D = (X1-X2)**2
            IF (D.LE.DM) THEN
              DM = D
              IM1 = IP1
            END IF
   12     CONTINUE
          IF (IM1.EQ.0) GO TO 9995
          XM1=ZR(INUAX1-1+ (IM1-1)*NX1+1)

C         -- IM2 EST L'INDICE DU POINT LE + PROCHE DE IP2
C            ET DIFFERENT DE IM1
          IM2 = 0
          DM = ZR(IADM0-1+IP2)
          DO 13,IP1 = 1,NP1
            IF (.NOT.ZL(INUAL1-1+ (IP1-1)*NC1+IC1)) GO TO 13
            X1 = ZR(INUAX1-1+ (IP1-1)*NX1+1)
            IF ((X1-XM1)**2.LT.EPSABS) GO TO 13
            D = (X1-X2)**2
            IF (D.LE.DM) THEN
              DM = D
              IM2 = IP1
            END IF
   13     CONTINUE
          IF (IM2.EQ.0) GO TO 9996
          DREF(IP2) = DM

   10   CONTINUE
        GO TO 9999

      ELSE IF (NX1.EQ.2) THEN
C     ------------------
        DO 20,IP2 = 1,NP2
          IF (.NOT.ZL(INUAL2-1+ (IP2-1)*NC2+IC2)) GO TO 20
          X2 = ZR(INUAX2-1+ (IP2-1)*NX2+1)
          Y2 = ZR(INUAX2-1+ (IP2-1)*NX2+2)


C         -- IM1 EST L'INDICE DU POINT LE + PROCHE DE IP2
          IM1 = 0
          DM = ZR(IADM0-1+IP2)
          DO 22,IP1 = 1,NP1
            IF (.NOT.ZL(INUAL1-1+ (IP1-1)*NC1+IC1)) GO TO 22
            X1 = ZR(INUAX1-1+ (IP1-1)*NX1+1)
            Y1 = ZR(INUAX1-1+ (IP1-1)*NX1+2)
            D = (X2-X1)**2+(Y2-Y1)**2
            IF (D.LE.DM) THEN
              DM = D
              IM1 = IP1
            END IF
   22     CONTINUE
          IF (IM1.EQ.0) GO TO 9995
          XM1=ZR(INUAX1-1+ (IM1-1)*NX1+1)
          YM1=ZR(INUAX1-1+ (IM1-1)*NX1+2)

C         -- IM2 EST L'INDICE DU POINT LE + PROCHE DE IP2
C            ET DIFFERENT DE IM1
          IM2 = 0
          DM = ZR(IADM0-1+IP2)
          DO 23,IP1 = 1,NP1
            IF (.NOT.ZL(INUAL1-1+ (IP1-1)*NC1+IC1)) GO TO 23
            X1 = ZR(INUAX1-1+ (IP1-1)*NX1+1)
            Y1 = ZR(INUAX1-1+ (IP1-1)*NX1+2)
            IF ((X1-XM1)**2+(Y1-YM1)**2.LE.EPSABS) GO TO 23
            D = (X2-X1)**2+(Y2-Y1)**2
            IF (D.LE.DM) THEN
              DM = D
              IM2 = IP1
            END IF
   23     CONTINUE
          IF (IM2.EQ.0) GO TO 9996

C         VECTEUR M1M2 :
          M1M2(1)=ZR(INUAX1-1+(IM1-1)*NX1+1)-ZR(INUAX1-1+(IM2-1)*NX1+1)
          M1M2(2)=ZR(INUAX1-1+(IM1-1)*NX1+2)-ZR(INUAX1-1+(IM2-1)*NX1+2)
          L2=M1M2(1)**2+M1M2(2)**2

C         -- IM3 EST L'INDICE DU POINT M3 LE + PROCHE DE P2
C            DIFFERENT DE M1 ET M2 ET TEL QUE M1 M2 M3 FORMENT UN PLAN
          IM3 = 0
          DM = ZR(IADM0-1+IP2)
          DO 24,IP1 = 1,NP1
C           IF ((IP1.EQ.IM1).OR.(IP1.EQ.IM2)) GO TO 24
            IF (.NOT.ZL(INUAL1-1+ (IP1-1)*NC1+IC1)) GO TO 24
            X1 = ZR(INUAX1-1+ (IP1-1)*NX1+1)
            Y1 = ZR(INUAX1-1+ (IP1-1)*NX1+2)

C           SI LES POINTS M1 M2 ET P1 NE FORMENT PAS UN PLAN GO TO 24
            M1P1(1)=ZR(INUAX1-1+(IM1-1)*NX1+1)-X1
            M1P1(2)=ZR(INUAX1-1+(IM1-1)*NX1+2)-Y1
            S=ABS(M1M2(1)*M1P1(2)-M1M2(2)*M1P1(1))
            IF (S.LE.(1.D-3*L2)) GO TO 24
            D = (X2-X1)**2+(Y2-Y1)**2
            IF (D.LE.DM) THEN
              DM = D
              IM3 = IP1
            END IF
   24     CONTINUE
          IF (IM3.EQ.0) GO TO 9997
          DREF(IP2) = DM
   20   CONTINUE
        GO TO 9999


      ELSE IF (NX1.EQ.3) THEN
C     ------------------
        DO 30,IP2 = 1,NP2
          IF (.NOT.ZL(INUAL2-1+ (IP2-1)*NC2+IC2)) GO TO 30
          X2 = ZR(INUAX2-1+ (IP2-1)*NX2+1)
          Y2 = ZR(INUAX2-1+ (IP2-1)*NX2+2)
          Z2 = ZR(INUAX2-1+ (IP2-1)*NX2+3)


C         -- IM1 EST L'INDICE DU POINT LE + PROCHE DE IP2
          IM1 = 0
          DM = ZR(IADM0-1+IP2)
          DO 32,IP1 = 1,NP1
            IF (.NOT.ZL(INUAL1-1+ (IP1-1)*NC1+IC1)) GO TO 32
            X1 = ZR(INUAX1-1+ (IP1-1)*NX1+1)
            Y1 = ZR(INUAX1-1+ (IP1-1)*NX1+2)
            Z1 = ZR(INUAX1-1+ (IP1-1)*NX1+3)
            D = (X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2
            IF (D.LE.DM) THEN
              DM = D
              IM1 = IP1
            END IF
   32     CONTINUE
          IF (IM1.EQ.0) GO TO 9995
          XM1=ZR(INUAX1-1+ (IM1-1)*NX1+1)
          YM1=ZR(INUAX1-1+ (IM1-1)*NX1+2)
          ZM1=ZR(INUAX1-1+ (IM1-1)*NX1+3)

C         -- IM2 EST L'INDICE DU POINT LE + PROCHE DE IP2
C            ET DIFFERENT DE IM1
          IM2 = 0
          DM = ZR(IADM0-1+IP2)
          DO 33,IP1 = 1,NP1
            IF (.NOT.ZL(INUAL1-1+ (IP1-1)*NC1+IC1)) GO TO 33
            X1 = ZR(INUAX1-1+ (IP1-1)*NX1+1)
            Y1 = ZR(INUAX1-1+ (IP1-1)*NX1+2)
            Z1 = ZR(INUAX1-1+ (IP1-1)*NX1+3)
            IF ((X1-XM1)**2+(Y1-YM1)**2+(Z1-ZM1)**2.LE.EPSABS) GO TO 33
            D = (X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2
            IF (D.LE.DM) THEN
              DM = D
              IM2 = IP1
            END IF
   33     CONTINUE
          IF (IM2.EQ.0) GO TO 9996

C         -- VECTEUR M1M2 :
          M1M2(1)=ZR(INUAX1-1+(IM2-1)*NX1+1)-ZR(INUAX1-1+(IM1-1)*NX1+1)
          M1M2(2)=ZR(INUAX1-1+(IM2-1)*NX1+2)-ZR(INUAX1-1+(IM1-1)*NX1+2)
          M1M2(3)=ZR(INUAX1-1+(IM2-1)*NX1+3)-ZR(INUAX1-1+(IM1-1)*NX1+3)
          L2=M1M2(1)**2+M1M2(2)**2+M1M2(3)**2
          L=SQRT(L2)

C         -- IM3 EST L'INDICE DU POINT M3 LE + PROCHE DE P2
C            DIFFERENT DE M1 ET M2 ET TEL QUE M1 M2 M3 FORMENT UN PLAN
          IM3 = 0
          DM = ZR(IADM0-1+IP2)
          DO 34,IP1 = 1,NP1
C           IF ((IP1.EQ.IM1).OR.(IP1.EQ.IM2)) GO TO 34
            IF (.NOT.ZL(INUAL1-1+ (IP1-1)*NC1+IC1)) GO TO 34
            X1 = ZR(INUAX1-1+ (IP1-1)*NX1+1)
            Y1 = ZR(INUAX1-1+ (IP1-1)*NX1+2)
            Z1 = ZR(INUAX1-1+ (IP1-1)*NX1+3)

C           SI LES POINTS M1 M2 ET P1 NE FORMENT PAS UN PLAN GO TO 34
            M1P1(1)=ZR(INUAX1-1+(IM1-1)*NX1+1)-X1
            M1P1(2)=ZR(INUAX1-1+(IM1-1)*NX1+2)-Y1
            M1P1(3)=ZR(INUAX1-1+(IM1-1)*NX1+3)-Z1
            N2(1)=M1M2(2)*M1P1(3)-M1M2(3)*M1P1(2)
            N2(2)=M1M2(3)*M1P1(1)-M1M2(1)*M1P1(3)
            N2(3)=M1M2(1)*M1P1(2)-M1M2(2)*M1P1(1)
            S2=N2(1)**2+N2(2)**2+N2(3)**2

            IF (S2.LE.(1.D-3*L2)**2) GO TO 34
            D = (X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2
            IF (D.LE.DM) THEN
              DM = D
              IM3 = IP1
            END IF
   34     CONTINUE
          IF (IM3.EQ.0) GO TO 9997

C         -- VECTEUR M1M3 :
          M1M3(1)=ZR(INUAX1-1+(IM3-1)*NX1+1)-ZR(INUAX1-1+(IM1-1)*NX1+1)
          M1M3(2)=ZR(INUAX1-1+(IM3-1)*NX1+2)-ZR(INUAX1-1+(IM1-1)*NX1+2)
          M1M3(3)=ZR(INUAX1-1+(IM3-1)*NX1+3)-ZR(INUAX1-1+(IM1-1)*NX1+3)

C         -- N = M1M2 X M1M3 :
          N(1)=M1M2(2)*M1M3(3)-M1M2(3)*M1M3(2)
          N(2)=M1M2(3)*M1M3(1)-M1M2(1)*M1M3(3)
          N(3)=M1M2(1)*M1M3(2)-M1M2(2)*M1M3(1)
          S2=N(1)**2+N(2)**2+N(3)**2
          S=SQRT(S2)

C         -- IM4 EST L'INDICE DU POINT LE + PROCHE DE P2
C            DIFFERENT DE M1 M2 M3 ET TEL QUE M1 M2 M3 M4
C            FORMENT UN VOLUME
          IM4 = 0
          DM = ZR(IADM0-1+IP2)
          DO 35,IP1 = 1,NP1
C           IF ((IP1.EQ.IM1).OR.(IP1.EQ.IM2).OR.(IP1.EQ.IM3)) GO TO 35
            IF (.NOT.ZL(INUAL1-1+ (IP1-1)*NC1+IC1)) GO TO 35
            X1 = ZR(INUAX1-1+ (IP1-1)*NX1+1)
            Y1 = ZR(INUAX1-1+ (IP1-1)*NX1+2)
            Z1 = ZR(INUAX1-1+ (IP1-1)*NX1+3)

C           SI LES POINTS M1 M2 M3 ET P1 NE FORMENT PAS
C           UN VOLUME GO TO 35
            M1P1(1)=ZR(INUAX1-1+(IM1-1)*NX1+1)-X1
            M1P1(2)=ZR(INUAX1-1+(IM1-1)*NX1+2)-Y1
            M1P1(3)=ZR(INUAX1-1+(IM1-1)*NX1+3)-Z1

            V=ABS(M1P1(1)*N(1)+M1P1(2)*N(2)+M1P1(3)*N(3))
            IF (V.LE.(1.D-3*S*L)) GO TO 35

            D = (X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2
            IF (D.LE.DM) THEN
              DM = D
              IM4 = IP1
            END IF
   35     CONTINUE
          IF (IM4.EQ.0) GO TO 9998

          DREF(IP2) = DM
   30   CONTINUE
        GO TO 9999


      ELSE
        CALL U2MESS('F','UTILITAI2_54')
      END IF

9994  CONTINUE
      CALL U2MESS('F','UTILITAI2_55')

9995  CONTINUE
      CALL U2MESS('F','UTILITAI2_56')

9996  CONTINUE
      CALL U2MESS('F','UTILITAI2_57')

9997  CONTINUE
      CALL U2MESS('F','UTILITAI2_58')


9998  CONTINUE
      CALL U2MESS('F','UTILITAI2_59')


9999  CONTINUE
      CALL JEDETR('&&NUADRF.DM0')
      CALL JEDEMA()
      END

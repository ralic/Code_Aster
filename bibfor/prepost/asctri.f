      SUBROUTINE ASCTRI ( MAILLA, RM )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNOM
      CHARACTER*8         MAILLA
      REAL*8              RM
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 18/12/2012   AUTEUR SELLENET N.SELLENET 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C
C     TRI DES GROUPES PETIT AXE ET GRAND AXE FISSURE
C
C
C-----------------DONNEES FOURNIES PAR L'UTILISATEUR--------------------
C
C     RM     = RAYON MOYEN DU COUDE
C     MAILLA = S.D. MAILLAGE
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
      INTEGER       JGRN1, JGRN2, ICOOR, J, K, IBID, KMIN, KMAX,
     +              NBNOP, NBNOG , IRET1, IRET2, JGRN3, JGRN4, IRET3,
     +              IRET4,IRET5
      REAL*8        X, Y, Z, XMIN, ZMAX, YMAX, PI, R8PI, R8MAEM, ZINF,
     +              ZSUP, RNORM, XOR, YOR, ZOR, XEX, YEX, ZEX
      CHARACTER*8   K8B
      CHARACTER*24  GRPNOE,COORD,NOGRN1,NOGRN2,NOGRN3,NOGRN4,NOGRN5
C     ------------------------------------------------------------------
C
      CALL JEMARQ ( )
C
      PI = R8PI()
      GRPNOE = MAILLA//'.GROUPENO'
      COORD  = MAILLA//'.COORDO    .VALE'
C
      CALL JEVEUO (COORD,'E',ICOOR)
C
      NOGRN1 = 'P_AXE_1'
      NOGRN2 = 'P_AXE_2'
      CALL JEEXIN ( JEXNOM(GRPNOE,NOGRN1), IRET1 )
      CALL JEEXIN ( JEXNOM(GRPNOE,NOGRN2), IRET2 )
C
      IF ( IRET1 .NE. 0 .OR. IRET2.NE. 0) THEN
C
        IF (IRET1.NE.0) THEN
          CALL JELIRA ( JEXNOM(GRPNOE,NOGRN1), 'LONUTI', NBNOP, K8B )
          CALL JEVEUO ( JEXNOM(GRPNOE,NOGRN1), 'E', JGRN1 )
        END IF
        IF (IRET2.NE.0) THEN
          CALL JELIRA ( JEXNOM(GRPNOE,NOGRN2), 'LONUTI', NBNOP, K8B )
          CALL JEVEUO ( JEXNOM(GRPNOE,NOGRN2),'E',JGRN2)
        END IF
C
C  TRI POINTS DES PETIT AXE (DE LA PEAU INTERNE VERS LA PEAU EXTERNE)
C   GROUPES P_AXE_1 ET P_AXE_2
C
        DO 10 J=1 , NBNOP-1
C
          IF (IRET1.NE.0) THEN
            XMIN = R8MAEM()
            DO 20 K=J , NBNOP
              X = ZR(ICOOR+3*(ZI(JGRN1+K-1)-1))
              IF (X.LT.XMIN) THEN
                      KMIN = K
                      XMIN = X
              END IF
20          CONTINUE
            IBID = ZI(JGRN1+J-1)
            ZI(JGRN1+J-1) = ZI(JGRN1+KMIN-1)
            ZI(JGRN1+KMIN-1) = IBID
          END IF

          IF (IRET2.NE.0) THEN
            XMIN = R8MAEM()
            DO 30 K=J , NBNOP
              X = ZR(ICOOR+3*(ZI(JGRN2+K-1)-1))
              IF (X.LT.XMIN) THEN
                      KMIN = K
                      XMIN = X
              END IF
30          CONTINUE
            IBID = ZI(JGRN2+J-1)
            ZI(JGRN2+J-1) = ZI(JGRN2+KMIN-1)
            ZI(JGRN2+KMIN-1) = IBID
          END IF
C
10      CONTINUE
C
      END IF
C
      NOGRN1 = 'NOLIG1'
      NOGRN2 = 'NOLIG2'
      CALL JEEXIN ( JEXNOM(GRPNOE,NOGRN1), IRET1 )
      CALL JEEXIN ( JEXNOM(GRPNOE,NOGRN2), IRET2 )
C
      IF ( IRET1 .NE. 0 .OR. IRET2.NE. 0) THEN
C
        IF (IRET1 .NE. 0) THEN
          CALL JELIRA ( JEXNOM(GRPNOE,NOGRN1), 'LONUTI', NBNOP, K8B )
          CALL JEVEUO ( JEXNOM(GRPNOE,NOGRN1), 'E', JGRN1 )
        END IF
        IF (IRET2 .NE. 0) THEN
          CALL JELIRA (JEXNOM(GRPNOE,NOGRN2), 'LONUTI', NBNOP, K8B )
          CALL JEVEUO (JEXNOM(GRPNOE,NOGRN2),'E',JGRN2)
        END IF
C
C  TRI POINTS DE NOLIG1 ET NOLIG2 (PETIT AXE + PTS DANS LE PROLONGEMENT)
C  (TRIES DE LA PEAU INTERNE VERS LA PEAU EXTERNE)
C
        DO 11 J=1 , NBNOP-1
C
          IF (IRET1 .NE. 0) THEN
            XMIN = R8MAEM()
            DO 21 K=J , NBNOP
              X = ZR(ICOOR+3*(ZI(JGRN1+K-1)-1))
              IF (X.LT.XMIN) THEN
                  KMIN = K
                  XMIN = X
              END IF
21          CONTINUE
            IBID = ZI(JGRN1+J-1)
            ZI(JGRN1+J-1) = ZI(JGRN1+KMIN-1)
            ZI(JGRN1+KMIN-1) = IBID
          END IF

          IF (IRET2 .NE. 0) THEN
                XMIN = R8MAEM()
                DO 31 K=J , NBNOP
                  X = ZR(ICOOR+3*(ZI(JGRN2+K-1)-1))
                  IF (X.LT.XMIN) THEN
                      KMIN = K
                      XMIN = X
                  END IF
31              CONTINUE
                IBID = ZI(JGRN2+J-1)
                ZI(JGRN2+J-1) = ZI(JGRN2+KMIN-1)
                ZI(JGRN2+KMIN-1) = IBID
          END IF
C
11      CONTINUE
C
      END IF
C
C   TRI POINTS DES GRAND AXE
C   GROUPES G_AXE_1 ET G_AXE_2
C
      NOGRN1 = 'G_AXE_1'
      NOGRN2 = 'G_AXE_2'
      CALL JEEXIN ( JEXNOM(GRPNOE,NOGRN1), IRET1 )
      CALL JEEXIN ( JEXNOM(GRPNOE,NOGRN2), IRET2 )
C
      IF ( IRET1 .NE. 0 .OR. IRET2.NE. 0) THEN
C
        IF ( IRET1 .NE. 0) THEN
          CALL JELIRA ( JEXNOM(GRPNOE,NOGRN1), 'LONUTI', NBNOG, K8B)
          CALL JEVEUO ( JEXNOM(GRPNOE,NOGRN1), 'E', JGRN1 )
        END IF
        IF ( IRET2 .NE. 0) THEN
          CALL JELIRA(JEXNOM(GRPNOE,NOGRN2), 'LONUTI', NBNOG, K8B)
          CALL JEVEUO(JEXNOM(GRPNOE,NOGRN2),'E',JGRN2)
        END IF
C
C -- AUTRES ORIENTATION DE FISSURE : TRI DE Z<0 A Z>0
C
        DO 90 J=1 , NBNOG-1
C
          IF ( IRET1 .NE. 0) THEN
            ZMAX = R8MAEM()
            DO 100 K=J , NBNOG
              Z = ZR(ICOOR+3*(ZI(JGRN1+K-1)-1)+2)
              IF (Z.LT.ZMAX) THEN
                  KMAX = K
                  ZMAX = Z
              END IF
100         CONTINUE
            IBID = ZI(JGRN1+J-1)
            ZI(JGRN1+J-1) = ZI(JGRN1+KMAX-1)
            ZI(JGRN1+KMAX-1) = IBID
          END IF
          IF ( IRET2 .NE. 0) THEN
            ZMAX = R8MAEM()
            DO 110 K=J , NBNOG
              Z = ZR(ICOOR+3*(ZI(JGRN2+K-1)-1)+2)
              IF (Z.LT.ZMAX) THEN
                   KMAX = K
                   ZMAX = Z
              END IF
110         CONTINUE
            IBID = ZI(JGRN2+J-1)
            ZI(JGRN2+J-1) = ZI(JGRN2+KMAX-1)
            ZI(JGRN2+KMAX-1) = IBID
          END IF
C
90      CONTINUE

        ZINF = ZR(ICOOR+3*(ZI(JGRN2+1-1)-1)+2)
        ZSUP = ZR(ICOOR+3*(ZI(JGRN2+NBNOG-1)-1)+2)

        IF (ABS(ZSUP-ZINF).LT.1.D-5) THEN
C
C -- FISSURE CIRCONFERENTIELLE (90 DEGRE) : TRI DE Y<0 A Y>0
C
          DO 60 J=1 , NBNOG-1
C
            IF ( IRET1 .NE. 0) THEN
                   YMAX = PI*RM
                   DO 70 K=J , NBNOG
                     Y = ZR(ICOOR+3*(ZI(JGRN1+K-1)-1)+1)
                     IF (Y.LT.YMAX) THEN
                         KMAX = K
                         YMAX = Y
                     END IF
70                 CONTINUE
                   IBID = ZI(JGRN1+J-1)
                   ZI(JGRN1+J-1) = ZI(JGRN1+KMAX-1)
                   ZI(JGRN1+KMAX-1) = IBID
            END IF

            IF ( IRET2 .NE. 0) THEN
              YMAX = PI*RM
              DO 80 K=J , NBNOG
                  Y = ZR(ICOOR+3*(ZI(JGRN2+K-1)-1)+1)
                  IF (Y.LT.YMAX) THEN
                      KMAX = K
                      YMAX = Y
                  END IF
80            CONTINUE
              IBID = ZI(JGRN2+J-1)
              ZI(JGRN2+J-1) = ZI(JGRN2+KMAX-1)
              ZI(JGRN2+KMAX-1) = IBID
            END IF
C
60        CONTINUE
C
        END IF
C
      END IF
C
C   TRI POINTS DU FOND DE FISSURE
C
      NOGRN1 = 'FONDFISS'
      CALL JEEXIN ( JEXNOM(GRPNOE,NOGRN1), IRET1 )
C
      IF ( IRET1 .NE. 0 ) THEN
C
        CALL JELIRA ( JEXNOM(GRPNOE,NOGRN1), 'LONUTI', NBNOG, K8B )
        CALL JEVEUO ( JEXNOM(GRPNOE,NOGRN1), 'E', JGRN1 )
C
C -- AUTRES ORIENTATION DE FISSURE : TRI DE Z<0 A Z>0
C
        DO 130 J=1 , NBNOG-1
C
          ZMAX = R8MAEM()
          DO 120 K=J , NBNOG
            Z = ZR(ICOOR+3*(ZI(JGRN1+K-1)-1)+2)
            IF (Z.LT.ZMAX) THEN
                KMAX = K
                ZMAX = Z
            END IF
120       CONTINUE
          IBID = ZI(JGRN1+J-1)
          ZI(JGRN1+J-1) = ZI(JGRN1+KMAX-1)
          ZI(JGRN1+KMAX-1) = IBID
C
130      CONTINUE

        ZINF = ZR(ICOOR+3*(ZI(JGRN1+1-1)-1)+2)
        ZSUP = ZR(ICOOR+3*(ZI(JGRN1+NBNOG-1)-1)+2)

        IF (ABS(ZSUP-ZINF).LT.1.D-5) THEN
C
C -- FISSURE CIRCONFERENTIELLE (90 DEGRE) : TRI DE Y<0 A Y>0
C
          DO 140 J=1 , NBNOG-1
C
            YMAX = PI*RM
            DO 150 K=J , NBNOG
              Y = ZR(ICOOR+3*(ZI(JGRN1+K-1)-1)+1)
              IF (Y.LT.YMAX) THEN
                  KMAX = K
                  YMAX = Y
              END IF
150          CONTINUE
            IBID = ZI(JGRN1+J-1)
            ZI(JGRN1+J-1) = ZI(JGRN1+KMAX-1)
            ZI(JGRN1+KMAX-1) = IBID
C
140        CONTINUE
C
        END IF
C
      END IF
C
C  -- CALCUL DE LA POSITION DU CHAMP THETA A L'ORIGINE ET A
C     L'EXTREMITE DU FOND DE FISSURE
C
      NOGRN3 = 'THOR'
      NOGRN4 = 'THEX'
      NOGRN5 = 'MED1'
      CALL JEEXIN ( JEXNOM(GRPNOE,NOGRN3), IRET3 )
      CALL JEEXIN ( JEXNOM(GRPNOE,NOGRN4), IRET4 )
      CALL JEEXIN ( JEXNOM(GRPNOE,NOGRN5), IRET5 )
C
      IF ( IRET3.NE.0 .AND. IRET4.NE.0) THEN

         CALL JEVEUO ( JEXNOM(GRPNOE,NOGRN3), 'L', JGRN3 )
         CALL JEVEUO ( JEXNOM(GRPNOE,NOGRN4), 'L', JGRN4 )

        IF (IRET5.NE.0) THEN

         XOR = ZR(ICOOR+3*(ZI(JGRN1)-1))
         YOR = ZR(ICOOR+3*(ZI(JGRN1)-1)+1)
         ZOR = ZR(ICOOR+3*(ZI(JGRN1)-1)+2)
         XEX = ZR(ICOOR+3*(ZI(JGRN1+NBNOG-1)-1))
         YEX = ZR(ICOOR+3*(ZI(JGRN1+NBNOG-1)-1)+1)
         ZEX = ZR(ICOOR+3*(ZI(JGRN1+NBNOG-1)-1)+2)
C
         RNORM = SQRT( (XEX-XOR)**2 + (YEX-YOR)**2 + (ZEX-ZOR)**2 )
C
         ZR(ICOOR+3*(ZI(JGRN3)-1))  = XOR + (XOR - XEX)/RNORM
         ZR(ICOOR+3*(ZI(JGRN3)-1)+1)= YOR + (YOR - YEX)/RNORM
         ZR(ICOOR+3*(ZI(JGRN3)-1)+2)= ZOR + (ZOR - ZEX)/RNORM
C
         ZR(ICOOR+3*(ZI(JGRN4)-1))  = XEX + (XEX - XOR)/RNORM
         ZR(ICOOR+3*(ZI(JGRN4)-1)+1)= YEX + (YEX - YOR)/RNORM
         ZR(ICOOR+3*(ZI(JGRN4)-1)+2)= ZEX + (ZEX - ZOR)/RNORM

         ELSE

         CALL JELIRA ( JEXNOM(GRPNOE,'P_AXE_2'), 'LONUTI', NBNOG, K8B )
         CALL JEVEUO ( JEXNOM(GRPNOE,'P_AXE_2'), 'L', JGRN1 )

         XOR = ZR(ICOOR+3*(ZI(JGRN1)-1))
         YOR = ZR(ICOOR+3*(ZI(JGRN1)-1)+1)
         ZOR = ZR(ICOOR+3*(ZI(JGRN1)-1)+2)
         XEX = ZR(ICOOR+3*(ZI(JGRN1+NBNOG-1)-1))
         YEX = ZR(ICOOR+3*(ZI(JGRN1+NBNOG-1)-1)+1)
         ZEX = ZR(ICOOR+3*(ZI(JGRN1+NBNOG-1)-1)+2)
C
         RNORM = SQRT( (XEX-XOR)**2 + (YEX-YOR)**2 + (ZEX-ZOR)**2 )
C
         ZR(ICOOR+3*(ZI(JGRN3)-1))  = XOR + (XOR - XEX)/RNORM
         ZR(ICOOR+3*(ZI(JGRN3)-1)+1)= YOR + (YOR - YEX)/RNORM
         ZR(ICOOR+3*(ZI(JGRN3)-1)+2)= ZOR + (ZOR - ZEX)/RNORM
C
         CALL JELIRA ( JEXNOM(GRPNOE,'G_AXE_2'), 'LONUTI', NBNOG, K8B )
         CALL JEVEUO ( JEXNOM(GRPNOE,'G_AXE_2'), 'L', JGRN1 )

         XOR = ZR(ICOOR+3*(ZI(JGRN1)-1))
         YOR = ZR(ICOOR+3*(ZI(JGRN1)-1)+1)
         ZOR = ZR(ICOOR+3*(ZI(JGRN1)-1)+2)
         XEX = ZR(ICOOR+3*(ZI(JGRN1+NBNOG-1)-1))
         YEX = ZR(ICOOR+3*(ZI(JGRN1+NBNOG-1)-1)+1)
         ZEX = ZR(ICOOR+3*(ZI(JGRN1+NBNOG-1)-1)+2)
C
         RNORM = SQRT( (XEX-XOR)**2 + (YEX-YOR)**2 + (ZEX-ZOR)**2 )
C
         ZR(ICOOR+3*(ZI(JGRN4)-1))  = XEX + (XEX - XOR)/RNORM
         ZR(ICOOR+3*(ZI(JGRN4)-1)+1)= YEX + (YEX - YOR)/RNORM
         ZR(ICOOR+3*(ZI(JGRN4)-1)+2)= ZEX + (ZEX - ZOR)/RNORM

         END IF
C
      END IF
C
      CALL JEDEMA ( )
C
      END

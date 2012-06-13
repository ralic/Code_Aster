      SUBROUTINE TE0513(OPTION,NOMTE)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*16       NOMTE,OPTION
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C     BUT:
C       1) POUR L'OPTION : 'CARA_SECT_POUT3 ' :
C          CALCUL DU CHAMP ELEMENTAIRE SUR DES ELEMENTS
C          ISOPARAMETRIQUES DE COQUE
C           10 COMPOSANTES
C              SOMME/S_ELEMENT(DS,  X.DS,  Y.DS,  Z.DS,
C                                   X*X.DS,Y*Y.DS,Z*Z.DS,
C                                   X*Y.DS,X*Z.DS,Y*Z.DS)
C              CES 10 QUANTITES GEOMETRIQUES SONT NOTEES :
C                    S,AX,AY,AZ,AXX,AYY,AZZ,AXY,AXZ,AYZ
C
C            6 COMPOSANTES
C              VECTEUR TANGENT ORTHONORMAL A l'ELEMENT
C                    VT1 VT2
C
C
C       2) POUR L'OPTION : 'CARA_SECT_POUT4 ' :
C          CALCUL DES 2 VECTEURS DEFINIS AUX NOEUDS DES ELEMENTS
C          AYANT POURS VALEURS AU NOEUD I DE L'ELEMENT:
C          POUR LE PREMIER VECTEUR
C               SOMME/S_ELEMENT(NI.DS,0,0,0,0,0)
C          POUR LE SECOND VECTEUR
C               SOMME/S_ELEMENT(X*NI.DS,Y*NI.DS,Z*NI.DS,0,0,0)
C          SUR DES ELEMENTS ISOPARAMETRIQUES DE COQUE ==> 6DDL/NOEUD
C
C          AVEC X = XM - XG = NJ*XJ - XG
C               Y = YM - YG = NJ*YJ - YG
C               Z = ZM - ZG = NJ*ZJ - ZG
C          OU (XG,YG,ZG) SONT LES COORDONNEES DU CENTRE GEOMETRIQUE
C                        DU LIGREL DES MAILLES DE SURFACE TRAITE
C
C     ENTREES  ---> OPTION : OPTION DE CALCUL
C              ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................
C
      REAL*8             NORME,JAC,NX,NY,NZ,ZERO
      REAL*8             SX(9,9),SY(9,9),SZ(9,9)
      REAL*8             SIGAU,AXGAU,AYGAU,AZGAU,XGAU,YGAU,ZGAU
      REAL*8             AXXGAU,AYYGAU,AZZGAU,AXYGAU,AXZGAU,AYZGAU
      REAL*8             VT1(3),VT2(3),VNN(3)
      INTEGER            IPOIDS,IVF,IDFDX,IDFDY,IGEOM
      INTEGER            NDIM,NNO,IPG,NPG1,NNOS,JGANO
      INTEGER            IDEC,JDEC,KDEC,LDEC
      INTEGER            INO,JNO,NDDLNO
      INTEGER            I,J,ISECT,IORIG
      INTEGER            IVECT1,IVECT2
C
      COMMON /NOMAJE/PGC
      CHARACTER*6 PGC
C
      ZERO = 0.0D0
C
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDX,JGANO)
      IDFDY  = IDFDX  + 1
C     COQUE ==> 6 DDL PAR NOEUD
      NDDLNO=6
C
      CALL JEVECH('PGEOMER','L',IGEOM)
C
      IF (OPTION.EQ.'CARA_SECT_POUT3') THEN
         CALL JEVECH('PCASECT','E',ISECT)
C
         DO 20 I = 1,10
            ZR(ISECT+I-1) = 0.0D0
20       CONTINUE
C
      ELSEIF (OPTION.EQ.'CARA_SECT_POUT4') THEN
         CALL JEVECH('PORIGIN','L',IORIG)
         CALL JEVECH('PVECTU1','E',IVECT1)
         CALL JEVECH('PVECTU2','E',IVECT2)
C
         DO 30 I = 1, NDDLNO*NNO
            ZR(IVECT1+I-1) = ZERO
            ZR(IVECT2+I-1) = ZERO
30       CONTINUE
C
      ENDIF
C
C     CALCUL DES PRODUITS VECTORIELS OMI X OMJ
      DO 40 INO = 1,NNO
         I = IGEOM + 3*(INO-1) -1
         DO 50 JNO = 1,NNO
            J = IGEOM + 3*(JNO-1) -1
            SX(INO,JNO) = ZR(I+2) * ZR(J+3) - ZR(I+3) * ZR(J+2)
            SY(INO,JNO) = ZR(I+3) * ZR(J+1) - ZR(I+1) * ZR(J+3)
            SZ(INO,JNO) = ZR(I+1) * ZR(J+2) - ZR(I+2) * ZR(J+1)
50       CONTINUE
40    CONTINUE
C
C    ---------------------------
C--- - OPTION : CARA_SECT_POUT3-
C    ---------------------------
      IF (OPTION.EQ.'CARA_SECT_POUT3') THEN
C     BOUCLE SUR LES POINTS DE GAUSS
         DO 60 IPG=1,NPG1
            KDEC = (IPG-1)*NNO*NDIM
            LDEC = (IPG-1)*NNO
            NX = 0.0D0
            NY = 0.0D0
            NZ = 0.0D0
C
C           CALCUL DE LA NORMALE AU POINT DE GAUSS IPG
            DO 70 I=1,NNO
               IDEC = IDFDX+KDEC+(I-1)*NDIM
               DO 75 J=1,NNO
                  JDEC = IDFDY+KDEC+(J-1)*NDIM
                  NX = NX + ZR(IDEC)*ZR(JDEC)*SX(I,J)
                  NY = NY + ZR(IDEC)*ZR(JDEC)*SY(I,J)
                  NZ = NZ + ZR(IDEC)*ZR(JDEC)*SZ(I,J)
75             CONTINUE
70          CONTINUE
C
C           LE JACOBIEN EST EGAL A LA NORME DE LA NORMALE
            JAC = SQRT (NX*NX + NY*NY + NZ*NZ)
            SIGAU = ZR(IPOIDS+IPG-1)*JAC
C           CALCUL DE AX, AY, AZ = SOMME(X.DS, Y.DS, Z.DS)
            AXGAU = ZERO
            AYGAU = ZERO
            AZGAU = ZERO
            DO 80 INO = 1, NNO
               I = IGEOM + 3*(INO-1) -1
               AXGAU = AXGAU + ZR(IVF+LDEC+INO-1) * ZR(I+1)
               AYGAU = AYGAU + ZR(IVF+LDEC+INO-1) * ZR(I+2)
               AZGAU = AZGAU + ZR(IVF+LDEC+INO-1) * ZR(I+3)
80         CONTINUE
C
C---        CALCUL DE  AXX, AYY, AZZ, AXY, AXZ, AYZ
C---        = SOMME(X*X.DS, Y*Y.DS, Z*Z.DS, X*Y.DS, X*Z.DS, Y*Z.DS)
            XGAU = ZERO
            YGAU = ZERO
            ZGAU = ZERO
C
            DO 90 INO = 1, NNO
               I = IGEOM + 3*(INO-1) -1
               XGAU = XGAU + ZR(IVF+LDEC+INO-1) * ZR(I+1)
               YGAU = YGAU + ZR(IVF+LDEC+INO-1) * ZR(I+2)
               ZGAU = ZGAU + ZR(IVF+LDEC+INO-1) * ZR(I+3)
 90         CONTINUE
C
            AXXGAU = XGAU * XGAU
            AYYGAU = YGAU * YGAU
            AZZGAU = ZGAU * ZGAU
            AXYGAU = XGAU * YGAU
            AXZGAU = XGAU * ZGAU
            AYZGAU = YGAU * ZGAU
C
C           CALCUL DE A1 = S
            ZR(ISECT+1-1)  = ZR(ISECT+1-1)  +        SIGAU
C           AX
            ZR(ISECT+2-1)  = ZR(ISECT+2-1)  + AXGAU* SIGAU
C           AY
            ZR(ISECT+3-1)  = ZR(ISECT+3-1)  + AYGAU* SIGAU
C           AZ
            ZR(ISECT+4-1)  = ZR(ISECT+4-1)  + AZGAU* SIGAU
C           AXX
            ZR(ISECT+5-1)  = ZR(ISECT+5-1)  + AXXGAU*SIGAU
C           AYY
            ZR(ISECT+6-1)  = ZR(ISECT+6-1)  + AYYGAU*SIGAU
C           AZZ
            ZR(ISECT+7-1)  = ZR(ISECT+7-1)  + AZZGAU*SIGAU
C           AXY
            ZR(ISECT+8-1)  = ZR(ISECT+8-1)  + AXYGAU*SIGAU
C           AXZ
            ZR(ISECT+9-1)  = ZR(ISECT+9-1)  + AXZGAU*SIGAU
C           AYZ
            ZR(ISECT+10-1) = ZR(ISECT+10-1) + AYZGAU*SIGAU
C
60       CONTINUE
C---     FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION
         I = IGEOM - 1
C        ARETE 1
         VT1(1) = ZR(I+4) - ZR(I+1)
         VT1(2) = ZR(I+5) - ZR(I+2)
         VT1(3) = ZR(I+6) - ZR(I+3)
C        ARETE 2
         VT2(1) = ZR(I+7) - ZR(I+1)
         VT2(2) = ZR(I+8) - ZR(I+2)
         VT2(3) = ZR(I+9) - ZR(I+3)
C        VECTEUR NORMAL VN = VT1^VT2
         CALL PROVEC(VT1,VT2,VNN)
C        VECTEUR TANGENT 2
         CALL PROVEC(VNN,VT1,VT2)
C        VECTEUR TANGENT 1 et 2, NORMES
         CALL NORMEV(VT1,NORME)
         CALL NORMEV(VT2,NORME)
C
         ZR(ISECT+11-1) = VT1(1)
         ZR(ISECT+12-1) = VT1(2)
         ZR(ISECT+13-1) = VT1(3)
         ZR(ISECT+14-1) = VT2(1)
         ZR(ISECT+15-1) = VT2(2)
         ZR(ISECT+16-1) = VT2(3)
C---  FIN DE L'OPTION 'CARA_SECT_POUT3'
C
C    ---------------------------
C--- - OPTION : CARA_SECT_POUT4-
C    ---------------------------
      ELSEIF (OPTION.EQ.'CARA_SECT_POUT4') THEN
C        BOUCLE SUR LES POINTS DE GAUSS
         DO 100 IPG=1,NPG1
            KDEC = (IPG-1)*NNO*NDIM
            LDEC = (IPG-1)*NNO
            NX = 0.0D0
            NY = 0.0D0
            NZ = 0.0D0
C           CALCUL DE LA NORMALE AU POINT DE GAUSS IPG
            DO 110 I=1,NNO
               IDEC = IDFDX+KDEC+(I-1)*NDIM
               DO 115 J=1,NNO
                  JDEC = IDFDY+KDEC+(J-1)*NDIM
                  NX = NX + ZR(IDEC)*ZR(JDEC)*SX(I,J)
                  NY = NY + ZR(IDEC)*ZR(JDEC)*SY(I,J)
                  NZ = NZ + ZR(IDEC)*ZR(JDEC)*SZ(I,J)
115            CONTINUE
110         CONTINUE
C
C           LE JACOBIEN EST EGAL A LA NORME DE LA NORMALE
            JAC = SQRT (NX*NX + NY*NY + NZ*NZ)
            SIGAU = ZR(IPOIDS+IPG-1)*JAC
C           VECT1(I) = SOMME(NI.DS, 0, 0, 0, 0, 0)
            DO 120 INO = 1, NNO
               I = IVECT1+NDDLNO*(INO-1)
               ZR(I)  = ZR(I) + ZR(IVF+LDEC+INO-1)*SIGAU
120         CONTINUE
C
C           VECT2(I) = SOMME(X*NI.DS, Y*NI.DS, Z*NI.DS, 0, 0, 0)
            XGAU = ZERO
            YGAU = ZERO
            ZGAU = ZERO
C
            DO 130 INO = 1, NNO
               I = IGEOM + 3*(INO-1) -1
               J = IVF+LDEC+INO-1
               XGAU = XGAU + ZR(J) * ZR(I+1)
               YGAU = YGAU + ZR(J) * ZR(I+2)
               ZGAU = ZGAU + ZR(J) * ZR(I+3)
 130        CONTINUE
C
            DO 140 INO = 1, NNO
               I = IVECT2+NDDLNO*(INO-1)-1
               J = IVF+LDEC+INO-1
               ZR(I+1) = ZR(I+1) + ZR(J)*(XGAU-ZR(IORIG+1-1))*SIGAU
               ZR(I+2) = ZR(I+2) + ZR(J)*(YGAU-ZR(IORIG+2-1))*SIGAU
               ZR(I+3) = ZR(I+3) + ZR(J)*(ZGAU-ZR(IORIG+3-1))*SIGAU
140         CONTINUE
C
100      CONTINUE
C---  FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION
C---   ET FIN DE L'OPTION 'CARA_SECT_POUT4'
      ENDIF
      END

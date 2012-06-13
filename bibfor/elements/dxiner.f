      SUBROUTINE DXINER ( NNOE, XYZG1, RHO, EPAIS, MASS, CDG, INERTI )
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'jeveux.h'
      INTEGER            NNOE
      REAL*8             XYZG1(3,*), RHO, EPAIS, MASS, CDG(*), INERTI(*)
C     ------------------------------------------------------------------
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     CALCULE LE CDG ET LA MASSE D'UNE MAILLE TRIA ET QUAD
C
C     ------------------------------------------------------------------
      REAL*8       JAC,NX,NY,NZ,SX(9,9),SY(9,9),SZ(9,9),ZERO
      REAL*8       PGL(3,3), XYZL1(3,4)
      REAL*8       XYZG(3,8), XYZL(3,8)
      REAL*8       IGXX, IGYY, IGXY, MATINE(6), IGZZ
      REAL*8       INERT0(6)
C
C --- INITIALISATIONS :
C     ---------------
      ZERO  =  0.0D0
      UNDEMI=  0.5D0
      UN    =  1.0D0
      DOUZE = 12.0D0
C
      AIRE = ZERO
      AXG  = ZERO
      AYG  = ZERO
      AZG  = ZERO
      AXL  = ZERO
      AYL  = ZERO
      AXX  = ZERO
      AYY  = ZERO
      AXY  = ZERO
C
C --- RECUPERATION DES DONNEES RELATIVES A L'INTEGRATION DES ELEMENTS
C --- DE TYPE 'FACE6' ET 'FACE8' :
C     -------------------------
      CALL ELREF4 ( ' ', 'MASS', NDIM, NNO, NNOS, NPG1, IPOIDS, IVF,
     &              IDFDX, JGANO )
      IDFDY  = IDFDX  + 1
C
      ROEP = RHO * EPAIS
C
C --- DETERMINATION DE LA MATRICE DE PASSAGE DU REPERE GLOBAL
C --- AU REPERE LOCAL :
C     ---------------
      IF ( NNOE .EQ. 3 ) THEN
         CALL DXTPGL ( XYZG1 , PGL )
      ELSEIF( NNOE .EQ. 4 ) THEN
         CALL DXQPGL ( XYZG1 , PGL )
      ENDIF
C
C --- DETERMINATION DES COORDONNEES DES NOEUDS DANS LE REPERE LOCAL :
C     -------------------------------------------------------------
      CALL UTPVGL ( NNOE , 3 , PGL , XYZG1 , XYZL1 )
C
C --- AFFECTATION DES COORDONNEES DES NOEUDS DE L'ELEMENT FACE6
C --- OU FACE8 CORRESPONDANT A L'ELEMENT DE PLAQUE COURANT,
C --- XYZG DESIGNENT LES COORDONNNEES DES CONNECTIVITES DANS
C --- LE REPERE GLOBAL, XYZL DESIGNENT LES COORDONNEES DES
C --- CONNECTIVITES DANS LE REPERE LOCAL A L'ELEMENT DE PLAQUE:
C     -------------------------------------------------------
C
C --- NOEUDS SOMMETS :
C     --------------
      DO 20 INO =1, NNOE
       DO 30 K = 1, 3
        XYZL(K,INO) = XYZL1(K,INO)
        XYZG(K,INO) = XYZG1(K,INO)
 30   CONTINUE
 20   CONTINUE
C
C --- NOEUDS MILIEUX DES COTES , ON PREND COMME COORDONNEES
C --- LA DEMI-SOMME DES COORDONNEES DES NOEUDS SOMMETS PUISQU'IL
C --- S'AGIT D'ELEMENTS DE PLAQUE :
C     ---------------------------
      DO 40 INO =1, NNOE-1
       DO 50 K = 1, 3
        XYZL(K,NNOE+INO) = UNDEMI*(XYZL1(K,INO)+XYZL1(K,INO+1))
        XYZG(K,NNOE+INO) = UNDEMI*(XYZG1(K,INO)+XYZG1(K,INO+1))
 50    CONTINUE
 40   CONTINUE
C
       DO 60 K = 1, 3
        XYZL(K,NNOE+NNOE) = UNDEMI*(XYZL1(K,1)+XYZL1(K,NNOE))
        XYZG(K,NNOE+NNOE) = UNDEMI*(XYZG1(K,1)+XYZG1(K,NNOE))
 60   CONTINUE
C
C --- CALCUL DES PRODUITS VECTORIELS OMI X OMJ :
C     ----------------------------------------
      DO 70 INO = 1,NNO
          DO 80 JNO = 1,NNO
              SX(INO,JNO) =   XYZG(2,INO) * XYZG(3,JNO)
     &                      - XYZG(3,INO) * XYZG(2,JNO)
              SY(INO,JNO) =   XYZG(3,INO) * XYZG(1,JNO)
     &                      - XYZG(1,INO) * XYZG(3,JNO)
              SZ(INO,JNO) =   XYZG(1,INO) * XYZG(2,JNO)
     &                      - XYZG(2,INO) * XYZG(1,JNO)
 80       CONTINUE
 70   CONTINUE
C
C --- BOUCLE SUR LES POINTS DE GAUSS :
C     ------------------------------
      DO 90 IPG=1,NPG1
         KDEC = (IPG-1)*NNO*NDIM
         LDEC = (IPG-1)*NNO
C
         NX = ZERO
         NY = ZERO
         NZ = ZERO
C
C ---   CALCUL DE LA NORMALE AU POINT DE GAUSS IPG :
C       ------------------------------------------
         DO 100 I=1,NNO
            IDEC = (I-1)*NDIM
            DO 100 J=1,NNO
              JDEC = (J-1)*NDIM
C
              NX = NX + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) *
     &                  SX(I,J)
              NY = NY + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) *
     &                  SY(I,J)
              NZ = NZ + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) *
     &                  SZ(I,J)
C
 100     CONTINUE
C
C ---   LE JACOBIEN EST EGAL A LA NORME DE LA NORMALE :
C       ---------------------------------------------
         JAC = SQRT (NX*NX + NY*NY + NZ*NZ)
C
         SIGAU = ZR(IPOIDS+IPG-1)*JAC
C
C ---   CALCUL DE AX, AY, AZ = SOMME(X.DS, Y.DS, Z.DS)
C ---   DANS LE REPERE GLOBAL ET DANS LE REPERE LOCAL :
C       ---------------------------------------------
         AXGGAU = ZERO
         AYGGAU = ZERO
         AZGGAU = ZERO
C
         AXLGAU = ZERO
         AYLGAU = ZERO
C
         DO 110 INO = 1, NNO
C
             AXGGAU = AXGGAU + ZR(IVF+LDEC+INO-1) * XYZG(1,INO)
             AYGGAU = AYGGAU + ZR(IVF+LDEC+INO-1) * XYZG(2,INO)
             AZGGAU = AZGGAU + ZR(IVF+LDEC+INO-1) * XYZG(3,INO)
C
             AXLGAU = AXLGAU + ZR(IVF+LDEC+INO-1) * XYZL(1,INO)
             AYLGAU = AYLGAU + ZR(IVF+LDEC+INO-1) * XYZL(2,INO)
C
 110    CONTINUE
C
C ---     CALCUL DE  AXX, AYY, AZZ, AXY
C ---     = SOMME(X*X.DS, Y*Y.DS, Z*Z.DS, X*Y.DS) DANS LE REPERE LOCAL:
C         ------------------------------------------------------------
         XGAU = ZERO
         YGAU = ZERO
C
         DO 120 INO = 1, NNO
C
             XGAU = XGAU + ZR(IVF+LDEC+INO-1) * XYZL(1,INO)
             YGAU = YGAU + ZR(IVF+LDEC+INO-1) * XYZL(2,INO)
 120     CONTINUE
C
         AXXGAU = XGAU * XGAU
         AYYGAU = YGAU * YGAU
         AXYGAU = XGAU * YGAU
C
C ---      CALCUL DE LA SURFACE :
         AIRE = AIRE + SIGAU
C ---      AX :
         AXG = AXG + AXGGAU * SIGAU
         AXL = AXL + AXLGAU * SIGAU
C ---      AY :
         AYG = AYG + AYGGAU * SIGAU
         AYL = AYL + AYLGAU * SIGAU
C ---      AZ :
         AZG = AZG + AZGGAU * SIGAU
C ---      AXX :
         AXX = AXX + AXXGAU * SIGAU
C ---      AYY :
         AYY = AYY + AYYGAU * SIGAU
C ---      AXY :
         AXY = AXY + AXYGAU * SIGAU
 90   CONTINUE
C
      IF (ABS(AIRE).LT.R8PREM()) THEN
          CALL U2MESS('F','ELEMENTS_48')
      ENDIF
C
      S1 = UN/AIRE
C
C --- COORDONNEES DU CENTRE GEOMETRIQUE G DE L'ELEMENT
C --- DANS LE REPERE GLOBAL.
C --- XG = AX/S, YG = AY/S, ZG = AZ/S :
C     -------------------------------
      XG = S1*AXG
      YG = S1*AYG
      ZG = S1*AZG
C
C --- COORDONNEES DU CENTRE GEOMETRIQUE G DE L'ELEMENT
C --- DANS LE REPERE LOCAL :
C     --------------------
      XL = S1*AXL
      YL = S1*AYL
C
C --- CALCUL DES TERMES DU TENSEUR D'INERTIE AU CENTRE GEOMETRIQUE
C --- DE L'ELEMENT ET DANS LE REPERE LOCAL :
C     -----------------------------------
C ---        IGXX = EPAIS*AYY + SOMME (Z**2.DV) -V*(YL**2 + ZL**2)
C ---   SOIT IGXX = EPAIS*AYY + S*(EPAIS**3)/12 -V*(YL**2)
C ---   EN FAIT ON CALCULE IGXX/EPAIS, LA MULTIPLICATION PAR
C ---   L'EPAISSEUR SERA FAITE EN FIN DE ROUTINE LORS DE LA
C ---   MULTIPLICATION PAR ROEP :
C       -----------------------
      IGXX = AYY + AIRE*EPAIS*EPAIS/DOUZE - AIRE*YL*YL
C
C ---        IGYY = EPAIS*AXX + SOMME (Z**2.DV) -V*(XL**2 + ZL**2) :
C            ----------------------------------------------------
      IGYY = AXX + AIRE*EPAIS*EPAIS/DOUZE - AIRE*XL*XL
C
C ---        IGXY = EPAIS*AXY - V*XL*YL
C            ------------------------
      IGXY = AXY - AIRE*XL*YL
C
C ---        IGZZ = EPAIS*(AXX+AYY)  - V*(XL**2 + YL**2) :
C            -------------------------------------------
      IGZZ = AXX + AYY  - AIRE*(XL*XL + YL*YL)
C
C --- AFFECTATION DES TERMES DU TENSEUR D'INERTIE LOCAL :
C     -------------------------------------------------
C     MULTIPLICATION PAR MOINS DES TERMES EXTRA_DIAGONAUX
      MATINE(1) = ROEP*IGXX
      MATINE(2) = - ROEP*IGXY
      MATINE(3) = ROEP*IGYY
      MATINE(4) = ZERO
      MATINE(5) = ZERO
      MATINE(6) = ROEP*IGZZ
C
C --- PASSAGE DU TENSEUR D'INERTIE DANS LE REPERE GLOBAL :
C     --------------------------------------------------
      CALL UTPSLG ( 1 , 3 , PGL , MATINE , INERT0 )
C
C     REMULTIPLICATION PAR MOINS DES TERMES EXTRA_DIAGONAUX
      INERTI(1) = INERT0(1)
      INERTI(2) = INERT0(3)
      INERTI(3) = INERT0(6)
      INERTI(4) = - INERT0(2)
      INERTI(5) = - INERT0(4)
      INERTI(6) = - INERT0(5)
C
C --- AFFECTATION DU VECTEUR DES COORDONNEES DU CENTRE DE GRAVITE
C --- DANS LE REPERE GLOBAL :
C     ---------------------
      CDG(1) = XG
      CDG(2) = YG
      CDG(3) = ZG
C
C --- CALCUL DE LA MASSE DE L'ELEMENT :
C     -------------------------------
      MASS = ROEP * AIRE
C
      END

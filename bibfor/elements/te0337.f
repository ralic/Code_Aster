      SUBROUTINE TE0337(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 11/07/2005   AUTEUR VABHHTS J.PELLET 
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
C TOLE CRP_20
C.......................................................................
C
C     BUT:
C       1) POUR L'OPTION : 'CARA_SECT_POUT3 ' :
C          CALCUL DU CHAMP ELEMENTAIRE A 10 COMPOSANTES :
C          SOMME/S_ELEMENT(DS,X.DS,Y.DS,Z.DS,X*X.DS,Y*Y.DS,Z*Z.DS,
C                             X*Y.DS,X*Z.DS,Y*Z.DS)
C          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 3D
C
C          CES 10 QUANTITES GEOMETRIQUES SONT NOTEES :
C          A1 = S,AX,AY,AZ,AXX,AYY,AZZ,AXY,AXZ,AYZ
C
C       2) POUR L'OPTION : 'CARA_SECT_POUT4 ' :
C          CALCUL DES 2 VECTEURS DEFINIS AUX NOEUDS DES ELEMENTS
C          AYANT POURS VALEURS AU NOEUD I DE L'ELEMENT:
C          POUR LE PREMIER VECTEUR
C               SOMME/S_ELEMENT(NI.DS,0,0)
C          POUR LE SECOND VECTEUR
C               SOMME/S_ELEMENT(X*NI.DS,Y*NI.DS,Z*NI.DS)
C          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 3D
C
C          AVEC X = XM - XG = NJ*XJ - XG
C               Y = YM - YG = NJ*YJ - YG
C               Z = ZM - ZG = NJ*ZJ - ZG
C          OU (XG,YG,ZG) SONT LES COORDONNEES DU CENTRE GEOMETRIQUE
C                        DU LIGREL DES MAILLES DE SURFACE TRAITE
C
C       3) POUR L'OPTION : 'CARA_SECT_POUT5 ' : 3D_TUYAU
C          CALCUL DU VECTEUR DEFINI AUX NOEUDS DES ELEMENTS
C          AYANT POUR VALEURS AU NOEUD I DE L'ELEMENT:
C          SOMME/S_ELEMENT(NI.COS(M.PHI).P.DS)
C          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 3D
C
C     ENTREES  ---> OPTION : OPTION DE CALCUL
C              ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................
C
      IMPLICIT NONE
      CHARACTER*8        ELREFE
      CHARACTER*16       NOMTE,OPTION
      CHARACTER*24       CHVAL,CHCTE
      REAL*8             JACPOI,E1(3),E2(3)
      REAL*8             JAC,NX,NY,NZ,SX(9,9),SY(9,9),SZ(9,9),ZERO
      REAL*8             GP0(3),GPG(3),XPG(3),VSIN(3),DDOT
      REAL*8             NORGP0,NORGPG,ANGL(3),PGL(3,3)
      REAL*8             COSPHI,SINPHI,PHI,COSMFI,SINMFI,PHI0,RAY
      REAL*8             SIGAU,AXGAU,AYGAU,AZGAU,XGAU,YGAU,ZGAU
      REAL*8             AXXGAU,AYYGAU,AZZGAU,AXYGAU,AXZGAU,AYZGAU
      INTEGER            IPOIDS,IVF,IDFDX,IDFDY,IGEOM,M
      INTEGER            NDIM,NNO,IPG,NPG1,INUMOD,NNOS,JGANO
      INTEGER            IDEC,JDEC,KDEC,LDEC
      INTEGER            NBPG(10),INO,JNO,II
      INTEGER            JIN,NBFPG,I,J,JVAL,ISECT,IORIG,IORIFI,IAXE
      INTEGER            IVECT1,IVECT2,IVECT3,IVECT4,IVECT5,IVECT6
C
C
C
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      COMMON /NOMAJE/PGC
      CHARACTER*6 PGC
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C------------FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      ZERO = 0.0D0
C
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDX,JGANO)
      IDFDY  = IDFDX  + 1
C
      CALL JEVECH('PGEOMER','L',IGEOM)
C
      IF (OPTION.EQ.'CARA_SECT_POUT3') THEN
          CALL JEVECH('PCASECT','E',ISECT)
C
          DO 20 I = 1,10
             ZR(ISECT+I-1) = 0.0D0
20        CONTINUE
C
      ELSEIF (OPTION.EQ.'CARA_SECT_POU3R') THEN
          CALL JEVECH('PORIGIN','L',IORIG)
          CALL JEVECH('PRAYONM','E',ISECT)
          ZR(ISECT+1-1) = ZERO
C
      ELSEIF (OPTION.EQ.'CARA_SECT_POUT4') THEN
          CALL JEVECH('PORIGIN','L',IORIG)
          CALL JEVECH('PVECTU1','E',IVECT1)
          CALL JEVECH('PVECTU2','E',IVECT2)
C
          DO 30 I = 1, 3*NNO
             ZR(IVECT1+I-1) = ZERO
             ZR(IVECT2+I-1) = ZERO
 30       CONTINUE
C
      ELSEIF (OPTION.EQ.'CARA_SECT_POUT5') THEN
          CALL JEVECH('PORIGIN','L',IORIG)
          CALL JEVECH('PORIGFI','L',IORIFI)
          CALL JEVECH('PNUMMOD','L',INUMOD)
          CALL JEVECH('PCAORIE','L',IAXE)
          CALL JEVECH('PVECTU1','E',IVECT1)
          CALL JEVECH('PVECTU2','E',IVECT2)
          CALL JEVECH('PVECTU3','E',IVECT3)
          CALL JEVECH('PVECTU4','E',IVECT4)
          CALL JEVECH('PVECTU5','E',IVECT5)
          CALL JEVECH('PVECTU6','E',IVECT6)
C
          E1(1) = ZR(IAXE+1-1)
          E1(2) = ZR(IAXE+2-1)
          E1(3) = ZR(IAXE+3-1)
C
C         COORDONNES DU POINT P TEL QUE GP EST L'ORIGINE
C         DE L'ANGLE PHI
C
          CALL VDIFF(3,ZR(IORIFI),ZR(IORIG),GP0)
          CALL NORMEV(GP0,NORGP0)
C
C         NUMERO DE MODE DE FOURIER
C
          M = ZI(INUMOD)
          DO 40 I = 1, 3*NNO
             ZR(IVECT1+I-1) = ZERO
             ZR(IVECT2+I-1) = ZERO
             ZR(IVECT3+I-1) = ZERO
             ZR(IVECT4+I-1) = ZERO
             ZR(IVECT5+I-1) = ZERO
             ZR(IVECT6+I-1) = ZERO
 40       CONTINUE
      ENDIF
C
C--- CALCUL DES PRODUITS VECTORIELS OMI X OMJ
C
      DO 41 INO = 1,NNO
          I = IGEOM + 3*(INO-1) -1
          DO 50 JNO = 1,NNO
              J = IGEOM + 3*(JNO-1) -1
              SX(INO,JNO) = ZR(I+2) * ZR(J+3) - ZR(I+3) * ZR(J+2)
              SY(INO,JNO) = ZR(I+3) * ZR(J+1) - ZR(I+1) * ZR(J+3)
              SZ(INO,JNO) = ZR(I+1) * ZR(J+2) - ZR(I+2) * ZR(J+1)
 50       CONTINUE
 41   CONTINUE
C
C    ---------------------------
C--- - OPTION : CARA_SECT_POUT3-
C    ---------------------------
C
      IF (OPTION.EQ.'CARA_SECT_POUT3') THEN
C
C---  BOUCLE SUR LES POINTS DE GAUSS
C     ------------------------------
C
        DO 60 IPG=1,NPG1
           KDEC = (IPG-1)*NNO*NDIM
           LDEC = (IPG-1)*NNO
C
           NX = 0.0D0
           NY = 0.0D0
           NZ = 0.0D0
C
C---    CALCUL DE LA NORMALE AU POINT DE GAUSS IPG
C       ------------------------------------------
C
          DO 70 I=1,NNO
            IDEC = (I-1)*NDIM
            DO 70 J=1,NNO
              JDEC = (J-1)*NDIM
C
              NX = NX + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) *
     +                  SX(I,J)
              NY = NY + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) *
     +                  SY(I,J)
              NZ = NZ + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) *
     +                  SZ(I,J)
C
 70       CONTINUE
C
C---  LE JACOBIEN EST EGAL A LA NORME DE LA NORMALE
C     ---------------------------------------------
C
          JAC = SQRT (NX*NX + NY*NY + NZ*NZ)
C
          SIGAU = ZR(IPOIDS+IPG-1)*JAC
C
C---  CALCUL DE AX, AY, AZ = SOMME(X.DS, Y.DS, Z.DS)
C     ----------------------------------------------
C
          AXGAU = ZERO
          AYGAU = ZERO
          AZGAU = ZERO
C
          DO 80 INO = 1, NNO
             I = IGEOM + 3*(INO-1) -1
C
             AXGAU = AXGAU + ZR(IVF+LDEC+INO-1) * ZR(I+1)
             AYGAU = AYGAU + ZR(IVF+LDEC+INO-1) * ZR(I+2)
             AZGAU = AZGAU + ZR(IVF+LDEC+INO-1) * ZR(I+3)
 80      CONTINUE
C
C---   CALCUL DE  AXX, AYY, AZZ, AXY, AXZ, AYZ
C---   = SOMME(X*X.DS, Y*Y.DS, Z*Z.DS, X*Y.DS, X*Z.DS, Y*Z.DS)
C      -------------------------------------------------------
C
          XGAU = ZERO
          YGAU = ZERO
          ZGAU = ZERO
C
          DO 90 INO = 1, NNO
             I = IGEOM + 3*(INO-1) -1
C
             XGAU = XGAU + ZR(IVF+LDEC+INO-1) * ZR(I+1)
             YGAU = YGAU + ZR(IVF+LDEC+INO-1) * ZR(I+2)
             ZGAU = ZGAU + ZR(IVF+LDEC+INO-1) * ZR(I+3)
 90       CONTINUE
C
          AXXGAU = XGAU * XGAU
          AYYGAU = YGAU * YGAU
          AZZGAU = ZGAU * ZGAU
          AXYGAU = XGAU * YGAU
          AXZGAU = XGAU * ZGAU
          AYZGAU = YGAU * ZGAU
C
C---  CALCUL DE A1 = S
          ZR(ISECT+1-1)  = ZR(ISECT+1-1)  +        SIGAU
C---  AX
          ZR(ISECT+2-1)  = ZR(ISECT+2-1)  + AXGAU* SIGAU
C---  AY
          ZR(ISECT+3-1)  = ZR(ISECT+3-1)  + AYGAU* SIGAU
C---  AZ
          ZR(ISECT+4-1)  = ZR(ISECT+4-1)  + AZGAU* SIGAU
C---  AXX
          ZR(ISECT+5-1)  = ZR(ISECT+5-1)  + AXXGAU*SIGAU
C---  AYY
          ZR(ISECT+6-1)  = ZR(ISECT+6-1)  + AYYGAU*SIGAU
C---  AZZ
          ZR(ISECT+7-1)  = ZR(ISECT+7-1)  + AZZGAU*SIGAU
C---  AXY
          ZR(ISECT+8-1)  = ZR(ISECT+8-1)  + AXYGAU*SIGAU
C---  AXZ
          ZR(ISECT+9-1)  = ZR(ISECT+9-1)  + AXZGAU*SIGAU
C---  AYZ
          ZR(ISECT+10-1) = ZR(ISECT+10-1) + AYZGAU*SIGAU
C
 60     CONTINUE
C--- FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION
C---  ET FIN DE L'OPTION 'CARA_SECT_POUT3'
C
C    ---------------------------
C--- - OPTION : CARA_SECT_POU3R-
C    ---------------------------
C
      ELSEIF (OPTION.EQ.'CARA_SECT_POU3R') THEN
C
C
C---  BOUCLE SUR LES POINTS DE GAUSS
C     ------------------------------
C
        DO 63 IPG=1,NPG1
           KDEC = (IPG-1)*NNO*NDIM
           LDEC = (IPG-1)*NNO
C
           NX = 0.0D0
           NY = 0.0D0
           NZ = 0.0D0
C
C---    CALCUL DE LA NORMALE AU POINT DE GAUSS IPG
C       ------------------------------------------
C
          DO 73 I=1,NNO
            IDEC = (I-1)*NDIM
            DO 73 J=1,NNO
              JDEC = (J-1)*NDIM
C
              NX = NX + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) *
     +                  SX(I,J)
              NY = NY + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) *
     +                  SY(I,J)
              NZ = NZ + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) *
     +                  SZ(I,J)
C
 73       CONTINUE
C
C---  LE JACOBIEN EST EGAL A LA NORME DE LA NORMALE
C     ---------------------------------------------
C
          JAC = SQRT (NX*NX + NY*NY + NZ*NZ)
          SIGAU = ZR(IPOIDS+IPG-1)*JAC
          XGAU = ZERO
          YGAU = ZERO
          ZGAU = ZERO
C
          DO 93 INO = 1, NNO
             I = IGEOM + 3*(INO-1) -1
C
             XGAU = XGAU + ZR(IVF+LDEC+INO-1) * ZR(I+1)
             YGAU = YGAU + ZR(IVF+LDEC+INO-1) * ZR(I+2)
             ZGAU = ZGAU + ZR(IVF+LDEC+INO-1) * ZR(I+3)
 93       CONTINUE
          RAY = (XGAU-ZR(IORIG+1-1))**2.D0 +
     &          (YGAU-ZR(IORIG+2-1))**2.D0 +
     &          (ZGAU-ZR(IORIG+3-1))**2.D0
          RAY = SQRT( RAY )
C
C---  CALCUL DE A1 = RAYON
          ZR(ISECT+1-1)  = ZR(ISECT+1-1)  +  RAY*SIGAU
C
 63     CONTINUE
C--- FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION
C---  ET FIN DE L'OPTION 'CARA_SECT_POU3R'
C
C    ---------------------------
C--- - OPTION : CARA_SECT_POUT4-
C    ---------------------------
C
      ELSEIF (OPTION.EQ.'CARA_SECT_POUT4') THEN
C
C---  BOUCLE SUR LES POINTS DE GAUSS
C     ------------------------------
C
        DO 100 IPG=1,NPG1
           KDEC = (IPG-1)*NNO*NDIM
           LDEC = (IPG-1)*NNO
C
           NX = 0.0D0
           NY = 0.0D0
           NZ = 0.0D0
C
C---    CALCUL DE LA NORMALE AU POINT DE GAUSS IPG
C       ------------------------------------------
C
          DO 110 I=1,NNO
            IDEC = (I-1)*NDIM
            DO 110 J=1,NNO
              JDEC = (J-1)*NDIM
C
              NX = NX + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) *
     +                  SX(I,J)
              NY = NY + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) *
     +                  SY(I,J)
              NZ = NZ + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) *
     +                  SZ(I,J)
C
 110      CONTINUE
C
C---  LE JACOBIEN EST EGAL A LA NORME DE LA NORMALE
C     ---------------------------------------------
C
          JAC = SQRT (NX*NX + NY*NY + NZ*NZ)
C
          SIGAU = ZR(IPOIDS+IPG-1)*JAC
C
C---  CALCUL DE VECT1(I) = SOMME(NI.DS, 0, 0)
C     ---------------------------------------
C
          DO 120 INO = 1, NNO
C
             ZR(IVECT1+3*(INO-1)+1-1)  = ZR(IVECT1+3*(INO-1)+1-1) +
     +                                   ZR(IVF+LDEC+INO-1)*SIGAU
 120      CONTINUE
C
C---  CALCUL DE VECT2(I) = SOMME(X*NI.DS, Y*NI.DS, Z*NI.DS)
C     -----------------------------------------------------
C
          XGAU = ZERO
          YGAU = ZERO
          ZGAU = ZERO
C
          DO 130 INO = 1, NNO
             I = IGEOM + 3*(INO-1) -1
C
             XGAU = XGAU + ZR(IVF+LDEC+INO-1) * ZR(I+1)
             YGAU = YGAU + ZR(IVF+LDEC+INO-1) * ZR(I+2)
             ZGAU = ZGAU + ZR(IVF+LDEC+INO-1) * ZR(I+3)
 130      CONTINUE
C
          DO 140 INO = 1, NNO
C
             ZR(IVECT2+3*(INO-1)+1-1)  = ZR(IVECT2+3*(INO-1)+1-1) +
     +                ZR(IVF+LDEC+INO-1)*(XGAU-ZR(IORIG+1-1))*SIGAU
C
             ZR(IVECT2+3*(INO-1)+2-1)  = ZR(IVECT2+3*(INO-1)+2-1) +
     +                ZR(IVF+LDEC+INO-1)*(YGAU-ZR(IORIG+2-1))*SIGAU
C
             ZR(IVECT2+3*(INO-1)+3-1)  = ZR(IVECT2+3*(INO-1)+3-1) +
     +                ZR(IVF+LDEC+INO-1)*(ZGAU-ZR(IORIG+3-1))*SIGAU
 140      CONTINUE
C
 100    CONTINUE
C---  FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION
C---   ET FIN DE L'OPTION 'CARA_SECT_POUT4'

C     ---------------------------
C --- - OPTION : CARA_SECT_POUT5-
C     ---------------------------
C
      ELSEIF (OPTION.EQ.'CARA_SECT_POUT5') THEN


        DO 200 IPG=1,NPG1
           KDEC = (IPG-1)*NNO*NDIM
           LDEC = (IPG-1)*NNO
C
           NX = 0.0D0
           NY = 0.0D0
           NZ = 0.0D0
C
C---    CALCUL DE LA NORMALE AU POINT DE GAUSS IPG
C       ------------------------------------------
C
          DO 210 I=1,NNO
            IDEC = (I-1)*NDIM
            DO 210 J=1,NNO
              JDEC = (J-1)*NDIM
C
              NX = NX + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) *
     +                  SX(I,J)
              NY = NY + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) *
     +                  SY(I,J)
              NZ = NZ + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) *
     +                  SZ(I,J)
C
 210      CONTINUE
C
C---  LE JACOBIEN EST EGAL A LA NORME DE LA NORMALE
C     ---------------------------------------------
C
          JAC = SQRT (NX*NX + NY*NY + NZ*NZ)
          JACPOI = JAC*ZR(IPOIDS+IPG-1)
C
C ---   COORDONNEES DU POINT D'INTEGRATION COURANT :
C       ------------------------------------------
          DO 219 II=1,3
             XPG(II) = ZERO
219       CONTINUE
          DO 220 INO = 1, NNO
             I = IGEOM + 3*(INO-1) -1
             XPG(1) = XPG(1) + ZR(IVF+LDEC+INO-1) * ZR(I+1)
             XPG(2) = XPG(2) + ZR(IVF+LDEC+INO-1) * ZR(I+2)
             XPG(3) = XPG(3) + ZR(IVF+LDEC+INO-1) * ZR(I+3)
 220      CONTINUE
C
C  CALCUL DU VECTEUR G-PG ET DE L'ANGLE PHI ENTRE G-P0 ET G-PG
C
          CALL VDIFF(3,XPG,ZR(IORIG),GPG)
          CALL NORMEV(GPG,NORGPG)
          COSPHI=DDOT(3,GP0,1,GPG,1)
CPM          CALL PROVEC(GP0,GPG,VSIN)
          CALL PROVEC(GPG,GP0,VSIN)
          SINPHI=DDOT(3,E1,1,VSIN,1)
          PHI0=ATAN2(SINPHI,COSPHI)
CJMP          PHI=-PHI0
          PHI=PHI0
          COSMFI=COS(M*PHI)
          SINMFI=SIN(M*PHI)
C
C  CALCUL DE PGL MATRICE DE PASSAGE DE X,Y,Z GLOBAL A E1,E2,E3
C
          CALL PROVEC(GPG,E1,E2)
          CALL ANGVXY(E1,E2,ANGL)
          CALL MATROT(ANGL,PGL)
C
          DO 230 INO = 1, NNO
             DO 231 II=1,3
C
C CALCUL DE VECT1(I) : TERMES EN UMI(COS(M.PHI)) ET UMO (SIN(M.PHI))
C
              ZR(IVECT1+3*(INO-1)+II-1)  = ZR(IVECT1+3*(INO-1)+II-1)+
     +                    COSMFI*PGL(1,II)*ZR(IVF+LDEC+INO-1)*JACPOI
              ZR(IVECT4+3*(INO-1)+II-1)  = ZR(IVECT4+3*(INO-1)+II-1)+
     +                    SINMFI*PGL(1,II)*ZR(IVF+LDEC+INO-1)*JACPOI
C
C CALCUL DE VECT2(I) : TERMES EN VMI(COS(M.PHI)) ET VMO (SIN(M.PHI))
C
              ZR(IVECT2+3*(INO-1)+II-1)  = ZR(IVECT2+3*(INO-1)+II-1)+
     +                    COSMFI*PGL(2,II)*ZR(IVF+LDEC+INO-1)*JACPOI
              ZR(IVECT5+3*(INO-1)+II-1)  = ZR(IVECT5+3*(INO-1)+II-1)+
     +                    SINMFI*PGL(2,II)*ZR(IVF+LDEC+INO-1)*JACPOI
C
C CALCUL DE VECT3(I) : TERMES EN WMI(COS(M.PHI)) ET WMO (SIN(M.PHI))
C
              ZR(IVECT3+3*(INO-1)+II-1)  = ZR(IVECT3+3*(INO-1)+II-1)+
     +                    COSMFI*PGL(3,II)*ZR(IVF+LDEC+INO-1)*JACPOI
              ZR(IVECT6+3*(INO-1)+II-1)  = ZR(IVECT6+3*(INO-1)+II-1)+
     +                    SINMFI*PGL(3,II)*ZR(IVF+LDEC+INO-1)*JACPOI
 231         CONTINUE
 230      CONTINUE
  200    CONTINUE
C ---  FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION
C ---  ET FIN DE L'OPTION 'CARA_SECT_POUT5'
      ENDIF
      END

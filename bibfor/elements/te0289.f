      SUBROUTINE TE0289 ( OPTION , NOMTE )
      IMPLICIT   NONE
      CHARACTER*16        OPTION , NOMTE
C.......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/04/2004   AUTEUR JMBHH01 J.M.PROIX 
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
C
C      CALCUL DU TAUX DE RESTITUTION D'ENERGIE ELEMENTAIRE
C      ELEMENTS ISOPARAMETRIQUES 2D AVEC DEFORMATION INITIALE
C
C      OPTION : 'CALC_GLAG_EPSI_R' OU 'CALC_GLAG_EPSI_F'
C
C
C ENTREES  ---> OPTION : OPTION DE CALCUL
C          ---> NOMTE  : NOM DU TYPE ELEMENT
C
C VECTEURS DIMENSIONNES POUR  NNO = 9
C.......................................................................
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER       IPOIDS,IVF,IDFDE,IGEOM,ICODE
      INTEGER       ITHET,IALPH,IE1,IE2,IE3,IE4,MATER,NNO,KP,NPG1,NDIM
      INTEGER       IDEPL,ITREF,ITEMPE,IMATE,IFORC,IFORF,IGTHET
      INTEGER       COMPT,I,J,K,IDEFI,ITEMPS,NNOS,JGANO
C
      REAL*8        G,TPG,C1,C2,C3,R,ALPHA
      REAL*8        CP1,CP2,CP3,DP1,DP2,DP3,EXX,EYY,EXY,EZZ
      REAL*8        DGRT,DIVT,EPS,POIDS,VF,ADET,GEYCS,GEXYCS
      REAL*8        DFDM(7),DFDX(9),DFDY(9),GUGT(5),DUDM(7),DEDM(8)
      REAL*8        GUC(4),GUC1(4),GEGT(4),TGD(2),GEC(4),VALPA2(2)
      REAL*8        GEGC(8),GEXC(2),GEYC(2),GEXYC(2),DTDM(7)
      REAL*8        GEXC1(2),GEYC1(2),GEXYC1(2),VALRES(3),VALPAR(3)
      REAL*8        S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15
      REAL*8        S16,S17,GEXCS,GEXSC1,GEYSC1,GEXYS1,THETX,THETY,XG,YG
      REAL*8        M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15
      REAL*8        M16,M17,EXX2,EYY2,EXY2,EZZ2,A1,A2,A3,D
C                      NDIM*NNO
      REAL*8        FORCN(18)
C
      CHARACTER*2   CODRET(3)
      CHARACTER*8   NOMRES(3), NOMPAR(3)
C
      LOGICAL       FONC
C.......................................................................
C
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDE,JGANO)
      CALL JEMARQ()
      EPS = 1.D-10
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PDEPLAR','L',IDEPL)
      CALL JEVECH('PTHETAR','L',ITHET)
      CALL JEVECH('PALPHAR','L',IALPH)
      CALL JEVECH('PTEREF' ,'L',ITREF)
      CALL JEVECH('PTEMPER','L',ITEMPE)
      CALL JEVECH('PMATERC','L',IMATE)
      IF (OPTION.EQ.'CALC_GLAG_EPSI_F') THEN
        FONC = .TRUE.
        CALL JEVECH('PFFVOLU','L',IFORF)
        CALL JEVECH('PTEMPSR','L',ITEMPS)
        CALL JEVECH('PEPSINF','L',IDEFI)
        NOMPAR(1) = 'X'
        NOMPAR(2) = 'Y'
        NOMPAR(3) = 'INST'
        VALPAR(3) = ZR(ITEMPS)
      ELSE
        FONC =.FALSE.
        CALL JEVECH('PFRVOLU','L',IFORC)
        CALL JEVECH('PEPSINR','L',IDEFI)
        DO 2 I = 1 , NDIM*NNO
           FORCN(I) = ZR(IFORC+I-1)
 2      CONTINUE
        EXX = ZR(IDEFI)
        EYY = ZR(IDEFI+1)
        EXY = ZR(IDEFI+3)
        EZZ = ZR(IDEFI+2)
      ENDIF
      CALL JEVECH('PGTHETA','E',IGTHET)
C
      MATER = ZI(IMATE)
      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      NOMRES(3) = 'ALPHA'
C
C  INITIALISATION
C
      DO 203 I = 1,8
        DEDM(I) = 0.D0
  203 CONTINUE
C
      ALPHA = ZR(IALPH)
      G = 0.D0
C
C PAS DE CALCUL DE G POUR LES ELEMENTS HORS DU SUPPORT DE THETA
C
      COMPT = 0
      DO 222 I = 1,NNO
        THETX = ZR(ITHET+2* (I-1))
        THETY = ZR(ITHET+2* (I-1)+1)
        IF ((ABS(THETX).LT.EPS) .AND. (ABS(THETY).LT.EPS)) THEN
          COMPT = COMPT + 1
        END IF
222   CONTINUE
      IF (COMPT.EQ.NNO) GOTO 9999
C
C - SI CHARGE FONCTION RECUPERATION DES VALEURS EN CHAQUE NOEUD
C
      IF (FONC) THEN
        DO 50 I=1,NNO
          DO 60 J=1,NDIM
            VALPAR(J) = ZR(IGEOM+NDIM*(I-1)+J-1)
60        CONTINUE
          DO 70 J=1,NDIM
            CALL FOINTE('FM',ZK8(IFORF+J-1),NDIM+1,NOMPAR,VALPAR,
     &                                        FORCN(NDIM*(I-1)+J),ICODE)
70        CONTINUE
50      CONTINUE
      ENDIF
C
C  BOUCLE SUR LES POINTS DE GAUSS
C
      DO 800 KP = 1,NPG1
C
        K = (KP-1)*NNO
        TPG = 0.D0
        R = 0.D0
        XG = 0.D0
        YG = 0.D0
        THETX = 0.D0
        THETY = 0.D0
        DO 300 I = 1,NNO
          VF = ZR(IVF+K+I-1)
          R = R + ZR(IGEOM+2*I-2)*VF
          XG = XG + ZR(IGEOM+2* (I-1))*VF
          YG = YG + ZR(IGEOM+2* (I-1)+1)*VF
          TPG = TPG + ZR(ITEMPE+I-1)*VF
          THETX = THETX + VF*ZR(ITHET+2* (I-1))
          THETY = THETY + VF*ZR(ITHET+2* (I-1)+1)
  300   CONTINUE
C
        CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS)
C
        IF (FONC) THEN
          VALPAR(1) = XG + ALPHA*THETX
          VALPAR(2) = YG + ALPHA*THETY
          CALL FOINTE('FM',ZK8(IDEFI)  ,2,NOMPAR,VALPAR,EXX,IE1)
          CALL FOINTE('FM',ZK8(IDEFI+1),2,NOMPAR,VALPAR,EYY,IE2)
          CALL FOINTE('FM',ZK8(IDEFI+3),2,NOMPAR,VALPAR,EXY,IE3)
          CALL FOINTE('FM',ZK8(IDEFI+2),2,NOMPAR,VALPAR,EZZ,IE4)
          DO 204 I = 1,8
            DEDM(I) = 0.D0
  204     CONTINUE
C
C  CALCULE LES GRADIENTS DES DEFORMATIONS EN CHAQUE POINT DE GAUSS
C  AVEC LA PRISE EN COMPTE DU TRANSPORT
C
          DO 3 I = 1,NNO
            THETX = ZR(ITHET+2* (I-1))
            THETY = ZR(ITHET+2* (I-1)+1)
            VALPA2(1) = ZR(IGEOM+2* (I-1)) + ALPHA*THETX
            VALPA2(2) = ZR(IGEOM+2* (I-1)+1) + ALPHA*THETY
            CALL FOINTE('FM',ZK8(IDEFI)  ,2,NOMPAR,VALPA2,EXX2,IE1)
            CALL FOINTE('FM',ZK8(IDEFI+1),2,NOMPAR,VALPA2,EYY2,IE2)
            CALL FOINTE('FM',ZK8(IDEFI+3),2,NOMPAR,VALPA2,EXY2,IE3)
            CALL FOINTE('FM',ZK8(IDEFI+2),2,NOMPAR,VALPA2,EZZ2,IE4)
            DEDM(1) = DEDM(1) + EXX2*DFDX(I)
            DEDM(2) = DEDM(2) + EYY2*DFDY(I)
            DEDM(3) = DEDM(3) + EXX2*DFDY(I)
            DEDM(4) = DEDM(4) + EYY2*DFDX(I)
            DEDM(5) = DEDM(5) + EXY2*DFDX(I)
            DEDM(6) = DEDM(6) + EXY2*DFDY(I)
            DEDM(7) = DEDM(7) + EZZ2*DFDX(I)
            DEDM(8) = DEDM(8) + EZZ2*DFDY(I)
    3     CONTINUE
        END IF
C
C  FIN DU CALCUL LES GRADIENTS DES DEFORMATIONS
C
        CALL RCVALA(MATER,' ','ELAS',1,'TEMP',TPG,3,NOMRES,
     &                VALRES, CODRET, 'FM' )
C
        CP1 = VALRES(1)/ (1.D0-VALRES(2)*VALRES(2))
        CP2 = VALRES(2)*CP1
        CP3 = ((1.D0-VALRES(2))/2.D0)*CP1
        DP1 = VALRES(1)* (1.D0-VALRES(2))/
     &        ((1.D0+VALRES(2))* (1.D0-2.D0*VALRES(2)))
        DP2 = (VALRES(2)/ (1.D0-VALRES(2)))*DP1
        DP3 = ((1.D0-2.D0*VALRES(2))/ (2.D0* (1.D0-VALRES(2))))*DP1
C
        IF (NOMTE(3:4).EQ.'DP') THEN
          C1 = DP1
          C2 = DP2
          C3 = DP3
C
        ELSE
          C1 = CP1
          C2 = CP2
          C3 = CP3
          EZZ = 0.D0
          GEC(4) = 0.D0
          GEGT(4) = 0.D0
        END IF
C
C
C DEFINITIONS DES COEFFICIENTS
C
        CALL GDFONC ( DFDX,DFDY,KP,ZR(IVF),ZR(IDEPL),ZR(ITHET),FORCN,
     &                ZR(ITEMPE),NNO,DUDM,DTDM,DFDM,TGD)
C
C CALCUL DE GRAD(EPSI0).THETA
        GEC(1) = DEDM(1)*DTDM(4) + DEDM(3)*DTDM(7)
        GEC(2) = DEDM(4)*DTDM(4) + DEDM(2)*DTDM(7)
        GEC(3) = 2*DEDM(5)*DTDM(4) + 2*DEDM(6)*DTDM(7)
C
        IF (NOMTE(3:4).EQ.'DP') THEN
          GEC(4) = DEDM(7)*DTDM(4) + DEDM(8)*DTDM(7)
        END IF
C
C CALCUL DE G
C -----------
C
C*** D'ABORD LE CAS AXI
C
        IF (NOMTE(3:4).EQ.'AX') THEN
          POIDS = POIDS*R
          C1 = VALRES(1)/ (1.D0+VALRES(2))
          C2 = (1.D0-VALRES(2))/ (1.D0-2.D0*VALRES(2))
          C3 = VALRES(2)/ (1.D0-2.D0*VALRES(2))
          A1 = DTDM(1) + DTDM(2) + (DTDM(4)/XG)
          A2 = (DTDM(4)/XG)* (DTDM(1)+DTDM(2)) + DTDM(1)*DTDM(2) -
     &         DTDM(3)*DTDM(5)
          A3 = (DTDM(4)/XG)* (DTDM(1)*DTDM(2)-DTDM(3)*DTDM(5))
C
C DETERMINANT DE FALPHA :
          ADET = 1.D0 + ALPHA*A1 + ALPHA*ALPHA*A2 + (ALPHA**3)*A3
C
C FALPHA(-1) = (1/ADET) . (ID + ALPHA.C + ALPHA.ALPHA.D)
C
C CALCUL DE GRADIENT(U).C :
          GUC(1) = DUDM(1)* (DTDM(2)+ (DTDM(4)/XG)) - DUDM(3)*DTDM(5)
C
          GUC(2) = (DUDM(4)/XG)* (DTDM(2)+DTDM(1))
C
          GUC(3) = DUDM(2)* ((DTDM(4)/XG)+DTDM(1)) - DUDM(5)*DTDM(3)
C
          GUC(4) = DUDM(3)* ((DTDM(4)/XG)+DTDM(1)) +
     &             DUDM(5)* ((DTDM(4)/XG)+DTDM(2)) - DUDM(1)*DTDM(3) -
     &             DUDM(2)*DTDM(5)
C
C CALCUL DE GRADIENT(U).D :
          GUC1(1) = (DTDM(4)/XG)* (DUDM(1)*DTDM(2)-DUDM(3)*DTDM(5))
          GUC1(2) = (DUDM(4)/XG)* (DTDM(1)*DTDM(2)-DTDM(3)*DTDM(5))
          GUC1(3) = (DTDM(4)/XG)* (DUDM(2)*DTDM(1)-DUDM(5)*DTDM(3))
          GUC1(4) = (DTDM(4)/XG)* (DUDM(3)*DTDM(1)+DUDM(5)*DTDM(2)-
     &              DUDM(1)*DTDM(3)-DUDM(2)*DTDM(5))
C
C CALCUL DES GRADIENT(THETA).THETA ET
C            GRADIENT(THETA).C.THETA ET
C            GRADIENT(THETA).D.THETA ET :

          GEXC(1) = DEDM(1)* (DTDM(2)+ (DTDM(4)/XG)) - DEDM(3)*DTDM(5)
          GEXC(2) = -DEDM(1)*DTDM(3) + DEDM(3)* (DTDM(1)+ (DTDM(4)/XG))
          GEYC(1) = DEDM(4)* (DTDM(2)+ (DTDM(4)/XG)) - DEDM(2)*DTDM(5)
          GEYC(2) = -DEDM(4)*DTDM(3) + DEDM(2)* (DTDM(1)+ (DTDM(4)/XG))
C
          GEXYC(1) = DEDM(5)* (DTDM(2)+ (DTDM(4)/XG)) - DEDM(6)*DTDM(5)
          GEXYC(2) = -DEDM(5)*DTDM(3) + DEDM(6)* (DTDM(1)+ (DTDM(4)/XG))
C
          GEXCS = GEXC(1)*DTDM(4) + GEXC(2)*DTDM(7)
          GEYCS = GEYC(1)*DTDM(4) + GEYC(2)*DTDM(7)
C
          GEXYCS = GEXYC(1)*DTDM(4) + GEXYC(2)*DTDM(7)
C
          GEXC1(1) = (DTDM(4)/XG)* (DTDM(2)*DEDM(1)-DTDM(5)*DEDM(3))
          GEXC1(2) = (DTDM(4)/XG)* (-DTDM(3)*DEDM(1)+DTDM(1)*DEDM(3))
C
          GEYC1(1) = (DTDM(4)/XG)* (DTDM(2)*DEDM(4)-DTDM(5)*DEDM(2))
          GEYC1(2) = (DTDM(4)/XG)* (-DTDM(3)*DEDM(4)+DTDM(1)*DEDM(2))
C
          GEXYC1(1) = (DTDM(4)/XG)* (DTDM(2)*DEDM(5)-DTDM(5)*DEDM(6))
          GEXYC1(2) = (DTDM(4)/XG)* (-DTDM(3)*DEDM(5)+DTDM(1)*DEDM(6))
C
          GEXSC1 = GEXC1(1)*DTDM(4) + GEXC1(2)*DTDM(7)
          GEYSC1 = GEYC1(1)*DTDM(4) + GEYC1(2)*DTDM(7)
          GEXYS1 = GEXYC1(1)*DTDM(4) + GEXYC1(2)*DTDM(7)
C
C COEFFICIENT DES INTEGRALES :
          M1 = (A1+2.D0*ALPHA*A2+3.D0*ALPHA*ALPHA*A3)/ (2.D0)
          M2 = (2.D0*ALPHA+ALPHA*ALPHA*A1- (ALPHA**4)*A3)/ (-2.D0)
          M3 = (4.D0* (ALPHA**3)+3.D0* (ALPHA**4)*A1+2.D0*
     &         (ALPHA**5)*A2+ (ALPHA**6)*A3)/ (-2.D0)
          M4 = (1-ALPHA*ALPHA*A2-2.D0* (ALPHA**3)*A3)/ (-1.D0)
          M5 = 2.D0*M2
C
          M6 = (3.D0*ALPHA*ALPHA+2.D0* (ALPHA**3)*A1+ (ALPHA**4)*A2)/
     &         (-1.D0)
          M7 = 1.D0
          M8 = 2*ALPHA
          M9 = M7
          M10 = ALPHA
          M11 = ALPHA*ALPHA
          M12 = M10
          M13 = M11
          M14 = M11*ALPHA
          M15 = M11
          M16 = M14
          M17 = M14*ALPHA
C
C CALCUL DES INTEGRALES :
          S1 = C1*C2* (DUDM(1)*DUDM(1)) + 2.D0*C1*C3*DUDM(2)*DUDM(1) +
     &         2.D0*C1*C3* (DUDM(4)/XG)*DUDM(1) +
     &         2.D0*C1*C3* (DUDM(4)/XG)*DUDM(2) +
     &         C1*C2* (DUDM(4)*DUDM(4)/XG**2) + C1*C2*DUDM(2)*DUDM(2) +
     &         C1/2.D0* (DUDM(3)+DUDM(5))* (DUDM(3)+DUDM(5))
C
          S2 = C1*C2* ((GUC(1)*GUC(1))+ (GUC(2)*GUC(2))+
     &         (GUC(3)*GUC(3))) + C1/2.D0* (GUC(4)*GUC(4)) +
     &         2.D0*C1*C3* (GUC(1)*GUC(2)+GUC(2)*GUC(3)+GUC(1)*GUC(3))
C
          S3 = C1*C2* ((GUC1(1)*GUC1(1))+ (GUC1(2)*GUC1(2))+
     &         (GUC1(3)*GUC1(3))) + C1/2.D0* (GUC1(4)*GUC1(4)) +
     &         2.D0*C1*C3* (GUC1(1)*GUC1(2)+GUC1(2)*GUC1(3)+
     &         GUC1(1)*GUC1(3))
C
          S4 = C1*GUC(1)* (C2*DUDM(1)+C3* (DUDM(4)/XG)+C3*DUDM(2)) +
     &         C1*GUC(2)* (C3*DUDM(1)+C2* (DUDM(4)/XG)+C3*DUDM(2)) +
     &         C1*GUC(3)* (C3*DUDM(1)+C3* (DUDM(4)/XG)+C2*DUDM(2)) +
     &         C1/2.D0* (DUDM(5)+DUDM(3))*GUC(4)
C
          S5 = C1*GUC1(1)* (C2*DUDM(1)+C3* (DUDM(4)/XG)+C3*DUDM(2)) +
     &         C1*GUC1(2)* (C3*DUDM(1)+C2* (DUDM(4)/XG)+C3*DUDM(2)) +
     &         C1*GUC1(3)* (C3*DUDM(1)+C3* (DUDM(4)/XG)+C2*DUDM(2)) +
     &         C1/2.D0*GUC1(4)* (DUDM(5)+DUDM(3))
C
          S6 = C1*C2* ((GUC(1)*GUC1(1))+ (GUC(2)*GUC1(2))+
     &         (GUC(3)*GUC1(3))) + C1*C3* (GUC(1)*GUC1(2)+
     &         GUC(2)*GUC1(3)+GUC(1)*GUC1(3)+GUC(2)*GUC1(1)+
     &         GUC(3)*GUC1(1)+GUC(3)*GUC1(2)) + C1/2.D0*GUC(4)*GUC1(4)
C
          S7 = C1* (C2*EXX+C3*EYY)*GUC(1) + C1* (C3*EXX+C3*EYY)*GUC(2) +
     &         C1* (C3*EXX+C2*EYY)*GUC(3) + C1*EXY*GUC(4)

          S8 = C1*GUC1(1)* (C2*EXX+C3*EYY) +
     &         C1*GUC1(2)* (C3*EXX+C3*EYY) +
     &         C1*GUC1(3)* (C3*EXX+C2*EYY) + C1*EXY*GUC1(4)
C
          S9 = C1*DUDM(1)* (C2*GEC(1)+C3*GEC(2)) +
     &         C1*DUDM(2)* (C3*GEC(1)+C2*GEC(2)) +
     &         C1* (DUDM(4)/XG)* (C3*GEC(1)+C3*GEC(2)) +
     &         C1/2.D0* (DUDM(3)+DUDM(5))*GEC(3)
C
          S10 = C1*GUC(1)* (C2*GEC(1)+C3*GEC(2)) +
     &          C1*GUC(2)* (C3*GEC(1)+C3*GEC(2)) +
     &          C1*GUC(3)* (C3*GEC(1)+C2*GEC(2)) + C1/2.D0*GUC(4)*GEC(3)
C
          S11 = C1*GUC1(1)* (C2*GEC(1)+C3*GEC(2)) +
     &          C1*GUC1(2)* (C3*GEC(1)+C3*GEC(2)) +
     &          C1*GUC1(3)* (C3*GEC(1)+C2*GEC(2)) +
     &          C1/2.D0*GUC1(4)*GEC(3)
C
          S12 = C1*DUDM(1)* (C2*GEXCS+C3*GEYCS) +
     &          C1*DUDM(2)* (C3*GEXCS+C2*GEYCS) +
     &          C1* (DUDM(4)/XG)* (C3*GEXCS+C3*GEYCS) +
     &          C1/2.D0*GEXYCS* (DUDM(3)+DUDM(5))
C
          S13 = C1*GUC(1)* (C2*GEXCS+C3*GEYCS) +
     &          C1*GUC(2)* (C3*GEXCS+C3*GEYCS) +
     &          C1*GUC(3)* (C3*GEXCS+C2*GEYCS) + C1/2.D0*GUC(4)*GEXYCS
C
          S14 = C1*GUC1(1)* (C2*GEXCS+C3*GEYCS) +
     &          C1*GUC1(2)* (C3*GEXCS+C3*GEYCS) +
     &          C1*GUC1(3)* (C3*GEXCS+C2*GEYCS) + C1/2.D0*GUC1(4)*GEXYCS
C
          S15 = C1*DUDM(1)* (C2*GEXSC1+C3*GEYSC1) +
     &          C1*DUDM(2)* (C3*GEXSC1+C2*GEYSC1) +
     &          C1* (DUDM(4)/XG)* (C3*GEXSC1+C3*GEYSC1) +
     &          C1/2.D0*GEXYS1* (DUDM(3)+DUDM(5))
C
          S16 = C1*GUC(1)* (C2*GEXSC1+C3*GEYSC1) +
     &          C1*GUC(2)* (C3*GEXSC1+C3*GEYSC1) +
     &          C1*GUC(3)* (C3*GEXSC1+C2*GEYSC1) + C1/2.D0*GUC(4)*GEXYS1
C
          S17 = C1*GUC1(1)* (C2*GEXSC1+C3*GEYSC1) +
     &          C1*GUC1(2)* (C3*GEXSC1+C3*GEYSC1) +
     &          C1*GUC1(3)* (C3*GEXSC1+C2*GEYSC1) +
     &          C1/2.D0*GUC1(4)*GEXYS1
C
C
          G = G + POIDS* (M1*S1+M2*S2+M3*S3+M4*S4+M5*S5+M6*S6)/
     &        (ADET*ADET) + POIDS* (M7*S7+M8*S8) +
     &        POIDS* (M9*S9+M10*S10+M11*S11+M12*S12+M13*S13+M14*S14+
     &        M15*S15+M16*S16+M17*S17)/ADET
C
C** SI ON N'EST PAS EN AXI ALORS
C
        ELSE
C CALCUL DE DIV(THETA) ET DET(GRADIENT(THETA)) :
          DIVT = DTDM(1) + DTDM(2)
          DGRT = DTDM(1)*DTDM(2) - DTDM(3)*DTDM(5)
C
          GEGC(1) = DEDM(1)*DTDM(1) + DEDM(3)*DTDM(5)
          GEGC(2) = DEDM(1)*DTDM(3) + DEDM(3)*DTDM(2)
          GEGC(3) = DEDM(4)*DTDM(1) + DEDM(2)*DTDM(5)
          GEGC(4) = DEDM(4)*DTDM(3) + DEDM(2)*DTDM(2)
          GEGC(5) = 2*DEDM(5)*DTDM(1) + 2*DEDM(6)*DTDM(5)
          GEGC(6) = 2*DEDM(5)*DTDM(3) + 2*DEDM(6)*DTDM(2)
C
          IF (NOMTE(3:4).EQ.'DP') THEN
            GEGC(7) = DEDM(7)*DTDM(1) + DEDM(8)*DTDM(5)
            GEGC(8) = DEDM(7)*DTDM(3) + DEDM(8)*DTDM(2)
            GEGT(4) = DTDM(4)*GEGC(7) + DTDM(7)*GEGC(8)
          END IF
C
C CALCUL DE GRAD(EPSI0).GRAD(THETA).THETA :
          GEGT(1) = DTDM(4)*GEGC(1) + DTDM(7)*GEGC(2)
          GEGT(2) = DTDM(4)*GEGC(3) + DTDM(7)*GEGC(4)
          GEGT(3) = DTDM(4)*GEGC(5) + DTDM(7)*GEGC(6)
          IF (ABS(DGRT).LT.EPS) THEN
            DGRT = 0.D0
          END IF
C
C CALCUL DE GRAD(U).GRAD(THETA) :
          GUGT(1) = DUDM(1)*DTDM(1) + DUDM(3)*DTDM(5)
          GUGT(2) = DUDM(5)*DTDM(3) + DUDM(2)*DTDM(2)
          GUGT(3) = DUDM(1)*DTDM(3) + DUDM(3)*DTDM(2)
          GUGT(5) = DUDM(5)*DTDM(1) + DUDM(2)*DTDM(5)
C
C CALCUL DES INTEGRALES :
          S1 = C1* (DUDM(1)*DUDM(1)+DUDM(2)*DUDM(2)) +
     &         C2* (DUDM(2)*DUDM(1)+DUDM(1)*DUDM(2)) +
     &         C3* (DUDM(3)+DUDM(5))* (DUDM(3)+DUDM(5))
C
          S2 = C1* (DUDM(1)*GUGT(1)+DUDM(2)*GUGT(2)) +
     &         C2* (DUDM(2)*GUGT(1)+DUDM(1)*GUGT(2)) +
     &         C3* (DUDM(3)+DUDM(5))* (GUGT(3)+GUGT(5))
C
          S3 = C1* (GUGT(1)*GUGT(1)+GUGT(2)*GUGT(2)) +
     &         C2* (GUGT(2)*GUGT(1)+GUGT(1)*GUGT(2)) +
     &         C3* (GUGT(3)+GUGT(5))* (GUGT(3)+GUGT(5))
C
          S4 = (C1*EXX+C2* (EYY+EZZ))*DUDM(1) +
     &         (C2* (EXX+EZZ)+C1*EYY)*DUDM(2) +
     &         2*C3*EXY* (DUDM(3)+DUDM(5))
C
          S5 = (C1*EXX+C2* (EYY+EZZ))*GUGT(1) +
     &         (C2* (EXX+EZZ)+C1*EYY)*GUGT(2) +
     &         2*C3*EXY* (GUGT(3)+GUGT(5))
C
          S6 = (C1*GEC(1)+C2* (GEC(2)+GEC(4)))*DUDM(1) +
     &         (C2* (GEC(1)+GEC(4))+C1*GEC(2))*DUDM(2) +
     &         C3* (DUDM(3)+DUDM(5))*GEC(3)
C
          S7 = (C1*GEC(1)+C2* (GEC(2)+GEC(4)))*GUGT(1) +
     &         (C2* (GEC(1)+GEC(4))+C1*GEC(2))*GUGT(2) +
     &         C3* (GUGT(3)+GUGT(5))*GEC(3)
C
          S8 = (C1*GEGT(1)+C2* (GEGT(2)+GEGT(4)))*DUDM(1) +
     &         (C2* (GEGT(1)+GEGT(4))+C1*GEGT(2))*DUDM(2) +
     &         C3* (DUDM(3)+DUDM(5))*GEGT(3)
C
          S9 = (C1*GEGT(1)+C2* (GEGT(2)+GEGT(4)))*GUGT(1) +
     &         (C2* (GEGT(1)+GEGT(4))+C1*GEGT(2))*GUGT(2) +
     &         C3* (GUGT(3)+GUGT(5))*GEGT(3)
C
C CALCUL DES COEFFICIENTS DES INTEGRALES
          M1 = (ALPHA*ALPHA* (2*DGRT*DIVT-DIVT**3)+
     &         2*ALPHA* (DGRT-DIVT*DIVT)-DIVT)/ (2.D0)
          M2 = ALPHA*ALPHA* (DIVT*DIVT-DGRT) + 2*ALPHA*DIVT + 1
          M3 = (ALPHA*ALPHA*DIVT+2*ALPHA)/ (-2.D0)
          M4 = DIVT
          M5 = -1.D0
          M6 = (1.D0+2*ALPHA*DIVT+ ((ALPHA**2)*DIVT**2))
          M7 = -ALPHA - ((ALPHA**2)*DIVT)
          M8 = M7
          M9 = ALPHA**2
C
C CALCUL DE DET(FALPHA)
          D = 1 + ALPHA*DIVT + ALPHA*ALPHA*DGRT
C
          G = G + POIDS* ((M1*S1+M2*S2+M3*S3)/ (D*D)+ (M4*S4+M5*S5)+
     &        (M6*S6+M7*S7+M8*S8+M9*S9)/D)
C
        ENDIF
800   CONTINUE
9999  CONTINUE
C
      ZR(IGTHET) = G
C
      CALL JEDEMA()
      END

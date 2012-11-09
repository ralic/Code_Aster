       SUBROUTINE TE0299(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
      IMPLICIT NONE
C
C.......................................................................
C FONCTION REALISEE:
C
C      CALCUL DES COEFFICIENTS DE CONTRAINTES K1 ET K2
C      A PARTIR DE LA FORME BILINEAIRE SYMETRIQUE G
C      ET DES DEPLACEMENTS SINGULIERS EN FOND DE FISSURE
C
C      POUR LES ELEMENTS ISOPARAMETRIQUES 2D
C
C      OPTION : 'CALC_K_G'    (CHARGES REELLES)
C               'CALC_K_G_F'  (CHARGES FONCTIONS)
C
C ENTREES  ---> OPTION : OPTION DE CALCUL
C          ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................
C
      INCLUDE 'jeveux.h'

      INTEGER ICODRE(2)
      CHARACTER*4  FAMI
      CHARACTER*8  NOMRES(2),NOMPAR(3)
      CHARACTER*16 NOMTE,OPTION,PHENOM
C
      REAL*8   EPSI,R8PREM
      REAL*8   DFDI(18),F(3,3),EPS(6),FNO(18)
      REAL*8   DUDM(3,4),DFDM(3,4),DTDM(3,4),DER(4)
      REAL*8   DU1DM(3,4),DU2DM(3,4)
      REAL*8   RHO,OM,OMO,RBID,E,NU, RBID2(3,3,3)
      REAL*8   THET,TPN(20),TGDM(3)
      REAL*8   XAG,YAG,XG,YG,XA,YA,NORM,A,B
      REAL*8   C1,C2,C3,CS, U1(2), U2(2)
      REAL*8   E1(3),E2(3),E3(3),P(3,3),INVP(3,3),RG,TG
      REAL*8   U1L(3),U2L(3),RBID3(3),MU, KA, RBID4(3,4)
      REAL*8   TH,VALRES(2),VALPAR(3)
      REAL*8   COEFK, DU1DME(3,3), DU2DME(3,3)
      REAL*8   GELEM,GUV1,GUV2,GUV3,K1,K2,G,POIDS,RAY
C
      INTEGER  IPOIDS,IVF,IDFDE,NNO,KP,NPG1,COMPT,IER,NNOS,JGANO
      INTEGER  IGEOM,ITHET,IROTA,IPESA,IFIC,IDEPL,IRET
      INTEGER  IMATE,IFORC,IFORF,IFOND,ITEMPS,K,I,J,KK,L,NDIM
C
      LOGICAL  FONC,AXI,LTEATT, LCOUR
C

C
      CALL JEMARQ()
C
      FAMI = 'RIGI'
      CALL ELREF4(' ',FAMI,NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDE,JGANO)
      CALL JEVECH('PTHETAR','L',ITHET)
C
C - PAS DE CALCUL DE G POUR LES ELEMENTS OU LA VALEUR DE THETA EST NULLE
C
      AXI = .FALSE.
      IF ( LTEATT(' ','AXIS','OUI'))  AXI = .TRUE.
      GUV1   = 0.D0
      GUV2   = 0.D0
      GUV3   = 0.D0
      COMPT = 0
      EPSI = R8PREM()
      DO 15 I=1,NNO
        THET = 0.D0
        DO 14 J=1,NDIM
          THET = THET + ABS(ZR(ITHET+NDIM*(I-1)+J-1))
14      CONTINUE
        IF(THET.LT.EPSI) COMPT = COMPT+1
15    CONTINUE
      IF(COMPT.EQ.NNO)  GOTO 9999
C
C - RECUPERATION CHARGES, MATER... ----------------
      EPSI = R8PREM()
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PDEPLAR','L',IDEPL)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PFISSR' ,'L',IFOND)
      IF (OPTION.EQ.'CALC_K_G_F') THEN
        FONC = .TRUE.
        CALL JEVECH('PFFVOLU','L',IFORF)
        CALL JEVECH('PTEMPSR','L',ITEMPS)
        NOMPAR(1) = 'X'
        NOMPAR(2) = 'Y'
        NOMPAR(3) = 'INST'
        VALPAR(3) = ZR(ITEMPS)
      ELSE
        FONC =.FALSE.
        CALL JEVECH('PFRVOLU','L',IFORC)
      ENDIF
      CALL TECACH('ONN','PPESANR',1,IPESA,IRET)
      CALL TECACH('ONN','PROTATR',1,IROTA,IRET)
C
      CALL JEVECH('PGTHETA','E',IFIC)

      DO 646 KP = 1,NNO
        CALL RCVARC(' ','TEMP','+','NOEU',KP,1,TPN(KP),IRET)
        IF ( IRET.NE.0 ) TPN(KP) = 0.D0
  646 CONTINUE

      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      NORM = SQRT(ZR(IFOND-1+3)**2+ZR(IFOND-1+4)**2)
      A =  ZR(IFOND-1+4)/NORM
      B = -ZR(IFOND-1+3)/NORM
C
C - RECUPERATION DES CHARGES ET DEFORMATIONS INITIALES ----------------
C
      IF (FONC) THEN
        DO 50 I=1,NNO
          DO 30 J=1,NDIM
            VALPAR(J) = ZR(IGEOM+NDIM*(I-1)+J-1)
30        CONTINUE
          DO 40 J=1,NDIM
            KK = NDIM*(I-1)+J
            CALL FOINTE('FM',ZK8(IFORF+J-1),3,NOMPAR,VALPAR,FNO(KK),IER)
40        CONTINUE
50      CONTINUE
      ELSE
        DO 80 I=1,NNO
          DO 60 J=1,NDIM
            FNO(NDIM*(I-1)+J)= ZR(IFORC+NDIM*(I-1)+J-1)
60        CONTINUE
80      CONTINUE
      ENDIF
C
      IF ((IPESA.NE.0).OR.(IROTA.NE.0)) THEN
        CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,ICODRE)
        CALL RCVALB('RIGI',1,1,'+',ZI(IMATE),' ',PHENOM,
     &              1,' ',RBID,1,'RHO',RHO,
     &              ICODRE,1)
        IF (IPESA.NE.0) THEN
          DO 95 I=1,NNO
            DO 90 J=1,NDIM
              KK = NDIM*(I-1)+J
              FNO(KK)=FNO(KK)+RHO*ZR(IPESA)*ZR(IPESA+J)
90          CONTINUE
95        CONTINUE
        ENDIF
        IF (IROTA.NE.0) THEN
          OM = ZR(IROTA)
          DO 105 I=1,NNO
            OMO = 0.D0
            DO 100 J=1,NDIM
              OMO = OMO + ZR(IROTA+J)* ZR(IGEOM+NDIM*(I-1)+J-1)
100         CONTINUE
            DO 103 J=1,NDIM
              KK = NDIM*(I-1)+J
              FNO(KK)=FNO(KK)+RHO*OM*OM*(ZR(IGEOM+KK-1)-OMO*ZR(IROTA+J))
103         CONTINUE
105       CONTINUE
        ENDIF
      ENDIF
C
C ======================================================================
C
      DO 800 KP=1,NPG1
        L  = (KP-1)*NNO
        XG = 0.D0
        YG = 0.D0
        CALL VECINI(9,0.D0,DU1DME)
        CALL VECINI(9,0.D0,DU2DME)
        DO 220 I=1,3
          TGDM(I) = 0.D0
          DO 210 J=1,4
            DUDM(I,J) = 0.D0
            DU1DM(I,J)= 0.D0
            DU2DM(I,J)= 0.D0
            DTDM(I,J) = 0.D0
            DFDM(I,J) = 0.D0
210       CONTINUE
220     CONTINUE
C
C - CALCUL DES ELEMENTS GEOMETRIQUES
C
        CALL NMGEOM (NDIM,NNO,AXI,.FALSE.,ZR(IGEOM),KP,
     &               IPOIDS,IVF,IDFDE,
     &               ZR(IDEPL),.TRUE.,POIDS,DFDI,F,EPS,RAY)
C
C - CALCULS DES GRADIENTS DE U (DUDM),THETA (DTDM) ET FORCE(DFDM)
C   DU GRADIENT DE TEMPERATURE AUX POINTS DE GAUSS (TGDM)
C
        DO 320 I=1,NNO
          DER(1) = DFDI(I)
          DER(2) = DFDI(I+NNO)
          DER(3) = 0.D0
          DER(4) = ZR(IVF+L+I-1)
          XG = XG + ZR(IGEOM+2*(I-1)  )*DER(4)
          YG = YG + ZR(IGEOM+2*(I-1)+1)*DER(4)

          DO 310 J=1,NDIM
            TGDM(J)     = TGDM(J)   + TPN(I)*DER(J)

            DO 300 K=1,NDIM
              DUDM(J,K) = DUDM(J,K) + ZR(IDEPL+NDIM*(I-1)+J-1)*DER(K)
              DTDM(J,K) = DTDM(J,K) + ZR(ITHET+NDIM*(I-1)+J-1)*DER(K)
              DFDM(J,K) = DFDM(J,K) + FNO(NDIM*(I-1)+J)*DER(K)
300         CONTINUE
              DUDM(J,4) = DUDM(J,4) + ZR(IDEPL+NDIM*(I-1)+J-1)*DER(4)
              DTDM(J,4) = DTDM(J,4) + ZR(ITHET+NDIM*(I-1)+J-1)*DER(4)
              DFDM(J,4) = DFDM(J,4) + FNO(NDIM*(I-1)+J)*DER(4)
310       CONTINUE
320     CONTINUE
        IF (AXI) THEN
          IF (RAY .LT.  R8PREM()) CALL U2MESS('F','RUPTURE0_56')
          DUDM(3,3)= DUDM(1,4)/RAY
          DTDM(3,3)= DTDM(1,4)/RAY
          DFDM(3,3)= DFDM(1,4)/RAY
        ENDIF

        CALL RCVALB (FAMI,KP,1,'+',ZI(IMATE),' ','ELAS',0,' ',0.D0,
     &               2,NOMRES,VALRES,ICODRE,1)
        E     = VALRES(1)
        NU    = VALRES(2)
        C3 = E/(2.D0*(1.D0+NU))
        IF ( LTEATT(' ','D_PLAN','OUI').OR.
     &       LTEATT(' ','AXIS','OUI') ) THEN
          MU = E/(2.D0*(1.D0+NU))
          KA = 3.D0-4.D0*NU
          C1 = E*(1.D0-NU)/((1.D0+NU)*(1.D0-2.D0*NU))
          C2 = NU/(1.D0-NU)*C1
          TH = 1.D0
          COEFK = E/(1.D0-NU*NU)
        ELSE
          KA = (3.D0-NU)/(1.D0+NU)
          MU  = E/(2.D0*(1.D0+NU))
          C1 = E/(1.D0-NU*NU)
          C2 = NU*C1
          TH = (1.D0-2.D0*NU)/(1.D0-NU)
          COEFK = E
        ENDIF

C       ------------------------------------------------
C      CALCUL DES CHAMPS AUXILIAIRES ET DE LEURS DERIVEES
C       -----------------------------------------------------
       XA = ZR(IFOND-1+1)
       YA = ZR(IFOND-1+2)

C       COORDONNÉES POLAIRES DU POINT
       XAG =  A*(XG-XA)+B*(YG-YA)
       YAG = -B*(XG-XA)+A*(YG-YA)
       RG = SQRT(XAG*XAG+YAG*YAG)

        IF (RG.GT.R8PREM()) THEN
C         LE POINT N'EST PAS SUR LE FOND DE FISSURE
          TG = ATAN2(YAG,XAG)
          IRET=1
        ELSE
C         LE POINT EST SUR LE FOND DE FISSURE :
C         L'ANGLE N'EST PAS DÉFINI, ON LE MET À ZÉRO
C         ON NE FERA PAS LE CALCUL DES DÉRIVÉES
          TG=0.D0
          IRET=0
        ENDIF
C       ON A PAS PU CALCULER LES DERIVEES DES FONCTIONS SINGULIERES
C       CAR ON SE TROUVE SUR LE FOND DE FISSURE
        CALL ASSERT(IRET.NE.0)

        LCOUR=.FALSE.
C
C       CALCUL DE LA MATRICE DE PASSAGE P TQ 'GLOBAL' = P * 'LOCAL'
        CALL VECINI(9,0.D0,P)

        E1(1) = A
        E1(2) = B
        E1(3) = 0
        E2(1) = -B
        E2(2) = A
        E2(3) = 0
        E3(1) = 0
        E3(2) = 0
        E3(3) = 0

        DO 120 I=1,3
          P(I,1)=E1(I)
          P(I,2)=E2(I)
          P(I,3)=E3(I)
 120    CONTINUE

C       CALCUL DE L'INVERSE DE LA MATRICE DE PASSAGE : INV=TRANSPOSE(P)
        DO 130 I=1,3
          DO 131 J=1,3
            INVP(I,J)=P(J,I)
 131      CONTINUE
 130    CONTINUE

        CALL CHAUXI(NDIM,MU,KA,RG,TG,INVP,LCOUR,RBID2,
     &              DU1DME,DU2DME,RBID4,U1L,U2L,RBID3)

C       CHAMPS SINGULIERS DANS LA BASE GLOBALE
        CALL VECINI(NDIM,0.D0,U1)
        CALL VECINI(NDIM,0.D0,U2)
        DO 510 I=1,NDIM
          DO 511 J=1,NDIM
            U1(I) = U1(I) + P(I,J) * U1L(J)
            U2(I) = U2(I) + P(I,J) * U2L(J)
 511      CONTINUE
 510    CONTINUE

         DO 700 I=1,NDIM
          DO 701 J=1,NDIM
            DU1DM(I,J) = DU1DME(I,J)
            DU2DM(I,J) = DU2DME(I,J)
 701      CONTINUE
 700    CONTINUE
C
       IF (AXI) THEN
          DU1DM(3,3)= U1(1)/RAY
          DU2DM(3,3)= U2(1)/RAY
       ENDIF
C
C   INTRODUCTION DES DEPLACEMENTS SINGULIERS ET DE LEURS DERIVEES
C   A        POINT EN FOND DE FISSURE
C   RPOL,PHI COORDONNEES POLAIRES DU POINT DE GAUSS
C   INTRODUCTION DE U1S ET U2S DANS G(U,V)
C
        GELEM =0.D0
        CS    =0.5D0
        CALL GBILIN(FAMI,KP,ZI(IMATE),DUDM,DU1DM,DTDM,DFDM,TGDM,POIDS,
     &              C1,C2,C3,CS,TH,1.D0,0.D0,0.D0,AXI,GELEM)
        GUV1  = GUV1 + GELEM
C
        GELEM =0.D0
        CS    =0.5D0
        CALL GBILIN(FAMI,KP,ZI(IMATE),DUDM,DU2DM,DTDM,DFDM,TGDM,POIDS,
     &              C1,C2,C3,CS,TH,1.D0,0.D0,0.D0,AXI,GELEM)
        GUV2  = GUV2 + GELEM
C
        GELEM =0.D0
        CS    =1.D0
        CALL GBILIN(FAMI,KP,ZI(IMATE),DUDM,DUDM,DTDM,DFDM,TGDM,POIDS,
     &            C1,C2,C3,CS,TH,2.D0,0.D0,0.D0,AXI,GELEM)
        GUV3  = GUV3 + GELEM
800   CONTINUE
C
      K1= GUV1*COEFK
      K2= GUV2*COEFK
      G = GUV3
C
      ZR(IFIC)   = G
      ZR(IFIC+1) = K1/SQRT(COEFK)
      ZR(IFIC+2) = K2/SQRT(COEFK)
      ZR(IFIC+3) = K1
      ZR(IFIC+4) = K2
C
9999  CONTINUE
      CALL JEDEMA()
      END

      SUBROUTINE TE0383(OPTION,NOMTE)
      IMPLICIT   NONE
      CHARACTER*16 OPTION,NOMTE
C.......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 08/09/2003   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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

C      CALCUL DU TAUX DE RESTITUTION D'ENERGIE ELEMENTAIRE
C      ELEMENTS ISOPARAMETRIQUES 3D

C      OPTION : 'CALC_G_LGLO'    (CHARGES REELLES)
C               'CALC_G_LGLO_F'  (CHARGES FONCTIONS)

C ENTREES  ---> OPTION : OPTION DE CALCUL
C          ---> NOMTE  : NOM DU TYPE ELEMENT

C VECTEURS DIMENSIONNES POUR  NNO = 27
C.......................................................................
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

      INTEGER IPOIDS,IVF,IDFDE,IDFDN,IDFDK,IGEOM,IDEPL,ITREF
      INTEGER ITEMPE,IMATE,IFORC,IGTHET,JVAL,COMPT
      INTEGER ITHET1,ITHET,IFORF,ITEMPS,ICOMPO,ICODE,IALPH
      INTEGER JGANO,NNO,NDIM,KP,NPG1,I,J,K,L,NNOS

      REAL*8 TPG,VALRES(3),DEVRES(3),VALPAR(4)
      REAL*8 C11,C12,C13,C21,C22,C23,C31,C32,C33,C1,C2,C3
      REAL*8 D11,D12,D13,D21,D22,D23,D31,D32,D33,DC2,DC3
      REAL*8 DETFA,DDETFA,COEF,ALPHA,ALPHD
      REAL*8 M1,M2,S1,S2,G,GUC(6),GUD(6),DT2DM(3,3),DUDM(3,3)
      REAL*8 EPS,POIDS,ZERO,UN,DEUX,UNDEMI,THETX,THETY,THETZ
      REAL*8 DER(3),DFDX(27),DFDY(27),DFDZ(27),DTDM(3,3)
C                        NDIM*NNO
      REAL*8 FORCN(81)

      CHARACTER*2 CODRET(3)
      CHARACTER*8 NOMRES(3),NOMPAR(4)

      LOGICAL FONC
C.......................................................................
      DATA ZERO,UN,DEUX,EPS/0.0D0,1.0D0,2.0D0,1.0D-15/
C.......................................................................

      CALL JEMARQ()

      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDE,JGANO)
      IDFDN = IDFDE + 1
      IDFDK = IDFDN + 1
      DO 10 I = 1,1
   10 CONTINUE

      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PDEPLAR','L',IDEPL)
      CALL JEVECH('PTHETA1','L',ITHET1)
      CALL JEVECH('PTHETAR','L',ITHET)
      CALL JEVECH('PALPHAR','L',IALPH)
      CALL JEVECH('PTEREF','L',ITREF)
      CALL JEVECH('PTEMPER','L',ITEMPE)
      IF (OPTION.EQ.'CALC_G_LGLO_F') THEN
        FONC = .TRUE.
        CALL JEVECH('PFFVOLU','L',IFORF)
        CALL JEVECH('PTEMPSR','L',ITEMPS)
        NOMPAR(1) = 'X'
        NOMPAR(2) = 'Y'
        NOMPAR(3) = 'Z'
        NOMPAR(4) = 'INST'
        VALPAR(4) = ZR(ITEMPS)
      ELSE
        FONC = .FALSE.
        CALL JEVECH('PFRVOLU','L',IFORC)
        DO 20 I = 1,NNO*NDIM
          FORCN(I) = ZR(IFORC+I-1)
   20   CONTINUE
      END IF
      CALL JEVECH('PCOMPOR','L',ICOMPO)
      CALL JEVECH('PMATERC','L',IMATE)
      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      NOMRES(3) = 'ALPHA'
      CALL JEVECH('PGTHETA','E',IGTHET)

      ALPHA = ZR(IALPH)

C - PAS DE CALCUL DE G POUR LES ELEMENTS OU LA VALEUR DE THETA EST NULLE

      COMPT = 0
      DO 30 I = 1,NNO
        THETX = ZR(ITHET+3* (I-1)+1-1)
        THETY = ZR(ITHET+3* (I-1)+2-1)
        THETZ = ZR(ITHET+3* (I-1)+3-1)
        IF ((ABS(THETX).LT.EPS) .AND. (ABS(THETY).LT.EPS) .AND.
     &      (ABS(THETZ).LT.EPS)) THEN
          COMPT = COMPT + 1
        END IF

   30 CONTINUE
      IF (COMPT.EQ.NNO) GO TO 130

C - SI CHARGE FONCTION RECUPERATION DES VALEURS EN CHAQUE NOEUD

      IF (FONC) THEN
        DO 60 I = 1,NNO
          DO 40 J = 1,NDIM
            VALPAR(J) = ZR(IGEOM+NDIM* (I-1)+J-1)
   40     CONTINUE
          DO 50 J = 1,NDIM
            CALL FOINTE('FM',ZK8(IFORF+J-1),NDIM+1,NOMPAR,VALPAR,
     &                  FORCN(NDIM* (I-1)+J),ICODE)
   50     CONTINUE
   60   CONTINUE
      END IF

C ======================================================================

      UNDEMI = 0.5D0
      G = ZERO

      DO 120 KP = 1,NPG1
        L = (KP-1)*NNO
        K = L*3
        TPG = ZERO
        CALL DFDM3D(NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDN+K),
     &              ZR(IDFDK+K),ZR(IGEOM),DFDX,DFDY,DFDZ,POIDS)

C  CALCUL DES GRADIENTS DE U, THETA1, THETA2
C  -----------------------------------------

        DO 80 I = 1,3
          DO 70 J = 1,3
            DUDM(I,J) = ZERO
            DTDM(I,J) = ZERO
            DT2DM(I,J) = ZERO
   70     CONTINUE
   80   CONTINUE

        DO 110 I = 1,NNO
          DER(1) = DFDX(I)
          DER(2) = DFDY(I)
          DER(3) = DFDZ(I)
          TPG = TPG + ZR(ITEMPE+I-1)*ZR(IVF+L+I-1)

          DO 100 J = 1,3
            DO 90 K = 1,3
              DUDM(J,K) = DUDM(J,K) + ZR(IDEPL+3*I-4+J)*DER(K)
              DTDM(J,K) = DTDM(J,K) + ZR(ITHET1+3*I-4+J)*DER(K)
              DT2DM(J,K) = DT2DM(J,K) + ZR(ITHET+3*I-4+J)*DER(K)
   90       CONTINUE
  100     CONTINUE
  110   CONTINUE

        CALL RCVADA(ZI(IMATE),'ELAS',TPG,3,NOMRES,VALRES,DEVRES,CODRET)
        IF (CODRET(3).NE.'OK') THEN
          VALRES(3) = ZERO
          DEVRES(3) = ZERO
        END IF

        C1 = VALRES(1)/ (UN+VALRES(2))
        C2 = (UN-VALRES(2))/ (UN-DEUX*VALRES(2))
        C3 = VALRES(2)/ (UN-DEUX*VALRES(2))
        DC2 = C2*DEUX
        DC3 = C3*DEUX
        ALPHD = ALPHA*ALPHA


        C11 = (UN+ALPHA*DTDM(2,2))* (UN+ALPHA*DTDM(3,3)) -
     &        ALPHD*DTDM(3,2)*DTDM(2,3)
        C21 = ALPHD*DTDM(3,1)*DTDM(2,3) -
     &        ALPHA*DTDM(2,1)* (UN+ALPHA*DTDM(3,3))
        C31 = ALPHD*DTDM(2,1)*DTDM(3,2) -
     &        ALPHA*DTDM(3,1)* (UN+ALPHA*DTDM(2,2))
        C12 = ALPHD*DTDM(3,2)*DTDM(1,3) -
     &        ALPHA*DTDM(1,2)* (UN+ALPHA*DTDM(3,3))
        C22 = (UN+ALPHA*DTDM(1,1))* (UN+ALPHA*DTDM(3,3)) -
     &        ALPHD*DTDM(3,1)*DTDM(1,3)
        C32 = ALPHD*DTDM(3,1)*DTDM(1,2) -
     &        ALPHA*DTDM(3,2)* (UN+ALPHA*DTDM(1,1))
        C13 = ALPHD*DTDM(1,2)*DTDM(2,3) -
     &        ALPHA*DTDM(1,3)* (UN+ALPHA*DTDM(2,2))
        C23 = ALPHD*DTDM(2,1)*DTDM(1,3) -
     &        ALPHA*DTDM(2,3)* (UN+ALPHA*DTDM(1,1))
        C33 = (UN+ALPHA*DTDM(1,1))* (UN+ALPHA*DTDM(2,2)) -
     &        ALPHD*DTDM(1,2)*DTDM(2,1)

        D11 = DT2DM(3,3)* (UN+ALPHA*DTDM(2,2)) +
     &        DT2DM(2,2)* (UN+ALPHA*DTDM(3,3)) -
     &        ALPHA*DTDM(3,2)*DT2DM(2,3) - ALPHA*DTDM(2,3)*DT2DM(3,2)
        D21 = -ALPHA*DT2DM(3,3)*DTDM(2,1) -
     &        DT2DM(2,1)* (UN+ALPHA*DTDM(3,3)) +
     &        ALPHA*DTDM(3,1)*DT2DM(2,3) + ALPHA*DTDM(2,3)*DT2DM(3,1)
        D31 = -ALPHA*DT2DM(3,1)*DTDM(2,2) -
     &        DT2DM(3,1)* (UN+ALPHA*DTDM(2,2)) +
     &        ALPHA*DTDM(2,1)*DT2DM(3,2) + ALPHA*DTDM(3,2)*DT2DM(2,1)
        D12 = -ALPHA*DT2DM(3,3)*DTDM(1,2) -
     &        DT2DM(1,2)* (UN+ALPHA*DTDM(3,3)) +
     &        ALPHA*DTDM(3,2)*DT2DM(1,3) + ALPHA*DTDM(1,3)*DT2DM(3,2)
        D22 = DT2DM(3,3)* (UN+ALPHA*DTDM(1,1)) +
     &        DT2DM(1,1)* (UN+ALPHA*DTDM(3,3)) -
     &        ALPHA*DTDM(3,1)*DT2DM(1,3) - ALPHA*DTDM(1,3)*DT2DM(3,1)
        D32 = -ALPHA*DT2DM(1,1)*DTDM(3,2) -
     &        DT2DM(3,2)* (UN+ALPHA*DTDM(1,1)) +
     &        ALPHA*DTDM(3,1)*DT2DM(1,2) + ALPHA*DTDM(1,2)*DT2DM(3,1)
        D13 = -ALPHA*DT2DM(2,2)*DTDM(1,3) -
     &        DT2DM(1,3)* (UN+ALPHA*DTDM(2,2)) +
     &        ALPHA*DTDM(1,2)*DT2DM(2,3) + ALPHA*DTDM(2,3)*DT2DM(1,2)
        D23 = -ALPHA*DT2DM(1,1)*DTDM(2,3) -
     &        DT2DM(2,3)* (UN+ALPHA*DTDM(1,1)) +
     &        ALPHA*DTDM(2,1)*DT2DM(1,3) + ALPHA*DTDM(1,3)*DT2DM(2,1)
        D33 = DT2DM(2,2)* (UN+ALPHA*DTDM(1,1)) +
     &        DT2DM(1,1)* (UN+ALPHA*DTDM(2,2)) -
     &        ALPHA*DTDM(1,2)*DT2DM(2,1) - ALPHA*DTDM(2,1)*DT2DM(1,2)

        DETFA = (UN+ALPHA*DTDM(1,1))*C11 + ALPHA*DTDM(1,2)*C21 +
     &          ALPHA*DTDM(1,3)*C31

        DDETFA = DT2DM(1,1)*C11 + DT2DM(1,2)*C21 + DT2DM(1,3)*C31 +
     &           (UN+ALPHA*DTDM(1,1))*D11 + ALPHA*DTDM(1,2)*D21 +
     &           ALPHA*DTDM(1,3)*D31

        S1 = UNDEMI* (DDETFA/ (DETFA*DETFA))
        S2 = -UN/DETFA

        GUC(1) = C11*DUDM(1,1) + C21*DUDM(1,2) + C31*DUDM(1,3)
        GUC(2) = C12*DUDM(2,1) + C22*DUDM(2,2) + C32*DUDM(2,3)
        GUC(3) = C13*DUDM(3,1) + C23*DUDM(3,2) + C33*DUDM(3,3)
        GUC(4) = C12*DUDM(1,1) + C22*DUDM(1,2) + C32*DUDM(1,3) +
     &           C11*DUDM(2,1) + C21*DUDM(2,2) + C31*DUDM(2,3)
        GUC(5) = C13*DUDM(1,1) + C23*DUDM(1,2) + C33*DUDM(1,3) +
     &           C11*DUDM(3,1) + C21*DUDM(3,2) + C31*DUDM(3,3)
        GUC(6) = C13*DUDM(2,1) + C23*DUDM(2,2) + C33*DUDM(2,3) +
     &           C12*DUDM(3,1) + C22*DUDM(3,2) + C32*DUDM(3,3)

        GUD(1) = D11*DUDM(1,1) + D21*DUDM(1,2) + D31*DUDM(1,3)
        GUD(2) = D12*DUDM(2,1) + D22*DUDM(2,2) + D32*DUDM(2,3)
        GUD(3) = D13*DUDM(3,1) + D23*DUDM(3,2) + D33*DUDM(3,3)
        GUD(4) = D12*DUDM(1,1) + D22*DUDM(1,2) + D32*DUDM(1,3) +
     &           D11*DUDM(2,1) + D21*DUDM(2,2) + D31*DUDM(2,3)
        GUD(5) = D13*DUDM(1,1) + D23*DUDM(1,2) + D33*DUDM(1,3) +
     &           D11*DUDM(3,1) + D21*DUDM(3,2) + D31*DUDM(3,3)
        GUD(6) = D13*DUDM(2,1) + D23*DUDM(2,2) + D33*DUDM(2,3) +
     &           D12*DUDM(3,1) + D22*DUDM(3,2) + D32*DUDM(3,3)

        M1 = DC2* (GUC(1)*GUC(1)+GUC(2)*GUC(2)+GUC(3)*GUC(3)) +
     &       DEUX*DC3* (GUC(1)*GUC(2)+GUC(1)*GUC(3)+GUC(2)*GUC(3)) +
     &       GUC(4)*GUC(4) + GUC(5)*GUC(5) + GUC(6)*GUC(6)

        M2 = DC2* (GUD(1)*GUC(1)+GUD(2)*GUC(2)+GUD(3)*GUC(3)) +
     &       DC3* (GUD(2)*GUC(1)+GUD(3)*GUC(1)+GUD(1)*GUC(3)+
     &       GUD(3)*GUC(2)+GUD(1)*GUC(2)+GUD(2)*GUC(3)) +
     &       GUD(4)*GUC(4) + GUD(5)*GUC(5) + GUD(6)*GUC(6)

        COEF = POIDS*C1*UNDEMI

        G = G + COEF* (M1*S1+M2*S2)

  120 CONTINUE

      ZR(IGTHET) = G

  130 CONTINUE
      CALL JEDEMA()
      END

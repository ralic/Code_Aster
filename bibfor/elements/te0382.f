      SUBROUTINE TE0382 ( OPTION , NOMTE )
      IMPLICIT   NONE
      CHARACTER*16        OPTION , NOMTE
C.......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 11/04/2002   AUTEUR CIBHHLV L.VIVAN 
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
C
C      CALCUL DU TAUX DE RESTITUTION D'ENERGIE ELEMENTAIRE
C      AVEC PROPAGATION LAGRANGIENNE
C      ELEMENTS ISOPARAMETRIQUES 3D
C
C      OPTION : 'CALC_G_LAGR'     (CHARGES REELLES)
C               'CALC_G_LAGR_F'   (CHARGES FONCTIONS)
C
C ENTREES  ---> OPTION : OPTION DE CALCUL
C          ---> NOMTE  : NOM DU TYPE ELEMENT
C
C VECTEURS DIMENSIONNES POUR  NNO = 27
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
      INTEGER        IPOIDS,IVF,IDFDE,IDFDN,IDFDK,IGEOM,IDEPL,ITREF
      INTEGER        ITEMPE,IMATE,IFORC,IFORF,ITHET,IGTHET,JIN,JVAL
      INTEGER        NNO,NDIM,NBFPG,NBPG(10),KP,NPG1,I,J,K,L
      INTEGER        ICOMPT,IALPH,ITEMPS,ICODE
C
      REAL*8         TPG,VALRES(3),DEVRES(3)
      REAL*8         C11,C12,C13,C21,C22,C23,C31,C32,C33,C1,C2,C3
      REAL*8         D11,D12,D13,D21,D22,D23,D31,D32,D33,DC2,DC3
      REAL*8         DETFA,DDETFA,COEF,X,Y,Z,G,THETX,THETY,THETZ
      REAL*8         ALPHA,ALPHD,ALPHT,ALPHQ
      REAL*8         M1,M2,M3,M4,M5,M6,S1,S2,S3,S4,S5,S6
      REAL*8         GUC(6),GUD(6),VALPAR(4),DUDM(3,3),DTDM(3,3)
      REAL*8         EPS,POIDS,ZERO,UN,DEUX,UNDEMI
      REAL*8         DER(3),DFDX(27),DFDY(27),DFDZ(27)
C                        NDIM*NNO
      REAL*8         FORCN(81)
C
      CHARACTER*8    ELREFE,NOMRES(3),CODRET(3),NOMPAR(4)
      CHARACTER*24   CHVAL,CHCTE
C
      LOGICAL        FONC
C.......................................................................
      DATA  ZERO, UN, DEUX, EPS /0.0D0, 1.0D0, 2.0D0, 1.0D-15/
C.......................................................................
C

      CALL JEMARQ()
      CALL ELREF1(ELREFE)


      CHCTE = '&INEL.'//ELREFE//'.CARACTE'
      CALL JEVETE(CHCTE,' ',JIN)
      NDIM  = ZI(JIN+1-1)
      NNO   = ZI(JIN+2-1)
      NBFPG = ZI(JIN+3-1)
C
      DO 100 I = 1,NBFPG
         NBPG(I) = ZI(JIN+3-1+I)
100   CONTINUE
      NPG1 = NBPG(1)
C
      CHVAL = '&INEL.'//ELREFE//'.FFORMES'
      CALL JEVETE(CHVAL,' ',JVAL)
      IPOIDS = JVAL + (NDIM+1)*NNO*NNO
      IVF    = IPOIDS + NPG1
      IDFDE  = IVF    + NPG1*NNO
      IDFDN  = IDFDE  + 1
      IDFDK  = IDFDN  + 1
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PDEPLAR','L',IDEPL)
      CALL JEVECH('PTHETAR','L',ITHET)
      CALL JEVECH('PALPHAR','L',IALPH)
      CALL JEVECH('PTEREF' ,'L',ITREF)
      CALL JEVECH('PTEMPER','L',ITEMPE)
      IF (OPTION.EQ.'CALC_G_LAGR_F') THEN
        FONC = .TRUE.
        CALL JEVECH('PFFVOLU','L',IFORF)
        CALL JEVECH('PTEMPSR','L',ITEMPS)
        NOMPAR(1) = 'X'
        NOMPAR(2) = 'Y'
        NOMPAR(3) = 'Z'
        NOMPAR(4) = 'INST'
        VALPAR(4) = ZR(ITEMPS)
      ELSE
        FONC =.FALSE.
        CALL JEVECH('PFRVOLU','L',IFORC)
        DO 2 I = 1 , NNO*NDIM
           FORCN(I) = ZR(IFORC+I-1)
 2      CONTINUE
      ENDIF
      CALL JEVECH('PMATERC','L',IMATE)
      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      NOMRES(3) = 'ALPHA'
      CALL JEVECH('PGTHETA','E',IGTHET)
C
      ALPHA= ZR(IALPH)
C
C - PAS DE CALCUL DE G POUR LES ELEMENTS OU LA VALEUR DE THETA EST NULLE
C
      ICOMPT = 0
      DO 250 I=1,NNO
         THETX = ZR(ITHET + 3*(I - 1) + 1 - 1)
         THETY = ZR(ITHET + 3*(I - 1) + 2 - 1)
         THETZ = ZR(ITHET + 3*(I - 1) + 3 - 1)
         IF((ABS(THETX).LT.EPS).AND.(ABS(THETY).LT.EPS).AND.
     &      (ABS(THETZ).LT.EPS)) THEN
             ICOMPT = ICOMPT + 1
         ENDIF
250   CONTINUE
      IF(ICOMPT.EQ.NNO) GOTO 9999
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
C ======================================================================
C
      UNDEMI = 0.5D0
      G = ZERO
C
      DO 800 KP=1,NPG1
        L    = (KP-1)*NNO
        K    = L*3
        TPG  = ZERO
        CALL DFDM3D (NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDN+K),
     &               ZR(IDFDK+K),ZR(IGEOM),DFDX,DFDY,DFDZ,POIDS)
C
C  CALCUL DES GRADIENTS DE U, THETA
C  --------------------------------
C
        DO 350 I=1,3
           DO 350 J=1,3
                 DUDM(I,J) = ZERO
                 DTDM(I,J) = ZERO
350     CONTINUE
C
C     CALCUL DES DERIVEES DE U ET THETA
C
        DO 400 I=1,NNO
           DER(1) = DFDX(I)
           DER(2) = DFDY(I)
           DER(3) = DFDZ(I)
           TPG    = TPG + ZR(ITEMPE+I-1)*ZR(IVF+L+I-1)
C
           DO 450 J=1,3
              DO 450 K=1,3
                DUDM(J,K) = DUDM(J,K) + ZR(IDEPL+3*I-4+J)*DER(K)
                DTDM(J,K) = DTDM(J,K) + ZR(ITHET+3*I-4+J)*DER(K)
  450      CONTINUE
400     CONTINUE
C
        CALL RCVADA(ZI(IMATE),'ELAS',TPG,3,NOMRES,VALRES,DEVRES,
     +                                     CODRET)
C
           C1 = VALRES(1)/(UN + VALRES(2))
           C2 = (UN - VALRES(2))/(UN - DEUX*VALRES(2))
           C3 = VALRES(2)/(UN - DEUX*VALRES(2))
           DC2 = C2 * DEUX
           DC3 = C3 * DEUX
           ALPHD= ALPHA*ALPHA
           ALPHT= ALPHD*ALPHA
           ALPHQ= ALPHT*ALPHA
C
C    DETERMINANT DE FALPHA
C
        X = DTDM(1,1) + DTDM(2,2) + DTDM(3,3)
C
        Y = DTDM(2,2)*DTDM(3,3) - DTDM(3,2)*DTDM(2,3)
     &    + DTDM(1,1)*DTDM(2,2) - DTDM(1,2)*DTDM(2,1)
     &    + DTDM(1,1)*DTDM(3,3) - DTDM(1,3)*DTDM(3,1)
C
        Z = DTDM(1,1)*DTDM(2,2)*DTDM(3,3) -
     &      DTDM(1,1)*DTDM(3,2)*DTDM(2,3) +
     &      DTDM(1,2)*DTDM(3,1)*DTDM(2,3) -
     &      DTDM(1,2)*DTDM(2,1)*DTDM(3,3) +
     &      DTDM(1,3)*DTDM(2,1)*DTDM(3,2) -
     &      DTDM(1,3)*DTDM(3,1)*DTDM(2,2)
C
       DETFA = UN + ALPHA*X + ALPHD*Y + ALPHT*Z
C
C    DERIVEE DU DETERMINANT DE FALPHA
C
       DDETFA = X + DEUX*ALPHA*Y + 3.D0*ALPHD*Z
C
C    COEFFICIENTS DE C
       C11 = DTDM(2,2)+DTDM(3,3)
       C12 = -DTDM(1,2)
       C13 = -DTDM(1,3)
       C21 = -DTDM(2,1)
       C22 = DTDM(1,1)+DTDM(3,3)
       C23 = -DTDM(2,3)
       C31 = -DTDM(3,1)
       C32= -DTDM(3,2)
       C33 = DTDM(1,1)+DTDM(2,2)
C
C    COEFFICIENTS DE D
       D11 = DTDM(2,2)*DTDM(3,3) - DTDM(3,2)*DTDM(2,3)
       D12 = DTDM(1,3)*DTDM(3,2) - DTDM(1,2)*DTDM(3,3)
       D13 = DTDM(1,2)*DTDM(2,3) - DTDM(2,2)*DTDM(1,3)
       D21 = DTDM(3,1)*DTDM(2,3) - DTDM(2,1)*DTDM(3,3)
       D22 = DTDM(1,1)*DTDM(3,3) - DTDM(3,1)*DTDM(1,3)
       D23 = DTDM(2,1)*DTDM(1,3) - DTDM(2,3)*DTDM(1,1)
       D31 = DTDM(2,1)*DTDM(3,2) - DTDM(3,1)*DTDM(2,2)
       D32 = DTDM(3,1)*DTDM(1,2) - DTDM(1,1)*DTDM(3,2)
       D33 = DTDM(1,1)*DTDM(2,2) - DTDM(2,1)*DTDM(1,2)
C
C    COEFFICIENTS DES INTEGRALES
       S1 = UNDEMI*( DDETFA/(DETFA*DETFA) )
       S2 = ( (ALPHA*DDETFA - DETFA)/(DETFA*DETFA) )
       S3 = ( (ALPHD*DDETFA - DEUX*ALPHA*DETFA)/(DETFA*DETFA) )
       S4 = UNDEMI *
     &      ( (ALPHD*DDETFA - DEUX*ALPHA*DETFA)/(DETFA*DETFA) )
       S5 = ( (ALPHT*DDETFA - 3.D0*ALPHD*DETFA)/(DETFA*DETFA) )
       S6 = UNDEMI *
     &      ( (ALPHQ*DDETFA - 4.D0*ALPHT*DETFA)/(DETFA*DETFA) )
C
C GRAD(U).C
       GUC(1) = C11*DUDM(1,1) + C21*DUDM(1,2) + C31*DUDM(1,3)
       GUC(2) = C12*DUDM(2,1) + C22*DUDM(2,2) + C32*DUDM(2,3)
       GUC(3) = C13*DUDM(3,1) + C23*DUDM(3,2) + C33*DUDM(3,3)
       GUC(4) = C12*DUDM(1,1) + C22*DUDM(1,2) + C32*DUDM(1,3)
     &        + C11*DUDM(2,1) + C21*DUDM(2,2) + C31*DUDM(2,3)
       GUC(5) = C13*DUDM(1,1) + C23*DUDM(1,2) + C33*DUDM(1,3)
     &        + C11*DUDM(3,1) + C21*DUDM(3,2) + C31*DUDM(3,3)
       GUC(6) = C13*DUDM(2,1) + C23*DUDM(2,2) + C33*DUDM(2,3)
     &        + C12*DUDM(3,1) + C22*DUDM(3,2) + C32*DUDM(3,3)
C
C GRAD(U).D
       GUD(1) = D11*DUDM(1,1) + D21*DUDM(1,2) + D31*DUDM(1,3)
       GUD(2) = D12*DUDM(2,1) + D22*DUDM(2,2) + D32*DUDM(2,3)
       GUD(3) = D13*DUDM(3,1) + D23*DUDM(3,2) + D33*DUDM(3,3)
       GUD(4) = D12*DUDM(1,1) + D22*DUDM(1,2) + D32*DUDM(1,3)
     &        + D11*DUDM(2,1) + D21*DUDM(2,2) + D31*DUDM(2,3)
       GUD(5) = D13*DUDM(1,1) + D23*DUDM(1,2) + D33*DUDM(1,3)
     &        + D11*DUDM(3,1) + D21*DUDM(3,2) + D31*DUDM(3,3)
       GUD(6) = D13*DUDM(2,1) + D23*DUDM(2,2) + D33*DUDM(2,3)
     &        + D12*DUDM(3,1) + D22*DUDM(3,2) + D32*DUDM(3,3)
C
C LES INTEGRALES
       M1 = DC2*( DUDM(1,1)*DUDM(1,1) + DUDM(2,2)*DUDM(2,2)
     &          + DUDM(3,3)*DUDM(3,3) )
     &    + DEUX*DC3*( DUDM(1,1)*DUDM(2,2) + DUDM(1,1)*DUDM(3,3)
     &             + DUDM(2,2)*DUDM(3,3) )
     &    + (DUDM(1,2)+DUDM(2,1))*(DUDM(1,2)+DUDM(2,1))
     &    + (DUDM(1,3)+DUDM(3,1))*(DUDM(1,3)+DUDM(3,1))
     &    + (DUDM(2,3)+DUDM(3,2))*(DUDM(2,3)+DUDM(3,2))
C
       M2 = DC2*( GUC(1)*DUDM(1,1) + GUC(2)*DUDM(2,2)
     &          + GUC(3)*DUDM(3,3) )
     &    + DC3*( GUC(2)*DUDM(1,1) + GUC(3)*DUDM(1,1)
     &          + GUC(1)*DUDM(2,2) + GUC(3)*DUDM(2,2)
     &          + GUC(1)*DUDM(3,3) + GUC(2)*DUDM(3,3) )
     &    + GUC(4)*(DUDM(1,2)+DUDM(2,1))
     &    + GUC(5)*(DUDM(1,3)+DUDM(3,1))
     &    + GUC(6)*(DUDM(2,3)+DUDM(3,2))
C
       M3 = DC2*( GUD(1)*DUDM(1,1) + GUD(2)*DUDM(2,2)
     &          + GUD(3)*DUDM(3,3) )
     &    + DC3*( GUD(2)*DUDM(1,1) + GUD(3)*DUDM(1,1)
     &          + GUD(1)*DUDM(2,2) + GUD(3)*DUDM(2,2)
     &          + GUD(1)*DUDM(3,3) + GUD(2)*DUDM(3,3) )
     &    + GUD(4)*(DUDM(1,2)+DUDM(2,1))
     &    + GUD(5)*(DUDM(1,3)+DUDM(3,1))
     &    + GUD(6)*(DUDM(2,3)+DUDM(3,2))
C
       M4 = DC2*( GUC(1)*GUC(1) + GUC(2)*GUC(2)
     &          + GUC(3)*GUC(3) )
     &    + DEUX*DC3*( GUC(1)*GUC(2) + GUC(1)*GUC(3)
     &             + GUC(2)*GUC(3) )
     &    + GUC(4)*GUC(4)
     &    + GUC(5)*GUC(5)
     &    + GUC(6)*GUC(6)
C
       M5 = DC2*( GUD(1)*GUC(1) + GUD(2)*GUC(2)
     &          + GUD(3)*GUC(3) )
     &    + DC3*( GUD(2)*GUC(1) + GUD(3)*GUC(1)
     &          + GUD(1)*GUC(3) + GUD(3)*GUC(2)
     &          + GUD(1)*GUC(2) + GUD(2)*GUC(3) )
     &    + GUD(4)*GUC(4)
     &    + GUD(5)*GUC(5)
     &    + GUD(6)*GUC(6)
C
       M6 = DC2*( GUD(1)*GUD(1) + GUD(2)*GUD(2)
     &          + GUD(3)*GUD(3) )
     &    + DEUX*DC3*( GUD(1)*GUD(2) + GUD(1)*GUD(3)
     &             + GUD(2)*GUD(3) )
     &    + GUD(4)*GUD(4)
     &    + GUD(5)*GUD(5)
     &    + GUD(6)*GUD(6)
C
           COEF = POIDS * C1 * UNDEMI
C
       G = G + COEF*(M1*S1 + M2*S2 + M3*S3 + M4*S4 + M5*S5
     &              + M6*S6 )
C
800     CONTINUE
C
      ZR(IGTHET) = G
C
9999  CONTINUE
      CALL JEDEMA()
      END

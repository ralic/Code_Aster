      SUBROUTINE TE0288 ( OPTION , NOMTE )
      IMPLICIT   NONE
      CHARACTER*16        OPTION , NOMTE
C.......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 04/04/2002   AUTEUR VABHHTS J.PELLET 
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
C      ELEMENTS ISOPARAMETRIQUES 2D
C
C      OPTION : 'CALC_G_LAGR'     (CHARGES REELLES)
C               'CALC_G_LAGR_F'   (CHARGES FONCTIONS)
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
      INTEGER        IPOIDS,IVF,IDFDE,IDFDK,IGEOM,JIN,NDIM,JVAL
      INTEGER        ITHET,IALPH,IFORF,ITEMPS,MATER,NNO,KP,NPG1,IGTHET
      INTEGER        IDEPL,ITREF,ITEMPE,IMATE,IFORC,ICODE,I,J,K
C
      REAL*8         G,TPG,C1,C2,C3,R,ALPHA,D,ADET,A1,A2,A3
      REAL*8         CP1,CP2,CP3,DP1,DP2,DP3,XG,S1,S2,S3,S4,S5,S6
      REAL*8         DGRT,DIVT,EPS,POIDS,M1,M2,M3,M4,M5,M6
      REAL*8         VALRES(3),DUDM(7),DTDM(7),TGD(2)
      REAL*8         DFDM(7),DFDX(9),DFDY(9),GUGT(5)
      REAL*8         GUC(4),GUC1(4),VALPAR(3)
C                       NDIM*NNO
      REAL*8         FORCN(18)
C
      CHARACTER*2    CODRET(3)
      CHARACTER*8    NOMRES(3),ELREFE
      CHARACTER*24   CHVAL, CHCTE, NOMPAR(3)
C
      LOGICAL        FONC
C.......................................................................
C
      CALL ELREF1(ELREFE)
      CALL JEMARQ()


      EPS = 1.D-10
      CHCTE = '&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CHCTE,' ',JIN)
      NDIM  = 2
      NNO   = ZI(JIN)
      NPG1  = ZI(JIN+2)
C
      CHVAL = '&INEL.'//ELREFE//'.FF'
      CALL JEVETE(CHVAL,' ',JVAL)
C
      IPOIDS = JVAL
      IVF    = IPOIDS + NPG1
      IDFDE  = IVF    + NPG1*NNO
      IDFDK  = IDFDE  + NPG1*NNO
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PDEPLAR','L',IDEPL)
      CALL JEVECH('PTHETAR','L',ITHET)
      CALL JEVECH('PALPHAR','L',IALPH)
      CALL JEVECH('PTEREF','L',ITREF)
      CALL JEVECH('PTEMPER','L',ITEMPE)
      IF (OPTION.EQ.'CALC_G_LAGR_F') THEN
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
        DO 2 I = 1 , NDIM*NNO
           FORCN(I) = ZR(IFORC+I-1)
 2      CONTINUE
      ENDIF
      CALL JEVECH('PGTHETA','E',IGTHET)
      CALL JEVECH('PMATERC','L',IMATE)
      MATER = ZI(IMATE)
      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      NOMRES(3) = 'ALPHA'
C
C  INITIALISATION
C
      ALPHA  = ZR(IALPH)
      G      = 0.D0
C
C PAS DE CALCUL DE G POUR LES ELEMENTS HORS DU SUPPORT DE THETA
C
      DO 250 KP=1,NPG1
         K  = (KP-1)*NNO
         XG = 0.D0
      DO 220 I=1,NNO
         XG = XG + ZR(IGEOM+2*(I-1))   * ZR(IVF+K+I-1)
220   CONTINUE
250   CONTINUE
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
      DO 800 KP=1,NPG1
C
        K    = (KP-1)*NNO
        TPG  = 0.D0
        R    = 0.D0
        XG   = 0.D0
        CALL DFDM2D (NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     &               ZR(IGEOM),DFDX,DFDY,POIDS)
C
        DO 300 I=1,NNO
          R    = R   + ZR(IGEOM+2*I-2) * ZR(IVF+K+I-1)
          XG   = XG  + ZR(IGEOM+2*(I-1)) * ZR(IVF+K+I-1)
          TPG  = TPG + ZR(ITEMPE+I-1) * ZR(IVF+K+I-1)
300     CONTINUE
C
        CALL RCVALA (MATER,'ELAS',1,'TEMP',TPG,3,NOMRES,
     &                VALRES, CODRET, 'FM' )
C
        CP1 = VALRES(1)/(1.D0 -VALRES(2)*VALRES(2))
        CP2 = VALRES(2)*CP1
        CP3 = ((1.D0 -VALRES(2))/2.D0)*CP1
        DP1 = VALRES(1)*(1.D0-VALRES(2))
     &        /((1.D0+VALRES(2))*(1.D0-2.D0*VALRES(2)))
        DP2 = (VALRES(2)/(1.D0-VALRES(2)))*DP1
        DP3 = ((1.D0-2.D0*VALRES(2))/(2.D0*(1.D0-VALRES(2))))*DP1
C
         IF ( NOMTE(3:4) .EQ. 'DP' ) THEN
           C1 = DP1
           C2 = DP2
           C3 = DP3
         ELSE
           C1 = CP1
           C2 = CP2
           C3 = CP3
         ENDIF
C
        TPG = TPG - ZR(ITREF)
C
C DEFINITIONS DES COEFFICIENTS
C
        CALL GDFONC ( DFDX,DFDY,KP,ZR(IVF),ZR(IDEPL),ZR(ITHET),FORCN,
     &                ZR(ITEMPE),NNO,DUDM,DTDM,DFDM,TGD)
C
C CALCUL DE G
C -----------
C
C*** D'ABORD LE CAS AXI
C
        IF (NOMTE(3:4).EQ.'AX') THEN
           POIDS = POIDS*R
           DTDM(4) = DTDM(4)/R
           DUDM(4) = DUDM(4)/R
           C1 = VALRES(1)/(1.D0 + VALRES(2))
           C2 = (1.D0 - VALRES(2))/(1.D0 - 2.D0*VALRES(2))
           C3 = VALRES(2)/(1.D0 - 2.D0*VALRES(2))
           A1 = DTDM(1)+DTDM(2)+DTDM(4)
           A2 = DTDM(4)*(DTDM(1)+DTDM(2)) + DTDM(1)*DTDM(2)
     &          - DTDM(3)*DTDM(5)
           A3 = DTDM(4)*(DTDM(1)*DTDM(2)-DTDM(3)*DTDM(5))
C
C
           ADET = 1.D0+ ALPHA*A1 + ALPHA*ALPHA*A2 + (ALPHA**3)*A3
C
           GUC(1) = DUDM(1)*(DTDM(2)+DTDM(4)) -
     &               DUDM(3)*DTDM(5)
C
           GUC(2) = DUDM(4)*(DTDM(2)+DTDM(1))
C
           GUC(3) = DUDM(2)*(DTDM(4)+DTDM(1)) -
     &               DUDM(5)*DTDM(3)
C
           GUC(4) = DUDM(3)*(DTDM(4)+DTDM(1)) +
     &               DUDM(5)*(DTDM(4)+DTDM(2)) -
     &               DUDM(1)*DTDM(3) - DUDM(2)*DTDM(5)
C
C
           GUC1(1) = DTDM(4)*( DUDM(1)*DTDM(2) - DUDM(3)*DTDM(5) )
C
           GUC1(2) = DUDM(4)*( DTDM(1)*DTDM(2) - DTDM(3)*DTDM(5) )
C
           GUC1(3) = DTDM(4)*( DUDM(2)*DTDM(1) - DUDM(5)*DTDM(3) )
C
           GUC1(4) = DTDM(4)*( DUDM(3)*DTDM(1) + DUDM(5)*DTDM(2)
     &                          - DUDM(1)*DTDM(3) - DUDM(2)*DTDM(5) )
C
           M1 = (A1 + 2.D0*ALPHA*A2 + 3.D0*ALPHA*ALPHA*A3)/
     &          (2.D0)
           M2 = (2.D0*ALPHA + ALPHA*ALPHA*A1 - (ALPHA**4)*A3) /
     &           (-2.D0)
           M3 = (4.D0*(ALPHA**3) + 3.D0*(ALPHA**4)*A1 + 2.D0*(ALPHA**5)*
     &           A2 + (ALPHA**6)*A3)/(-2.D0)
           M4 = (1 - ALPHA*ALPHA*A2 - 2.D0*(ALPHA**3)*A3)/
     &          (-1.D0)
           M5 = 2.D0*M2
C
           M6 = (3.D0*ALPHA*ALPHA + 2.D0*(ALPHA**3)*A1 + (ALPHA**4)*A2)/
     &          (-1.D0)
C
C
           S1 = C1*C2*(DUDM(1)*DUDM(1))+ 2.D0*C1*C3*DUDM(2)*DUDM(1)
     &        + 2.D0*C1*C3*DUDM(4)*DUDM(1) + 2.D0*C1*C3*DUDM(4)*
     &          DUDM(2) + C1*C2*DUDM(4)*DUDM(4) +
     &        C1*C2*DUDM(2)*DUDM(2) + C1/2.D0*(DUDM(3)+DUDM(5))*
     &                                      (DUDM(3)+DUDM(5))
C
           S2 = C1*C2*( (GUC(1)*GUC(1)) + (GUC(2)*GUC(2)) +
     &          (GUC(3)*GUC(3)) ) + C1/2.D0*(GUC(4)*GUC(4))
     &         + 2.D0*C1*C3*( GUC(1)*GUC(2) + GUC(2)*GUC(3)
     &                    + GUC(1)*GUC(3) )
C
           S3 = C1*C2*( (GUC1(1)*GUC1(1)) + (GUC1(2)*GUC1(2)) +
     &          (GUC1(3)*GUC1(3)) ) + C1/2.D0*(GUC1(4)*GUC1(4))
     &         + 2.D0*C1*C3*( GUC1(1)*GUC1(2) + GUC1(2)*GUC1(3)
     &                    + GUC1(1)*GUC1(3) )
C
           S4 = C1*GUC(1)*( C2*DUDM(1)+C3*DUDM(4)+C3*DUDM(2) )
     &        + C1*GUC(2)*( C3*DUDM(1)+C2*DUDM(4)+C3*DUDM(2) )
     &        + C1*GUC(3)*( C3*DUDM(1)+C3*DUDM(4)+C2*DUDM(2) )
     &        + C1/2.D0*(DUDM(5)+DUDM(3))*GUC(4)
C
           S5 = C1*GUC1(1)*( C2*DUDM(1)+C3*DUDM(4)+C3*DUDM(2) )
     &        + C1*GUC1(2)*( C3*DUDM(1)+C2*DUDM(4)+C3*DUDM(2) )
     &        + C1*GUC1(3)*( C3*DUDM(1)+C3*DUDM(4)+C2*DUDM(2) )
     &        + C1/2.D0*GUC1(4)*(DUDM(5)+DUDM(3))
C
           S6 = C1*C2*( (GUC(1)*GUC1(1)) + (GUC(2)*GUC1(2)) +
     &          (GUC(3)*GUC1(3)) ) + C1*C3*
     &          ( GUC(1)*GUC1(2) + GUC(2)*GUC1(3) +
     &            GUC(1)*GUC1(3) + GUC(2)*GUC1(1) +
     &            GUC(3)*GUC1(1) + GUC(3)*GUC1(2) )
     &          + C1/2.D0*GUC(4)*GUC1(4)
C
        G  = G + POIDS  * ( M1*S1 + M2*S2 + M3*S3 + M4*S4 +
     &                     M5*S5 + M6*S6 ) / (ADET*ADET)
C
C** SI ON N'EST PAS EN AXI ALORS
C
        ELSE
        DIVT    = DTDM(1) + DTDM(2)
        DGRT    = DTDM(1) * DTDM(2) - DTDM(3) * DTDM(5)
        IF (ABS(DGRT).LT.EPS) THEN
           DGRT=0.D0
        ENDIF
        GUGT(1) = DUDM(1) * DTDM(1) + DUDM(3) * DTDM(5)
        GUGT(2) = DUDM(5) * DTDM(3) + DUDM(2) * DTDM(2)
        GUGT(3) = DUDM(1) * DTDM(3) + DUDM(3) * DTDM(2)
        GUGT(5) = DUDM(5) * DTDM(1) + DUDM(2) * DTDM(5)
        S1 = C1*(DUDM(1)*DUDM(1)+DUDM(2)*DUDM(2))
     &      +C2*(DUDM(2)*DUDM(1)+DUDM(1)*DUDM(2))
     &      +C3*(DUDM(3)+DUDM(5))*(DUDM(3)+DUDM(5))
        S2 = C1*(DUDM(1)*GUGT(1)+DUDM(2)*GUGT(2))
     &      +C2*(DUDM(2)*GUGT(1)+DUDM(1)*GUGT(2))
     &      +C3*(DUDM(3)+DUDM(5))*(GUGT(3)+GUGT(5))
        S3 = C1*(GUGT(1)*GUGT(1)+GUGT(2)*GUGT(2))
     &      +C2*(GUGT(2)*GUGT(1)+GUGT(1)*GUGT(2))
     &      +C3*(GUGT(3)+GUGT(5))*(GUGT(3)+GUGT(5))
        M1 =(  ALPHA*ALPHA*(2*DGRT*DIVT-DIVT**3)
     &       + 2*ALPHA*(DGRT-DIVT*DIVT)
     &       - DIVT ) / 2.D0
        M2 =   ALPHA*ALPHA*(DIVT*DIVT-DGRT)
     &       + 2*ALPHA*DIVT + 1
        M3 =(  ALPHA*ALPHA*DIVT + 2*ALPHA ) / (-2.D0)
        D  = 1+ALPHA*DIVT+ALPHA*ALPHA*DGRT
C
        G  = G + POIDS * ( M1*S1 + M2*S2 + M3*S3 ) / ( D*D )
C
        ENDIF
800   CONTINUE
C
      ZR(IGTHET) = G
C
      CALL JEDEMA()
      END

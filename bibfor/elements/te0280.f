      SUBROUTINE TE0280 ( OPTION , NOMTE )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      CHARACTER*16        OPTION , NOMTE
C.......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 28/08/2012   AUTEUR TRAN V-X.TRAN 
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
C  CALCUL DU TAUX DE RESTITUTION D'ENERGIE ELEMENTAIRE
C  BORDS ELEMENTS ISOPARAMETRIQUES 3D AVEC PRESSION
C
C  OPTION : 'CALC_G'   (CHARGES REELLES)
C           'CALC_G_F' (CHARGES FONCTIONS)
C
C ENTREES  ---> OPTION : OPTION DE CALCUL
C          ---> NOMTE  : NOM DU TYPE ELEMENT
C
C VECTEURS DIMENSIONNES POUR  NNO = 9 , NPG = 9
C.......................................................................
C
      INTEGER       NDIM,NNO,NPG1,COMPT,IFORF
      INTEGER       IPOIDS,IVF,IDFDE,I,J,K,KP,IFORC
      INTEGER       IDEPL,IPRES,ITHET,IGTHET,IGEOM,IPREF,ITEMPS,ICODE
      INTEGER       NNOS,JGANO
C
      REAL*8        A1(3),A2(3),A3(3),I1(3),I2(3),EPSI,DFDX(9),DFDY(9)
      REAL*8        COOR(18),DEPL(3),VALPAR(4)
      REAL*8        A1NORM,A3NORM,I2NORM,DIVT,TCLA,THETX,THETY,THETZ
      REAL*8        DTH1D1,DTH2D2,POIDS,TH1,TH2,TSOM,TSURF,TSURP
      REAL*8        FORC,DFORD1(3),DFORD2(3),DFOR(3),COORG(3)
C                                         NNO       3*NNO
      REAL*8        PRESG, FORCG(3), PRESN(9), FORCN(27)
C
      CHARACTER*8   NOMPAR(4)
C
      LOGICAL       FONC
C.......................................................................
C
      CALL JEMARQ()
C
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDE,JGANO)
      CALL JEVECH ( 'PTHETAR', 'L', ITHET )
      TCLA   = 0.D0
      TSURF   = 0.D0
      TSURP   = 0.D0
      CALL JEVECH('PGTHETA','E',IGTHET)
C
C - PAS DE CALCUL DE G POUR LES ELEMENTS OU LA VALEUR DE THETA EST NULLE
C
      COMPT = 0
      EPSI  = 1.D-10
      DO 20 I=1,NNO
        THETX = ZR(ITHET + 3*(I - 1) + 1 - 1)
        THETY = ZR(ITHET + 3*(I - 1) + 2 - 1)
        THETZ = ZR(ITHET + 3*(I - 1) + 3 - 1)
        IF((ABS(THETX).LT.EPSI).AND.(ABS(THETY).LT.EPSI).AND.
     &     (ABS(THETZ).LT.EPSI)) THEN
          COMPT = COMPT + 1
        ENDIF
20    CONTINUE
      IF ( COMPT .EQ. NNO )  GOTO 9999
C
C RECUPERATION DES CHAMPS LOCAUX
C
      CALL JEVECH ( 'PGEOMER', 'L', IGEOM )
      CALL JEVECH ( 'PDEPLAR', 'L', IDEPL )
      IF ((OPTION.EQ.'CALC_G_F').OR.(OPTION.EQ.'CALC_G_GLOB_F')) THEN
        FONC = .TRUE.
        CALL JEVECH ( 'PFF2D3D', 'L', IFORF  )
        CALL JEVECH ( 'PPRESSF', 'L', IPREF  )
        CALL JEVECH ( 'PTEMPSR', 'L', ITEMPS )
        NOMPAR(1) = 'X'
        NOMPAR(2) = 'Y'
        NOMPAR(3) = 'Z'
        NOMPAR(4) = 'INST'
        VALPAR(4) = ZR(ITEMPS)
      ELSE
        FONC =.FALSE.
        CALL JEVECH ( 'PFR2D3D', 'L', IFORC )
        CALL JEVECH ( 'PPRESSR', 'L', IPRES )
      ENDIF
C
C - SI CHARGE FONCTION RECUPERATION DES VALEURS AUX PG ET NOEUDS
C
      IF ( FONC ) THEN
         DO 70 I = 1,NNO
            DO 80 J = 1,3
               VALPAR(J) = ZR(IGEOM+3*(I-1)+J-1)
80          CONTINUE
            CALL FOINTE ('FM',ZK8(IPREF),4,NOMPAR,VALPAR,PRESN(I),ICODE)
            DO 75 J=1,3
               CALL FOINTE ('FM', ZK8(IFORF+J-1), 4,NOMPAR,VALPAR,
     &                                       FORCN(3*(I-1)+J),ICODE)
75          CONTINUE
70       CONTINUE
      ENDIF
C
C CALCUL DU REPERE LOCAL ( A1, A2, A3)
C
      DO 130 J=1,3
         A1(J) =  ZR(IGEOM+3*(2-1)+J-1)- ZR(IGEOM+3*(1-1)+J-1)
         A2(J) =  ZR(IGEOM+3*(3-1)+J-1)- ZR(IGEOM+3*(1-1)+J-1)
130   CONTINUE
C
      A3(1) = A1(2)*A2(3)- A1(3)*A2(2)
      A3(2) = A1(3)*A2(1)- A1(1)*A2(3)
      A3(3) = A1(1)*A2(2)- A1(2)*A2(1)
C
C CALCUL DU REPERE LOCAL ORTHONORME ( I1, I2, A3)
C
      I2(1) = A3(2)*A1(3)-A3(3)*A1(2)
      I2(2) = A3(3)*A1(1)-A3(1)*A1(3)
      I2(3) = A3(1)*A1(2)-A3(2)*A1(1)
C
      A1NORM = SQRT(A1(1)*A1(1)+A1(2)*A1(2)+A1(3)*A1(3))
      I2NORM = SQRT(I2(1)*I2(1)+I2(2)*I2(2)+I2(3)*I2(3))
      A3NORM = SQRT(A3(1)*A3(1)+A3(2)*A3(2)+A3(3)*A3(3))
      DO 150 I=1,3
         I1(I) = A1(I) / A1NORM
         I2(I) = I2(I) / I2NORM
         A3(I) = A3(I) / A3NORM
150   CONTINUE
C
      DO 1400 I=1,NNO
         COOR(2*I-1) = 0.D0
         COOR(2*I  ) = 0.D0
         DO 1410 J=1,3
            COOR(2*I-1)= COOR(2*I-1)+( ZR(IGEOM+3*(I-1)+J-1)-
     &                                 ZR(IGEOM+J-1))*I1(J)
            COOR(2*I  )= COOR(2*I  )+( ZR(IGEOM+3*(I-1)+J-1)-
     &                                 ZR(IGEOM+J-1))*I2(J)
1410     CONTINUE
1400  CONTINUE
C
C --- BOUCLE SUR LES POINTS DE GAUSS
C
      DO 800 KP = 1 , NPG1
         K = (KP-1)*NNO
C
         DO 810 J=1,3
            DEPL(J)   = 0.D0
            DFORD1(J) = 0.D0
            DFORD2(J) = 0.D0
            DFOR(J)   = 0.D0
            COORG(J)  = 0.D0
810      CONTINUE
         TH1 = 0.D0
         TH2 = 0.D0
         DTH1D1 = 0.D0
         DTH2D2 = 0.D0
C
         DO 820 I=1,NNO
            DO 830 J=1,3
               COORG(J) = COORG(J)+ZR(IVF+K+I-1)*ZR(IGEOM+3*(I-1)+J-1)
830         CONTINUE
820      CONTINUE
C
         CALL DFDM2D ( NNO, KP,IPOIDS, IDFDE, COOR,
     &                 DFDX, DFDY, POIDS )
C
         IF ( FONC ) THEN
            DO 60 J = 1 , 3
               VALPAR(J) = COORG(J)
60          CONTINUE
            CALL FOINTE ('FM',ZK8(IPREF), 4,NOMPAR,VALPAR, PRESG, ICODE)
            DO 65 J = 1 , 3
               CALL FOINTE ('FM', ZK8(IFORF+J-1), 4, NOMPAR, VALPAR,
     &                                          FORCG(J),ICODE)
65          CONTINUE
C
            DO 400 I = 1 , NNO
               DO 410 J = 1 , 3
                  DFORD1(J) = DFORD1(J) +
     &                   ( FORCN(3*(I-1)+J) - PRESN(I)*A3(J) ) * DFDX(I)
                  DFORD2(J) = DFORD2(J) +
     &                   ( FORCN(3*(I-1)+J) - PRESN(I)*A3(J) ) * DFDY(I)
410            CONTINUE
400         CONTINUE
         ELSE
            PRESG = 0.D0
            FORCG(1) = 0.D0
            FORCG(2) = 0.D0
            FORCG(3) = 0.D0
            DO 4 I = 1 , NNO
               PRESG = PRESG + ZR(IPRES+I-1)*ZR(IVF+K+I-1)
               DO 6 J = 1 , 3
                  FORCG(J)=FORCG(J)+ZR(IFORC+3*(I-1)+J-1)*ZR(IVF+K+I-1)
 6             CONTINUE
 4          CONTINUE
         ENDIF
C
         DO 300 I=1,NNO
            DO 310 J=1,3
               DEPL(J) = DEPL(J)+ ZR(IVF+K+I-1)*ZR(IDEPL+3*(I-1)+J-1)
               TH1  = TH1 + ZR(IVF+K+I-1)*ZR(ITHET+3*(I-1)+J-1)*I1(J)
               TH2  = TH2 + ZR(IVF+K+I-1)*ZR(ITHET+3*(I-1)+J-1)*I2(J)
               DTH1D1= DTH1D1+ ZR(ITHET+3*(I-1)+J-1)*I1(J)*DFDX(I)
               DTH2D2= DTH2D2+ ZR(ITHET+3*(I-1)+J-1)*I2(J)*DFDY(I)
310         CONTINUE
300      CONTINUE
C
         DO 320 J=1,3
            DFOR(J) = DFOR(J) + DFORD1(J)*TH1+DFORD2(J)*TH2
320      CONTINUE
C
         DIVT = DTH1D1+DTH2D2
         DO 510 J =1,3
            FORC = FORCG(J) - PRESG*A3(J)
            TCLA = TCLA + POIDS*(FORC*DIVT+DFOR(J))*DEPL(J)
510      CONTINUE
800   CONTINUE
9999  CONTINUE
C
      TSOM = TCLA + TSURF + TSURP
      ZR(IGTHET) = TSOM
C
      CALL JEDEMA()
      END

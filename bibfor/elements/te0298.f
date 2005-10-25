      SUBROUTINE TE0298 ( OPTION , NOMTE )
      IMPLICIT   NONE
      CHARACTER*16        OPTION , NOMTE
C.......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 24/10/2005   AUTEUR GALENNE E.GALENNE 
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
C  CALCUL DU TAUX DE RESTITUTION D'ENERGIE ELEMENTAIRE
C  BORDS ELEMENTS ISOPARAMETRIQUES 3D AVEC PRESSION
C
C  OPTION : 'CALC_G_BILI'   (CHARGES REELLES)
C           'CALC_G_BILI_F' (CHARGES FONCTIONS)
C
C ENTREES  ---> OPTION : OPTION DE CALCUL
C          ---> NOMTE  : NOM DU TYPE ELEMENT
C
C VECTEURS DIMENSIONNES POUR  NNO = 9 , NPG = 9
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
      INTEGER       NDIM,NNO,NBFPG,NBPG(10),NPG1,JIN,JVAL,COMPT
      INTEGER       IFORFU,IFORFV
      INTEGER       IPOIDS,IVF,IDFDE,I,J,K,KP,IFORCU,IFORCV,KK
      INTEGER       IDEPLU,IPRESU,IDEPLV,IPRESV
      INTEGER       ITHET,IGTHET,IGEOM
      INTEGER       IPREFU,IPREFV,ITEMPU,ITEMPV,ICODE,IPRESF,IDEPSE
      INTEGER       NNOS,JGANO
      INTEGER       IADZI,IAZK24
C
      REAL*8        A1(3),A2(3),A3(3),I1(3),I2(3),EPSI
      REAL*8        DFDX(9),DFDY(9)
      REAL*8        COOR(18),DEPLU(3),DEPLV(3)
      REAL*8        VAPARU(4),VAPARV(4)
      REAL*8        A1NORM,A3NORM,I2NORM,DIVT,TCLA,THETX,THETY,THETZ
      REAL*8        DTH1D1,DTH2D2,POIDS,TH1,TH2,TSOM,FORCU,FORCV,VF
      REAL*8        DFODU1(3),DFODU2(3),DFODV1(3),DFODV2(3)
      REAL*8        DFORU(3), DFORV(3), COORG(3)
      REAL*8        PRESGU, PRESGV, FORCGU(3), FORCGV(3)
      REAL*8        PRESNU(9), PRESNV(9)
      REAL*8        FORCNU(27), FORCNV(27)
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

C RECUPERATION CHARGE, MATER...
      CALL JEVECH ( 'PGEOMER', 'L', IGEOM )
      CALL JEVECH ( 'PDEPLAU', 'L', IDEPLU )
      CALL JEVECH ( 'PDEPLAV', 'L', IDEPLV )
      IF ( OPTION .EQ. 'CALC_G_BILI_F' ) THEN
        FONC = .TRUE.
        CALL JEVECH ( 'UPFF23D', 'L', IFORFU  )
        CALL JEVECH ( 'UPFF23D', 'L', IFORFV  )
        CALL JEVECH ( 'UPRESSF', 'L', IPREFU  )
        CALL JEVECH ( 'VPRESSF', 'L', IPREFV  )
        CALL JEVECH ( 'UTEMPSR', 'L', ITEMPU )
        CALL JEVECH ( 'UTEMPSR', 'L', ITEMPV )
        NOMPAR(1) = 'X'
        NOMPAR(2) = 'Y'
        NOMPAR(3) = 'Z'
        NOMPAR(4) = 'INST'
        VAPARU(4) = ZR(ITEMPU)
        VAPARV(4) = ZR(ITEMPV)
      ELSE
        FONC =.FALSE.
        CALL JEVECH ( 'UPFR23D', 'L', IFORCU )
        CALL JEVECH ( 'VPFR23D', 'L', IFORCV )
        CALL JEVECH ( 'UPRESSR', 'L', IPRESU )
        CALL JEVECH ( 'VPRESSR', 'L', IPRESV )
      ENDIF
C
C
C - SI CHARGE FONCTION RECUPERATION DES VALEURS AUX PG ET NOEUDS
C
      IF ( FONC ) THEN
         DO 70 I = 1,NNO
            DO 80 J = 1,3
               VAPARU(J) = ZR(IGEOM+3*(I-1)+J-1)
               VAPARV(J) = ZR(IGEOM+3*(I-1)+J-1)
80          CONTINUE
            CALL FOINTE ('FM',ZK8(IPREFU),4,NOMPAR,
     &                   VAPARU,PRESNU(I),ICODE)
            CALL FOINTE ('FM',ZK8(IPREFV),4,NOMPAR,
     &                   VAPARV,PRESNV(I),ICODE)
            DO 75 J=1,3
               CALL FOINTE ('FM',ZK8(IFORFU+J-1),4,NOMPAR,VAPARU,
     &                      FORCNU(3*(I-1)+J),ICODE)
               CALL FOINTE ('FM',ZK8(IFORFV+J-1), 4,NOMPAR,VAPARV,
     &                      FORCNV(3*(I-1)+J),ICODE)
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
            DEPLU(J) = 0.D0
            DEPLV(J) = 0.D0
            DFODU1(J) = 0.D0
            DFODU2(J) = 0.D0
            DFODV1(J) = 0.D0
            DFODV2(J) = 0.D0
            DFORU(J)   = 0.D0
            DFORV(J)   = 0.D0
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
               VAPARU(J) = COORG(J)
               VAPARV(J) = COORG(J)
60          CONTINUE
            CALL FOINTE ('FM',ZK8(IPREFU),4,NOMPAR,VAPARU,PRESGU,ICODE)
            CALL FOINTE ('FM',ZK8(IPREFV),4,NOMPAR,VAPARV,PRESGV,ICODE)
            DO 65 J = 1 , 3
               CALL FOINTE ('FM', ZK8(IFORFU+J-1), 4, NOMPAR, VAPARU,
     &                                          FORCGU(J),ICODE)
               CALL FOINTE ('FM', ZK8(IFORFV+J-1), 4, NOMPAR, VAPARV,
     &                                          FORCGV(J),ICODE)
65          CONTINUE
C
            DO 400 I = 1 , NNO
               DO 410 J = 1 , 3
                  DFODU1(J) = DFODU1(J) +
     &            ( FORCNU(3*(I-1)+J) - PRESNU(I)*A3(J) ) * DFDX(I)
                  DFODV1(J) = DFODV1(J) +
     &            ( FORCNV(3*(I-1)+J) - PRESNV(I)*A3(J) ) * DFDX(I)
C
                  DFODU2(J) = DFODU2(J) +
     &            ( FORCNU(3*(I-1)+J) - PRESNU(I)*A3(J) ) * DFDY(I)
                  DFODV2(J) = DFODV2(J) +
     &            ( FORCNV(3*(I-1)+J) - PRESNV(I)*A3(J) ) * DFDY(I)
410            CONTINUE
400         CONTINUE
         ELSE
            PRESGU = 0.D0
            FORCGU(1) = 0.D0
            FORCGU(2) = 0.D0
            FORCGU(3) = 0.D0
            PRESGV = 0.D0
            FORCGV(1) = 0.D0
            FORCGV(2) = 0.D0
            FORCGV(3) = 0.D0
            DO 4 I = 1 , NNO
               PRESGU = PRESGU + ZR(IPRESU+I-1)*ZR(IVF+K+I-1)
               PRESGV = PRESGV + ZR(IPRESV+I-1)*ZR(IVF+K+I-1)
               DO 6 J = 1 , 3
                  FORCGU(J)=FORCGU(J)+
     &               ZR(IFORCU+3*(I-1)+J-1)*ZR(IVF+K+I-1)
                  FORCGV(J)=FORCGV(J)+
     &               ZR(IFORCV+3*(I-1)+J-1)*ZR(IVF+K+I-1)
 6             CONTINUE
 4          CONTINUE
         ENDIF
C
         DO 300 I=1,NNO
            DO 310 J=1,3
               DEPLU(J) = DEPLU(J)+ 
     &                    ZR(IVF+K+I-1)*ZR(IDEPLU+3*(I-1)+J-1)
               DEPLV(J) = DEPLV(J)+ 
     &                    ZR(IVF+K+I-1)*ZR(IDEPLV+3*(I-1)+J-1)    
C     
               TH1  = TH1 + ZR(IVF+K+I-1)*ZR(ITHET+3*(I-1)+J-1)*I1(J)
               TH2  = TH2 + ZR(IVF+K+I-1)*ZR(ITHET+3*(I-1)+J-1)*I2(J)
               DTH1D1= DTH1D1+ ZR(ITHET+3*(I-1)+J-1)*I1(J)*DFDX(I)
               DTH2D2= DTH2D2+ ZR(ITHET+3*(I-1)+J-1)*I2(J)*DFDY(I)
310         CONTINUE
300      CONTINUE
C
         DO 320 J=1,3
            DFORU(J) = DFORU(J) + DFODU1(J)*TH1+DFODU2(J)*TH2
            DFORV(J) = DFORV(J) + DFODV1(J)*TH1+DFODV2(J)*TH2
320      CONTINUE
C
         DIVT = DTH1D1+DTH2D2
C         
         DO 510 J =1,3
            FORCU = FORCGU(J) - PRESGU*A3(J)
            FORCV = FORCGV(J) - PRESGV*A3(J)
C
            TCLA = TCLA + 
     &             POIDS*0.5D0*(FORCU*DIVT+DFORU(J))*DEPLV(J)+
     &             POIDS*0.5D0*(FORCV*DIVT+DFORV(J))*DEPLU(J) 
C
510      CONTINUE
800   CONTINUE
9999  CONTINUE
C
      ZR(IGTHET) = TCLA
C
      CALL JEDEMA()
      END

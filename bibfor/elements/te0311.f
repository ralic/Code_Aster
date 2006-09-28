      SUBROUTINE TE0311(OPTION,NOMTE)
      IMPLICIT NONE
      CHARACTER*16 OPTION,NOMTE
C.......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C  ET DES FACTEURS D'INTENSITE DES CONTRAINTES K1 K2 K3
C  BORDS ELEMENTS ISOPARAMETRIQUES 3D
C
C  OPTION : 'CALC_K_G'    (CHARGES REELLES)
C           'CALC_K_G_F'  (CHARGES FONCTIONS)
C
C ENTREES  ---> OPTION : OPTION DE CALCUL
C          ---> NOMTE  : NOM DU TYPE ELEMENT
C
C VECTEURS DIMENSIONNES POUR  NNO = 9 , NPG = 9
C.......................................................................
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER NDIM,NNO,NBFPG,NBPG(10),NPG1,JIN,JVAL,COMPT,IFORF
      INTEGER IPOIDS,IVF,IDFDE,I,J,K,KP,IFORC,KK,IND
      INTEGER IDEPL,IPRES,ITHET,IGTHET,IGEOM,IPREF,ITEMPS,ICODE
      INTEGER IDEPSE,IFO23R,IFO23F,IPRESS,IPRESF
      INTEGER NNOS,JGANO,INO,JLSN,JLST
      INTEGER IADZI,IAZK24
C
      REAL*8 A1(3),A2(3),A3(3),I1(3),I2(3),EPSI,DFDX(9),DFDY(9)
      REAL*8 COOR(18),DEPL(3),VALPAR(4),R8PREM
      REAL*8 A1NORM,A3NORM,I2NORM,DIVT,TCLA,THETX,THETY,THETZ
      REAL*8 DTH1D1,DTH2D2,POIDS,TH1,TH2,TSOM,LSNG,LSTG
      REAL*8 FORC,DFORD1(3),DFORD2(3),DFOR(3),COORG(3)
C                                NNO      3*NNO
      REAL*8 PRESG,FORCG(3),PRESN(9),FORCN(27)
C
      REAL*8 DEPI,R8DEPI,NORME,FF,VF
C
      REAL*8 TCLA1,TCLA2,TCLA3
      REAL*8 A(3),E1(3),E2(3),E3(3),P(3,3),COURB(3,3,3)
      REAL*8 VGL(3),XLG,YLG,AG(3),PHIG,RG,TG,VALRES(3),DEVRES(3)
      REAL*8 E,NU,MU
      REAL*8 CR2,KA,COEFF,COEFF3
      REAL*8 U1L(3),U1G(3),U2L(3),U2G(3),U3L(3),U3G(3)
      REAL*8 G,K1,K2,K3,PRSC
      LOGICAL LCOUR
      INTEGER IBALO,ITEMP,IMATE,IRET
      CHARACTER*8 NOMRES(3)
      CHARACTER*2 CODRET(3)
C
      CHARACTER*8 NOMPAR(4)
C
      LOGICAL FONC
C.......................................................................
C
      CALL JEMARQ
C
      DEPI = R8DEPI()
C
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDE,JGANO)
      CALL JEVECH('PTHETAR','L',ITHET)
      TCLA = 0.D0
      TCLA1 = 0.D0
      TCLA2 = 0.D0
      TCLA3 = 0.D0
      COEFF = 0.D0
      COEFF3 = 0.D0
      CALL JEVECH('PGTHETA','E',IGTHET)

C
C - PAS DE CALCUL DE G POUR LES ELEMENTS OU LA VALEUR DE THETA EST NULLE
C
      COMPT = 0
      EPSI = 1.D-10
      DO 20 I = 1,NNO
        THETX = ZR(ITHET+3*(I-1)+1-1)
        THETY = ZR(ITHET+3*(I-1)+2-1)
        THETZ = ZR(ITHET+3*(I-1)+3-1)
        IF ((ABS(THETX).LT.EPSI) .AND. (ABS(THETY).LT.EPSI) .AND.
     &      (ABS(THETZ).LT.EPSI)) THEN
          COMPT = COMPT + 1
        END IF
 20   CONTINUE
      IF (COMPT .EQ. NNO) GOTO 9999
C
C RECUPERATION CHARGE, MATER...
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PDEPLAR','L',IDEPL)
C
      CALL JEVECH('PBASLOR','L',IBALO)
      CALL JEVECH('PLSN','L',JLSN)
      CALL JEVECH('PLST','L',JLST)

      CALL JEVECH('PTEMPER','L',ITEMP)
      CALL JEVECH('PMATERC','L',IMATE)
C
      IF (OPTION.EQ.'CALC_K_G_F') THEN
        FONC = .TRUE.
        CALL JEVECH('PFF2D3D','L',IFORF)
        CALL JEVECH('PPRESSF','L',IPREF)
        CALL JEVECH('PTEMPSR','L',ITEMPS)
        NOMPAR(1) = 'X'
        NOMPAR(2) = 'Y'
        NOMPAR(3) = 'Z'
        NOMPAR(4) = 'INST'
        VALPAR(4) = ZR(ITEMPS)
      ELSE
        FONC = .FALSE.
        CALL JEVECH('PFR2D3D','L',IFORC)
        CALL JEVECH('PPRESSR','L',IPRES)
      END IF
C
C
C
C - SI CHARGE FONCTION RECUPERATION DES VALEURS AUX PG ET NOEUDS
C
      IF (FONC) THEN
        DO 70 I = 1,NNO
          DO 80 J = 1,3
            VALPAR(J) = ZR(IGEOM+3*(I-1)+J-1)
 80       CONTINUE
          CALL FOINTE('FM',ZK8(IPREF),4,NOMPAR,VALPAR,PRESN(I),ICODE)
          DO 75 J = 1,3
            CALL FOINTE('FM',ZK8(IFORF+J-1),4,NOMPAR,VALPAR,
     &                 FORCN(3*(I-1)+J),ICODE)
 75       CONTINUE
 70     CONTINUE
      END IF
C
C CALCUL DU REPERE LOCAL (A1, A2, A3)
C
      DO 130 J = 1,3
        A1(J) = ZR(IGEOM+3*(2-1)+J-1) - ZR(IGEOM+3*(1-1)+J-1)
        A2(J) = ZR(IGEOM+3*(3-1)+J-1) - ZR(IGEOM+3*(1-1)+J-1)
 130  CONTINUE
C
      A3(1) = A1(2)*A2(3) - A1(3)*A2(2)
      A3(2) = A1(3)*A2(1) - A1(1)*A2(3)
      A3(3) = A1(1)*A2(2) - A1(2)*A2(1)
C
C CALCUL DU REPERE LOCAL ORTHONORME (I1, I2, A3)
C
      I2(1) = A3(2)*A1(3) - A3(3)*A1(2)
      I2(2) = A3(3)*A1(1) - A3(1)*A1(3)
      I2(3) = A3(1)*A1(2) - A3(2)*A1(1)
C
      A1NORM = SQRT(A1(1)*A1(1)+A1(2)*A1(2)+A1(3)*A1(3))
      I2NORM = SQRT(I2(1)*I2(1)+I2(2)*I2(2)+I2(3)*I2(3))
      A3NORM = SQRT(A3(1)*A3(1)+A3(2)*A3(2)+A3(3)*A3(3))
      DO 150 I = 1,3
        I1(I) = A1(I) / A1NORM
        I2(I) = I2(I) / I2NORM
        A3(I) = A3(I) / A3NORM
 150  CONTINUE
C
      DO 1400 I = 1,NNO
        COOR(2*I-1) = 0.D0
        COOR(2*I) = 0.D0
        DO 1410 J = 1,3
          COOR(2*I-1) =
     &      COOR(2*I-1) + (ZR(IGEOM+3*(I-1)+J-1)-ZR(IGEOM+J-1))*I1(J)
          COOR(2*I) =
     &      COOR(2*I) + (ZR(IGEOM+3*(I-1)+J-1)-ZR(IGEOM+J-1))*I2(J)
 1410   CONTINUE
 1400 CONTINUE
C
C --- BOUCLE SUR LES POINTS DE GAUSS
C
      DO 800 KP = 1,NPG1
C
        K = (KP-1) * NNO
C
        DO 810 J = 1,3
          DEPL(J) = 0.D0
          DFORD1(J) = 0.D0
          DFORD2(J) = 0.D0
          DFOR(J) = 0.D0
          COORG(J) = 0.D0
 810    CONTINUE
        TG = 0.D0
        TH1 = 0.D0
        TH2 = 0.D0
        DTH1D1 = 0.D0
        DTH2D2 = 0.D0
        LSNG=0.D0
        LSTG=0.D0
C
        DO 820 I = 1,NNO

          TG   = TG   + ZR(ITEMP+I-1) * ZR(IVF+K+I-1)
          LSNG = LSNG + ZR(JLSN-1+I)  * ZR(IVF+K+I-1)
          LSTG = LSTG + ZR(JLST-1+I)  * ZR(IVF+K+I-1)

          DO 830 J = 1,3
            COORG(J) = COORG(J) + ZR(IVF+K+I-1)*ZR(IGEOM+3*(I-1)+J-1)
 830      CONTINUE
 820    CONTINUE
C
C       BASE LOCALE ASSOCIEE AU POINT DE GAUSS KP
C       (E1=GRLT,E2=GRLN,E3=E1^E2)
        DO 123 I = 1,3
          E1(I) = 0.D0
          E2(I) = 0.D0
          DO 321 INO = 1,NNO
            FF = ZR(IVF-1+NNO*(KP-1)+INO)
            E1(I) = E1(I) + ZR(IBALO-1+9*(INO-1)+I+3)*FF
            E2(I) = E2(I) + ZR(IBALO-1+9*(INO-1)+I+6)*FF
 321      CONTINUE
 123    CONTINUE
C
C       NORMALISATION DE LA BASE
        CALL NORMEV(E1,NORME)
        CALL NORMEV(E2,NORME)
        CALL PROVEC(E1,E2,E3)
C
C       CALCUL DE LA MATRICE DE PASSAGE P TELLE QUE
C       'GLOBAL' = P * 'LOCAL'
C
        DO 124 I = 1,3
          P(I,1) = E1(I)
          P(I,2) = E2(I)
          P(I,3) = E3(I)
 124    CONTINUE
C
C       COORDONNÉES POLAIRES DU POINT
        RG=SQRT(LSNG**2+LSTG**2)

        IF (RG.GT.R8PREM()) THEN
C         LE POINT N'EST PAS SUR LE FOND DE FISSURE
          PHIG = SIGN(1.D0,LSNG) * ABS(ATAN2(LSNG,LSTG))
          IRET=1
        ELSE
C         LE POINT EST SUR LE FOND DE FISSURE :
C         L'ANGLE N'EST PAS DÉFINI, ON LE MET À ZÉRO
C         ON NE FERA PAS LE CALCUL DES DÉRIVÉES
          PHIG=0.D0
          IRET=0
        ENDIF
C
        IF (IRET.EQ.0) CALL U2MESS('F','ELEMENTS3_69')

C
        IF ((ABS(LSNG) .LT. 1.0D-8) .AND.
     &      (LSTG .LT. 0.0D0)) THEN
C
C         ON DETERMINE SI ON EST SUR LA LEVRE X2 > 0 OU
C         SUR LA LEVRE X2 < 0
C
C         PRODUIT SCALAIRE E2 (AXE X2) * A3 (NORMALE DE L'ELEMENT)
          PRSC = 0.0D0
          DO 1111 I = 1,3
            PRSC = PRSC + E2(I)*A3(I)
 1111     CONTINUE
          IF (PRSC .GT. 0.0D0) THEN
C            ON EST SUR LA LEVRE X2 < 0
             PHIG = -1.0D0 * ABS(PHIG)
          ELSE
C            ON EST SUR LA LEVRE X2 > 0
             PHIG = ABS(PHIG)
          END IF
        END IF
C
C       RECUPERATION DES DONNEES MATERIAU
C
        NOMRES(1) = 'E'
        NOMRES(2) = 'NU'
        NOMRES(3) = 'ALPHA'
C
        CALL RCVADA(ZI(IMATE),'ELAS',TG,3,NOMRES,VALRES,DEVRES,CODRET)
        IF ((CODRET(1).NE.'OK') .OR. (CODRET(2).NE.'OK')) THEN
          CALL U2MESS('F','ELEMENTS3_70')
        END IF
        IF (CODRET(3) .NE. 'OK') THEN
          VALRES(3) = 0.D0
          DEVRES(3) = 0.D0
        END IF
C
        E = VALRES(1)
        NU = VALRES(2)
C
        MU = E / (2.D0*(1.D0+NU))
C
C       COEFFICIENTS DE CALCUL
        CR2 = SQRT(RG) / (2.D0*MU*SQRT(DEPI))
        KA = 3.D0 - 4.D0*NU
C
        COEFF = E / (1.D0-NU*NU)
        COEFF3 = 2.D0 * MU
C
C-----------------------------------------------------------------------
C       DEFINITION DU CHAMP SINGULIER AUXILIAIRE U1
C-----------------------------------------------------------------------
C       CHAMP SINGULIER AUXILIAIRE U1 DANS LA BASE LOCALE
        U1L(1) = CR2 * COS(PHIG*0.5D0) * (KA-COS(PHIG))
        U1L(2) = CR2 * SIN(PHIG*0.5D0) * (KA-COS(PHIG))
        U1L(3) = 0.D0
C
C       CHAMP SINGULIER U1 DANS LA BASE GLOBALE
        DO 5090 I = 1,3
          U1G(I) = 0.0D0
          DO 5190 IND = 1,3
            U1G(I) = U1G(I) + P(I,IND)*U1L(IND)
 5190     CONTINUE
 5090   CONTINUE
C
C-----------------------------------------------------------------------
C       DEFINITION DU CHAMP SINGULIER AUXILIAIRE U2
C-----------------------------------------------------------------------
C       CHAMP SINGULIER AUXILIAIRE U2 DANS LA BASE LOCALE
        U2L(1) = CR2 * SIN(PHIG*0.5D0) * (KA + 2.D0 + COS(PHIG))
        U2L(2) = CR2 * COS(PHIG*0.5D0) * (2.D0 - 1.D0 *
     &           (KA+COS(PHIG)))
        U2L(3) = 0.D0
C
C       CHAMP SINGULIER U2 DANS LA BASE GLOBALE
        DO 6090 I = 1,3
          U2G(I) = 0.0D0
          DO 6190 IND = 1,3
            U2G(I) = U2G(I) + P(I,IND)*U2L(IND)
 6190     CONTINUE
 6090   CONTINUE
C
C-----------------------------------------------------------------------
C       DEFINITION DU CHAMP SINGULIER AUXILIAIRE U3
C-----------------------------------------------------------------------
C       CHAMP SINGULIER AUXILIAIRE U3 DANS LA BASE LOCALE
        U3L(1) = 0.D0
        U3L(2) = 0.D0
        U3L(3) = 4.D0 * CR2 * SIN(PHIG*0.5D0)
C
C       CHAMP SINGULIER U3 DANS LA BASE GLOBALE
        DO 7090 I = 1,3
          U3G(I) = 0.0D0
          DO 7190 IND = 1,3
            U3G(I) = U3G(I) + P(I,IND)*U3L(IND)
 7190     CONTINUE
 7090   CONTINUE
C
        CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,COOR,DFDX,DFDY,POIDS)
C
        IF (FONC) THEN
          DO 60 J = 1,3
            VALPAR(J) = COORG(J)
 60       CONTINUE
          CALL FOINTE('FM',ZK8(IPREF),4,NOMPAR,VALPAR,PRESG,ICODE)
          DO 65 J = 1,3
            CALL FOINTE('FM',ZK8(IFORF+J-1),4,NOMPAR,VALPAR,FORCG(J),
     &                 ICODE)
 65       CONTINUE
C
          DO 400 I = 1,NNO
            DO 410 J = 1,3
              DFORD1(J) =
     &          DFORD1(J) + (FORCN(3*(I-1)+J)-PRESN(I)*A3(J))*DFDX(I)
              DFORD2(J) =
     &          DFORD2(J) + (FORCN(3*(I-1)+J)-PRESN(I)*A3(J))*DFDY(I)
 410        CONTINUE
 400      CONTINUE
        ELSE
          PRESG = 0.D0
          FORCG(1) = 0.D0
          FORCG(2) = 0.D0
          FORCG(3) = 0.D0
          DO 4 I = 1,NNO
            PRESG = PRESG + ZR(IPRES+I-1)*ZR(IVF+K+I-1)
            DO 6 J = 1,3
              FORCG(J) = FORCG(J) + ZR(IFORC+3*(I-1)+J-1)*ZR(IVF+K+I-1)
 6          CONTINUE
 4        CONTINUE
        END IF
C
        DO 300 I = 1,NNO
          DO 310 J = 1,3
            DEPL(J) = DEPL(J) + ZR(IVF+K+I-1)*ZR(IDEPL+3*(I-1)+J-1)
            TH1 = TH1 + ZR(IVF+K+I-1)*ZR(ITHET+3*(I-1)+J-1)*I1(J)
            TH2 = TH2 + ZR(IVF+K+I-1)*ZR(ITHET+3*(I-1)+J-1)*I2(J)
            DTH1D1 = DTH1D1 + ZR(ITHET+3*(I-1)+J-1)*I1(J)*DFDX(I)
            DTH2D2 = DTH2D2 + ZR(ITHET+3*(I-1)+J-1)*I2(J)*DFDY(I)
 310      CONTINUE
 300    CONTINUE
C
        DO 320 J = 1,3
          DFOR(J) = DFOR(J) + DFORD1(J)*TH1 + DFORD2(J)*TH2
 320    CONTINUE
C
        DIVT = DTH1D1 + DTH2D2
C
        DO 510 J = 1,3
          FORC = FORCG(J) - PRESG*A3(J)
          TCLA = TCLA + POIDS*(FORC*DIVT+DFOR(J))*DEPL(J)
          TCLA1 = TCLA1 + 0.5D0*POIDS*(FORC*DIVT+DFOR(J))*U1G(J)
          TCLA2 = TCLA2 + 0.5D0*POIDS*(FORC*DIVT+DFOR(J))*U2G(J)
          TCLA3 = TCLA3 + 0.5D0*POIDS*(FORC*DIVT+DFOR(J))*U3G(J)
C
 510    CONTINUE
C
 800  CONTINUE
 9999 CONTINUE
C
      G = TCLA
      K1 = TCLA1 * COEFF
      K2 = TCLA2 * COEFF
      K3 = TCLA3 * COEFF3
C
      ZR(IGTHET) = G
      ZR(IGTHET+1) = K1
      ZR(IGTHET+2) = K2
      ZR(IGTHET+3) = K3
C
      CALL JEDEMA
      END

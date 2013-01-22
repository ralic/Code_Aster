      SUBROUTINE DXMAT2(PGL,ICOU,NPG,ORDI,EPI,EPAIS,DM,INDITH)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER ICOU,NPG,INDITH
      REAL*8 PGL(3,3),ORDI,EPI,EPAIS,DM(3,3)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 21/01/2013   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C
C     BUT:
C       CALCUL DES MATRICES DE COEFFCIENTS THERMOELASTIQUES DE MEMBRANE,
C       POUR UN MATERIAU ISOTROPE OU MULTICOUCHE
C       LA VARIABLE INDITH EST INITIALISEE A 0
C       DANS LE CAS OU LE COEFFICIENT DE DILATATION ALPHA N'A
C       PAS ETE DONNE, INDITH VAUT -1 ET ON  NE CALCULE PAS LES
C       CONTRAINTES THERMIQUES
C
C     ARGUMENTS:
C     ----------
C
C      ENTREE :
C-------------
C IN   PGL(3,3) : MATRICE DE PASSAGE DU REPERE GLOBAL AU REPERE LOCAL
C IN   ICOU     : NUMERO DE LA COUCHE
C IN   NPG      : NOMBRE DE POINT DE GAUSS
C
C      SORTIE :
C-------------
C OUT  ORDI     :
C OUT  EPI      :
C OUT  EPAIS    :
C OUT  DM       :
C OUT  INDITH   :
C
C ......................................................................
C
C
C
C
      INTEGER JCOQU,JMATE,IRET
      INTEGER NBV,I,NBPAR,NBCOU,JCOU
      INTEGER ICODRE(134)

      REAL*8 CDF,CDM,VALRES(134),DF(3,3),DMF(3,3),T1VE(9)
      REAL*8 YOUNG,NU,VALPAR
      REAL*8 XAB1(3,3),DH(3,3)
      REAL*8 S,C
      REAL*8 ALPHAT
      REAL*8 ALPHA,BETA,R8DGRD,R8BID4(4)

      CHARACTER*2 VAL
      CHARACTER*3 NUM
      CHARACTER*8 NOMRES(134),NOMPAR
      CHARACTER*10 PHENOM
C
C     ------------------------------------------------------------------
C
      CALL R8INIR(9,0.D0,DM,1)
      CALL R8INIR(9,0.D0,DF,1)
      CALL R8INIR(9,0.D0,DH,1)
      CALL R8INIR(9,0.D0,DMF,1)

      CALL JEVECH('PCACOQU','L',JCOQU)
      EPAIS = ZR(JCOQU)
      EPI = EPAIS
      ORDI = 0.D0
      ALPHA = ZR(JCOQU+1)*R8DGRD()
      BETA = ZR(JCOQU+2)*R8DGRD()

C     ------------------------------------------------
      INDITH = 0
      CALL JEVECH('PMATERC','L',JMATE)
      CALL RCCOMA(ZI(JMATE),'ELAS',1,PHENOM,ICODRE)

      IF (PHENOM.EQ.'ELAS_COQMU') THEN

        CALL COQREP(PGL, ALPHA, BETA, R8BID4,R8BID4,C,S)
C       CALCUL DE LA MATRICE T1VE DE PASSAGE D'UNE MATRICE
C       (3,3) DU REPERE DE LA VARIETE AU REPERE ELEMENT
        T1VE(1) = C*C
        T1VE(4) = S*S
        T1VE(7) = C*S
        T1VE(2) =  T1VE(4)
        T1VE(5) =  T1VE(1)
        T1VE(8) = -T1VE(7)
        T1VE(3) = -T1VE(7) - T1VE(7)
        T1VE(6) =  T1VE(7) + T1VE(7)
        T1VE(9) =  T1VE(1) - T1VE(4)
        NBV = 56
        DO 10 I = 1,NBV
          CALL CODENT(I,'G',NUM)
          NOMRES(I) = 'HOM_'//NUM
   10   CONTINUE
        CALL CODENT(ICOU,'G',NUM)
        DO 20 I = 1,78
          CALL CODENT(I,'G',VAL)
          NOMRES(56+I) = 'C'//NUM//'_V'//VAL
   20   CONTINUE

      ELSE IF (PHENOM.EQ.'ELAS') THEN
        NBV = 3
        NOMRES(1) = 'E'
        NOMRES(2) = 'NU'
        NOMRES(3) = 'ALPHA'

      ELSE IF (PHENOM.EQ.'ELAS_COQUE') THEN

        CALL COQREP(PGL, ALPHA, BETA, R8BID4,R8BID4,C,S)

C       CALCUL DE LA MATRICE T1VE DE PASSAGE D'UNE MATRICE
C       (3,3) DU REPERE DE LA VARIETE AU REPERE ELEMENT

        T1VE(1) = C*C
        T1VE(4) = S*S
        T1VE(7) = C*S
        T1VE(2) =  T1VE(4)
        T1VE(5) =  T1VE(1)
        T1VE(8) = -T1VE(7)
        T1VE(3) = -T1VE(7) - T1VE(7)
        T1VE(6) =  T1VE(7) + T1VE(7)
        T1VE(9) =  T1VE(1) - T1VE(4)

        NBV = 10
        NOMRES(1) = 'MEMB_L  '
        NOMRES(2) = 'MEMB_LT '
        NOMRES(3) = 'MEMB_T  '
        NOMRES(4) = 'MEMB_G_L'
        NOMRES(5) = 'FLEX_L  '
        NOMRES(6) = 'FLEX_LT '
        NOMRES(7) = 'FLEX_T  '
        NOMRES(8) = 'FLEX_G_L'
        NOMRES(9) = 'CISA_L  '
        NOMRES(10) = 'CISA_T  '
        NOMRES(11) = 'ALPHA   '
      ELSE
        CALL U2MESS('F','ELEMENTS_42')
      END IF

C===============================================================
C     -- RECUPERATION DE LA TEMPERATURE POUR LE MATERIAU:
      CALL JEVECH('PNBSP_I','L',JCOU)
      NBCOU=ZI(JCOU)
      CALL MOYTEM('RIGI',NPG,3*NBCOU,'+',VALPAR,IRET)
      NBPAR = 1
      NOMPAR = 'TEMP'
C===============================================================

      IF (PHENOM.EQ.'ELAS') THEN
C        ------ MATERIAU ISOTROPE ------------------------------------


        CALL RCVALB('RIGI',1,1,'+',ZI(JMATE),' ',PHENOM,
     &              NBPAR,NOMPAR,VALPAR,2,NOMRES,
     &              VALRES,ICODRE,1)
        CALL RCVALB('RIGI',1,1,'+',ZI(JMATE),' ',PHENOM,
     &              NBPAR,NOMPAR,VALPAR,1,
     &              NOMRES(3),VALRES(3),ICODRE(3),0)
        IF ((ICODRE(3).NE.0).OR.(VALRES(3).EQ.0.D0)) THEN
          INDITH = -1
          GO TO 70
        ELSEIF ((IRET.EQ.1).AND.(ICODRE(3).NE.0)) THEN
          CALL U2MESS('F','CALCULEL_15')
        END IF
        YOUNG = VALRES(1)
        NU = VALRES(2)
        ALPHAT = VALRES(3)
        YOUNG = YOUNG*ALPHAT
C      ------------------------------------------------------------
C      ---- CALCUL DE LA MATRICE DE RIGIDITE EN FLEXION --------------
        CDF = YOUNG*EPAIS*EPAIS*EPAIS/12.D0/ (1.D0-NU*NU)
        DF(1,1) = CDF
        DF(1,2) = CDF*NU
        DF(2,1) = DF(1,2)
        DF(2,2) = DF(1,1)
C      ---- CALCUL DE LA MATRICE DE RIGIDITE EN MEMBRANE -------------
        CDM = EPAIS*YOUNG/ (1.D0-NU*NU)
        DM(1,1) = CDM
        DM(1,2) = CDM*NU
        DM(2,1) = DM(1,2)
        DM(2,2) = DM(1,1)
C        ---------------------------------------------------------------

      ELSE IF (PHENOM.EQ.'ELAS_COQUE') THEN
        CALL RCVALB('RIGI',1,1,'+',ZI(JMATE),' ',PHENOM,
     &              NBPAR,NOMPAR,VALPAR,NBV,NOMRES,
     &              VALRES,ICODRE,1)
        CALL RCVALB('RIGI',1,1,'+',ZI(JMATE),' ',PHENOM,
     &            NBPAR,NOMPAR,VALPAR,1,
     &            NOMRES(11),  VALRES(11),ICODRE(11),0)
        IF ((ICODRE(11).NE.0).OR.(VALRES(11).EQ.0.D0)) THEN
          INDITH = -1
          GO TO 70
        ELSEIF ((IRET.EQ.1).AND.(ICODRE(11).NE.0)) THEN
          CALL U2MESS('F','CALCULEL_15')
        END IF
        ALPHAT = VALRES(11)
C        ---- CALCUL DE LA MATRICE DE RIGIDITE EN MEMBRANE -------------
        DM(1,1) = VALRES(1)*ALPHAT
        DM(1,2) = VALRES(2)*ALPHAT
        DM(2,1) = DM(1,2)
        DM(2,2) = VALRES(3)*ALPHAT
C        ---- CALCUL DE LA MATRICE DE RIGIDITE EN FLEXION --------------
        DF(1,1) = VALRES(5)*ALPHAT
        DF(1,2) = VALRES(6)*ALPHAT
        DF(2,1) = DF(1,2)
        DF(2,2) = VALRES(7)*ALPHAT
C        ----------- MATRICES DANS LE REPERE INTRINSEQUE DE L'ELEMENT --
        CALL UTBTAB('ZERO',3,3, DM,T1VE,XAB1,DM)
        CALL UTBTAB('ZERO',3,3, DF,T1VE,XAB1,DF)
        CALL UTBTAB('ZERO',3,3,DMF,T1VE,XAB1,DMF)

      ELSE IF (PHENOM.EQ.'ELAS_COQMU') THEN
C        ------ MATERIAU MULTICOUCHE -----------------------------------
        CALL RCVALB('RIGI',1,1,'+',ZI(JMATE),' ',PHENOM,
     &             NBPAR,NOMPAR,VALPAR,1,
     &             NOMRES(19), VALRES(19),ICODRE(19),1)
        EPAIS = VALRES(19)
        CALL RCVALB('RIGI',1,1,'+',ZI(JMATE),' ',PHENOM,
     &             NBPAR,NOMPAR,VALPAR,1,
     &             NOMRES(57), VALRES(57),ICODRE(57),1)
        EPI = VALRES(57)
        CALL RCVALB('RIGI',1,1,'+',ZI(JMATE),' ',PHENOM,
     &              NBPAR,NOMPAR,VALPAR,1,
     &              NOMRES(59),VALRES(59),ICODRE(59),1)
        ORDI = VALRES(59)
        CALL RCVALB('RIGI',1,1,'+',ZI(JMATE),' ',PHENOM,
     &             NBPAR,NOMPAR,VALPAR,27,
     &             NOMRES(102), VALRES(102),ICODRE(102),1)
        DM(1,1) = VALRES(102)
        DM(1,2) = VALRES(103)
        DM(1,3) = VALRES(104)
        DM(2,1) = VALRES(105)
        DM(2,2) = VALRES(106)
        DM(2,3) = VALRES(107)
        DM(3,1) = VALRES(108)
        DM(3,2) = VALRES(109)
        DM(3,3) = VALRES(110)
        DMF(1,1) = VALRES(111)
        DMF(1,2) = VALRES(112)
        DMF(1,3) = VALRES(113)
        DMF(2,1) = VALRES(114)
        DMF(2,2) = VALRES(115)
        DMF(2,3) = VALRES(116)
        DMF(3,1) = VALRES(117)
        DMF(3,2) = VALRES(118)
        DMF(3,3) = VALRES(119)
        DF(1,1) = VALRES(120)
        DF(1,2) = VALRES(121)
        DF(1,3) = VALRES(122)
        DF(2,1) = VALRES(123)
        DF(2,2) = VALRES(124)
        DF(2,3) = VALRES(125)
        DF(3,1) = VALRES(126)
        DF(3,2) = VALRES(127)
        DF(3,3) = VALRES(128)

C        ----------- MATRICES DANS LE REPERE INTRINSEQUE DE L'ELEMENT --

        CALL UTBTAB('ZERO',3,3, DM,T1VE,XAB1,DM)
        CALL UTBTAB('ZERO',3,3, DF,T1VE,XAB1,DF)
        CALL UTBTAB('ZERO',3,3,DMF,T1VE,XAB1,DMF)

      END IF
   70 CONTINUE
      END

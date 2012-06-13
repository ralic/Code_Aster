      SUBROUTINE TE0158 ( OPTION , NOMTE )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      CHARACTER*(*)       OPTION , NOMTE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C     ------------------------------------------------------------------
C IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
C      'DEGE_ELNO     : DEFORMATIONS GENERALISEES DE POUTRE
C IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
C       'MECA_POU_D_E'  : POUTRE DROITE D'EULER       (SECTION VARIABLE)
C       'MECA_POU_D_T'  : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
C       'MECA_POU_C_T'  : POUTRE COURBE DE TIMOSHENKO(SECTION CONSTANTE)
C       'MECA_POU_D_EM' : POUTRE DROITE MULTIFIBRE D EULER (SECT. CONST)
C       'MECA_POU_D_TG' : POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
C       'MECA_POU_D_TGM': POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
C                         MULTI-FIBRES SECTION CONSTANTE
C     ------------------------------------------------------------------
C IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
C IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
C     ------------------------------------------------------------------
C
      INTEGER      JEFFG, LMATER, LSECT, LX,IRET,
     &             LORIEN, JDEPL, I, J, KP, NNO, NC,
     &             NPG,ITEMP

      CHARACTER*4  FAMI
      CHARACTER*8  NOMAIL
      CHARACTER*16 CH16
      INTEGER      LSECT2, IPOS, IN, IADZI, IAZK24
      REAL*8       B(4),GG,XI,WI
      REAL*8       UL(14), PGL(3,3), D1B(6,12), DEGE(3,7),D1BTG(7,14)
      REAL*8       DEGEM(6)
      REAL*8       ZERO, UN, DEUX, TEMP, E, XNU, EPSTHE, G, XL
      REAL*8       A, XIY, XIZ, ALFAY, ALFAZ, PHIY, PHIZ
      REAL*8       KSI1, D1B3(2,3), EY, EZ
C     ------------------------------------------------------------------
      ZERO   = 0.D0
      UN     = 1.D0
      DEUX   = 2.D0
C     ------------------------------------------------------------------
C
C NOMBRE DE POINTS DE GAUSS
      NPG = 3
      FAMI = 'RIGI'

      IF ( OPTION .EQ. 'DEGE_ELNO' ) THEN
         CALL JEVECH ('PDEFOGR', 'E', JEFFG )
      ELSE IF ( OPTION .EQ. 'DEGE_ELGA' ) THEN
         CALL JEVECH ('PDEFOPG', 'E', JEFFG )
      ELSE
         CH16 = OPTION
         CALL U2MESK('F','ELEMENTS2_47',1,CH16)
      ENDIF
C
C     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
      CALL JEVECH ('PGEOMER', 'L',LX)
      LX = LX - 1
      XL = SQRT( (ZR(LX+4)-ZR(LX+1))**2
     &  + (ZR(LX+5)-ZR(LX+2))**2 + (ZR(LX+6)-ZR(LX+3))**2 )
      IF( XL .EQ. ZERO ) THEN
         CALL TECAEL(IADZI,IAZK24)
         NOMAIL = ZK24(IAZK24-1+3)(1:8)
         CALL U2MESK('F','ELEMENTS2_43',1,NOMAIL)
      ENDIF
C
C     --- RECUPERATION DES ORIENTATIONS ---
      CALL JEVECH ('PCAORIE', 'L', LORIEN )
      CALL MATROT ( ZR(LORIEN) , PGL )
C
C     --- RECUPERATION DES DEPLACEMENTS ---
      CALL JEVECH ('PDEPLAR', 'L', JDEPL)

      IF(NOMTE.NE.'MECA_POU_D_EM') THEN
C     --- CARACTERISTIQUES MATERIAUX ---
         CALL JEVECH ('PMATERC', 'L', LMATER)
C
         CALL VERIFM(FAMI,NPG,1,'+',ZI(LMATER),'ELAS',1,EPSTHE,IRET)
         ITEMP=0
         IF (IRET.EQ.0) ITEMP=1

         CALL MOYTEM(FAMI,NPG,1,'+',TEMP,IRET)
         CALL MATELA(ZI(LMATER),' ',ITEMP,TEMP,E,XNU)
C
         G = E / ( DEUX * ( UN + XNU ) )
C
C        --- CARACTERISTIQUES GENERALES DES SECTIONS ---
         CALL JEVECH ('PCAGNPO', 'L',LSECT)
         LSECT = LSECT-1
C
C        --- SECTION INITIALE ---
         A     =  ZR(LSECT+ 1)
         XIY   =  ZR(LSECT+ 2)
         XIZ   =  ZR(LSECT+ 3)
         ALFAY =  ZR(LSECT+ 4)
         ALFAZ =  ZR(LSECT+ 5)

         IF (NOMTE.EQ.'MECA_POU_D_TG' .OR.
     &       NOMTE.EQ.'MECA_POU_D_TGM' ) THEN
            LSECT2 = LSECT + 11
            EY     = -(ZR(LSECT+6)+ZR(LSECT2+6))/DEUX
            EZ     = -(ZR(LSECT+7)+ZR(LSECT2+7))/DEUX
         ENDIF
C
         IF     ( NOMTE .EQ. 'MECA_POU_D_E' )  THEN
            NNO = 2
            NC  = 6
            PHIY = ZERO
            PHIZ = ZERO
         ELSEIF ( NOMTE .EQ. 'MECA_POU_D_T'  ) THEN
            NNO = 2
            NC  = 6
            PHIY = E*XIZ*12.D0*ALFAY/ (XL*XL*G*A)
            PHIZ = E*XIY*12.D0*ALFAZ/ (XL*XL*G*A)
         ELSEIF (NOMTE.EQ.'MECA_POU_D_TG' .OR.
     &           NOMTE.EQ.'MECA_POU_D_TGM' ) THEN
            NNO = 2
            NC  = 7
            PHIY = E*XIZ*12.D0*ALFAY/ (XL*XL*G*A)
            PHIZ = E*XIY*12.D0*ALFAZ/ (XL*XL*G*A)
         ELSE
            CH16 = NOMTE
            CALL U2MESK('F','ELEMENTS2_42',1,CH16)
         ENDIF
C
C        --- PASSAGE DES DEPLACEMENTS DANS LE REPERE LOCAL ---
         CALL UTPVGL ( NNO, NC, PGL, ZR(JDEPL), UL )
C
         IF (NOMTE.EQ.'MECA_POU_D_TG' .OR.
     &       NOMTE.EQ.'MECA_POU_D_TGM' ) THEN
C
C        --- PASSAGE DE G (CENTRE DE GRAVITE) A C (CENTRE DE TORSION)
            DO 20 I = 1,2
               UL(7* (I-1)+2) = UL(7* (I-1)+2) - EZ*UL(7* (I-1)+4)
               UL(7* (I-1)+3) = UL(7* (I-1)+3) + EY*UL(7* (I-1)+4)
20          CONTINUE
         ENDIF
C
C        BOUCLE SUR LES POINTS DE GAUSS
         DO 30 KP = 1,3
            IF (NOMTE.EQ.'MECA_POU_D_TG' .OR.
     &          NOMTE.EQ.'MECA_POU_D_TGM' ) THEN
               CALL JSD1FF ( KP, XL, PHIY, PHIZ, D1BTG )
            ELSE
               CALL JPD1FF ( KP, XL, PHIY, PHIZ, D1B )
            ENDIF
C
            DO 32 I = 1,NC
               DEGE(KP,I) = ZERO
               DO 34 J = 1,2*NC
                  IF (NOMTE.EQ.'MECA_POU_D_TG' .OR.
     &                NOMTE.EQ.'MECA_POU_D_TGM' ) THEN
                     DEGE(KP,I) = DEGE(KP,I) + D1BTG(I,J)*UL(J)
                  ELSE
                     DEGE(KP,I) = DEGE(KP,I) + D1B(I,J)*UL(J)
                  ENDIF
34             CONTINUE
32          CONTINUE
            DEGE(KP,1) = DEGE(KP,1) - EPSTHE
C
30       CONTINUE

         IF (OPTION.EQ.'DEGE_ELGA') THEN
             DO 36 KP=1,3
               ZR(JEFFG-1+(KP-1)*NC + 1) = DEGE(KP,1)
               ZR(JEFFG-1+(KP-1)*NC + 2) = DEGE(KP,2)
               ZR(JEFFG-1+(KP-1)*NC + 3) = DEGE(KP,3)
               ZR(JEFFG-1+(KP-1)*NC + 4) = DEGE(KP,4)
               ZR(JEFFG-1+(KP-1)*NC + 5) = DEGE(KP,4)
               ZR(JEFFG-1+(KP-1)*NC + 6) = DEGE(KP,6)
36           CONTINUE
         ELSE   
C        --- POUR LE POINT 1 ---
           KSI1 = -SQRT( 5.D0 / 3.D0 )
           D1B3(1,1) = KSI1*(KSI1-1.D0)/2.0D0
           D1B3(1,2) = 1.D0-KSI1*KSI1
           D1B3(1,3) = KSI1*(KSI1+1.D0)/2.0D0
C        --- POUR LE POINT 2 ---
           KSI1 = SQRT( 5.D0 / 3.D0 )
           D1B3(2,1) = KSI1*(KSI1-1.D0)/2.0D0
           D1B3(2,2) = 1.D0-KSI1*KSI1
           D1B3(2,3) = KSI1*(KSI1+1.D0)/2.0D0
C
           DO 42 I = 1,NC
             DO 44 KP = 1 , 3
                ZR(JEFFG+I-1)   =ZR(JEFFG+I-1)   +DEGE(KP,I)*D1B3(1,KP)
                ZR(JEFFG+NC+I-1)=ZR(JEFFG+NC+I-1)+DEGE(KP,I)*D1B3(2,KP)
44           CONTINUE
42        CONTINUE
        ENDIF
      ELSE
C
C     POUTRE MULTIFIBRES MECA_POU_D_EM
         NNO = 2
         NC  = 6
C        --- PASSAGE DES DEPLACEMENTS DANS LE REPERE LOCAL ---
         CALL UTPVGL(NNO,NC,PGL,ZR(JDEPL),UL)
C
         IF(OPTION.EQ.'DEGE_ELNO') THEN
            DO 50 IN=1,2
               CALL PMFPTI(-IN,XL,XI,WI,B,GG)
C   ZERO POUR LA VARIABLE ALPHA DES MODES INCOMPATIBLES CAR NON ACTIF
C   SI CALCUL ELASTIQUE (RIGI_MECA et X_X_DEPL)
               CALL PMFDGE(B,GG,UL,ZERO,DEGEM)
               IPOS=JEFFG+NC*(IN-1)
               DO 60 I = 1, NC
                  ZR(IPOS+I-1) = DEGEM(I)
60             CONTINUE
50          CONTINUE
C
         ELSEIF(OPTION.EQ.'DEGE_ELGA') THEN
            DO 55 IN=1,2
               CALL PMFPTI(IN,XL,XI,WI,B,GG)
               CALL PMFDGE(B,GG,UL,ZERO,DEGEM)
               IPOS=JEFFG+NC*(IN-1)
               DO 65 I = 1, NC
                  ZR(IPOS+I-1) = DEGEM(I)
65             CONTINUE
55          CONTINUE
         ENDIF
      ENDIF

      END

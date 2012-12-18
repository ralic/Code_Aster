      SUBROUTINE TE0150(OPTION,NOMTE)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*(*)     OPTION,NOMTE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 18/12/2012   AUTEUR SELLENET N.SELLENET 
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
C     CALCULE LE CHARGEMENT INDUIT PAR UNE ELEVATION UNIFORME DE
C     TEMPERATURE DANS LES POUTRES D'EULER ET DE TIMOSHENKO
C     ------------------------------------------------------------------
C IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
C       'FC1D1D_MECA'       : FORCES LINEIQUES (COMP)
C       'FR1D1D_MECA'       : FORCES LINEIQUES (REEL)
C       'FF1D1D_MECA'       : FORCES LINEIQUES (FONCTION)
C       'SR1D1D_MECA'       : FORCES LINEIQUES SUIVEUSES (REEL)
C       'SF1D1D_MECA'       : FORCES LINEIQUES SUIVEUSES (FONCTION)
C       'CHAR_MECA_PESA_R'  : CHARGES DE PESANTEUR
C       'CHAR_MECA_ROTA_R'  : CHARGES DE ROTATION
C       'CHAR_MECA_TEMP_R'  : DEFORMATIONS THERMIQUES
C       'CHAR_MECA_SECH_R'  : DEFORMATIONS DUES AU SECHAGE
C       'CHAR_MECA_HYDR_R'  : DEFORMATIONS HYDRIQUES
C IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
C       'MECA_POU_D_E'  : POUTRE DROITE D'EULER       (SECTION VARIABLE)
C       'MECA_POU_D_T'  : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
C       'MECA_POU_C_T'  : POUTRE COURBE DE TIMOSHENKO(SECTION CONSTANTE)
C       'MECA_POU_D_EM' : POUTRE DROITE MULTIFIBRE D EULER (SECT. CONST)
C       'MECA_POU_D_TG' : POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
C       'MECA_POU_D_TGM': POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
C                         MULTI-FIBRES SECTION CONSTANTE
C     ------------------------------------------------------------------
      INTEGER      NBRES,NBPAR,LMATER,ITEMPS,IRET,LSECT,LSECT2
      INTEGER      ISTRUC,LORIEN,LRCOU,LVECT,LX
      INTEGER      ITYPE,NC,IND,I,J,IGAU
      PARAMETER                 (NBRES=2)
      REAL*8       VALPAR(3),VALRES(NBRES)
      INTEGER CODRES(NBRES)
      CHARACTER*4  FAMI
      CHARACTER*8  NOMPAR(3),NOMRES(NBRES),MATERI,NOMAIL
      CHARACTER*16 CH16
      REAL*8       E   ,  NU  , G
      REAL*8       A   ,  XIY ,  XIZ ,  ALFAY ,  ALFAZ ,  XJX ,  EZ,  EY
      REAL*8       A2  ,  XIY2,  XIZ2,  ALFAY2,  ALFAZ2,  XJX2,  XL
      REAL*8       ANG ,  RAD ,ANGARC,   ANGS2,   ALONG,   XFLY, XFLZ
      REAL*8       PGL(3,3), PGL1(3,3), PGL2(3,3), DE(14), FFE(14)
      REAL*8       BSM(14,14),MATK(105) ,CARSEC(6), R8BID
      REAL*8       XFL, TEMP, F, TRIGOM,ZERO, XJG
      REAL*8       FR(14), FI(14), FGR(14), FGI(14)
      REAL*8       FER(12), FEI(12)

      REAL*8       KENDOG,KDESSI,SECH,HYDR,INSTAN,SECHG(3),HYDRG(3),SREF
      INTEGER      NDIM,NNO,NNOS,NPG,IPOIDS
      INTEGER      IVF,IDFDX,JGANO
      INTEGER      ICOMPO,ISDCOM,NBGFMX,IADZI,IAZK24,ISICOM
      LOGICAL      LRHO
C
      DATA NOMRES / 'E', 'NU' /
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      ZERO = 0.D0
      FAMI = 'RIGI'
C
      CALL ELREF4(' ',FAMI,NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDX,JGANO)

C     -- POUR LA PESANTEUR ET LA ROTATION, ON N'A BESOIN QUE DE RHO
C        QUI EST FORCEMENT CONSTANT DANS LA MAILLE
      LRHO=(OPTION.EQ.'CHAR_MECA_PESA_R'.OR.
     &      OPTION.EQ.'CHAR_MECA_ROTA_R')

C     --- RECUPERATION DES CARACTERISTIQUES MATERIAUX (MOYENNE)
      IF(OPTION(13:16).NE.'1D1D'.AND..NOT.LRHO)THEN
         CALL JEVECH ('PMATERC', 'L', LMATER)
         CALL MOYTEM(FAMI,NPG,1,'+',VALPAR(1),IRET)
      ENDIF
      DO 10 I=1, NBRES
         VALRES(I) = 0.D0
 10   CONTINUE
C
      NBPAR  = 1
      NOMPAR(1) = 'TEMP'
C
      MATERI=' '
      IF(NOMTE.EQ.'MECA_POU_D_EM')THEN
        IF(OPTION(13:16).NE.'1D1D'.AND..NOT.LRHO) THEN
C       -- POUTRES MULTIFIBRES
C    --- APPEL INTEGRATION SUR SECTION
            CALL PMFITX(ZI(LMATER),1,CARSEC,R8BID)
C    --- RECUPERATION DU MATERIAU TORSION POUR ALPHA
            CALL JEVECH('PCOMPOR','L',ICOMPO)
            CALL JEVEUO(ZK16(ICOMPO-1+7),'L',ISDCOM)
            CALL JEVEUO(ZK16(ICOMPO-1+7)(1:8)//'.CPRI','L',ISICOM)
            NBGFMX=ZI(ISICOM+2)
            MATERI=ZK24(ISDCOM+6*NBGFMX)(1:8)
         ELSE
            ITYPE=0
            A   = ZERO
            A2  = ZERO
         ENDIF
      ELSE
C       -- POUTRES CLASSIQUES
         IF (OPTION(13:16).NE.'1D1D'.AND..NOT.LRHO) THEN
            CALL RCVALB(FAMI,1,1,'+',ZI(LMATER),MATERI,'ELAS',
     &                  NBPAR,NOMPAR,VALPAR,
     &                  NBRES,NOMRES,VALRES,CODRES,1)
C
            E      = VALRES(1)
            NU     = VALRES(2)
            G      = E / (2.D0*(1.D0+NU))
         ENDIF
C       -- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS
         CALL JEVECH ('PCAGNPO', 'L',LSECT)
         LSECT = LSECT-1
         ITYPE =  NINT(ZR(LSECT+23))
C
C       --- SECTION INITIALE ---
         A     =  ZR(LSECT+1)
         XIY   =  ZR(LSECT+2)
         XIZ   =  ZR(LSECT+3)
         ALFAY =  ZR(LSECT+4)
         ALFAZ =  ZR(LSECT+5)
C        EY    = -ZR(LSECT+6)
C        EZ    = -ZR(LSECT+7)
         XJX   =  ZR(LSECT+8)
C
C       --- SECTION FINALE ---
         LSECT2 = LSECT + 11
         A2     = ZR(LSECT2+1)
         XIY2   = ZR(LSECT2+2)
         XIZ2   = ZR(LSECT2+3)
         ALFAY2 = ZR(LSECT2+4)
         ALFAZ2 = ZR(LSECT2+5)
         EY     = -(ZR(LSECT+6)+ZR(LSECT2+6))/2.D0
         EZ     = -(ZR(LSECT+7)+ZR(LSECT2+7))/2.D0
         XJX2   = ZR(LSECT2+8)
      ENDIF
C
C     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
      CALL JEVECH ('PGEOMER', 'L',LX)
      LX = LX - 1
      XL = SQRT( (ZR(LX+4)-ZR(LX+1))**2 +
     &           (ZR(LX+5)-ZR(LX+2))**2 +
     &           (ZR(LX+6)-ZR(LX+3))**2 )
      IF( XL .EQ. ZERO ) THEN
        CALL TECAEL(IADZI,IAZK24)
        NOMAIL = ZK24(IAZK24-1+3)(1:8)
        CALL U2MESK('F','ELEMENTS2_43',1,NOMAIL)
      ENDIF
C
C     --- RECUPERATION DES ORIENTATIONS ---
      CALL JEVECH ('PCAORIE', 'L',LORIEN)
C
      IF     ( NOMTE .EQ. 'MECA_POU_D_E' )  THEN
C        --- POUTRE DROITE D'EULER A 6 DDL ---
         ISTRUC = 1
         NNO = 2
         NC  = 6
         ALFAY  = 0.D0
         ALFAZ  = 0.D0
         ALFAY2 = 0.D0
         ALFAZ2 = 0.D0
         CALL MATROT ( ZR(LORIEN) , PGL )
      ELSEIF ( NOMTE .EQ. 'MECA_POU_D_T' ) THEN
C        --- POUTRE DROITE DE TIMOSKENKO A 6 DDL ---
         ISTRUC = 1
         NNO = 2
         NC  = 6
         CALL MATROT ( ZR(LORIEN) , PGL )

      ELSEIF ( NOMTE .EQ. 'MECA_POU_C_T' ) THEN
C        --- POUTRE COURBE DE TIMOSKENKO A 6 DDL ---
         ISTRUC = 1
         NNO = 1
         NC  = 6
         CALL JEVECH ('PCAARPO', 'L',LRCOU)
         RAD    = ZR(LRCOU)
         ANGARC = ZR(LRCOU+1)
         XFL    = ZR(LRCOU+2)
         XFLY   = XFL
         XFLZ   = XFL
         IF (XFL.EQ.0.D0) THEN
            XFLY   = ZR(LRCOU+4)
            XFLZ   = ZR(LRCOU+6)
         ENDIF
         ANGS2  = TRIGOM('ASIN', XL / ( 2.D0 * RAD ) )
         ANG    = ANGS2 * 2.D0
         XL     = RAD * ANG
         XIY    =  XIY / XFLY
         XIZ    =  XIZ / XFLZ
         XIY2   = XIY2 / XFLY
         XIZ2   = XIZ2 / XFLZ
         CALL MATRO2 ( ZR(LORIEN), ANGARC , ANGS2 , PGL1 , PGL2 )

      ELSEIF ( NOMTE .EQ. 'MECA_POU_D_EM' ) THEN
C        --- POUTRE MULTIFIBRE DROITE D'EULER A 6 DDL ---
         IF(LRHO) THEN
            ITYPE=0
         ELSE
            ITYPE = 20
         ENDIF
         NNO = 2
         NC  = 6
         CALL MATROT ( ZR(LORIEN) , PGL )

      ELSEIF ( NOMTE .EQ. 'MECA_POU_D_TG' ) THEN
C        --- POUTRE DROITE DE TIMOSKENKO A 7 DDL (GAUCHISSEMENT)---
         ITYPE = 30
         NNO = 2
         NC  = 7
         CALL MATROT ( ZR(LORIEN) , PGL )
         A2    = A
         EY    = -ZR(LSECT+6)
         EZ    = -ZR(LSECT+7)
         XJG   =  ZR(LSECT+12)

      ELSEIF ( NOMTE .EQ. 'MECA_POU_D_TGM' ) THEN
C        --- POUTRE DROITE DE TIMOSKENKO A 7 DDL ---
C           (GAUCHISSEMENT, MULTIFIBRES)---
         ITYPE = 30
         NNO = 2
         NC  = 7
         CALL MATROT ( ZR(LORIEN) , PGL )
         EY    = -ZR(LSECT+6)
         EZ    = -ZR(LSECT+7)
         XJG   =  ZR(LSECT+12)

      ELSE
         CH16 = NOMTE
         CALL U2MESK('F','ELEMENTS2_42',1,CH16)
      ENDIF
C
C     --- PASSAGE DU REPERE LOCAL AU REPERE GLOBAL ---
      IF ( OPTION .EQ. 'CHAR_MECA_FC1D1D' ) THEN
         CALL JEVECH ('PVECTUC','E',LVECT)
         IF (NOMTE.EQ.'MECA_POU_D_TG' .OR.
     &       NOMTE.EQ.'MECA_POU_D_TGM') THEN
             CALL PTFOCP(ITYPE,OPTION,NOMTE,XL,RAD,
     &                  ANGS2,NNO,6,PGL,PGL1,PGL2, FR, FI )
               CALL UTPVLG ( NNO, 6, PGL, FR, FGR )
               CALL UTPVLG ( NNO, 6, PGL, FI, FGI )
                DO 25 I = 1,6
                  ZC(LVECT+I-1)   = DCMPLX(FGR(I),FGI(I))
                  ZC(LVECT+I-1+7) = DCMPLX(FGR(I+6),FGI(I+6))
 25             CONTINUE
                ZC(LVECT+7-1)  = DCMPLX(0.D0,0.D0)
                ZC(LVECT+14-1) = DCMPLX(0.D0,0.D0)
         ELSE
            CALL PTFOCP(ITYPE,OPTION,NOMTE,XL,RAD,
     &                  ANGS2,NNO,NC,PGL,PGL1,PGL2, FR, FI )
         IF ( NOMTE .EQ. 'MECA_POU_C_T' ) THEN
            CALL UTPVLG ( NNO, NC, PGL1, FR, FGR )
            CALL UTPVLG ( NNO, NC, PGL2, FR(7), FGR(7) )
            CALL UTPVLG ( NNO, NC, PGL1, FI, FGI )
            CALL UTPVLG ( NNO, NC, PGL2, FI(7), FGI(7) )
         ELSE
            CALL UTPVLG ( NNO, NC, PGL, FR, FGR )
            CALL UTPVLG ( NNO, NC, PGL, FI, FGI )
         ENDIF
         DO 15 I = 1,12
            ZC(LVECT+I-1) = DCMPLX(FGR(I),FGI(I))
 15      CONTINUE
         ENDIF
      ELSE IF( OPTION.EQ.'CHAR_MECA_FR1D1D' .OR.
     &         OPTION.EQ.'CHAR_MECA_FF1D1D' .OR.
     &         OPTION.EQ.'CHAR_MECA_SR1D1D' .OR.
     &         OPTION.EQ.'CHAR_MECA_SF1D1D' .OR.
     &         OPTION.EQ.'CHAR_MECA_ROTA_R' .OR.
     &         OPTION.EQ.'CHAR_MECA_PESA_R' ) THEN
         IF(NOMTE.EQ.'MECA_POU_D_TG' .OR.
     &      NOMTE.EQ.'MECA_POU_D_TGM' ) THEN
            CALL PTFORP(0,OPTION,NOMTE,A,A2,XL,RAD,
     &               ANGS2,1,NNO,6,PGL,PGL1,PGL2, FER, FEI )
         ELSE
            CALL PTFORP(ITYPE,OPTION,NOMTE,A,A2,XL,RAD,
     &               ANGS2,1,NNO,NC,PGL,PGL1,PGL2, FER, FEI )
         ENDIF
         DO 20 I = 1,6
            FFE(I)    = FER(I)
            FFE(I+NC) = FER(I+6)
 20      CONTINUE
         IF (NC.EQ.7) THEN
            FFE(7) = 0.D0
            FFE(14) = 0.D0
         ENDIF

      ELSE
         IF ( ITYPE .EQ. 0 ) THEN
C        --- POUTRE DROITE A SECTION CONSTANTE ---
            CALL PTKA01(MATK,E,A,XL,
     &                  XIY,XIZ,XJX,G,ALFAY,ALFAZ,EY,EZ,ISTRUC)
         ELSE IF ( ITYPE .EQ. 1 .OR. ITYPE .EQ. 2 ) THEN
C        --- POUTRE DROITE A SECTION VARIABLE (TYPE 1 OU 2) ---
            CALL PTKA02(ITYPE,MATK,E,A,A2,XL,
     &                  XIY,XIY2,XIZ,XIZ2,XJX,XJX2,G,
     &                  ALFAY,ALFAY2,ALFAZ,ALFAZ2,EY,EZ,ISTRUC)
         ELSE IF  ( ITYPE .EQ. 10 ) THEN
C        --- POUTRE COURBE A SECTION CONSTANTE ---
            CALL PTKA10(MATK,E,A,
     &                  XIY,XIZ,XJX,G,ALFAY,ALFAZ,RAD,ANG,ISTRUC)
         ELSE IF  ( ITYPE .EQ. 20 ) THEN
C        --- POUTRE DROITE MULTIFIBRE A SECTION CONSTANTE ---
            CALL PMFK01(CARSEC,0.D0,XL,MATK)
         ELSE IF  ( ITYPE .EQ. 30 ) THEN
C        --- POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT, MULTIFIBRES) --
            CALL PTKA21(MATK,E,A,XL,XIY,XIZ,XJX,XJG,G,ALFAY,ALFAZ,EY,EZ)
         ENDIF
C
C     --- REMPLISSAGE DE LA MATRICE CARREE ---
         IND = 0
         DO 30 I = 1, NC*2
            DE(I) = 0.D0
            DO 40 J = 1, I-1
               IND = IND + 1
               BSM(I,J) = MATK(IND)
               BSM(J,I) = MATK(IND)
40          CONTINUE
            IND = IND + 1
            BSM(I,I) = MATK(IND)
30       CONTINUE
C
         IF ( OPTION.EQ.'CHAR_MECA_TEMP_R' ) THEN

C        --- CALCUL DU DEPLACEMENT LOCAL INDUIT PAR L'ELEVATION DE TEMP.
            CALL VERIFM(FAMI,NPG,1,'+',ZI(LMATER),'ELAS',1,F,IRET)

         ELSEIF ( OPTION.EQ.'CHAR_MECA_SECH_R' ) THEN

C        --- CALCUL DU DEPLACEMENT LOCAL INDUIT PAR L'ELEVATION DE TEMP.
C        TEMPERATURE EFFECTIVE
            CALL MOYTEM(FAMI,NPG,1,'+',TEMP,IRET)
C ----   RECUPERATION DU CHAMP DU SECHAGE SUR L'ELEMENT
            SECH=0.D0
            DO 50 IGAU=1,NPG
               CALL RCVARC(' ','SECH','+','RIGI',IGAU,1,
     &                     SECHG(IGAU),IRET)
               IF (IRET.EQ.1) SECHG(IGAU)=0.D0
               SECH=SECH+SECHG(IGAU)/NPG
50          CONTINUE
C ----   RECUPERATION DU SECHAGE DE REFERENCE
            CALL RCVARC(' ','SECH','REF','RIGI',1,1,SREF,IRET)
            IF (IRET.EQ.1) SREF=0.D0
C ----   RECUPERATION DE L'INSTANT
            CALL TECACH('ONN','PTEMPSR',1,ITEMPS,IRET)
            IF (ITEMPS.NE.0) THEN
               INSTAN = ZR(ITEMPS)
            ELSE
               INSTAN = 0.D0
            ENDIF
            NOMPAR(1) = 'TEMP'
            VALPAR(1) = TEMP
            NOMPAR(2) = 'INST'
            VALPAR(2) = INSTAN
            NOMPAR(3) = 'SECH'
            VALPAR(3) = SECH
C ----   INTERPOLATION DE K_DESSIC EN FONCTION DE LA TEMPERATURE
C        DE L HYDRATATION OU DU SECHAGE
C        ----------------------------------------------------------
            CALL RCVALB(FAMI,1,1,'+',ZI(LMATER),MATERI,'ELAS',
     &                  3,NOMPAR,VALPAR,1,
     &                  'K_DESSIC',KDESSI, CODRES, 0)
            IF (CODRES(1).NE.0) KDESSI=0.D0
            F = -KDESSI*(SREF-SECH)

         ELSEIF ( OPTION.EQ.'CHAR_MECA_HYDR_R' ) THEN
C
C        TEMPERATURE EFFECTIVE
            CALL MOYTEM(FAMI,NPG,1,'+',TEMP,IRET)
C ----    RECUPERATION DU CHAMP D HYDRATATION SUR L'ELEMENT
            HYDR=0.D0
            DO 60 IGAU=1,NPG
               CALL RCVARC(' ','HYDR','+','RIGI',IGAU,1,
     &                     HYDRG(IGAU),IRET)
               IF (IRET.EQ.1) HYDRG(IGAU)=0.D0
               HYDR=HYDR+HYDRG(IGAU)/NPG
 60         CONTINUE
C ----   RECUPERATION DE L'INSTANT
            CALL TECACH('ONN','PTEMPSR',1,ITEMPS,IRET)
            IF (ITEMPS.NE.0) THEN
               INSTAN = ZR(ITEMPS)
            ELSE
               INSTAN = 0.D0
            ENDIF
            NOMPAR(1) = 'TEMP'
            VALPAR(1) = TEMP
            NOMPAR(2) = 'INST'
            VALPAR(2) = INSTAN
            NOMPAR(3) = 'HYDR'
            VALPAR(3) = HYDR
C ----   INTERPOLATION DE B_ENDOGE EN FONCTION DE LA TEMPERATURE
C        ET DE L HYDRATATION
            CALL RCVALB(FAMI,1,1,'+',ZI(LMATER),MATERI,'ELAS',
     &          3,NOMPAR,VALPAR,1,
     &          'B_ENDOGE',KENDOG, CODRES, 0)
            IF (CODRES(1).NE.0) KENDOG=0.D0
C        DEPLACEMENT INDUIT PAR L'HYDRATATION
            F = -KENDOG*HYDR

         ELSE
            CH16 = OPTION
            CALL U2MESK('F','ELEMENTS2_47',1,CH16)
         ENDIF

         IF ( ITYPE .EQ. 10 ) THEN
            ALONG  = 2.D0 * RAD * F * SIN(ANGS2)
            DE(1) = -ALONG * COS(ANGS2)
            DE(2) =  ALONG * SIN(ANGS2)
            DE(7) = -DE(1)
            DE(8) =  DE(2)
         ELSEIF ( ITYPE .EQ.30 ) THEN
            DE(1) = -F * XL
            DE(8) = -DE(1)
         ELSE
            DE(1) = -F * XL
            DE(7) = -DE(1)
         ENDIF
C
C        --- CALCUL DES FORCES INDUITES ---
         DO 70 I=1,NC
            FFE(I)   = 0.D0
            FFE(I+NC) = 0.D0
            DO 80 J=1,NC
               FFE(I)    = FFE(I)    + BSM(I,J)      * DE(J)
               FFE(I+NC) = FFE(I+NC) + BSM(I+NC,J+NC) * DE(J+NC)
   80       CONTINUE
   70    CONTINUE
      ENDIF
C
      IF(OPTION.NE.'CHAR_MECA_FC1D1D') THEN
         CALL JEVECH ('PVECTUR','E',LVECT)
C      --- MATRICE DE PASSAGE DU REPERE GLOBAL AU REPERE LOCAL: PGL
         IF ( ITYPE .EQ. 10 ) THEN
            CALL UTPVLG ( NNO, NC, PGL1, FFE, ZR(LVECT) )
            CALL UTPVLG ( NNO, NC, PGL2, FFE(7), ZR(LVECT+6) )
         ELSE
            CALL UTPVLG ( NNO, NC, PGL, FFE, ZR(LVECT) )
         ENDIF
      ENDIF
C
      CALL JEDEMA()
      END

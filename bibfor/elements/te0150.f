      SUBROUTINE TE0150(OPTION,NOMTE)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     OPTION,NOMTE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/04/2006   AUTEUR JMBHH01 J.M.PROIX 
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
C     CALCULE LE CHARGEMENT INDUIT PAR UNE ELEVATION UNIFORME DE
C     TEMPERATURE DANS LES POUTRES D'EULER ET DE TIMOSHENKO
C     ------------------------------------------------------------------
C IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
C        'CHAR_MECA_TEMP_R'  :
C IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
C        'MECA_POU_D_E' : POUTRE DROITE D'EULER       (SECTION VARIABLE)
C        'MECA_POU_D_T' : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
C        'MECA_POU_C_T' : POUTRE COURBE DE TIMOSHENKO(SECTION CONSTANTE)
C        'MECA_POU_D_EM': POUTRE DROITE MULTIFIBRE D EULER (SECT. CONST)
C     ------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
      PARAMETER                 (NBRES=3)
      REAL*8       VALPAR(3),VALRES(NBRES)
      CHARACTER*2         CODRES(NBRES)
      CHARACTER*8  NOMPAR(3),NOMRES(NBRES)
      CHARACTER*16 CH16
      REAL*8       E   ,  NU  , G    ,  ALPHA
      REAL*8       A   ,  XIY ,  XIZ ,  ALFAY ,  ALFAZ ,  XJX ,  EZ,  EY
      REAL*8       A2  ,  XIY2,  XIZ2,  ALFAY2,  ALFAZ2,  XJX2,  XL
      REAL*8       ANG ,  RAD ,ANGARC,   ANGS2,   ALONG,   XFLY, XFLZ
      REAL*8       PGL(3,3), PGL1(3,3), PGL2(3,3), DE(12), FFE(12)
      REAL*8       BSM(12,12),MATK(78) ,CARSEC(6)
      
      REAL*8       KENDOG,KDESSI,SECH,HYDR,INSTAN
      INTEGER NBFIB,NCARFI,JACF,JNF
C
      DATA NOMRES / 'E', 'NU', 'ALPHA'/
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
C     --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
      CALL JEVECH ('PMATERC', 'L', LMATER)
      DO 10 I=1, NBRES
         VALRES(I) = 0.D0
 10   CONTINUE
C
      CALL TECACH('ONN','PTEMPER',1,ITEMPE,IRET)
      IF ( IRET .NE. 0 ) THEN
         NBPAR  = 0
         NOMPAR(1) = ' '
         VALPAR(1) = 0.D0
      ELSE
         NBPAR  = 1
         NOMPAR(1) = 'TEMP'
         VALPAR(1) = 0.5D0*(ZR(ITEMPE) + ZR(ITEMPE+1))
      ENDIF
C
      CALL RCVALA(ZI(LMATER),' ','ELAS',NBPAR,NOMPAR,VALPAR,NBRES,
     +                                  NOMRES,VALRES,CODRES,'FM')
C
      E      = VALRES(1)
      NU     = VALRES(2)
      G      = E / (2.D0*(1.D0+NU))
      ALPHA  = VALRES(3)

      IF(NOMTE.EQ.'MECA_POU_D_EM')THEN
C       -- POUTRES MULTIFIBRES
C       -- RECUPERATION DES CARACTERISTIQUES DES FIBRES :
        CALL JEVECH('PNBSP_I','L',JNF)
        NBFIB = ZI(JNF)
        CALL JEVECH('PFIBRES','L',JACF)
        NCARFI = 3
        CALL PMFITG(NBFIB,NCARFI,ZR(JACF),CARSEC)
        DO 53 I=1,6
          CARSEC(I)=E*CARSEC(I)
  53    CONTINUE
      ELSE
C       -- POUTRES CLASSIQUES
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
C       EY    = -ZR(LSECT+6)
C       EZ    = -ZR(LSECT+7)
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
      XL = SQRT( (ZR(LX+4)-ZR(LX+1))**2
     +            + (ZR(LX+5)-ZR(LX+2))**2 + (ZR(LX+6)-ZR(LX+3))**2 )
      IF( XL .EQ. 0.D0 ) THEN
         CH16 = ' ?????????'
         CALL UTMESS('F','ELEMENTS DE POUTRE (TE0150)',
     +                  'NOEUDS CONFONDUS POUR UN ELEMENT: '//CH16(:8))
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
         ITYPE = 20
         NNO = 2
         NC  = 6
         CALL MATROT ( ZR(LORIEN) , PGL )
      ELSE
         CH16 = NOMTE
         CALL UTMESS('F','ELEMENTS DE POUTRE (TE0150)',
     &                   '"'//CH16//'"    NOM D''ELEMENT INCONNU.')
      ENDIF
C
      IF ( ITYPE .EQ. 0 ) THEN
C        --- POUTRE DROITE A SECTION CONSTANTE ---
         CALL PTKA01(MATK,E,A,XL,
     &                        XIY,XIZ,XJX,G,ALFAY,ALFAZ,EY,EZ,ISTRUC)
      ELSE IF ( ITYPE .EQ. 1 .OR. ITYPE .EQ. 2 ) THEN
C        --- POUTRE DROITE A SECTION VARIABLE (TYPE 1 OU 2) ---
         CALL PTKA02(ITYPE,MATK,E,A,A2,XL,
     &               XIY,XIY2,XIZ,XIZ2,XJX,XJX2,G,
     &               ALFAY,ALFAY2,ALFAZ,ALFAZ2,EY,EZ,ISTRUC)
      ELSE IF  ( ITYPE .EQ. 10 ) THEN
C        --- POUTRE COURBE A SECTION CONSTANTE ---
         CALL PTKA10(MATK,E,A,
     &               XIY,XIZ,XJX,G,ALFAY,ALFAZ,RAD,ANG,ISTRUC)
      ELSE IF  ( ITYPE .EQ. 20 ) THEN
C        --- POUTRE DROITE MULTIFIBRE A SECTION CONSTANTE ---
         CALL PMFK01(CARSEC,0.D0,XL,MATK)
      ENDIF
C
C     --- REMPLISSAGE DE LA MATRICE CARREE ---
      IND = 0
      DO 20 I = 1, 12
         DE(I) = 0.D0
         DO 25 J = 1, I-1
            IND = IND + 1
            BSM(I,J) = MATK(IND)
            BSM(J,I) = MATK(IND)
  25     CONTINUE
         IND = IND + 1
         BSM(I,I) = MATK(IND)
  20  CONTINUE
C
      IF ( OPTION.EQ.'CHAR_MECA_TEMP_R' ) THEN
      
C        --- CALCUL DU DEPLACEMENT LOCAL INDUIT PAR L'ELEVATION DE TEMP.
C        TEMPERATURE DE REFERENCE
         CALL JEVECH('PTEREF','L',LTREF)
C        TEMPERATURE EFFECTIVE
         CALL JEVECH('PTEMPER','L',LTEMP)
         TEMP = 0.5D0*(ZR(LTEMP)+ZR(LTEMP+1)) - ZR(LTREF)
         F = ALPHA * TEMP

      ELSEIF ( OPTION.EQ.'CHAR_MECA_SECH_R' ) THEN
      
C        --- CALCUL DU DEPLACEMENT LOCAL INDUIT PAR L'ELEVATION DE TEMP.
C        TEMPERATURE EFFECTIVE
         CALL JEVECH('PTEMPER','L',LTEMP)
         TEMP = 0.5D0*(ZR(LTEMP)+ZR(LTEMP+1))
C ----   RECUPERATION DU CHAMP DU SECHAGE SUR L'ELEMENT
         CALL JEVECH('PSECHER','L',ISECH)
C ----   RECUPERATION DU SECHAGE DE REFERENCE
         CALL JEVECH('PSECREF','L',ISREF)
         SREF=ZR(ISREF)
         SECH = 0.5D0*(ZR(ISECH)+ZR(ISECH+1))
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
C ----   INTERPOLATION DE K_DESSICCA EN FONCTION DE LA TEMPERATURE
C        DE L HYDRATATION OU DU SECHAGE
C        ----------------------------------------------------------
         CALL RCVALA(ZI(LMATER),' ','ELAS',3,NOMPAR,VALPAR,1,
     &       'K_DESSIC',KDESSI, CODRES, ' ' )
         IF (CODRES(1).NE.'OK') KDESSI=0.D0
         F = -KDESSI*(SREF-SECH) 

      ELSEIF ( OPTION.EQ.'CHAR_MECA_HYDR_R' ) THEN
C
C        TEMPERATURE EFFECTIVE
         CALL JEVECH('PTEMPER','L',LTEMP)
         TEMP = 0.5D0*(ZR(LTEMP)+ZR(LTEMP+1))   
C ----    RECUPERATION DU CHAMP D HYDRATATION SUR L'ELEMENT
         CALL JEVECH('PHYDRER','L',IHYDR)
         HYDR = ZR(IHYDR)
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
         CALL RCVALA(ZI(LMATER),' ','ELAS',3,NOMPAR,VALPAR,1,
     &       'B_ENDOGE',KENDOG, CODRES, ' ' )
         IF (CODRES(1).NE.'OK') KENDOG=0.D0
C        DEPLACEMENT INDUIT PAR L'HYDRATATION
         F = -KENDOG*HYDR

      ELSE
         CH16 = OPTION
         CALL UTMESS('F','ELEMENTS DE POUTRE (TE0150)',
     &                   'L''OPTION "'//CH16//'" EST INCONNUE')
      ENDIF
      
      IF ( ITYPE .NE. 10 ) THEN
         DE(1) = -F * XL
         DE(7) = -DE(1)
      ELSE
         ALONG  = 2.D0 * RAD * F * SIN(ANGS2)
         DE(1) = -ALONG * COS(ANGS2)
         DE(2) =  ALONG * SIN(ANGS2)
         DE(7) = -DE(1)
         DE(8) =  DE(2)
      ENDIF
C
C        --- CALCUL DES FORCES INDUITES ---
      DO 35 I=1,6
         FFE(I)   = 0.D0
         FFE(I+6) = 0.D0
         DO 30 J=1,6
            FFE(I)   = FFE(I)   + BSM(I,J)     * DE(J)
            FFE(I+6) = FFE(I+6) + BSM(I+6,J+6) * DE(J+6)
   30    CONTINUE
   35 CONTINUE
C
      CALL JEVECH ('PVECTUR','E',LVECT)
C
C      --- MATRICE DE PASSAGE DU REPERE GLOBAL AU REPERE LOCAL: PGL
      IF ( ITYPE .EQ. 10 ) THEN
         CALL UTPVLG ( NNO, NC, PGL1, FFE, ZR(LVECT) )
         CALL UTPVLG ( NNO, NC, PGL2, FFE(7), ZR(LVECT+6) )
      ELSE
         CALL UTPVLG ( NNO, NC, PGL, FFE, ZR(LVECT) )
      ENDIF
C
      CALL JEDEMA()
      END

      SUBROUTINE  EPSTMC(MODELI, TEMPE, TREF, HYDR, SECH, SREF, INSTAN,
     &                    MATER,OPTION, EPSTH)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 17/05/2004   AUTEUR ROMEO R.FERNANDES 
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
C.======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
C      EPSTMC :   CALCUL DES DEFORMATIONS THERMIQUES / HYDRIQUE (RETRAIT
C                 ENDOGENE) / DE SECHAGE (RETRAIT DE DESSICCATION)
C                 POUR LES ELEMENTS ISOPARAMETRIQUES
C
C   ARGUMENT        E/S  TYPE         ROLE
C    MODELI         IN     K8       MODELISATION (AXI, FOURIER,...)
C    TEMPE          IN     R        TEMPERATURE AU POINT D'INTEGRATION
C    TREF           IN     R        TEMPERATURE DE REFERENCE
C    HYDR           IN     R        HYDRATATION AU POINT D'INTEGRATION
C    SECH           IN     R        SECHAGE AU POINT D'INTEGRATION
C    SREF           IN     R        SECHAGE DE REFERENCE
C    INSTAN         IN     R        INSTANT DE CALCUL (0 PAR DEFAUT)
C    MATER          IN     I        MATERIAU
C    OPTION         IN     K16      OPTION DE CALCUL
C    EPSTH(6)       IN     R        VECTEUR DES DEFORMATIONS THERMIQUES
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
           CHARACTER*8  MODELI
           CHARACTER*16 OPTION
           REAL*8       EPSTH(6), INSTAN, HYDR, SECH
C -----  VARIABLES LOCALES
           PARAMETER (NBRES = 3)
C
           CHARACTER*2  BL2, FB2, CODRET(NBRES)
           CHARACTER*8  NOMRES(NBRES), NOMPAR(4)
           CHARACTER*16 PHENOM
C
           REAL*8 VALRES(NBRES), VALPAR(4), BENDOG, KDESSI
C
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C ---- INITIALISATIONS
C      ---------------
      ZERO   = 0.0D0
      BL2    = '  '
C
      NOMPAR(1) = 'TEMP'
      NOMPAR(2) = 'INST'
      VALPAR(1) = TEMPE
      VALPAR(2) = INSTAN
      NOMPAR(3) = 'HYDR'
      NOMPAR(4) = 'SECH'
      VALPAR(3) = HYDR
      VALPAR(4) = SECH
C
      DO 10 I = 1, 6
         EPSTH(I)  = ZERO
 10   CONTINUE
C
C ---- ------------------------------------------------------------
C ---- CALCUL DES DEFORMATIONS HYDRIQUES (OPTION CHAR_MECA_HYDR_R)
C ---- ------------------------------------------------------------
C
      IF (OPTION(11:14).EQ.'HYDR') THEN
         IF (HYDR.NE.0.D0) THEN
C
            PHENOM = 'ELAS'
            NOMRES(1) = 'B_ENDOGE'
            NBV = 1
C
C ----      INTERPOLATION DE B_ENDOGENE EN FONCTION DE LA TEMPERATURE
C           DE L HYDRATATION OU DU SECHAGE
C           ----------------------------------------------------------
            CALL RCVALA(MATER,' ',PHENOM,4,NOMPAR,VALPAR,NBV,NOMRES,
     +                VALRES,  CODRET, BL2 )
C
            IF (CODRET(1).EQ.'OK') THEN
C
                BENDOG = VALRES(1)
C
                EPSTH(1) = - BENDOG*(HYDR)
                EPSTH(2) = - BENDOG*(HYDR)
C
                IF (MODELI(1:2).EQ.'CA'.OR.MODELI(1:2).EQ.'DP'.OR.
     +              MODELI(1:2).EQ.'FO'.OR.MODELI(1:2).EQ.'AX') THEN
                    EPSTH(3) = - BENDOG*(HYDR)
                ENDIF
C
            ELSE
C
               CALL UTMESS('I','EPSTMC','LA NATURE DU MATERIAU '//
     +           PHENOM// ' NECESSITE LA DEFINITION DU COEFFICIENT '//
     +                   ' B_ENDOGE DANS DEFI_MATERIAU.')
C
            ENDIF
         ENDIF
C
C ---- ---------------------------------------------------------------
C ---- CALCUL DES DEFORMATIONS DU AU SECAHGE (OPTION CHAR_MECA_SECH_R)
C ---- ---------------------------------------------------------------
      ELSEIF (OPTION(11:14).EQ.'SECH' ) THEN

            PHENOM = 'ELAS'
            NOMRES(1) = 'K_DESSIC'
            NBV = 1
C
C ----      INTERPOLATION DE K_DESSICCA EN FONCTION DE LA TEMPERATURE
C           DE L HYDRATATION OU DU SECHAGE
C           ----------------------------------------------------------
            CALL RCVALA(MATER,' ',PHENOM,4,NOMPAR,VALPAR,NBV,NOMRES,
     +                 VALRES, CODRET, BL2 )
C
            IF (CODRET(1).NE.'OK') VALRES(1)=0.D0
C
            KDESSI = VALRES(1)
C
            EPSTH(1) = - KDESSI*(SREF-SECH)
            EPSTH(2) = - KDESSI*(SREF-SECH)
C
            IF (MODELI(1:2).EQ.'CA'.OR.MODELI(1:2).EQ.'DP'.OR.
     +          MODELI(1:2).EQ.'FO'.OR.MODELI(1:2).EQ.'AX') THEN
                EPSTH(3) = - KDESSI*(SREF-SECH)
            ENDIF

C ---- ------------------------------------------------------------
C ---- CALCUL DES DEFORMATIONS THERMIQUES (OPTION CHAR_MECA_TEMP_R)
C ---- ------------------------------------------------------------
      ELSE
C
C
C ---- RECUPERATION DU TYPE DU MATERIAU DANS PHENOM
C      --------------------------------------------
      CALL RCCOMA(MATER,'ELAS',PHENOM,CODRET)
C
C      ------------
C ---- CAS ISOTROPE
C      ------------
      IF (PHENOM.EQ.'ELAS') THEN
C
          NOMRES(1) = 'ALPHA'
          NBV = 1
C
C ----   INTERPOLATION DE ALPHA EN FONCTION DE LA TEMPERATURE
C        ----------------------------------------------------
          CALL RCVALA(MATER,' ',PHENOM,4,NOMPAR,VALPAR,NBV,NOMRES,
     +               VALRES, CODRET, BL2 )
C
          IF (CODRET(1).NE.'OK') VALRES(1) = ZERO
C
          ALPHA = VALRES(1)
C
          EPSTH(1) = ALPHA*(TEMPE-TREF)
          EPSTH(2) = ALPHA*(TEMPE-TREF)
C
          IF (MODELI(1:2).EQ.'CA'.OR.MODELI(1:2).EQ.'DP'.OR.
     +        MODELI(1:2).EQ.'FO'.OR.MODELI(1:2).EQ.'AX') THEN
                 EPSTH(3) = ALPHA*(TEMPE-TREF)
          ENDIF
C
C      --------------
C ---- CAS ORTHOTROPE
C      --------------
      ELSEIF (PHENOM.EQ.'ELAS_ORTH') THEN
C
          NOMRES(1)='ALPHA_L'
          NOMRES(2)='ALPHA_T'
          NOMRES(3)='ALPHA_N'
          NBV = 3
C
C ----   INTERPOLATION DES COEFFICIENTS EN FONCTION DE LA TEMPERATURE
C        ------------------------------------------------------------
          CALL RCVALA(MATER,' ',PHENOM,4,NOMPAR,VALPAR,NBV,NOMRES,
     +                VALRES,CODRET, BL2 )
C
          IF (CODRET(1).NE.'OK') VALRES(1) = ZERO
          IF (CODRET(2).NE.'OK') VALRES(2) = ZERO
          IF (CODRET(3).NE.'OK') VALRES(3) = ZERO
C
          ALPHAL = VALRES(1)
          ALPHAT = VALRES(2)
          ALPHAN = VALRES(3)
C
          EPSTH(1) = ALPHAL*(TEMPE-TREF)
          EPSTH(2) = ALPHAT*(TEMPE-TREF)
C
          IF (MODELI(1:2).EQ.'CA'.OR.MODELI(1:2).EQ.'DP'.OR.
     +        MODELI(1:2).EQ.'FO'.OR.MODELI(1:2).EQ.'AX') THEN
                 EPSTH(3) = ALPHAN*(TEMPE-TREF)
          ENDIF
C
C      -----------------------
C ---- CAS ISOTROPE-TRANSVERSE
C      -----------------------
      ELSEIF (PHENOM.EQ.'ELAS_ISTR') THEN
C
          NOMRES(1)='ALPHA_L'
          NOMRES(2)='ALPHA_N'
          NBV = 2
C
C ----   INTERPOLATION DES COEFFICIENTS EN FONCTION DE LA TEMPERATURE
C        ------------------------------------------------------------
          CALL RCVALA(MATER,' ',PHENOM,4,NOMPAR,VALPAR,NBV,NOMRES,
     +              VALRES,  CODRET, BL2 )
C
          IF (CODRET(1).NE.'OK') VALRES(1) = ZERO
          IF (CODRET(2).NE.'OK') VALRES(2) = ZERO
C
          ALPHAL = VALRES(1)
          ALPHAN = VALRES(2)
C
          EPSTH(1) = ALPHAL*(TEMPE-TREF)
          EPSTH(2) = ALPHAL*(TEMPE-TREF)
C
          IF (MODELI(1:2).EQ.'CA'.OR.MODELI(1:2).EQ.'DP'.OR.
     +        MODELI(1:2).EQ.'FO'.OR.MODELI(1:2).EQ.'AX') THEN
                 EPSTH(3) = ALPHAN*(TEMPE-TREF)
          ENDIF
      ELSE
          CALL UTMESS('F','EPSTMC','LA NATURE DU MATERIAU '//PHENOM//
     +                ' N''EST PAS TRAITEE, SEULES SONT CONSIDEREES '//
     +                'LES NATURES : ELAS, ELAS_ISTR, ELAS_ORTH .')
      ENDIF
      ENDIF
C
C.============================ FIN DE LA ROUTINE ======================
      END

        SUBROUTINE INSMAT ( MOD,    IMAT,   NMAT,   TEMPD,    TEMPF,
     1                      HYDRD,  HYDRF,  SECHD,  SECHF,   MATERD,
     2                      MATERF, MATCST, NDT,    NDI,    NR , NVI)
        IMPLICIT REAL*8 (A-H,O-Z)
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/04/2004   AUTEUR JMBHH01 J.M.PROIX 
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
C       ----------------------------------------------------------------
C   NADAI_BETON : RECUPERATION DU MATERIAU A T(TEMPD) ET T+DT(TEMPF)
C                 NB DE CMP DIRECTES/CISAILLEMENT , NB VAR. INTERNES
C                 MATER(*,1) = E , NU , ALPHA
C                 MATER(*,2) = F_C, F_T, CRIT_E_C
C                 EPSP_R_C, EPSP_P_C, EPSI_R_T, FAC_T_C
C                 VARIABLES INTERNES PLASTICITE:  P , EPS3
C       ----------------------------------------------------------------
C       IN  IMAT   :  ADRESSE DU MATERIAU CODE
C           MOD    :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION  DE MATER
C           TEMPD  :  TEMPERATURE  A T
C           TEMPF  :  TEMPERATURE  A T+DT
C           HYDRD  :   HYDRATATION A L'INSTANT PRECEDENT
C           HYDRF  :   HYDRATATION A L'INSTANT DU CALCUL
C           SECHD  :   SECHAGE A L'INSTANT PRECEDENT
C           SECHF  :   SECHAGE A L'INSTANT DU CALCUL
C       OUT MATERD :  COEFFICIENTS MATERIAU A T
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C                     MATER(*,1) = CARACTERISTIQUES   ELASTIQUES
C                     MATER(*,2) = CARACTERISTIQUES   PLASTIQUES
C           MATCST :  'OUI' SI  MATERIAU A T = MATERIAU A T+DT
C                     'NON' SINON
C           NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
C           NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
C           NR     :  NB DE COMPOSANTES SYSTEME NL
C           NVI    :  NB DE VARIABLES INTERNES
C       ----------------------------------------------------------------
        INTEGER         NMAT, IMAT, NDT , NDI  , NR , NVI
        REAL*8          MATERD(NMAT,2) , MATERF(NMAT,2) , TEMPD , TEMPF
        REAL*8          HYDRD , HYDRF , SECHD , SECHF
        REAL*8          VALPAD(3), VALPAF(3)
        REAL*8          EPSI
        CHARACTER*8     MOD , NOMC(12) , NOMPAR(3)
        CHARACTER*2     BL2, FB2, CERR(12)
        CHARACTER*3     MATCST
        DATA EPSI       /1.D-15/
C       ----------------------------------------------------------------
C
C -     NB DE COMPOSANTES / VARIABLES INTERNES -------------------------
C
        CALL INSNVI ( MOD , NDT , NDI , NR , NVI )
C
        BL2 = '  '
        FB2 = 'F '
C
C -     RECUPERATION MATERIAU -----------------------------------------
C
          NOMC(1) = 'E       '
          NOMC(2) = 'NU      '
          NOMC(3) = 'ALPHA   '
          NOMC(4) = 'B_ENDOGE'
          NOMC(5) = 'K_DESSIC'
          NOMC(6) = 'F_C     '
          NOMC(7) = 'F_T     '
          NOMC(8) = 'CRIT_E_C'
          NOMC(9) = 'EPSP_P_C'
          NOMC(10)= 'EPSP_R_C'
          NOMC(11)= 'EPSI_R_T'
          NOMC(12)= 'FAC_T_C '
C
          NOMPAR(1) = 'TEMP'
          NOMPAR(2) = 'HYDR'
          NOMPAR(3) = 'SECH'
          VALPAD(1) = TEMPD
          VALPAD(2) = HYDRD
          VALPAD(3) = SECHD
          VALPAF(1) = TEMPF
          VALPAF(2) = HYDRF
          VALPAF(3) = SECHF
C
C -     RECUPERATION MATERIAU A TEMPD (T)
C
          CALL RCVALA(IMAT,' ',    'ELAS',       3,  NOMPAR,VALPAD, 5,
     1                   NOMC(1),  MATERD(1,1),  CERR(1), BL2 )
          IF ( CERR(3) .NE. 'OK' ) MATERD(3,1) = 0.D0
          IF ( CERR(4) .NE. 'OK' ) MATERD(4,1) = 0.D0
          IF ( CERR(5) .NE. 'OK' ) MATERD(5,1) = 0.D0
          CALL RCVALA(IMAT,' ', 'NADAI_B',       3,  NOMPAR,VALPAD, 7,
     1                   NOMC(6),  MATERD(1,2),  CERR(6), FB2 )
          IF ( MATERD(1,2) .LT. 0.D0 )  THEN
           CALL UTMESS('S','NADAI_B','ERREUR'//
     1       'RESISTANCE F_C < 0  OU = 0 !')
          ENDIF
          IF ( MATERD(2,2) .LT. 0.D0 )  THEN
           CALL UTMESS('S','NADAI_B','ERREUR'//
     1       'F_T < 0 ! ')
          ENDIF
          IF ( MATERD(3,2) .GT. 1.D0) THEN
            CALL UTMESS('S','NADAI_B','ERREUR '//
     1     '- VALEUR DE CRIT_E_C SUPERIEURE  A  1')
          ENDIF
          IF ( MATERD(3,2) .LT. 0.D0) THEN
           CALL UTMESS('S','NADAI_B','ERREUR '//
     1     '- VALEUR DE CRIT_E_C NEGATIVE !!!!')
          ENDIF
          IF ( MATERD(4,2) .LT. 0.D0) THEN
           CALL UTMESS('S','NADAI_B','ERREUR '//
     1     '- VALEUR DE EPSP_P_C NEGATIVE !!!!')
          ENDIF
          IF ( MATERD(5,2) .LT. 0.D0) THEN
           CALL UTMESS('S','NADAI_B','ERREUR '//
     1     '- VALEUR DE EPSP_R_C NEGATIVE !!!!')
          ENDIF
          IF ( MATERD(6,2) .LT. 0.D0) THEN
           CALL UTMESS('S','NADAI_B','ERREUR '//
     1     '- VALEUR DE EPSI_R_T NEGATIVE !!!!')
          ENDIF
          IF (MATERD(7,2).LT.0.D0.OR.MATERD(7,2).GT.1.D0) THEN
           CALL UTMESS('S','NADAI_B','ERREUR '//
     1     '- VALEUR DE FAC_T_C NEGATIVE OU > 1   !!!!')
          ENDIF
C
C -     RECUPERATION MATERIAU A TEMPF (T+DT)
C
          CALL RCVALA(IMAT,' ',    'ELAS',       3,  NOMPAR,VALPAF, 5,
     1                   NOMC(1),  MATERF(1,1),  CERR(1), BL2 )
          IF ( CERR(3) .NE. 'OK' ) MATERF(3,1) = 0.D0
          IF ( CERR(4) .NE. 'OK' ) MATERF(4,1) = 0.D0
          IF ( CERR(5) .NE. 'OK' ) MATERF(5,1) = 0.D0
          CALL RCVALA(IMAT,' ', 'NADAI_B',       3,  NOMPAR,VALPAF, 7,
     1                   NOMC(6),  MATERF(1,2),  CERR(6), FB2 )
C
C -     MATERIAU CONSTANT ?
C
        MATCST = 'OUI'
        DO 30 I = 1,5
          IF ( ABS ( MATERD(I,1) - MATERF(I,1) ) .GT. EPSI )THEN
          MATCST = 'NON'
          GOTO 9999
        ENDIF
 30     CONTINUE
        DO 40 I = 1,6
          IF ( ABS ( MATERD(I,2) - MATERF(I,2) ) .GT. EPSI )THEN
          MATCST = 'NON'
          GOTO 9999
          ENDIF
 40     CONTINUE
C
 9999   CONTINUE
        END

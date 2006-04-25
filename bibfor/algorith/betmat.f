        SUBROUTINE BETMAT ( FAMI, KPG, KSP, MOD, IMAT, NMAT, TEMPD,
     2                      TEMPF,  SECHD,  SECHF,   MATERD,
     1                      MATERF, MATCST, NDT,    NDI,     NR , NVI )
        IMPLICIT REAL*8 (A-H,O-Z)
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 25/04/2006   AUTEUR CIBHHPD L.SALMONA 
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
C  BETON_DOUBLE_DP : RECUPERATION DU MATERIAU A T(TEMPD) ET T+DT(TEMPF)
C                    NB DE CMP DIRECTES/CISAILLEMENT , NB VAR. INTERNES
C                    MATER(*,1) = E , NU , ALPHA , B_ENDOGE , K_DESSIC
C                    MATER(*,2) = RESI_COMP_UNIAX, RESI_TRAC_UNIAX,
C                                 RESI_COEF_BIAX,
C                                 E_RUPT_COMP,     E_RUPT_TRAC
C                                 COMP_POST_PIC,   TRAC_POST_PIC'
C                                 COEF_POST_PIC
C                    VARIABLES INTERNES : EPPC, EPPC, THETA , E1, E2
C       ----------------------------------------------------------------
C       IN  IMAT   :  ADRESSE DU MATERIAU CODE
C           MOD    :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION  DE MATER
C           TEMPD  :  TEMPERATURE  A T
C           TEMPF  :  TEMPERATURE  A T+DT
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
        INTEGER         NMAT, NDT , NDI  , NR , NVI,KPG,KSP
        REAL*8          MATERD(NMAT,2) ,MATERF(NMAT,2) , TEMPD , TEMPF
        REAL*8          SECHD , SECHF
        REAL*8          VALPAD(2), VALPAF(2)
        REAL*8          EPSI , THETA
        CHARACTER*8     MOD, NOM , NOMC(14) , NOMPAR(2)
        CHARACTER*2     BL2, FB2, CERR(14)
        CHARACTER*3     MATCST
        CHARACTER*(*)   FAMI
        DATA EPSI       /1.D-15/
C       ----------------------------------------------------------------
C
C -     NB DE COMPOSANTES / VARIABLES INTERNES -------------------------
C
        CALL BETNVI ( MOD , NDT , NDI , NR , NVI )
C
        BL2 = '  '
        FB2 = 'F '
       CALL R8INIR(2*NMAT, 0.D0, MATERD, 1)
       CALL R8INIR(2*NMAT, 0.D0, MATERF, 1)
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
        NOMC(8) = 'COEF_BIA'
        NOMC(9) = 'ENER_COM'
        NOMC(10)= 'ENER_TRA'
        NOMC(11)= 'COEF_ELA'
        NOMC(12)= 'ECRO_COM'
        NOMC(13)= 'ECRO_TRA'
        NOMC(14)= 'LONG_CARA'
C
C -     TEMPERATURE MAXIMAL AU COURS DE L'HISTORIQUE DE CHARGEMENT
C -     THEMIQUE THETA (T+DT)
C
        THETA = TEMPF
        IF ( TEMPD .GT. TEMPF ) THETA = TEMPD
C
        NOMPAR(1) = 'TEMP'
        NOMPAR(2) = 'SECH'
        VALPAD(1) = TEMPD
        VALPAD(2) = SECHD
        VALPAF(1) = THETA
        VALPAF(2) = SECHF
C
C -     RECUPERATION MATERIAU A TEMPD (T)
C
        CALL RCVALB (FAMI,KPG,KSP,'-',IMAT,' ','ELAS', 2,NOMPAR,
     1               VALPAD,5,NOMC(1),MATERD(1,1),CERR(1),BL2 )
        IF ( CERR(3) .NE. 'OK' ) MATERD(3,1) = 0.D0
        IF ( CERR(4) .NE. 'OK' ) MATERD(4,1) = 0.D0
        IF ( CERR(5) .NE. 'OK' ) MATERD(5,1) = 0.D0
        CALL RCVALB (FAMI,KPG,KSP,'-',IMAT,' ','BETON_DOUBLE_DP',2,
     1               NOMPAR,VALPAD, 8,NOMC(6),  MATERD(1,2),  CERR(6),
     2               FB2 )
        CALL RCVALB (FAMI,KPG,KSP,'-',IMAT,' ','BETON_DOUBLE_DP',2,
     1               NOMPAR,VALPAD,1,NOMC(14),MATERD(9,2),CERR(14),
     2               BL2 )
        IF ( CERR(14).NE. 'OK' ) MATERD(9,2) = -1.D0
C
C -     RECUPERATION MATERIAU A TEMPF (T+DT)
C
        CALL RCVALB (FAMI,KPG,KSP,'+',IMAT,' ','ELAS',2,NOMPAR,
     1               VALPAF,5,NOMC(1),MATERF(1,1),CERR(1),BL2 )
        IF ( CERR(3) .NE. 'OK' ) MATERF(3,1) = 0.D0
        IF ( CERR(4) .NE. 'OK' ) MATERF(4,1) = 0.D0
        IF ( CERR(5) .NE. 'OK' ) MATERF(5,1) = 0.D0
        CALL RCVALB (FAMI,KPG,KSP,'+',IMAT,' ','BETON_DOUBLE_DP',2,
     1               NOMPAR,VALPAF,8,NOMC(6),MATERF(1,2),CERR(6),FB2)
        CALL RCVALB (FAMI,KPG,KSP,'+',IMAT,' ','BETON_DOUBLE_DP',2,
     1               NOMPAR,VALPAF,1,NOMC(14),MATERF(9,2),CERR(14),BL2)
        IF ( CERR(14).NE. 'OK' ) MATERF(9,2) = -1.D0
C
C -     VERIFICATION DU COEFFICIENT DE LIMITE D'ELASTICITE
C
        IF ( MATERD(6,2).LE.0.D0.OR.MATERD(6,2).GE.100.D0 ) THEN
           CALL UTMESS('F','BETMAT','POUR LA LOI BETON_DOUBLE_DP '
     &          //'LE PARAMETRE COEF_ELAS_COMP DOIT ETRE COMPRIS '
     &          //'ENTRE 0. ET 100.')
        ENDIF
C
C -     VERIFICATION DE LA LONGUEUR CARACTERISTIQUE
C
        IF ( CERR(14).EQ. 'OK' ) THEN
           IF ( MATERD(9,2).LE.0.D0 ) THEN
              CALL UTMESS('F','BETMAT','POUR LA LOI BETON_DOUBLE_DP '
     &             //'LE PARAMETRE LONG_CARA DOIT ETRE STRICTEMENT '
     &             //'POSITIF')
           ENDIF
        ENDIF
C
        MATERD(6,2) = MATERD(6,2) * 0.01D0
        MATERF(6,2) = MATERF(6,2) * 0.01D0
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
        DO 40 I = 1,9
          IF ( ABS ( MATERD(I,2) - MATERF(I,2) ) .GT. EPSI )THEN
          MATCST = 'NON'
          GOTO 9999
          ENDIF
 40     CONTINUE
C
 9999   CONTINUE
        END

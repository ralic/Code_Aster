        SUBROUTINE RSVMAT( MOD, IMAT, NMAT, TEMPD, TEMPF,
     &                     HYDRD,  HYDRF,  SECHD,  SECHF,   MATERD,
     &                     MATERF, MATCST, NDT, NDI , NR , NVI, VIND)
        IMPLICIT NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/09/2002   AUTEUR T2BAXJM R.MASSON 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C              SEE THE FILE "LICENSE.TERMS" FOR INFORMATION ON USAGE AND
C              REDISTRIBUTION OF THIS FILE.
C ======================================================================
C       ----------------------------------------------------------------
C       ROUSS_VISC : RECUPERATION DU MATERIAU A TEMPD ET TEMPF
C                    NB DE CMP DIRECTES/CISAILLEMENT , NB VAR. INTERNES
C                    MATER(*,1) = E , NU , ALPHA
C                    MATER(*,2) = D , SIG1 , PORO_INIT, PORO_CRIT
C                                            PORO_ACCE, PORO_LIMI
C                                            AN
C                    VARIABLES INTERNES : P , B , E
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
        INTEGER         I, IMAT, NMAT, NDT , NDI  , NR , NVI
        INTEGER         JPROL, JVALE, NBVALE
C
        REAL*8          MATERD(NMAT,2) ,MATERF(NMAT,2) , TEMPD , TEMPF
        REAL*8          EPSI, VIND(NVI), F0
        REAL*8          HYDRD , HYDRF , SECHD , SECHF
        REAL*8          VALPAD(3), VALPAF(3), R8BID
C
        CHARACTER*8     MOD, NOMC(15) , NOMPAR(3)
        CHARACTER*2     BL2, FB2, CERR(15)
        CHARACTER*3     MATCST
C
        DATA EPSI       /1.D-15/
C       ----------------------------------------------------------------
C
C -     NB DE COMPOSANTES / VARIABLES INTERNES -------------------------
C
        CALL RSLNVI ( MOD , NDT , NDI , NR , NVI )
C
C -   RECUPERATION MATERIAU ------------------------------------------
C
      BL2 = '  '
      FB2 = 'F '
C
          NOMC(1) = 'E        '
          NOMC(2) = 'NU       '
          NOMC(3) = 'ALPHA    '
          NOMC(4) = 'B_ENDOGE'
          NOMC(5) = 'K_DESSIC'
          NOMC(6) = 'D        '
          NOMC(7) = 'SIGM_1   '
          NOMC(8) = 'PORO_INIT'
          NOMC(9) = 'PORO_CRIT'
          NOMC(10)= 'PORO_ACCE'
          NOMC(11)= 'PORO_LIMI'
          NOMC(12)= 'D_SIGM_EPSI_NORM'
          NOMC(13)= 'SIGM_0'
          NOMC(14)= 'EPSI_0'
          NOMC(15)= 'M'
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
          CALL RCVALA (  IMAT,    'ELAS',       3,  NOMPAR,VALPAD, 5,
     1                   NOMC(1),  MATERD(1,1),  CERR(1), BL2 )
          IF ( CERR(3) .NE. 'OK' ) MATERD(3,1) = 0.D0
          IF ( CERR(4) .NE. 'OK' ) MATERD(4,1) = 0.D0
          IF ( CERR(5) .NE. 'OK' ) MATERD(5,1) = 0.D0
          CALL RCVALA (  IMAT,    'ROUSSELIER',  1, 'TEMP', TEMPD, 7,
     1                   NOMC(6),  MATERD(1,2),  CERR(6), FB2 )
          CALL RCVALA (  IMAT,    'ROUSS_VISC',  0, ' ', R8BID, 3,
     1                   NOMC(13),  MATERD(8,2),  CERR(13), FB2 )
C
C         RECUPERATION DE E(TEMPD) VIA LES COURBES DE TRACTION MONOTONES
C         SIG = F(EPS,TEMPD) ENTREES POINT PAR POINT  (MOT CLE TRACTION)
C         > ECRASEMENT DU E RECUPERE PAR MOT CLE ELAS
C
          CALL RCTRAC (IMAT,'TRACTION','SIGM',TEMPD,
     1                 JPROL,JVALE,NBVALE,MATERD(1,1))
C
C -     RECUPERATION MATERIAU A TEMPF (T+DT)
C
          CALL RCVALA (  IMAT,    'ELAS',       3,  NOMPAR,VALPAF, 5,
     1                   NOMC(1),  MATERF(1,1),  CERR(1), BL2 )
          IF ( CERR(3) .NE. 'OK' ) MATERF(3,1) = 0.D0
          IF ( CERR(4) .NE. 'OK' ) MATERF(4,1) = 0.D0
          IF ( CERR(5) .NE. 'OK' ) MATERF(5,1) = 0.D0
          CALL RCVALA (  IMAT,    'ROUSSELIER',  1, 'TEMP', TEMPF, 7,
     1                   NOMC(6),  MATERF(1,2),  CERR(6), FB2 )
          CALL RCVALA (  IMAT,    'ROUSS_VISC',  0, ' ', R8BID, 3,
     1                   NOMC(13),  MATERF(8,2),  CERR(13), FB2 )
C
C         RECUPERATION DE E(TEMPF) VIA LES COURBES DE TRACTION MONOTONES
C         SIG = F(EPS,TEMP) ENTREES POINT PAR POINT  (MOT CLE TRACTION)
C         > ECRASEMENT DU E RECUPERE PAR MOT CLE ELAS
C
          CALL RCTRAC (IMAT,'TRACTION','SIGM',TEMPF,
     1                 JPROL,JVALE,NBVALE,MATERF(1,1))
C
C -     MATERIAU CONSTANT ? ------------------------------------------
C
C       PRINT * ,'MATERD = ',MATERD,'MATERF = ',MATERF
        MATCST = 'OUI'
        DO 30 I = 1,5
          IF ( ABS ( MATERD(I,1) - MATERF(I,1) ) .GT. EPSI )THEN
          MATCST = 'NON'
          GOTO 50
          ENDIF
 30     CONTINUE
        DO 40 I = 1,10
          IF ( ABS ( MATERD(I,2) - MATERF(I,2) ) .GT. EPSI )THEN
          MATCST = 'NON'
          GOTO 50
          ENDIF
 40     CONTINUE
C
C ---- INITIALISATION DE LA POROSITE INITIALE -------------------------
 50     CONTINUE
        IF (VIND(2) .EQ. 0.D0) THEN
          F0 = MATERF(3,2)
          VIND(2) = F0
          IF (F0.LT.0.D0) THEN
            CALL UTMESS('F','RSVMAT','LA POROSITE INITIALE'//
     &      ' F0 NE PEUT ETRE NEGATIVE')
          ELSEIF (F0.GE.1.D0) THEN
            CALL UTMESS('F','RSVMAT','LA POROSITE INITIALE'//
     &      ' F0 NE PEUT ETRE EGAL OU PLUS GRAND QUE UN')
          ENDIF
        ENDIF
C
C ----ET C EST TOUT ---------
 9999   CONTINUE
        END

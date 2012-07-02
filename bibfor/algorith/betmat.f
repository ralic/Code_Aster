        SUBROUTINE BETMAT ( FAMI, KPG, KSP, MOD, IMAT, NMAT, TEMPD,
     &                      TEMPF, MATERD, MATERF, MATCST, NDT, NDI,
     &                      NR, NVI )
      IMPLICIT NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
        INTEGER         NMAT, NDT , NDI  , NR , NVI,KPG,KSP,IISNAN
        REAL*8          MATERD(NMAT,2) ,MATERF(NMAT,2) , TEMPD , TEMPF
        REAL*8           VALPAF
        REAL*8          EPSI , THETA,R8NNEM
        CHARACTER*8     MOD , NOMC(14) , NOMPAR
      INTEGER CERR(14)
        CHARACTER*3     MATCST
        CHARACTER*(*)   FAMI
C-----------------------------------------------------------------------
      INTEGER I ,IMAT 
C-----------------------------------------------------------------------
        DATA EPSI       /1.D-15/
C       ----------------------------------------------------------------
C
C -     NB DE COMPOSANTES / VARIABLES INTERNES -------------------------
C
        CALL BETNVI ( MOD , NDT , NDI , NR , NVI )
C
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
        IF ((IISNAN(TEMPD).NE.0).OR.(IISNAN(TEMPF).NE.0)) THEN
          THETA=R8NNEM()
        ELSE
          IF ( TEMPD .GT. TEMPF ) THETA = TEMPD
        ENDIF
C
        NOMPAR = 'TEMP'
        VALPAF = THETA
C
C -     RECUPERATION MATERIAU A TEMPD (T)
C
        CALL RCVALB (FAMI,KPG,KSP,'-',IMAT,' ','ELAS', 0,' ',
     &               0.D0,5,NOMC(1),MATERD(1,1),CERR(1),0)
        IF ( CERR(3) .NE. 0 ) MATERD(3,1) = 0.D0
        IF ( CERR(4) .NE. 0 ) MATERD(4,1) = 0.D0
        IF ( CERR(5) .NE. 0 ) MATERD(5,1) = 0.D0
        CALL RCVALB (FAMI,KPG,KSP,'-',IMAT,' ','BETON_DOUBLE_DP',0,
     &               ' ',0.D0, 8,NOMC(6),  MATERD(1,2),  CERR(6),
     &               2)
        CALL RCVALB (FAMI,KPG,KSP,'-',IMAT,' ','BETON_DOUBLE_DP',0,
     &               ' ',0.D0,1,NOMC(14),MATERD(9,2),CERR(14),
     &               0)
        IF ( CERR(14).NE. 0 ) MATERD(9,2) = -1.D0
C
C -     RECUPERATION MATERIAU A TEMPF (T+DT)
C
        CALL RCVALB (FAMI,KPG,KSP,'+',IMAT,' ','ELAS',1,NOMPAR,
     &               VALPAF,5,NOMC(1),MATERF(1,1),CERR(1),0)
        IF ( CERR(3) .NE. 0 ) MATERF(3,1) = 0.D0
        IF ( CERR(4) .NE. 0 ) MATERF(4,1) = 0.D0
        IF ( CERR(5) .NE. 0 ) MATERF(5,1) = 0.D0
        CALL RCVALB (FAMI,KPG,KSP,'+',IMAT,' ','BETON_DOUBLE_DP',1,
     &               NOMPAR,VALPAF,8,NOMC(6),MATERF(1,2),CERR(6),2)
        CALL RCVALB (FAMI,KPG,KSP,'+',IMAT,' ','BETON_DOUBLE_DP',1,
     &               NOMPAR,VALPAF,1,NOMC(14),MATERF(9,2),CERR(14),0)
        IF ( CERR(14).NE. 0 ) MATERF(9,2) = -1.D0
C
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

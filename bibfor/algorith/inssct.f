      SUBROUTINE INSSCT (SIGR,SIGM,VIND,SCT,NMAT,MATERF,LFISU)
        IMPLICIT REAL*8 (A-H,O-Z)
C       -----------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/03/2004   AUTEUR REZETTE C.REZETTE 
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
C       -----------------------------------------------------------
C       NADAI_B :  CALCUL DE LA PROPORTION DE CONTRAINTE SCT POUR
C                  LAQUELLE LE CRITERE DE FISSURATION EST VERIFIE
C
C       IN
C           SIGR    :  CONTRAINTE CORRIGEE A T
C           SIGM    :  INCREMENT DE CONTRAINTE ELASTIQUE CORRIGEE
C           VIND    :  VARIABLES INTERNES A T INITIALISEES DANS INSVAR
C           NMAT    :  DIMENSION MATER
C           MATERF  :  COEFFICIENTS MATERIAU A T+DT
C       OUT
C           SCT     :  SCALAIRE T
C           VIND(6) :  STOCKAGE DE TETA = ANGLE DE FISSURATION
C                      (RESTE CONSTANT AU COURS DU TEMPS)
C           VIND(14):  RTM1 = RESISTANCE EN TRACTION UNIAXIALE
C                      DIRECTION ORTHOTROPIE 1
C       -----------------------------------------------------------
        INTEGER   NMAT , NDT , NDI ,LFISU
        REAL*8    VIND(*)
        REAL*8    SIGD(6) , DSIG(6) , SIGM(6), SIGR(6) , SIGF(6)
        REAL*8    MATERF(NMAT,2), V1(4), SEQD, SEQF, SEUILD, SEUILF
        REAL*8    LTR, LCS, RAC2, SCT, TETA, RTM1, DTAU
C       ------------------------------------------------------------
        COMMON /TDIM/   NDT , NDI
C       ------------------------------------------------------------
        RAC2 = SQRT(2.D0)
        LCS     = MATERF(1,2)
        LTR     = MATERF(2,2)
        CALL LCEQVN ( 6 , SIGR , SIGD )
        CALL LCEQVN ( 6 , SIGM , DSIG )
        CALL LCSOVE ( SIGD, DSIG  , SIGF )
        CALL INSNAT ( SIGD, NMAT, MATERF, SEQD , SEUILD )
        CALL INSNAT ( SIGF, NMAT, MATERF, SEQF , SEUILF )
C
        DTAU = SEQF - SEQD
        IF( ABS(DTAU) .LT. 1.D-6 ) THEN
         SCT = 1.D0
        ELSE
         SCT = ( LCS - SEQD) / DTAU
        ENDIF
        CALL LCPRSV ( SCT  , SIGM  , SIGF )
        CALL LCSOVE ( SIGF , SIGR  , SIGF )
C
C       CORRECTION DES CONTRAINTES POUR INSPRI
        SIGF(3) = SIGF(4) / RAC2
C
        CALL INSPRI ( SIGF , V1 )
C
C       RECORRECTION DES CONTRAINTES APRES INSPRI
        SIGF(3) = 0.D0
C
C
        LFISU = 0
        IF ( V1(1) .GE. 0.D0) THEN
          LFISU = 1
          TETA = V1(4)
          RTM1 = V1(1)
          IF ( RTM1 .LT. 0.D0 ) RTM1 = 0.D0
          IF ( RTM1 .GT. LTR  ) RTM1 = LTR
          VIND(6)  = TETA
          VIND(14) = RTM1
        ENDIF
C
        END

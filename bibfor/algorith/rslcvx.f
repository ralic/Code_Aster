        SUBROUTINE RSLCVX(FAMI, KPG, KSP, IMAT, NMAT, MATER, 
     &                    SIG, VIN, SEUIL)
        IMPLICIT NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/10/2007   AUTEUR SALMONA L.SALMONA 
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
C       ROUSSELIER : CONVEXE ELASTO PLASTIQUE POUR (MATER,TEMP,SIG,B,P)
C                    SEUIL    F    = S - R(P) + D S1 F EXP(SIGM/S1)
C                                                T  1/2
C                       AVEC  S    = (3/2 SIGDV SIGDV)
C                             SIGDV  = SIG -1/3 TR(SIG)I
C                             R(P) = FCT DE P PAR POINTS
C                             SIGM  = 1/3 TR(SIG) I
C       ----------------------------------------------------------------
C       IN  FAMI   :  FAMILLE DES POINTS DE GAUSS  
C       IN  KPG    :  NUMERO DU POINT DE GAUSS  
C       IN  KSP    :  NUMERO DU SOUS POINT DE GAUSS 
C       IN  SIG    :  CONTRAINTE
C       IN  VIN    :  VARIABLES INTERNES
C       IN  IMAT   :  ADRESSE DU MATERIAU CODE
C       IN  NMAT   :  DIMENSION MATER
C       IN  MATER  :  COEFFICIENTS MATERIAU A T+DT
C       OUT SEUIL  :  SEUIL  ELASTICITE
C       ----------------------------------------------------------------
        INTEGER         NMAT, IMAT,KPG,KSP
        CHARACTER*(*)   FAMI
C
        REAL*8          MATER(NMAT,2), TEMP , SEUIL
        REAL*8          SIG(6), RIG(6), RIGDV(6), RIGM, VIN(3)
        REAL*8          UNRHO, D, S1, P, F, F0, RP, DRDP
        REAL*8          LCNRTS, UN, ARGMAX
C
        PARAMETER       ( UN     = 1.D0   )
C       ----------------------------------------------------------------
        D  = MATER(1,2)
        S1 = MATER(2,2)
        F0 = MATER(3,2)
        P  = VIN(1)
        F  = VIN(2)
        ARGMAX = 200.D0
C
C --    MATERIAU CASSE
        IF (F .GE. MATER(6,2)) THEN
          SEUIL = UN
C
C --    MATERIAU SAIN
        ELSE
          UNRHO = (UN-F0)/(UN-F)
          CALL  LCPRSV (UNRHO, SIG, RIG)
          CALL  LCHYDR (RIG,  RIGM)
          CALL  LCSOMH (RIG, -RIGM, RIGDV)
          CALL  RSLISO (FAMI, KPG, KSP, '+', IMAT, P, RP, DRDP)
          SEUIL = LCNRTS(RIGDV) - RP
          IF ((RIGM/S1).GT.(ARGMAX)) THEN
            SEUIL = SEUIL + D*S1*F*EXP(ARGMAX)
          ELSE
            SEUIL = SEUIL + D*S1*F*EXP(RIGM/S1)
          ENDIF
        ENDIF
C
        END

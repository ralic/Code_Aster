        SUBROUTINE INSCVX ( NMAT, MATER, SIG ,VIN, SEUIL )
        IMPLICIT REAL*8 (A-H,O-Z)
C       ==============================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/02/2004   AUTEUR REZETTE C.REZETTE 
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
C       --------------------------------------------------------------
C       NADAI_BETON  : CONVEXE ELASTO PLASTIQUE POUR LE BETON
C
C       SEUIL DE NADAI-COMPRESSION : F = ( TOCT + A * SOCT ) / B - TAU
C
C          AVEC  TOCT, SOCT = CONTRAINTES OCTAEDRALES
C
C          A, B = CONSTANTES CRITERE NADAI-COMPRESSION
C
C
C       --------------------------------------------------------------
C       IN  SIG    :  CONTRAINTE
C       IN  VIN    :  VARIABLES INTERNES = (KAPA)
C       IN  NMAT   :  DIMENSION MATER
C       IN  MATER  :  COEFFICIENTS MATERIAU
C       OUT SEUIL  :  SEUIL  ELASTICITE
C       --------------------------------------------------------------
        INTEGER         NDT , NDI , NMAT
        REAL*8          KAPA, TAU, HP,  SIG(6) ,  VIN(*)
        REAL*8          MATER(NMAT,2) , LCS  , KPIC , R0
        REAL*8          SEUIL, I1, J2, J3, RCOS3T , KRUP
C       --------------------------------------------------------------
        COMMON /TDIM/   NDT , NDI
C       --------------------------------------------------------------
C
        KAPA  = VIN(1)
C
        LCS    = MATER(1,2)
        R0     = MATER(3,2)
        KPIC   = MATER(4,2)
        KRUP   = MATER(5,2)
C
        CALL INSINV ( SIG , I1, J2, J3, RCOS3T )
        CALL INSISO (LCS, KPIC, R0, KRUP, KAPA, HP, TAU)
        CALL INSNAD ( I1 , J2 , TAU , SEUIL )
C
        END

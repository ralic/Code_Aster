        SUBROUTINE CHBCVX ( NMAT, MATER, SIG ,VIN, SEUIL )
        IMPLICIT REAL*8 (A-H,O-Z)
C       ================================================================
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
C       ----------------------------------------------------------------
C       CHABOCHE  :   CONVEXE ELASTO PLASTIQUE POUR (MATER,SIG,X1,X2,P)
C                     SEUIL   F    = S   -  R(P)
C                                                T           1/2
C                       AVEC  S    = (3/2(D-X1-X2) (D-X1-X2))
C                       ET    D    = SIG - 1/3 TR(SIG) I
C       ----------------------------------------------------------------
C       IN  SIG    :  CONTRAINTE
C       IN  VIN    :  VARIABLES INTERNES = ( X1 X2 P )
C       IN  NMAT   :  DIMENSION MATER
C       IN  MATER  :  COEFFICIENTS MATERIAU A TEMP
C       OUT SEUIL  :  SEUIL  ELASTICITE
C       ----------------------------------------------------------------
        INTEGER         NDT , NDI , NMAT
        REAL*8          P ,   SIG(6) , X1(6) , X2(6) , DEV(6) , VIN(*)
        REAL*8          MATER(NMAT,2) , RI , RO , B , SEUIL
        REAL*8          LCNRTS , CHBISO
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT , NDI
C       ----------------------------------------------------------------
C
        CALL LCEQVN ( NDT , VIN(1)     , X1  )
        CALL LCEQVN ( NDT , VIN(NDT+1) , X2  )
        P  = VIN(2*NDT+1)
C
        RI = MATER(1,2)
        RO = MATER(2,2)
        B  = MATER(3,2)
C
        CALL LCDEVI ( SIG , DEV )
        CALL LCDIVE ( DEV , X1 , DEV )
        CALL LCDIVE ( DEV , X2 , DEV )
        SEUIL  = LCNRTS( DEV ) - CHBISO( RI ,  RO , B , P )
        END

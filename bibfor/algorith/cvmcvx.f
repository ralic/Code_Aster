      SUBROUTINE CVMCVX ( NMAT, MATER, SIG ,VIN, SEUIL )
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
C       VISCOCHABOCHE  :
C          CONVEXE VISCO PLASTIQUE POUR (MATER,SIG,X1,X2,R)  (OPTION 1 )
C                     SEUIL   F    = S   -  AR * R - K
C                                                T           1/2
C                       AVEC  S    = (3/2(D-X1-X2) (D-X1-X2))
C                       ET    D    = SIG - 1/3 TR(SIG) I
C       ----------------------------------------------------------------
C       IN  SIG    :  CONTRAINTE
C       IN  VIN    :  VARIABLES INTERNES = ( X1, X2, P, R, Q, XXI, E3 )
C       IN  NMAT   :  DIMENSION MATER
C       IN  MATER  :  COEFFICIENTS MATERIAU A TEMPERATURE
C       OUT SEUIL  :  SEUIL  ELASTICITE
C       ----------------------------------------------------------------
        INTEGER         NDT , NDI , NMAT
        REAL*8          SIG(6)   , X1(6)   , X2(6)   , DEV(6) , VIN(*)
        REAL*8          VTMP(6)  , XXI(6)
        REAL*8          DIFC1  , DIFC2
        REAL*8          KOOH(6,6), AR , R  , K  , Q  , C1D , C2D
        REAL*8          MATER(NMAT,2)      , SEUIL
        REAL*8          LCNRTS
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT , NDI
        COMMON /COED/   C1D , C2D
C       ----------------------------------------------------------------
C
C - CALCUL DU PREMIER SEUIL
C
        AR     = MATER(3,2)
        K       = MATER(4,2)
C        C1D      = MATERD(15,2)
C        C2D      = MATERD(20,2)
        C1F      = MATER(15,2)
        C2F      = MATER(20,2)
        CALL LCEQVN ( NDT  , VIN(1)       ,  X1  )
        CALL LCEQVN ( NDT  , VIN(NDT+1)   ,  X2  )
C
C --   CAS ANISOTHERME
C
        IF (C1D .NE. 0.D0) THEN
          DIFC1 = C1F/C1D
          CALL LCPRSV ( DIFC1  , X1       , X1  )
        ENDIF
        IF (C2D .NE. 0.D0) THEN
          DIFC2 = C2F/C2D
          CALL LCPRSV ( DIFC2  , X2       , X2  )
        ENDIF
C
        R  = VIN(2*NDT+2)
        CALL LCDEVI ( SIG  , DEV                 )
        CALL LCDIVE ( DEV  , X1           , DEV  )
        CALL LCDIVE ( DEV  , X2           , DEV  )
        SEUIL  = LCNRTS( DEV ) - AR * R - K
        END

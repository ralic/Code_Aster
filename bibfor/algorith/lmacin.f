        FUNCTION LMACIN ( IMAT , NMAT , MATER , V )
        IMPLICIT REAL*8 (A-H,O-Z)
C       ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 10/03/98   AUTEUR VABHHTS J.PELLET 
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
C       MODELE VISCOPLASTIQUE DE BESANCON EN VITESSE
C       LOI ECROUISSAGE CINEMATIQUE Y(V) A V DONNE
C           YI    :  COEF
C           Y0    :  COEF
C           B      :  COEF
C       ----------------------------------------------------------------
C       IN  NMAT   :  DIMENSION  DE MATER
C       IN  MATER  :  COEFFICIENTS MATERIAU
C       IN  V      :  DEF CUMULEE
C       OUT LMACIN :  Y(V) = YI + ( Y0 - YI )EXP( -BV )
C       ----------------------------------------------------------------
        INTEGER         NMAT
        REAL*8          MATER(NMAT,2)
        REAL*8          YI , Y0 , B , V , LMACIN
C
        YI   = MATER(5,2)
        Y0   = MATER(6,2)
        B     = MATER(7,2)
C
        LMACIN = YI + ( Y0 - YI ) * EXP(-B*V)
C
C
        END

      SUBROUTINE INSETA ( ETATD , VIND , NVI  )
        IMPLICIT REAL*8 (A-H,O-Z)
C       -----------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 27/11/96   AUTEUR INBHHOM O.MERABET 
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
C       NADAI_B :  TEST ETAT DU BETON A T
C
C       IN  VIND   :  VARIABLES INTERNES A T
C           NVI    :  NB VARIABLES INTERNES
C
C       OUT
C           ETATD  :  ETAT ELASTIQUE OU PLASTIQUE
C       -----------------------------------------------------------
        INTEGER        NVI , JFIS1
        REAL*8         VIND(*) , EPS3  , EPSI
        CHARACTER*7    ETATD
        PARAMETER      ( EPSI = 1.D-15 )
C
        JFIS1   = INT(VIND(NVI-1))
        EPS3    = VIND(NVI)
        IF ( ABS(EPS3) .LE. EPSI .AND. JFIS1 .EQ. 0 ) THEN
        ETATD = 'ELASTIC'
        ELSE
        ETATD = 'PLASTIC'
        ENDIF
        END

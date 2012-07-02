      SUBROUTINE COESP1 ( REN, PHI0, EPS, FRC, BETA )
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C-----------------------------------------------------------------------
      IMPLICIT NONE
C
C
C DESCRIPTION : VALEURS DES COEFFICIENTS DEFINISSANT
C -----------   LE SPECTRE DE TURBULENCE.
C
C
C    PHI0, EPS ET BETA DEPENDENT DU REYNOLDS REN.
C    FRC EST CONSTANT.
C
C ******************   DECLARATION DES VARIABLES   *********************
C
C ARGUMENTS
C ---------
      REAL*8 REN, PHI0, EPS, FRC, BETA
C
C ******************   DEBUT DU CODE EXECUTABLE   **********************
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      IF ( REN .LE. 1.5D+4 ) THEN
         PHI0 = 2.1808D0
      ELSE IF ( REN .LE. 5.0D+4 ) THEN
         PHI0 = 20.42D0                           -
     S          14.00D-4  * REN                 -
     S           9.81D-8  * REN*REN             +
     S          11.97D-12 * REN*REN*REN         -
     S          35.95D-17 * REN*REN*REN*REN     +
     S          34.69D-22 * REN*REN*REN*REN*REN
      ELSE
         PHI0 = 38.6075D0
      END IF
      PHI0 = PHI0 * 1.3D-4
C
      IF ( REN .LE. 3.5D+4 ) THEN
         EPS  = 0.7D0
         BETA = 3.0D0
      ELSE IF ( REN .GT. 5.5D+4 ) THEN
         EPS  = 0.6D0
         BETA = 4.0D0
      ELSE
         EPS  = 0.3D0
         BETA = 4.0D0
      END IF
C
      FRC = 0.2D0
C
      END

      SUBROUTINE RVEGAL(EPSI,CRITER,X,Y,OK,ECCART)
      IMPLICIT NONE
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C
      CHARACTER*1 CRITER
      LOGICAL   OK
      REAL*8    EPSI,X,Y,ECCART
C
C********************************************************************
C
C     OPERATION REALISEE
C     ------------------
C
C       TEST DE L' EGALITE " X = Y "
C
C********************************************************************
C
      REAL*8 SEUIL,Z
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      ECCART = X - Y
      SEUIL  = EPSI
      Z      = ABS(X)
C
      IF ( (CRITER .EQ. 'R') .AND. (Z .GE. EPSI) ) THEN
C
         SEUIL = SEUIL*Z
C
      ENDIF
C
      OK = ( ABS(ECCART) .LT. SEUIL)
C
      END

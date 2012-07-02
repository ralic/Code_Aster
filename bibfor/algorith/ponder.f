      SUBROUTINE PONDER(DNORM,CL,P)
      IMPLICIT NONE
      REAL*8 DNORM,CL,P
C ----------------------------------------------
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
C
C PONDERATION LINEAIRE LAME FLUIDE - CHOC SEC
C   = 0 SUR OBSTACLE
C   = 1 LOIN DE L'OBSTACLE
C ----------------------------------------------
C IN : DNORM  : DISTANCE A L'OBSTACLE
C IN : CL     : EPAISSEUR DE LA COUCHE LIMITE
C OUT: P      : PONDERATION
C ----------------------------------------------
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      IF (DNORM .GE. CL) THEN
        P = 1.D0
      ELSEIF (DNORM. LE. 0.5D0*CL) THEN
        P = 0.D0
      ELSE
        P = (DNORM-0.5D0*CL)/(0.5D0*CL)
      ENDIF
C
      END

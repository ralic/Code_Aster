      SUBROUTINE ECPUIS(E,SIGY,ALFAFA,UNSURN,PM,DP,RP,RPRIM)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/11/2007   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
      IMPLICIT NONE
C
C     ARGUMENTS:
C     ----------
      REAL*8 DP,RPRIM,E,SIGY,PM,RP,ALFAFA,UNSURN
C ----------------------------------------------------------------------
C BUT: EVALUER LA FONCTION D'ECROUISSAGE ISOTROPE AVEC LOI EN PUISSANCE
C    IN: DP     : DEFORMATION PLASTIQUE CUMULEE
C   OUT: RP     : R(PM+DP)
C   OUT: RPRIM  : R'(PM+DP)
C ----------------------------------------------------------------------
C     VARIABLES LOCALES:
C     ------------------
      REAL*8         P0,RP0,COEF
C
      P0=1.D-10
      IF ((PM+DP).LE.P0) THEN
         RP0= SIGY*(E/ALFAFA/SIGY*(P0))**UNSURN + SIGY
         RP= SIGY+(PM+DP)*(RP0-SIGY)/P0
         RPRIM=(RP0-SIGY)/P0
      ELSE
         COEF = E/ALFAFA/SIGY
         RP= SIGY*(E/ALFAFA/SIGY*(PM+DP))**UNSURN + SIGY
         RPRIM= UNSURN * SIGY * COEF * (COEF*(PM+DP))**(UNSURN-1.D0)
      ENDIF
C
      END

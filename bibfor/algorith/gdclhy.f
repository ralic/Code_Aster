      SUBROUTINE GDCLHY(JE,E)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/02/2007   AUTEUR MICHEL S.MICHEL 
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
      REAL*8   JE,E(6)

C ----------------------------------------------------------------------
C            GRANDES DEFORMATIONS CANO-LORENTZ
C         CORRECTION HYDROSTATIQUE DE LA DEFORMATION ELASTIQUE
C ----------------------------------------------------------------------
C IN  JE      DETERMINANT DE E CIBLE
C VAR E       DEFORMATION ELASTIQUE (XX,YY,ZZ,RAC2*XY,RAC2*XZ,RAC2*YZ)
C ----------------------------------------------------------------------
      CALL GDSMHY(JE,E) 
      END 

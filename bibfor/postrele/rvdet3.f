      SUBROUTINE RVDET3(T,D)
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
      REAL*8 T(*),D
C
C*********************************************************************
C
C   OPERATION REALISEE
C   ------------------
C
C     CALCUL DU DETERMINANT DU TENSEUR 3X3 SYMETRIQUE T
C
C     T EST REPRESENTE PAR LA TABLE DE SES COMPOSANTES DANS L' ORDRE :
C
C        XX, YY, ZZ, XY, XZ, YZ
C
C
C*********************************************************************
C
      REAL*8 AUX
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      D   = 0.0D0
      AUX = 0.0D0
C
      CALL RVDET2(T(2),T(6),T(6),T(3),AUX)
C
      D = T(1)*AUX
C
      CALL RVDET2(T(4),T(6),T(5),T(3),AUX)
C
      D = D - T(4)*AUX
C
      CALL RVDET2(T(4),T(2),T(5),T(6),AUX)
C
      D = D + T(5)*AUX
C
      END

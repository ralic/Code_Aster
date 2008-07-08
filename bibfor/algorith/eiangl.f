      SUBROUTINE EIANGL(NDIM,NNO2,ANGEUL,ANG)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 07/07/2008   AUTEUR LAVERNE J.LAVERNE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER NDIM, NNO2
      REAL*8 ANGEUL(3),ANG(24)
      
C--------------------------------------------------
C  DEFINITION DES ANGLES D'EULER AUX NOEUDS
C  POUR L'ELEMENT D'INTERFACE
C
C  IN  : NDIM,NNO2
C        ANGEUL : ANGLES D'EULER EN DEGRES
C  OUT : 
C        ANG : ANGLES D'EULER AUX NOEUDS EN RADIANS 
C--------------------------------------------------
      INTEGER N
      REAL*8 R8DGRD
      
      IF (NDIM.EQ.2) THEN
        DO 10 N = 1,NNO2
          ANG(N) = ANGEUL(1)*R8DGRD()
   10   CONTINUE
      ELSE
        DO 20 N = 1,NNO2
          ANG(1 + (N-1)*3) = ANGEUL(1)*R8DGRD()
          ANG(2 + (N-1)*3) = ANGEUL(2)*R8DGRD()
          ANG(3 + (N-1)*3) = ANGEUL(3)*R8DGRD()
   20   CONTINUE
      ENDIF
      
      END

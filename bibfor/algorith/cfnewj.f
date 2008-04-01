      SUBROUTINE CFNEWJ(NDIM  ,COORDE,COORDP,NORM  ,JEU   )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 01/04/2008   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT    NONE      
      INTEGER      NDIM
      REAL*8       JEU
      REAL*8       COORDE(3),COORDP(3)    
      REAL*8       NORM(3)
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODES DISCRETES - APPARIEMENT)
C
C CALCUL DU JEU SUR LA NORMALE
C
C ----------------------------------------------------------------------
C
C
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  COORDE : COORDONNEES DU NOEUD ESCLAVE E
C IN  COORDP : COORDONNEES DE LA PROJECTION DU NOEUD ESCLAVE E
C IN  NORM   : NORMALE 
C OUT JEU    : JEU
C
C ----------------------------------------------------------------------
C   

C
C --- CALCUL JEU
C
      JEU = (COORDP(1)-COORDE(1))*NORM(1) +
     &      (COORDP(2)-COORDE(2))*NORM(2)
      IF (NDIM.EQ.3) THEN 
        JEU = JEU + (COORDP(3)-COORDE(3))*NORM(3)
      ENDIF     

      END

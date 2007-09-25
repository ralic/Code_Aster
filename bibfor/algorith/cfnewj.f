      SUBROUTINE CFNEWJ(NDIM,COORDP,COORDM,NORM,JEU)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/09/2007   AUTEUR ABBAS M.ABBAS 
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
      INTEGER     NDIM
      REAL*8      JEU
      REAL*8      COORDP(3)
      REAL*8      COORDM(3)    
      REAL*8      NORM(3)
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
C IN  COORDP : COORDONNEES DU NOEUD ESCLAVE P
C IN  COORDM : COORDONNEES DE LA PROJECTION M DU NOEUD P SUR LA MAILLE
C IN  NORM   : NORMALE LISSEE/MOYENNE (OPTIONS LISSA/MOYEN)
C OUT JEU    : JEU
C
C ----------------------------------------------------------------------
C   
      JEU = (COORDM(1)-COORDP(1))*NORM(1) +
     &      (COORDM(2)-COORDP(2))*NORM(2)
      IF (NDIM.EQ.3) THEN 
        JEU = JEU + (COORDM(3)-COORDP(3))*NORM(3)
      ENDIF     

      END

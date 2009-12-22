      SUBROUTINE MMMJEV(NDIM  ,NORM  ,VITPE ,VITPM ,JEUVIT)
C    
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/12/2009   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      INTEGER      NDIM
      REAL*8       VITPE(3),VITPM(3)
      REAL*8       NORM(3)
      REAL*8       JEUVIT
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
C
C CALCUL DU JEU ACTUALISE - SAUT DE VITESSE
C
C ----------------------------------------------------------------------
C
C
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  NORM   : VALEUR DE LA NORMALE
C IN  VITPE  : VITESSE DU POINT DE CONTACT
C IN  VITPM  : VITESSE DU PROJETE DU POINT DE CONTACT
C OUT JEUVIT : VALEUR DU JEU (SAUT DE VITESSE NORMAL)
C
C ----------------------------------------------------------------------
C
      INTEGER IDIM
C
C ----------------------------------------------------------------------
C
      JEUVIT = 0.D0         
      DO 10 IDIM = 1,NDIM 
        JEUVIT = JEUVIT +  (VITPE(IDIM)-VITPM(IDIM)) * NORM(IDIM)
 10   CONTINUE
C    
      END

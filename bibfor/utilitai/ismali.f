      FUNCTION ISMALI (TYPMA)
      IMPLICIT  NONE

      LOGICAL      ISMALI
      CHARACTER*8  TYPMA
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 06/10/2009   AUTEUR GENIAUT S.GENIAUT 
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
C RESPONSABLE GENIAUT S.GENIAUT
C ----------------------------------------------------------------------
C
C FONCTION VALANT TRUE SI LE TYPE DE MAILLE EST LINEAIRE
C        (AUTANT DE NOEUDS QUE DE NOEUDS SOMMET)     
C
C ----------------------------------------------------------------------
C
C IN   TYPMA  : TYPE DE MAILLE
C OUT  ISMALI : TRUE SI LE TYPE DE MAILLE EST LINEAIRE

      IF (TYPMA.EQ.'POI1'  .OR.
     &    TYPMA.EQ.'SEG2'  .OR.
     &    TYPMA.EQ.'TRIA3' .OR.
     &    TYPMA.EQ.'QUAD4' .OR.
     &    TYPMA.EQ.'TETRA4'.OR.
     &    TYPMA.EQ.'PYRAM5'.OR.
     &    TYPMA.EQ.'PENTA6'.OR.
     &    TYPMA.EQ.'HEXA8') THEN

        ISMALI=.TRUE.

      ELSEIF (TYPMA.EQ.'SEG3'   .OR.
     &        TYPMA.EQ.'TRIA6'  .OR.
     &        TYPMA.EQ.'TRIA7'  .OR.
     &        TYPMA.EQ.'QUAD8'  .OR.
     &        TYPMA.EQ.'QUAD9'  .OR.
     &        TYPMA.EQ.'TETRA10'.OR.
     &        TYPMA.EQ.'PYRAM13'.OR.
     &        TYPMA.EQ.'PENTA15'.OR.
     &        TYPMA.EQ.'HEXA20' .OR.
     &        TYPMA.EQ.'HEXA27') THEN

        ISMALI=.FALSE.

      ELSE
        
        CALL ASSERT(.FALSE.)
        
      ENDIF     

      END

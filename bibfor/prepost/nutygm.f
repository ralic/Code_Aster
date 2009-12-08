      FUNCTION NUTYGM(NOMTYP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 24/06/2002   AUTEUR CIBHHGB G.BERTRAND 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER     NUTYGM
      CHARACTER*8 NOMTYP
C-----------------------------------------------------------------------
C
C- BUT : CETTE FONCTION RETOURNE LE TYPE GMSH DU TYPE D'ELEMENT ASTER
C
C-----------------------------------------------------------------------
C --- DESCRIPTION DES PARAMETRES
C IN   K8  NOMTYP  : NOM    ASTER DU TYPE D'ELEMENT
C OUT  I   NUTYGM  : NUMERO GMSH  DU TYPE D'ELEMENT
C ----------------------------------------------------------------------
C
      IF (NOMTYP.EQ.'SEG2') THEN
        NUTYGM = 1
      ELSEIF (NOMTYP.EQ.'TRIA3') THEN
        NUTYGM = 2
      ELSEIF (NOMTYP.EQ.'QUAD4') THEN
        NUTYGM = 3
      ELSEIF (NOMTYP.EQ.'TETRA4') THEN
        NUTYGM = 4
      ELSEIF (NOMTYP.EQ.'HEXA8') THEN
        NUTYGM = 5
      ELSEIF (NOMTYP.EQ.'PENTA6') THEN
        NUTYGM = 6
      ELSEIF (NOMTYP.EQ.'PYRAM5') THEN
        NUTYGM = 7
      ELSEIF (NOMTYP.EQ.'POI1') THEN
        NUTYGM = 15
      ENDIF
C
      END

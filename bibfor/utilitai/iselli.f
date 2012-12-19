      FUNCTION ISELLI (ELREFZ)
      IMPLICIT  NONE
      LOGICAL      ISELLI
      CHARACTER*(*)  ELREFZ
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 19/12/2012   AUTEUR PELLET J.PELLET 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C RESPONSABLE GENIAUT S.GENIAUT
C ----------------------------------------------------------------------
C
C FONCTION VALANT TRUE SI L'ELEMENT DE REFERENCE EST LINEAIRE
C        (AUTANT DE NOEUDS QUE DE NOEUDS SOMMET)
C
C ----------------------------------------------------------------------
C
C IN   ELREFE : ELEMENT DE REFERENCE (ELREFE)
C OUT  ISELLI : TRUE SI L'ELEMENT DE REFERENCE EST LINEAIRE

      CHARACTER*3 ELREFE

      ELREFE = ELREFZ

      IF (ELREFE.EQ.'PO1'.OR.
     &    ELREFE.EQ.'SE2'.OR.
     &    ELREFE.EQ.'TR3'.OR.
     &    ELREFE.EQ.'QU4'.OR.
     &    ELREFE.EQ.'TE4'.OR.
     &    ELREFE.EQ.'PY5'.OR.
     &    ELREFE.EQ.'PE6'.OR.
     &    ELREFE.EQ.'HE8') THEN
C
        ISELLI=.TRUE.
C
      ELSEIF (ELREFE.EQ.'SE3'.OR.
     &        ELREFE.EQ.'TR6'.OR.
     &        ELREFE.EQ.'TR7'.OR.
     &        ELREFE.EQ.'QU8'.OR.
     &        ELREFE.EQ.'QU9'.OR.
     &        ELREFE.EQ.'T10'.OR.
     &        ELREFE.EQ.'P13'.OR.
     &        ELREFE.EQ.'P15'.OR.
     &        ELREFE.EQ.'P18'.OR.
     &        ELREFE.EQ.'H20'.OR.
     &        ELREFE.EQ.'H27') THEN
C
        ISELLI=.FALSE.
C
      ELSE
C
        CALL ASSERT(.FALSE.)
C
      ENDIF
C
      END

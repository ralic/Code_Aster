      FUNCTION DEDANS(M,TYPEMA) 

      IMPLICIT NONE

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 08/11/2004   AUTEUR DURAND C.DURAND 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C A_UTIL
C ----------------------------------------------------------------------
C        TEST D'APPARTENANCE D'UN POINT A LA MAILLE DE REFERENCE
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C REAL*8       M(*)       : POINT (X, Y[, Z])
C CHARACTER*8  TYPEMA     : TYPE DE LA MAILLE 
C       
C VARIABLES DE SORTIE
C LOGICAL      IRET       : .TRUE. SI M DANS MAILLE DE REFERENCE
C ---------------------------------------------------------------------

C --- VARIABLES
      CHARACTER*8  TYPEMA
      REAL*8       M(*),X,Y,Z,A
      LOGICAL      DEDANS,IR

      IF (TYPEMA(1:4).EQ.'TRIA') THEN
        X = M(1)
        Y = M(2)
        IR = ((X.GE.0.D0).AND.(Y.GE.0.D0).AND.((X+Y).LE.1.D0))

      ELSEIF (TYPEMA(1:4).EQ.'QUAD') THEN
        IR = ((ABS(M(1)).LE.1.D0).AND.(ABS(M(2)).LE.1.D0))

      ELSEIF (TYPEMA(1:5).EQ.'TETRA') THEN
        X = M(1)
        Y = M(2)
        Z = M(3)
        IR = ((X.GE.0.D0).AND.(Y.GE.0.D0).AND.
     &          (Z.GE.0.D0).AND.((X+Y+Z).LE.1.D0))

      ELSEIF (TYPEMA(1:5).EQ.'PENTA') THEN    
        Y = M(2)
        Z = M(3)
        IR = ((ABS(M(1)).LE.1.D0).AND.(Y.GE.0.D0).AND.
     &          (Z.GE.0.D0).AND.((Y+Z).LE.1.D0)) 

      ELSEIF (TYPEMA(1:4).EQ.'HEXA') THEN    
        X = ABS(M(1))
        Y = ABS(M(2))
        Z = ABS(M(3))
        IR = ((X.LE.1.D0).AND.(Y.LE.1.D0).AND.(Z.LE.1.D0))
          
      ELSEIF (TYPEMA(1:4).EQ.'PYRA') THEN
        X = ABS(M(1))
        Y = ABS(M(2))
        Z = M(3)
        IR = ((ABS(Z-0.5D0).LE.0.5D0).AND.((X+Y).LE.(1.D0-Z)))

      ELSE
        CALL UTMESS('F','DEDANS','MAILLE '//TYPEMA//' INDISPONIBLE')
      ENDIF

      DEDANS = IR

      END

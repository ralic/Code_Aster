      FUNCTION DEDANS(M,TYPEMA)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 09/01/2007   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      LOGICAL      DEDANS       
      CHARACTER*8  TYPEMA
      REAL*8       M(*)
C      
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C   
C TEST D'APPARTENANCE D'UN POINT A LA MAILLE DE REFERENCE
C      
C ----------------------------------------------------------------------
C 
C IN  M      : POINT (X, Y[, Z])
C IN  TYPEMA  : TYPE DE LA MAILLE
C
C LA FONCTION RETOURNE .TRUE. SI M DANS MAILLE DE REFERENCE
C
C ---------------------------------------------------------------------
C
      REAL*8       X,Y,Z
C
C ---------------------------------------------------------------------
C
      IF (TYPEMA(1:4).EQ.'TRIA') THEN
        X = M(1)
        Y = M(2)
        DEDANS = ((X.GE.0.D0).AND.(Y.GE.0.D0).AND.((X+Y).LE.1.D0))
      ELSEIF (TYPEMA(1:4).EQ.'QUAD') THEN
        X = M(1)
        Y = M(2)      
        DEDANS = ((ABS(X).LE.1.D0).AND.(ABS(Y).LE.1.D0))
      ELSEIF (TYPEMA(1:5).EQ.'TETRA') THEN
        X = M(1)
        Y = M(2)
        Z = M(3)
        DEDANS = ((X.GE.0.D0).AND.(Y.GE.0.D0).AND.
     &          (Z.GE.0.D0).AND.((X+Y+Z).LE.1.D0))
      ELSEIF (TYPEMA(1:5).EQ.'PENTA') THEN
        Y = M(2)
        Z = M(3)
        DEDANS = ((ABS(M(1)).LE.1.D0).AND.(Y.GE.0.D0).AND.
     &          (Z.GE.0.D0).AND.((Y+Z).LE.1.D0))
      ELSEIF (TYPEMA(1:4).EQ.'HEXA') THEN
        X = ABS(M(1))
        Y = ABS(M(2))
        Z = ABS(M(3))
        DEDANS = ((X.LE.1.D0).AND.(Y.LE.1.D0).AND.(Z.LE.1.D0))
      ELSEIF (TYPEMA(1:4).EQ.'PYRA') THEN
        X = ABS(M(1))
        Y = ABS(M(2))
        Z = M(3)
        DEDANS = ((ABS(Z-0.5D0).LE.0.5D0).AND.((X+Y).LE.(1.D0-Z)))
      ELSE
        WRITE(6,*) 'TYPEMA: ',TYPEMA
        CALL ASSERT(.FALSE.)
      ENDIF
C
      END

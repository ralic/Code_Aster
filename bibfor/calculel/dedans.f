      SUBROUTINE DEDANS(M,TYPEMA,PREC,IRET) 

      IMPLICIT NONE

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 02/04/2002   AUTEUR RATEAU G.RATEAU 
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
C REAL*8       PREC       : PRECISION DU TEST
C       
C VARIABLES DE SORTIE
C LOGICAL      IRET       : .TRUE. SI M DANS MAILLE DE REFERENCE
C ---------------------------------------------------------------------

C --- VARIABLES
      CHARACTER*8  TYPEMA
      REAL*8       M(*),PREC,X,Y,Z,A
      LOGICAL      IRET

      IF (TYPEMA(1:4).EQ.'TRIA') THEN
        X = M(1)
        Y = M(2)
        A = X + Y - 1.D0
        IF ((X.GE.-PREC).AND.(Y.GE.-PREC).AND.(A.LE.PREC)) THEN
          IRET = .TRUE.
        ELSE
          IRET = .FALSE.
        ENDIF

      ELSEIF (TYPEMA(1:4).EQ.'QUAD') THEN
        X = ABS(M(1))-1.D0
        Y = ABS(M(2))-1.D0
        IF ((X.LE.PREC).AND.(Y.LE.PREC)) THEN
          IRET = .TRUE.
        ELSE
          IRET = .FALSE.
        ENDIF

      ELSEIF (TYPEMA(1:5).EQ.'TETRA') THEN
        X = M(1)
        Y = M(2)
        Z = M(3)
        A = X + Y + Z - 1.D0
        IF ( (X.GE.-PREC).AND.(Y.GE.-PREC).AND.
     &       (Z.GE.-PREC).AND.(A.LE.PREC)) THEN
          IRET = .TRUE.
        ELSE
          IRET = .FALSE.
        ENDIF

      ELSEIF (TYPEMA(1:5).EQ.'PENTA') THEN    
        X = ABS(M(1)) - 1.D0
        Y = M(2)
        Z = M(3)
        A = Y + Z - 1.D0
        IF ((X.LE.PREC).AND.(Y.GE.-PREC).AND.
     &      (Z.GE.-PREC).AND.(A.LE.PREC)) THEN
          IRET = .TRUE.
        ELSE
          IRET = .FALSE.
        ENDIF

      ELSEIF (TYPEMA(1:4).EQ.'HEXA') THEN    
        X = ABS(M(1)) - 1.D0
        Y = ABS(M(2)) - 1.D0
        Z = ABS(M(3)) - 1.D0
        IF ((X.LE.PREC).AND.(Y.LE.PREC).AND.(Z.LE.PREC)) THEN
          IRET = .TRUE.
        ELSE
          IRET = .FALSE.
        ENDIF
          
      ELSEIF (TYPEMA(1:4).EQ.'PYRA') THEN
        Z = M(3)
        A = ABS(M(1)) + ABS(M(2)) + Z - 1.D0
        Z = ABS(Z-0.5D0) - 0.5D0
        IF ((Z.LE.PREC).AND.(A.LE.PREC)) THEN
          IRET = .TRUE.
        ELSE
          IRET = .FALSE.
        ENDIF

      ELSE
        CALL UTMESS('F','DEDANS','MAILLE '//TYPEMA//' INDISPONIBLE')
      ENDIF

      END

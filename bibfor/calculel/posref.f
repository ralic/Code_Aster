      SUBROUTINE POSREF(M,TYPEMA,R,NPAN)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 02/04/2002   AUTEUR RATEAU G.RATEAU 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C                                                                       
C                                                                       
C ======================================================================
C ----------------------------------------------------------------------
C  POSITION D'UN POINT M PAR RAPPORT AUX PANS DE LA MAILLE DE REFERENCE
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE 
C REAL*8       M(*)     : COORDONNEES DU POINT
C CHARACTER*8  TYPEMA   : TYPE DE LA MAILLE
C
C VARIABLES DE SORTIE
C REAL*8       R(NPAN)  : DISTANCE DE M AUX PANS DE LA MAILLE DE REF.
C                         (R1, R2, ...) R* DISTANCE ORIENTEE AU PAN *
C INTEGER      NPAN     : NOMBRE DE PANS DE MAILLE DE REFERENCE
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- VARIABLES
      CHARACTER*8  TYPEMA 
      REAL*8       M(*),R(*)
      INTEGER      NPAN
 
      IF (TYPEMA(1:4).EQ.'TRIA') THEN
        R(1) = -M(1)
        R(2) = -M(2)
        R(3) =  M(1) + M(2) - 1.D0
        NPAN =  3

      ELSEIF (TYPEMA(1:4).EQ.'QUAD') THEN
        R(1) = -M(1) - 1.D0
        R(2) =  M(1) - 1.D0
        R(3) = -M(2) - 1.D0
        R(4) =  M(2) - 1.D0
        NPAN =  4
        
      ELSEIF (TYPEMA(1:5).EQ.'TETRA') THEN
        R(1) = -M(1)
        R(2) = -M(2)
        R(3) = -M(3)
        R(4) =  M(1) + M(2) + M(3) - 1.D0
        NPAN =  4

      ELSEIF (TYPEMA(1:5).EQ.'PENTA') THEN
        R(1) = -M(1) - 1.D0
        R(2) =  M(1) + 1.D0
        R(3) = -M(2)
        R(4) = -M(3)
        R(5) =  M(2) + M(3) - 1.D0
        NPAN =  5
       
      ELSEIF (TYPEMA(1:4).EQ.'HEXA') THEN
        R(1) = -M(1) - 1.D0
        R(2) =  M(1) - 1.D0
        R(3) = -M(2) - 1.D0
        R(4) =  M(2) - 1.D0
        R(5) = -M(3) - 1.D0
        R(6) =  M(3) - 1.D0
        NPAN =  6

      ELSE

        CALL UTMESS('F','POSREF','TYPEMA INDISPONIBLE')

      ENDIF

      END

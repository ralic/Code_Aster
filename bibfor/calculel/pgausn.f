      SUBROUTINE PGAUSN(TYPEMA,FG,NG)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 08/11/2004   AUTEUR DURAND C.DURAND 
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
C A_UTIL
C ----------------------------------------------------------------------
C        NOMBRE DE POINTS DE GAUSS POUR UNE FAMILLE D'INTEGRATION
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C CHARACTER*8    TYPEMA     : TYPE DE LA MAILLE
C INTEGER        FG         : FAMILLE D'INTEGRATION
C
C VARIABLE DE SORTIE
C INTEGER        NG         : NOMBRE DE POINTS DE GAUSS
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- VARIABLES
      CHARACTER*8 TYPEMA
      INTEGER     FG,NG,SEG(4),TRI(8),QUA(7),TET(3),PEN(4),HEX(3),PYR(3)

C --- DONNEES
      DATA SEG /1,2,3,4/
      DATA TRI /1,3,3,3,4,6,7,12/
      DATA QUA /1,4,4,8,9,9,16/
      DATA TET /4,5,15/
      DATA PEN /6,6,8,21/
      DATA HEX /8,27,64/
      DATA PYR /5,6,27/

      IF (TYPEMA(1:3).EQ.'SEG') THEN
        NG = SEG(FG)
      ELSEIF (TYPEMA(1:3).EQ.'TRI') THEN
        NG = TRI(FG)
      ELSEIF (TYPEMA(1:3).EQ.'QUA') THEN
        NG = QUA(FG)
      ELSEIF (TYPEMA(1:3).EQ.'TET') THEN
        NG = TET(FG)
      ELSEIF (TYPEMA(1:3).EQ.'PEN') THEN
        NG = PEN(FG)
      ELSEIF (TYPEMA(1:3).EQ.'HEX') THEN
        NG = HEX(FG)
      ELSEIF (TYPEMA(1:3).EQ.'PYR') THEN
        NG = PYR(FG)
      ELSE
        CALL UTMESS('F','PGAUSN','FAMILLE NON DISPONIBLE')
      ENDIF

      END

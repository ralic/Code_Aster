      SUBROUTINE PANNO(TYPEMA,PAN,NSOM)

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
C ----------------------------------------------------------------------
C        PANS TOUCHANT LES NOEUDS PRINCIPAUX D'UN TYPE DE MAILLE
C ----------------------------------------------------------------------
C VARIABLE D'ENTREE
C CHARACTER*8  TYPEMA         : TYPE DE MAILLE
C
C VARIABLES D'ENTREE/SORTIE
C REAL*8       PAN(DIM,NSOM)  : PANS TOUCHANT LES NOEUDS PRINCIPAUX
C                              ( PAN1.1, PAN1.2, [PAN1.3], PAN2.1, ... )
C                                PAN*.1 PAN*.2 [PAN*.3] TOUCHENT LE 
C                                NOEUD *
C INTEGER      NSOM           : NOMBRE DE NOEUDS PRINCIPAUX
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- VARIABLES
      CHARACTER*8 TYPEMA      
      INTEGER PAN(*),NSOM

      IF (TYPEMA(1:4).EQ.'TRIA') THEN

        NSOM = 3

        PAN(1) = 3
        PAN(2) = 1

        PAN(3) = 1 
        PAN(4) = 2

        PAN(5) = 2
        PAN(6) = 3

      ELSEIF (TYPEMA(1:4).EQ.'QUAD') THEN

        NSOM = 4

        PAN(1) = 4
        PAN(2) = 1

        PAN(3) = 1 
        PAN(4) = 2

        PAN(5) = 2
        PAN(6) = 3

        PAN(7) = 3
        PAN(8) = 4

      ELSEIF (TYPEMA(1:5).EQ.'TETRA') THEN

        NSOM = 4

        PAN(1) = 1
        PAN(2) = 2
        PAN(3) = 3
 
        PAN(4) = 1
        PAN(5) = 2
        PAN(6) = 4

        PAN(7) = 1
        PAN(8) = 3
        PAN(9) = 4

        PAN(10) = 2
        PAN(11) = 3
        PAN(12) = 4

      ELSEIF (TYPEMA(1:5).EQ.'PENTA') THEN

        NSOM = 6

        PAN(1) = 1
        PAN(2) = 2
        PAN(3) = 4
 
        PAN(4) = 1
        PAN(5) = 2
        PAN(6) = 3

        PAN(7) = 1
        PAN(8) = 3
        PAN(9) = 4

        PAN(10) = 5
        PAN(11) = 2
        PAN(12) = 4

        PAN(13) = 5
        PAN(14) = 2
        PAN(15) = 3

        PAN(16) = 5
        PAN(17) = 3
        PAN(18) = 4


      ELSEIF (TYPEMA(1:4).EQ.'HEXA') THEN

        NSOM = 8

        PAN(1) = 1
        PAN(2) = 2
        PAN(3) = 5
 
        PAN(4) = 1
        PAN(5) = 2
        PAN(6) = 3

        PAN(7) = 1
        PAN(8) = 3
        PAN(9) = 4

        PAN(10) = 1
        PAN(11) = 4
        PAN(12) = 5

        PAN(13) = 2
        PAN(14) = 5
        PAN(15) = 6

        PAN(16) = 2
        PAN(17) = 3
        PAN(18) = 6

        PAN(19) = 3
        PAN(20) = 4
        PAN(21) = 6

        PAN(22) = 4
        PAN(23) = 5
        PAN(24) = 6

      ELSE

        CALL UTMESS('F','PANNO',TYPEMA//' INDISPONIBLE')

      ENDIF

      END

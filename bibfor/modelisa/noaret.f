      SUBROUTINE NOARET(TYPEMA,ARE,NARE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 08/11/2004   AUTEUR DURAND C.DURAND 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_20
C ----------------------------------------------------------------------
C               NOEUDS DEFINISSANT LES ARETES D'UNE MAILLE
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C CHARACTER*8  TYPEMA     : TYPE DE LA MAILLE
C
C VARIABLE DE SORTIE
C INTEGER      ARE(*)     : NOEUDS DES ARETES DE LA MAILLE
C                           ( NOMBRE DE NOEUDS ARETES 1, N1, N2, ...
C                             ..., NOMBRE DE NOEUDS ARETES 2, ... )
C INTEGER      NARE       : NOMBRE D'ARETES
C
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- VARIABLES
      CHARACTER*8 TYPEMA
      INTEGER ARE(*),NARE

      IF (TYPEMA(1:5).EQ.'TRIA3') THEN
        
        NARE = 3

        ARE(1) = 2
          ARE(2) = 1
          ARE(3) = 2

        ARE(4) = 2
          ARE(5) = 2
          ARE(6) = 3
          
        ARE(7) = 2
          ARE(8) = 3
          ARE(9) = 1

      ELSEIF (TYPEMA(1:4).EQ.'TRIA') THEN

        NARE = 3

        ARE(1) = 3
          ARE(2) = 1
          ARE(3) = 2
          ARE(4) = 4

        ARE(5) = 3
          ARE(6) = 2
          ARE(7) = 3
          ARE(8) = 5

        ARE(9) = 3
          ARE(10) = 3
          ARE(11) = 1
          ARE(12) = 6
        
      ELSEIF (TYPEMA(1:5).EQ.'QUAD4') THEN
        
        NARE = 4

        ARE(1) = 2
          ARE(2) = 1
          ARE(3) = 2

        ARE(4) = 2
          ARE(5) = 2
          ARE(6) = 3
          
        ARE(7) = 2
          ARE(8) = 3
          ARE(9) = 4

        ARE(10) = 2
          ARE(11) = 4
          ARE(12) = 1

      ELSEIF (TYPEMA(1:5).EQ.'QUAD6') THEN
        
        NARE = 4

        ARE(1) = 3
          ARE(2) = 1
          ARE(3) = 2
          ARE(4) = 5        

        ARE(5) = 2
          ARE(6) = 2
          ARE(7) = 3
          
        ARE(8) = 3
          ARE(9) = 3
          ARE(10) = 4
          ARE(11) = 6

        ARE(12) = 2
          ARE(13) = 4
          ARE(14) = 1

      ELSEIF (TYPEMA(1:4).EQ.'QUAD') THEN
        
        NARE = 4

        ARE(1) = 3
          ARE(2) = 1
          ARE(3) = 2
          ARE(4) = 5        

        ARE(5) = 3
          ARE(6) = 2
          ARE(7) = 3
          ARE(8) = 6
          
        ARE(9) = 3
          ARE(10) = 3
          ARE(11) = 4
          ARE(12) = 7

        ARE(13) = 3
          ARE(14) = 4
          ARE(15) = 1
          ARE(16) = 8

      ELSEIF (TYPEMA(1:6).EQ.'TETRA4') THEN

        NARE = 6

        ARE(1) = 2
          ARE(2) = 1
          ARE(3) = 2

        ARE(4) = 2
          ARE(5) = 2
          ARE(6) = 3

        ARE(7) = 2
          ARE(8) = 3
          ARE(9) = 1

        ARE(10) = 2
          ARE(11) = 1
          ARE(12) = 4

        ARE(13) = 2
          ARE(14) = 4
          ARE(15) = 2

        ARE(16) = 2
          ARE(17) = 4
          ARE(18) = 3

      ELSEIF (TYPEMA(1:7).EQ.'TETRA10') THEN

        NARE = 6

        ARE(1) = 3
          ARE(2) = 1
          ARE(3) = 2
          ARE(4) = 5

        ARE(5) = 3
          ARE(6) = 2
          ARE(7) = 3
          ARE(8) = 6

        ARE(9) = 3
          ARE(10) = 3
          ARE(11) = 1
          ARE(12) = 7

        ARE(13) = 3
          ARE(14) = 1
          ARE(15) = 4
          ARE(16) = 8

        ARE(17) = 3
          ARE(18) = 4
          ARE(19) = 2
          ARE(20) = 9

        ARE(21) = 3
          ARE(22) = 4
          ARE(23) = 3
          ARE(24) = 10

      ELSEIF (TYPEMA(1:6).EQ.'PENTA6') THEN

        NARE = 9
          
        ARE(1) = 2
          ARE(2) = 1
          ARE(3) = 2

        ARE(4) = 2
          ARE(5) = 2
          ARE(6) = 3

        ARE(7) = 2
          ARE(8) = 3
          ARE(9) = 1

        ARE(10) = 2
          ARE(11) = 1 
          ARE(12) = 4
 
        ARE(13) = 2
          ARE(14) = 2
          ARE(15) = 5

        ARE(16) = 2
          ARE(17) = 3
          ARE(18) = 6

        ARE(19) = 2
          ARE(20) = 4
          ARE(21) = 5

        ARE(22) = 2
          ARE(23) = 5
          ARE(24) = 6

        ARE(25) = 2
          ARE(26) = 6
          ARE(27) = 4

      ELSEIF ((TYPEMA(1:7).EQ.'PENTA12').OR.
     &        (TYPEMA(1:7).EQ.'PENTA14')) THEN

        NARE = 9
          
        ARE(1) = 3
          ARE(2) = 1
          ARE(3) = 2
          ARE(4) = 7

        ARE(5) = 3
          ARE(6) = 2
          ARE(7) = 3
          ARE(8) = 8

        ARE(9) = 3
          ARE(10) = 3
          ARE(11) = 1
          ARE(12) = 9

        ARE(13) = 2
          ARE(14) = 1 
          ARE(15) = 4
 
        ARE(16) = 2
          ARE(17) = 2
          ARE(18) = 5

        ARE(19) = 2
          ARE(20) = 3
          ARE(21) = 6

        ARE(22) = 3
          ARE(23) = 4
          ARE(24) = 5
          ARE(25) = 10

        ARE(26) = 3
          ARE(27) = 5
          ARE(28) = 6
          ARE(29) = 11

        ARE(30) = 3
          ARE(31) = 6
          ARE(32) = 4
          ARE(33) = 12

      ELSEIF (TYPEMA(1:7).EQ.'PENTA15') THEN

        NARE = 9
          
        ARE(1) = 3
          ARE(2) = 1
          ARE(3) = 2
          ARE(4) = 7

        ARE(5) = 3
          ARE(6) = 2
          ARE(7) = 3
          ARE(8) = 8

        ARE(9) = 3
          ARE(10) = 3
          ARE(11) = 1
          ARE(12) = 9

        ARE(13) = 3
          ARE(14) = 1 
          ARE(15) = 4
          ARE(16) = 10
 
        ARE(17) = 3
          ARE(18) = 2
          ARE(19) = 5
          ARE(20) = 11

        ARE(21) = 3
          ARE(22) = 3
          ARE(23) = 6
          ARE(24) = 12

        ARE(25) = 3
          ARE(26) = 4
          ARE(27) = 5
          ARE(28) = 13

        ARE(29) = 3
          ARE(30) = 5
          ARE(31) = 6
          ARE(32) = 14

        ARE(33) = 3
          ARE(34) = 6
          ARE(35) = 4
          ARE(36) = 15

      ELSEIF (TYPEMA(1:5).EQ.'HEXA8') THEN

        NARE = 12

        ARE(1) = 2
          ARE(2) = 1
          ARE(3) = 2

        ARE(4) = 2
          ARE(5) = 2
          ARE(6) = 3

        ARE(7) = 2
          ARE(8) = 3
          ARE(9) = 4

        ARE(10) = 2
          ARE(11) = 4
          ARE(12) = 1

        ARE(13) = 2
          ARE(14) = 1
          ARE(15) = 5

        ARE(16) = 2
          ARE(17) = 2
          ARE(18) = 6

        ARE(19) = 2
          ARE(20) = 3
          ARE(21) = 7

        ARE(22) = 2
          ARE(23) = 4
          ARE(24) = 8

        ARE(25) = 2
          ARE(26) = 5
          ARE(27) = 6

        ARE(28) = 2
          ARE(29) = 6
          ARE(30) = 7

        ARE(31) = 2
          ARE(32) = 7
          ARE(33) = 8

        ARE(34) = 2
          ARE(35) = 8
          ARE(36) = 5

      ELSEIF((TYPEMA(1:6).EQ.'HEXA16').OR.(TYPEMA(1:6).EQ.'HEXA18'))THEN

        NARE = 12

        ARE(1) = 2
          ARE(2) = 1
          ARE(3) = 2

        ARE(4) = 3
          ARE(5) = 2
          ARE(6) = 3
          ARE(7) = 9

        ARE(8) = 2
          ARE(9) = 3
          ARE(10) = 4

        ARE(11) = 3
          ARE(12) = 4
          ARE(13) = 1
          ARE(14) = 10

        ARE(15) = 3
          ARE(16) = 1
          ARE(17) = 5
          ARE(18) = 11

        ARE(19) = 3
          ARE(20) = 2
          ARE(21) = 6
          ARE(22) = 12

        ARE(23) = 3
          ARE(24) = 3
          ARE(25) = 7
          ARE(26) = 13

        ARE(27) = 3
          ARE(28) = 4
          ARE(29) = 8
          ARE(30) = 14

        ARE(31) = 2
          ARE(32) = 5
          ARE(33) = 6

        ARE(34) = 3
          ARE(35) = 6
          ARE(36) = 7
          ARE(37) = 15

        ARE(38) = 2
          ARE(39) = 7
          ARE(40) = 8

        ARE(41) = 3
          ARE(42) = 8
          ARE(43) = 5
          ARE(44) = 16

      ELSEIF (TYPEMA(1:4).EQ.'HEXA') THEN

        NARE = 12

        ARE(1) = 3
          ARE(2) = 1
          ARE(3) = 2
          ARE(4) = 9

        ARE(5) = 3
          ARE(6) = 2
          ARE(7) = 3
          ARE(8) = 10

        ARE(9) = 3
          ARE(10) = 3
          ARE(11) = 4
          ARE(12) = 11

        ARE(13) = 3
          ARE(14) = 4
          ARE(15) = 1
          ARE(16) = 12

        ARE(17) = 3
          ARE(18) = 1
          ARE(19) = 5
          ARE(20) = 13

        ARE(21) = 3
          ARE(22) = 2
          ARE(23) = 6
          ARE(24) = 14

        ARE(25) = 3
          ARE(26) = 3
          ARE(27) = 7
          ARE(28) = 15

        ARE(29) = 3
          ARE(30) = 4
          ARE(31) = 8
          ARE(32) = 16

        ARE(33) = 3
          ARE(34) = 5
          ARE(35) = 6
          ARE(36) = 17

        ARE(37) = 3
          ARE(38) = 6
          ARE(39) = 7
          ARE(40) = 18

        ARE(41) = 3
          ARE(42) = 7
          ARE(43) = 8
          ARE(44) = 19

        ARE(45) = 3
          ARE(46) = 8
          ARE(47) = 5
          ARE(48) = 20

      ELSE

       CALL UTMESS('F','NOARET',TYPEMA//' INDISPONIBLE')

      ENDIF

      END

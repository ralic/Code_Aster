      SUBROUTINE NOCOQU(DIME,NNO,NOECOQ)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 02/04/2002   AUTEUR RATEAU G.RATEAU 
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
C           NOEUDS DEFINISSANT LE PASSAGE D'UNE MAILLE DE COQUE 
C                         EN UNE MAILLE VOLUMIQUE 
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C INTEGER  DIME          : DIMENSION DE L'ESPACE
C INTEGER  NNO           : NOMBRE DE NOEUDS DE LA MAILLE DE COQUE
C
C VARIABLES DE SORTIE
C INTEGER  NOECOQ(2,NNO) : NOEUDS POUR LE PASSAGE D'UNE MAILLE DE COQUE
C                          EN UNE MAILLE VOLUMIQUE
C                         ( NO1.1, NO1.2, NO2.1, NO2.2, NO3.1, ...)
C
C        NO1.1 x--------------x NO2.1   --- CONTOUR DE LA 
C              |              |             MAILLE VOLUMIQUE
C          NO1 x==============x NO2         
C              |              |         === FIBRE MOYENNE DE 
C        NO1.2 x--------------x NO2.2       LA COQUE
C
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- VARIABLES
      INTEGER  DIME,NNO,NOECOQ(2,*)

C --- 2D

      IF (DIME.EQ.2) THEN

C ----- QUAD6

        IF (NNO.EQ.3) THEN

          NOECOQ(1,1) = 1
          NOECOQ(2,1) = 4
          NOECOQ(1,2) = 2
          NOECOQ(2,2) = 3
          NOECOQ(1,3) = 5          
          NOECOQ(2,3) = 6

        ELSE

          CALL UTMESS('F','NOCOQU','MAILLE NON DISPONIBLE')

        ENDIF

C --- 3D

      ELSEIF (DIME.EQ.3) THEN

C ----- PENTA6

        IF (NNO.EQ.3) THEN
 
          NOECOQ(1,1) = 1
          NOECOQ(2,1) = 4
          NOECOQ(1,2) = 2
          NOECOQ(2,2) = 5
          NOECOQ(1,3) = 3
          NOECOQ(2,3) = 6

C ----- PENTA12

        ELSEIF (NNO.EQ.6) THEN

          NOECOQ(1,1) = 1
          NOECOQ(2,1) = 4
          NOECOQ(1,2) = 2
          NOECOQ(2,2) = 5
          NOECOQ(1,3) = 3
          NOECOQ(2,3) = 6
          NOECOQ(1,4) = 7
          NOECOQ(2,4) = 10
          NOECOQ(1,5) = 8
          NOECOQ(2,5) = 11
          NOECOQ(1,6) = 9
          NOECOQ(2,6) = 12

C ----- PENTA14

        ELSEIF (NNO.EQ.7) THEN

          NOECOQ(1,1) = 1
          NOECOQ(2,1) = 4
          NOECOQ(1,2) = 2
          NOECOQ(2,2) = 5
          NOECOQ(1,3) = 3
          NOECOQ(2,3) = 6
          NOECOQ(1,4) = 7
          NOECOQ(2,4) = 10
          NOECOQ(1,5) = 8
          NOECOQ(2,5) = 11
          NOECOQ(1,6) = 9
          NOECOQ(2,6) = 12
          NOECOQ(1,7) = 13
          NOECOQ(2,7) = 14

C ----- HEXA8

        ELSEIF (NNO.EQ.4) THEN

          NOECOQ(1,1) = 1
          NOECOQ(2,1) = 2
          NOECOQ(1,2) = 4
          NOECOQ(2,2) = 3
          NOECOQ(1,3) = 8
          NOECOQ(2,3) = 7
          NOECOQ(1,4) = 5
          NOECOQ(2,4) = 6

C ----- HEXA16

        ELSEIF (NNO.EQ.8) THEN

          NOECOQ(1,1) = 1
          NOECOQ(2,1) = 2
          NOECOQ(1,2) = 4
          NOECOQ(2,2) = 3
          NOECOQ(1,3) = 8
          NOECOQ(2,3) = 7
          NOECOQ(1,4) = 5
          NOECOQ(2,4) = 6
          NOECOQ(1,5) = 10
          NOECOQ(2,5) = 9
          NOECOQ(1,6) = 14
          NOECOQ(2,6) = 13
          NOECOQ(1,7) = 16
          NOECOQ(2,7) = 15
          NOECOQ(1,8) = 11
          NOECOQ(2,8) = 12

C ----- HEXA18

        ELSEIF (NNO.EQ.9) THEN

          NOECOQ(1,1) = 1
          NOECOQ(2,1) = 2
          NOECOQ(1,2) = 4
          NOECOQ(2,2) = 3
          NOECOQ(1,3) = 8
          NOECOQ(2,3) = 7
          NOECOQ(1,4) = 5
          NOECOQ(2,4) = 6
          NOECOQ(1,5) = 10
          NOECOQ(2,5) = 9
          NOECOQ(1,6) = 14
          NOECOQ(2,6) = 13
          NOECOQ(1,7) = 16
          NOECOQ(2,7) = 15
          NOECOQ(1,8) = 11
          NOECOQ(2,8) = 12
          NOECOQ(1,9) = 17
          NOECOQ(2,9) = 18

        ELSE

          CALL UTMESS('F','NOCOQU','MAILLE NON DISPONIBLE')

        ENDIF
 
      ENDIF
 
      END

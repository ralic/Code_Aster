      SUBROUTINE NOAREL(DIM,NNO,NOEUD,NARE)

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
C         NOEUDS DEFINISSANT LES ARETES LINEAIRES D'UNE MAILLE
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C INTEGER  DIM      : DIMENSION DE L'ESPACE
C INTEGER  NNO      : NOMBRE DE NOEUDS DE LA MAILLE
C
C VARIABLES DE SORTIE
C INTEGER  NOEUD(*) : NOEUDS DES ARETES LINEAIRES DE LA MAILLE   
C                    ( NO1.1, NO1.2, NO2.1, NO2.2, NO3.1, ... )
C                      NO*.1 ET NO*.2 SONT LES EXTREMITES DE L'ARETE *
C INTEGER  NARE     : NOMBRE D'ARETES 
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- VARIABLES
      INTEGER DIM,NNO,NOEUD(*),NARE

      IF (DIM.EQ.2) THEN

        IF (NNO.EQ.3) THEN

          NARE = 3

          NOEUD(1) = 1
          NOEUD(2) = 2

          NOEUD(3) = 2
          NOEUD(4) = 3
          
          NOEUD(5) = 3
          NOEUD(6) = 1

        ELSEIF (NNO.EQ.4) THEN

          NARE = 4

          NOEUD(1) = 1
          NOEUD(2) = 2

          NOEUD(3) = 2
          NOEUD(4) = 3
          
          NOEUD(5) = 3
          NOEUD(6) = 4

          NOEUD(7) = 4
          NOEUD(8) = 1

        ELSE

          GOTO 10

        ENDIF

      ELSE

        IF (NNO.EQ.4) THEN

          NARE = 6

          NOEUD(1) = 1
          NOEUD(2) = 2

          NOEUD(3) = 2
          NOEUD(4) = 3

          NOEUD(5) = 3
          NOEUD(6) = 1

          NOEUD(7) = 1
          NOEUD(8) = 4

          NOEUD(9) = 4
          NOEUD(10) = 2

          NOEUD(11) = 4
          NOEUD(12) = 3

        ELSEIF (NNO.EQ.6) THEN

          NARE = 9
          
          NOEUD(1) = 1
          NOEUD(2) = 2

          NOEUD(3) = 2
          NOEUD(4) = 3

          NOEUD(5) = 3
          NOEUD(6) = 1

          NOEUD(7) = 1 
          NOEUD(8) = 4
 
          NOEUD(9) = 2
          NOEUD(10) = 5

          NOEUD(11) = 3
          NOEUD(12) = 6

          NOEUD(13) = 4
          NOEUD(14) = 5

          NOEUD(15) = 5
          NOEUD(16) = 6

          NOEUD(17) = 6
          NOEUD(18) = 4

        ELSEIF (NNO.EQ.8) THEN

          NARE = 12

          NOEUD(1) = 1
          NOEUD(2) = 2

          NOEUD(3) = 2
          NOEUD(4) = 3

          NOEUD(5) = 3
          NOEUD(6) = 4

          NOEUD(7) = 4
          NOEUD(8) = 1

          NOEUD(9) = 1
          NOEUD(10) = 5

          NOEUD(11) = 2
          NOEUD(12) = 6

          NOEUD(13) = 3
          NOEUD(14) = 7

          NOEUD(15) = 4
          NOEUD(16) = 8

          NOEUD(17) = 5
          NOEUD(18) = 6

          NOEUD(19) = 6
          NOEUD(20) = 7

          NOEUD(21) = 7
          NOEUD(22) = 8

          NOEUD(23) = 8
          NOEUD(24) = 5

        ELSE
           
          GOTO 10

        ENDIF

      ENDIF

      GOTO 20

 10   CONTINUE

      CALL UTMESS('F','NOAREL','TYPE DE MAILLE INDISPONIBLE')

 20   CONTINUE

      END

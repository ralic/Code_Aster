      SUBROUTINE ARLPAN(NNR,ARE,NPAN)

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
C
C ----------------------------------------------------------------------
C              ARETES LINEAIRES DES FACES DES MAILLES 3D
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE 
C INTEGER  NNR   : NOMBRE DE NOEUDS DE LA MAILLE LINEAIRE
C                    NNR = 4 : TETRAEDRE
C                    NNR = 6 : PENTAEDRE
C                    NNR = 8 : HEXAEDRE
C
C VARIABLE DE SORTIE
C INTEGER ARE(*) : ARETES LINEAIRES ASSOCIEES AUX FACES
C                 ( NB ARETES FACE.1, FACE.1.ARETE.1, FACE.1.ARETE.2,
C                   ..., NB ARETES FACE.2, FACE.2.ARETE.1, ... )
C                    FACE.* INDEX SUIVANT PANNO 
C                    ARETE.* INDEX SUIVANT NOAREL
C                    SI ARETE.* > 0 : MEME SENS QUE DANS NOAREL
C                    SI ARETE.* < 0 : SENS OPPOSE
C INTEGER NPAN   : NOMBRE DE FACE DE LA MAILLE

      IMPLICIT NONE

C --- VARIABLES
      INTEGER     NNR,ARE(*),NPAN

      IF (NNR.EQ.4) THEN
        NPAN = 4
        ARE(1) = 3
          ARE(2) = -3
          ARE(3) = -2
          ARE(4) = -1
        ARE(5) = 3
          ARE(6) = 1
          ARE(7) = -5
          ARE(8) = -4
        ARE(9) = 3
          ARE(10) = 4
          ARE(11) = 6
          ARE(12) = 3
        ARE(13) = 3
          ARE(14) = 2
          ARE(15) = -6
          ARE(16) = 5
      ELSEIF (NNR.EQ.6) THEN
        NPAN = 5
        ARE(1) = 3
          ARE(2) = -3
          ARE(3) = -2
          ARE(4) = -1
        ARE(5) = 3
          ARE(6) = 7
          ARE(7) = 8
          ARE(8) = 9
        ARE(9) = 4
          ARE(10) = 1
          ARE(11) = 5
          ARE(12) = -7
          ARE(13) = -4
        ARE(14) = 4
          ARE(15) = 2
          ARE(16) = 6
          ARE(17) = -8
          ARE(18) = -5
        ARE(19) = 4
          ARE(20) = 3
          ARE(21) = 4
          ARE(22) = -9
          ARE(23) = -6
      ELSEIF (NNR.EQ.8) THEN
        NPAN = 6 
        ARE(1) = 4
          ARE(2) = -4
          ARE(3) = -3
          ARE(4) = -2
          ARE(5) = -1
        ARE(6) = 4
          ARE(7) = 1
          ARE(8) = 6
          ARE(9) = -9
          ARE(10) = -5
        ARE(11) = 4
          ARE(12) = 2
          ARE(13) = 7
          ARE(14) = -10
          ARE(15) = -6
        ARE(16) = 4
          ARE(17) = 3
          ARE(18) = 8
          ARE(19) = -11
          ARE(20) = -7
        ARE(21) = 4
          ARE(22) = 4
          ARE(23) = 5
          ARE(24) = -12
          ARE(25) = -8
        ARE(26) = 4
          ARE(27) = 9
          ARE(28) = 10
          ARE(29) = 11
          ARE(30) = 12
      ELSE
        CALL UTMESS('F','ARLPAN','TYPE DE MAILLE INDISPONIBLE')
      ENDIF  

      END

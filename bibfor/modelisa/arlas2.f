      SUBROUTINE ARLAS2(DIM,DD,CNX,CNXC,TANG,NN1,IM1,NN2,IJ,B0,B)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 04/04/2002   AUTEUR VABHHTS J.PELLET 
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
C  ASSEMBLAGE MATRICE ELEMENTAIRE ARLEQUIN MAILLE COQUE / MAILLE SOLIDE
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE 
C INTEGER      DIM               : DIMENSION DE L'ESPACE             
C INTEGER      DD                : LONGUEUR MATRICE NOEUD (CF ARLFAC)
C INTEGER      CNX(*)            : COLLECTION CONNECTIVITE DU MAILLAGE
C INTEGER      CNXC(*)           : LONGUEUR CUMULEE ASSOCIEE A CNX
C REAL*8       TANG(DIM,DIM-1,*) : TANGENTES LISSEES COQUE (CF LISNOR)
C INTEGER      NN1               : NOMBRE DE COLONNES DE B0
C INTEGER      IM1               : MAILLE LIGNE DANS B (INDEX CNXC)
C INTEGER      NN2               : NOMBRE DE LIGNES DE B0             
C INTEGER      IJ(NN2,NN1)       : POINTEURS DANS B (CF ARLAS0)
C
C VARIABLE D'ENTREE/SORTIE
C REAL*8       B0(NN2,NN1)       : MATRICE ELEMENTAIRE ARLEQUIN
C REAL*8       B(*)              : MATRICE ARLEQUIN MORSE (CF ARLFAC)
C ----------------------------------------------------------------------
C                    ATTENTION : B0 EST REMISE A ZERO
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- VARIABLES
      INTEGER DIM,DD,CNX(*),CNXC(*),IM1,NN1,NN2,IJ(NN2,*)
      INTEGER NOECOQ(2,9),NNR,DTG,INO1,I,J,I1,I2,P,Q
      REAL*8  TANG(DIM*(DIM-1),*),B0(NN2,*),B(*),R1,R2

C --- ASSEMBLAGE

      NNR = NN1/2
      P = CNXC(IM1)
      DTG = DIM*(DIM-1)

      CALL NOCOQU(DIM,NNR,NOECOQ)

      DO 10 I = 1, NNR

        I1 = NOECOQ(1,I)
        I2 = NOECOQ(2,I)

        INO1 = CNX(P)
        P = P + 1

        DO 10 J = 1, NN2

          R1 = B0(J,I1)
          R2 = B0(J,I2)
          B0(J,I1) = 0.D0
          B0(J,I2) = 0.D0
          Q = 1 + DD*(IJ(J,I)-1)

          B(Q) = B(Q) + R2 + R1
          Q = Q + 1

          CALL R8AXPY(DTG,R2 - R1,TANG(1,INO1),1,B(Q),1)
          Q = Q + DTG

 10   CONTINUE

      END

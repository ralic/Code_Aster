      SUBROUTINE ARLAS3(DIM,DD,EP,NN1,NO2,NN2,IJ,B0,B)

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
C  ASSEMBLAGE MATRICE ELEMENTAIRE ARLEQUIN MAILLE SOLIDE / MAILLE COQUE
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE 
C INTEGER      DIM         : DIMENSION DE L'ESPACE             
C INTEGER      DD          : LONGUEUR MATRICE NOEUD (CF ARLFAC)
C REAL*8       EP          : EPAISSEUR COQUE
C INTEGER      NN1         : NOMBRE DE COLONNES DE B0
C REAL*8       NO2         : COORDONNEES NOEUDS MAILLE COLONNE DANS B
C INTEGER      NN2         : NOMBRE DE LIGNES DE B0             
C INTEGER      IJ(NN2,NN1) : POINTEURS DANS B (CF ARLAS0)
C
C VARIABLE D'ENTREE/SORTIE
C REAL*8       B0(NN2,NN1) : MATRICE ELEMENTAIRE ARLEQUIN
C REAL*8       B(*)        : MATRICE ARLEQUIN MORSE (CF ARLFAC)
C ----------------------------------------------------------------------
C                  ATTENTION : B0 EST REMISE A ZERO
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- FONCTIONS
      REAL*8  R8NRM2

C --- VARIABLES
      INTEGER DIM,DD,NN1,NN2,NNR,IJ(NN2/2,*)
      INTEGER NOECOQ(2,9),P,Q,I,J,J1,J2
      REAL*8  NO2(*),EP,B0(NN2,*),B(*)
      REAL*8  R,R1,R2,NORMAL(3),TANG(54)

      P = 1
      NNR = NN2 / 2
      CALL NOCOQU(DIM,NNR,NOECOQ)

C --- COQUE 2D

      IF (DIM.EQ.2) THEN

        CALL TANGNT(NO2,NNR,DIM,0,1,TANG)

        DO 10 J = 1, NNR

          R = EP / R8NRM2(2,TANG(P),1)
    
          NORMAL(1) = -TANG(P+1)*R
          NORMAL(2) =  TANG(P)*R

          P = P + 2

          J1 = NOECOQ(1,J)
          J2 = NOECOQ(2,J)

          DO 10 I = 1, NN1

            R1 = B0(J1,I)
            R2 = B0(J2,I)
            B0(J1,I) = 0.D0
            B0(J2,I) = 0.D0

            Q = 1 + DD*(IJ(J,I)-1)

            B(Q) = B(Q) + R2 + R1
            Q = Q + 1

            CALL R8AXPY(2,R2 - R1,NORMAL,1,B(Q),1)
            Q = Q + 2

 10     CONTINUE

C --- COQUE FACETTE

      ELSEIF ((NNR.EQ.3).OR.(NNR.EQ.4)) THEN

        CALL TANGNT(NO2,NNR,DIM,1,1,TANG)

        NORMAL(1) = TANG(2)*TANG(6) - TANG(5)*TANG(3)
        NORMAL(2) = TANG(3)*TANG(4) - TANG(6)*TANG(1)
        NORMAL(3) = TANG(1)*TANG(5) - TANG(4)*TANG(2)
         
        R = EP / R8NRM2(3,NORMAL,1)
        CALL R8SCAL(3,R,NORMAL,1)

        DO 20 J = 1, NNR

          J1 = NOECOQ(1,J)
          J2 = NOECOQ(2,J)

          DO 20 I = 1, NN1

            R1 = B0(J1,I)
            R2 = B0(J2,I)
            B0(J1,I) = 0.D0
            B0(J2,I) = 0.D0

            Q = 1 + DD*(IJ(J,I)-1)

            B(Q) = B(Q) + R2 + R1
            Q = Q + 1

            CALL R8AXPY(3,R2 - R1,NORMAL,1,B(Q),1)
            Q = Q + 3

 20     CONTINUE

C --- COQUE VOLUMIQUE

      ELSE

        CALL TANGNT(NO2,NNR,DIM,0,1,TANG)

        DO 30 J = 1, NNR

          NORMAL(1) = TANG(P+1)*TANG(P+5) - TANG(P+4)*TANG(P+2)
          NORMAL(2) = TANG(P+2)*TANG(P+3) - TANG(P+5)*TANG(P  )
          NORMAL(3) = TANG(P  )*TANG(P+4) - TANG(P+3)*TANG(P+1)
         
          R = EP / R8NRM2(3,NORMAL,1)
          CALL R8SCAL(3,R,NORMAL,1)

          P = P + 6

          J1 = NOECOQ(1,J)
          J2 = NOECOQ(2,J)

          DO 30 I = 1, NN1

            R1 = B0(J1,I)
            R2 = B0(J2,I)
            B0(J1,I) = 0.D0
            B0(J2,I) = 0.D0

            Q = 1 + DD*(IJ(J,I)-1)

            IF (J.NE.NNR) B(Q) = B(Q) + R2 + R1
            Q = Q + 1

            CALL R8AXPY(3,R2 - R1,NORMAL,1,B(Q),1)
            Q = Q + 3

 30     CONTINUE

      ENDIF

      END

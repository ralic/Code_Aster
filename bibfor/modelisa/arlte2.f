      SUBROUTINE ARLTE2(DIM,PG,FG,DFG,NG,NO,NNO,MQ,B)

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
C               MATRICE ELEMENTAIRE DE COUPLAGE ARLEQUIN
C                              MEME MAILLE
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE 
C INTEGER  DIM             : DIMENSION DE L'ESPACE
C REAL*8   PG(NG)          : POIDS DE GAUSS
C REAL*8   FG(NNO,NG)      : FONCTIONS DE FORME AUX POINTS DE GAUSS
C REAL*8   DFG(DIM,NNO,NG) : DERIVEES FONCTIONS DE FORME AUX PTS GAUSS
C INTEGER  NG              : NOMBRE DE POINTS DE GAUSS
C REAL*8   NO(DIM,NNO)     : COORDONNEES NOEUDS DE LA MAILLE (CF CONOEU)
C INTEGER  NNO             : NOMBRE DE NOEUDS DE LA MAILLE
C REAL*8   MQ(DIM,DIM)     : METRIQUE D'ADIMENSIONEMENT (CF ARLMTR)
C
C VARIABLES D'ENTREE / SORTIE
C REAL*8   B(NNO,NNO)      : MATRICE ELEMENTAIRE DE COUPLAGE ARLEQUIN
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- FONCTIONS
      REAL*8  R8DOT

C --- VARIABLES
      LOGICAL IRET
      INTEGER DIM,NG,NNO,NN,I,J,K,P0,P1,P2
      REAL*8  PG(*),NO(*),FG(NNO,*),DFG(*),MQ(*),B(NNO,*)
      REAL*8  JAC(9),DF0(81),DF(81),DET,R0,R1

C --- INTEGRATION

      P0 = 1
      DET = 1.D0
      IRET = .TRUE.
      NN = DIM*NNO

      DO 10 I = 1, NG

        CALL R8COPY(NN,DFG(P0),1,DF0,1)
        CALL MTPROD(NO,DIM,0,DIM,0,NNO,DF0,DIM,0,DIM,0,JAC)
        CALL MGAUST(JAC,DF0,DIM,DIM,NNO,DET,IRET)
        CALL MMPROD(MQ,DIM,0,DIM,0,DIM,DF0,DIM,0,0,NNO,DF)
        DET = ABS(DET) * PG(I)

        IF (.NOT.IRET) CALL UTMESS('F','ARLTE2','MAILLE NON CONFORME')

        P1 = 1
        P0 = P0 + NN

        DO 10 J = 1, NNO

          P2 = 1
          R0 = FG(J,I)
          
          DO 20 K = 1, J

            R1 = R8DOT(DIM,DF(P1),1,DF(P2),1)
            B(K,J) = B(K,J) + DET*(R1 + R0*FG(K,I))
C            B(K,J) = B(K,J) + DET*R0*FG(K,I)
C            B(K,J) = B(K,J) + DET*R1
            P2 = P2 + DIM

 20       CONTINUE

          P1 = P1 + DIM

 10   CONTINUE

C --- SYMETRIE

      DO 30 I = 2, NNO

        DO 30 J = 1, I-1

          B(I,J) = B(J,I)

 30   CONTINUE

      END

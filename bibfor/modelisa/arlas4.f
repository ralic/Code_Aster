      SUBROUTINE ARLAS4(DIM,DD,CNX,CNXC,T1,EP,NN1,IM1,NO2,NN2,IJ,B0,B)

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
C  ASSEMBLAGE MATRICE ELEMENTAIRE ARLEQUIN MAILLE COQUE / MAILLE COQUE
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE 
C INTEGER   DIM              : DIMENSION DE L'ESPACE             
C INTEGER   DD               : LONGUEUR MATRICE POINT (CF ARLFAC)
C INTEGER   CNX(*)           : COLLECTION CONNECTIVITE DU MAILLAGE
C INTEGER   CNXC(*)          : LONGUEUR CUMULEE ASSOCIEE A CNX
C REAL*8    T1(DIM,DIM-1,*)  : TANGENTES LISSEES COQUE (CF LISNOR)
C REAL*8    EP               : EPAISSEUR COQUE
C INTEGER   NN1              : NOMBRE DE COLONNES DE B0
C INTEGER   IM1              : MAILLE LIGNE DANS B (INDEX CNXC)
C REAL*8    NO2              : COORDONNEES NOEUDS MAILLE COLONNE DANS B
C INTEGER   NN2              : NOMBRE DE LIGNES DE B0             
C INTEGER   IJ(NN2,NN1)      : POINTEURS DANS B (CF ARLAS0)
C
C VARIABLE D'ENTREE/SORTIE
C REAL*8    B0(NN2,NN1)      : MATRICE ELEMENTAIRE ARLEQUIN
C REAL*8    B(*)             : MATRICE ARLEQUIN MORSE (CF ARLFAC)
C ----------------------------------------------------------------------
C                   ATTENTION : B0 EST REMISE A ZERO
C ----------------------------------------------------------------------

      IMPLICIT NONE
      
C --- FONCTIONS
      REAL*8  R8NRM2

C --- VARIABLES
      INTEGER DIM,DD,CNX(*),CNXC(*),NN1,IM1,NN2,NR1,NR2,IJ(NN2/2,*)
      INTEGER NOCOQ1(2,9),NOCOQ2(2,9),N1,P0,P1,P2,Q,I,J,I1,I2,J1,J2
      REAL*8  T1(DIM*(DIM-1),*),NO2(*),EP,B0(NN2,*),B(*)
      REAL*8  N(3),T2(54),R11,R12,R21,R22,R

      NR1 = NN1/2
      NR2 = NN2/2

      P0 = CNXC(IM1)

      CALL NOCOQU(DIM,NR1,NOCOQ1)
      CALL NOCOQU(DIM,NR2,NOCOQ2)

C --- COQUE 2D

      IF (DIM.EQ.2) THEN

        CALL TANGNT(NO2,NR2,DIM,0,1,T2)

        P2 = 1

        DO 10 J = 1, NR2

          R = EP / R8NRM2(2,T2(P2),1)

          N(1) = -T2(P2+1)*R
          N(2) =  T2(P2)*R

          P2 = P2 + 2

          J1 = NOCOQ2(1,J)
          J2 = NOCOQ2(2,J)

          P1 = P0

          DO 10 I = 1, NR1

            N1 = CNX(P1)
            P1 = P1 + 1

            I1 = NOCOQ1(1,I)
            I2 = NOCOQ1(2,I)

            R11 = B0(J1,I1)
            R21 = B0(J2,I1)
            R12 = B0(J1,I2)
            R22 = B0(J2,I2)

            B0(J1,I1) = 0.D0
            B0(J2,I1) = 0.D0
            B0(J1,I2) = 0.D0
            B0(J2,I2) = 0.D0

            Q = 1 + DD*(IJ(J,I)-1)

            R = R22 + R21 + R12 + R11
            B(Q) = B(Q) + R
            Q = Q + 1

            R = R22 + R21 - R12 - R11
            CALL R8AXPY(2,R,N,1,B(Q),1)
            Q = Q + 2
            
            R = R22 - R21 + R12 - R11 
            CALL R8AXPY(2,R,T1(1,N1),1,B(Q),1)
            Q = Q + 2

            R = R22 - R21 - R12 + R11
            B(Q) = B(Q) + R*(N(1)*T1(2,N1) - N(2)*T1(1,N1))
            Q = Q + 1

 10     CONTINUE
            
C --- COQUE FACETTE
        
      ELSEIF ((NR2.EQ.3).OR.(NR2.EQ.4)) THEN

        CALL TANGNT(NO2,NR2,DIM,1,1,T2)

        N(1) = T2(2)*T2(6) - T2(5)*T2(3)
        N(2) = T2(3)*T2(4) - T2(6)*T2(1)
        N(3) = T2(1)*T2(5) - T2(4)*T2(2)

        R = EP / R8NRM2(3,N,1)
        CALL R8SCAL(3,R,N,1)

        DO 20 I = 1, NR1

          N1 = CNX(P0)
          P0 = P0 + 1

          I1 = NOCOQ1(1,I)
          I2 = NOCOQ1(2,I)
          
          DO 20 J = 1, NR2

            J1 = NOCOQ2(1,J)
            J2 = NOCOQ2(2,J)
            
            R11 = B0(J1,I1)
            R21 = B0(J2,I1)
            R12 = B0(J1,I2)
            R22 = B0(J2,I2)

            B0(J1,I1) = 0.D0
            B0(J2,I1) = 0.D0
            B0(J1,I2) = 0.D0
            B0(J2,I2) = 0.D0

            Q = 1 + DD*(IJ(J,I)-1)

            R = R22 + R21 + R12 + R11
            B(Q) = B(Q) + R
            Q = Q + 1

            R = R22 + R21 - R12 - R11
            CALL R8AXPY(3,R,N,1,B(Q),1)
            Q = Q + 3
            
            R = R22 - R21 + R12 - R11 
            CALL R8AXPY(6,R,T1(1,N1),1,B(Q),1)
            Q = Q + 6

            R = R22 - R21 - R12 + R11
            B(Q  ) = B(Q  ) + R*(N(2)*T1(3,N1) - N(3)*T1(2,N1))
            B(Q+1) = B(Q+1) + R*(N(3)*T1(1,N1) - N(1)*T1(3,N1))
            B(Q+2) = B(Q+2) + R*(N(1)*T1(2,N1) - N(2)*T1(1,N1))
            B(Q+3) = B(Q+3) + R*(N(2)*T1(6,N1) - N(3)*T1(5,N1))
            B(Q+4) = B(Q+4) + R*(N(3)*T1(4,N1) - N(1)*T1(6,N1))
            B(Q+5) = B(Q+5) + R*(N(1)*T1(5,N1) - N(2)*T1(4,N1))
            Q = Q + 6

 20     CONTINUE

C --- COQUE VOLUMIQUE

      ELSE

        CALL TANGNT(NO2,NR2,DIM,0,1,T2)

        DO 30 J = 1, NR2

          N(1) = T2(P2+1)*T2(P2+5) - T2(P2+4)*T2(P2+2)
          N(2) = T2(P2+2)*T2(P2+3) - T2(P2+5)*T2(P2  )          
          N(3) = T2(P2  )*T2(P2+4) - T2(P2+3)*T2(P2+1)

          R = EP / R8NRM2(3,N,1)
          CALL R8SCAL(3,R,N,1)

          P2 = P2 + 6

          J1 = NOCOQ2(1,J)
          J2 = NOCOQ2(2,J)

          P1 = P0

          DO 30 I = 1, NR1

            N1 = CNX(P1)
            P1 = P1 + 1

            I1 = NOCOQ1(1,I)
            I2 = NOCOQ1(2,I)

            R11 = B0(J1,I1)
            R21 = B0(J2,I1)
            R12 = B0(J1,I2)
            R22 = B0(J2,I2)

            B0(J1,I1) = 0.D0
            B0(J2,I1) = 0.D0
            B0(J1,I2) = 0.D0
            B0(J2,I2) = 0.D0

            Q = 1 + DD*(IJ(J,I)-1)

            IF (J.NE.NR2) THEN
          
              R = R22 + R21 + R12 + R11
              B(Q) = B(Q) + R
              Q = Q + 1

              R = R22 + R21 - R12 - R11
              CALL R8AXPY(3,R,N,1,B(Q),1)
              Q = Q + 3

            ENDIF

            R = R22 - R21 + R12 - R11 
            CALL R8AXPY(6,R,T1(1,N1),1,B(Q),1)
            Q = Q + 6

            R = R22 - R21 - R12 + R11
            B(Q  ) = B(Q  ) + R*(N(2)*T1(3,N1) - N(3)*T1(2,N1))
            B(Q+1) = B(Q+1) + R*(N(3)*T1(1,N1) - N(1)*T1(3,N1))
            B(Q+2) = B(Q+2) + R*(N(1)*T1(2,N1) - N(2)*T1(1,N1))
            B(Q+3) = B(Q+3) + R*(N(2)*T1(6,N1) - N(3)*T1(5,N1))
            B(Q+4) = B(Q+4) + R*(N(3)*T1(4,N1) - N(1)*T1(6,N1))
            B(Q+5) = B(Q+5) + R*(N(1)*T1(5,N1) - N(2)*T1(4,N1))
            Q = Q + 6

 30     CONTINUE

      ENDIF

      END

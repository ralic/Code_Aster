      SUBROUTINE ARLCH2(DIM,NC,S,NU,B,INO,INC,E,ND,NE,NL0,IP,ML,LL,NM)

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
C  ECRITURE RELATIONS LINEAIRES COUPLAGE ARLEQUIN MAILLE SOLIDE / COQUE
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE 
C INTEGER   DIM       : DIMENSION DE L'ESPACE
C INTEGER   NC        : NOMBRE DE NOEUDS DOMAINE DE COLLAGE
C INTEGER   S         : SIGNE DEVANT LA MATRICE B (0 : +, 1 : -)
C INTEGER   NU(6)     : NUMERO DES ELEMENTS DE LIAISON 'D_DEPL_R_*'
C                       * = (DX, DY, DZ, DRX, DRY, DRZ)
C REAL*8    B(*)      : VALEURS DE LA MATRICE ARLEQUIN MORSE (CF ARLCAL)
C INTEGER   INO(*)    : COLLECTION NOEUDS COLONNES DE B   
C INTEGER   INC(NC)   : LONGUEUR CUMULEE ASSOCIEE A INO
C REAL*8    E         : PRECISION RELATIVE SUR LES TERMES DE B
C
C VARIABLES D'ENTREE / SORTIE
C INTEGER   ND        : NOMBRE DE TERMES
C INTEGER   NE        : NOMBRE D'EQUATIONS
C INTEGER   NL0       : NOMBRE DE LAGRANGES
C REAL*8    IP(*)     : VECTEUR CHME.CIMPO.VALE
C REAL*8    ML(*)     : VECTEUR CHME.CMULT.VALE
C INTEGER   LL(*)     : COLLECTION CHME.LIGRE.LIEL
C INTEGER   NM(*)     : COLLECTION CHME.LIGRE.NEMA
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- VARIABLES
      INTEGER DIM,S,NC,ND,NE,NL0,NU(*),INO(*),INC(*),LL(2,*),NM(4,*)
      INTEGER NO,I,J,K,P0,P1,Q,NL
      REAL*8  B(*),IP(*),ML(6,*),E,R

C --- TABLE
      INTEGER VEC(2,3)
      DATA VEC / 2,3, 3,1, 1,2 /

      Q = 0
      P1 = INC(1)

      DO 10 I = 1, NC

        P0 = P1
        P1 = INC(1+I)

        DO 20 J = P0, P1-1 

          NO = INO(J)

          Q = Q + 1
          R = B(Q)
          NL = NL0

C ------- RELATIONS COUPLAGE TRANSLATION / TRANSLATION

          IF (ABS(R).GT.E) THEN

            DO 30 K = 1, DIM
              CALL ARLASS(S,NO,NU,K,R,ND,NE,NL,IP,ML,LL,NM)
              NL = NL + 2
 30         CONTINUE

          ELSE

            NL = NL + 2*DIM

          ENDIF

C ------- RELATIONS COUPLAGE TRANSLATION / ROTATION 2D

          IF (DIM.EQ.2) THEN

            NL = NL0 + 2

            Q = Q + 1
            R = B(Q) 
            IF (ABS(R).GT.E) THEN
              CALL ARLASS(S,NO,NU,6,-R,ND,NE,NL,IP,ML,LL,NM)
            ENDIF

            Q = Q + 1
            R = B(Q) 
            IF (ABS(R).GT.E) THEN
              CALL ARLASS(S,NO,NU,6,R,ND,NE,NL0,IP,ML,LL,NM)
            ENDIF

C ------- RELATIONS COUPLAGE TRANSLATION / ROTATION 3D

          ELSE

            DO 50 K = 1, 3

              Q = Q + 1
              R = B(Q)
              IF (ABS(R).GT.E) THEN
                NL = NL0 + 2*(VEC(1,K)-1)
                CALL ARLASS(S,NO,NU,3+VEC(2,K),R,ND,NE,NL,IP,ML,LL,NM)
                NL = NL0 + 2*(VEC(2,K)-1)
                CALL ARLASS(S,NO,NU,3+VEC(1,K),-R,ND,NE,NL,IP,ML,LL,NM)
              ENDIF

 50         CONTINUE

          ENDIF

 20     CONTINUE

        NL0 = NL0 + 2*DIM
 
 10   CONTINUE

      END      

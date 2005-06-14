      SUBROUTINE ARLCH3(D,NC,S,NU,B,INO,INC,E,EQ,ND,NE,NL0,IP,ML,LL,NM)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 08/11/2004   AUTEUR DURAND C.DURAND 
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
C  ECRITURE RELATIONS LINEAIRES COUPLAGE ARLEQUIN MAILLE COQUE / SOLIDE
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE 
C INTEGER   D         : DIMENSION DE L'ESPACE
C INTEGER   NC        : NOMBRE DE NOEUDS DOMAINE DE COLLAGE
C INTEGER   S         : SIGNE DEVANT LA MATRICE B (0 : +, 1 : -)
C INTEGER   NU(6)     : NUMERO DES ELEMENTS DE LIAISON 'D_DEPL_R_*'
C                       * = (DX, DY, DZ, DRX, DRY, DRZ)
C REAL*8    B(*)      : VALEURS DE LA MATRICE ARLEQUIN MORSE (CF ARLCPL)
C INTEGER   INO(*)    : COLLECTION NOEUDS COLONNES DE B   
C INTEGER   INC(NC)   : LONGUEUR CUMULEE ASSOCIEE A INO
C REAL*8    E         : PRECISION RELATIVE SUR LES TERMES DE B
C LOGICAL   EQ(5,*)   : EQUATIONS SELECTIONNEES
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
      INTEGER D,S,ND,NE,NL0,NC,NU(*),INO(*),INC(*),LL(2,*),NM(4,*)
      INTEGER NO,I,J,K,L,P0,P1,Q,NL,N
      REAL*8  B(*),IP(*),ML(6,*),E,R
      LOGICAL EQ(5,*)

      Q = 0
      N = 2*D-1
      P1 = INC(1)

      DO 10 I = 1, NC

        P0 = P1
        P1 = INC(1+I)

        DO 20 J = P0, P1-1 

          NO = INO(J)
          NL = NL0

C ------- RELATIONS COUPLAGE TRANSLATION + ROTATION / TRANSLATION

          DO 20 K = 1, N

            IF (.NOT.EQ(K,I)) THEN

              Q = Q + D

            ELSE

              DO 30 L = 1, D

                Q = Q + 1
                R = B(Q)
                IF (ABS(R).GT.E) THEN
                  CALL ARLASS(S,NO,NU,L,R,ND,NE,NL,IP,ML,LL,NM)
                ENDIF

 30           CONTINUE

              NL = NL + 2

            ENDIF

 20     CONTINUE

        NL0 = NL
 
 10   CONTINUE

      END      

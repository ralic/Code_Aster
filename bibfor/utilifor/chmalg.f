      SUBROUTINE CHMALG(MATE,PGL,NI,NJ)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 15/04/96   AUTEUR GJBHHHH H.HADDAR 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
      IMPLICIT NONE
      INTEGER       NI,NJ
      REAL*8        MATE(1),PGL(3,3)
C .....................................................................C
C .....................................................................C
C    - FONCTION REALISEE:  TRANSFORMATION DES MATRICES ELEMENTAIRES    C
C                          PASSAGE DU REPERE LOCAL AU REPERE GLOBAL    C
C      ON TRAITE SEULEMENT LE CAS OU LES NOEUDS CONTIENNENT UN DES     C
C      DEUX GROUPES DE DDL SUIVANTS                                    C
C           1- DX DY DZ DRX DRY DRZ PHI                                C
C           2- PHI                                                     C
C      VOIR TE0470 OU TE0471                                           C
C                                                                      C
C    - ARGUMENTS:                                                      C
C        DONNEES:      MATE    -->  MATRICE ELEMENTAIRE                C
C                      PGL     -->  MATRICE DE PASSAGE L -> G          C
C                      NI      -->  DIMENTION DU PREMIER INDICE        C
C                      NJ      -->  DIMENTION DU DEUXIEME INDICE       C
C        SORTIE :      MATE    -->  MATRICE ELEMENTAIRE GLOBALE        C
C .....................................................................C
C .....................................................................
      INTEGER      I, J, K, II
      REAL*8       MT(7,7), MATG(7,7)
C .....................................................................
      IF (NJ .GT. 7 ) THEN
         GOTO 9999
      ENDIF
      DO 1 I = 1, 7
         DO 2 J = 1, 7
            MT(J,I) = 0.D0
 2       CONTINUE
 1    CONTINUE
C --- MATRICE DE TRANSFERT
      DO 10 I = 1, 3
         DO 20 J = 1, 3
            MT(I  ,J  ) = PGL(I,J)
            MT(I+3,J+3) = PGL(I,J)
 20      CONTINUE
 10   CONTINUE
C
      MT(7,7) = 1.D0
C --- ON EFFECTUE : MATG() = MATE() * MT()
      DO 40 K = 1, NJ
         DO 50 I = 1, NI
            II = NJ * (I-1)
            MATG(I,K) = 0.D0
            DO 60 J = 1, NJ
               MATG(I,K) = MATG(I,K) + MATE(II+J) * MT(J,K)
60          CONTINUE
50       CONTINUE
40    CONTINUE
C --- MULTIPLICATION PAR LA MATRICE TRANSPOSEE DE "MT" LORSQUE
C           "MATE" EST RECTANGULAIRE DE DIMENSIONS 7X7
      IF (NI .NE. 1) THEN
         DO 70 I = 1, NI
            II = NJ * (I-1)
            DO 80 K = 1, NJ
               MATE(II+K) = 0.D0
               DO 90 J = 1, NJ
                  MATE(II+K) = MATE(II+K) + MT(J,I)*MATG(J,K)
90             CONTINUE
80          CONTINUE
70       CONTINUE
      ELSE
         DO 100 I = 1, NI
            II = NJ * (I-1)
            DO 110 J = 1, NJ
               MATE(II+J) = MATG(I,J)
 110        CONTINUE
 100     CONTINUE
       ENDIF
C .....................................................................
9999   CONTINUE
       END

      SUBROUTINE ARLCH0(DIME,IAN,IAC,NNC,ILGN,MX,PREC,EQ,B,NT)

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
C      COMPTE NOMBRE DE TERMES NON-NEGLIGEABLES ET ADIMENSIONNEMENT 
C             DES RELATIONS LINEAIRES DE COUPLAGE ARLEQUIN
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE 
C INTEGER   DIME      : DIMENSION DE L'ESPACE
C LOGICAL   IAN       : .TRUE. SI MODELE MECANIQUE EST DE TYPE COQUE
C LOGICAL   IAC       : .TRUE. SI MODELE COLLAGE EST DE TYPE COQUE
C INTEGER   NNC       : NOMBRE DE NOEUDS MAILLES DE COLLAGE
C INTEGER   ILGN(NNC) : LONGUEUR CUMULEE COLLECTION NOEUDS COLONNES DE B
C REAL*8    MX(2,NNC) : MAXIMA EN VALEUR ABSOLUE SUIVANT LIGNES DE B
C                       POUR TRANSLATION ET ROTATION (CF ARLMAX) 
C REAL*8    PREC      : PRECISION RELATIVE SUR LES TERMES DE B
C LOGICAL   EQ(5,NNC) : EQUATIONS SELECTIONNEES
C
C VARIABLE D'ENTREE / SORTIE
C REAL*8    B(*)      : VALEURS DE LA MATRICE ARLEQUIN MORSE (CF ARLCPL)
C INTEGER   NT        : NOMBRE DE TERMES NON-NEGLIGEABLES DANS LES 
C                       RELATIONS LINEAIRES DE COUPLAGE ARLEQUIN
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- VARIABLES
      INTEGER  DIME,NNC,ILGN(*),NT
      INTEGER  P0,P1,P2,I,J,K,L,DR,OF
      REAL*8   B(*),PREC,MX(2,*),R
      LOGICAL  IAN,IAC,EQ(5,*)

C --- PARCOURS DES LIGNES

      DR = 2*DIME - 3 
      OF = DIME - 1
      P1 = ILGN(1)
      P2 = 0

      DO 10 I = 1, NNC

        P0 = P1
        P1 = ILGN(I+1)
         
        DO 20 J = 1, P1-P0

C ------- COUPLAGE TRANSLATION / TRANSLATION

          DO 30 K = 1, DIME
            IF (.NOT.EQ(K,I)) THEN
              P2 = P2 + DIME
            ELSE
              DO 40 L = 1, DIME
                P2 = P2 + 1
                R = B(P2)/MX(1,I)
                B(P2) = R
                IF (ABS(R).GT.PREC) NT = NT + 1
 40           CONTINUE
            ENDIF
 30       CONTINUE

C ------- COUPLAGE ROTATION / TRANSLATION

          IF (IAC) THEN
            DO 50 K = 2, DIME
              IF (.NOT.EQ(OF+K,I)) THEN
                P2 = P2 + DIME
              ELSE
                DO 60 L = 1, DIME
                  P2 = P2 + 1
                  R = B(P2)/MX(2,I)
                  B(P2) = R
                  IF (ABS(R).GT.PREC) NT = NT + 1
 60             CONTINUE
              ENDIF
 50         CONTINUE
          ENDIF

C ------- COUPLAGE TRANSLATION / ROTATION

          IF (IAN) THEN

            DO 70 K = 1, DIME
              IF (.NOT.EQ(K,I)) THEN
                P2 = P2 + DR
              ELSE
                DO 80 L = 1, DR
                  P2 = P2 + 1
                  R = B(P2)/MX(1,I)
                  B(P2) = R
                  IF (ABS(R).GT.PREC) NT = NT + 1 
 80             CONTINUE
              ENDIF
 70         CONTINUE

C --------- COUPLAGE ROTATION / ROTATION

            IF (IAC) THEN
              DO 90 K = 2, DIME
                IF (.NOT.EQ(OF+K,I)) THEN
                  P2 = P2 + DR
                ELSE
                  DO 100 L = 1, DR
                    P2 = P2 + 1
                    R = B(P2)/MX(2,I)
                    B(P2) = R
                    IF (ABS(R).GT.PREC) NT = NT + 1
 100              CONTINUE
                ENDIF
 90           CONTINUE
            ENDIF

          ENDIF
 
 20     CONTINUE

 10   CONTINUE

      END

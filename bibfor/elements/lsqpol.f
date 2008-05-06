      SUBROUTINE LSQPOL(ORDRE,E1,NPT,XX,YY,ORDOK,POLY,SIGMA)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 06/05/2008   AUTEUR MARKOVIC D.MARKOVIC 
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

C ------------------------------------------------------------------
C 
C              REGRESSION POLYNOMIALE DE TYPE MOINDRE CARRES
C                  (LEAST SQUARES POLYNOMIAL FITTING)
C 
C              REFERENCE: BASIC SCIENTIFIC SUBROUTINES, VOL. II
C                         F.R. RUCKDESCHEL, BYTE/MCGRAWW-HILL, 1981
C                         ET D'APRES J-P MOREAU, PARIS
C 
C                                                   P.KOECHLIN 02-2004
C 
C ------------------------------------------------------------------
C 
C   ENTREE
C       ORDRE : ORDRE DE LA REGRESSION (ORDRE>0)
C               = DEGRE MAX DU POLYNOME QUE L'ON CHERCHE
C       E1    : SI E1/=0 : PARAMETRE SERVANT DE CRITERE POUR PASSER A
C               L'ORDRE SUPERIEUR
C               ON COMPARE E1 A LA DIMINUTION DE LA DEVIATION STANDARD
C       NPT   : NOMBRE DE POINTS (NPT>1)
C       XX    : ABSCISSE DES POINTS (XX DISTINCTS)
C       YY    : ORDONNEES DES POINTS
C 
C   SORTIE
C       ORDOK : ORDRE FINALEMENT OBTENU >=0
C                  (  AVANT VERIFICATION DU DEGRE:
C                        ORDOK <= ORDRE  SI E1/=0
C                        ORDOK = ORDRE   SI E1=0     )
C                  ORDOK=-1 CORRESPOND A UNE ERREUR
C       POLY  : POLYNOME OBTENU
C               POLY=POLY(1) + POLY(2)*X + POLY(2)*X^2 +...+ POLY(N)*X^N
C       SIGMA : DEVIATION STANDARD
C 
C 
C      LA ROUTINE N'EST VALABLE QUE POUR ORDRE >=1
C      MAIS EN SORTIE, ON A ORDOK >=0

      REAL*8    GRAND
      PARAMETER (GRAND = 1.0D20)
      
      INTEGER   ORDRE,NPT
      REAL*8    XX(NPT),YY(NPT)
      REAL*8    POLY(ORDRE+1),SIGMA

      REAL*8    AA(ORDRE+1),BB(ORDRE+1),FF(ORDRE+1),POLY2(ORDRE+1)
      REAL*8    VV(NPT),DD(NPT),EE(NPT)
      INTEGER   I,J,ORDOK,ORDOK2,ORDLOO
      REAL*8    A1,A2,B1,B2,POLY1,D1,E1,F1,F2,V1,V2

      DO 10, I = 1,ORDRE+1
        POLY(I)  = 0.D0
 10   CONTINUE
      ORDOK = -1

      IF (ORDRE .LT. 1) THEN
         GOTO 999
      ENDIF

C      ON A TOUJOURS DES POINTS DISTINCTS (LES XX SONT DISTINCTS)
C      ON A TOUJOURS NPT>1

      IF (NPT .EQ. 2) THEN
C         UNIQUEMENT POUR + DE PRECISION: MAIS LA ROUTINE MARCHE AUSSI
C         POUR NPT=2
         ORDOK = 1
         POLY(2)=(YY(2)-YY(1))/(XX(2)-XX(1))
         POLY(1)= (YY(1)+YY(2)-POLY(2)*(XX(1)+XX(2)))/2.D0
         SIGMA=0.D0
         GOTO 999
      ENDIF

      V1 = GRAND

C --- INITIALISATION ------------------------------

      DO 13, I = 1,ORDRE+1
        AA(I) = 0.D0
        BB(I) = 0.D0
        FF(I) = 0.D0
 13   CONTINUE  
      DO 16, I = 1,NPT
        VV(I) = 0.D0
        DD(I) = 0.D0
 16   CONTINUE  

      D1 = SQRT(NPT*1.0D0)

      DO 18, I = 1,NPT
        EE(I) = 1.D0 / D1
 18   CONTINUE  
      F1 = D1

      A1 = 0.D0
      DO 20, I=1, NPT
        A1 = A1 + XX(I) * EE(I) * EE(I)
 20   CONTINUE

      POLY1 = 0.D0
      DO 30, I=1, NPT
        POLY1 = POLY1 + YY(I) * EE(I)
 30   CONTINUE

      BB(1) = 1.D0 / F1
      FF(1) = BB(1) * POLY1

      DO 35, I = 1,NPT 
        VV(I) = VV(I) + POLY1*EE(I)
 35   CONTINUE  

C --- DEBUT BOUCLE ----------------------------------------

      DO 40, ORDLOO=1,ORDRE

C SAVE LATEST RESULTS
        DO 45, I = 1,ORDRE+1
          POLY2(I) = POLY(I)
 45     CONTINUE  
        ORDOK2 = ORDOK
        V2  = V1
        F2  = F1
        A2  = A1

        F1 = 0.D0
        DO 50, I=1, NPT
          B1 = EE(I)
          EE(I) = (XX(I) - A2) * B1 - F2 * DD(I)
          DD(I) = B1
          F1 = F1 + EE(I) * EE(I)
 50     CONTINUE

        F1 = SQRT(F1)
        DO 55, I=1, NPT
          EE(I) = EE(I) / F1
 55     CONTINUE
        A1 = 0.D0
        DO 60, I=1, NPT
          A1 = A1 + XX(I) * EE(I) * EE(I)
 60     CONTINUE

        POLY1 = 0.D0
        DO 70, I=1, NPT
          POLY1 = POLY1 + YY(I) * EE(I)
 70     CONTINUE

        DO 80, I=0,ORDLOO
          J = ORDLOO - I + 1
          B2 = BB(J)
          D1 = 0.D0
          IF (J .GT. 1)  D1 = BB(J - 1)
          D1 = D1 - A2 * BB(J) - F2 * AA(J)
          BB(J) = D1 / F1
          AA(J) = B2
 80     CONTINUE

        DO 83, I = 1,NPT
          VV(I) = VV(I) + POLY1*EE(I)
 83     CONTINUE 
  
        DO 86, I = 1,ORDRE+1 
          FF(I) = FF(I) + POLY1*BB(I)
 86     CONTINUE
     
        DO 88, I = 1,ORDRE+1 
          POLY(I) = FF(I)
 88     CONTINUE 
    
        ORDOK = ORDLOO

        SIGMA = 0.D0
        DO 90, I=1, NPT
          SIGMA = SIGMA + (VV(I) - YY(I)) * (VV(I) - YY(I))
 90     CONTINUE

C        NOTE THE DIVISION IS BY THE NUMBER OF DEGREES OF FREEDOM
        IF (NPT .GT. ORDLOO + 1) THEN
C          SIGMA = SQRT(SIGMA / DFLOAT(NPT - ORDLOO - 1))
          SIGMA = SQRT(SIGMA / (NPT - ORDLOO - 1))
        ELSE
          GOTO 999
        ENDIF

        IF (E1 .GT. 0.D0) THEN
C          TEST FOR MINIMAL IMPROVEMENT OR IF ERROR IS LARGER, QUIT
          IF (     (  ABS(V1 - SIGMA) .LT. (E1*SIGMA) )
     &        .OR. (  E1 * SIGMA .GT. E1 * V1         ))  THEN
C           ABORTED SEQUENCE, RECOVER LAST VALUES
            ORDOK = ORDOK2
            SIGMA = V2
            DO 100, I = 1,ORDRE+1
              POLY(I) = POLY2(I)
 100        CONTINUE   
            GOTO 999
          ENDIF
        ENDIF

        V1 = SIGMA

 40   CONTINUE
 999  CONTINUE
      END

      SUBROUTINE ALWOLF(CONTEX, Q, WOLFE, T)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/09/2001   AUTEUR PBBHHPB P.BADEL 
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
      LOGICAL  WOLFE
      REAL*8   T, Q(2), CONTEX(9)

C ----------------------------------------------------------------------
C       RECHERCHE LINEAIRE CUBIQUE AVEC CRITERE D'ARRET DE WOLFE
C ----------------------------------------------------------------------
C VAR CONTEX R8  CONTEXTE (POUR EVITER UN SAVE)
C IN  Q      R8  VALEUR (1) ET GRADIENT (2) DE LA FONCTION Q(T)
C VAR WOLFE  L   CONVERGENCE DE LA RL (TRUE SI WOLFE OK)
C                 IN  : SI TRUE -> INITIALISATION
C                 OUT : CONVERGENCE OU NON
C VAR T      R8  PARAMETRE D'AVANCEE DE LA RECHERCHE LINEAIRE
C                 IN  : VALEUR ACTUELLE
C                 OUT : NOUVELLE PROPOSITION
C ----------------------------------------------------------------------

      LOGICAL GAUCHE, DROITE, EXTRAP
      REAL*8  Q0(2), QM(2), TM, TG, TD, M1, M2
      REAL*8  P, PA, PB, DIS, RDIS, NUM, DEN, TP, A, THETA, PETIT, GRAND
      REAL*8  THETA0

      DATA M1, M2        /0.001D0, 0.9D0/
      DATA A, THETA0  /2.D0,  0.01D0/
      DATA PETIT, GRAND  /1.D-4, 1.D+4/
C ----------------------------------------------------------------------



C -- INITIALISATION

      IF (WOLFE) THEN
        Q0(1) = Q(1)
        Q0(2) = Q(2)
        QM(1) = Q(1)
        QM(2) = Q(2)
        TG    = 0.D0
        TD    = 0.D0
        TM    = 0.D0
        T     = 1.D0
        WOLFE = .FALSE.
        EXTRAP= .TRUE.
        THETA = THETA0
        GOTO 9000
      END IF


C -- LECTURE DU CONTEXTE

      Q0(1) = CONTEX(1)
      Q0(2) = CONTEX(2)
      QM(1) = CONTEX(3)
      QM(2) = CONTEX(4)
      TG    = CONTEX(5)
      TD    = CONTEX(6)
      TM    = CONTEX(7)
      EXTRAP=(CONTEX(8).GT.0.5D0)
      THETA = CONTEX(9)
      
      
C -- TEST D'ARRET DE WOLFE

      DROITE = Q(1) .LE. Q0(1) + T*M1*Q0(2)
      GAUCHE = Q(2) .GE.           M2*Q0(2)
      WOLFE  = GAUCHE .AND. DROITE
      IF (WOLFE) GOTO 9000


C -- REACTUALISATION DE L'INTERVALLE DE RECHERCHE

      IF (.NOT. DROITE) THEN
        TD = T
        EXTRAP = .FALSE.
      ELSE
        TG = T
      END IF

      IF (EXTRAP) TD = T*A


C -- APPROXIMATION CUBIQUE DE LA FONCTION Q(T)

      P   = Q(2) + QM(2) - 3*(Q(1)-QM(1))/(T-TM)
      PA  = (Q(2)+QM(2)+2*P) / (T-TM)**2
      PB  = (Q(2)+P)/(T-TM)
      DIS = PB**2 - PA*Q(2)

C    IL EXISTE UN MINIMUM
      IF (DIS.GE.0) THEN

        RDIS = SQRT(DIS)
        IF (PB.LE.0) THEN
          NUM = RDIS - PB
          DEN = PA
        ELSE
          NUM = -Q(2)
          DEN = PB + RDIS
        END IF

C      ET CE MINIMUM EST DANS L'INTERVALLE DE RECHERCHE
        IF (ABS(NUM) .LE. ABS(DEN)*(TD-TG)) THEN
          TP = T + NUM/DEN
          GOTO 100
        END IF
      END IF

C    DANS LE CAS CONTRAIRE, CHOIX D'UN BORD DE L'INTERVALLE

      IF (Q(2).LE.0) THEN
        TP = TD
      ELSE
        TP = TG
      END IF

 100  CONTINUE


C -- PROJECTION DANS LE DOMAINE LICITE

      TP = MIN(TP, TD - THETA*(TD-TG))
      TP = MAX(TP, TG + THETA*(TD-TG))


C -- TESTS DE SECOURS

      IF (EXTRAP .AND. TG .GT. GRAND) THEN
        WOLFE = .TRUE.
        GOTO 9000
      END IF

      IF (.NOT. EXTRAP .AND. (TD-TG).LT.PETIT) THEN
        WOLFE = .TRUE.
        GOTO 9000
      END IF

      IF (ABS(T-TM).LE.PETIT) THEN
        WOLFE = .TRUE.
        GOTO 9000
      END IF


C -- MISE A JOUR DES RESULTATS

      TM    = T
      T     = TP
      QM(1) = Q(1)
      QM(2) = Q(2)


 9000 CONTINUE


C -- SAUVEGARDE DU CONTEXTE

      CONTEX(1) = Q0(1)
      CONTEX(2) = Q0(2)
      CONTEX(3) = QM(1)
      CONTEX(4) = QM(2)
      CONTEX(5) = TG
      CONTEX(6) = TD
      CONTEX(7) = TM
      CONTEX(8) = 0.D0
      IF (EXTRAP) CONTEX(8)=1.D0
      CONTEX(9) = THETA

      END

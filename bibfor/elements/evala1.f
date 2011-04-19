      SUBROUTINE EVALA1( MOD, RELCOM, SIG, VIN, IMAT, MODULE, ICODE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 20/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C RESPONSABLE FOUCAULT A.FOUCAULT
C =====================================================================
C    - FONCTION REALISEE:  CALCUL DU MODULE DE RIGIDITE
C                          DE MICRO-DILTATION CONTENU
C                          AUX POINTS DE GAUSS
C    - ARGUMENTS:
C        DONNEES:      RELCOM  -->  RELATION DE COMPORTEMENT LOCAL
C                      MOD     -->  TYPE DE MODELISATION
C                      SIG     --> ETAT DE CONTRAINTES
C                      VIN     --> VARIABLES INTERNES
C                      IMAT    --> INDICE DU MATERIAU
C        SORTIE:       MODULE  --> MODULE DE RIGIDITE
C                                  DE MICRO-DILATATION
C                      ICODE   -->
C =====================================================================
      IMPLICIT NONE
      REAL*8 MODULE, SIG(6), VIN(50), DSDE(6,6), SIGT(2,2)
      REAL*8 ANGLE, ROT(2,2), DEGR, SIGR1(2,2), SIGR2(2,2), TEMP
      REAL*8 SIGF(6), RATIO1, PI, VALUE1, VALUE2, RATIO2, VALEUR
      REAL*8 R8PREM
      INTEGER INCANG, I, J, K, IANG
      INTEGER IMAT, ICODE
      CHARACTER*8  MOD
      CHARACTER*16 RELCOM
      PARAMETER   ( DEGR = 0.0174532925199D0 )
      PARAMETER   ( PI = 3.14159265358979D0 )

C =====================================================================
C DEFINITION DES ELEMENTS NECESSAIRES A L'EVALUATION DU MODULE
C =====================================================================
C --- DISCRETISATION ANGULAIRE DE LA RECHERCHE ENTRE [0° ET 90°[
C =====================================================================
      INCANG = 5
C =====================================================================
C --- INITIALISATION MATRICE DE ROTATION ANGULAIRE
C =====================================================================
      DO 5 I=1, 2
        DO 5 J=1,2
          ROT(I,J) = 0.D0
 5    CONTINUE
C =====================================================================
C --- EXPRESSION DU TENSEUR DES CONTRAINTES SOUS FORME MATRICIEL
C --- LIMITE AU 2D POUR L'INSTANT
C =====================================================================
      SIGT(1,1) = SIG(1)
      SIGT(1,2) = SIG(4)/SQRT(2.0D0)
      SIGT(2,1) = SIGT(1,2)
      SIGT(2,2) = SIG(2)
C =====================================================================
C --- INITIALISATION A ZERO DE LA VALEUR DU MODULE
C =====================================================================
      MODULE = 0.D0
C =====================================================================
C BOUCLE SUR LES ANGLES POUR RECHERCHER LA VALEUR DU MODULE
C =====================================================================
      DO 10 IANG = 1, 36
C =====================================================================
C CONSTRUCTION MATRICE DE ROTATION
C =====================================================================
        ANGLE = (IANG-1)*INCANG*DEGR
        ROT(1,1) = COS(ANGLE)
        ROT(1,2) = -SIN(ANGLE)
        ROT(2,1) = SIN(ANGLE)
        ROT(2,2) = COS(ANGLE)
C =====================================================================
C CALCUL DE L'ETAT DE CONTRAINTES SUITE A LA ROTATION
C --- SIGF = TRANSPOSE(ROT)*SIG*ROT
C =====================================================================
        DO 30 I = 1, 2
          DO 31 J = 1, 2
            TEMP = 0.D0
            DO 32 K = 1,2
              TEMP = TEMP + SIGT(I,K)*ROT(K,J)
 32         CONTINUE
            SIGR1(I,J) = TEMP
 31       CONTINUE
 30     CONTINUE

C =====================================================================
C --- TRANSPOSE(ROT) = ROT
C =====================================================================
        TEMP = ROT(2,1)
        ROT(2,1) = ROT(1,2)
        ROT(1,2) = TEMP

        DO 40 I = 1, 2
          DO 41 J = 1, 2
            TEMP = 0.D0
            DO 42 K = 1,2
              TEMP = TEMP + ROT(I,K)*SIGR1(K,J)
 42         CONTINUE
              SIGR2(I,J) = TEMP
 41       CONTINUE
 40     CONTINUE

C =====================================================================
C --- EXPRESSION DES CONTRAINTES SOUS FORME VECTORIELLE
C =====================================================================
        SIGF(1) = SIGR2(1,1)
        SIGF(2) = SIGR2(2,2)
        SIGF(3) = SIG(3)
        SIGF(4) = SIGR2(1,2)*SQRT(2.0D0)
        SIGF(5) = SIG(5)
        SIGF(6) = SIG(6)

C =====================================================================
C --- CALCUL DE LA MATRICE TANGENTE -----------------------------------
C --- (FONCTION DE LA RELATION DE COMPORTEMENT) -----------------------
C =====================================================================
        IF (RELCOM.EQ.'DRUCK_PRAGER') THEN
C =====================================================================
C --- LOI DE TYPE DRUCKER_PRAGER --------------------------------------
C =====================================================================
          CALL REDRPR(MOD,IMAT,SIGF,VIN,DSDE,ICODE)
C =====================================================================
C ----------- LOI DE TYPE HUJEUX --------------------------------------
C =====================================================================
        ELSEIF (RELCOM.EQ.'HUJEUX') THEN
          CALL HUJTID(MOD,IMAT,SIGF,VIN,DSDE,ICODE)

        ELSE
C =====================================================================
CC RELATION DE COMPORTEMENT INVALIDE
C =====================================================================
          CALL ASSERT(.FALSE.)
        ENDIF
C =====================================================================
C ----------- EVALUATION DU MODULE ------------------------------------
C =====================================================================
        IF(ABS(DSDE(4,4)).GT.R8PREM())THEN
          RATIO1  = (DSDE(4,1)*DSDE(1,4)-DSDE(4,4)*DSDE(1,1))
     &              /(3*DSDE(4,4))
          VALUE1 = (1.0D0/PI)**2*RATIO1
          RATIO2  = (DSDE(4,2)*DSDE(2,4)-DSDE(4,4)*DSDE(2,2))
     &            /(3*DSDE(4,4))
          VALUE2 = (1.0D0/PI)**2*RATIO2
          VALEUR = MAX(VALUE1,VALUE2)
          MODULE = MAX(MODULE,VALEUR)
        ENDIF

 10   CONTINUE

      END

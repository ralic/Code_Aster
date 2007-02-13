        SUBROUTINE HUJPLA (MOD, CRIT, MATER, SEUILI, SEUILD,
     &             NVI, EPSD, DEPS, SIGD, VIND, SIGF, VINF, MECANI,
     &             NITER, NDEC, EPSCON, IRET)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 12/02/2007   AUTEUR KHAM M.KHAM 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   ------------------------------------------------------------------
C   INTEGRATION PLASTIQUE DE LA LOI DE HUJEUX
C   IN  MOD    :  MODELISATION
C   	CRIT   :  CRITERES DE CONVERGENCE
C   	MATER  :  COEFFICIENTS MATERIAU A T+DT
C   	SEUILI :  FONCTION DE CHARGE ISO. CALCULEE AVEC PREDICT ELAS
C   	SEUILD :  FONCTION DE CHARGE DEV. CALCULEE AVEC PREDICT ELAS
C   	NVI    :  NOMBRE DE VARIABLES INTERNES
C   	EPSD   :  DEFORMATIONS A T
C   	DEPS   :  INCREMENT DE DEFORMATION
C   	SIGD   :  CONTRAINTE  A T
C   	VIND   :  VARIABLES INTERNES  A T
C   VAR SIGF   :  CONTRAINTE A T+DT  (IN -> ELAS, OUT -> PLASTI)
C   OUT VINF   :  VARIABLES INTERNES A T+DT
C   	MECANI :  MECANISME(S) ACTIF(S)
C   	NITER  :  NOMBRE D ITERATIONS POUR PLASTICITE
C   		  (CUMUL DECOUPAGE)
C   	NDEC   :  NOMBRE DE DECOUPAGE
C   	EPSCON :  EPSILON A CONVERGENCE
C   	IRET   :  CODE RETOUR DE  L'INTEGRATION DE LA LOI DE HUJEUX
C   		     IRET=0 => PAS DE PROBLEME
C   		     IRET=1 => ECHEC 
C   ------------------------------------------------------------------
        INTEGER       NDT, NDI, NVI, NITER, NDEC, IRET
        INTEGER       I, K
        INTEGER       NVIMAX
        PARAMETER     (NVIMAX=16)
        REAL*8        EPSD(6), DEPS(6)
        REAL*8        SIGD(6), SIGF(6), PREDIC(6)
        REAL*8        SIGD0(6), DEPS0(6), PREDI0(6)
        REAL*8        VIND(*), VINF(*), VIND0(NVIMAX), EPSCON
        REAL*8        MATER(20,2), CRIT(*), CRITR(4)
        REAL*8        SEUILI, SEUILD(3) , PA, PREF, I1F
        REAL*8        ZERO, UN, TOLE
        LOGICAL       CHGMEC, NOCONV, AREDEC, STOPNC, NEGMUL(4)
        CHARACTER*6   MECANI
        CHARACTER*8   MOD
        PARAMETER     (TOLE  = 1.D-6)
        PARAMETER     (ZERO  = 0.D0)
        PARAMETER     (UN    = 1.D0)
        INTEGER       IDEC
        INTEGER       NITER0

        COMMON /TDIM/ NDT, NDI

        IF (NVI.GT.NVIMAX) CALL U2MESS('F', 'COMPOR1_1')

        PA = MATER(8,2)


C ----  SAUVEGARDE DES GRANDEURS D ENTREE INITIALES
        CALL LCEQVE(SIGF,PREDI0)
        CALL LCEQVE(SIGD,SIGD0)
        CALL LCEQVE(DEPS,DEPS0)
        CALL LCEQVN(NVI,VIND,VIND0)


C  ARRET OU NON EN NON CONVERGENCE INTERNE
C  ---------------------------------------
        IF (INT(CRIT(1)).LT.0) THEN
          STOPNC = .TRUE.
        ELSE
          STOPNC = .FALSE.
        ENDIF


C  INITIALISATION DES VARIABLES DE REDECOUPAGE
C  -------------------------------------------
C  INT( CRIT(5) ) = 0  1 OU -1 ==> PAS DE REDECOUPAGE DU PAS DE TEMPS
        IF ((INT(CRIT(5)) .EQ.  0) .OR.
     &      (INT(CRIT(5)) .EQ. -1) .OR.
     &      (INT(CRIT(5)) .EQ.  1)) THEN
              NDEC   = 1
              AREDEC = .TRUE.
              NOCONV = .FALSE.


C ---- INT( CRIT(5) ) < -1 ==> REDECOUPAGE DU PAS DE TEMPS
C                              SI NON CONVERGENCE
        ELSEIF (INT(CRIT(5)) .LT. -1) THEN
              NDEC   = 1
              AREDEC = .FALSE.
              NOCONV = .FALSE.


C ---- INT( CRIT(5) ) > 1 ==> REDECOUPAGE IMPOSE DU PAS DE TEMPS
        ELSEIF (INT(CRIT(5)) .GT. 1) THEN
              NDEC   = INT(CRIT(5))
              AREDEC = .TRUE.
              NOCONV = .FALSE.
        ENDIF


C  POINT DE RETOUR EN CAS DE DECOUPAGE
C  APRES UNE NON CONVERGENCE, POUR  INT(CRIT(5)) < -1
  500   CONTINUE
        IF (NOCONV) THEN
           NDEC   = -INT(CRIT(5))
           AREDEC = .TRUE.
        ENDIF


C   RESTAURATION DE SIGD VIND DEPS ET PREDIC ELAS SIGF
C   EN TENANT COMPTE DU DECOUPAGE EVENTUEL
        CALL LCEQVE (SIGD0, SIGD)
        CALL LCEQVN (NVI, VIND0, VIND)

        DO  10 I = 1, NDT
         DEPS(I) = DEPS0(I)/NDEC
         SIGF(I) = SIGD0(I) + (PREDI0(I)-SIGD(I)) /NDEC
 10      CONTINUE


C  BOUCLE SUR LES DECOUPAGES
C  -------------------------
        DO 400 IDEC = 1, NDEC


C SAUVEGARDE PREDIC ELASTIQUE POUR EVENTUEL CHANGEMENT
C DE MECANISME
         CALL LCEQVE (SIGF, PREDIC)

         PREF = ABS(PA)

         DO 25 K = 1, 4
           VIND(5+K) = ZERO
           NEGMUL(K) = .FALSE.
 25        CONTINUE

         CALL HUJCRI (MATER, SIGF, VIND, SEUILI)
         IF ((SEUILI/PREF) .GT. TOLE) VIND(9) = UN

         DO 11 K = 1, 3
            CALL HUJCRD (K, MATER, SIGF, VIND, SEUILD(K))
            IF (SEUILD(K) .GT. TOLE) VIND(5+K) = UN
  11        CONTINUE

         CHGMEC = .FALSE.

         CALL LCEQVN (NVI, VIND, VINF)

 100     CONTINUE

C--->   RESOLUTION EN FONCTION DES MECANISMES ACTIVES
C       MECANISMES ISOTROPE ET DEVIATOIRE
C-----------------------------------------------------
         CALL HUJMID (MOD, CRIT, MATER, NVI, EPSD, DEPS,
     &     SIGD, SIGF, VIND, VINF, NOCONV, AREDEC, STOPNC,
     &     NEGMUL, NITER0, EPSCON, IRET)
         NITER = NITER + NITER0

         IF (NOCONV .AND. (.NOT. AREDEC)) GOTO 500
         IF (NOCONV) IRET = 1
         IF (IRET.EQ.1) GOTO 9999

         DO 24 K = 1, 4
           CRITR(K) = ZERO
 24        CONTINUE

C --- VERIF FIN INTEGRATION PLASTIQUE
         CALL HUJCRI (MATER, SIGF, VINF, SEUILI)

         IF ((SEUILI/PREF) .GT. TOLE) CRITR(4) = UN

         DO 23 K = 1, 3
           CALL HUJCRD (K, MATER, SIGF, VINF, SEUILD(K))
           IF (SEUILD(K) .GT. TOLE) CRITR(K) = UN
  23       CONTINUE


C ---> TEST SOLUTIONS FINALES (POSITIVITE DES MULTIPLICATEURS)
          DO 40 K = 1, 4
            IF (NEGMUL(K) .AND. (.NOT.AREDEC)) THEN

              WRITE(6,1001)'ALARME :: LAMBDA NEGATIF AU MECANISME NO',K
              CALL U2MESS('A','COMPOR1_12')
              NDEC      = -INT(CRIT(5))
              NEGMUL(K) = .FALSE.
              AREDEC    = .TRUE.
              GOTO 500

            ELSEIF (NEGMUL(K) .AND. AREDEC) THEN

              WRITE(6,1001)'ALARME :: LAMBDA NEGATIF AU MECANISME NO',K
              NEGMUL(K) = .FALSE.
              CHGMEC    = .TRUE.
C ---- CONSERVER LA MIS-A-JOUR DE VINF POUR CALCUL DE MATRICE TANGENTE
C      FULL-MECA
              VIND(5+K) = ZERO

            ENDIF
  40        CONTINUE

C---> VERIFICATION DES MECANISMES ACTIFS
         DO 30 K = 1, 4
           IF (VIND(5+K) .EQ. ZERO .AND.
     &         CRITR(K) .GT. ZERO) THEN
             VIND(5+K) = UN
             CHGMEC    = .TRUE.
           ENDIF
  30       CONTINUE


C - SI ON ACTIVE UN MECANISME SUPPLEMENTAIRE : RETOUR
C   ET SI ON AVAIT CONVERGE
        IF (CHGMEC .AND. (.NOT. NOCONV)) THEN

          CALL U2MESS('A', 'COMPOR1_13')
          CHGMEC = .FALSE.


C --- reinitialisation de sigf à la prediction élastique predic
          CALL LCEQVE (PREDIC, SIGF)
          CALL LCEQVN (NVI, VIND, VINF)
          GOTO 100

        ELSE

          IF (IDEC .LT. NDEC) THEN
            CALL LCEQVE (SIGF, SIGD)
            DO 32 I = 1, NVI
              VIND(I) = VINF(I)
 32           CONTINUE
            DO 33 I = 1, NDT
              SIGF(I) = SIGD(I) + (PREDI0(I)-SIGD(I)) /NDEC
 33           CONTINUE
          ENDIF

        ENDIF

 400    CONTINUE

9999    CONTINUE
1001    FORMAT(A,I2)
        END

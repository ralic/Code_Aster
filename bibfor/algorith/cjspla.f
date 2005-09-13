        SUBROUTINE CJSPLA ( MOD, CRIT, MATER, SEUILI, SEUILD,
     >                NVI, EPSD, DEPS, SIGD, VIND, SIGF, VINF, MECANI,
     >                NIVCJS ,NITER,NDEC,EPSCON,IRET)
        IMPLICIT NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/09/2005   AUTEUR LEBOUVIE F.LEBOUVIER 
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
C       ----------------------------------------------------------------
C       INTEGRATION PLASTIQUE DE LA LOI CJS
C       IN  MOD    :  MODELISATION
C           CRIT   :  CRITERES DE CONVERGENCE
C           MATER  :  COEFFICIENTS MATERIAU A T+DT
C           SEUILI :  FONCTION DE CHARGE ISO. CALCULEE AVEC PREDICT ELAS
C           SEUILD :  FONCTION DE CHARGE DEV. CALCULEE AVEC PREDICT ELAS
C           NVI    :  NOMBRE DE VARIABLES INTERNES
C           EPSD   :  DEFORMATIONS A T
C           DEPS   :  INCREMENT DE DEFORMATION
C           SIGD   :  CONTRAINTE  A T
C           VIND   :  VARIABLES INTERNES  A T
C       VAR SIGF   :  CONTRAINTE A T+DT  (IN -> ELAS, OUT -> PLASTI )
C       OUT VINF   :  VARIABLES INTERNES A T+DT
C           MECANI :  MECANISME(S) ACTIVE(S)
C           NITER  :  NOMBRE D ITERATIONS POUR PLASTICITE
C                          (CUMUL DECOUPAGE)
C           NDEC   :  NOMBRE DE DECOUPAGE
C           EPSCON :  EPSILON A CONVERGENCE
C           IRET   :  CODE RETOUR DE  L'INTEGRATION DE LA LOI CJS
C                              IRET=0 => PAS DE PROBLEME
C                              IRET=1 => ECHEC 
C       ----------------------------------------------------------------

        INTEGER       NDT, NDI, NVI,NITER,NDEC, IRET
        INTEGER       NVIMAX
        PARAMETER(NVIMAX=16)
        REAL*8        EPSD(6), DEPS(6)
        REAL*8        SIGD(6), SIGF(6), PREDIC(6)
        REAL*8        SIGD0(6), DEPS0(6), PREDI0(6)
        REAL*8        VIND(*), VINF(*),VIND0(NVIMAX),EPSCON
        REAL*8        MATER(14,2), CRIT(*)
        REAL*8        I1F
        REAL*8        SEUILI, SEUILD , PA, PREF,QINIT
        REAL*8        ZERO
        LOGICAL       CHGMEC, NOCONV, AREDEC,STOPNC
        CHARACTER*6   MECANI
        CHARACTER*4     NIVCJS

        CHARACTER*8   MOD
        PARAMETER     ( ZERO = 0.D0   )
        INTEGER       IDEC
        INTEGER       I,NITER0


        COMMON /TDIM/   NDT, NDI
C

        IF ( NVI.GT.NVIMAX) THEN
         CALL UTMESS('F', 'CJSPLA',' NVI > NVIMAX')
        ENDIF
C
        PA    = MATER(12,2)
        QINIT = MATER(13,2)


C  SAUVEGARDE DES GRANDEURS D ENTREE INITIALES
        CALL LCEQVE( SIGF, PREDI0 )
        CALL LCEQVE( SIGD, SIGD0  )
        CALL LCEQVE( DEPS, DEPS0  )
        CALL LCEQVN( NVI, VIND, VIND0 )

C
C  ARRET OU NON EN NON CONVERGENCE INTERNE
C  -------------------------------------------
C


        IF ( INT(CRIT(1)) .LT.  0) THEN
          STOPNC = .TRUE.
        ELSE
          STOPNC = .FALSE.
        ENDIF
C
C  INITIALISATION DES VARIABLES DE REDECOUPAGE
C  -------------------------------------------
C
C INT(CRIT(5)) = 0  1 OU -1 -> PAS DE REDECOUPAGE DU PAS DE TEMPS
C

        IF ( (INT(CRIT(5)) .EQ.  0) .OR.
     &       (INT(CRIT(5)) .EQ. -1) .OR.
     &       (INT(CRIT(5)) .EQ.  1) )  THEN
              NDEC = 1
              AREDEC = .TRUE.
              NOCONV = .FALSE.


C INT(CRIT(5)) < -1 -> REDECOUPAGE DU PAS DE TEMPS SI NON CONVERGENCE
C
        ELSEIF ( INT(CRIT(5)) .LT. -1 ) THEN
              NDEC = 1
              AREDEC = .FALSE.
              NOCONV = .FALSE.


C INT(CRIT(5)) > 1 -> REDECOUPAGE IMPOSE DU PAS DE TEMPS
C
        ELSEIF ( INT(CRIT(5)) .GT. 1 ) THEN
              NDEC = INT(CRIT(5))
              AREDEC = .TRUE.
              NOCONV = .FALSE.
        ENDIF



C  POINT DE RETOUR EN CAS DE DECOUPAGE
C  APRES UNE NON CONVERGENCE, POUR  INT(CRIT(5)) < -1

  500   CONTINUE
        IF(NOCONV) THEN
         NDEC = -  INT(CRIT(5))
         AREDEC=.TRUE.
        ENDIF

C
C   RESTAURATION DE SIGD VIND DEPS ET PREDIC ELAS SIGF
C   EN TENANT COMPTE DU DECOUPAGE EVENTUEL
C
        CALL LCEQVE( SIGD0, SIGD )
        CALL LCEQVN( NVI, VIND0, VIND )

        DO  10 I = 1 , NDT
         DEPS(I) = DEPS0(I)/NDEC
         SIGF(I) = SIGD0(I)+(PREDI0(I)-SIGD(I))/NDEC
 10     CONTINUE

C
C  BOUCLE SUR LES DECOUPAGES
C  -------------------------
        DO 400 IDEC = 1 , NDEC

C
C SAUVEGARDE PREDIC ELASTIQUE POUR EVENTUEL CHANGEMENT
C DE MECANISME
C
         CALL LCEQVE( SIGF, PREDIC )

         I1F  = ZERO
         DO 20  I=1, NDI
          I1F  = I1F  +   SIGF(I)
 20      CONTINUE

         IF((I1F+QINIT)  .EQ. 0.D0 ) THEN
          I1F  = -QINIT+1.D-12 * PA
          PREF = ABS(PA)
         ELSE
          PREF = ABS(I1F+QINIT)
         ENDIF

         CALL CJSSMI ( MATER, SIGF, VIND, SEUILI )
         CALL CJSSMD ( MATER, SIGF, VIND, SEUILD )
         SEUILI = SEUILI/PREF
         SEUILD = SEUILD/PREF
         CHGMEC=.FALSE.
         IF ( SEUILI .GT. ZERO .AND. SEUILD .LE. ZERO) THEN
          MECANI='ISOTRO'
         ENDIF
         IF ( SEUILI .LE. ZERO .AND. SEUILD .GT. ZERO) THEN
          MECANI='DEVIAT'
         ENDIF
         IF ( SEUILI .GT. ZERO .AND. SEUILD .GT. ZERO) THEN
          MECANI='ISODEV'
         ENDIF


         DO 21 I=1, NVI-1
          VINF(I) = VIND(I)
 21      CONTINUE


 100     CONTINUE

C--->   RESOLUTION EN FONCTION DES MECANISMES ACTIVES

C       MECANISME ISOTROPE SEUL
C       -----------------------

         IF ( MECANI .EQ. 'ISOTRO')  THEN
          CALL CJSMIS( MOD, CRIT, MATER, NVI, EPSD, DEPS,
     &                  SIGD, SIGF, VIND, VINF,
     >                  NOCONV,AREDEC,STOPNC,
     >                  NITER0,EPSCON)
          NITER = NITER + NITER0
          IF ( NOCONV.AND.(.NOT.AREDEC)) GOTO 500
          IF(NOCONV) THEN
            IRET=1
            GOTO 9999
          ENDIF
         ENDIF


C       MECANISME DEVIATOIRE SEUL
C       -------------------------

         IF ( MECANI .EQ. 'DEVIAT')  THEN
          CALL CJSMDE ( MOD, CRIT, MATER, NVI, EPSD, DEPS,
     &                  SIGD, SIGF, VIND, VINF,
     >                  NOCONV,AREDEC,STOPNC,
     >                  NITER0,EPSCON)
          NITER = NITER + NITER0
          IF ( NOCONV.AND.(.NOT.AREDEC)) GOTO 500
          IF(NOCONV) THEN
            IRET=1
            GOTO 9999
          ENDIF
         ENDIF
C
C       MECANISMES ISOTROPE ET DEVIATOIRE
C       ---------------------------------

         IF ( MECANI .EQ. 'ISODEV') THEN
          CALL CJSMID ( MOD, CRIT, MATER, NVI, EPSD, DEPS,
     &                  SIGD, SIGF, VIND, VINF,
     >                  NOCONV,AREDEC,STOPNC,
     >                  NITER0,EPSCON)
          NITER = NITER + NITER0
          IF ( NOCONV.AND.(.NOT.AREDEC)) GOTO 500
          IF(NOCONV) THEN
            IRET=1
            GOTO 9999
          ENDIF
         ENDIF



C--->   CALCUL DES FONCTIONS DE CHARGES SUR ETAT FINAL

         CALL CJSSMI ( MATER, SIGF, VINF, SEUILI )
         CALL CJSSMD ( MATER, SIGF, VINF, SEUILD )
         I1F  = ZERO
         DO 22  I=1, NDI
          I1F  = I1F  +   SIGF(I)
 22      CONTINUE
         IF((I1F+QINIT)  .EQ. 0.D0 ) THEN
          I1F  = -QINIT + 1.D-12 * PA
          PREF = ABS(PA)
         ELSE
          PREF = ABS(I1F+QINIT)
         ENDIF
C--->   VERIFICATION DES MECANISMES ACTIVES


         IF ( (MECANI .EQ. 'ISOTRO') .AND. (SEUILD .GT. ZERO) ) THEN
          MECANI='ISODEV'
          CHGMEC=.TRUE.
         ENDIF
         IF ( (MECANI .EQ. 'DEVIAT') .AND. (SEUILI .GT. ZERO) ) THEN
          MECANI='ISODEV'
          CHGMEC=.TRUE.
         ENDIF
C
C - SI ON ACTIVE EN FAIT LES DEUX MECANISMES AU LIEU D'UN SEUL : RETOUR
C   ET SI ON AVAIT CONVERGE
C
         IF( CHGMEC.AND.(.NOT.NOCONV) ) THEN
           CHGMEC=.FALSE.
           CALL LCEQVE( PREDIC, SIGF )
           GOTO 100
         ELSE
          IF ( IDEC .LT. NDEC) THEN
           CALL LCEQVE( SIGF, SIGD )
           DO 32 I=1, NVI-1
            VIND(I) = VINF(I)
 32        CONTINUE
           DO 33 I=1, NDT
             SIGF(I) = SIGD(I)+(PREDI0(I)-SIGD(I))/NDEC
 33        CONTINUE
          ENDIF
         ENDIF
  400   CONTINUE

C
C--->   CALCUL DE LA VARIABLE INTERNE CORRESPONDANT AU MECANISME PLASTIC
C

         IF (MECANI .EQ. 'ISOTRO') VINF(NVI) = 1.D0
         IF (MECANI .EQ. 'DEVIAT') VINF(NVI) = 2.D0
         IF (MECANI .EQ. 'ISODEV') VINF(NVI) = 3.D0

9999     CONTINUE
         END

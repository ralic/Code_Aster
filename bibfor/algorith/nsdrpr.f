      SUBROUTINE NSDRPR(OPTION,TYPMOD,COMPOR,NDIM,IMATE,IMATSE,DEPS,
     &    DEDT,SIGMS,VARMS,VARM,SIGM,VARP,SIPAS,SIGP,SIGPS,VARPS,STYPSE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 15/11/2004   AUTEUR ROMEO R.FERNANDES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C TOLE CRP_20
      IMPLICIT NONE
      INTEGER NDIM,IMATE,IMATSE
      CHARACTER*8 TYPMOD(*)
      CHARACTER*16 OPTION,COMPOR(4)
      CHARACTER*24 STYPSE
      REAL*8 DEPS(*),DEDT(*),SIGMS(*),VARMS(*),VARPS(*)
      REAL*8 VARM(*),SIGM(*),VARP(*),SIPAS(*),SIGP(*),SIGPS(*)
C ----------------------------------------------------------------------
C --- VARIABLES UTILISEES LORS DE LA RECUPERATION DES CARACTERISTIQUES
      CHARACTER*8 NOMRES(6)
      CHARACTER*2 CODRET(6),FB2
      REAL*8 VALRES(6),TYPEDP,R8VIDE
C --- CARACTERISTIQUES MATERIAUX
      REAL*8 E,NU,ALPHA,SY,PULTM,H,SYULTM,TROISK,DEUXMU,TROIMU
C --- CARACTERISTIQUES MATERIAUX SENSIBLES
      REAL*8 ES,NUS,ALPHAS,SIGYS,PULTMS,HS,SYULTS
      REAL*8 DDEUMU,DTROIK,DALPHA,DSY,DPULTM,DH,DSYULT
      REAL*8 DELTAP,DEPSMO,DEPSDV(6),DSIEEQ
      REAL*8 DPPHI,DPSIG(6),DPP
      REAL*8 SIGMMO,SIGMDV(6),SIGEL(6),SIELEQ,SIELDV(6)
      REAL*8 DSIDEP(6,6),SIGPMO,SIPSMO,SIGPEQ, SIPEQS, SIEEQS, SIMSMO
      REAL*8 KRON(6),SOUTO1,SOUTO2,DPMIN
      INTEGER NDIMSI,K,L,IRET
      PARAMETER (DPMIN = 1.D-15)
      CHARACTER*24 BLAN24
      PARAMETER ( BLAN24 = '                        ' )
      DATA KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
C ----------------------------------------------------------------------
C --- INITIALISATIONS --------------------------------------------------
C ----------------------------------------------------------------------
      NDIMSI = 2*NDIM
      DELTAP = VARP(1) - VARM(1)
      DDEUMU = 0.D0
      DTROIK = 0.D0
      DALPHA = 0.D0
      DSY = 0.D0
      DPULTM = 0.D0
      DH = 0.D0
      DSYULT = 0.D0
      FB2 = 'F '
C ----------------------------------------------------------------------
C --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ----------------------
C ----------------------------------------------------------------------
      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      NOMRES(3) = 'ALPHA'
      NOMRES(4) = 'SY'
      NOMRES(5) = 'P_ULTM'
      CALL RCVALA(IMATE, ' ', 'ELAS', 0, ' ', 0.D0,
     &            2, NOMRES(1),VALRES(1), CODRET,'FM')
      CALL RCVALA(IMATE, ' ', 'DRUCK_PRAGER', 0, ' ', 0.D0,
     &            3, NOMRES(3),VALRES(3), CODRET,'FM')
      E = VALRES(1)
      NU = VALRES(2)
      DEUXMU = E/(1.D0+NU)
      TROIMU = 1.5D0*DEUXMU
      TROISK = E/(1.D0-2.D0*NU)
      ALPHA = VALRES(3)
      SY = VALRES(4)
      PULTM = VALRES(5)
      TYPEDP = R8VIDE()
      CALL RCVALA(IMATE, ' ', 'DRUCK_PRAGER', 0, ' ', 0.D0,
     &            1, 'TYPE_DP', TYPEDP, CODRET, '  ')

      IF (TYPEDP.EQ.1.D0) THEN
C ----------------------------------------------------------------------
C --- DRUCKER PRAGER LINEAIRE : PARAMETRES MATERIAUX SENSIBLES ---------
C ----------------------------------------------------------------------
        NOMRES(6) = 'H'
        CALL RCVALA(IMATE, ' ', 'DRUCK_PRAGER', 0, ' ', 0.D0,
     &              1, NOMRES(6),VALRES(6), CODRET,'FM')
        H = VALRES(6)

        IF (STYPSE.NE.BLAN24) THEN

         NOMRES(1) = 'E'
         NOMRES(2) = 'NU'
         CALL RCVALA(IMATSE,' ','ELAS',0,' ',0.D0,2,
     &           NOMRES(1),VALRES(1),CODRET(1),'FM')
         ES = VALRES(1)
         NUS = VALRES(2)

         IF (COMPOR(1)(1:14).EQ.'DRUCKER_PRAGER') THEN
          NOMRES(1) = 'ALPHA'
          NOMRES(2) = 'SY'
          NOMRES(3) = 'P_ULTM'
          NOMRES(4) = 'H'
          CALL RCVALA(IMATSE,' ','DRUCK_PRAGER',0,' ',0.D0,4,
     &             NOMRES,VALRES, CODRET,FB2)
          ALPHAS = VALRES(1)
          SIGYS = VALRES(2)
          PULTMS = VALRES(3)
          HS = VALRES(4)
         ENDIF

         DTROIK = (ES*(1.D0-2.D0*NU)+2.D0*E*NUS)/
     &          ((1.D0-2.D0*NU)*(1.D0-2.D0*NU))
         DDEUMU = (ES*(1.D0+NU)-E*NUS)/((1.D0+NU)*(1.D0+NU))

         IF (COMPOR(1)(1:14).EQ.'DRUCKER_PRAGER') THEN
          DALPHA = ALPHAS
          DSY    = SIGYS
          DPULTM = PULTMS
          DH     = HS
         ENDIF

        ENDIF

      ELSEIF (TYPEDP.EQ.2.D0) THEN
C ----------------------------------------------------------------------
C --- DRUCKER PRAGER PARABOLIQUE : PARAMETRES MATERIAUX SENSIBLES ------
C ----------------------------------------------------------------------
        NOMRES(6) = 'SY_ULTM'
        CALL RCVALA(IMATE, ' ', 'DRUCK_PRAGER', 0, ' ', 0.D0,
     &              1, NOMRES(6),VALRES(6), CODRET,'FM')
        SYULTM = VALRES(6)

        IF (STYPSE.NE.BLAN24) THEN

         NOMRES(1) = 'E'
         NOMRES(2) = 'NU'
         CALL RCVALA(IMATSE,' ','ELAS',0,' ',0.D0,2,
     &           NOMRES(1),VALRES(1),CODRET(1),'FM')
         ES = VALRES(1)
         NUS = VALRES(2)

         IF (COMPOR(1)(1:14).EQ.'DRUCKER_PRAGER') THEN
          NOMRES(1) = 'ALPHA'
          NOMRES(2) = 'SY'
          NOMRES(3) = 'P_ULTM'
          NOMRES(4) = 'SY_ULTM'
          CALL RCVALA(IMATSE,' ','DRUCK_PRAGER',0,' ',0.D0,4,
     &             NOMRES,VALRES, CODRET,FB2)
          ALPHAS = VALRES(1)
          SIGYS = VALRES(2)
          PULTMS = VALRES(3)
          SYULTS = VALRES(4)
         ENDIF

         DTROIK = (ES*(1.D0-2.D0*NU)+2.D0*E*NUS)/
     &          ((1.D0-2.D0*NU)*(1.D0-2.D0*NU))
         DDEUMU = (ES*(1.D0+NU)-E*NUS)/((1.D0+NU)*(1.D0+NU))

         IF (COMPOR(1)(1:14).EQ.'DRUCKER_PRAGER') THEN
          DALPHA = ALPHAS
          DSY    = SIGYS
          DPULTM = PULTMS
          DSYULT = SYULTS
         ENDIF

        ENDIF

      ENDIF

      IF (OPTION(1:14).EQ.'MECA_SENS_MATE') THEN
C ----------------------------------------------------------------------
C --- PREMIER PASSAGE, SENSIBILITE AUX DONNEES MATERIAUX ---------------
C ----------------------------------------------------------------------
        DEPSMO = (DEPS(1)+DEPS(2)+DEPS(3)) / 3.D0

        DO 100 K = 1,NDIMSI
          DEPSDV(K) = DEPS(K) - DEPSMO*KRON(K)
 100    CONTINUE

        SIGMMO = (SIGM(1)+SIGM(2)+SIGM(3)) / 3.D0

        IF (DELTAP.LE.DPMIN) THEN
C ----------------------------------------------------------------------
C ----- PREMIER CAS : ELASTICITE LINEAIRE ------------------------------
C ----------------------------------------------------------------------
          DO 200 K = 1,NDIMSI
            SIGPS(K) = SIGMS(K) + DDEUMU*DEPSDV(K) +
     &                 DTROIK*DEPSMO*KRON(K)
 200      CONTINUE
        ELSE
C ----------------------------------------------------------------------
C ----- DEUXIEME CAS : ELASTOPLASTICITE DRUCKER PRAGER -----------------
C ----------------------------------------------------------------------
          SIELEQ = 0.D0
          DO 210 K = 1,NDIMSI
            SIGMDV(K) = SIGM(K) - SIGMMO*KRON(K)
            SIELDV(K) = SIGMDV(K) + DEUXMU*DEPSDV(K)
            SIELEQ = SIELEQ + SIELDV(K)**2
 210      CONTINUE
          SIELEQ = SQRT(1.5D0*SIELEQ)

          DSIEEQ = 0.D0
          DO 220 K = 1,NDIMSI
            DSIEEQ = DSIEEQ + DEPSDV(K)*SIELDV(K)
 220      CONTINUE
          DSIEEQ = 1.5D0*DDEUMU*DSIEEQ/SIELEQ
        
          IF (TYPEDP.EQ.1.D0) THEN
C ------- DRUCKER PRAGER LINEAIRE
            IF ((DELTAP+DPMIN).GT.(SIELEQ/TROIMU)) THEN
C --------- NON EXISTENCE DE LA SOLUTION
              DPPHI = (DSIEEQ-DELTAP*1.5D0*DDEUMU)/TROIMU

              DO 300 K = 1,NDIMSI
                DPSIG(K) = (1.5D0*SIELDV(K)/SIELEQ)/TROIMU
 300          CONTINUE

              DPP = 0.D0

            ELSEIF (VARP(1).LT.PULTM) THEN
C --------- EXISTENCE DE LA SOLUTION, ON A PAS DEPASSE PULTM
              DPPHI = (DSIEEQ+DALPHA*3.D0*(SIGMMO+TROISK*DEPSMO)
     &                 +ALPHA*DTROIK*3.D0*DEPSMO
     &                 -DH*VARM(1)-DSY-
     &                 DELTAP*(3.D0*DTROIK*ALPHA**2+
     &                     6.D0*TROISK*ALPHA*DALPHA+1.5D0*DDEUMU+DH)) /
     &                (3.D0*TROISK*ALPHA**2 + TROIMU + H)

              DO 310 K = 1,NDIMSI
                DPSIG(K) = (ALPHA*KRON(K) + 1.5D0*SIELDV(K)/SIELEQ) /
     &                     (TROIMU + 3.D0*TROISK*ALPHA**2 + H)
 310          CONTINUE

              DPP = -H / (TROIMU+3.D0*TROISK*ALPHA**2+H) * VARMS(1)

            ELSE
C --------- EXISTENCE DE LA SOLUTION, ON A DEPASSE PULTM
              DPPHI = (DSIEEQ+DALPHA*3.D0*(SIGMMO+TROISK*DEPSMO)+
     &                 ALPHA*DTROIK*3.D0*DEPSMO-DH*PULTM-H*DPULTM-DSY-
     &                 DELTAP*(3.D0*DTROIK*ALPHA**2+
     &                         6.D0*TROISK*ALPHA*DALPHA+1.5D0*DDEUMU))/
     &                (TROIMU+3.D0*TROISK*ALPHA**2)

              DO 320 K = 1,NDIMSI
                DPSIG(K) = (ALPHA*KRON(K)+1.5D0*SIELDV(K)/SIELEQ)/
     &                     (TROIMU+3.D0*TROISK*ALPHA**2)
 320          CONTINUE

              DPP = 0.D0

            ENDIF
          ELSEIF (TYPEDP.EQ.2.D0) THEN
C ------- DRUCKER PRAGER PARABOLIQUE

            IF ((DELTAP+DPMIN).GT.(SIELEQ/TROIMU)) THEN
C --------- NON EXISTENCE DE LA SOLUTION
              DPPHI = (DSIEEQ-DELTAP*1.5D0*DDEUMU)/TROIMU

              DO 400 K = 1,NDIMSI
                DPSIG(K) = (1.5D0*SIELDV(K)/SIELEQ)/TROIMU
 400          CONTINUE

              DPP = 0.D0

            ELSE IF (VARP(1).LT.PULTM) THEN
C --------- EXISTENCE DE LA SOLUTION, ON A PAS DEPASSE PULTM
              H = -2.D0*SY*(1.D0-(1.D0-SQRT(SYULTM/SY))*VARP(1)/PULTM)*
     &                     (1.D0-SQRT(SYULTM/SY))/PULTM

              DPPHI = (DSIEEQ + DALPHA*3.D0*(SIGMMO+TROISK*DEPSMO) +
     &                 ALPHA*DTROIK*3.D0*DEPSMO -
     &                 DELTAP*(3.D0*DTROIK*ALPHA**2+
     &                         6.D0*TROISK*ALPHA*DALPHA+1.5D0*DDEUMU) -
     &                 DSY*
     &                 (1.D0-(1.D0-SQRT(SYULTM/SY))*VARP(1)/PULTM)**2 +
     &                 H*VARP(1)*DPULTM/PULTM +
     &                 H*VARP(1)*.5D0*DSYULT/(SQRT(SY*SYULTM)-SYULTM) -
     &                 H*VARP(1)*.5D0*DSY/(SY*(SQRT(SY/SYULTM)-1.D0)))/
     &                (3.D0*TROISK*ALPHA**2+TROIMU+H)

              DO 410 K = 1,NDIMSI
                DPSIG(K) = (1.5D0*SIELDV(K)/SIELEQ+ALPHA*KRON(K))/
     &                     (3.D0*TROISK*ALPHA**2+TROIMU+H)
 410          CONTINUE

              DPP = -H/(3.D0*TROISK*ALPHA**2+TROIMU+H)*VARMS(1)
            ELSE
C --------- EXISTENCE DE LA SOLUTION, ON A DEPASSE PULTM
              DPPHI = (DSIEEQ+DALPHA*3.D0*(SIGMMO+TROISK*DEPSMO)+
     &                 ALPHA*DTROIK*3.D0*DEPSMO-DSYULT-
     &                 DELTAP*(3.D0*DTROIK*ALPHA**2+
     &                        6.D0*TROISK*ALPHA*DALPHA+1.5D0*DDEUMU))/
     &                (3.D0*TROISK*ALPHA**2+TROIMU)

              DO 420 K = 1,NDIMSI
                DPSIG(K) = (ALPHA*KRON(K)+1.5D0*SIELDV(K)/SIELEQ)/
     &                     (TROIMU+3.D0*TROISK*ALPHA**2)
 420          CONTINUE

              DPP = 0.D0
            ENDIF
          ENDIF
C ----------------------------------------------------------------------

          SOUTO1 = 0.D0
          DO 500 K = 1,NDIMSI
            SOUTO1 = SOUTO1 + SIELDV(K)*SIGMS(K)
 500      CONTINUE

          SOUTO2 = 0.D0
          DO 510 K = 1,NDIMSI
            SOUTO2 = SOUTO2 + DPSIG(K)*SIGMS(K)
 510      CONTINUE

          DO 520 K = 1,NDIMSI
            SIGPS(K) = SIGMS(K) +
     &                 DDEUMU*DEPSDV(K) -
     &                 1.5D0*DDEUMU*DELTAP/SIELEQ*SIELDV(K) -
     &                 TROIMU/SIELEQ*(DPPHI*SIELDV(K) -
     &                        DELTAP*DSIEEQ/SIELEQ*SIELDV(K) +
     &                        DELTAP*DDEUMU*DEPSDV(K)) +
     &                 KRON(K)*(DTROIK*DEPSMO -
     &                          DTROIK*ALPHA*DELTAP -
     &                          TROISK*DALPHA*DELTAP -
     &                          TROISK*ALPHA*DPPHI) -
     &                 TROIMU/SIELEQ*(
     &                        SIELDV(K)*SOUTO2 -
     &                        DELTAP*1.5D0*SIELDV(K)*SOUTO1/(SIELEQ**2)+
     &                        DELTAP*(SIGMS(K) - KRON(K)*
     &                            (SIGMS(1)+SIGMS(2)+SIGMS(3))/3.D0)) -
     &                 KRON(K)*TROISK*ALPHA*SOUTO2 -
     &                 TROIMU*DPP/SIELEQ*SIELDV(K) -
     &                 KRON(K)*TROISK*ALPHA*DPP
 
 520      CONTINUE
        ENDIF

      ELSEIF (OPTION(1:14).EQ.'MECA_SENS_CHAR') THEN
C ----------------------------------------------------------------------
C --- PREMIER PASSAGE, SENSIBILITE AU CHARGEMENT -----------------------
C ----------------------------------------------------------------------
        IF (DELTAP.LE.DPMIN) THEN
C ----- PREMIER CAS : ELASTICITE LINEAIRE ------------------------------
          DO 600 K = 1,NDIMSI
            SIGPS(K) = SIGMS(K)
 600      CONTINUE
        ELSE
C ----- DEUXIEME CAS : ELASTOPLASTICITE DRUCKER PRAGER -----------------
          DEPSMO = (DEPS(1)+DEPS(2)+DEPS(3))/3.D0

          DO 610 K = 1,NDIMSI
            DEPSDV(K) = DEPS(K) - DEPSMO*KRON(K)
 610      CONTINUE

          SIGMMO = (SIGM(1)+SIGM(2)+SIGM(3))/3.D0

          SIELEQ = 0.D0
          DO 620 K = 1,NDIMSI
            SIGMDV(K) = SIGM(K) - SIGMMO*KRON(K)
            SIELDV(K) = SIGMDV(K) + DEUXMU*DEPSDV(K)
            SIELEQ = SIELEQ + SIELDV(K)**2
 620      CONTINUE
          SIELEQ = SQRT(1.5D0*SIELEQ)

          IF (TYPEDP.EQ.1.D0) THEN
C ------- DRUCKER PRAGER LINEAIRE
            IF ((DELTAP+DPMIN).GT.(SIELEQ/TROIMU)) THEN
C --------- NON EXISTENCE DE LA SOLUTION
              DO 630 K = 1,NDIMSI
                DPSIG(K) = (1.5D0*SIELDV(K)/SIELEQ) / TROIMU
 630          CONTINUE
                
              DPP = 0.D0

            ELSEIF (VARP(1).LT.PULTM) THEN
C --------- EXISTENCE DE LA SOLUTION, ON A PAS DEPASSE PULTM
              DO 640 K = 1,NDIMSI
                DPSIG(K) = (ALPHA*KRON(K) + 1.5D0*SIELDV(K)/SIELEQ) /
     &                     (TROIMU + 3.D0*TROISK*ALPHA**2 + H)
 640          CONTINUE

              DPP = -H/(TROIMU+3.D0*TROISK*ALPHA**2+H)*VARMS(1)

            ELSE
C --------- EXISTENCE DE LA SOLUTION, ON A DEPASSE PULTM
              DO 650 K = 1,NDIMSI
                DPSIG(K) = (ALPHA*KRON(K) + 1.5D0*SIELDV(K)/SIELEQ) /
     &                     (TROIMU + 3.D0*TROISK*ALPHA**2)
 650          CONTINUE

              DPP = 0.D0

            ENDIF
          ELSEIF (TYPEDP.EQ.2.D0) THEN
C ------- DRUCKER PRAGER PARABOLIQUE
            IF ((DELTAP+DPMIN).GT.(SIELEQ/TROIMU)) THEN
C --------- NON EXISTENCE DE LA SOLUTION
              DO 660 K = 1,NDIMSI
                DPSIG(K) = (1.5D0*SIELDV(K)/SIELEQ) / TROIMU
 660          CONTINUE

              DPP = 0.D0

            ELSEIF (VARP(1).LT.PULTM) THEN
C --------- EXISTENCE DE LA SOLUTION, ON A PAS DEPASSE PULTM
              H = -2.D0*SY*(1.D0-(1.D0-SQRT(SYULTM/SY))*VARP(1)/PULTM)*
     &                     (1.D0-SQRT(SYULTM/SY)) / PULTM

              DO 670 K = 1,NDIMSI
                DPSIG(K) = (1.5D0*SIELDV(K)/SIELEQ + ALPHA*KRON(K)) /
     &                     (3.D0*TROISK*ALPHA**2 + TROIMU + H)
 670          CONTINUE

              DPP = -H / (3.D0*TROISK*ALPHA**2 + TROIMU + H) * VARMS(1)

            ELSE
C --------- EXISTENCE DE LA SOLUTION, ON A DEPASSE PULTM
              DO 680 K = 1,NDIMSI
                DPSIG(K) = (ALPHA*KRON(K) + 1.5D0*SIELDV(K)/SIELEQ) /
     &                     (TROIMU + 3.D0*TROISK*ALPHA**2)
 680          CONTINUE

              DPP = 0.D0

            ENDIF
          ENDIF

          SOUTO1 = 0.D0
          DO 750 K = 1,NDIMSI
            SOUTO1 = SOUTO1 + SIELDV(K)*SIGMS(K)
 750      CONTINUE

          SOUTO2 = 0.D0
          DO 760 K = 1,NDIMSI
            SOUTO2 = SOUTO2 + DPSIG(K)*SIGMS(K)
 760      CONTINUE

          DO 770 K = 1,NDIMSI
            SIGPS(K) = SIGMS(K) +
     &                 0.D0 +
     &                 0.D0 -
     &                 TROIMU/SIELEQ*(
     &                        SIELDV(K)*SOUTO2 -
     &                        DELTAP*1.5D0*SIELDV(K)*SOUTO1/(SIELEQ**2)+
     &                        DELTAP*(SIGMS(K) - KRON(K)*
     &                            (SIGMS(1)+SIGMS(2)+SIGMS(3))/3.D0)) -
     &                 KRON(K)*TROISK*ALPHA*SOUTO2 -
     &                 TROIMU*DPP/SIELEQ*SIELDV(K) -
     &                 KRON(K)*TROISK*ALPHA*DPP
 770      CONTINUE
        ENDIF
C ----------------------------------------------------------------------
      ELSEIF (OPTION(1:14).EQ.'MECA_SENS_RAPH') THEN
C ----------------------------------------------------------------------
C --- DEUXIEME PASSAGE : CALCUL DES DERIVEES DES CONTRAINTES -----------
C ---------------------- ET DES VARIABLES INTERNES ---------------------
C ----------------------------------------------------------------------
        DEPSMO = (DEPS(1)+DEPS(2)+DEPS(3))/3.D0

        DO 800 K = 1,NDIMSI
          DEPSDV(K) = DEPS(K) - DEPSMO*KRON(K)
 800    CONTINUE
        SIGMMO = (SIGM(1)+SIGM(2)+SIGM(3))/3.D0

        IF (DELTAP.LE.DPMIN) THEN
C ----------------------------------------------------------------------
C ----- PREMIER CAS : ELASTICITE LINEAIRE ------------------------------
C ----------------------------------------------------------------------
C ------- CALCUL DE SIGPS
          DO 810 K = 1,NDIMSI
            SIGPS(K) = SIPAS(K) + DEUXMU*DEPSDV(K) +
     &                 TROISK*DEPSMO*KRON(K)
 810      CONTINUE

C ------- CALCUL DE VARPS(1)
          VARPS(1) = VARMS(1)
        ELSE
C ----------------------------------------------------------------------
C ----- DEUXIEME CAS : ELASTOPLASTICITE DRUCKER PRAGER -----------------
C ----------------------------------------------------------------------
C ------- CALCUL DE L'OPERATEUR TANGENT DSIDEP
          CALL REDRPR(TYPMOD(1),IMATE,SIGP,VARP,DSIDEP,IRET)

C ------- CALCUL DE SIGPS
          DO 900 K = 1,NDIMSI
            SIGPS(K) = SIPAS(K)
            DO 900 L = 1,NDIMSI
              SIGPS(K) = SIGPS(K) + DSIDEP(K,L)*DEPS(L)
 900      CONTINUE

C ------- CALCUL DES CONTRAINTES EQUIVALENTES SENSIBLES SIEEQS ET SIPEQS
          SIGPMO = (SIGP(1)+SIGP(2)+SIGP(3))/3.D0
          SIPSMO = (SIGPS(1)+SIGPS(2)+SIGPS(3))/3.D0
          SIMSMO = (SIGMS(1)+SIGMS(2)+SIGMS(3))/3.D0
          SIGPEQ = 0.D0
          DO 910 K = 1,NDIMSI
            SIGPEQ = SIGPEQ + (SIGP(K) - KRON(K)*SIGPMO)**2
 910      CONTINUE
          SIGPEQ = SQRT(1.5D0*SIGPEQ)

          IF (SIGPEQ.GT.1.D0) THEN
C ------- EXISTENCE DE LA SOLUTION

C --------- CALCUL DE SIEEQS
            SIEEQS = 0.D0
            SIELEQ = 0.D0
            DEPSMO = (DEDT(1)+DEDT(2)+DEDT(3)) / 3.D0
            SIGMMO = (SIGM(1)+SIGM(2)+SIGM(3)) / 3.D0
            DO 935 K = 1,NDIMSI
              DEPSDV(K) = DEDT(K) - DEPSMO*KRON(K)
              SIGMDV(K) = SIGM(K) - SIGMMO*KRON(K)
              SIELDV(K) = SIGMDV(K) + DEUXMU*DEPSDV(K)
              SIEEQS = SIEEQS + (SIGMS(K) - SIMSMO*KRON(K) +
     &                           DDEUMU*DEPSDV(K) + DEUXMU*DEPS(K)) *
     &                           SIELDV(K)
              SIELEQ = SIELEQ + SIELDV(K)**2
 935        CONTINUE
            SIELEQ = SQRT(1.5D0*SIELEQ)
            SIEEQS = 1.5D0*SIEEQS/SIELEQ

C --------- CALCUL DE SIPEQS
            SIPEQS = 0.D0
            DO 940 K = 1,NDIMSI
              SIPEQS = SIPEQS + 
     &               (SIGPS(K)-SIPSMO*KRON(K))*(SIGP(K)-SIGPMO*KRON(K))
 940        CONTINUE
            SIPEQS = 1.5D0*SIPEQS/SIGPEQ

C --------- ON EN DEDUIT VARPS(1)
            VARPS(1) = VARMS(1) +
     &                 (SIEEQS - SIPEQS -1.5D0*DDEUMU*DELTAP) / TROIMU
          ELSE
C ------- NON EXISTENCE DE LA SOLUTION
C --------- CALCUL DE SIEEQS
            SIEEQS = 0.D0
            SIELEQ = 0.D0
            DEPSMO = (DEDT(1)+DEDT(2)+DEDT(3)) / 3.D0
            SIGMMO = (SIGM(1)+SIGM(2)+SIGM(3)) / 3.D0
            DO 950 K = 1,NDIMSI
              DEPSDV(K) = DEDT(K) - DEPSMO*KRON(K)
              SIGMDV(K) = SIGM(K) - SIGMMO*KRON(K)
              SIELDV(K) = SIGMDV(K) + DEUXMU*DEPSDV(K)
              SIEEQS = SIEEQS + (SIGMS(K) - SIMSMO*KRON(K) +
     &                           DDEUMU*DEPSDV(K) + DEUXMU*DEPS(K)) *
     &                           SIELDV(K)
              SIELEQ = SIELEQ + SIELDV(K)**2
 950        CONTINUE
            SIELEQ = SQRT(1.5D0*SIELEQ)
            SIEEQS = 1.5D0*SIEEQS/SIELEQ

C --------- ON EN DEDUIT VARPS(1)
            VARPS(1) = VARMS(1) + (SIEEQS -1.5D0*DDEUMU*DELTAP)/TROIMU
          ENDIF
        ENDIF
C ----------------------------------------------------------------------
      ENDIF
C ----------------------------------------------------------------------
C --- FIN --------------------------------------------------------------
C ----------------------------------------------------------------------
      END

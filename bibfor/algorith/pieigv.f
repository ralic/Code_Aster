      SUBROUTINE PIEIGV(NDIM, TYPMOD, TAU,IMATE, NONLOC, VIM,EPSM,
     &                  EPSPC, EPSDC, ETAMIN,ETAMAX,A0, A1,A2,A3,ETAS)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 15/05/2007   AUTEUR GENIAUT S.GENIAUT 
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

C TOLE CRP_20

      IMPLICIT NONE
      CHARACTER*8        TYPMOD(2)
      INTEGER            NDIM, IMATE
      REAL*8             EPSM(6),EPSPC(6), EPSDC(6),NONLOC(4),ETAMIN
      REAL*8             ETAMAX,TAU
      REAL*8             VIM(2),A0, A1,A2,A3,ETAS
C ----------------------------------------------------------------------
C     PILOTAGE LOI DE COMPORTEMENT ENDO_ISOT_BETON EN NON LOCAL
C     GRAD_VARI
C
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  TYPMOD  : TYPE DE MODELISATION
C IN  TAU     : 2ND MEMBRE DE L'EQUATION F(ETA)=TAU
C IN  IMATE   : NATURE DU MATERIAU
C IN  SIGM    : CONTRAINTE EN T-
C IN  VIM     : VARIABLES INTERNES EN T-
C IN  EPSPC   : CORRECTION DE DEFORMATIONS DUES AUX CHARGES FIXES
C IN  EPSDC   : CORRECTION DE DEFORMATIONS DUES AUX CHARGES PILOTEES
C IN  ETAMIN  : DONNEE UTILISATEUR DU MINIMUM DE ETA
C IN  ETAMAX  : DONNEE UTILISATEUR DU MAXIMUM DE ETA
C OUT A0      : LINEARISATION DU CRITERE : FEL = A0 + A1*ETA
C OUT A1      : CF A0
C OUT A2      : IDEM A0 POUR LA SECONDE SOLUTION EVENTUELLE;R8VIDE SINON
C OUT A3      : IDEM A1 POUR LA SECONDE SOLUTION EVENTUELLE;R8VIDE SINON
C OUT ETAS    : SI PAS DE SOLUTION : LE MINIMUM ; R8VIDE SINON
C ----------------------------------------------------------------------

      LOGICAL     CPLAN,MINI,FINI,RECHBG,RECHBD
      INTEGER     NDIMSI, K, NRAC,NSOL,ITER,NITMAX
      INTEGER     NCAS
      REAL*8      TREPSD, COPLAN, SIGELP(6), SIGELD(6)
      REAL*8      CRIT,RTEMP,DMAX
      REAL*8      TR(6),VECP(3,3),RAC2,CRITP
      REAL*8      FPD, DM, D, P0, P1, P2, ETA, RAC(2), IND(4),EPM(3)
      REAL*8      E, NU, LAMBDA, DEUXMU, GAMMA, SEUIL, SEUREL,TREPSM
      REAL*8      K0,K1,SICR,R
      REAL*8      R8NRM2,EPSP(7), EPSD(7),X(4),Y(4),Z(4)
      REAL*8      RMINI,EPSTOL
      REAL*8      TREPS,SIGEL(3),ETA1,ETA2,ETAC,CRIT1,CRIT2,CRITC,CRITP3
      REAL*8      RTEMP1,RTEMP2,R8VIDE,SEUILA,CRITP1,CRITP2,CRIT3,RPAS
      REAL*8      CRITR1,CRITR2,LAMBDD,DEUMUD,FD,RIGMIN,ETA0,ETA3,C,Q
      REAL*8      EPSVP
      CHARACTER*2 CODRET(3)
      CHARACTER*8 NOMRES(3)
      REAL*8      VALRES(3)
      REAL*8      R8DOT

      NITMAX=50
      EPSTOL=1.D-6
      Q=0.61803399D0
      C=1.D0-Q
      NSOL=0
      EPSVP = 1.D-6/ABS(ETAMAX-ETAMIN)



C -- OPTION ET MODELISATION
      CPLAN = (TYPMOD(1).EQ.'C_PLAN  ')
      NDIMSI = 2*NDIM
      RAC2=SQRT(2.D0)

C -- CAS DE L'ENDOMMAGEMENT SATURE
      IF ((NINT(VIM(2)) .EQ. 2)) THEN
        GOTO 666
      END IF


C -- LECTURE DES CARACTERISTIQUES THERMOELASTIQUES
      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      CALL RCVALA(IMATE,' ','ELAS',0,' ',0.D0,2,
     &              NOMRES,VALRES,CODRET, 'FM')
      E      = VALRES(1)
      NU     = VALRES(2)
      LAMBDA = E * NU / (1.D0+NU) / (1.D0 - 2.D0*NU)
      DEUXMU = E/(1.D0+NU)

C    LECTURE DES CARACTERISTIQUES D'ENDOMMAGEMENT
      NOMRES(1) = 'D_SIGM_EPSI'
      NOMRES(2) = 'SYT'
      NOMRES(3) = 'SYC'
      CALL RCVALA(IMATE,' ','BETON_ECRO_LINE',0,' ',0.D0,3,
     &            NOMRES,VALRES,CODRET,' ')
      GAMMA  = - E/VALRES(1)
      K0=VALRES(2)**2 *(1.D0+GAMMA)/(2.D0*E)
     &               *(1.D0+NU-2.D0*NU**2)/(1.D0+NU)
      IF (NU.EQ.0) THEN
        IF (CODRET(3).EQ.'OK') THEN
          CALL U2MESS('F','ALGORITH4_52')
        ELSE
          SEUIL=K0
        ENDIF
      ELSE
        SICR=SQRT((1.D0+NU-2.D0*NU**2)/(2.D0*NU**2))*VALRES(2)
        IF (CODRET(3).EQ.'NO') THEN
          SEUIL=K0
        ELSE
          IF (VALRES(3).LT.SICR) THEN
            CALL U2MESS('F','ALGORITH4_53')
          ELSE
            K1=VALRES(3)*(1.D0+GAMMA)*NU**2/(1.D0+NU)/(1.D0-2.D0*NU)
     &        -K0*E/(1.D0-2.D0*NU)/VALRES(3)
            TREPSM=0.D0
            DO 1 K=1,NDIM
              TREPSM=TREPSM+EPSM(K)
 1          CONTINUE
            IF (TREPSM.GT.0.D0) THEN
              TREPSM=0.D0
            ENDIF
            SEUIL  = K0-K1*TREPSM
          ENDIF
        ENDIF
      ENDIF
      SEUILA = SEUIL


      R=NONLOC(2)

C ======================================================================
C                CALCUL DES DEFORMATIONS POUR LINEARISATION
C ======================================================================

C    ETAT MECANIQUE EN T-

      DM   = VIM(1)
      DMAX=1.D0
      D = MIN(DMAX,DM+TAU)
      FPD = (1+GAMMA) / (1+GAMMA*D)**2


C      D   = VIM(1)
C      FPD = (1+GAMMA) / (1+GAMMA*D)**2
C      SEUREL = SEUIL * (1+GAMMA*D)**2 / (1+GAMMA)
C      SEUIL=SEUIL+SEUREL*TAU

C -- CALCUL DES DEFORMATIONS EN PRESENCE DE CONTRAINTES PLANES

      IF (CPLAN) THEN
        COPLAN  = - NU/(1.D0-NU)
        EPSPC(3)  = COPLAN * (EPSPC(1)+EPSPC(2))
        EPSDC(3)  = COPLAN * (EPSDC(1)+EPSDC(2))
      END IF
      DO 44 K=1,3
        EPSP(K) = EPSPC(K)
        EPSD(K) = EPSDC(K)
44    CONTINUE
       DO 45 K=4,NDIMSI
        EPSP(K) = EPSPC(K)/RAC2
        EPSD(K) = EPSDC(K)/RAC2
45    CONTINUE
      IF (NDIMSI.LT.6) THEN
        DO 46 K=NDIMSI+1,6
          EPSP(K)=0.D0
          EPSD(K)=0.D0
46      CONTINUE
      ENDIF

        EPSP(7) = NONLOC(3)
        EPSD(7) = NONLOC(4)

C -- QUEL PB A RESOUDRE ?
C      SI EPSD N'A QUE DES VP POSITIVES :F(+INFINI)=+INFINI
C                                        F(-INFINI)=-SEUIL    NSOL=1
C                             NEGATIVES :F(-INFINI)=+INFINI
C                                        F(+INFINI)=-SEUIL    NSOL=-1
C      SINON : F(+INFINI)=+INFINI ET F(-INFINI)=+INFINI       NSOL=2





C - ON COMMENCE DONC PAR REGARDER LES VP DE EPSD
      RECHBG=.FALSE.
      RECHBD=.FALSE.
      TR(1) = EPSD(1)
      TR(2) = EPSD(4)
      TR(3) = EPSD(5)
      TR(4) = EPSD(2)
      TR(5) = EPSD(6)
      TR(6) = EPSD(3)

C -- DIAGONALISATION AVEC TRI EN VAL RELATIVE CROISSANT
      CALL DIAGP3(TR,VECP,EPM)
      NSOL=2
      IF (EPM(1).GT.EPSVP) THEN
        NSOL=1
      ELSE IF (ABS(EPM(1)).LT.EPSVP) THEN
        RPAS=(ETAMAX-ETAMIN)
        ETA=ETAMIN
        CALL CRITEV(EPSP,EPSD,ETA,LAMBDA,DEUXMU,FPD,SEUIL,R*D,
     &              CRIT1,CRITP1)
301     CONTINUE
        RPAS=RPAS*2
        IF (.NOT.((CRIT1.GT.0.D0).AND.(CRITP1.LT.0.D0))) THEN
          IF (CRIT1.LT.0.D0) THEN
            NSOL=1
          ELSE
            ETA=ETA-RPAS
            CALL CRITEV(EPSP,EPSD,ETA,LAMBDA,DEUXMU,FPD,SEUIL,R*D,
     &                  CRIT1,CRITP1)
            GOTO 301
          ENDIF
        ENDIF
      ENDIF
      IF (EPM(3).LT.-EPSVP) THEN
        NSOL=-1
      ELSE IF (ABS(EPM(3)).LT.EPSVP) THEN
        RPAS=(ETAMAX-ETAMIN)
        ETA=ETAMAX
        CALL CRITEV(EPSP,EPSD,ETA,LAMBDA,DEUXMU,FPD,SEUIL,R*D,
     &              CRIT1,CRITP1)
302     CONTINUE
        RPAS=RPAS*2
        IF (.NOT.((CRIT1.GT.0.D0).AND.(CRITP1.GT.0.D0))) THEN
          IF (CRIT1.LT.0.D0) THEN
            IF (NSOL.EQ.1) THEN
              GOTO 666
            ELSE
              NSOL=-1
            ENDIF
          ELSE
            ETA=ETA+RPAS
            CALL CRITEV(EPSP,EPSD,ETA,LAMBDA,DEUXMU,FPD,SEUIL,R*D,
     &                  CRIT1,CRITP1)
            GOTO 302
          ENDIF
        ENDIF
      ENDIF





C -- RECHBG : VRAI -> IL FAUT TROUVER ETA SUFFISAMMENT PETIT POUR
C                     AVOIR F(ETA)>0 ET F'(ETA)<0
C             FAUX -> IL FAUT TROUVER ETA SUFFISAMMENT PETIT POUR
C                     AVOIR F(ETA)<0
C    RECHBD : IDEM A DROITE
      IF (ABS(NSOL).GT.0) THEN
        IF ((NSOL.EQ.2).OR.(NSOL.EQ.-1)) RECHBG=.TRUE.
        IF ((NSOL.EQ.2).OR.(NSOL.EQ.1))  RECHBD=.TRUE.

        ETA=ETAMIN
        CALL CRITEV(EPSP,EPSD,ETA,LAMBDA,DEUXMU,FPD,SEUIL,R*D,
     &             CRIT1,CRITP1)

        ITER=0
        RPAS=(ETAMAX-ETAMIN)
        IF (RECHBG) THEN
20        CONTINUE
          ITER=ITER+1
          RPAS=RPAS*2
          IF ((CRIT1.LT.0.D0).OR.(CRITP1.GE.0.D0)) THEN
            ETA=ETA-RPAS
            CALL CRITEV(EPSP,EPSD,ETA,LAMBDA,DEUXMU,FPD,SEUIL,R*D,
     &             CRIT1,CRITP1)
            GOTO 20
          ENDIF
C          write (6,*) 'ITER-1 = ',ITER
        ELSE
30        CONTINUE
          ITER=ITER+1
          RPAS=RPAS*2
          IF (CRIT1.GE.0.D0) THEN
            ETA=ETA-RPAS
            CALL CRITEV(EPSP,EPSD,ETA,LAMBDA,DEUXMU,FPD,SEUIL,R*D,
     &             CRIT1,CRITP1)
            GOTO 30
          ENDIF
C          write (6,*) 'ITER-1b = ',ITER
        ENDIF
        ETA1=ETA

        ETA=ETAMAX
        RPAS=(ETAMAX-ETAMIN)
        CALL CRITEV(EPSP,EPSD,ETA,LAMBDA,DEUXMU,FPD,SEUIL,R*D,
     &             CRIT2,CRITP2)
        ITER=0
        IF (RECHBD) THEN
40        CONTINUE
            ITER=ITER+1
            RPAS=RPAS*2
            IF ((CRIT2.LT.0.D0).OR.(CRITP2.LE.0.D0)) THEN
              ETA=ETA+RPAS
              CALL CRITEV(EPSP,EPSD,ETA,LAMBDA,DEUXMU,FPD,SEUIL,R*D,
     &             CRIT2,CRITP2)
            GOTO 40
          ENDIF
C          write (6,*) 'ITER-2 = ',ITER
        ELSE
50        CONTINUE
          ITER=ITER+1
          RPAS=RPAS*2
          IF (CRIT2.GE.0.D0) THEN
            ETA=ETA+RPAS
            CALL CRITEV(EPSP,EPSD,ETA,LAMBDA,DEUXMU,FPD,SEUIL,R*D,
     &             CRIT2,CRITP2)
            GOTO 50
          ENDIF
C          write (6,*) 'ITER-2b = ',ITER
        ENDIF
        ETA2=ETA
      ENDIF

C -- CAS A UNE SOLUTION
      IF (ABS(NSOL).EQ.1) THEN
        IF (NSOL.EQ.1) THEN
          X(1)=ETA1
          Y(1)=CRIT1
          Z(1)=CRITP1
          X(2)=ETA2
          Y(2)=CRIT2
          Z(2)=CRITP2
        ELSE
          X(1)=ETA2
          Y(1)=CRIT2
          Z(1)=CRITP2

          X(2)=ETA1
          Y(2)=CRIT1
          Z(2)=CRITP1
        ENDIF
        X(3)=X(1)
        Y(3)=Y(1)
        Z(3)=Z(1)
        DO 200 ITER = 1, NITMAX
C          IF (ABS(Y(3)) .LE. EPSTOL*SEUREL*TAU) GOTO 201
          IF (ABS(Y(3)) .LE. EPSTOL*SEUIL) GOTO 201
          IF (MOD(ITER,3) .NE. 0) THEN
            CALL ZEROG2(X,Y,Z,ITER)
          ELSE
            CALL ZEROD2(X,Y,Z)
          END IF
          CALL CRITEV(EPSP,EPSD,X(3),LAMBDA,DEUXMU,FPD,SEUIL,R*D,
     &             Y(3),Z(3))
200     CONTINUE
        GOTO 666
201     CONTINUE
C        write (6,*) 'ITER-3 = ',ITER
        ETA=X(3)
        NSOL=1
      ENDIF

C -- CAS A MINIMUM (ZERO OU DEUX SOLUTIONS)
      IF (NSOL.EQ.2) THEN
        ETAMIN=ETA1
        ETAMAX=ETA2
        ITER=0
C -- ON CHERCHE LE MINIMUM : ON SE DEPLACE SUR LE SEGMENT [ETA1,ETA2]
C    ET ON RACCOURCIT L'INTERVALLE EN UTILISANT LA DERIVEE
250     CONTINUE
C     TEST D'ARRET POUR UN MINIMUM AU-DESSUS DE 0
          ITER=ITER+1
          IF (ITER.GT.NITMAX) THEN
            GOTO 666
          ENDIF
C          IF ((ABS(CRITP1*(ETA2-ETA1)).LT.EPSTOL*SEUREL*TAU).AND.
C     &            (ABS(CRITP2*(ETA2-ETA1)).LT.EPSTOL*SEUREL*TAU)) THEN
          IF ((ABS(CRITP1*(ETA2-ETA1)).LT.EPSTOL*SEUIL).AND.
     &            (ABS(CRITP2*(ETA2-ETA1)).LT.EPSTOL*SEUIL)) THEN
            IF ((CRIT1+CRITP1*(ETA2-ETA1)).GT.0.D0) THEN
              IF ((CRIT2+CRITP2*(ETA1-ETA2)).GT.0.D0) THEN
                GOTO 260
              ENDIF
            ENDIF
          ENDIF
          IF (CRIT1.LT.CRIT2) THEN
            ETAC=C*ETA1+Q*ETA2
          ELSE
            ETAC=C*ETA2+Q*ETA1
          ENDIF
          CALL CRITEV(EPSP,EPSD,ETAC,LAMBDA,DEUXMU,FPD,SEUIL,R*D,
     &               CRITC,CRITP)
C     TEST D'ARRET SI ON PASSE EN DESSOUS DE 0 (-> 2 SOLUTIONS)
          IF (CRITC.LT.0.D0) THEN
            GOTO 260
          ENDIF
          IF (CRITP.GT.0.D0) THEN
            ETA2=ETAC
            CRIT2=CRITC
            CRITP2=CRITP
          ELSE
            ETA1=ETAC
            CRIT1=CRITC
            CRITP1=CRITP
          ENDIF
C     TEST D'ARRET DE PRECISION NUMERIQUE
          IF (ETA2.EQ.ETA1)
     &      CALL U2MESS('F','ALGORITH9_84')
            GOTO 250

C -- SI MINIMUM SOUS 0 : 2 SOLUTIONS, SINON : 0 SOLUTION

260     CONTINUE
C       write (6,*) 'ITER-4 = ',ITER
        IF (CRITC.LT.0.D0) THEN
          NSOL=2
          ETA3=ETAC
          CRIT3=CRITC
          CRITP3=CRITP

          X(1)=ETAC
          Y(1)=CRITC
          Z(1)=CRITP
          X(2)=ETAMAX
          CALL CRITEV(EPSP,EPSD,X(2),LAMBDA,DEUXMU,FPD,SEUIL,R*D,
     &               Y(2),Z(2))
          X(3)=X(2)
          Y(3)=Y(2)
          Z(3)=Z(2)
          DO 400 ITER = 1, NITMAX
C            IF (ABS(Y(3)) .LE. EPSTOL*SEUREL*TAU) GOTO 401
            IF (ABS(Y(3)) .LE. EPSTOL*SEUIL) GOTO 401
            IF (MOD(ITER,3) .NE. 0) THEN
              CALL ZEROG2(X,Y,Z,ITER)
            ELSE
              CALL ZEROD2(X,Y,Z)
            END IF
            CALL CRITEV(EPSP,EPSD,X(3),LAMBDA,DEUXMU,FPD,SEUIL,R*D,
     &             Y(3),Z(3))
400       CONTINUE
          GOTO 666
401       CONTINUE
C          write (6,*) 'ITER-5 = ',ITER
          ETA1=X(3)

          X(1)=ETA3
          Y(1)=CRIT3
          Z(1)=CRITP3
          X(2)=ETAMIN
          CALL CRITEV(EPSP,EPSD,X(2),LAMBDA,DEUXMU,FPD,SEUIL,R*D,
     &               Y(2),Z(2))
          X(3)=X(2)
          Y(3)=Y(2)
          Z(3)=Z(2)
          DO 500 ITER = 1, NITMAX
C            IF (ABS(Y(3)) .LE. EPSTOL*SEUREL*TAU) GOTO 501
            IF (ABS(Y(3)) .LE. EPSTOL*SEUIL) GOTO 501
            IF (MOD(ITER,3) .NE. 0) THEN
              CALL ZEROG2(X,Y,Z,ITER)
            ELSE
              CALL ZEROD2(X,Y,Z)
            END IF
            CALL CRITEV(EPSP,EPSD,X(3),LAMBDA,DEUXMU,FPD,SEUIL,R*D,
     &             Y(3),Z(3))
500       CONTINUE
          GOTO 666
501       CONTINUE
C          write (6,*) 'ITER-5b = ',ITER
          ETA2=X(3)
        ELSE
          ETA=ETAC
          NSOL=0
        ENDIF
      ENDIF

      IF (NSOL.EQ.0) THEN
        ETAS=R8VIDE()
C        CALL CRITEV(EPSP,EPSD,ETA,LAMBDA,DEUXMU,FPD,SEUILA,
C     &             CRIT1,CRITP)
C        A0=CRIT1/SEUREL
      ELSE
        SEUIL=SEUILA
        ETAS=R8VIDE()
        IF (NSOL.EQ.2) THEN
          ETA=ETA1
        ENDIF
        CALL CRITEV(EPSP,EPSD,ETA,LAMBDA,DEUXMU,FPD,SEUILA,R*D,
     &             CRIT1,CRITP)
C        WRITE(6,*)
C        WRITE(6,*) 'critere1=',CRIT1
C        WRITE(6,*) 'ETA1=',ETA
C        WRITE(6,*)
C ======================================================================
C                        LINEARISATION DU CRITERE
C ======================================================================

C        SEUREL = SEUIL * (1+GAMMA*D)**2 / (1+GAMMA)
C        A0 = (CRIT1 - ETA*CRITP ) /SEUREL
C        A1 =CRITP/ SEUREL

        A1 =CRITP
        A0 = TAU-ETA*A1

C      WRITE(6,*) 'ETA1111111111=',ETA
C      WRITE(6,*) 'CRITERE=',CRIT1
C      WRITE(6,*) 'D_CRITERE=',CRITP


        IF ((NSOL.EQ.2).AND.(ETA2.GT.0.D0)) THEN
          ETA=ETA2

          CALL CRITEV(EPSP,EPSD,ETA,LAMBDA,DEUXMU,FPD,SEUILA,R*D,
     &             CRIT1,CRITP)

C        WRITE(6,*)
C        WRITE(6,*) 'critere2=',CRIT1
C        WRITE(6,*) 'ETA2=',ETA
C        WRITE(6,*)


C ======================================================================
C                        LINEARISATION DU CRITERE
C ======================================================================

C          A2 = (CRIT1 - ETA*CRITP ) /SEUREL
C          A3 =CRITP/ SEUREL

          A3 =CRITP
          A2 = TAU-ETA*A3

C      WRITE(6,*) 'ETA222222222222=',ETA
C      WRITE(6,*) 'CRITERE=',CRIT1
C      WRITE(6,*) 'D_CRITERE=',CRITP

        ELSE
          A2=R8VIDE()
          A3=R8VIDE()
        ENDIF
      ENDIF
      GOTO 9999

 666  CONTINUE
      A0 = 0.D0
      A1 = 0.D0
      A2 = R8VIDE()
      A3 = R8VIDE()
      ETAS = R8VIDE()

 9999 CONTINUE

C      WRITE(6,*) 'EPSM(1)=',EPSM(1)
C      WRITE(6,*) 'EPSM(2)=',EPSM(2)
C      WRITE(6,*) 'EPSM(3)=',EPSM(3)
C      WRITE(6,*) 'EPSM(4)=',EPSM(4)

C      WRITE(6,*) 'EPSP(1)=',EPSP(1)
C      WRITE(6,*) 'EPSP(2)=',EPSP(2)
C      WRITE(6,*) 'EPSP(3)=',EPSP(3)
C      WRITE(6,*) 'EPSP(4)=',EPSP(4)

C      WRITE(6,*) 'EPSP(7)=',EPSP(7)

C      WRITE(6,*) 'EPSD(1)=',EPSD(1)
C      WRITE(6,*) 'EPSD(2)=',EPSD(2)
C      WRITE(6,*) 'EPSD(3)=',EPSD(3)
C      WRITE(6,*) 'EPSD(4)=',EPSD(4)

C      WRITE(6,*) 'EPSD(7)=',EPSD(7)
   
C      WRITE(6,*) 'A0=',A0
C      WRITE(6,*) 'A1=',A1
C      WRITE(6,*) 'A2=',A2
C      WRITE(6,*) 'A3=',A3
C      WRITE(6,*) 'NSOL=',NSOL


      END

      SUBROUTINE PIPEDS(NDIM, TYPMOD, TAU,IMATE, SIGM, VIM,
     &                  EPSPC, EPSDC, ETAMIN,ETAMAX,A0, A1,A2,A3,ETAS)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/12/2001   AUTEUR PBBHHPB P.BADEL 
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
      CHARACTER*8        TYPMOD(2)
      INTEGER            NDIM, IMATE
      REAL*8             EPSPC(6), EPSDC(6),ETAMIN,ETAMAX,TAU
      REAL*8             VIM(2), SIGM(6), A0, A1,A2,A3,ETAS
C ----------------------------------------------------------------------
C     LOI DE COMPORTEMENT ELASTIQUE FRAGILE (EN DELOCALISE)
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
      INTEGER     NPERM, NITJAC, TTRIJ, OTRIJ,NCAS
      REAL*8      TREPSD, COPLAN, SIGELP(6), SIGELD(6)
      REAL*8      EPS1(6), EPS2(6), D1, D2,CRIT,RTEMP
      REAL*8      TR(6),TU(6),EDP(6),VECP(3,3),RAC2,CRITP
      REAL*8      FPD, D, P0, P1, P2, ETA, RAC(2), IND(4),EPM(3)
      REAL*8      E, NU, LAMBDA, DEUXMU, GAMMA, SEUIL, SEUREL
      REAL*8      R8NRM2,EPSP(6), EPSD(6),X(4),Y(4),Z(4)
      REAL*8      TOL,TOLDYN,JACAUX(3),RMINI,EPSTOL
      REAL*8      TREPS,SIGEL(3),ETA1,ETA2,ETAC,CRIT1,CRIT2,CRITC,CRITP3
      REAL*8      RTEMP1,RTEMP2,R8VIDE,SEUILA,CRITP1,CRITP2,CRIT3,RPAS
      REAL*8      CRITR1,CRITR2,LAMBDD,DEUMUD,FD,RIGMIN,ETA0,ETA3,C,R

      CHARACTER*2 CODRET(2)
      CHARACTER*8 NOMRES(2)
      REAL*8      VALRES(2)

      REAL*8      R8DOT

      DATA   NPERM ,TOL,TOLDYN    /12,1.D-12,1.D-2/
      DATA   TTRIJ,OTRIJ  /2,2/
      
      NITMAX=50
      EPSTOL=1.D-1
      R=0.61803399D0
      C=1.D0-R
      NSOL=0

C -- OPTION ET MODELISATION

      CPLAN = (TYPMOD(1).EQ.'C_PLAN  ')
      NDIMSI = 2*NDIM
      RAC2=SQRT(2.D0)

C -- CAS DE L'ENDOMMAGEMENT SATURE

      IF (NINT(VIM(2)) .EQ. 2) THEN
        A0 = 0.D0
        A1 = 0.D0
        A2 = R8VIDE()
        A3 = R8VIDE()
        ETAS = R8VIDE()
        GOTO 9999
      END IF

C -- LECTURE DES CARACTERISTIQUES THERMOELASTIQUES

      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      CALL RCVALA ( IMATE,'ELAS',0,' ',0.D0,2,
     &              NOMRES,VALRES,CODRET, 'FM')
      E      = VALRES(1)
      NU     = VALRES(2)
      LAMBDA = E * NU / (1.D0+NU) / (1.D0 - 2.D0*NU)
      DEUXMU = E/(1.D0+NU)

C -- LECTURE DES CARACTERISTIQUES D'ENDOMMAGEMENT

      NOMRES(1) = 'SY'
      NOMRES(2) = 'D_SIGM_EPSI'
      CALL RCVALA(IMATE,'ECRO_LINE',0,' ',0.D0,2,
     &            NOMRES,VALRES,CODRET,'FM')
      GAMMA  = - E/VALRES(2)
      SEUIL  = VALRES(1)**2 * (1.D0+GAMMA) / (2.D0*E)
      SEUILA = SEUIL

C ======================================================================
C                CALCUL DES DEFORMATIONS POUR LINEARISATION
C ======================================================================

C    ETAT MECANIQUE EN T-
      D   = VIM(1)
      FPD = (1+GAMMA) / (1+GAMMA*D)**2      
      SEUREL = SEUIL * (1+GAMMA*D)**2 / (1+GAMMA)      
      SEUIL=SEUIL+SEUREL*TAU

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
C --
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
      TU(1) = 1.D0
      TU(2) = 0.D0
      TU(3) = 0.D0
      TU(4) = 1.D0
      TU(5) = 0.D0
      TU(6) = 1.D0

C -- DIAGONALISATION AVEC TRI EN VAL RELATIVE CROISSANT
      CALL JACOBI(3,NPERM,TOL,TOLDYN,TR,TU,VECP,EPM,JACAUX,
     &       NITJAC,0,0)
     
      NSOL=2
      IF (EPM(1).GT.0.D0) NSOL=1
      IF (EPM(3).LT.0.D0) NSOL=-1
      IF ((EPM(1).EQ.0.D0).OR.(EPM(3).EQ.0.D0)) THEN
        DO 47 K=1,6
          EPS1(K)=EPSP(K)
47      CONTINUE
      ENDIF
      IF (EPM(1).EQ.0.D0) THEN
        CALL BGTOBP(EPS1,EPS2,VECP)
        IF (EPM(2).EQ.0.D0) THEN
          IF (EPM(3).EQ.0.D0) THEN
            CALL UTMESS('F','PIPEDS.F','EPSD NUL : PILOTAGE IMPOSSIBLE')
          ELSE
            EPS2(5)=0.D0
            EPS2(6)=0.D0
            EPS2(3)=0.D0
            CALL CRITET(EPS2,EPSD,0.D0,0.D0,DEUXMU,FPD,SEUIL,NPERM,TOL
     &             ,TOLDYN,JACAUX,NITJAC,TTRIJ,OTRIJ,CRIT1,CRITP1)
          ENDIF
        ELSE
          EPS2(2)=0.D0
          EPS2(3)=0.D0
          EPS2(4)=0.D0
          EPS2(5)=0.D0
          EPS2(6)=0.D0
          CALL CRITET(EPS2,EPSD,0.D0,0.D0,DEUXMU,FPD,SEUIL,NPERM,TOL
     &             ,TOLDYN,JACAUX,NITJAC,TTRIJ,OTRIJ,CRIT1,CRITP1)     
        ENDIF
        IF (CRIT1.GT.0.D0) THEN
          NSOL=0
          ETA=ETAMIN
        ELSE
          NSOL=1
        ENDIF
      ENDIF            

      IF (EPM(3).EQ.0.D0) THEN
        CALL BGTOBP(EPS1,EPS2,VECP)
        IF (EPM(2).EQ.0.D0) THEN
          IF (EPM(1).EQ.0.D0) THEN
            CALL UTMESS('F','PIPEDS.F','EPSD NUL : PILOTAGE IMPOSSIBLE')
          ELSE
            EPS2(5)=0.D0
            EPS2(4)=0.D0
            EPS2(1)=0.D0
            CALL CRITET(EPS2,EPSD,0.D0,0.D0,DEUXMU,FPD,SEUIL,NPERM,TOL
     &             ,TOLDYN,JACAUX,NITJAC,TTRIJ,OTRIJ,CRIT1,CRITP1)
          ENDIF
        ELSE
          EPS2(2)=0.D0
          EPS2(1)=0.D0
          EPS2(4)=0.D0
          EPS2(5)=0.D0
          EPS2(6)=0.D0
          CALL CRITET(EPS2,EPSD,0.D0,0.D0,DEUXMU,FPD,SEUIL,NPERM,TOL
     &             ,TOLDYN,JACAUX,NITJAC,TTRIJ,OTRIJ,CRIT1,CRITP1)     
        ENDIF
        IF (CRIT1.GT.0.D0) THEN
          NSOL=0
          ETA=ETAMAX
        ELSE
          NSOL=-1
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
        CALL CRITET(EPSP,EPSD,ETA,LAMBDA,DEUXMU,FPD,SEUIL,NPERM,TOL,
     &             TOLDYN,JACAUX,NITJAC,TTRIJ,OTRIJ,CRIT1,CRITP1)

        ITER=0
        RPAS=(ETAMAX-ETAMIN)
        IF (RECHBG) THEN
20        CONTINUE
          ITER=ITER+1
          RPAS=RPAS*2
          IF ((CRIT1.LT.0.D0).OR.(CRITP1.GE.0.D0)) THEN
            ETA=ETA-RPAS
            CALL CRITET(EPSP,EPSD,ETA,LAMBDA,DEUXMU,FPD,SEUIL,NPERM,
     &             TOL,TOLDYN,JACAUX,NITJAC,TTRIJ,OTRIJ,CRIT1,CRITP1)
            GOTO 20
          ENDIF
C          write (6,*) 'ITER-1 = ',ITER
        ELSE
30        CONTINUE
          ITER=ITER+1
          RPAS=RPAS*2
          IF (CRIT1.GE.0.D0) THEN
            ETA=ETA-RPAS
            CALL CRITET(EPSP,EPSD,ETA,LAMBDA,DEUXMU,FPD,SEUIL,NPERM,
     &             TOL,TOLDYN,JACAUX,NITJAC,TTRIJ,OTRIJ,CRIT1,CRITP1)
            GOTO 30
          ENDIF
C          write (6,*) 'ITER-1b = ',ITER
        ENDIF
        ETA1=ETA
      
        ETA=ETAMAX
        RPAS=(ETAMAX-ETAMIN)
        CALL CRITET(EPSP,EPSD,ETA,LAMBDA,DEUXMU,FPD,SEUIL,NPERM,TOL,
     &             TOLDYN,JACAUX,NITJAC,TTRIJ,OTRIJ,CRIT2,CRITP2)
        ITER=0
        IF (RECHBD) THEN
40        CONTINUE
            ITER=ITER+1
            RPAS=RPAS*2
            IF ((CRIT2.LT.0.D0).OR.(CRITP2.LE.0.D0)) THEN
              ETA=ETA+RPAS
              CALL CRITET(EPSP,EPSD,ETA,LAMBDA,DEUXMU,FPD,SEUIL,NPERM,
     &             TOL,TOLDYN,JACAUX,NITJAC,TTRIJ,OTRIJ,CRIT2,CRITP2)
            GOTO 40
          ENDIF
C          write (6,*) 'ITER-2 = ',ITER
        ELSE
50        CONTINUE
          ITER=ITER+1
          RPAS=RPAS*2
          IF (CRIT2.GE.0.D0) THEN
            ETA=ETA+RPAS
            CALL CRITET(EPSP,EPSD,ETA,LAMBDA,DEUXMU,FPD,SEUIL,NPERM,
     &             TOL,TOLDYN,JACAUX,NITJAC,TTRIJ,OTRIJ,CRIT2,CRITP2)
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
          IF (ABS(Y(3)) .LE. EPSTOL*SEUREL*TAU) GOTO 201
          IF (MOD(ITER,5) .NE. 0) THEN
            CALL ZEROG2(X,Y,Z,ITER)
          ELSE
            CALL ZEROD2(X,Y,Z)
          END IF
          CALL CRITET(EPSP,EPSD,X(3),LAMBDA,DEUXMU,FPD,SEUIL,NPERM,
     &             TOL,TOLDYN,JACAUX,NITJAC,TTRIJ,OTRIJ,Y(3),Z(3))
200     CONTINUE
        CALL UTMESS('F','PIPEDS-1 ','NOMBRE MAX D''ITERATIONS ATTEINT')
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
C            write (6,*) 'ETAMIN = ',ETAMIN,' ; ETAMAX = ',ETAMAX
C            write (6,*) 'ETA1 = ',ETA1,' ; CRIT1 = ',CRIT1,
C     &                     ' ; CRITP1',CRITP1
C            write (6,*) 'ETA2 = ',ETA2,' ; CRIT2 = ',CRIT2,
C     &                     ' ; CRITP2',CRITP2
            CALL UTMESS('F','PIPEDS-0 ','NOMBRE MAX ITERATIONS ATTEINT')
          ENDIF
          IF ((ABS(CRITP1*(ETA2-ETA1)).LT.EPSTOL*SEUREL*TAU).AND.
     &            (ABS(CRITP2*(ETA2-ETA1)).LT.EPSTOL*SEUREL*TAU)) THEN
            IF ((CRIT1+CRITP1*(ETA2-ETA1)).GT.0.D0) THEN
              IF ((CRIT2+CRITP2*(ETA1-ETA2)).GT.0.D0) THEN
                GOTO 260
              ENDIF
            ENDIF
          ENDIF
          IF (CRIT1.LT.CRIT2) THEN
            ETAC=C*ETA1+R*ETA2 
          ELSE
            ETAC=C*ETA2+R*ETA1 
          ENDIF            
          CALL CRITET(EPSP,EPSD,ETAC,LAMBDA,DEUXMU,FPD,SEUIL,NPERM,
     &               TOL,TOLDYN,JACAUX,NITJAC,TTRIJ,OTRIJ,CRITC,CRITP)
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
     &      CALL UTMESS('F','PIPEDS','PRECISION MACHINE DEPASSEE')
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
          CALL CRITET(EPSP,EPSD,X(2),LAMBDA,DEUXMU,FPD,SEUIL,NPERM,
     &               TOL,TOLDYN,JACAUX,NITJAC,TTRIJ,OTRIJ,Y(2),Z(2))
          X(3)=X(2)
          Y(3)=Y(2)
          Z(3)=Z(2)
          DO 400 ITER = 1, NITMAX
            IF (ABS(Y(3)) .LE. EPSTOL*SEUREL*TAU) GOTO 401
            IF (MOD(ITER,5) .NE. 0) THEN
              CALL ZEROG2(X,Y,Z,ITER)
            ELSE
              CALL ZEROD2(X,Y,Z)
            END IF
            CALL CRITET(EPSP,EPSD,X(3),LAMBDA,DEUXMU,FPD,SEUIL,NPERM,
     &             TOL,TOLDYN,JACAUX,NITJAC,TTRIJ,OTRIJ,Y(3),Z(3))
400       CONTINUE
          CALL UTMESS('F','PIPEDS-2 ','NOMBRE MAX ITERATIONS ATTEINT')
401       CONTINUE
C          write (6,*) 'ITER-5 = ',ITER
          ETA1=X(3)

          X(1)=ETA3
          Y(1)=CRIT3
          Z(1)=CRITP3
          X(2)=ETAMIN
          CALL CRITET(EPSP,EPSD,X(2),LAMBDA,DEUXMU,FPD,SEUIL,NPERM,
     &               TOL,TOLDYN,JACAUX,NITJAC,TTRIJ,OTRIJ,Y(2),Z(2))
          X(3)=X(2)
          Y(3)=Y(2)
          Z(3)=Z(2)
          DO 500 ITER = 1, NITMAX
            IF (ABS(Y(3)) .LE. EPSTOL*SEUREL*TAU) GOTO 501
            IF (MOD(ITER,5) .NE. 0) THEN
              CALL ZEROG2(X,Y,Z,ITER)
            ELSE
              CALL ZEROD2(X,Y,Z)
            END IF
            CALL CRITET(EPSP,EPSD,X(3),LAMBDA,DEUXMU,FPD,SEUIL,NPERM,
     &             TOL,TOLDYN,JACAUX,NITJAC,TTRIJ,OTRIJ,Y(3),Z(3))
500       CONTINUE
          CALL UTMESS('F','PIPEDS-3 ','NOMBRE MAX ITERATIONS ATTEINT')
501       CONTINUE
C          write (6,*) 'ITER-5b = ',ITER
          ETA2=X(3)
        ELSE
          ETA=ETAC
          NSOL=0
        ENDIF
      ENDIF
         
     
      IF (NSOL.EQ.0) THEN
        ETAS=ETA
        CALL CRITET(EPSP,EPSD,ETA,LAMBDA,DEUXMU,FPD,SEUILA,NPERM,TOL,
     &             TOLDYN,JACAUX,NITJAC,TTRIJ,OTRIJ,CRIT1,CRITP)
        A0=CRIT1/SEUREL
      ELSE
        SEUIL=SEUILA
        ETAS=R8VIDE()
        IF (NSOL.EQ.2) THEN    
          ETA=ETA1
        ENDIF       
        CALL CRITET(EPSP,EPSD,ETA,LAMBDA,DEUXMU,FPD,SEUILA,NPERM,TOL,
     &             TOLDYN,JACAUX,NITJAC,TTRIJ,OTRIJ,CRIT1,CRITP)


C ======================================================================
C                        LINEARISATION DU CRITERE
C ======================================================================

        SEUREL = SEUIL * (1+GAMMA*D)**2 / (1+GAMMA)
        A0 = (CRIT1 - ETA*CRITP ) /SEUREL
        A1 =CRITP/ SEUREL
        
        IF (NSOL.EQ.2) THEN
          ETA=ETA2
       
          CALL CRITET(EPSP,EPSD,ETA,LAMBDA,DEUXMU,FPD,SEUILA,NPERM,TOL,
     &             TOLDYN,JACAUX,NITJAC,TTRIJ,OTRIJ,CRIT1,CRITP)


C ======================================================================
C                        LINEARISATION DU CRITERE
C ======================================================================

          A2 = (CRIT1 - ETA*CRITP ) /SEUREL
          A3 =CRITP/ SEUREL
        ELSE
          A2=R8VIDE()
          A3=R8VIDE()
        ENDIF
      ENDIF
 9999 CONTINUE
      END

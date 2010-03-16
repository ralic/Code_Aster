      SUBROUTINE PIPEDO(NDIM  ,TYPMOD,TAU   ,MATE  ,VIM   ,
     &                  EPSM  ,EPSPC ,EPSDC ,ETAMIN,ETAMAX,
     &                  A0    ,A1    ,A2    ,A3    ,ETAS  )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/03/2010   AUTEUR ABBAS M.ABBAS 
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
C TOLE CRP_20
C
      IMPLICIT NONE
      CHARACTER*8        TYPMOD(*)
      INTEGER            NDIM, MATE
      REAL*8             VIM(7),EPSM(6),EPSPC(6),EPSDC(6)
      REAL*8             ETAMIN,ETAMAX,TAU
      REAL*8             A0,A1,A2,A3,ETAS
C       
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (PILOTAGE - PRED_ELAS)
C
C LOI DE COMPORTEMENT ENDO_ORTH_BETON
C
C ----------------------------------------------------------------------
C    
C
C IN  NDIM   : DIMENSION DE L'ESPACE
C IN  TYPMOD : TYPE DE MODELISATION
C IN  TAU    : 2ND MEMBRE DE L'EQUATION F(ETA)=TAU
C IN  MATE   : MATERIAU CODE
C IN  VIM    : VARIABLES INTERNES EN T-
C IN  EPSM   : DEFORMATIONS EN T-
C IN  EPSPC  : CORRECTION DE DEFORMATIONS DUES AUX CHARGES FIXES
C IN  EPSDC  : CORRECTION DE DEFORMATIONS DUES AUX CHARGES PILOTEES
C IN  ETAMIN : DONNEE UTILISATEUR DU MINIMUM DE ETA
C IN  ETAMAX : DONNEE UTILISATEUR DU MAXIMUM DE ETA
C OUT A0     : LINEARISATION DU CRITERE : FEL = A0 + A1*ETA
C OUT A1     : CF A0
C OUT A2     : IDEM A0 POUR LA SECONDE SOLUTION EVENTUELLE;R8VIDE SINON
C OUT A3     : IDEM A1 POUR LA SECONDE SOLUTION EVENTUELLE;R8VIDE SINON
C OUT ETAS   : SI PAS DE SOLUTION : LE MINIMUM ; R8VIDE SINON
C
C ----------------------------------------------------------------------
C
      INTEGER     NBRES
      PARAMETER   (NBRES=6)
      CHARACTER*2 CODRET(NBRES)
      CHARACTER*8 NOMRES(NBRES)
      REAL*8      VALRES(NBRES)
C

      LOGICAL     CPLAN,RECHBG,RECHBD
      INTEGER     NDIMSI, K,NSOL,ITER,NITMAX
      INTEGER     I,J,L,T(3,3)
      REAL*8      COPLAN,UN
      REAL*8      RAC2,CRITP
      REAL*8      ETA
      REAL*8      E, NU, LAMBDA, MU, SEUIL,TREPSM
      REAL*8      K0,K1,K2,ALPHA
      REAL*8      EPSP(6), EPSD(6),X(4),Y(4),Z(4)
      REAL*8      EPSTOL
      REAL*8      TREPS,ETA1,ETA2,ETAC,CRIT1,CRIT2,CRITC,CRITP3
      REAL*8      R8VIDE,SEUILA,CRITP1,CRITP2,CRIT3,RPAS
      REAL*8      ETA3,C,R
      REAL*8      B(6),D,REC(6),BR(6),VECB(3,3),VALB(3),TOLE
      REAL*8      EPSDP(6),EPSDM(6),CCP(6),CCM(6),VECCP(3,3),VECCM(3,3)
      REAL*8      VALCCP(3),VALCCM(3),CCPP(6),CCPM(6),CPEP(6),CPEM(6)
      REAL*8      FBP(6),FBM(6),TREBP,TREBM,VECFBP(3,3),VALFBP(3)
      REAL*8      VECFBM(3,3),VALFBM(3),RTEMPP,RTEMPM,VECC(3,3),VALCC(3)
      REAL*8      DCOEFD,ENE,FDP,FDM,TREM
      REAL*8      STRA,TRB
      REAL*8      ECROB,ECROD
C
C ----------------------------------------------------------------------
C
      UN     = 1.D0
      NITMAX = 50
      EPSTOL = 1.D-1
      R      = 0.61803399D0
      C      = 1.D0-R
      NSOL   = 0

C TOLE: TOLERANCE POUR ARRET EVOLUTION DE L ENDOMMAGEMENT
      TOLE=1.D-2

      T(1,1)=1
      T(1,2)=4
      T(1,3)=5
      T(2,1)=4
      T(2,2)=2
      T(2,3)=6
      T(3,1)=5
      T(3,2)=6
      T(3,3)=3

C -- OPTION ET MODELISATION
      CPLAN  = (TYPMOD(1).EQ.'C_PLAN  ')
      NDIMSI = 2*NDIM
      RAC2   = SQRT(2.D0)


C -- LECTURE DES CARACTERISTIQUES THERMOELASTIQUES
      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      CALL RCVALA(MATE  ,' '   ,'ELAS',0     ,' '   ,
     &            0.D0  ,2     ,NOMRES,VALRES,CODRET,
     &           'FM')
      E      = VALRES(1)
      NU     = VALRES(2)
      LAMBDA = E * NU / (1.D0+NU) / (1.D0 - 2.D0*NU)
      MU     = E/(2.D0*(1.D0+NU))

C -- LECTURE DES CARACTERISTIQUES D'ENDOMMAGEMENT
      NOMRES(1) = 'ALPHA'
      NOMRES(2) = 'K0'
      NOMRES(3) = 'K1'
      NOMRES(4) = 'K2'
      NOMRES(5) = 'ECROB'
      NOMRES(6) = 'ECROD'
      CALL RCVALA(MATE  ,' '   ,'ENDO_ORTH_BETON',0     ,' '   ,
     &            0.D0  ,NBRES ,NOMRES           ,VALRES,CODRET,
     &            'FM')
      ALPHA  = VALRES(1)
      K0     = VALRES(2)
      K1     = VALRES(3)
      K2     = VALRES(4)
      ECROB  = VALRES(5)
      ECROD  = VALRES(6)


      TREPSM=EPSM(1)+EPSM(2)+EPSM(3)
      IF (TREPSM.GT.0.D0) THEN
       TREPSM=0.D0
      ENDIF

      STRA   = TREPSM
      SEUIL  = K0-K1*STRA*(ATAN2(-STRA/K2,UN))
      SEUILA = SEUIL





C ======================================================================
C                CALCUL DES DEFORMATIONS POUR LINEARISATION
C ======================================================================



C    ETAT MECANIQUE EN T-
      DO 3 I = 1,3
        B(I)   = 1.D0-VIM(I)
 3    CONTINUE
      DO 300 I = 4,6
        B(I)   = -VIM(I)
 300  CONTINUE
      D      = VIM(7)

C      SEUIL=SEUIL+K0*TAU
      SEUIL  = SEUIL+SEUILA*TAU
      TRB    = B(1)+B(2)+B(3)

C -- CAS DE L'ENDOMMAGEMENT SATURE
      IF ((TRB.LE.TOLE).OR.(D.GE.(1.D0-TOLE))) THEN
        A0 = 0.D0
        A1 = 0.D0
        A2 = R8VIDE()
        A3 = R8VIDE()
        ETAS = R8VIDE()
        GOTO 9999
      END IF

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


C-- CALCUL DES FORCES THERMO CALCULEES AVEC +EPSD ET -EPSD
C-- ON SUPPOSE EPS=ETA*EPSD POUR ETA>INFINI
C--IL FAUT TRAVAILLER DANS L ESPACE PROPRE DE B


      CALL R8INIR(6,1.D0,REC,1)
      CALL R8INIR(6,0.D0,BR,1)
      CALL R8INIR(6,0.D0,EPSDP,1)
      CALL R8INIR(6,0.D0,EPSDM,1)

      CALL DIAGO3(B,VECB,VALB)
      DO 701 I=1,3
        BR(I)=VALB(I)
 701  CONTINUE

      IF (ABS(VALB(1)).LT.TOLE) THEN
        REC(1)=0.D0
        REC(4)=0.D0
        REC(5)=0.D0
      ENDIF
      IF (ABS(VALB(2)).LT.TOLE) THEN
        REC(2)=0.D0
        REC(4)=0.D0
        REC(6)=0.D0
      ENDIF
      IF (ABS(VALB(3)).LT.TOLE) THEN
        REC(3)=0.D0
        REC(5)=0.D0
        REC(6)=0.D0
      ENDIF

        DO 202 I=1,3
          DO 203 J=I,3
            DO 204 K=1,3
              DO 205 L=1,3
            EPSDP(T(I,J))=EPSDP(T(I,J))+VECB(K,I)*EPSD(T(K,L))*VECB(L,J)
            EPSDM(T(I,J))=EPSDM(T(I,J))-VECB(K,I)*EPSD(T(K,L))*VECB(L,J)
 205        CONTINUE
 204        CONTINUE
 203      CONTINUE
 202    CONTINUE

      CALL R8INIR(6,0.D0,CCP,1)
      CALL R8INIR(6,0.D0,CCM,1)

      DO 9 I=1,3
        DO 10 J=I,3
          DO 11 K=1,3
            CCP(T(I,J))=CCP(T(I,J))+BR(T(I,K))*EPSDP(T(K,J))+
     &                 BR(T(J,K))*EPSDP(T(K,I))
            CCM(T(I,J))=CCM(T(I,J))+BR(T(I,K))*EPSDM(T(K,J))+
     &                 BR(T(J,K))*EPSDM(T(K,I))
 11       CONTINUE
 10     CONTINUE
 9    CONTINUE
      CALL DIAGO3(CCP,VECCP,VALCCP)
      CALL DIAGO3(CCM,VECCM,VALCCM)


      CALL R8INIR(6,0.D0,CCPP,1)
      CALL R8INIR(6,0.D0,CCPM,1)
      CALL R8INIR(6,0.D0,CPEP,1)
      CALL R8INIR(6,0.D0,CPEM,1)


      DO 12 I=1,3
        IF (VALCCP(I).LT.0.D0) THEN
          VALCCP(I)=0.D0
        ENDIF
        IF (VALCCM(I).LT.0.D0) THEN
          VALCCM(I)=0.D0
        ENDIF
 12   CONTINUE

      DO 13 I=1,3
        DO 14 J=I,3
          DO 15 K=1,3
          CCPP(T(I,J))=CCPP(T(I,J))+VECCP(I,K)*VALCCP(K)*VECCP(J,K)
          CCPM(T(I,J))=CCPM(T(I,J))+VECCM(I,K)*VALCCM(K)*VECCM(J,K)
 15       CONTINUE
 14     CONTINUE
 13   CONTINUE

      DO 16 I=1,3
        DO 17 J=I,3
          DO 18 K=1,3
            CPEP(T(I,J))=CPEP(T(I,J))+ CCPP(T(I,K))*EPSDP(T(K,J))+
     &                    CCPP(T(J,K))*EPSDP(T(K,I))
            CPEM(T(I,J))=CPEM(T(I,J))+ CCPM(T(I,K))*EPSDM(T(K,J))+
     &                    CCPM(T(J,K))*EPSDM(T(K,I))
  18      CONTINUE
  17    CONTINUE
  16  CONTINUE

      CALL R8INIR(6,0.D0,FBP,1)
      CALL R8INIR(6,0.D0,FBM,1)

      TREBP=0.D0
      TREBM=0.D0

      DO 301 I=1,3
      TREBP=TREBP+CCP(I)/2
      TREBM=TREBM+CCM(I)/2
 301  CONTINUE

      IF (TREBP.GT.0.D0) THEN
        DO 19 I=1,6
          FBP(I)=-LAMBDA*TREBP*EPSDP(I)
  19    CONTINUE
      ENDIF
      IF (TREBM.GT.0.D0) THEN
        DO 21 I=1,6
          FBM(I)=-LAMBDA*TREBM*EPSDM(I)
  21    CONTINUE
      ENDIF

      DO 20 I=1,6
        FBP(I)=(FBP(I)-MU/2.D0*CPEP(I))
        FBM(I)=(FBM(I)-MU/2.D0*CPEM(I))
  20  CONTINUE

      CALL DIAGO3(FBP,VECFBP,VALFBP)
      CALL DIAGO3(FBM,VECFBM,VALFBM)

      RTEMPP=0.D0
      RTEMPM=0.D0

      DO 29 I=1,3
        IF (VALFBP(I).GT.0.D0) THEN
          VALFBP(I)=0.D0
        ENDIF
        RTEMPP=RTEMPP+VALFBP(I)*VALFBP(I)
        IF (VALFBM(I).GT.0.D0) THEN
          VALFBM(I)=0.D0
        ENDIF
        RTEMPM=RTEMPM+VALFBM(I)*VALFBM(I)
  29  CONTINUE


      TREPS=EPSDP(1)+EPSDP(2)+EPSDP(3)
      CALL DIAGO3(EPSDP,VECC,VALCC)
      DO 22 I=1,3
        IF (VALCC(I).GT.0.D0) THEN
          VALCC(I)=0.D0
        ENDIF
 22   CONTINUE
      TREM=VALCC(1)**2+VALCC(2)**2+VALCC(3)**2
      IF (TREPS.GT.0.D0) THEN
        TREPS=0.D0
      ENDIF
      DCOEFD=2.D0*(1.D0-D)
      ENE=LAMBDA/2*TREPS**2+MU*TREM
C      FDP=DCOEFD*ENE-2.D0*ECROD*D
      FDP=DCOEFD*ENE
      IF (FDP.LT.0.D0) THEN
        FDP=0.D0
      ENDIF

      TREPS=EPSDM(1)+EPSDM(2)+EPSDM(3)
      CALL DIAGO3(EPSDM,VECC,VALCC)
      DO 32 I=1,3
        IF (VALCC(I).GT.0.D0) THEN
          VALCC(I)=0.D0
        ENDIF
 32   CONTINUE
      TREM=VALCC(1)**2+VALCC(2)**2+VALCC(3)**2
      IF (TREPS.GT.0.D0) THEN
        TREPS=0.D0
      ENDIF
      ENE=LAMBDA/2*TREPS**2+MU*TREM
C      FDM=DCOEFD*ENE-2.D0*ECROD*D
      FDM=DCOEFD*ENE
      IF (FDM.LT.0.D0) THEN
        FDM=0.D0
      ENDIF

C----------------------------------------------------------
C---COMPORTEMENT A L INFINI ET NOMBRE DE SOLUTIONS---------
C
C   DE MANIERE GENERALE: NSOL=2 (ou NSOL=0)
C                        RECHBG=TRUE    RECHBD=TRUE
C
C   EXCEPTIONS:  RTEMPM=0 ET FDM=0   NSOL=1
C                        RECHBG=FALSE   RECHBD=TRUE
C                RTEMPP=0 ET FDP=0   NSOL=-1
C                        RECHBG=TRUE    RECHBD=FALSE
C
C  on ne considere pas le cas proche de zero pour l instant
C
C-----------------------------------------------------------



C -- RECHBG : VRAI -> IL FAUT TROUVER ETA SUFFISAMMENT PETIT POUR
C                     AVOIR F(ETA)>0 ET F'(ETA)<0
C             FAUX -> IL FAUT TROUVER ETA SUFFISAMMENT PETIT POUR
C                     AVOIR F(ETA)<0
C    RECHBD : IDEM A DROITE

      NSOL=2
      IF ((RTEMPM.EQ.0.D0).AND.(FDM.EQ.0.D0)) NSOL=1
      IF ((RTEMPP.EQ.0.D0).AND.(FDP.EQ.0.D0)) NSOL=-1


      IF (ABS(NSOL).GT.0) THEN
        IF ((NSOL.EQ.2).OR.(NSOL.EQ.-1)) RECHBG=.TRUE.
        IF ((NSOL.EQ.2).OR.(NSOL.EQ.1))  RECHBD=.TRUE.

        ETA=ETAMIN

        CALL CRITEO(EPSP  ,EPSD  ,ETA   ,B     ,D     ,
     &              LAMBDA,MU    ,ALPHA ,ECROB ,ECROD ,
     &              SEUIL ,CRIT1 ,CRITP1)

        ITER=0
        RPAS=(ETAMAX-ETAMIN)

        IF (RECHBG) THEN
60        CONTINUE
          ITER=ITER+1
          RPAS=RPAS*2
          IF ((CRIT1.LT.0.D0).OR.(CRITP1.GE.0.D0)) THEN
            ETA=ETA-RPAS
            CALL CRITEO(EPSP  ,EPSD  ,ETA   ,B     ,D     ,
     &                  LAMBDA,MU    ,ALPHA ,ECROB ,ECROD ,
     &                  SEUIL ,CRIT1 ,CRITP1)
            GOTO 60
          ENDIF
C          write (6,*) 'ITER-1 = ',ITER
        ELSE
30        CONTINUE
          ITER=ITER+1
          RPAS=RPAS*2
          IF (CRIT1.GE.0.D0) THEN
            ETA=ETA-RPAS
            CALL CRITEO(EPSP  ,EPSD  ,ETA   ,B     ,D     ,
     &                  LAMBDA,MU    ,ALPHA ,ECROB ,ECROD ,
     &                  SEUIL ,CRIT1 ,CRITP1)
            GOTO 30
          ENDIF
C          write (6,*) 'ITER-1b = ',ITER
        ENDIF
        ETA1=ETA


        ETA=ETAMAX
        RPAS=(ETAMAX-ETAMIN)
        CALL CRITEO(EPSP  ,EPSD  ,ETA   ,B     ,D     ,
     &              LAMBDA,MU    ,ALPHA ,ECROB ,ECROD ,
     &              SEUIL ,CRIT2 ,CRITP2)
        ITER=0
        IF (RECHBD) THEN
40        CONTINUE
            ITER=ITER+1
            RPAS=RPAS*2
            IF ((CRIT2.LT.0.D0).OR.(CRITP2.LE.0.D0)) THEN
              ETA=ETA+RPAS
              CALL CRITEO(EPSP  ,EPSD  ,ETA   ,B     ,D     ,
     &                    LAMBDA,MU    ,ALPHA ,ECROB ,ECROD ,
     &                    SEUIL ,CRIT2 ,CRITP2)
            GOTO 40
          ENDIF
C          write (6,*) 'ITER-2 = ',ITER
        ELSE
50        CONTINUE
          ITER=ITER+1
          RPAS=RPAS*2
          IF (CRIT2.GE.0.D0) THEN
            ETA=ETA+RPAS
              CALL CRITEO(EPSP  ,EPSD  ,ETA   ,B     ,D     ,
     &                    LAMBDA,MU    ,ALPHA ,ECROB ,ECROD ,
     &                    SEUIL ,CRIT2 ,CRITP2)
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
          IF (ABS(Y(3)) .LE. EPSTOL*SEUILA*TAU) GOTO 201
          IF (MOD(ITER,5) .NE. 0) THEN
            CALL ZEROG2(X,Y,Z,ITER)
          ELSE
            CALL ZEROD2(X,Y,Z)
          END IF
          CALL CRITEO(EPSP  ,EPSD  ,X(3)  ,B     ,D     ,
     &                LAMBDA,MU    ,ALPHA ,ECROB ,ECROD ,
     &                SEUIL ,Y(3)  ,Z(3)  )
200     CONTINUE
        CALL U2MESS('F','UTILITAI2_53')
201     CONTINUE
C        write (6,*) 'ITER-3 = ',ITER
        ETA=X(3)
        NSOL=1
      ENDIF

C -- CAS A MINIMUM (ZERO OU DEUX SOLUTIONS)
      IF (NSOL.EQ.2) THEN
        ETAMIN = ETA1
        ETAMAX = ETA2
        ITER   = 0
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
            CALL U2MESS('F','PILOTAGE_83')
          ENDIF
          IF ((ABS(CRITP1*(ETA2-ETA1)).LT.EPSTOL*SEUILA*TAU).AND.
     &            (ABS(CRITP2*(ETA2-ETA1)).LT.EPSTOL*SEUILA*TAU)) THEN
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
          CALL CRITEO(EPSP  ,EPSD  ,ETAC  ,B     ,D     ,
     &                LAMBDA,MU    ,ALPHA ,ECROB ,ECROD ,
     &                SEUIL ,CRITC ,CRITP )
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
     &      CALL U2MESS('F','PILOTAGE_84')
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
          CALL CRITEO(EPSP  ,EPSD  ,X(2)  ,B     ,D     ,
     &                LAMBDA,MU    ,ALPHA ,ECROB ,ECROD ,
     &                SEUIL ,Y(2)  ,Z(2)  )
          X(3)=X(2)
          Y(3)=Y(2)
          Z(3)=Z(2)
          DO 400 ITER = 1, NITMAX
            IF (ABS(Y(3)) .LE. EPSTOL*SEUILA*TAU) GOTO 401
            IF (MOD(ITER,5) .NE. 0) THEN
              CALL ZEROG2(X,Y,Z,ITER)
            ELSE
              CALL ZEROD2(X,Y,Z)
            END IF
          CALL CRITEO(EPSP  ,EPSD  ,X(3)  ,B     ,D     ,
     &                LAMBDA,MU    ,ALPHA ,ECROB ,ECROD ,
     &                SEUIL ,Y(3)  ,Z(3)  )
400       CONTINUE
          CALL U2MESS('F','PILOTAGE_83')
401       CONTINUE
C          write (6,*) 'ITER-5 = ',ITER
          ETA1=X(3)

          X(1)=ETA3
          Y(1)=CRIT3
          Z(1)=CRITP3
          X(2)=ETAMIN
          CALL CRITEO(EPSP,EPSD,X(2),B,D,LAMBDA,MU,ALPHA,ECROB,
     &                   ECROD,SEUIL,Y(2),Z(2))
          X(3)=X(2)
          Y(3)=Y(2)
          Z(3)=Z(2)
          DO 500 ITER = 1, NITMAX
            IF (ABS(Y(3)) .LE. EPSTOL*SEUILA*TAU) GOTO 501
            IF (MOD(ITER,5) .NE. 0) THEN
              CALL ZEROG2(X,Y,Z,ITER)
            ELSE
              CALL ZEROD2(X,Y,Z)
            END IF


          CALL CRITEO(EPSP,EPSD,X(3),B,D,LAMBDA,MU,ALPHA,ECROB,
     &                   ECROD,SEUIL,Y(3),Z(3))

500       CONTINUE
C          WRITE(6,*) 'ETA=',X(3)
C          WRITE(6,*) 'CRITERE=',Y(3)
C          WRITE(6,*) 'DCRIT=',Z(3)
          CALL U2MESS('F','PILOTAGE_83')
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
        CALL CRITEO(EPSP,EPSD,ETA,B,D,LAMBDA,MU,ALPHA,ECROB,
     &                   ECROD,SEUILA,CRIT1,CRITP)
        A0=CRIT1/SEUILA
      ELSE
        SEUIL=SEUILA
        ETAS=R8VIDE()

C        WRITE(6,*) 'ETA1=',ETA1
C        WRITE(6,*) 'ETA2=',ETA2

        IF (NSOL.EQ.2) THEN
          ETA=ETA1
        ENDIF
        CALL CRITEO(EPSP,EPSD,ETA,B,D,LAMBDA,MU,ALPHA,ECROB,
     &                   ECROD,SEUILA,CRIT1,CRITP)





C ======================================================================
C                        LINEARISATION DU CRITERE
C ======================================================================

        A0 = (CRIT1 - ETA*CRITP ) /SEUILA
        A1 =CRITP/ SEUILA

        IF (NSOL.EQ.2) THEN
          ETA=ETA2
          CALL CRITEO(EPSP,EPSD,ETA,B,D,LAMBDA,MU,ALPHA,ECROB,
     &                   ECROD,SEUILA,CRIT1,CRITP)
C ======================================================================
C                        LINEARISATION DU CRITERE
C ======================================================================
          A2 = (CRIT1 - ETA*CRITP ) /SEUILA
          A3 =CRITP/ SEUILA
        ELSE
          A2=R8VIDE()
          A3=R8VIDE()
        ENDIF
      ENDIF
C         WRITE(6,*) 'A0=',A0
C         WRITE(6,*) 'A1=',A1
C         WRITE(6,*) 'A2=',A2
C         WRITE(6,*) 'A3=',A3

 9999 CONTINUE
      END

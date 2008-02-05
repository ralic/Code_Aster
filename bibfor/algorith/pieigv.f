      SUBROUTINE PIEIGV(NDIM, TYPMOD, TAU,IMATE, NONLOC, VIM,EPSM,
     &                  EPSPC, EPSDC, ETAMIN,ETAMAX,A0, A1,A2,A3,ETAS)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 04/02/2008   AUTEUR GODARD V.GODARD 
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

      LOGICAL     CPLAN,MINI,FINI,MODIF
      INTEGER     NDIMSI, K, NRAC,ITER,NITMAX
      INTEGER     NCAS
      REAL*8      TREPSD, COPLAN, SIGELP(6), SIGELD(6)
      REAL*8      CRIT,RTEMP,DMAX
      REAL*8      TR(6),VECP(3,3),RAC2,CRITP
      REAL*8      FPD, DM, D, P0, P1, P2, ETA, RAC(2), IND(4),EPM(3)
      REAL*8      E, NU, LAMBDA, DEUXMU, GAMMA, SEUIL, SEUREL,TREPSM
      REAL*8      K0,K1,SICR,R
      REAL*8      R8NRM2,EPSP(7), EPSD(7),X(4),Y(4),Z(4)
      REAL*8      RMINI,EPSTOL
      REAL*8      TREPS,SIGEL(3),ETA1,ETA2,ETAC,CRIT1,CRIT2
      REAL*8      RTEMP1,RTEMP2,R8VIDE,CRITP1,CRITP2
      REAL*8      EPSVP
      CHARACTER*2 CODRET(3)
      CHARACTER*8 NOMRES(3)
      REAL*8      VALRES(3)
      REAL*8      R8DOT
      REAL*8      DDOT

      REAL*8      EPSMAX,ETASUP,ETAINF,EPSNOR
      REAL*8      TREINF,TRESUP
      REAL*8      LINTER,EPSTO2
      REAL*8      XS,YS,ZS
      REAL*8      X1,Y1,Z1
      REAL*8      X2,Y2,Z2

      REAL*8      KRON(6)
      DATA  KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/



      NITMAX=100
      EPSTOL=1.D-6
      EPSVP = 1.D-6/ABS(ETAMAX-ETAMIN)
      EPSTO2=1.D-2

      ETAS = R8VIDE()



C -- OPTION ET MODELISATION
      CPLAN = (TYPMOD(1).EQ.'C_PLAN  ')
      NDIMSI = 2*NDIM
      RAC2=SQRT(2.D0)

C -- CAS DE L'ENDOMMAGEMENT SATURE, on ne pilote pas
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
      GAMMA  = -E/VALRES(1)


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

      R=NONLOC(2)





C    ETAT MECANIQUE EN T-


      DM   = VIM(1)
      DMAX=1.D0
      D = MIN(DMAX,DM+TAU)
      FPD = (1+GAMMA) / (1+GAMMA*D)**2





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
        EPSP(K) = EPSPC(K)
        EPSD(K) = EPSDC(K)
45    CONTINUE

      IF (NDIMSI.LT.6) THEN
        DO 46 K=NDIMSI+1,6
          EPSP(K)=0.D0
          EPSD(K)=0.D0
46      CONTINUE
      ENDIF



        EPSP(7) = NONLOC(3)
        EPSD(7) = NONLOC(4)






C Calcul du nombre de solutions sur un intervalle raisonnable

C - ON COMMENCE DONC PAR REGARDER LES VP DE EPSD
      TR(1) = EPSD(1)
      TR(2) = EPSD(4)/RAC2
      TR(3) = EPSD(5)/RAC2
      TR(4) = EPSD(2)
      TR(5) = EPSD(6)/RAC2
      TR(6) = EPSD(3)



C -- DIAGONALISATION AVEC TRI EN VAL RELATIVE CROISSANT
      CALL DIAGP3(TR,VECP,EPM)


C On prend la valeur absolue max des valeurs propres de EPSD
        EPSMAX=MAX(ABS(EPM(1)),ABS(EPM(3)))

C Si les valeurs propres sont trop petites, on ne pilote pas ce point
      IF (EPSMAX.LT.EPSVP) GOTO 666



C on "normalise" les deformations pilotees

      TREPSD = EPSD(1)+EPSD(2)+EPSD(3)
      DO 60 K=1,NDIMSI
        SIGELD(K) = LAMBDA*TREPSD*KRON(K) + DEUXMU*EPSD(K)
 60   CONTINUE

      EPSNOR = 1.D0/SQRT(0.5D0 * DDOT(NDIMSI,EPSD,1,SIGELD,1))

      DO 678 K=1,7
        EPSD(K)=EPSD(K)*EPSNOR
678   CONTINUE




C CALIBRAGE DE L'INTERVALLE DE RECHERCHE POUR EVITER LES DIVERGENCES
C DE L'ALGORITHME DE RECHERCHE

C On repercute la normalisation sur les bornes de ETA pour
C definir l'intervalle de recherche

      ETASUP=ETAMAX/EPSNOR
      ETAINF=-ETAMAX/EPSNOR


C Test sur la valeur de la trace de la deformee pour eta=etainf 
C pour s'assurer qu'elle ne diverge pas. On fixe une borne tr(eps)<1
      TREINF=EPSP(1)+EPSP(2)+EPSP(3)+ETAINF*(EPSD(1)+EPSD(2)+EPSD(3))
      IF (ABS(TREINF).GT.1.D0) THEN
C        WRITE(6,*) 'Modification de etainf  :',ETAINF
        ETAINF=(TREINF/ABS(TREINF)-(EPSP(1)+EPSP(2)+EPSP(3)))
     &          /(EPSD(1)+EPSD(2)+EPSD(3))
C        WRITE(6,*) 'devient  :',ETAINF
      ENDIF

      ETA=ETAINF
      CALL CRITEV(EPSP,EPSD,ETA,LAMBDA,DEUXMU,FPD,SEUIL,R*D,
     &              CRIT1,CRITP1)


C Test sur la valeur de la trace de la deformee pour eta=etasup 
C pour s'assurer qu'elle ne diverge pas. On fixe une borne tr(eps)<1

      TRESUP=EPSP(1)+EPSP(2)+EPSP(3)+ETASUP*(EPSD(1)+EPSD(2)+EPSD(3))
      IF (ABS(TRESUP).GT.1.D0) THEN
C        WRITE(6,*) 'Modification de etasup  :',ETASUP
        ETASUP=(TRESUP/ABS(TRESUP)-(EPSP(1)+EPSP(2)+EPSP(3)))
     &          /(EPSD(1)+EPSD(2)+EPSD(3))
C        WRITE(6,*) 'devient  :',ETASUP
      ENDIF

      ETA=ETASUP
      CALL CRITEV(EPSP,EPSD,ETA,LAMBDA,DEUXMU,FPD,SEUIL,R*D,
     &              CRIT2,CRITP2)



C Longueur de l'intervalle
      LINTER=ABS(ETASUP-ETAINF)



C###############################################################
C RECHERCHE DES SOLUTIONS SUR L'INTERVALLE [-ETASUP,ETASUP]
C###############################################################


C CAS A 0 SOLUTION

C on reste en dessous du seuil sur l'intervalle
      IF ((CRIT1.LT.0.D0).AND.(CRIT2.LT.0.D0)) THEN
C        WRITE(6,*) 'cas 1'
        GOTO 666
      ENDIF

C on reste au dessus du seuil sur l'intervalle, 
C        on utilise la convexite pour le voir

      IF (((CRIT1.GT.0.D0).AND.(CRITP1.GT.(-CRIT1/LINTER))).OR.
     &    ((CRIT2.GT.0.D0).AND.(CRITP2.LT.(CRIT2/LINTER)))) THEN
C        WRITE(6,*) 'cas 2'
        GOTO 666
      ENDIF


C CAS A 1 SOLUTION

      IF ((CRIT1.LT.0.D0).AND.(CRIT2.GT.0.D0)) THEN
C        WRITE(6,*) 'cas 3'
        X(1)=ETAINF
        Y(1)=CRIT1
        Z(1)=CRITP1
        X(2)=ETASUP
        Y(2)=CRIT2
        Z(2)=CRITP2

        X(3)=X(1)
        Y(3)=Y(1)
        Z(3)=Z(1)

        DO 200 ITER = 1, NITMAX

          IF (ABS(Y(3)) .LE. EPSTOL*SEUIL) GOTO 201
            IF (ABS(Z(1)-Z(2)).LT.EPSTO2*ABS(Z(2))) THEN
              X(3)=(-Y(3)+Z(3)*X(3))/Z(3)
              GOTO 555
            ENDIF
            CALL ZEROD2(X,Y,Z)
555       CONTINUE

          CALL CRITEV(EPSP,EPSD,X(3),LAMBDA,DEUXMU,FPD,SEUIL,R*D,
     &             Y(3),Z(3))

200     CONTINUE
          CALL U2MESS('F','ALGORITH9_87')
201     CONTINUE

        A1 =Z(3)/EPSNOR
        A0 = TAU-X(3)*A1*EPSNOR
        A2=R8VIDE()
        A3=R8VIDE()

        GOTO 9999

      ENDIF
        
      IF ((CRIT1.GT.0.D0).AND.(CRIT2.LT.0.D0)) THEN
C        WRITE(6,*) 'cas 4'
        X(2)=ETAINF
        Y(2)=CRIT1
        Z(2)=CRITP1
        X(1)=ETASUP
        Y(1)=CRIT2
        Z(1)=CRITP2

        X(3)=X(1)
        Y(3)=Y(1)
        Z(3)=Z(1)
        DO 202 ITER = 1, NITMAX
          IF (ABS(Y(3)) .LE. EPSTOL*SEUIL) GOTO 203

            IF (ABS(Z(1)-Z(2)).LT.EPSTO2*ABS(Z(2))) THEN
              X(3)=(-Y(3)+Z(3)*X(3))/Z(3)
              GOTO 556
            ENDIF
            CALL ZEROD2(X,Y,Z)
556       CONTINUE

          CALL CRITEV(EPSP,EPSD,X(3),LAMBDA,DEUXMU,FPD,SEUIL,R*D,
     &             Y(3),Z(3))
202     CONTINUE
          CALL U2MESS('F','ALGORITH9_87')
203     CONTINUE

        A1 =Z(3)/EPSNOR
        A0 = TAU-X(3)*A1*EPSNOR
        A2=R8VIDE()
        A3=R8VIDE()

        GOTO 9999

      ENDIF



C CAS A 2 OU 0 SOLUTIONS

      IF (((CRIT1.GT.0.D0).AND.(CRITP1.LT.(-CRIT1/LINTER))).AND.
     &    ((CRIT2.GT.0.D0).AND.(CRITP2.GT.(CRIT2/LINTER)))) THEN

C        WRITE(6,*) 'cas 5'


C il faut chercher s'il y a une valeur dans l'intervalle qui donne une
C valeur du critere negative
C s'il y en a une, il y a 2 solutions, sinon 0 solution
C
C On utilise les tangentes pour aller vers le "minimum"
C on s'arrete quand le critere est negatif, on se fiche 
C de trouver exactement le minimum

         X1=ETAINF
         Y1=CRIT1
         Z1=CRITP1
         X2=ETASUP
         Y2=CRIT2
         Z2=CRITP2

         YS=Y1

         ITER=0

750      CONTINUE

         IF (ITER.LT.NITMAX) THEN
           XS=(Y2-Y1+Z1*X1-Z2*X2)/(Z1-Z2)
           CALL CRITEV(EPSP,EPSD,XS,LAMBDA,DEUXMU,FPD,SEUIL,R*D,
     &              YS,ZS)
           IF (YS.LT.0.D0) GOTO 751

           IF (ZS.GT.0.D0) THEN
             X2=XS
             Y2=YS
             Z2=ZS
             LINTER=X2-X1
             IF ((Z1.GT.(-Y1/LINTER)).OR.
     &           (Z2.LT.(Y2/LINTER))) THEN
               GOTO 666
             ENDIF
             GOTO 750
           ELSE
             X1=XS
             Y1=YS
             Z1=ZS
             LINTER=X2-X1
             IF ((Z1.GT.(-Y1/LINTER)).OR.
     &           (Z2.LT.(Y2/LINTER))) THEN
               GOTO 666
             ENDIF
             GOTO 750
           ENDIF
         ELSE
           GOTO 666
         ENDIF

751      CONTINUE


C il y a une solution sur [ETAINF,XS] et une sur [XS,ETASUP]


C Calcul de la solution sur [XS,ETASUP]
        X(1)=XS
        Y(1)=YS
        Z(1)=ZS
        X(2)=ETASUP
        Y(2)=CRIT2
        Z(2)=CRITP2

        X(3)=X(1)
        Y(3)=Y(1)
        Z(3)=Z(1)

        DO 204 ITER = 1, NITMAX
          IF (ABS(Y(3)) .LE. EPSTOL*SEUIL) GOTO 205

            IF (ABS(Z(1)-Z(2)).LT.EPSTO2*ABS(Z(2))) THEN
              X(3)=(-Y(3)+Z(3)*X(3))/Z(3)
              GOTO 557
            ENDIF
            CALL ZEROD2(X,Y,Z)
557       CONTINUE

          CALL CRITEV(EPSP,EPSD,X(3),LAMBDA,DEUXMU,FPD,SEUIL,R*D,
     &             Y(3),Z(3))

204     CONTINUE
          CALL U2MESS('F','ALGORITH9_87')
205     CONTINUE

        A1 =Z(3)/EPSNOR
        A0 = TAU-X(3)*A1*EPSNOR


C Calcul de la solution sur [-ETASUP,XS]
        X(1)=XS
        Y(1)=YS
        Z(1)=ZS
        X(2)=ETAINF
        Y(2)=CRIT1
        Z(2)=CRITP1

        X(3)=X(1)
        Y(3)=Y(1)
        Z(3)=Z(1)

        DO 206 ITER = 1, NITMAX
          IF (ABS(Y(3)) .LE. EPSTOL*SEUIL) GOTO 207
            IF (ABS(Z(1)-Z(2)).LT.EPSTO2*ABS(Z(2))) THEN
              X(3)=(-Y(3)+Z(3)*X(3))/Z(3)
              GOTO 558
            ENDIF
            CALL ZEROD2(X,Y,Z)
558       CONTINUE

          CALL CRITEV(EPSP,EPSD,X(3),LAMBDA,DEUXMU,FPD,SEUIL,R*D,
     &             Y(3),Z(3))

206     CONTINUE
          CALL U2MESS('F','ALGORITH9_87')
207     CONTINUE

        A3 =Z(3)/EPSNOR
        A2 = TAU-X(3)*A3*EPSNOR

        GOTO 9999


      ENDIF

666    CONTINUE
        A0 = 0.D0
        A1 = 0.D0
        A2 = R8VIDE()
        A3 = R8VIDE()


9999   CONTINUE

C on "redonne" le vrai EPSD
      DO 679 K=1,7
        EPSD(K)=EPSD(K)/EPSNOR
679   CONTINUE


      END

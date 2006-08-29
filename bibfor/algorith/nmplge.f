      SUBROUTINE NMPLGE(NDIM,NNO1,VFF1,IDFDE1,NNO2,VFF2,IDFDE2,NPG,IW,
     &  GEOM,TYPMOD,OPTION,MATE,COMPOR,CRIT,INSTAM,INSTAP,TEMPM,TEMPP,
     &  TREF,ANGMAS,DDLM,DDLD,SIGM,LGPG,VIM,SIGP,VIP,MATR,VECT,CODRET,
     &  DFDI2)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 28/08/2006   AUTEUR CIBHHPD L.SALMONA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_21

       IMPLICIT NONE

       CHARACTER*8   TYPMOD(*)
       CHARACTER*16  OPTION, COMPOR(*)

       INTEGER NDIM,NNO1,NNO2,NPG,IDFDE1,IDFDE2,IW,MATE,LGPG,CODRET
       REAL*8  VFF1(NNO1,NPG),VFF2(NNO2,NPG),GEOM(NDIM,NNO1)
       REAL*8  CRIT(*),INSTAM,INSTAP,TEMPM(*),TEMPP(*),TREF
       REAL*8  DDLM(*),DDLD(*),SIGM(2*NDIM,NPG),SIGP(2*NDIM,NPG)
       REAL*8  VIM(LGPG,NPG),VIP(LGPG,NPG),MATR(*),VECT(*)
       REAL*8  DFDI2(NNO2,NDIM),ANGMAS(3)
C ----------------------------------------------------------------------
C
C     RAPH_MECA, RIGI_MECA_* ET FULL_MECA_* POUR GRAD_EPSI (2D ET 3D)
C
C IN  NDIM    : DIMENSION DES ELEMENTS
C IN  NNO1    : NOMBRE DE NOEUDS (FAMILLE U)
C IN  VFF1    : VALEUR DES FONCTIONS DE FORME (FAMILLE U)
C IN  IDFDE1  : DERIVEES DES FONCTIONS DE FORME DE REFERENCE (FAMILLE U)
C IN  NNO2    : NOMBRE DE NOEUDS (FAMILLE E)
C IN  VFF2    : VALEUR DES FONCTIONS DE FORME (FAMILLE E)
C IN  IDFDE2  : DERIVEES DES FONCTIONS DE FORME DE REFERENCE (FAMILLE E)
C IN  NPG     : NOMBRE DE POINTS DE GAUSS
C IN  IW      : POIDS DES POINTS DE GAUSS DE REFERENCE (INDICE)
C IN  GEOM    : COORDONNEES DES NOEUDS
C IN  TYPMOD  : TYPE DE MODEELISATION
C IN  OPTION  : OPTION DE CALCUL
C IN  MATE   : MATERIAU CODE
C IN  COMPOR  : COMPORTEMENT
C IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
C IN  INSTAM  : INSTANT PRECEDENT
C IN  INSTAP  : INSTANT DE CALCUL
C IN  TEMPM   : TEMPERATURE AUX NOEUDS A L'INSTANT PRECEDENT
C IN  TEMPP   : TEMPERATURE AUX NOEUDS A L'INSTANT DE CALCUL
C IN  TREF    : TEMPERATURE DE REFERENCE
C IN  DDLM    : DDL A L'INSTANT PRECEDENT
C IN  DDLD    : INCREMENT DES DDL
C IN  SIGM    : CONTRAINTES A L'INSTANT PRECEDENT
C IN  LGPG    : LONGUEUR DU TABLEAU DES VARIABLES INTERNES
C IN  VIM     : VARIABLES INTERNES A L'INSTANT PRECEDENT
C IN  ANGMAS  : REPERE LOCAL 3D
C OUT SIGP    : CONTRAINTES DE CAUCHY (RAPH_MECA   ET FULL_MECA_*)
C OUT VIP     : VARIABLES INTERNES    (RAPH_MECA   ET FULL_MECA_*)
C OUT MATR    : MATRICE DE RIGIDITE   (RIGI_MECA_* ET FULL_MECA_*)
C OUT VECT    : FORCES INTERIEURES    (RAPH_MECA   ET FULL_MECA_*)
C OUT CODRET  : CODE RETOUR
C MEM DFDI2   :
C ----------------------------------------------------------------------

      CHARACTER*2 K2

      LOGICAL RESI,RIGI,GRAND,AXI
      INTEGER NDIMSI,NDDL,G,COD(27),N,I,M,J,KL,PQ,OS,KK
      INTEGER IU(3,27),IE(6,8)
      REAL*8  RAC2,LC,C,DEPLM(3,27),DEPLD(3,27),TM,TP,DFDI1(27,3)
      REAL*8  R,WG,EPSGM(6,2),EPSGD(6,2),GEPSM(6,3),GEPS(6,3),F(3,3)
      REAL*8  B(6,3,27),DE(6),TAMPON(10),SIGMA(6),DSIDEP(6,6,2),T1,T2
      REAL*8  P(6,6),SIGMAM(6),PERT
      REAL*8  DDOT

      PARAMETER (PERT = 1.D-4)
C ----------------------------------------------------------------------



C - INITIALISATION

      RESI = OPTION(1:9).EQ.'FULL_MECA' .OR. OPTION(1:9).EQ.'RAPH_MECA'
      RIGI = OPTION(1:9).EQ.'FULL_MECA' .OR. OPTION(1:9).EQ.'RIGI_MECA'
      RAC2   = SQRT(2.D0)
      GRAND  = .FALSE.
      AXI    = .FALSE.
      NDIMSI = 2*NDIM
      NDDL   = NNO1*NDIM + NNO2*NDIMSI
      CALL RCVALA(MATE,' ','NON_LOCAL',0,' ',0.D0,1,'LONG_CAR',
     &            LC,K2,'FM')
      C = LC**2
      DO 5 G=1,NPG
        COD(G)=0
 5    CONTINUE
      IF (RIGI) CALL R8INIR(NDDL*NDDL,0.D0,MATR,1)
      IF (RESI) CALL R8INIR(NDDL,0.D0,VECT,1)
      CALL R8INIR(6,0.D0,SIGMAM,1)

C    POSITION DES INDICES POUR LES DEPLACEMENTS ET LES DEFORMATIONS

      DO 10 N = 1,NNO2
        DO 20 I = 1,NDIM
          IU(I,N) = I + (N-1)*(NDIM+NDIMSI)
 20     CONTINUE
        DO 30 KL = 1,NDIMSI
          IE(KL,N) = KL + NDIM + (N-1)*(NDIM+NDIMSI)
 30     CONTINUE
 10   CONTINUE
      OS = (NDIMSI+NDIM)*NNO2
      DO 40 N = 1,NNO1-NNO2
        DO 50 I = 1,NDIM
          IU(I,N+NNO2) = I + (N-1)*NDIM + OS
 50     CONTINUE
 40   CONTINUE


C    EXTRACTION DES DEPLACEMENTS

      DO 100 N = 1,NNO1
        DO 110 I = 1,NDIM
          DEPLM(I,N) = DDLM(IU(I,N))
          DEPLD(I,N) = DDLD(IU(I,N))
 110    CONTINUE
 100  CONTINUE



C - CALCUL POUR CHAQUE POINT DE GAUSS

      DO 1000 G=1,NPG


C      VARIABLES DE COMMANDE

        TM = DDOT(NNO1,VFF1(1,G),1,TEMPM,1)
        TP = DDOT(NNO1,VFF1(1,G),1,TEMPP,1)


C      CALCUL DES ELEMENTS GEOMETRIQUES DE L'EF POUR E-BARRE

        CALL DFDMIP(NDIM,NNO2,AXI,GEOM,G,IW,VFF2(1,G),IDFDE2,R,WG,DFDI2)
        CALL NMEPSB(NDIM,NNO2,AXI,VFF2(1,G),DFDI2,DDLM,EPSGM(1,2),GEPSM)
        CALL NMEPSB(NDIM,NNO2,AXI,VFF2(1,G),DFDI2,DDLD,EPSGD(1,2),GEPS)


C      CALCUL DES ELEMENTS GEOMETRIQUES DE L'EF POUR U

        CALL DFDMIP(NDIM,NNO1,AXI,GEOM,G,IW,VFF1(1,G),IDFDE1,R,WG,DFDI1)
        CALL NMEPSI(NDIM,NNO1,AXI,GRAND,VFF1(1,G),R,DFDI1,DEPLM,F,EPSGM)
        CALL NMEPSI(NDIM,NNO1,AXI,GRAND,VFF1(1,G),R,DFDI1,DEPLD,F,EPSGD)
        CALL NMMABU(NDIM,NNO1,AXI,GRAND,DFDI1,B)


C      DEFORMATIONS ET ECARTS EN FIN DE PAS DE TEMPS

        CALL DAXPY(18,1.D0,GEPSM,1,GEPS,1)
        DO 200 KL = 1,NDIMSI
          DE(KL) = EPSGM(KL,2)+EPSGD(KL,2) - EPSGM(KL,1)-EPSGD(KL,1)
 200    CONTINUE


C      LOI DE COMPORTEMENT

        CALL DCOPY(NDIMSI,SIGM(1,G),1,SIGMAM,1)
        CALL DSCAL(3,RAC2,SIGMAM(4),1)

        CALL NMCOMP('RIGI',G,1,NDIM,TYPMOD,MATE,COMPOR,CRIT,INSTAM,
     &          INSTAP,TM,TP,TREF,EPSGM,EPSGD,SIGMAM,
     &          VIM(1,G),OPTION,
     &          ANGMAS,P,
     &          SIGMA,VIP(1,G),DSIDEP,COD(G))
        IF(COD(G).EQ.1) GOTO 9000


C      FORCE INTERIEURE ET DES CONTRAINTES DE CAUCHY

        IF (RESI) THEN

C        VECTEUR FINT:U
          DO 300 N=1,NNO1
            DO 310 I=1,NDIM
              KK = IU(I,N)
              T1 = 0
              DO 320 KL = 1,NDIMSI
                T1 = T1 + SIGMA(KL)*B(KL,I,N)
 320          CONTINUE
              VECT(KK) = VECT(KK) + WG*T1
 310        CONTINUE
 300      CONTINUE

C        VECTEUR FINT:E
          DO 350 N=1,NNO2
            DO 360 KL = 1,NDIMSI
              KK = IE(KL,N)
              T1 = 0
              DO 365 PQ = 1,NDIMSI
                T1 = T1 + P(KL,PQ)*DE(PQ)*VFF2(N,G)
 365          CONTINUE
              T2 = 0
              DO 370 I = 1,NDIM
                DO 375 PQ = 1,NDIMSI
                  T2 = T2 + C*DFDI2(N,I)*P(KL,PQ)*GEPS(PQ,I)
 375            CONTINUE
 370          CONTINUE
              VECT(KK) = VECT(KK) + WG*(T1+T2)
 360        CONTINUE
 350      CONTINUE

C        CONTRAINTES
          CALL DCOPY(NDIMSI,SIGMA,1,SIGP(1,G),1)
          CALL DSCAL(NDIMSI-3,1.D0/RAC2,SIGP(4,G),1)

        END IF


C - CALCUL DE LA MATRICE DE RIGIDITE (STOCKAGE LIGNE DE DFI/DUJ)
C
        IF (RIGI) THEN

C        MATRICE K:U(I,N),U(J,M)
          DO 500 N = 1,NNO1
            DO 510 I = 1,NDIM
              OS = NDDL*(IU(I,N)-1)
              DO 520 M = 1,NNO1
                DO 530 J = 1,NDIM
                  KK = OS+IU(J,M)
                  T1 = 0
                  DO 540 KL = 1,NDIMSI
                    DO 550 PQ = 1,NDIMSI
                      T1 = T1+DSIDEP(KL,PQ,1)*B(PQ,J,M)*B(KL,I,N)
 550                CONTINUE
 540              CONTINUE
                  MATR(KK) = MATR(KK) + WG*T1
 530            CONTINUE
 520          CONTINUE

C        MATRICE K:U(I,N),E(PQ,M)
              DO 600 M = 1,NNO2
                DO 610 PQ = 1,NDIMSI
                  KK = OS+IE(PQ,M)
                  T1 = 0
                  DO 620 KL = 1,NDIMSI
                    T1 = T1 + DSIDEP(KL,PQ,2)*VFF2(M,G)*B(KL,I,N)
 620              CONTINUE
                  MATR(KK) = MATR(KK) + WG*T1
 610            CONTINUE
 600          CONTINUE
 510        CONTINUE
 500      CONTINUE

C        MATRICE K:E(KL,N),U(J,M)
          DO 700 N = 1,NNO2
            DO 710 KL = 1,NDIMSI
              OS = NDDL*(IE(KL,N)-1)
              DO 720 M = 1,NNO1
                DO 730 J = 1,NDIM
                  KK = OS+IU(J,M)
                  T1 = 0
                  DO 735 PQ = 1,NDIMSI
                    T1 = T1 - P(KL,PQ)*B(PQ,J,M)*VFF2(N,G)
 735              CONTINUE
                  MATR(KK) = MATR(KK) + WG*T1
 730            CONTINUE
 720          CONTINUE
 710        CONTINUE
 700      CONTINUE


C        MATRICE K:E(KL,N),E(PQ,M)

C        RIGIDITE FICTIVE SI POINT SATURE
          CALL DSCAL(36,1-PERT,P,1)
          DO 780 KL = 1,6
            P(KL,KL) = P(KL,KL) + PERT
 780      CONTINUE


          DO 800 N = 1,NNO2
            DO 810 M = 1,NNO2
              T1 = VFF2(N,G)*VFF2(M,G)
              DO 820 I = 1,NDIM
                T1 = T1 + C*DFDI2(N,I)*DFDI2(M,I)
 820          CONTINUE
              DO 830 KL = 1,NDIMSI
                DO 835 PQ = 1,NDIMSI
                  KK = (IE(KL,N)-1)*NDDL + IE(PQ,M)
                  MATR(KK) = MATR(KK) + WG*T1*P(KL,PQ)
 835            CONTINUE
 830          CONTINUE
 810        CONTINUE
 800      CONTINUE


        END IF

 1000 CONTINUE


C - SYNTHESE DES CODES RETOUR

 9000 CONTINUE
      CALL CODERE(COD,NPG,CODRET)

      END

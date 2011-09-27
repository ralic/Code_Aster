      SUBROUTINE NMPLGV(FAMI,NDIM,NNO1,NNO2,NNO3,NPG,IW,VFF1,VFF2,VFF3,
     &  IDFDE1,IDFDE2,
     &  GEOM,TYPMOD,OPTION,MAT,COMPOR,LGPG,CRIT,INSTAM,INSTAP,
     &  DDLM,DDLD,ANGMAS,SIGM,VIM,SIGP,VIP,MATR,VECT,CODRET)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/09/2011   AUTEUR PROIX J-M.PROIX 
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

C TOLE CRP_21

       IMPLICIT NONE

       CHARACTER*8   TYPMOD(*)
       CHARACTER*(*) FAMI
       CHARACTER*16  OPTION, COMPOR(*)

       INTEGER NDIM,NNO1,NNO2,NNO3,NPG,IDFDE1,IDFDE2,IW,MAT,LGPG,CODRET
       REAL*8  VFF1(NNO1,NPG),VFF2(NNO2,NPG),VFF3(NNO3,NPG)
       REAL*8  GEOM(NDIM,NNO1)
       REAL*8  CRIT(*),INSTAM,INSTAP
       REAL*8  DDLM(*),DDLD(*),SIGM(2*NDIM+1,NPG),SIGP(2*NDIM+1,NPG)
       REAL*8  VIM(LGPG,NPG),VIP(LGPG,NPG),MATR(*),VECT(*)
       REAL*8  ANGMAS(3)

C ----------------------------------------------------------------------
C
C     RAPH_MECA, RIGI_MECA_* ET FULL_MECA_* POUR GRAD_VARI (2D ET 3D)
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
C IN  MAT     : MATERIAU CODE
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
C OUT SIGP    : CONTRAINTES DE CAUCHY (RAPH_MECA   ET FULL_MECA_*)
C OUT VIP     : VARIABLES INTERNES    (RAPH_MECA   ET FULL_MECA_*)
C OUT MATR    : MATRICE DE RIGIDITE   (RIGI_MECA_* ET FULL_MECA_*)
C OUT VECT    : FORCES INTERIEURES    (RAPH_MECA   ET FULL_MECA_*)
C OUT CODRET  : CODE RETOUR
C MEM DFDI2   :
C ----------------------------------------------------------------------
      
      INTEGER K2(2)
      CHARACTER*8 NOM(2)
      
      
      LOGICAL RESI,RIGI,GRAND,AXI
      INTEGER NDDL,NDIMSI,G,COD(27),N,I,M,J,KL,PQ,OS,OSA,OSL,KK
      INTEGER IU(3,27),IA(8),IL(8)
      REAL*8  RAC2,C,RAUG,VAL(2)
      REAL*8  DEPLM(3*27),DEPLD(3*27),DFDI1(27,3)
      REAL*8  AVM,AVD,AVP,MUM,MUD,MUP,AGM(3),AGD(3),AGP(3),BP
      REAL*8  R,WG,EPSM(6),EPSD(6),F(3,3),B(6,3,27)
      REAL*8  NONLOC(2),SIGMAM(6),SIGMA(6),DSIDEP(6,6,4),T1,T2,T3
      REAL*8  PHI,RBID

      REAL*8  DFDI2(8*3)

C     DATA  NOM /'C_GRAD_VARI','PENA_LAGR'/
      DATA  NOM /'C_GRAD_V','PENA_LAG'/
C ----------------------------------------------------------------------




C - INITIALISATION

      RESI  = OPTION(1:9).EQ.'FULL_MECA' .OR. OPTION(1:9).EQ.'RAPH_MECA'
      RIGI  = OPTION(1:9).EQ.'FULL_MECA' .OR. OPTION(1:9).EQ.'RIGI_MECA'
      RAC2  = SQRT(2.D0)
      GRAND = .FALSE.
      AXI   = TYPMOD(1) .EQ. 'AXIS'
      NDDL  = NNO1*NDIM + NNO2 + NNO3
      NDIMSI= 2*NDIM

      IF (RIGI) CALL R8INIR((NDDL*(NDDL+1))/2,0.D0,MATR,1)
      IF (RESI) CALL R8INIR(NDDL,0.D0,VECT,1)

      DO 5 G=1,NPG
        COD(G)=0
 5    CONTINUE

      CALL NMGVDD(NDIM,NNO1,NNO2,IU,IA,IL)

      
C - CALCUL DES ELEMENTS GEOMETRIQUES SPECIFIQUES AU COMPORTEMENT

      CALL RCVALA(MAT,' ','NON_LOCAL',0,' ',0.D0,2,NOM,VAL,K2,2)
      C    = VAL(1)
      RAUG = VAL(2)

 
C    EXTRACTION DES DEPLACEMENTS


      DO 100 N = 1,NNO1
        DO 110 I = 1,NDIM
          DEPLM(I+(N-1)*NDIM) = DDLM(IU(I,N))
          DEPLD(I+(N-1)*NDIM) = DDLD(IU(I,N))
 110    CONTINUE
 100  CONTINUE



C - CALCUL POUR CHAQUE POINT DE GAUSS

      DO 1000 G=1,NPG


C      CALCUL DES ELEMENTS GEOMETRIQUES DE L'EF POUR A ET L

        CALL DFDMIP(NDIM,NNO2,AXI,GEOM,G,IW,VFF2(1,G),IDFDE2,R,WG,DFDI2)
        AVM = 0
        AVD = 0
        DO 150 N = 1,NNO2
          AVM = AVM + VFF2(N,G)*DDLM(IA(N))
          AVD = AVD + VFF2(N,G)*DDLD(IA(N))
 150    CONTINUE
        AVP = AVM + AVD
 
        MUM = 0
        MUD = 0
        DO 160 N = 1,NNO3
          MUM = MUM + VFF3(N,G)*DDLM(IL(N))
          MUD = MUD + VFF3(N,G)*DDLD(IL(N))
 160    CONTINUE
        MUP = MUM + MUD
    
        DO 200 I = 1,NDIM
          AGM(I) = 0
          AGD(I) = 0
          DO 202 N = 1,NNO2
            AGM(I) = AGM(I) + DFDI2(NNO2*(I-1)+N)*DDLM(IA(N))
            AGD(I) = AGD(I) + DFDI2(NNO2*(I-1)+N)*DDLD(IA(N))
 202      CONTINUE
          AGP(I) = AGM(I) + AGD(I)
 200    CONTINUE  

        PHI= MUP + RAUG*AVP


        
C      CALCUL DES ELEMENTS GEOMETRIQUES DE L'EF POUR U

        CALL DFDMIP(NDIM,NNO1,AXI,GEOM,G,IW,VFF1(1,G),IDFDE1,R,WG,DFDI1)
        CALL R8INIR(6,0.D0,EPSM,1)
        CALL R8INIR(6,0.D0,EPSD,1)

        CALL NMEPSI(NDIM,NNO1,AXI,GRAND,VFF1(1,G),R,DFDI1,DEPLM,F,EPSM)
        CALL NMEPSI(NDIM,NNO1,AXI,GRAND,VFF1(1,G),R,DFDI1,DEPLD,F,EPSD)
        CALL NMMABU(NDIM,NNO1,.FALSE.,GRAND,DFDI1,B)
        IF (AXI) THEN
          DO 50 N=1,NNO1
            B(3,1,N) = VFF1(N,G)/R
 50       CONTINUE
        ENDIF



C      LOI DE COMPORTEMENT

C      CONVENTIONS :
C       1. PHI   EST PASSE DANS NONLOC(1)
C       2. B     EST RANGE DANS VI(1)
C       3. LA MATRICE TANGENTE EST RANGEE DE LA MANIERE SUIVANTE
C            DSIG/DEPS(IJ,KL) = DSIDEP(IJ,KL,1)
C            DSIG/DPHI(IJ)    = DSIDEP(IJ,1,2)   (INUTILE SYMETRIE)
C            DB/DEPS(IJ)      = DSIDEP(IJ,1,3)
C            DB/DPHI          = DSIDEP(1,1,4)
C       4. LE COEFFICIENT DE PENALISATION RAUG EST PASSE DANS NONLOC(2)

        DO 210 KL = 1,3
          SIGMAM(KL) = SIGM(KL,G)
 210    CONTINUE
        DO 220 KL = 4,NDIMSI
          SIGMAM(KL) = SIGM(KL,G)*RAC2
 220    CONTINUE          


        NONLOC(1)=PHI
        NONLOC(2)=RAUG

        CALL NMCOMP(FAMI,G,1,NDIM,TYPMOD,MAT,COMPOR,CRIT,
     &             INSTAM,INSTAP,
     &             6,EPSM,EPSD,
     &             6,SIGMAM,VIM(1,G),
     &             OPTION,
     &             ANGMAS,
     &             2,NONLOC,
     &             SIGMA,VIP(1,G),6*6*4,DSIDEP,1,RBID,COD(G))

        IF(COD(G).EQ.1) GOTO 9000

        
C      FORCE INTERIEURE ET CONTRAINTES DE CAUCHY

        IF (RESI) THEN
        
          BP = VIP(1,G)

C        CONTRAINTES
          DO 400 KL = 1,3
            SIGP(KL,G) = SIGMA(KL)
 400      CONTINUE
          DO 410 KL = 4,NDIMSI
            SIGP(KL,G) = SIGMA(KL)/RAC2
 410      CONTINUE  

          SIGP(NDIMSI+1,G) = BP        

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

C        VECTEUR FINT:A 
          DO 350 N=1,NNO2
            T1 = VFF2(N,G)*RAUG*(AVP-BP)
            T2 = VFF2(N,G)*MUP
            T3 = 0
            DO 360 I = 1,NDIM
              T3 = T3 + C*DFDI2(NNO2*(I-1)+N)*AGP(I)
 360        CONTINUE
            KK = IA(N)
            VECT(KK) = VECT(KK) + WG*(T3+T2+T1)
 350      CONTINUE


C        VECTEUR FINT:L
          DO 370 N=1,NNO3
            T1 = VFF3(N,G)*(AVP-BP)
            KK = IL(N)
            VECT(KK) = VECT(KK) + WG*T1
 370      CONTINUE

        END IF


C - CALCUL DE LA MATRICE DE RIGIDITE 
C   STOCKAGE TRIANGLE INFERIEUR LIGNE DE DFI/DUJ
C
        IF (RIGI) THEN


C        MATRICE K:U(I,N),U(J,M)      

          DO 500 N = 1,NNO1
            DO 510 I = 1,NDIM
              OS = ((IU(I,N)-1)*IU(I,N))/2
              DO 520 M = 1,NNO1
                DO 530 J = 1,NDIM
                  IF (IU(J,M).GT.IU(I,N)) GOTO 521
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
 521          CONTINUE
 510        CONTINUE
 500      CONTINUE


   
C        MATRICES K:A(N),U(J,M)
          DO 600 N = 1,NNO2
            DO 620 M = 1,NNO1
              DO 630 J = 1,NDIM
                T1 = 0
                DO 640 KL = 1,NDIMSI
                  T1 = T1 + DSIDEP(KL,1,3)*B(KL,J,M)
 640            CONTINUE
                T1 = - RAUG*VFF2(N,G)*T1

                IF (IA(N).GE.IU(J,M)) THEN
                  KK = ((IA(N)-1)*IA(N))/2 + IU(J,M)
                ELSE
                  KK = ((IU(J,M)-1)*IU(J,M))/2 +IA(N)
                END IF
                MATR(KK) = MATR(KK) + WG*T1
 630          CONTINUE
 620        CONTINUE
 600      CONTINUE

 
C        MATRICES K:L(N),U(J,M)  
          DO 650 N = 1,NNO3
            DO 660 M = 1,NNO1
              DO 670 J = 1,NDIM
                T1 = 0
                DO 680 KL = 1,NDIMSI
                  T1 = T1 + DSIDEP(KL,1,3)*B(KL,J,M)
 680            CONTINUE
                T1 = - VFF3(N,G)*T1

                IF (IL(N).GE.IU(J,M)) THEN
                  KK = ((IL(N)-1)*IL(N))/2 + IU(J,M)
                ELSE
                  KK = ((IU(J,M)-1)*IU(J,M))/2 + IL(N)
                END IF
                MATR(KK) = MATR(KK) + WG*T1
 670          CONTINUE
 660        CONTINUE
 650      CONTINUE
 
 

C        MATRICES K:A(N),A(M) 
          DO 700 N = 1,NNO2
            OSA = ((IA(N)-1)*IA(N))/2
            DO 710 M = 1,NNO2
              T1 = RAUG*VFF2(N,G)*VFF2(M,G)*(1-RAUG*DSIDEP(1,1,4))
              T2 = 0
              DO 720 I = 1,NDIM
                T2 = T2 + DFDI2(NNO2*(I-1)+N)*DFDI2(NNO2*(I-1)+M)
 720          CONTINUE
              T2 = C*T2
              IF (IA(M).LE.IA(N)) THEN
                KK = OSA+IA(M)
                MATR(KK) = MATR(KK) + WG*(T2+T1)
              END IF

 710        CONTINUE
 700      CONTINUE
 
 
C        MATRICES K:L(N),A(M)  
          DO 750 N = 1,NNO3
            DO 760 M = 1,NNO2
              T1 = VFF3(N,G)*VFF2(M,G)*(1-RAUG*DSIDEP(1,1,4))
              IF (IL(N).GE.IA(M)) THEN
                KK = ((IL(N)-1)*IL(N))/2 + IA(M)
              ELSE
                KK = ((IA(M)-1)*IA(M))/2 + IL(N)
              END IF
              MATR(KK) = MATR(KK) + WG*T1
 760        CONTINUE
 750      CONTINUE
 


C        MATRICES K:L(N),L(M)   
          DO 800 N = 1,NNO2
            OSL = ((IL(N)-1)*IL(N))/2
            DO 810 M = 1,NNO2
              T1 = - VFF3(N,G)*VFF3(M,G)*DSIDEP(1,1,4)

              IF (IL(M).LE.IL(N)) THEN
                KK = OSL+IL(M)
                MATR(KK) = MATR(KK) + WG*T1
              END IF

 810        CONTINUE
 800      CONTINUE

 
        END IF

 1000 CONTINUE

 9000 CONTINUE
      CALL CODERE(COD,NPG,CODRET)

      END

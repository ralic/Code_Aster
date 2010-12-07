      SUBROUTINE NMPLGS(NDIM,NNO1,VFF1,IDFDE1,NNO2,VFF2,IDFDE2,NPG,IW,
     &     GEOM,TYPMOD,OPTION,MATE,COMPOR,CRIT,INSTAM,INSTAP,
     &     ANGMAS,DDLM,DDLD,SIGM,LGPG,VIM,SIGP,VIP,MATR,VECT,CODRET,
     &     DFDI2,LIVOIS,NBVOIS,NUMA,LISOCO,NBSOCO)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 15/06/2010   AUTEUR GRANET S.GRANET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C TOLE CRP_20
C TOLE CRP_21
C ======================================================================
C
C CALCUL  RAPH_MECA, RIGI_MECA_* ET FULL_MECA_* POUR GRAD_SIGM(2D ET 3D)
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
C ----------------------------------------------------------------------
C
       IMPLICIT NONE
C
       COMMON  /TRUCIT/ITEAMM
       CHARACTER*8   TYPMOD(*)
       CHARACTER*16  OPTION, COMPOR(*)
       INTEGER NBVOIS,NVOIMA,NUMAV,IRET,REUSS,NSCOMA,ITEAMM
       PARAMETER(NVOIMA=12,NSCOMA=4)
       INTEGER NDIM,NNO1,NNO2,NPG,IDFDE1,IDFDE2,IW,MATE,LGPG,CODRET
       INTEGER LIVOIS(1:NVOIMA),NUMA
       INTEGER NBSOCO(1:NVOIMA),LISOCO(1:NVOIMA,1:NSCOMA,1:2)
       REAL*8  VFF1(NNO1,NPG),VFF2(NNO2,NPG),GEOM(NDIM,NNO1)
       REAL*8  CRIT(*),INSTAM,INSTAP
       REAL*8  DDLM(*),DDLD(*),SIGM(2*NDIM,NPG),SIGP(2*NDIM,NPG)
       REAL*8  VIM(LGPG,NPG),VIP(LGPG,NPG),MATR(*),VECT(*)
       REAL*8  DFDI2(NNO2,NDIM),ANGMAS(3),COMPAR
       REAL*8  GEOVOI(NDIM+1,NNO2,NBVOIS)
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      CHARACTER*2 K2
      LOGICAL RESI,RIGI,GRAND,AXI
      INTEGER NDIMSI,NDDL,G,GG,COD(27),N,I,M,J,KL,PQ,OS,KK,VIVOIS
      INTEGER IU(3,27),IE(6,8), ETAT, ETATM,KVOIS,ZZZ,LL
      INTEGER NFIN,VRARR(NNO2),NN,NNN,VIVONU,KVOINU,NINI,NUNU
      REAL*8  RAC2,LC,C,DEPLM(3*27),DEPLD(3*27),DFDI1(27,3),NONO
      REAL*8  R,WG,EPSGM(6,2),EPSGD(6,2),GEPSM(6,3),GEPS(6,3),F(3,3)
      REAL*8  B(6,3,27),DE(6),TAMPON(10),SIGMA(6),DSIDEP(6,6,2),T1,T2
      REAL*8  P(6,6),SIGMAM(6),EPSRSS(6),SIGELL(6),DIST(NNO2,2)
      REAL*8  DDOT,Z(3,3),W(3),WORK(9),BARY(NDIM),BARYO(NDIM),SCAL(3)
      REAL*8  DIRR(NDIM)
C
C ----------------------------------------------------------------------
C
C - INITIALISATION
C
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
C      
C INITIALISATION CAVINI + INCREMENTATION 
C DU COMPTEUR D'ITERATION + L ELEMENT EST-IL POINTE?
      CALL CAVINI(NDIM,NNO2,GEOM,VIM,NPG,LGPG,MATE)
C
      NONO=0.D0
      NINI=0
C            
8491  CONTINUE
C
      NUNU=0
C
      IF (RESI) THEN
C
        IF (NINI .EQ. 0) THEN
          DO 782 GG=1,NPG  
            VIP(8,GG) = VIP(8,GG)+1.0D0
782       CONTINUE
        ENDIF
C
        DO 1783,KVOIS=1,NBVOIS
C
          NUMAV=LIVOIS(KVOIS)
          CALL TECAC2('OOO',NUMAV,'PVARIMP',1,VIVOIS,IRET)
          CALL ASSERT(IRET.EQ.0)
C
          IF (NINT(ZR(VIVOIS-1+5)) .EQ. NUMA) THEN
            IF (ZR(VIVOIS-1+7) .LT. VIP(8,1)) THEN
              VIVONU=VIVOIS
              KVOINU=KVOIS
              IF (NINT(VIP(2,1)) .EQ. 0) THEN
                DO 1773,GG=1,NPG
                  VIP(2,GG)=1.D0
1773            CONTINUE
              ENDIF
              IF (NINT(VIP(2,1)) .EQ. 1) THEN
                BARY(1)=ZR(VIVONU-1+9)
                BARY(2)=ZR(VIVONU-1+10)
                BARYO(1)=ZR(VIVONU-1+11)
                BARYO(2)=ZR(VIVONU-1+12)
              ENDIF
            ENDIF
          ENDIF
C
          IF (NINT(ZR(VIVOIS-1+6)) .EQ. NUMA) THEN
            IF (ZR(VIVOIS-1+7) .LT. VIP(8,1)) THEN
              VIVONU=VIVOIS
              KVOINU=KVOIS
              IF (NINT(VIP(2,1)) .EQ. 0) THEN
                DO 1774,GG=1,NPG
                  VIP(2,GG)=1.0D0
1774            CONTINUE
              ENDIF
              IF (NINT(VIP(2,1)) .EQ. 1) THEN
                BARY(1)=ZR(VIVONU-1+11)
                BARY(2)=ZR(VIVONU-1+12)
                BARYO(1)=ZR(VIVONU-1+9)
                BARYO(2)=ZR(VIVONU-1+10)
              ENDIF
            ENDIF
          ENDIF
C
1783    CONTINUE
C
        IF(NINT(VIP(2,1)) .EQ. 0) THEN
          CALL R8INIR(6 ,0.D0,EPSRSS,1)
          DO 2118, KL=1,NDIMSI
            DO 2119, I=1,NNO2
               LL=KL+NDIM+((I-1)*(NDIM+NDIMSI))
               EPSRSS(KL)=EPSRSS(KL)+DDLM(LL)/DBLE(NNO2)
               EPSRSS(KL)=EPSRSS(KL)+DDLD(LL)/DBLE(NNO2)
2119        CONTINUE
2118      CONTINUE      
          CALL DSCAL(NDIMSI-3,1.D0/RAC2,EPSRSS(4),1)
        ENDIF
C
        IF(NINT(VIP(2,1)) .EQ. 1) THEN
          CALL R8INIR(6 ,0.D0,EPSRSS,1)
          SCAL(1)=0
          SCAL(2)=0
          SCAL(3)=0
          DO 7614 N=1,NBSOCO(KVOINU)
            PQ=LISOCO(KVOINU,N,1)
            DO 7663 I=1,NDIM
              SCAL(N)=SCAL(N)+(GEOM(I,PQ)-BARY(I))**2.D0
7663        CONTINUE
7614      CONTINUE
          SCAL(1)=SQRT(SCAL(1))
          SCAL(2)=SQRT(SCAL(2))
          SCAL(3)=SCAL(1)+SCAL(2)         
          DO 2618, KL=1,NDIMSI
            DO 2617 I=1,NBSOCO(KVOINU)
             PQ=LISOCO(KVOINU,I,1)
             LL=KL+NDIM+((PQ-1)*(NDIM+NDIMSI))
             EPSRSS(KL)=EPSRSS(KL)+(1.0D0-SCAL(I)/SCAL(3))*DDLM(LL)
             EPSRSS(KL)=EPSRSS(KL)+(1.0D0-SCAL(I)/SCAL(3))*DDLD(LL)
2617        CONTINUE
2618      CONTINUE      
          CALL DSCAL(NDIMSI-3,1.D0/RAC2,EPSRSS(4),1)
        ENDIF
C
        SIGELL(1)  = EPSRSS(1)
        SIGELL(2)  = EPSRSS(4)
        SIGELL(3)  = EPSRSS(2)
        SIGELL(4)  = EPSRSS(5)
        SIGELL(5)  = EPSRSS(6)
        SIGELL(6)  = EPSRSS(3)
C
        CALL DSPEV('V','U', 3, SIGELL, W, Z, 3, WORK, REUSS)
C
        IF (NINI .EQ. 0) THEN
          DO 5987 I=1,NDIM
            DIRR(I)=Z(I,3)
5987      CONTINUE
        ENDIF
C
        IF (NINT(VIP(2,1)) .EQ. 1) THEN
          IF (W(3) .GT. VIM(4,1)) THEN
            DO 6354, I=1,NPG
              VIP(2,I)=3.D0
              VIP(7,I)=VIP(8,I)
6354        CONTINUE
          ENDIF
        ENDIF
C
        IF (NINT(VIP(2,1)) .EQ. 0) THEN
          IF (W(3) .GT. VIM(3,1)) THEN
            IF (NINT(VIP(8,1)) .GT. ITEAMM) THEN
              ITEAMM=NINT(VIP(8,1))
            ELSE
              ITEAMM=0
              NONO=1.D0
            ENDIF
            DO 6355, I=1,NPG
              VIP(2,I)=2.D0
              VIP(7,I)=VIP(8,I)
6355        CONTINUE
C
          ENDIF
        ENDIF
C
        IF (NINT(VIP(7,1)).EQ.NINT(VIP(8,1))) THEN
C
          NNN=5
C
          IF(NINT(VIP(2,1)).EQ.2)THEN
            BARY(1)=0.D0
            BARY(2)=0.D0
            DO 5612,I=1,NDIM
              BARY(I)=0
              DO 5611,N=1,NNO2
                BARY(I)=BARY(I)+GEOM(I,N)/NNO2
5611          CONTINUE
5612        CONTINUE
          ENDIF
C
          DO 5614 N=1,NNO2
           NFIN=N+1
           IF(NFIN.GT.NNO2) THEN
             NFIN=NFIN-NNO2
           ENDIF
           SCAL(1)=0
           SCAL(2)=0
           SCAL(3)=0
           DO 5663 I=1,NDIM
             SCAL(1)=SCAL(1)+DIRR(I)*(GEOM(I,N)-BARY(I))
             SCAL(2)=SCAL(2)+DIRR(I)*(GEOM(I,NFIN)-BARY(I))
5663       CONTINUE
           SCAL(3)=SCAL(1)*SCAL(2)
           IF (SCAL(3) .LT. 0.D0) THEN
             VRARR(N)=1
           ELSE
             VRARR(N)=0
           ENDIF
5614     CONTINUE
C
         IF(NINT(VIP(2,1)).EQ.2)THEN

           DO 7636 N=1,NNO2
             NFIN=N+1
             IF(NFIN.GT.NNO2) THEN
               NFIN=NFIN-NNO2
             ENDIF
             DIST(N,1)=0.D0
             DIST(N,2)=0.D0
             IF(VRARR(N).EQ.1) THEN
                 SCAL(1)=0.D0
                 SCAL(2)=0.D0
                 SCAL(3)=0.D0
                 DO 7613 I=1,NDIM
                   SCAL(1)=SCAL(1)+DIRR(I)*(GEOM(I,N)-BARY(I))
                   SCAL(2)=SCAL(2)+DIRR(I)*(GEOM(I,NFIN)-BARY(I))
7613             CONTINUE
                 SCAL(1)=SQRT(SCAL(1)**2.D0)
                 SCAL(2)=SQRT(SCAL(2)**2.D0)
                 SCAL(3)=SCAL(1)+SCAL(2)
                 DIST(N,1)=
     &                DIST(N,1)+SCAL(2)*DIRR(2)*(GEOM(1,N)-BARY(1))
                 DIST(N,1)=
     &                DIST(N,1)+SCAL(1)*DIRR(2)*(GEOM(1,NFIN)-BARY(1))
                 DIST(N,1)=
     &                DIST(N,1)-SCAL(2)*DIRR(1)*(GEOM(2,N)-BARY(2))
                 DIST(N,1)=
     &                DIST(N,1)-SCAL(1)*DIRR(1)*(GEOM(2,NFIN)-BARY(2))
                 DIST(N,1)=DIST(N,1)/SCAL(3)
             ENDIF
7636       CONTINUE
C
           DO 5616 N=1,NNO2
             NFIN=N+1
             IF(NFIN.GT.NNO2) THEN
               NFIN=NFIN-NNO2
             ENDIF
             IF (VRARR(N).EQ.1) THEN
               DO 5615 KVOIS=1,NBVOIS
                 NN=0
                 DO 5617 I=1,NBSOCO(KVOIS)
                   IF (LISOCO(KVOIS,I,1).EQ.N) NN=NN+1
                   IF (LISOCO(KVOIS,I,1).EQ.NFIN) NN=NN+1
5617             CONTINUE
                 IF (NN.EQ.2) THEN
                   DO 5618,GG=1,NPG
                     VIP(NNN,GG)=LIVOIS(KVOIS)
                     VIP(2*NNN-1,GG)=BARY(1)+DIST(N,1)*DIRR(2)
                     VIP(2*NNN,GG)=BARY(2)-DIST(N,1)*DIRR(1)
5618               CONTINUE
                   NNN=NNN+1
                 ENDIF
5615           CONTINUE    
             ENDIF
5616       CONTINUE
         ENDIF
C
         IF(NINT(VIP(2,1)).EQ.3)THEN
           COMPAR=0.D0
           DO 5636 N=1,NNO2
             NFIN=N+1
             IF(NFIN.GT.NNO2) THEN
               NFIN=NFIN-NNO2
             ENDIF
             DIST(N,1)=0.D0
             DIST(N,2)=0.D0
             IF(VRARR(N).EQ.1) THEN
                 SCAL(1)=0.D0
                 SCAL(2)=0.D0
                 SCAL(3)=0.D0
                 DO 5613 I=1,NDIM
                   SCAL(1)=SCAL(1)+DIRR(I)*(GEOM(I,N)-BARY(I))
                   SCAL(2)=SCAL(2)+DIRR(I)*(GEOM(I,NFIN)-BARY(I))
5613             CONTINUE
                 SCAL(1)=SQRT(SCAL(1)**2.D0)
                 SCAL(2)=SQRT(SCAL(2)**2.D0)
                 SCAL(3)=SCAL(1)+SCAL(2)
                 DIST(N,1)=
     &              DIST(N,1)+SCAL(2)*DIRR(2)*(GEOM(1,N)-BARY(1))
                 DIST(N,1)=
     &              DIST(N,1)+SCAL(1)*DIRR(2)*(GEOM(1,NFIN)-BARY(1))
                 DIST(N,1)=
     &              DIST(N,1)-SCAL(2)*DIRR(1)*(GEOM(2,N)-BARY(2))
                 DIST(N,1)=
     &              DIST(N,1)-SCAL(1)*DIRR(1)*(GEOM(2,NFIN)-BARY(2))
                 DIST(N,1)=DIST(N,1)/SCAL(3)
                 DIST(N,2)=DIST(N,1)*DIST(N,1)
                 COMPAR=COMPAR+DIST(N,2)/2.D0
             ENDIF
5636       CONTINUE
C
           DO 5637 N=1,NNO2
             IF(DIST(N,2).GT.COMPAR) THEN
               VRARR(N)=1
             ELSE
               VRARR(N)=0
             ENDIF
5637       CONTINUE
           SCAL(1)=0.D0
           SCAL(2)=0.D0
           SCAL(3)=0.D0
           DO 5626 N=1,NNO2
             NFIN=N+1
             IF(NFIN.GT.NNO2) THEN
               NFIN=NFIN-NNO2
             ENDIF
             IF (VRARR(N).EQ.1) THEN
               DO 5625 KVOIS=1,NBVOIS
                 NN=0
                 DO 5627 I=1,NBSOCO(KVOIS)
                   IF (LISOCO(KVOIS,I,1).EQ.N) NN=NN+1
                   IF (LISOCO(KVOIS,I,1).EQ.NFIN) NN=NN+1
5627             CONTINUE
                 IF (NN.EQ.2) THEN
                   DO 5628,GG=1,NPG
                     VIP(5,GG)=LIVOIS(KVOIS)
                     VIP(9,GG)=BARY(1)+DIST(N,1)*DIRR(2)
                     VIP(10,GG)=BARY(2)-DIST(N,1)*DIRR(1)
                     VIP(11,GG)=BARY(1)
                     VIP(12,GG)=BARY(2)
5628               CONTINUE
                   SCAL(1)=BARY(1)+DIST(N,1)*DIRR(2)
                   SCAL(2)=BARY(2)-DIST(N,1)*DIRR(1)
                   NUNU=1
                   NNN=NNN+1
                 ENDIF
5625           CONTINUE    
             ENDIF
5626       CONTINUE
           NINI=0
           IF (NUNU .EQ. 1) THEN
             SCAL(3)=(SCAL(1)-BARY(1))*(BARY(1)-BARYO(1))
             SCAL(3)=SCAL(3)+(SCAL(2)-BARY(2))*(BARY(2)-BARYO(2))
             IF (SCAL(3) .LT. 0.D0) THEN
               SCAL(3)=0.D0
               SCAL(3)=SCAL(3)+(BARYO(2)-BARY(2))**(2.D0)
               SCAL(3)=SCAL(3)+(BARY(1)-BARYO(1))**(2.D0)
               SCAL(3)=SCAL(3)**(0.5D0)        
               DIRR(1)=(BARYO(2)-BARY(2))/SCAL(3)
               DIRR(2)=(BARY(1)-BARYO(1))/SCAL(3)
               NINI=1
             ENDIF
           ENDIF
         
         ENDIF
          
        ENDIF
      ENDIF
C
      IF (NINI .EQ. 1) GOTO 8491
C
C
      IF (RIGI) CALL R8INIR(NDDL*NDDL,0.D0,MATR,1)
      IF (RESI) CALL R8INIR(NDDL,0.D0,VECT,1)
      CALL R8INIR(6,0.D0,SIGMAM,1)
C
C    POSITION DES INDICES POUR LES DEPLACEMENTS ET LES DEFORMATIONS
C
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
          DEPLM(I+(N-1)*NDIM) = DDLM(IU(I,N))
          DEPLD(I+(N-1)*NDIM) = DDLD(IU(I,N))
 110    CONTINUE
 100  CONTINUE
C
C - CALCUL POUR CHAQUE POINT DE GAUSS
C
      DO 1000 G=1,NPG
C
C      CALCUL DES ELEMENTS GEOMETRIQUES DE L'EF POUR E-BARRE
C
        CALL DFDMIP(NDIM,NNO2,AXI,GEOM,G,IW,VFF2(1,G),IDFDE2,R,WG,DFDI2)
        CALL NMEPSB(NDIM,NNO2,AXI,VFF2(1,G),DFDI2,DDLM,EPSGM(1,2),GEPSM)
        CALL NMEPSB(NDIM,NNO2,AXI,VFF2(1,G),DFDI2,DDLD,EPSGD(1,2),GEPS)
C
C      CALCUL DES ELEMENTS GEOMETRIQUES DE L'EF POUR U
C
        CALL DFDMIP(NDIM,NNO1,AXI,GEOM,G,IW,VFF1(1,G),IDFDE1,R,WG,DFDI1)
        CALL NMEPSI(NDIM,NNO1,AXI,GRAND,VFF1(1,G),R,DFDI1,DEPLM,F,EPSGM)
        CALL NMEPSI(NDIM,NNO1,AXI,GRAND,VFF1(1,G),R,DFDI1,DEPLD,F,EPSGD)
        CALL NMMABU(NDIM,NNO1,AXI,GRAND,DFDI1,B)
C
C      DEFORMATIONS ET ECARTS EN FIN DE PAS DE TEMPS
C
        CALL DAXPY(18,1.D0,GEPSM,1,GEPS,1)
        DO 200 KL = 1,NDIMSI
          DE(KL) = EPSGM(KL,2)+EPSGD(KL,2)
 200    CONTINUE
C
C      LOI DE COMPORTEMENT
C
        CALL DCOPY(NDIMSI,SIGM(1,G),1,SIGMAM,1)
        CALL DSCAL(3,RAC2,SIGMAM(4),1)
        
        CALL R8INIR(36,0.D0,P,1)
        P(1,1)=NONO
C
        CALL NMCOMP('RIGI',G,1,NDIM,TYPMOD,MATE,COMPOR,CRIT,INSTAM,
     &          INSTAP,EPSGM,EPSGD,SIGMAM,
     &          VIM(1,G),OPTION,
     &          ANGMAS,P,
     &          SIGMA,VIP(1,G),DSIDEP,COD(G))
        IF(COD(G).EQ.1) GOTO 9000
        
        CALL R8INIR(6,1.D0,P,7)
C
C      FORCE INTERIEURE ET DES CONTRAINTES DE CAUCHY
C        
        IF (RESI) THEN
C
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
C
C        VECTEUR FINT:E
          DO 350 N=1,NNO2
            DO 360 KL = 1,NDIMSI
              KK = IE(KL,N)
              T1 = 0
              DO 365 PQ = 1,NDIMSI
                T1 = T1 + P(KL,PQ)*DE(PQ)*VFF2(N,G)
                T1 = T1 - P(KL,PQ)*SIGMA(PQ)*VFF2(N,G)
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
C
C        CONTRAINTES
          CALL DCOPY(NDIMSI,SIGMA,1,SIGP(1,G),1)
          CALL DSCAL(NDIMSI-3,1.D0/RAC2,SIGP(4,G),1)
C
        END IF
C
C - CALCUL DE LA MATRICE DE RIGIDITE (STOCKAGE LIGNE DE DFI/DUJ)
C
        IF (RIGI) THEN
C
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
                      T1 = T1 + DSIDEP(KL,PQ,1)*B(PQ,J,M)*B(KL,I,N)
 550                CONTINUE
 540              CONTINUE
                  MATR(KK) = MATR(KK) + WG*T1
 530            CONTINUE
 520          CONTINUE
C
C        MATRICE K:U(I,N),E(PQ,M)
              DO 600 M = 1,NNO2
                DO 610 PQ = 1,NDIMSI
                  KK = OS+IE(PQ,M)
                  T1 = 0
                  MATR(KK) = MATR(KK) + WG*T1
 610            CONTINUE
 600          CONTINUE
 510        CONTINUE
 500      CONTINUE
C
C        MATRICE K:E(KL,N),U(J,M)
          DO 700 N = 1,NNO2
            DO 710 KL = 1,NDIMSI
              OS = NDDL*(IE(KL,N)-1)
              DO 720 M = 1,NNO1
                DO 730 J = 1,NDIM
                  KK = OS+IU(J,M)
                  T1 = 0
                  DO 735 PQ = 1,NDIMSI
                    T1=T1 - DSIDEP(KL,PQ,1)*B(PQ,J,M)*VFF2(N,G)
 735              CONTINUE
                  MATR(KK) = MATR(KK) + WG*T1
 730            CONTINUE
 720          CONTINUE
 710        CONTINUE
 700      CONTINUE
C
C        MATRICE K:E(KL,N),E(PQ,M)
C
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
C
        END IF
        
 1000 CONTINUE
C
C - SYNTHESE DES CODES RETOUR
C
 9000 CONTINUE
C
      CALL CODERE(COD,NPG,CODRET)
C
      END

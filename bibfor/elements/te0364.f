       SUBROUTINE TE0364(OPTION,NOMTE)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 23/05/2005   AUTEUR LAMARCHE S.LAMARCHE 
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
C TOLE CRP_20
      IMPLICIT   NONE
      CHARACTER*16 OPTION,NOMTE
C.......................................................................


C  CALCUL CALCUL DES MATRICES DE CONTACT ET DE FROTTEMENT
C  DE COULOMB STANDARD  AVEC LA METHODE CONTINUE (ECP)


C  OPTION : 'RIGI_CONT' (CALCUL DES MATRICES DE CONTACT )
C           'RIGI_FROT' (CALCUL DES MATRICES DE FROTTEMENT STANDARD)

C  ENTREES  ---> OPTION : OPTION DE CALCUL
C           ---> NOMTE  : NOM DU TYPE ELEMENT
C  REMARQUE
C
C......................................................................
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX --------------------
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

C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX --------------------

      INTEGER I,J,K,AXIS
      REAL*8 COEFFF,COEFFA,COEFCA

      INTEGER NNE,NNM,NDDL,NDIM,INDCO,INADH
      INTEGER IGEOM,IDEPL,IMATT,JPCF
      INTEGER MGEOM,MDEPL,IDEPM,IJ,L,MALEK,INDNOR,IFORM

      REAL*8 XC,YC,PDS,ERR,ESS,JAC,JACM,LAMBDA,R(3,3),E(3,3),G(3,3)
      REAL*8 MMAT(81,81),C(3,3),FFE(9),FFM(9),TAU1(3),TAU2(3),NORM(3)
      REAL*8 C1(3),C2(3),C3(3),H1(3),H2(3),H(3,3),A(3,3),B(3,3)
      REAL*8 GEOMM(3),GEOME(3),DEPLE(6),D1(3),D2(3),D3(3),D(3,3),M(3,3)
      REAL*8 DEPLM(6),RESE(3),F(3,3),MCH(56,56),MPR(56,56),MM1(3,3)
      REAL*8 DEPLEE(6),NOOR,TT(3,3),GAUS,ORD,DT

      CHARACTER*8 ESC,MAIT

C......................................................................

      CALL JEMARQ()
C       
      IF (NOMTE.EQ.'CFS2S2') THEN
        ESC = 'SG2'
        NNM = 2
        NNE = 2
        NDDL = 12
        NDIM = 2
        MAIT = 'SG2'
      ELSE IF (NOMTE.EQ.'CFS2S3') THEN
        ESC = 'SG2'
        NNM = 3
        NNE = 2
        NDDL = 14
        NDIM = 2
        MAIT = 'SG3'
      ELSE IF (NOMTE.EQ.'CFS3S2') THEN
        ESC = 'SG3'
        NNM = 2
        NNE = 3
        NDDL = 16
        NDIM = 2
        MAIT = 'SG2'
      ELSE IF (NOMTE.EQ.'CFS3S3') THEN
        ESC = 'SG3'
        NNM = 3
        NNE = 3
        NDDL = 18
        NDIM = 2
        MAIT = 'SG3'
      ELSE IF (NOMTE.EQ.'CFT3T3') THEN
        ESC = 'TR3'
        NNM = 3
        NNE = 3
        NDDL = 27
        NDIM = 3
        MAIT = 'TR3'
      ELSE IF (NOMTE.EQ.'CFT3T6') THEN
        ESC = 'TR3'
        NNM = 6
        NNE = 3
        NDDL = 36
        NDIM = 3
        MAIT = 'TR6'
      ELSE IF (NOMTE.EQ.'CFT6T3') THEN
        ESC = 'TR6'
        NNM = 3
        NNE = 6
        NDDL = 45
        NDIM = 3
        MAIT = 'TR3'
      ELSE IF (NOMTE.EQ.'CFT6T6') THEN
        ESC = 'TR6'
        NNM = 6
        NNE = 6
        NDDL = 54
        NDIM = 3
        MAIT = 'TR6'
      ELSE IF (NOMTE.EQ.'CFQ4Q4') THEN
        ESC = 'QU4'
        NNM = 4
        NNE = 4
        NDDL = 36
        NDIM = 3
        MAIT = 'QU4'
      ELSE IF (NOMTE.EQ.'CFQ4Q8') THEN
        ESC = 'QU4'
        NNM = 8
        NNE = 4
        NDDL = 48
        NDIM = 3
        MAIT = 'QU8'
      ELSE IF (NOMTE.EQ.'CFQ8Q4') THEN
        ESC = 'QU8'
        NNM = 4
        NNE = 8
        NDDL = 60
        NDIM = 3
        MAIT = 'QU4'
      ELSE IF (NOMTE.EQ.'CFQ8Q8') THEN
        ESC = 'QU8'
        NNM = 8
        NNE = 8
        NDDL = 72
        NDIM = 3
        MAIT = 'QU8'
      ELSE IF (NOMTE.EQ.'CFQ8T6') THEN
        ESC = 'QU8'
        NNM = 6
        NNE = 8
        NDDL = 66
        NDIM = 3
        MAIT = 'TR6'
      ELSE IF (NOMTE.EQ.'CFT6Q8') THEN
        ESC = 'TR6'
        NNM = 8
        NNE = 6
        NDDL = 60
        NDIM = 3
        MAIT = 'QU8'
      ELSE IF (NOMTE.EQ.'CFQ4T3') THEN
        ESC = 'QU4'
        NNM = 3
        NNE = 4
        NDDL = 33
        NDIM = 3
        MAIT = 'TR3'
      ELSE IF (NOMTE.EQ.'CFT3Q4') THEN
        ESC = 'TR3'
        NNM = 4
        NNE = 3
        NDDL = 30
        NDIM = 3
        MAIT = 'QU4'
      ELSE IF (NOMTE.EQ.'CFQ9Q9') THEN
        ESC = 'QU9'
        NNM = 9
        NNE = 9
        NDDL = 81
        NDIM = 3
        MAIT = 'QU9'
      ELSE
        CALL UTMESS('F','TE0364', 'NOM DE L ELEMENT INCONNU')
      END IF

C  RECUPERATION DES DONNEES

      CALL JEVECH('PCONFR','L',JPCF)

      ORD =ZR(JPCF-1+1)
      ERR = ZR(JPCF-1+2)
      ESS = ZR(JPCF-1+3)
      TAU1(1) = ZR(JPCF-1+4)
      TAU1(2) = ZR(JPCF-1+5)
      TAU1(3) = ZR(JPCF-1+6)
      TAU2(1) = ZR(JPCF-1+7)
      TAU2(2) = ZR(JPCF-1+8)
      TAU2(3) = ZR(JPCF-1+9)
      GAUS    = ZR(JPCF-1+10)
      INDCO   = NINT(ZR(JPCF-1+11))
      LAMBDA  = ZR(JPCF-1+12)
      COEFCA  = ZR(JPCF-1+13)
      COEFFA  = ZR(JPCF-1+14)
      COEFFF  = ZR(JPCF-1+15)
      MALEK   = NINT(ZR(JPCF-1+16))
      INDNOR  = NINT(ZR(JPCF-1+17))
      AXIS    = NINT(ZR(JPCF-1+18))
      PDS     = ZR(JPCF-1+19)
      DT      = ZR(JPCF-1+20)
      IFORM   = NINT(ZR(JPCF-1+21))          
      INDNOR  = 1
      IF (INDNOR.EQ.1) GOTO 99
99     CONTINUE
C  RECUPERARTION DE LA GEOMETRIE ET DE CHAMPS DE DEPLACEMENT

      CALL JEVECH('PGEOMER','E',IGEOM)
      CALL JEVECH('PDEPL_P','L',IDEPL)
      CALL JEVECH('PDEPL_M','L',IDEPM)
      CALL JEVECH('PMATUUR','E',IMATT)

C     REACTUALISATION DE LA GEOMETRIE AVEC DEPMOI (ESCALVE)

      DO 20 I = 1,NNE
        DO 10 J = 1,NDIM
          ZR(IGEOM+(I-1)*NDIM+J-1) = ZR(IGEOM+(I-1)*NDIM+J-1) +
     &                               ZR(IDEPM+(I-1)*(2*NDIM)+J-1)
   10   CONTINUE
   20 CONTINUE

      DO 21 I = 1,NNM
         DO 11 J = 1,NDIM
           ZR(IGEOM+NNE*NDIM+(I-1)*NDIM+J-1)=
     &     ZR(IGEOM+NNE*NDIM+(I-1)*NDIM+J-1)+
     &           ZR(IDEPM+NNE*(2*NDIM)+(I-1)*NDIM+J-1)
   11    CONTINUE
   21 CONTINUE

C INITIALISATION DE LA MATRICE

      DO 40 I = 1,NDDL
        DO 30 J = 1,NDDL
          MMAT(I,J) = 0.D0
   30   CONTINUE
   40 CONTINUE

C   RECUPERATION DES COOR DE PC, JAC ET  FFE
C   ----------------------------------------

      CALL CALFFJ(ESC,ORD,GAUS,IGEOM,FFE,JAC,AXIS)

C   FF DE LA MAILLE MAITRE AU VIS AVIS

      MGEOM = IGEOM + NNE*NDIM

      CALL CALFFJ(MAIT,ERR,ESS,MGEOM,FFM,JACM,AXIS)

C CALCUL DE LA NORMALE

C        IF (INDNOR.EQ.0) THEN
         IF (NDIM.EQ.2) THEN
          NORM(1) =-TAU1(2)
          NORM(2) =TAU1(1)
          NORM(3) =0.D0
         ELSE IF (NDIM.EQ.3) THEN
          CALL PROVEC(TAU2,TAU1,NORM)
         END IF
C        ELSE IF (INDNOR.EQ.1) THEN
C          IF (NDIM.EQ.2) THEN
C           NORM(1) =TAU1(2)
C           NORM(2) =-TAU1(1)
C           NORM(3) =0.D0
C         ELSE IF (NDIM.EQ.3) THEN
C           CALL PROVEC(TAU2,TAU1,NORM)
C          END IF
C        END IF
        NOOR = 0.D0
      DO 1 I=1,NDIM
        NOOR = NORM(I)*NORM(I)+NOOR
1     CONTINUE
      DO 2 I=1,3
        NORM(I) = NORM(I)/SQRT(NOOR)
2     CONTINUE

C  CALCUL DU DEPLACEMENT ET LA GEOMETRIE DU PC ET SON VISAVIS
C
      DO 211 I = 1,NDIM
        GEOME(I) = 0.D0
        DO 112 J = 1,NNE
          GEOME(I) = GEOME(I) +FFE(J)*ZR(IGEOM+(J-1)*NDIM+I-1)
112     CONTINUE
211   CONTINUE

C  -----
       DO 212 I = 1,NDIM
         GEOMM(I) = 0.D0
         DO 113 J = 1,NNM
          GEOMM(I) = GEOMM(I) +FFM(J)*ZR(IGEOM+NNE*NDIM+
     &                                        (J-1)*NDIM+I-1)
113     CONTINUE
212    CONTINUE

C  -----

      DO 213 I = 1,2*NDIM
        DEPLE(I)=0.D0
        DO 114 J = 1,NNE
          DEPLE(I) = DEPLE(I) +FFE(J)*ZR(IDEPL+(J-1)*
     &                              (2*NDIM)+I-1)
114     CONTINUE
213   CONTINUE

C  -----

      DO 214 I = 1,NDIM
        DEPLM(I)=0.D0
        DO 117 J = 1,NNM
        DEPLM(I) = DEPLM(I)+FFM(J)*ZR(IDEPL+NNE*(2*NDIM)+(J-1)*
     &                                              NDIM+I-1)
117     CONTINUE
214   CONTINUE

C  -----


C  CALCUL DES MATRICES DE CONTACT
C  ..............................

      IF (OPTION.EQ.'RIGI_CONT') THEN

C POUR LA FORMULATION EN VITESSE
        IF (IFORM .EQ. 2) THEN
           COEFCA= COEFCA/DT
        ENDIF
 
C
C  --- PAS DE CONTACT

        IF (INDCO.EQ.0) THEN

          DO 60 I = 1,NNE
            DO 50 J = 1,NNE
              MMAT((2*NDIM)*(I-1)+NDIM+1,
     &        (J-1)*(2*NDIM)+NDIM+1) = -PDS*FFE(J)*FFE(I)*JAC/COEFCA
 50         CONTINUE
 60       CONTINUE

          GO TO 740
C
C  --- CONTACT

        ELSE IF (INDCO.EQ.1) THEN


C    1. CALCUL DE A ET DE AT
C    -----------------------

C    1.1 PREMIERE PARTIE DE A ET AT : PARTIE ESCLAVE ESCLAVE
C    ----------------------------------------------------------


          DO 90 I = 1,NNE
            DO 80 J = 1,NNE
              DO 70 K = 1,NDIM
                MMAT((2*NDIM)*(I-1)+NDIM+1,(2*NDIM)*(J-1)+K)=-PDS*
     &            FFE(I)*FFE(J)*JAC*NORM(K)
                MMAT((2*NDIM)* (J-1)+K,(2*NDIM)*(I-1)+NDIM+
     &            1) = MMAT((2*NDIM)*(I-1)+NDIM+1,(2*NDIM)*(J-1)+K)
   70         CONTINUE
   80       CONTINUE
   90     CONTINUE

C    1.2 DEUXIEME PARTIE DE A ET AT : PARTIE ESCLAVE MAITRE
C    ------------------------------------------------------

         DO 120 I = 1,NNE
            DO 110 J = 1,NNM
              DO 100 K = 1,NDIM
                MMAT((2*NDIM)*(I-1)+NDIM+1,
     &            (2*NDIM)*NNE+NDIM*(J-1)+K)=PDS*FFE(I)*FFM(J)*JAC*
     &            NORM(K)
                MMAT((2*NDIM)*NNE+NDIM*(J-1)+K,
     &            (2*NDIM)*(I-1)+NDIM+1)=MMAT((2*NDIM)*(I-1)+NDIM+1,
     &             (2*NDIM)*NNE+NDIM*(J-1)+K)
  100         CONTINUE
  110       CONTINUE
  120     CONTINUE
C
C    2.CALCUL DE A_U
C    ---------------
C    2.1 PREMIER BLOC DE LA MATRICE [AU]: PARTIE ESCLAVE ESCLAVE
C    -----------------------------------------------------------

          DO 160 I = 1,NNE
            DO 150 J = 1,NNE
              DO 140 K = 1,NDIM
                DO 130 L = 1,NDIM
               MMAT((2*NDIM)*(I-1)+L,(2*NDIM)*(J-1)+K) = COEFCA*
     &              PDS*FFE(I)*NORM(L)*FFE(J)*JAC*NORM(K)
  130           CONTINUE
  140         CONTINUE
  150       CONTINUE
  160     CONTINUE

C    2.2 DEUXIEME BLOC DE LA MATRICE [AU] PARTIE ESCLAVE MAITRE
C    --------------------------------------------------------
          DO 200 I = 1,NNE
            DO 190 J = 1,NNM
              DO 180 K = 1,NDIM
                DO 170 L = 1,NDIM
                  MMAT((2*NDIM)*(I-1)+L,(2*NDIM)*NNE+NDIM*(J-1)+
     &          K)=-COEFCA*PDS*FFE(I)*NORM(L)*FFM(J)*JAC*NORM(K)
  170           CONTINUE
  180         CONTINUE
  190       CONTINUE
  200     CONTINUE

C   2.3 TROISIEME BLOC DE LA MATRICE AU PARTIE MAITRE ESCLAVE
C   ---------------------------------------------------------
          DO 240 I = 1,NNM
            DO 230 J = 1,NNE
              DO 220 K = 1,NDIM
                DO 210 L = 1,NDIM
                  MMAT((2*NDIM)*NNE+NDIM*(I-1)+L,
     &              (2*NDIM)*(J-1)+K) = -COEFCA*PDS*FFM(I)*NORM(L)*
     &              FFE(J)*JAC*NORM(K)
  210           CONTINUE
  220         CONTINUE
  230       CONTINUE
  240     CONTINUE

C   2.4 QUATRIEME BLOC DE LA MATRICE AU PARTIE MAITRE MAITRE
C   --------------------------------------------------------
          DO 280 I = 1,NNM
            DO 270 J = 1,NNM
              DO 260 K = 1,NDIM
                DO 250 L = 1,NDIM
                  MMAT((2*NDIM)*NNE+NDIM*(I-1)+L,
     &              (2*NDIM)*NNE+NDIM*(J-1)+K) = COEFCA*PDS*FFM(I)*
     &              NORM(L)*FFM(J)*JAC*NORM(K)
  250           CONTINUE
  260         CONTINUE
  270       CONTINUE
  280     CONTINUE
        ELSE
          CALL JXABOR()
        END IF

      ELSE IF (OPTION.EQ.'RIGI_FROT') THEN

         IF (COEFFF.EQ.0.D0) INDCO = 0
         IF (LAMBDA.EQ.0.D0) INDCO = 0
         IF (MALEK.NE.3)     INDCO = 0

C    PAS DE CONTACT
C    --------------

         IF (INDCO.EQ.0) THEN
C
       DO 304 I = 1,3
          DO 305 J = 1,3
             TT(I,J) = 0.D00
 305      CONTINUE
 304   CONTINUE

C
                DO 301 K = 1,NDIM
                  TT(1,1) = TAU1(K)*TAU1(K) + TT(1,1)
                  TT(1,2) = TAU1(K)*TAU2(K) + TT(1,2)
                  TT(2,1) = TAU2(K)*TAU1(K) + TT(2,1)
                  TT(2,2) = TAU2(K)*TAU2(K) + TT(2,2)
 301           CONTINUE


          DO 284 I = 1,NNE
            DO 283 J = 1,NNE
               DO 282 L = 1,NDIM-1
                 DO 281 K = 1,NDIM-1
               MMAT((2*NDIM)*(I-1)+NDIM+1+L,
     &        (J-1)*(2*NDIM)+NDIM+1+K) = -PDS*FFE(I)*FFE(J)*JAC*TT(L,K)
 281             CONTINUE
 282           CONTINUE
 283        CONTINUE
 284      CONTINUE
         
C 

          GO TO 740

C    CONTACT
C    -------

         ELSE IF (INDCO.EQ.1) THEN
C

          DO 300 I = 1,3
            DO 290 J = 1,3
              A(I,J) = 0.D00
              B(I,J) = 0.D00
              C(I,J) = 0.D00
              D(I,J) = 0.D00
              E(I,J) = 0.D00
              G(I,J) = 0.D00
              H(I,J) = 0.D00
              R(I,J) = 0.D00
  290       CONTINUE
  300     CONTINUE

C  0.1  CALCUL DES MATRICES NECESSIRES
C  ...................................


          DO 320 I = 1,NDIM
              DO 310 J = 1,NDIM
              C(I,J) = -1.D0*NORM(I)*NORM(J)
  310         CONTINUE
  320     CONTINUE

          DO 330 I = 1,NDIM
              C(I,I) = 1 +C(I,I)
  330     CONTINUE

          DO 3 K = 1,3
              C1(K) = C(K,1)
              C2(K) = C(K,2)
              C3(K) = C(K,3)
3      CONTINUE

C  E : C.C

            DO 360 I = 1,NDIM
              DO 350 J = 1,NDIM
                DO 340 K = 1,NDIM
                  E(I,J) = C(K,I)*C(K,J) + E(I,J)
  340           CONTINUE
  350         CONTINUE
  360       CONTINUE

C  A : T.C
            DO 4  I = 1,NDIM
              DO 5  K = 1,NDIM
                  A(1,I) = TAU1(K)*C(K,I) + A(1,I)
  5           CONTINUE
  4         CONTINUE
            DO 6  I = 1,NDIM
              DO 7  K = 1,NDIM
                  A(2,I) = TAU2(K)*C(K,I) + A(2,I)
  7           CONTINUE
  6         CONTINUE


C  0.2 ON TEST L'ETAT D'ADHERENCE DU POINT PC
C  ..........................................

          CALL TTPRSM(NDIM,C,DEPLE,DEPLM,COEFFA,INADH,
     &                  RESE,TAU1,TAU2)

C     ADHERENCE
C     .........



         IF (INADH.EQ.1) THEN

C 0.1 ON CALCULE LES MATICES B ET BT
C ----------------------------------

C CALCUL DE B (1ER BLOC) ESCLAVE - ESCLAVE ET SA TRANSPOSEE
C ---------------------------------------------------------
      DO 361 I=1,NNE
       DO 362 J=1,NNE
         DO 363 L=1,NDIM-1
          DO 364 K=1,NDIM
        MMAT((2*NDIM)*(I-1)+NDIM+1+L,(J-1)*(2*NDIM)+K)=
     +         -1.D0*LAMBDA*COEFFF*PDS*FFE(I)*FFE(J)*JAC*A(L,K)
        MMAT((J-1)*(2*NDIM)+K,(2*NDIM)*(I-1)+NDIM+1+L)=
     +         MMAT((2*NDIM)*(I-1)+NDIM+1+L,(J-1)*(2*NDIM)+K)
 364      CONTINUE
 363     CONTINUE
 362    CONTINUE
 361   CONTINUE

C CALCUL DE B (SECOND BLOC) ESCLAVE - MAITRE ET SA TRANSPOSEE
C -----------------------------------------------------------
      DO 365 I=1,NNE
       DO 366 J=1,NNM
         DO 367 L=1,NDIM-1
          DO 368 K=1,NDIM
       MMAT((2*NDIM)*(I-1)+NDIM+1+L,2*NDIM*NNE+(J-1)*NDIM+K)=
     +         LAMBDA*COEFFF* PDS*FFE(I)*FFM(J)*JAC*A(L,K)
       MMAT(2*NDIM*NNE+(J-1)*NDIM+K,(2*NDIM)*(I-1)+NDIM+1+L)=
     +       MMAT((2*NDIM)*(I-1)+NDIM+1+L,2*NDIM*NNE+(J-1)*NDIM+K)
 368      CONTINUE
 367     CONTINUE
 366    CONTINUE
 365   CONTINUE

C  1.2 ON CALCULE LA MATRICE B_U (1ER BLOC E-E)
C  -------------------------------------------

            DO 400 I = 1,NNE
              DO 390 J = 1,NNE
                DO 380 L = 1,NDIM
                  DO 370 K = 1,NDIM
                MMAT((2*NDIM)*(I-1)+L,(J-1)*(2*NDIM)+K)=-COEFFA*
     &          COEFFF*PDS*LAMBDA*FFE(I)*FFE(J)*JAC*E(L,K)
  370             CONTINUE
  380           CONTINUE
  390         CONTINUE
  400       CONTINUE

C  1.3 ON CALCULE LE SECOND BLOC DE B_U (E-M)
C  ------------------------------------------

            DO 440 I = 1,NNE
              DO 430 J = 1,NNM
                DO 420 L = 1,NDIM
                  DO 410 K = 1,NDIM
                    MMAT((2*NDIM)*(I-1)+L,(2*NDIM)*NNE+(J-1)*NDIM+
     &                K) = COEFFA*COEFFF*LAMBDA*PDS*FFE(I)*FFM(J)*JAC*
     &                     E(L,K)
  410             CONTINUE
  420           CONTINUE
  430         CONTINUE
  440       CONTINUE

C   1.4 ON CALCULE LE TROISIEME  BLOC DE B_U (M-E)
C   ---------------------------------------------

            DO 480 I = 1,NNM
              DO 470 J = 1,NNE
                DO 460 L = 1,NDIM
                  DO 450 K = 1,NDIM
                    MMAT((2*NDIM)*NNE+NDIM*(I-1)+L,
     &              (2*NDIM)*(J-1)+K) = COEFFA*COEFFF*LAMBDA*PDS*
     &                                   FFM(I)*FFE(J)*JAC*E(L,K)
  450             CONTINUE
  460           CONTINUE
  470         CONTINUE
  480       CONTINUE


C   1.5 ON CALCULE LE QUATRIEME  BLOC DE B_U (M-M)
C   ---------------------------------------------

            DO 520 I = 1,NNM
              DO 510 J = 1,NNM
                DO 500 L = 1,NDIM
                  DO 490 K = 1,NDIM
                    MMAT(NNE*(2*NDIM)+NDIM*(I-1)+L,
     &               NNE*(2*NDIM)+NDIM*(J-1)+K)=-COEFFA*COEFFF*
     &                LAMBDA*PDS*FFM(I)*FFM(J)*JAC*E(L,K)
  490             CONTINUE
  500           CONTINUE
  510         CONTINUE
  520       CONTINUE

C     GLISSEMENT
C     ..........

         ELSE IF (INADH.EQ.0) THEN


C   2.1 CALCUL  DE IK(/\,U)
C   -----------------------

            CALL MKKVEC(RESE,NDIM,C1,D1)
            CALL MKKVEC(RESE,NDIM,C2,D2)
            CALL MKKVEC(RESE,NDIM,C3,D3)

            DO 12 K = 1,3
              G(K,1) = D1(K)
              G(K,2) = D2(K)
              G(K,3) = D3(K)
12          CONTINUE

C      D:K(-)C.C

           DO 13 I = 1,NDIM
              DO 14 J = 1,NDIM
                DO 15 K = 1,NDIM
                  D(I,J) = G(K,I)*C(K,J) + D(I,J)
  15           CONTINUE
  14         CONTINUE
  13       CONTINUE

            CALL MKKVEC(RESE,NDIM,TAU1,H1)
            CALL MKKVEC(RESE,NDIM,TAU2,H2)

            DO 16 K = 1,3
              H(K,1) = H1(K)
              H(K,2) = H2(K)
 16       CONTINUE

C      B:K(-)T.C

           DO  23 I = 1,NDIM-1
              DO 24 J = 1,NDIM
                DO 25  K = 1,NDIM
                  B(I,J) = H(K,I)*C(K,J)+B(I,J)
  25           CONTINUE
  24         CONTINUE
  23      CONTINUE

C 0.1 ON CALCULE LES MATICES B ET BT
C ----------------------------------

C CALCUL DE B (1ER BLOC) ESCLAVE - ESCLAVE ET SA TRANSPOSEE
C ---------------------------------------------------------
      DO 661 I=1,NNE
       DO 662 J=1,NNE
         DO 663 L=1,NDIM-1
          DO 664 K=1,NDIM
        MMAT((2*NDIM)*(I-1)+NDIM+1+L,(J-1)*(2*NDIM)+K)=
     +         - 1.D0*LAMBDA*COEFFF*PDS*FFE(I)*FFE(J)*JAC*B(L,K)
        MMAT((J-1)*(2*NDIM)+K,(2*NDIM)*(I-1)+NDIM+1+L)=
     +             MMAT((2*NDIM)*(I-1)+NDIM+1+L,(J-1)*(2*NDIM)+K)

 664      CONTINUE
 663     CONTINUE
 662    CONTINUE
 661   CONTINUE

C CALCUL DE B (SECOND BLOC) ESCLAVE - MAITRE ET SA TRANSPOSEE
C -----------------------------------------------------------
      DO 665 I=1,NNE
       DO 666 J=1,NNM
         DO 667 L=1,NDIM-1
          DO 668 K=1,NDIM
       MMAT((2*NDIM)*(I-1)+NDIM+1+L,2*NDIM*NNE+(J-1)*NDIM+K)=
     +         LAMBDA*COEFFF* PDS*FFE(I)*FFM(J)*JAC*B(L,K)
       MMAT(2*NDIM*NNE+(J-1)*NDIM+K,(2*NDIM)*(I-1)+NDIM+1+L)=
     +     MMAT((2*NDIM)*(I-1)+NDIM+1+L,2*NDIM*NNE+(J-1)*NDIM+K)
 668      CONTINUE
 667     CONTINUE
 666    CONTINUE
 665   CONTINUE

C  1.2 ON CALCULE LA MATRICE B_U (1ER BLOC E-E)
C  -------------------------------------------

            DO 700 I = 1,NNE
              DO 690 J = 1,NNE
                DO 680 L = 1,NDIM
                  DO 670 K = 1,NDIM
                    MMAT((2*NDIM)*(I-1)+L,(J-1)*(2*NDIM)+K) = - COEFFA*
     &                COEFFF*PDS*LAMBDA*FFE(I)*FFE(J)*JAC*D(L,K)
  670             CONTINUE
  680           CONTINUE
  690         CONTINUE
  700       CONTINUE

C  1.3 ON CALCULE LE SECOND BLOC DE B_U (E-M)
C  ------------------------------------------

            DO 940 I = 1,NNE
              DO 930 J = 1,NNM
                DO 920 L = 1,NDIM
                  DO 910 K = 1,NDIM
                    MMAT((2*NDIM)*(I-1)+L,(2*NDIM)*NNE+(J-1)*NDIM+
     &                K) = COEFFA*COEFFF*LAMBDA*PDS*FFE(I)*FFM(J)*JAC*
     &                     D(L,K)
  910             CONTINUE
  920           CONTINUE
  930         CONTINUE
  940       CONTINUE

C   1.4 ON CALCULE LE TROISIEME  BLOC DE B_U (M-E)
C   ---------------------------------------------

            DO 980 I = 1,NNM
              DO 970 J = 1,NNE
                DO 960 L = 1,NDIM
                  DO 950 K = 1,NDIM
                    MMAT((2*NDIM)*NNE+NDIM*(I-1)+L,
     &                (2*NDIM)*(J-1)+K) = COEFFA*COEFFF*LAMBDA*PDS*
     &                           FFM(I)*FFE(J)*JAC*D(L,K)
  950             CONTINUE
  960           CONTINUE
  970         CONTINUE
  980       CONTINUE


C   1.5 ON CALCULE LE QUATRIEME  BLOC DE B_U (M-M)
C   ---------------------------------------------

            DO 820 I = 1,NNM
              DO 810 J = 1,NNM
                DO 800 L = 1,NDIM
                  DO 890 K = 1,NDIM
                    MMAT(NNE*(2*NDIM)+NDIM*(I-1)+L,
     &               NNE*(2*NDIM)+NDIM*(J-1)+K)=-COEFFA*COEFFF*
     &                LAMBDA*PDS*FFM(I)*FFM(J)*JAC*D(L,K)
  890             CONTINUE
  800           CONTINUE
  810         CONTINUE
  820       CONTINUE

C
C  ATTENTION F EST CALCULEE SAUF S'IL YA  GLISSEMENT
C

C   R(I,J)= H_I . TAU_J

               DO 857 K = 1,NDIM
                  R(1,1) = (TAU1(K)-H1(K))*TAU1(K)+ R(1,1)
                  R(1,2) = (TAU1(K)-H1(K))*TAU2(K)+ R(1,2)
                  R(2,1) = (TAU2(K)-H2(K))*TAU1(K)+ R(2,1)
                  R(2,2) = (TAU2(K)-H2(K))*TAU2(K)+ R(2,2)
  857           CONTINUE

      DO 831 I=1,NNE
       DO 832 J=1,NNE
         DO 833 L=1,NDIM-1
          DO 834 K=1,NDIM-1
           MMAT(2*NDIM*(I-1)+NDIM+1+L,2*NDIM*(J-1)+NDIM+1+K)=
     +     COEFFF*LAMBDA*PDS*FFE(I)*FFE(J)*JAC*R(L,K)/COEFFA
 834      CONTINUE
 833     CONTINUE
 832    CONTINUE
 831   CONTINUE
      END IF

C    FIN DE IF CONTACT OU NON
        ELSE
          CALL UTMESS('F','TE0364',
     &          'L INDICATEUR DE CONTACT A UNE VALEUR DIFF DE 0 OU 1 ! '
     &                )
        END IF
      ELSE
        CALL UTMESS('F','TE0364','OPTION INCONNUE ')

      END IF

  740 CONTINUE


C FIN DE CHANGEMENT ET COPIE

       DO 760 J = 1,NDDL
         DO 750 I = 1,J
           IJ = (J-1)*J/2 + I
          ZR(IMATT+IJ-1) = MMAT(I,J)
  750   CONTINUE
  760 CONTINUE

      CALL JEDEMA()
      END

      SUBROUTINE TE0012(OPTION,NOMTE)
      IMPLICIT   NONE
C.......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 17/10/2011   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================

C     BUT: CALCUL DES MATRICES DE MASSE ELEMENTAIRES EN MECANIQUE
C          ELEMENTS ISOPARAMETRIQUES 3D

C          OPTION : 'MASS_MECA'
C          OPTION : 'MASS_MECA_DIAG'
C          OPTION : 'MASS_MECA_EXPLI'
C          OPTION : 'M_GAMMA'
C          OPTION : 'ECIN_ELEM'

C     ENTREES  ---> OPTION : OPTION DE CALCUL
C              ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................

C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------

      INTEGER ICODRE

      CHARACTER*16 NOMTE,OPTION,PHENOM
      CHARACTER*8 ELREFE
      CHARACTER*4 FAMI
      CHARACTER*1 STOPZ(3)
      REAL*8 A(3,3,27,27),MATP(81,81),MATV(3321)
      REAL*8 DFDX(27),DFDY(27),DFDZ(27),POIDS,RHO
      INTEGER IPOIDS,IVF,IDFDE,IGEOM,IMATE
      INTEGER JGANO,NNO,KP,I,J,K,IMATUU,IACCE,IVECT
      INTEGER IJKL,IK,L,NDIM,NPG,NDDL,NVEC
      INTEGER N1,N2,I2,J2,K2
      INTEGER IDIAG,NNOS,IRET
      INTEGER IDEPL,IVITE,IECIN,IFREQ
      REAL*8 TRACE,ALFA,WGT,MASVIT(81),MASDEP(81),DDOT
      REAL*8 VECT1(81), VECT2(81)
      INTEGER      MECANI(5),PRESS1(7),PRESS2(7),TEMPE(5),IBI,IDEC
C.......................................................................

      CALL ELREF1(ELREFE)
      IF (ELREFE.EQ.'HE8') THEN
        FAMI = 'RIGI'
      ELSE
        FAMI = 'MASS'
      END IF
      CALL ELREF4(' ',FAMI,NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
      NDDL = 3*NNO
      NVEC = NDDL* (NDDL+1)/2
      PRESS1(1) = 0
      PRESS2(1) = 0
      TEMPE(1) = 0
      CALL GRDTHM(NOMTE,.FALSE.,.FALSE.,3,MECANI,PRESS1,PRESS2,TEMPE,
     &            IBI,IBI,IBI,IBI,IBI,IBI)
      IDEC = PRESS1(1) + PRESS2(1) + TEMPE(1)

      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,ICODRE)

      DO 50 K = 1,3
        DO 40 L = 1,3
          DO 30 I = 1,NNO
            DO 20 J = 1,I
              A(K,L,I,J) = 0.0D0
   20       CONTINUE
   30     CONTINUE
   40   CONTINUE
   50 CONTINUE

C    BOUCLE SUR LES POINTS DE GAUSS

      DO 90 KP = 1,NPG
        L = (KP-1)*NNO
        CALL DFDM3D ( NNO, KP, IPOIDS, IDFDE,
     &                ZR(IGEOM), DFDX, DFDY, DFDZ, POIDS )
        CALL RCVALB(FAMI,KP,1,'+',ZI(IMATE),' ',PHENOM,0,' ',0.D0,
     &              1,'RHO',RHO,ICODRE,1)
        DO 80 I = 1,NNO
          DO 70 J = 1,I
            A(1,1,I,J) = A(1,1,I,J) + RHO*POIDS*ZR(IVF+L+I-1)*
     &                   ZR(IVF+L+J-1)
   70     CONTINUE
   80   CONTINUE
   90 CONTINUE

      DO 110 I = 1,NNO
        DO 100 J = 1,I
          A(2,2,I,J) = A(1,1,I,J)
          A(3,3,I,J) = A(1,1,I,J)
  100   CONTINUE
  110 CONTINUE

      IF (OPTION.EQ.'MASS_MECA') THEN

        CALL JEVECH('PMATUUR','E',IMATUU)
        DO 410 K = 1,NVEC
          MATV(K) = 0.0D0
  410   CONTINUE

C PASSAGE DU STOCKAGE RECTANGULAIRE (A) AU STOCKAGE TRIANGULAIRE (ZR)

        DO 150 K = 1,3
          DO 140 L = 1,3
            DO 130 I = 1,NNO
              IK = ((3*I+K-4)* (3*I+K-3))/2
              DO 120 J = 1,I
                IJKL = IK + 3* (J-1) + L
                MATV(IJKL) = A(K,L,I,J)
  120         CONTINUE
  130       CONTINUE
  140     CONTINUE
  150   CONTINUE
        IF (IDEC.EQ.0) THEN
          DO 400 I = 1 , NVEC
            ZR(IMATUU+I-1) = MATV(I)
 400      CONTINUE
        ELSE
          DO 401 K = 1 , NNO
            DO 402 N1 = 1 , 3
              I = 3*K+N1-3
              IF (K.LE.NNOS) THEN
                I2 = I+IDEC*(K-1)
              ELSE
                I2 = I+IDEC*NNOS
              ENDIF
              DO 403 L = 1 , NNO
                DO 404 N2 = 1 , 3
                  J = 3*L+N2-3
                  IF (J.GT.I) GOTO 405
                  IF (L.LE.NNOS) THEN
                    J2 = J+IDEC*(L-1)
                  ELSE
                    J2 = J+IDEC*NNOS
                  ENDIF
                  ZR(IMATUU+I2*(I2-1)/2+J2-1) = MATV(I*(I-1)/2+J)
 404            CONTINUE
 403          CONTINUE
 405          CONTINUE
 402        CONTINUE
 401      CONTINUE
        ENDIF

      ELSE IF (OPTION.EQ.'MASS_MECA_DIAG' .OR.
     &         OPTION.EQ.'MASS_MECA_EXPLI' ) THEN

        CALL JEVECH('PMATUUR','E',IMATUU)

C-- CALCUL DE LA MASSE DE L'ELEMENT

        WGT = A(1,1,1,1)
        DO 170 I = 2,NNO
          DO 160 J = 1,I - 1
            WGT = WGT + 2*A(1,1,I,J)
  160     CONTINUE
          WGT = WGT + A(1,1,I,I)
  170   CONTINUE

C-- CALCUL DE LA TRACE EN TRANSLATION SUIVANT X

        TRACE = 0.D0
        DO 180 I = 1,NNO
          TRACE = TRACE + A(1,1,I,I)
  180   CONTINUE

C-- CALCUL DU FACTEUR DE DIAGONALISATION

        ALFA = WGT/TRACE

C PASSAGE DU STOCKAGE RECTANGULAIRE (A) AU STOCKAGE TRIANGULAIRE (ZR)

        K = 0
        DO 200 J = 1,NNO
          DO 190 I = 1,3
            K = K + 1
            IF (IDEC.EQ.0) THEN
              IDIAG = K* (K+1)/2
            ELSE
              IF (J.LE.NNOS) THEN
                K2 = K+IDEC*(J-1)
              ELSE
                K2 = K+IDEC*NNOS
              ENDIF
              IDIAG = K2* (K2+1)/2
            ENDIF
            ZR(IMATUU+IDIAG-1) = A(I,I,J,J)*ALFA
  190     CONTINUE
  200   CONTINUE

      ELSE IF (OPTION.EQ.'M_GAMMA') THEN

        CALL JEVECH('PACCELR','L',IACCE)
        CALL JEVECH('PVECTUR','E',IVECT)
        DO 210 K = 1,NVEC
          MATV(K) = 0.0D0
  210   CONTINUE
        DO 250 K = 1,3
          DO 240 L = 1,3
            DO 230 I = 1,NNO
              IK = ((3*I+K-4)* (3*I+K-3))/2
              DO 220 J = 1,I
                IJKL = IK + 3* (J-1) + L
                MATV(IJKL) = A(K,L,I,J)
  220         CONTINUE
  230       CONTINUE
  240     CONTINUE
  250   CONTINUE
        CALL VECMA(MATV,NVEC,MATP,NDDL)
        IF (IDEC.EQ.0) THEN
          CALL PMAVEC('ZERO',NDDL,MATP,ZR(IACCE),ZR(IVECT))
        ELSE
          DO 320 K = 1,NDDL
            VECT1(K) = 0.0D0
            VECT2(K) = 0.0D0
 320      CONTINUE
          DO 311 K = 1 , NNO
            DO 312 N1 = 1 , 3
              I = 3*K+N1-3
              IF (K.LE.NNOS) THEN
                I2 = I+IDEC*(K-1)
              ELSE
                I2 = I+IDEC*NNOS
              ENDIF
              VECT1(I) = ZR(IACCE+I2-1)
 312        CONTINUE
 311      CONTINUE
          CALL PMAVEC('ZERO',NDDL,MATP,VECT1,VECT2)
          DO 313 K = 1 , NNO
            DO 314 N1 = 1 , 3
              I = 3*K+N1-3
              IF (K.LE.NNOS) THEN
                I2 = I+IDEC*(K-1)
              ELSE
                I2 = I+IDEC*NNOS
              ENDIF
              ZR(IVECT+I2-1) = VECT2(I)
 314        CONTINUE
 313      CONTINUE
        ENDIF

C OPTION ECIN_ELEM : CALCUL DE L'ENERGIE CINETIQUE

      ELSE IF (OPTION.EQ.'ECIN_ELEM') THEN
        STOPZ(1)='O'
        STOPZ(2)='N'
        STOPZ(3)='O'
        CALL TECACH(STOPZ,'PVITESR',1,IVITE,IRET)
C IRET NE PEUT VALOIR QUE 0 (TOUT EST OK) OU 2 (CHAMP NON FOURNI)
        IF (IRET.EQ.0) THEN
          CALL JEVECH('PENERCR','E',IECIN)
          DO 260 K = 1,NVEC
            MATV(K) = 0.0D0
  260     CONTINUE
          DO 300 K = 1,3
            DO 290 L = 1,3
              DO 280 I = 1,NNO
                IK = ((3*I+K-4)* (3*I+K-3))/2
                DO 270 J = 1,I
                  IJKL = IK + 3* (J-1) + L
                  MATV(IJKL) = A(K,L,I,J)
  270           CONTINUE
  280         CONTINUE
  290       CONTINUE
  300     CONTINUE
          CALL VECMA(MATV,NVEC,MATP,NDDL)
          CALL PMAVEC('ZERO',NDDL,MATP,ZR(IVITE),MASVIT)
          ZR(IECIN) = .5D0*DDOT(NDDL,ZR(IVITE),1,MASVIT,1)
        ELSE
          CALL TECACH(STOPZ,'PDEPLAR',1,IDEPL,IRET)
          IF (IRET.EQ.0) THEN
            CALL JEVECH('POMEGA2','E',IFREQ)
            CALL JEVECH('PENERCR','E',IECIN)
            DO 261 K = 1,NVEC
              MATV(K) = 0.0D0
  261       CONTINUE
            DO 301 K = 1,3
              DO 291 L = 1,3
                DO 281 I = 1,NNO
                  IK = ((3*I+K-4)* (3*I+K-3))/2
                  DO 271 J = 1,I
                    IJKL = IK + 3* (J-1) + L
                    MATV(IJKL) = A(K,L,I,J)
  271             CONTINUE
  281           CONTINUE
  291         CONTINUE
  301       CONTINUE
            CALL VECMA(MATV,NVEC,MATP,NDDL)
            CALL PMAVEC('ZERO',NDDL,MATP,ZR(IDEPL),MASDEP)
            ZR(IECIN) = .5D0*DDOT(NDDL,ZR(IDEPL),1,MASDEP,1)*ZR(IFREQ)
          ELSE
            CALL U2MESK('F','ELEMENTS2_1',1,OPTION) 
          ENDIF
        ENDIF
C 
      ELSE
CC OPTION DE CALCUL INVALIDE
        CALL ASSERT(.FALSE.)
      END IF

      END

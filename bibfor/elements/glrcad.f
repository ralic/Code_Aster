      SUBROUTINE GLRCAD (DELAS,ALPHA,BETA,GAMMA,K1,K2,
     &                   DMAX1,DMAX2,DAM1,DAM2,CURVCU,
     &                   C1,C2,NBACKN,DEPS,DEPSP,DF,DDISS,DSIDEP)

        IMPLICIT  NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C     CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL,VECTEU,MATRIC,TEMPNO
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

      REAL*8   DELAS(6,6),ALPHA,BETA,GAMMA,K1,K2,DMAX1
     &      ,DMAX2,CURVCU(3),C1(6,6),C2(6,6),DEPS(6)

      REAL*8   DAM1,DAM2,NBACKN(6)

      REAL*8   DEPSP(6),DDISS,DF(6),DSIDEP(6,6)
      REAL*8   DC1(6,6), DC2(6,6), REPS(6), DEPSTE(6),
     &         DDISST, DEPSPT(6),DEPST2(6),ZERODE,
     &         DTG(6,6),CURCUP(3),MP1(3),MP2(3),DCC1(3,3),DCC2(3,3)

      INTEGER NDICHO,NCRIT,NCRIT2,IER,CRITNU,IMP1MX,IMP2MI

      LOGICAL BBOK
      CHARACTER*24    CGLR

C---------------------------------------------
        REAL*8  NMNBN(6), NEWNBN(6)
C         = FORCE - BACKFORCE
        REAL*8  NMPLAS(2,3), NEWPLA(2,3)
C         = PLASMOM(BENDING,_X _Y _XY)
        REAL*8  NMDPLA(2,2), NEWDPL(2,2)
C         = DPLASMOM(BENDING,_X _Y)
        REAL*8  NMDDPL(2,2), NEWDDP(2,2)
C         = DDPLASMOM(BENDING,_X _Y)
        REAL*8  NMZEF, NEWZEF
C         ZERO ADIMENSIONNEL POUR LE CRITERE F
        REAL*8  NMZEG, NEWZEG, NEWZFG(2)
C         ZERO ADIMENSIONNEL POUR LE CRITERE G
        INTEGER NMIEF, NEWIEF
C         IER_FONC > 0 : NBN HORS DE LA ZONE DE
        INTEGER NMPROX(2), NEWPRO(2)
C         PROX>0 : NBN DANS ZONE DE CRITIQUE

C---------------------------------------------

      INTEGER   I, J,KK,KKK
      REAL*8    NORRM6,FPLASS,ZERO,DFP(6),DFP2(6)
      REAL*8    DFF(3,3)
      DATA      ZERO /1.0D-3/

      CALL JEMARQ()

      ZERODE = ZERO * NORRM6(DEPS)

      DO 10, I = 1,6
        NMNBN(I) = NBACKN(I)
 10   CONTINUE

      CALL MPPFFN(NMNBN,NMPLAS,NMDPLA,NMDDPL
     &                 ,NMZEF,NMZEG,NMIEF,NMPROX )
      CALL D0MPFN(NMNBN,NMPLAS,NMDPLA,NMDDPL
     &                 ,NMZEF,NMZEG,NMIEF,NMPROX )
      CALL DDMPFN(NMNBN,NMPLAS,NMDPLA,NMDDPL
     &                 ,NMZEF,NMZEG,NMIEF,NMPROX )

      IF(NMIEF  .GT.  0) THEN
         CALL U2MESS('F','ELEMENTS_85')
      ENDIF

      NDICHO=0
      DO 30, J = 1,6
        DO 20, I = 1,6
          DTG(I,J) = DELAS(I,J)
 20     CONTINUE
 30   CONTINUE
      CALL R8INIR(6,0.0D0,DF,1)
      DDISS=0.D0
      CALL R8INIR(6,0.0D0,DEPSP,1)
      DO 40, I = 1,3
        CURCUP(I) = CURVCU(I)
 40   CONTINUE

C-----------------------------------------------------------------------
C-----   mixed method: to be sure we have f(m,backm)<= 0 at each step
C-----------------------------------------------------------------------
      DO 50, I = 1,6
        REPS(I) = DEPS(I)
 50   CONTINUE

      DO 502, J = 1,6
        DO 501, I = 1,6
          DC1(I,J) = DTG(I,J)+C1(I,J)
          DC2(I,J) = DTG(I,J)+C2(I,J)
 501    CONTINUE
 502  CONTINUE


       DO 229, KK = 1,10000000
        IF (NORRM6(REPS) .LE. ZERODE) THEN
          GOTO 230
        ENDIF
        DO 60, I = 1,6
          DEPSTE(I) = REPS(I)
 60   CONTINUE

      CALL TANMAT(ALPHA,BETA,GAMMA,K1,K2,DMAX1,DMAX2,
     &            DAM1,DAM2,CURCUP,DEPSTE(4),DFF)
      DO 63, J = 1,3
      DO 62, I = 1,3
        DTG(I+3,J+3) = DFF(I,J)
 62   CONTINUE
 63   CONTINUE

      DO 80, J = 1,6
        DO 70, I = 1,6
          DC1(I,J) = DTG(I,J)+C1(I,J)
          DC2(I,J) = DTG(I,J)+C2(I,J)
 70     CONTINUE
 80   CONTINUE

      NCRIT=CRITNU(NMNBN,NMPLAS,NMDPLA,NMDDPL,NMZEF,NMZEG
     &            ,NMIEF,NMPROX,DEPSTE,DTG)

      DO 122, KKK = 1,10000000
        DO 90, J = 1,6
          DEPST2(J) = 0.5D0*DEPSTE(J)
 90     CONTINUE
        NCRIT2=CRITNU(NMNBN,NMPLAS,NMDPLA,NMDDPL,NMZEF
     &               ,NMZEG,NMIEF,NMPROX,DEPST2,DTG)

        NDICHO = NDICHO+1
        IF (NCRIT2 .NE. NCRIT) THEN
          DO 100, J = 1,6
            DEPSTE(J) = DEPST2(J)
 100      CONTINUE
          NCRIT=NCRIT2
        ELSE
          NEWZFG(1) = NEWZEF
          NEWZFG(2) = NEWZEG

          CALL DNDISS (NMNBN,NMPLAS,NMDPLA,NMDDPL,NMPROX,DEPSTE,NCRIT
     &                ,NEWNBN,NEWPLA,NEWDPL,NEWDDP,NEWZFG
     &                ,NEWIEF,NEWPRO,DEPSPT,DDISST,DC1,DC2,DTG,IER)
          NEWZEF = NEWZFG(1)
          NEWZEG = NEWZFG(2)

          IF (NDICHO  .GT.  10000) THEN
            CALL U2MESS('F','ELEMENTS_86')
          ENDIF
          IF(IER .GT. 0) THEN
            DO 110, J = 1,6
              DEPSTE(J) = DEPST2(J)
 110        CONTINUE
                    NCRIT=NCRIT2
          ELSE
C             LE POINT EST DANS LA ZONE G < 0
C             IF THE NEW MOMENT IS OUTSIDE THE YIELDING SURFACE
C             WE TRY TO BRING BACK THE MOMENT TO THE YIELDING SURFACE

            IF (FPLASS(NEWNBN,NEWPLA,1)  .GT.  NEWZEF
     &        .OR. FPLASS(NEWNBN,NEWPLA,2)  .GT.  NEWZEF) THEN


               CALL BRBAGL (NEWNBN,NEWPLA,NEWDPL,NEWDDP,NEWZEF
     &                 ,NEWZEG,NEWIEF,NEWPRO, DEPSPT,
     &                  DDISST, DC1,DC2,DTG,BBOK)
C                       BRING_BACK IS SUCCESSFUL: THE STEP IS VALID

                IF (BBOK) GOTO 123

C                       BRING_BACK IS NOT SUCCESSFUL: DICHOTOMY
                  DO 120, J = 1,6
                    DEPSTE(J) = DEPST2(J)
 120              CONTINUE
                  NCRIT=NCRIT2


C             IF THE NEW MOMENT IS WITHIN THE ELASTIC VOLUME
C             THE STEP IS VALID
                 ELSE
                   GOTO 123
                 ENDIF
             ENDIF
         ENDIF

 122   CONTINUE
 123   CONTINUE
C            THE STEP IS VALID: VARIABLES UPDATED
C------------------------------------------------

       DO 125, J = 1,6
         NMNBN(J)       = NEWNBN(J)
 125   CONTINUE
       DO 140, J = 1,3
         DO 130, I = 1,2
           NMPLAS(I,J)   = NEWPLA(I,J)
 130     CONTINUE
 140   CONTINUE
       DO 160, J = 1,2
         DO 150, I = 1,2
           NMDPLA(I,J)  = NEWDPL(I,J)
           NMDDPL(I,J) = NEWDDP(I,J)
 150     CONTINUE
 160   CONTINUE
       NMZEF    = NEWZEF
       NMZEG    = NEWZEG
       NMIEF  = NEWIEF
       DO 170, J = 1,2
         NMPROX(J) = NEWPRO(J)
 170   CONTINUE

C------------------------------------------------
       DO 180, J = 1,6
         DEPSP(J)  = DEPSP(J) + DEPSPT(J)
 180   CONTINUE
       DDISS = DDISS + DDISST
       DO 190, J = 1,3
           CURCUP(J) = CURCUP(J) + DEPSTE(J+3)
     &                   - DEPSPT(J+3)
 190   CONTINUE

       DO 200, J = 1,6
         DFP2(J) = DEPSTE(J) - DEPSPT(J)
 200   CONTINUE

       CALL MATMUL(DTG,DFP2,6,6,1,DFP)
       DO 210, J = 1,6
         DF(J) = DF(J) + DFP(J)
 210   CONTINUE
       DO 220, J = 1,6
         REPS(J) = REPS(J) - DEPSTE(J)
 220   CONTINUE

 229   CONTINUE
 230   CONTINUE

       DO 240, J = 1,6
         NBACKN(J) = NMNBN(J)
 240   CONTINUE

C ------- MATRICE TANGENTE
      CGLR = '&&GLRC.MP1'
      CALL JEVEUO(CGLR,'L',IMP1MX)
      CGLR = '&&GLRC.MP2'
      CALL JEVEUO(CGLR,'L',IMP2MI)

      DO 250 I=1,2
        MP1(I) = ZR(IMP1MX-1 + I)
        MP2(I) = ZR(IMP2MI-1 + I)
 250  CONTINUE


      DO 270 I=1,3
        DO 260 J=1,3
          DCC1(J,I) = DC1(3+J,3+I)
          DCC2(J,I) = DC2(3+J,3+I)
 260    CONTINUE
 270  CONTINUE

      CALL DCOPY(36,DELAS,1,DSIDEP,1)
      CALL DXKTAN(DTG,MP1,MP2,NBACKN,NCRIT,DCC1,DCC2,DSIDEP)

      CALL JEDEMA()
      END

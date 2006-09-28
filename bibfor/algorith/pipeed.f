      SUBROUTINE PIPEED( MATE     , GEOM     , SIGM     , VIM  ,TYPMOD,
     &                   DDEPL    , DEPLM    , DDEPL0   ,
     &                   DDEPL1   , DTAU     , COPILO   , DFDI ,IPOIDS,
     &                   LGPG     ,NPG       , NNO      , IVF  ,IDFDE,
     &                   COMPOR)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C TOLE CRP_20

      IMPLICIT NONE
      CHARACTER*8 TYPMOD(*)
      CHARACTER*16 COMPOR(4)
      INTEGER MATE,NNO,LGPG,NPG,  IPOIDS, IVF, IDFDE
      REAL*8 GEOM(2,4),VIM(LGPG,NPG),SIGM(4,NPG),DDEPL(2,4),DEPLM(2,4)
      REAL*8 DDEPL0(2,4), DDEPL1(2,4), DTAU, COPILO(5,NPG),DFDI(NNO,2)

C-----------------------------------------------------------------------
C
C BUT : CALCULER LA SOLUTION DE L'EQUATION SUPLEMENTAIRE INTRODUITE POUR
C       LE PILOTAGE DE L'ELEMENT A DISONTINUITE INTERNE.
C
C       L'IDEE EST D'ADAPTER LE CHARGEMENT POUR FAIRE EVOLUER LE SAUT DE
C       L'ELEMENT A DISONTINUITE DE FACON CONTROLEE. PERMET D'EVITER
C       QUE LE SAUT EVOLUE BRUTALEMENT QUAND ON ATTEINT UNE BRANCHE
C       D'EQUILIBRE INSTABLE ET A POUR INTERET DE SUIVRE LA BRANCHE
C       INSTABLE DE LA COURBE GLOBALE U(F).
C
C       LE CHARGEMENT N'EST PLUS MONOTONE, IL DEPEND DU SAUT DE
C       L'ELEMENT.
C
C IN  : GEOM, MATER, VIM, DDEPL, DEPLM, DDEPL0, DDELP1, DTAU,
C OUT : COPILO
C I/O :
C
C-----------------------------------------------------------------------

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

      INTEGER I,J,K,KONT,N,KPG
      REAL*8  UP(8),UD(8),SM,RCK,R,SIGMC,GC,RAC2,KAPPAM,PKM,DET
      REAL*8  ETA(2),POIDS,F(3,3)
      REAL*8  ALFP(2),ALFD(2),DIST
      REAL*8  ALPHA1,BETA1,ALPHA2,BETA2,GAMMA2,DELTA
      REAL*8  ALPHA,BETA,GAMMA
      REAL*8  A0,A1,A2,A3,A4,R8VIDE,SOL(4),MAT(2,2),MATINV(2,2)
      REAL*8  SPG(2),SDG(2),QG(2,2),SP(2),SD(2),Q(2,2),SIGP(2),SIGD(2)
      REAL*8  COTMP,SITMP,CO,SI,ROT(2,2),DROT,XA,XB,YA,YB
      REAL*8  DEF(4,4,2),BUP(6),BUD(6),D(4,2),RTEMP(4,2),VALRES(2)

      CHARACTER*2 CODRET(2)
      CHARACTER*8 NOMRES(2)

      LOGICAL GRAND,AXI

      AXI    = TYPMOD(1) .EQ. 'AXIS'
      GRAND  = .FALSE.

C RECUPERATION DES PARAMETRES PHYSIQUES :
      NOMRES(1) = 'GC'
      NOMRES(2) = 'SIGM_C'

      CALL RCVALA ( MATE,' ','RUPT_FRAG',0,' ',0.D0,2,
     &                 NOMRES,VALRES,CODRET, 'F ' )

      GC     = VALRES(1)
      SIGMC  = VALRES(2)
      RAC2 = SQRT(2.D0)
      KAPPAM = VIM(3,1)

C INITIALISATION DES VARIABLES  :
      CALL R8INIR(2,0.D0,SP,1)
      CALL R8INIR(2,0.D0,SD,1)
      CALL R8INIR(4,0.D0,Q,1)
      CALL R8INIR(2,0.D0,SIGP,1)
      CALL R8INIR(2,0.D0,SIGD,1)
      CALL R8INIR(2,0.D0,ALFP,1)
      CALL R8INIR(2,0.D0,ALFD,1)
      CALL R8INIR(8,0.D0,DFDI,1)
      CALL R8INIR(2,0.D0,ETA,1)
      CALL R8INIR(4,0.D0,SOL,1)
      CALL R8INIR(4,0.D0,MAT,1)
      CALL R8INIR(4,0.D0,MATINV,1)


      CALL DCOPY(8, DEPLM,1,  UP,1)
      CALL DAXPY(8, 1.D0, DDEPL,1,  UP,1)
      CALL DAXPY(8, 1.D0, DDEPL0,1, UP,1)
      CALL DCOPY(8, DDEPL1,1,  UD,1)

C MATRICE DE CHANGEMENT DE REPERE (GLOBAL AU LOCAL) POUR L'ED :
C    ROT X = XLOC

C SOIT A ET B LES MILIEUX DES COTES [14] ET [23]
C t TANGENT AU COTE [AB]

      XA = ( GEOM(1,1) + GEOM(1,4) ) / 2
      YA = ( GEOM(2,1) + GEOM(2,4) ) / 2
      XB = ( GEOM(1,2) + GEOM(1,3) ) / 2
      YB = ( GEOM(2,2) + GEOM(2,3) ) / 2

      COTMP = (YB - YA)
      SITMP = (XA - XB)

      CO = COTMP / SQRT(COTMP*COTMP + SITMP*SITMP)
      SI = SITMP / SQRT(COTMP*COTMP + SITMP*SITMP)

      ROT(1,1) =  CO
      ROT(2,1) = -SI
      ROT(1,2) =  SI
      ROT(2,2) =  CO

C ************************
C CALCUL DE SP, SD ET Q :
C ************************

      DO 800 KPG=1,NPG

        CALL R8INIR(6, 0.D0, BUP,1)
        CALL R8INIR(6, 0.D0, BUD,1)

C       CALCUL DE DFDI,F,R(EN AXI) ET POIDS
        CALL NMGEOM(2,NNO,AXI,GRAND,GEOM,KPG,IPOIDS,IVF,IDFDE,
     &              UP,POIDS,DFDI,F,BUP,R)

C       CALCUL DE D (LES AUTRES TERMES SONT NULS):
        CALL R8INIR(8, 0.D0, D,1)

        D(1,1) = - (DFDI(1,1) + DFDI(2,1))
        D(4,1) = - RAC2*(DFDI(1,2) + DFDI(2,2))/2
        D(2,2) = - (DFDI(1,2) + DFDI(2,2))
        D(4,2) = - RAC2*(DFDI(1,1) + DFDI(2,1))/2

C       ON INCLU LE CHANGEMENT DE REPERE DANS D : ON REMPLACE D PAR DRt
        CALL R8INIR(8, 0.D0, RTEMP,1)

        DO 32 I=1,4
          DO 33 J=1,2
            DROT = 0.D0
            DO 34 K=1,2
              DROT = DROT + D(I,K)*ROT(J,K)
  34        CONTINUE
            RTEMP(I,J) = DROT
  33      CONTINUE
  32    CONTINUE

        DO 35 I=1,4
          DO 36 J=1,2
            D(I,J) = RTEMP(I,J)
  36      CONTINUE
  35    CONTINUE

C       CALCUL DES PRODUITS SYMETR. DE F PAR N,
        CALL R8INIR(32, 0.D0, DEF,1)
        DO 40 N=1,NNO
          DO 30 I=1,2
            DEF(1,N,I) =  F(I,1)*DFDI(N,1)
            DEF(2,N,I) =  F(I,2)*DFDI(N,2)
            DEF(3,N,I) =  0.D0
            DEF(4,N,I) = (F(I,1)*DFDI(N,2) + F(I,2)*DFDI(N,1))/RAC2
 30       CONTINUE
 40     CONTINUE

C       TERME DE CORRECTION (3,3) AXI QUI PORTE EN FAIT SUR LE DDL 1
        IF (AXI) THEN
          DO 50 N=1,NNO
            DEF(3,N,1) = F(3,3)*ZR(IVF+N+(KPG-1)*NNO-1)/R
 50       CONTINUE
        ENDIF

C       CALCUL DE SP,SD ET Q AU POINT DE GAUSS COURANT :
        CALL NMEDPI(SPG,SDG,QG,D,NPG,TYPMOD,MATE,UP,UD,GEOM,NNO,DEF)

C       CALCUL DES S ET Q POUR L'ELEMENT :
        DO 64 I=1,2
          SP(I) = SP(I) + POIDS*SPG(I)
          SD(I) = SD(I) + POIDS*SDG(I)
          DO 65 J=1,2
            Q(I,J) = Q(I,J) + POIDS*QG(I,J)
   65     CONTINUE
   64   CONTINUE


  800 CONTINUE


      DO 66 I=1,2
        SIGP(I) = SIGP(I) + SP(I)
        DO 67 J=1,2
          SIGP(I) = SIGP(I) + Q(I,J)*VIM(J,1)
   67   CONTINUE
        SIGD(I) = SD(I)
   66 CONTINUE

C*****************************************************************
C CALCUL DES COPILO DANS LE CAS OU LE SEUIL EN SAUT EST NUL :
C ****************************************************************
C IL EXISTE DEUX CAS DE FIGURE :
C  INTERSECTION DE DEUX DEMI-DROITES AVEC LE CRITERE EN CONTRAINTE
C ****************************************************************

      IF (KAPPAM.EQ.0.D0) THEN

        RCK = SIGMC
        SM  = DTAU*RCK + RCK

C INTERSECTION DE LA PERMIERE DEMI-DROITE AVEC LE CRITERE EN CONTRAINTE
C ---------------------------------------------------------------------

C ON SUPPOSE QUE  SIGP(1) + ETA*SIGD(1) EST NEGATIF OU NUL
C RESOLUTION DE PM(ETA)  (INTERSECTION DE DROITES)

        KONT = 0

        IF (ABS(SIGD(2)).LE.1.D-12) GOTO 888

        ALPHA1 = SIGD(2)**2
        BETA1  = SIGD(2)*SIGP(2)

        SOL(1) = (   SM  - SIGP(2) ) / SIGD(2)
        SOL(2) = ( - SM  - SIGP(2) ) / SIGD(2)

C       ON TEST SI POUR LES SOL TROUVEES SI ON A BIEN
C       SIGP(1) + ETA*SIGD(1)<=0

        IF ((SIGP(1) + SOL(1)*SIGD(1)).LE.0.D0 ) THEN
          KONT = KONT + 1
          ETA(KONT) = SOL(1)
          A0 = DTAU*RCK - ETA(KONT)*( (ALPHA1*ETA(KONT) + BETA1) / SM)
          A1 = (ALPHA1*ETA(KONT) + BETA1) / SM
        ENDIF
        IF ((SIGP(1) + SOL(2)*SIGD(1)).LE.0.D0 ) THEN
          KONT = KONT + 1
          ETA(KONT) = SOL(2)
          IF (KONT.EQ.1) THEN
            A0 = DTAU*RCK - ETA(KONT)*( (ALPHA1*ETA(KONT) + BETA1) / SM)
            A1 = (ALPHA1*ETA(KONT) + BETA1) / SM
          ELSE
            A2 = DTAU*RCK - ETA(KONT)*( (ALPHA1*ETA(KONT) + BETA1) / SM)
            A3 = (ALPHA1*ETA(KONT) + BETA1) / SM
            A4 = R8VIDE()
            GOTO 999
          ENDIF
        ENDIF

 888    CONTINUE

C INTERSECTION DE LA DEUXIEME DEMI-DROITE AVEC LE CRITERE EN CONTRAINTE
C ---------------------------------------------------------------------

C RESOLUTION DE PP(ETA)  =  ALPHA2*ETA*ETA  +  2*BETA2*ETA  +  GAMMA2
C (INETRSECTION DROITE / DEMI-CERCLE)

        ALPHA2 = SIGD(1)**2 + SIGD(2)**2
        BETA2  = SIGD(1)*SIGP(1) + SIGD(2)*SIGP(2)
        GAMMA2 = SIGP(1)**2 + SIGP(2)**2 - SM**2
        DELTA = BETA2**2 - ALPHA2*GAMMA2

        IF (DELTA .GE. 0.D0) THEN

          SOL(3) = ( - BETA2 + SQRT( DELTA ) ) / (ALPHA2)
          SOL(4) = ( - BETA2 - SQRT( DELTA ) ) / (ALPHA2)

C ON TEST SI POUR LES SOL TROUVEES SI ON A BIEN SIGP(1) + ETA*SIGD(1)>0

          IF ((SIGP(1) + SOL(3)*SIGD(1)).GT.0.D0 ) THEN
            KONT = KONT + 1
            ETA(KONT) = SOL(3)
            IF (KONT.EQ.1) THEN
              A0 = DTAU*RCK - ETA(KONT)*( (ALPHA2*ETA(KONT)+BETA2) / SM)
              A1 = (ALPHA2*ETA(KONT) + BETA2) / SM
            ELSE
              A2 = DTAU*RCK - ETA(KONT)*( (ALPHA2*ETA(KONT)+BETA2) / SM)
              A3 = (ALPHA2*ETA(KONT) + BETA2) / SM
              A4 = R8VIDE()
            ENDIF
          ENDIF

          IF ((SIGP(1) + SOL(4)*SIGD(1)).GT.0.D0 ) THEN
            KONT = KONT + 1
            ETA(KONT) = SOL(4)
            IF (KONT.EQ.1) THEN
              A0 = DTAU*RCK - ETA(KONT)*( (ALPHA2*ETA(KONT)+BETA2) / SM)
              A1 = (ALPHA2*ETA(KONT) + BETA2) / SM
            ELSE
              A2 = DTAU*RCK - ETA(KONT)*( (ALPHA2*ETA(KONT)+BETA2) / SM)
              A3 = (ALPHA2*ETA(KONT) + BETA2) / SM
              A4 = R8VIDE()
            ENDIF
          ENDIF

        ELSE

          IF (KONT.EQ.0) THEN
            A4 = - BETA2/ALPHA2
            A0 = SQRT(ALPHA2*A4*A4 + 2*BETA2*A4 + GAMMA2 + SM**2) - RCK
            A1 = R8VIDE()
            A2 = R8VIDE()
            A3 = R8VIDE()
            GOTO 999
          ENDIF
        ENDIF

        IF (KONT.EQ.1) THEN
          ETA(2) = ETA(1)
          A2 = A0
          A3 = A1
          A4 = R8VIDE()
        ENDIF

        IF (KONT.GE.3)
     &  CALL U2MESS('F','ALGORITH9_85')

C****************************************************************
C CALCUL DES COPILO DANS LE CAS OU LE SEUIL EN SAUT EST NON NUL :
C ***************************************************************
C IL EXISTE DEUX CAS DE FIGURE :
C       -  INTERSECTION DROITE / ARC DE CERCLE
C       -  INTERSECTION DROITE / SEGMENT
C ***************************************************************

      ELSE

        KONT = 0
        RCK = SIGMC*EXP(-SIGMC*KAPPAM/GC)
        PKM = RCK / KAPPAM
        SM = DTAU*KAPPAM + KAPPAM

        MAT(1,1) = PKM - Q(1,1)
        MAT(2,2) = PKM - Q(2,2)
        MAT(1,2) = - Q(1,2)
        MAT(2,1) = - Q(2,1)
        DET  =  MAT(1,1)*MAT(2,2) -  MAT(1,2)**2

        IF ( ABS(DET) .LE. 1.D-12 )
     &  CALL U2MESS('F','ALGORITH9_86')

        MATINV(1,1) =  MAT(2,2)/DET
        MATINV(2,2) =  MAT(1,1)/DET
        MATINV(1,2) = -MAT(1,2)/DET
        MATINV(2,1) = -MAT(2,1)/DET

        DO 68 I=1,2
          DO 69 J=1,2
            ALFP(I) = ALFP(I) + MATINV(I,J)*SP(J)
            ALFD(I) = ALFD(I) + MATINV(I,J)*SD(J)
   69     CONTINUE
   68   CONTINUE

C -------------------------------
C INTERSECTION DROITE / SEGMENT :
C -------------------------------

C       CAS OU PAS D'INTERSECTION
        IF ( ABS(ALFD(1)) .LE. 1.D-12 ) GOTO 777
        SOL(3) = ( - ALFP(1) - KAPPAM*DTAU ) / ALFD(1)
        DIST = KAPPAM * SQRT(1.D0 + 2.D0*DTAU)
        IF (ABS(ALFP(2) + SOL(3)*ALFD(2)) .LT. DIST  ) THEN
          KONT = KONT + 1
          ETA(KONT) =  SOL(3)
          A0 = KAPPAM*DTAU + ETA(KONT)*ALFD(1)
          A1 = - ALFD(1)
        ENDIF

 777    CONTINUE

C -------------------------------------
C INTERSECTION DROITE / ARC DE CERCLE :
C -------------------------------------

        ALPHA = ALFD(1)**2 + ALFD(2)**2
        BETA  = ALFD(1)*ALFP(1) + ALFD(2)*ALFP(2)
        GAMMA = ALFP(1)**2 + ALFP(2)**2 - SM**2
        DELTA = BETA**2 - ALPHA*GAMMA

        IF (DELTA .GE. 0.D0) THEN
C       --------------------------
          SOL(1) = ( - BETA + SQRT( DELTA ) ) / (ALPHA)
          SOL(2) = ( - BETA - SQRT( DELTA ) ) / (ALPHA)

          IF ( (ALFP(1) + SOL(1)*ALFD(1) ).GT. -KAPPAM*DTAU ) THEN
            KONT = KONT + 1
            ETA(KONT) = SOL(1)
            IF (KONT.EQ.1) THEN
              A0 = DTAU*KAPPAM - ETA(KONT)*( (ALPHA*ETA(KONT)+BETA) /SM)
              A1 = (ALPHA*ETA(KONT) + BETA) / SM
            ELSE
              A2 = DTAU*KAPPAM - ETA(KONT)*( (ALPHA*ETA(KONT)+BETA) /SM)
              A3 = (ALPHA*ETA(KONT) + BETA) / SM
              A4 = R8VIDE()
            ENDIF
          ENDIF

          IF ( (ALFP(1) + SOL(2)*ALFD(1) ).GT.-KAPPAM*DTAU ) THEN
            KONT = KONT + 1
            ETA(KONT) = SOL(2)
            IF (KONT.EQ.1) THEN
              A0 = DTAU*KAPPAM - ETA(KONT)*( (ALPHA*ETA(KONT)+BETA) /SM)
              A1 = (ALPHA*ETA(KONT) + BETA) / SM
            ELSE
              A2 = DTAU*KAPPAM - ETA(KONT)*( (ALPHA*ETA(KONT)+BETA) /SM)
              A3 = (ALPHA*ETA(KONT) + BETA) / SM
              A4 = R8VIDE()
            ENDIF
          ENDIF

C     CAS OU LA DROITE EST TANG AU CERCLE MAIS A GAUCHE DE L'AXE  X=DIST
          IF (KONT.EQ.0) THEN
            A4 = - BETA/ALPHA
            A0 = SQRT( ALPHA*A4*A4 + 2*BETA*A4 + GAMMA + SM**2 )- KAPPAM
            A1 = R8VIDE()
            A2 = R8VIDE()
            A3 = R8VIDE()
          ENDIF
        ELSE
C       -----
          IF (KONT.NE.0)
     &    CALL U2MESS('F','ALGORITH9_87')

          A4 = - BETA/ALPHA
          A0 = SQRT( ALPHA*A4*A4 + 2*BETA*A4 + GAMMA + SM**2 ) - KAPPAM
          A1 = R8VIDE()
          A2 = R8VIDE()
          A3 = R8VIDE()

        ENDIF
C       -----

        IF (KONT.GE.3)
     &  CALL U2MESS('F','ALGORITH9_85')

      ENDIF

999   CONTINUE

      IF (KAPPAM.EQ.0.D0) THEN
        IF (KONT.NE.0) THEN
          COPILO(1,1) = A0/RCK
          COPILO(2,1) = A1/RCK
          COPILO(3,1) = A2/RCK
          COPILO(4,1) = A3/RCK
          COPILO(5,1) = A4
        ELSE
          COPILO(1,1) = A0/RCK
          COPILO(2,1) = A1
          COPILO(3,1) = A2
          COPILO(4,1) = A3
          COPILO(5,1) = A4/RCK
        ENDIF
      ELSE
        IF (KONT.NE.0) THEN
          COPILO(1,1) = A0/KAPPAM
          COPILO(2,1) = A1/KAPPAM
          COPILO(3,1) = A2/KAPPAM
          COPILO(4,1) = A3/KAPPAM
          COPILO(5,1) = A4
        ELSE
          COPILO(1,1) = A0/KAPPAM
          COPILO(2,1) = A1
          COPILO(3,1) = A2
          COPILO(4,1) = A3
          COPILO(5,1) = A4/KAPPAM
        ENDIF
      ENDIF

      DO 666 I=2,NPG
        COPILO(1,I) = COPILO(1,1)
        COPILO(2,I) = COPILO(2,1)
        COPILO(3,I) = COPILO(3,1)
        COPILO(4,I) = COPILO(4,1)
        COPILO(5,I) = COPILO(5,1)
666   CONTINUE

      END

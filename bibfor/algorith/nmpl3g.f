       SUBROUTINE  NMPL3G(NNO,NNOB,NPG,IPOIDS,IVF,IVFB,IDFDE,IDFDEB,
     &                    GEOM,TYPMOD,NDIM,
     &                    OPTION,IMATE,COMPOR,LGPG,CRIT,
     &                    INSTAM,INSTAP,
     &                    TM,TP,TREF,
     &                    DEPLGM,DDEPLG,
     &                    ANGMAS,
     &                    SIGM,VIM,
     &                    DFDI,DEF,DFDIB,SIGP,VIP,
     &                    MATRIG,VECTF,CODRET)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 28/08/2006   AUTEUR CIBHHPD L.SALMONA 
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
C TOLE CRP_21

       IMPLICIT NONE
C
       INTEGER       NNO,NNOB,NPG,NDIM,NDIMSI,IMATE, LGPG, CODRET
C
       CHARACTER*2   COD2
       CHARACTER*8   TYPMOD(*)
       CHARACTER*16  OPTION, COMPOR(4)
C
       REAL*8        INSTAM,INSTAP
       REAL*8        DFDIB(NNOB,NDIM)
       REAL*8        GEOM(NDIM,NNO), CRIT(3), TM(NNO),TP(NNO)
       REAL*8        TREF
       REAL*8        DEPLGM(*),DDEPLG(*),DFDI(NNO,NDIM)
       REAL*8        DEF(NDIM*2,NNO,NDIM)
       REAL*8        SIGM(NDIM*2,NPG),SIGP(NDIM*2,NPG)
       REAL*8        VIM(LGPG,NPG),VIP(LGPG,NPG)
       REAL*8        MATRIG((NNOB*3*NDIM+(NNO-NNOB)*NDIM)
     &                    *(NNOB*3*NDIM+(NNO-NNOB)*NDIM))
       REAL*8        VECTF((NDIM*2 + NDIM)*NNOB + NDIM*(NNO-NNOB))
C
       LOGICAL       GRAND, AXI
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER  ZI
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
C.......................................................................
C
C     BUT:  CALCUL  DES OPTIONS RIGI_MECA_TANG, RAPH_MECA ET FULL_MECA
C           EN HYPO-ELASTICITE EN 3D
C           POUR ELEMENTS NON LOCAUX
C.......................................................................
C IN  NNO     : NOMBRE DE NOEUDS DE L'ELEMENT
C IN  NPG     : NOMBRE DE POINTS DE GAUSS
C IN  IPOIDS  : POIDS DES POINTS DE GAUSS
C IN  IVF     : VALEUR  DES FONCTIONS DE FORME
C IN  IDFDE   : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
C IN  GEOM    : COORDONEES DES NOEUDS
C IN  TYPMOD  : TYPE DE MODEELISATION
C IN  OPTION  : OPTION DE CALCUL
C................MODIFS.................................................
C IN  NNOB    : NOMBRE DE NOEUDS DE L'ELEMENT
C IN  IVFB    : VALEUR  DES FONCTIONS DE FORME
C IN  IDFDEB  : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
C IN  GEOMB   : COORDONEES DES NOEUDS
C.................FIN MODIFS............................................
C IN  IMATE   : MATERIAU CODE
C IN  COMPOR  : COMPORTEMENT
C IN  LGPG    : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
C               CETTE LONGUEUR EST UN MAJORANT DU NBRE REEL DE VAR. INT.
C IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
C IN  INSTAM  : INSTANT PRECEDENT
C IN  INSTAP  : INSTANT DE CALCUL
C IN  TM      : TEMPERATURE AUX NOEUDS A L'INSTANT PRECEDENT
C IN  TP      : TEMPERATURE AUX NOEUDS A L'INSTANT DE CALCUL
C IN  TREF    : TEMPERATURE DE REFERENCE
C IN  DEPLM   : DEPLACEMENT A L'INSTANT PRECEDENT
C IN  DEPLP   : INCREMENT DE DEPLACEMENT
C IN  ANGMAS  : LES TROIS ANGLES DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
C IN  SIGM    : CONTRAINTES A L'INSTANT PRECEDENT
C IN  VIM     : VARIABLES INTERNES A L'INSTANT PRECEDENT
C OUT DFDI    : DERIVEE DES FONCTIONS DE FORME  AU DERNIER PT DE GAUSS
C OUT DEF     : PRODUIT DER. FCT. FORME PAR F   AU DERNIER PT DE GAUSS
C OUT SIGP    : CONTRAINTES DE CAUCHY (RAPH_MECA ET FULL_MECA)
C OUT VIP     : VARIABLES INTERNES    (RAPH_MECA ET FULL_MECA)
C OUT MATUU   : MATRICE DE RIGIDITE PROFIL (RIGI_MECA_TANG ET FULL_MECA)
C OUT VECTU   : FORCES NODALES (RAPH_MECA ET FULL_MECA)
C................MODIFS.................................................
C OUT DFDIB   : DERIVEE DES FONCTIONS DE FORME  AU DERNIER PT DE GAUSS
C OUT DEFB    : PRODUIT DER. FCT. FORME PAR F   AU DERNIER PT DE GAUSS
C OUT MATEE   : MATRICE DE RIGIDITE CALCUL EPSR/EPSR
C OUT MATUE   : MATRICE DE RIGIDITE CALCUL U/EPSR
C OUT MATEU   : MATRICE DE RIGIDITE CALCUL EPSR/U
C OUT VECTUB  : FORCES NODALES CALCUL EPSR (RAPH_MECA ET FULL_MECA)
C OUT MATRIG  : MATRICE DE RIGIDITE STOCKEE EN LIGNE
C.................FIN MODIFS............................................

      INTEGER KPG,KU,N,I,M,J,KL,K,L,KP,KKL,PQ,COD(27)
      INTEGER PNNO,PNNOB,PNDIM,PDIMSI,IVF,IDFDE,IPOIDS,IDFDEB,IVFB
      PARAMETER (PNNO=20)
      PARAMETER (PNNOB=8)
      PARAMETER (PNDIM=3)
      PARAMETER (PDIMSI=6)


      REAL*8 DSIDEP(6,6),F(3,3),EPSM(6),DEPS(6),R,SIGMA(6),SIGN(6)
      REAL*8 EPSB(PNDIM*2,PNNOB),DEPSB(PNDIM*2,PNNOB),GEOMB(3,PNNOB)
      REAL*8 DEPLM(PNDIM,PNNO),DDEPL(PNDIM,PNNO)
      REAL*8 DSIDPR(6,6),EPSRM(2*PNDIM),GEPSRM(2*PNDIM,PNDIM)
      REAL*8 DEPSR(2*PNDIM),DGEPSR(2*PNDIM,PNDIM)
      REAL*8 POIDS,TEMPM,TEMPP
      REAL*8 ELGEOM(10,27)
      REAL*8 CCC, RAC2,R8VIDE, R8NNEM,ADIME, ANGMAS(3)
      REAL*8       EPSTM(12), DEPST(12),DSIDPT(12,6)
      REAL*8       MATUU(PNDIM,PNNO,PNNO,PNDIM),VECTU(PNDIM,PNNO)
      REAL*8       MATEE(PNDIM*2,PNNOB,PNNOB,PNDIM*2)
      REAL*8       MATUE(PNDIM,PNNO,PNNOB,PNDIM*2)
      REAL*8       MATEU(PNDIM*2,PNNOB,PNNO,PNDIM),VECTUB(2*PNDIM,PNNOB)
      REAL*8       MATR(PNNOB*3*PNDIM+(PNNO-PNNOB)*PNDIM,
     &                   PNNOB*3*PNDIM+(PNNO-PNNOB)*PNDIM)
      REAL*8  VECP(PNDIM,PNDIM),VPP(PNDIM),TR(PDIMSI),ARRET
      REAL*8  PROJ(PDIMSI,PDIMSI)
      REAL*8  PROJ2(PDIMSI,PDIMSI),TOLB
      REAL*8  RTEMP,RTEMP2
      INTEGER T(PNDIM,PNDIM)

      CHARACTER*8 NOMRES(3)
      REAL*8      VALRES(3)

      T(1,1)=1
      T(2,2)=2
      T(3,3)=3
      T(1,2)=4
      T(2,1)=4
      T(1,3)=5
      T(3,1)=5
      T(2,3)=6
      T(3,2)=6

      TOLB=1.D-2

C - INITIALISATION

      ADIME  = 1.D0
      RAC2   = SQRT(2.D0)
      GRAND  = .FALSE.
      AXI    = .FALSE.
      NDIMSI = 2*NDIM
      NOMRES(1)='LONG_CARA'

      CALL RCVALA(IMATE,' ','NON_LOCAL',0,' ',0.D0,1,
     &             NOMRES,VALRES,COD2,'FM')

      CCC=VALRES(1)
C - INITIALISATION CODES RETOURS
      DO 1955 KPG=1,NPG
         COD(KPG)=0
1955  CONTINUE
C
C - SEPARATION U/EPSR DANS DEPLM
      IF (OPTION(1:14).EQ.'RIGI_MECA_TANG' .OR.
     &    OPTION(1:14).EQ.'RIGI_MECA_ELAS' .OR.
     &    OPTION(1:9).EQ.'FULL_MECA') THEN
        CALL R8INIR(PNDIM*PNNO*PNNO*PNDIM,0.D0,MATUU,1)
        CALL R8INIR(PDIMSI*PNNOB*PNNOB*PDIMSI,0.D0,MATEE,1)
        CALL R8INIR(PNDIM*PNNO*PNNOB*PDIMSI,0.D0,MATUE,1)
        CALL R8INIR(PNDIM*PNNO*PNNOB*PDIMSI,0.D0,MATEU,1)
        CALL R8INIR((PNNOB*3*PNDIM+(PNNO-PNNOB)*PNDIM)
     &       *(PNNOB*3*PNDIM+(PNNO-PNNOB)*PNDIM),0.D0,MATR,1)
        CALL R8INIR((NNOB*3*NDIM+(NNO-NNOB)*NDIM)
     &       *(NNOB*3*NDIM+(NNO-NNOB)*NDIM),0.D0,MATRIG,1)
      ENDIF

      IF (OPTION(1:9).EQ.'RAPH_MECA' .OR.
     &    OPTION(1:9).EQ.'FULL_MECA') THEN
        CALL R8INIR(PNDIM*PNNO,0.D0,VECTU,1)
        CALL R8INIR(PDIMSI*PNNOB,0.D0,VECTUB,1)
      ENDIF

      DO 70 I=1,NDIM
        DO 71 J=1,NNOB
          GEOMB(I,J)=GEOM(I,J)
71      CONTINUE
70    CONTINUE

      DO 1 N=1,NNOB
        DO 2 K=1,NDIM
          DEPLM(K,N) = DEPLGM(K+(NDIM+NDIMSI)*(N-1))
2       CONTINUE
1     CONTINUE

      DO 3 N=1,NNOB
        DO 4 K=1,NDIMSI
          EPSB(K,N) = DEPLGM(K+NDIM+(NDIM+NDIMSI)*(N-1))
4       CONTINUE
3     CONTINUE

      DO 9 N=1,NNOB
        DO 6 K=1,NDIM
          DDEPL(K,N) = DDEPLG(K+(NDIM+NDIMSI)*(N-1))
6       CONTINUE
9     CONTINUE

      DO 7 N=1,NNOB
        DO 8 K=1,NDIMSI
          DEPSB(K,N) = DDEPLG(K+NDIM+(NDIM+NDIMSI)*(N-1))
8       CONTINUE
7     CONTINUE

      DO 11 N=NNOB+1,NNO
        DO 12 K=1,NDIM
          DEPLM(K,N) = DEPLGM(K+NDIM*(N-NNOB-1)+(NDIM+NDIMSI)*NNOB)
12      CONTINUE
11    CONTINUE

      DO 19 N=NNOB+1,NNO
        DO 16 K=1,NDIM
          DDEPL(K,N) = DDEPLG(K+NDIM*(N-NNOB-1)+(NDIM+NDIMSI)*NNOB)
16      CONTINUE
19    CONTINUE
C
C - CALCUL POUR CHAQUE POINT DE GAUSS
C
      DO 800 KPG=1,NPG
C
C - CALCUL DE LA TEMPERATURE AU POINT DE GAUSS
C - ET DES DEFORMATIONS ANELASTIQUES AU POINT DE GAUSS
C
        TEMPM = 0.D0
        TEMPP = 0.D0
C
        DO 10 N=1,NNO
          TEMPM = TEMPM + TM(N)*ZR(IVF+N+(KPG-1)*NNO-1)
          TEMPP = TEMPP + TP(N)*ZR(IVF+N+(KPG-1)*NNO-1)
 10     CONTINUE
C
C     CALCUL DE DFDI,F,EPSM,DEPS,R(EN AXI) ET POIDS
C
        DO 20 J = 1,6
          EPSM (J)=0.D0
          DEPS(J)=0.D0
20      CONTINUE
C
        CALL NMGEOM(NDIM,NNO,AXI,GRAND,GEOM,KPG,IPOIDS,IVF,IDFDE,
     &              DEPLM,POIDS,DFDI,F,EPSM,R)

C     CALCUL DE DEPS
C
        CALL NMGEOM(NDIM,NNO,AXI,GRAND,GEOM,KPG,IPOIDS,IVF,IDFDE,
     &              DDEPL,POIDS,DFDI,F,DEPS,R)

C      CALCUL DES PRODUITS SYMETR. DE F PAR N,
        DO 40 N=1,NNO
          DO 30 I=1,3
            DEF(1,N,I) =  F(I,1)*DFDI(N,1)
            DEF(2,N,I) =  F(I,2)*DFDI(N,2)
            DEF(3,N,I) =  F(I,3)*DFDI(N,3)
            DEF(4,N,I) = (F(I,1)*DFDI(N,2) + F(I,2)*DFDI(N,1))/RAC2
            DEF(5,N,I) = (F(I,1)*DFDI(N,3) + F(I,3)*DFDI(N,1))/RAC2
            DEF(6,N,I) = (F(I,2)*DFDI(N,3) + F(I,3)*DFDI(N,2))/RAC2
 30       CONTINUE
 40     CONTINUE
C
        DO 60 I=1,3
          SIGN(I) = SIGM(I,KPG)
 60     CONTINUE
        DO 65 I=4,6
          SIGN(I) = SIGM(I,KPG)*RAC2
 65     CONTINUE
C
C     CALCUL DE DFDIB,FB,EPSR,DEPSR ET POIDS
        CALL NMGEOB(NDIM, NNOB, GEOMB, KPG, IPOIDS, IVFB,
     &                  IDFDEB, EPSB, POIDS, DFDIB,
     &                  EPSRM, GEPSRM)
C
C
C     CALCUL DE DEPSR
C
        CALL NMGEOB(NDIM, NNOB, GEOMB, KPG, IPOIDS, IVFB,
     &                  IDFDEB, DEPSB, POIDS, DFDIB,
     &                  DEPSR, DGEPSR)
C
C   REUNION DES EPSM, DEPS AVEC EPSRM, DEPSR POUR PASSAGE DANS NMCOMP


        CALL R8INIR(12,0.D0,EPSTM,1)
        CALL R8INIR(12,0.D0,DEPST,1)

        DO 311 I=1,NDIMSI
          EPSTM(I)=EPSM(I)
          EPSTM(I+6)=EPSRM(I)
          DEPST(I)=DEPS(I)
          DEPST(I+6)=DEPSR(I)
 311    CONTINUE

C - LOI DE COMPORTEMENT
C --- ANGLE DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
C --- INITIALISE A R8NNEM (ON NE S'EN SERT PAS)
       CALL R8INIR(3, R8NNEM(), ANGMAS ,1)

C -    APPEL A LA LOI DE COMPORTEMENT
       CALL NMCOMP('RIGI',KPG,1,NDIM,TYPMOD,IMATE,COMPOR,CRIT,
     &             INSTAM,INSTAP,
     &             TEMPM,TEMPP,TREF,
     &             EPSTM,DEPST,
     &             SIGN,VIM(1,KPG),
     &             OPTION,
     &             ANGMAS,
     &             ELGEOM(1,KPG),
     &             SIGMA,VIP(1,KPG),DSIDPT,COD(KPG))

       IF(COD(KPG).EQ.1) THEN
         GOTO 1956
       ENDIF

       DO 313 I=1,6
         DO 314 J=1,6
           DSIDEP(I,J)=DSIDPT(I,J)
           DSIDPR(I,J)=DSIDPT(I+6,J)
 314     CONTINUE
 313   CONTINUE


        CALL R8INIR(36,0.D0,PROJ,1)

        IF (COMPOR(1) .EQ. 'ENDO_ORTH_BETON') THEN
C MODIFICATION POUR ENDO_ORTH_BETON------------------------
C ON ARRETE DE REGULARISER DANS LES DIRECTIONS OU ON A CASSE
C-----------------------------------------------------------
          TR(1) = VIM(1,KPG)
          TR(2) = VIM(2,KPG)
          TR(3) = VIM(3,KPG)
          TR(4) = VIM(4,KPG)
          TR(5) = VIM(5,KPG)
          TR(6) = VIM(6,KPG)
          CALL DIAGO3(TR,VECP,VPP)

          DO 701 I=1,3
            DO 702 J=I,3
              ARRET=1.D0
              IF ((1.D0-VPP(I)).LE.TOLB) THEN
                ARRET=0.D0
              ELSEIF ((1.D0-VPP(J)).LE.TOLB) THEN
                ARRET=0.D0
              ENDIF
              IF (I.EQ.J) THEN
                RTEMP2=1.D0
              ELSE
                RTEMP2=RAC2
              ENDIF
              DO 703 K=1,3
                DO 704 L=1,3
                  IF (K.EQ.L) THEN
                    RTEMP=1.D0
                  ELSE
                    RTEMP=RAC2
                  ENDIF
                  PROJ(T(I,J),T(K,L))=PROJ(T(I,J),T(K,L))
     &                +ARRET*(VECP(I,K)*VECP(J,L)+VECP(J,K)*VECP(I,L))
     &                 /RTEMP*RTEMP2/2.D0
 704            CONTINUE
 703          CONTINUE
 702        CONTINUE
 701      CONTINUE

        ELSE

          CALL R8INIR(36,0.D0,PROJ,1)

          DO 705 I=1,NDIMSI
             PROJ(I,I)=1.D0
 705      CONTINUE

        ENDIF

C CONTRACTION DU PROJECTEUR
          CALL R8INIR(36,0.D0,PROJ2,1)

          DO 730 I=1,NDIMSI
            DO 731 J=1,NDIMSI
              DO 732 K=1,NDIMSI
                 PROJ2(I,J)=PROJ2(I,J)
     &                          +PROJ(K,I)*PROJ(K,J)
 732          CONTINUE
 731        CONTINUE
 730      CONTINUE

C
C
C - CALCUL DE LA MATRICE DE RIGIDITE U/U
C
       IF ( OPTION(1:9) .EQ. 'RIGI_MECA'
     &  .OR. OPTION(1: 9) .EQ. 'FULL_MECA'    ) THEN
C



        DO 190 I=1,NDIM
          DO 180 M=1,NNO
            DO 170 N=1,NNO
              DO 160 J=1,NDIM
                DO 150 PQ=1,NDIMSI
                  DO 140 KL=1,NDIMSI
                    MATUU(I,M,N,J)= MATUU(I,M,N,J)
     & +DEF(PQ,N,J)*DSIDEP(PQ,KL)*DEF(KL,M,I)*POIDS
 140              CONTINUE
 150            CONTINUE
 160          CONTINUE
 170        CONTINUE
 180      CONTINUE
 190    CONTINUE
C
C
C -  CALCUL DE LA MATRICE DE RIGIDITE U/EPSRM

          DO 192 J=1,NDIM
            DO 182 M=1,NNO
              DO 172 N=1,NNOB
                DO 162 PQ=1,NDIMSI
                  IF (OPTION.EQ.'RIGI_MECA_ELAS') THEN
                    MATUE(J,M,N,PQ)=0.D0
                  ELSE
                    DO 152 KL=1,NDIMSI
                      MATUE(J,M,N,PQ) = MATUE(J,M,N,PQ)+
     &                       DEF(KL,M,J)* DSIDPR(KL,PQ)*
     &                   ZR(IVFB+N+(KPG-1)*NNOB-1)*POIDS
 152                CONTINUE
                  ENDIF
 162            CONTINUE
 172          CONTINUE
 182        CONTINUE
 192      CONTINUE
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ON CHANGE CETTE PARTIE DE LA MATRICE DE RIGIDITE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ON N A PLUS LA MEME RIGIDITE POUR TOUTES LES COMPOSANTES
C DU TENSEUR DES DEFORMATIONS REGULARISEES
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C -  CALCUL DE LA MATRICE DE RIGIDITE EPSRM/EPSRM
C
          DO 171 N=1,NNOB
            DO 181 M=1,NNOB
              DO 720 PQ=1,NDIMSI
                DO 721 KL=1,NDIMSI

             IF (PQ.EQ.KL) THEN
              DO 191 K=1,NDIM
                MATEE(PQ,M,N,KL) = MATEE(PQ,M,N,KL)
     &           +(CCC*DFDIB(N,K)*DFDIB(M,K))*POIDS
 191          CONTINUE
             ENDIF

         MATEE(PQ,M,N,KL)=MATEE(PQ,M,N,KL)+ZR(IVFB+M+(KPG-1)*NNOB-1)*
     &              ZR(IVFB+N+(KPG-1)*NNOB-1)*PROJ2(PQ,KL)*POIDS

 721           CONTINUE
 720         CONTINUE
 181       CONTINUE
 171      CONTINUE
C
C
C
C -  CALCUL DE LA MATRICE DE RIGIDITE EPSRM/U

          DO 193 M=1,NNOB
            DO 183 N=1,NNO
              DO 173 PQ=1,NDIMSI
                DO 163 J=1,NDIM
                  DO 750 KL=1,NDIMSI
                  MATEU(PQ,M,N,J)=MATEU(PQ,M,N,J)-
     &            ZR(IVFB+M+(KPG-1)*NNOB-1)*
     &            PROJ2(PQ,KL)*DEF(KL,N,J)*POIDS
 750              CONTINUE
 163            CONTINUE
 173          CONTINUE
 183        CONTINUE
 193      CONTINUE
C
C
C - STOCKAGE DE LA MATRICE DE RIGIDITE

          DO 590 M=1,NNOB
            DO 580 I=1,NDIM
              KP = (NDIM+NDIMSI)*(M-1) + I
              DO 560 N=1,NNOB
                DO 550 KL=1,NDIM
                  KU = (NDIM+NDIMSI)*(N-1) + KL
                  MATR(KP,KU) = MATUU(I,M,N,KL)
550             CONTINUE
                DO 540 PQ=1,NDIMSI
                  KU = (NDIM+NDIMSI)*(N-1) + PQ +NDIM
                  MATR(KP,KU) = MATUE(I,M,N,PQ)
540             CONTINUE
560           CONTINUE
              DO 530 N=NNOB+1,NNO
                DO 520 KL=1,NDIM
                  KU =(NDIM+NDIMSI)*NNOB+ NDIM*(N-NNOB-1) + KL
                  MATR(KP,KU) = MATUU(I,M,N,KL)
520             CONTINUE
530           CONTINUE
580         CONTINUE
            DO 570 J=1,NDIMSI
              KP = (NDIM+NDIMSI)*(M-1) + NDIM + J
              DO 561 N=1,NNOB
                DO 551 KL=1,NDIM
                  KU = (NDIM+NDIMSI)*(N-1) + KL
                  MATR(KP,KU) = ADIME*MATEU(J,M,N,KL)
551             CONTINUE
                DO 552 I=1,NDIMSI
                  KU = (NDIM+NDIMSI)*(N-1) + I +NDIM
                  MATR(KP,KU) = ADIME*MATEE(J,M,N,I)
552             CONTINUE
561           CONTINUE
              DO 531 N=NNOB+1,NNO
                DO 521 KL=1,NDIM
                  KU=(NDIM+NDIMSI)*NNOB+NDIM*(N-NNOB-1)+KL
                  MATR(KP,KU) = ADIME*MATEU(J,M,N,KL)
521             CONTINUE
531           CONTINUE
570         CONTINUE
590       CONTINUE

          DO 690 M=NNOB+1,NNO
            DO 680 I=1,NDIM
              KP = (NDIM+NDIMSI)*NNOB + NDIM*(M-NNOB-1) + I
              DO 660 N=1,NNOB
                DO 650 KL=1,NDIM
                  KU = (NDIM+NDIMSI)*(N-1) + KL
                  MATR(KP,KU) = MATUU(I,M,N,KL)
650             CONTINUE
                DO 640 PQ=1,NDIMSI
                  KU = (NDIM+NDIMSI)*(N-1) + PQ +NDIM
                  MATR(KP,KU) = MATUE(I,M,N,PQ)
640             CONTINUE
660           CONTINUE
              DO 630 N=NNOB+1,NNO
                DO 620 KL=1,NDIM
                  KU=(NDIM+NDIMSI)*NNOB+NDIM*(N-NNOB-1)+KL
                  MATR(KP,KU) = MATUU(I,M,N,KL)
620             CONTINUE
630           CONTINUE
680         CONTINUE
690       CONTINUE
C
C - REMPLISSAGE CF ASSTHM  POUR L'ORDRE
           DO 790 M=1,(NDIM+NDIMSI)*NNOB + NDIM*(NNO-NNOB)
             DO 780 N=1,(NDIM+NDIMSI)*NNOB + NDIM*(NNO-NNOB)
               KKL = ((NDIM+NDIMSI)*NNOB + NDIM*(NNO-NNOB))*(M-1) + N
               MATRIG(KKL)= MATR(M,N)
780          CONTINUE
790        CONTINUE
         ENDIF

C
C - CALCUL DE LA FORCE INTERIEURE ET DES CONTRAINTES DE CAUCHY
C
        IF(OPTION(1:9).EQ.'FULL_MECA'.OR.
     &     OPTION(1:9).EQ.'RAPH_MECA') THEN
C
          DO 230 N=1,NNO
            DO 220 I=1,NDIM
              DO 210 KL=1,NDIMSI
                VECTU(I,N)=VECTU(I,N)+DEF(KL,N,I)*SIGMA(KL)*POIDS
 210          CONTINUE
 220        CONTINUE
 230      CONTINUE
C
C

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC- ON CHANGE LE VECTEUR FORCE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C - CALCUL DE LA "FORCE" DANS LE CALCUL DE EPSRM
          DO 290 N=1,NNOB
            DO 280 PQ=1,NDIMSI
              DO 260 KL=1,NDIMSI
              VECTUB(PQ,N)=VECTUB(PQ,N)+(ZR(IVFB+N+(KPG-1)*NNOB-1)*
     &         (EPSRM(KL)+DEPSR(KL))-ZR(IVFB+N+(KPG-1)*NNOB-1)*
     &         (EPSM(KL)+DEPS(KL)))*PROJ2(PQ,KL)*POIDS
 260          CONTINUE
              DO 270 I=1,NDIM
                VECTUB(PQ,N)=VECTUB(PQ,N)+CCC*(DFDIB(N,I)*(GEPSRM(PQ,I)
     &                       +DGEPSR(PQ,I))) * POIDS
 270          CONTINUE
 280        CONTINUE
 290      CONTINUE

C
C - STOCKAGE DU VECTEUR FORCE INTERIEUR GLOBAL

          DO 490 N=1,NNOB
            DO 480 I=1,NDIM
            KU = (NDIMSI + NDIM)*(N-1) + I
            VECTF(KU) = VECTU(I,N)
480         CONTINUE
            DO 470 PQ=1,NDIMSI
              KU = (NDIMSI + NDIM)*(N-1) + PQ + NDIM
              VECTF(KU) = ADIME*VECTUB(PQ,N)
470         CONTINUE
490       CONTINUE

          DO 460 N=NNOB+1,NNO
            DO 450 I=1,NDIM
              KU = (NDIMSI + NDIM)*NNOB + NDIM*(N-NNOB-1) + I
              VECTF(KU) = VECTU(I,N)
450         CONTINUE
460       CONTINUE

          DO 310 KL=1,3
            SIGP(KL,KPG) = SIGMA(KL)
310       CONTINUE
          DO 320 KL=4,6
            SIGP(KL,KPG) = SIGMA(KL)/RAC2
320       CONTINUE
C
        ENDIF
800   CONTINUE
1956  CONTINUE
C - SYNTHESE DES CODES RETOURS
      CALL CODERE(COD,NPG,CODRET)

      END

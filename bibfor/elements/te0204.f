      SUBROUTINE TE0204(OPTION,NOMTE)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 16/11/2009   AUTEUR REZETTE C.REZETTE 
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
C-----------------------------------------------------------------------
C
C FONCTION REALISEE:
C
C   CALCUL DE LA DERIVEE DU TAUX DE RESTITUTION D'ENERGIE ELEMENTAIRE
C   EN ELASTICITE PAR RAPPORT AU MODULE D'YOUNG E (CALC_DG_E)
C              OU PAR RAPPORT AU CHARGEMENT F (CALC_DG_FORC)
C
C   (EN ELASTIQUE ISOTROPE LINEAIRE PETIT OU GRAND DEPLACEMENTS).
C
C   ELEMENTS ISOPARAMETRIQUES 2D
C
C   OPTION : 'CALC_DG_E'    (DG/DE AVEC CHARGES REELLES)
C            'CALC_DG_E_F'  (DG/DE AVEC CHARGES FONCTIONS)
C            'CALC_DG_FORC'    (DG/DF AVEC CHARGES REELLES)
C            'CALC_DG_FORC_F'  (DG/DF AVEC CHARGES FONCTIONS)
C
C   -------------------------------------------------------------------
C     SUBROUTINES APPELLEES:
C       JEVEUX AND CO: JEMARQ, JEDEMA, JEVETE, JEVECH, TECACH.
C       ENVIMA: R8PREM
C       MATERIAUX: RCCOMA, RCVALB.
C       ELEMENTS FINIS: NMGEOM, PPGANO, D2GEOM.
C       DIVERS: FOINTE, NMELNL, NMPLRU.
C
C     FONCTIONS INTRINSEQUES:
C       SQRT, ABS.
C   -------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      CHARACTER*16      OPTION,NOMTE,PHENOM

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

C DECLARATION VARIABLES LOCALES
C
C
      CHARACTER*2   CODRET,CODRES(3)
      CHARACTER*8   NOMPAR(3),TYPMOD(2),ELREFE,NOMRES(3), BLAN8
      CHARACTER*16  COMPOR(4),OPRUPT

      REAL*8   E,NU,DPSIDE,LAM,MU,TRA,TRADE
      REAL*8   TERM1,TERM2,TERM3,TERM4,TERM5,TERM6,ALPHA
      REAL*8   COE1,COE2,COE3,COE4,COE5,COE6
      REAL*8   EPSI,RAC2,R8PREM,CRIT(3),DSRDE(3,3),DUDPDE(3,3)
      REAL*8   DFDI(27),F(3,3),SR(3,3),SIGL(6),SIGIN(6),DSIGIN(6,3)
      REAL*8   EPS(6),EPSIN(6),DEPSIN(6,3)
      REAL*8   EPSINO(36),EPSIPG(36),FNO(18),DFNO(18),DUDMDE(3),THET
      REAL*8   TREF,TG(27),TGD(20),TGDM(3),FLAG(3),DEVRES(3),EPSNO(36)
      REAL*8   TCLA,TTHE,TFOR,TINI,POIDS,R,RBID,DE,DNU,DALPHA,DDPSI
      REAL*8   PROD,PROD1,PROD2,DIVT,VALPAR(4),VALRES(3)
      REAL*8   ENERGI(2),RHO,OM,OMO,EPSEPS,EPSDEP
      REAL*8   DTDM(3,5),DER(6),DFDM(3,5),DUDM(3,4)
      REAL*8   TEMSEG, DEPS(6),
     &         DSIGL(6),DENERG(2),DSIGDE(6),DEPSDE(6),
     &         NU1,NU12,NU1M,COEF,TERMT1,TERMT2

      INTEGER  IPOIDS,IVF,IDFDE,IPOI1,IVF1,IDFDE1,JGANO
      INTEGER  ICOMP,IGEOM,ITEMPS,IDEPL,IMATE,IFOSSF
      INTEGER  IEPSR,IEPSF,ISIGI,IDEPI,IFOSSR
      INTEGER  IFORC,IFORF,ITHET,IGTHET,IROTA,IPESA,IER
      INTEGER  NNO,NNOS,NPG,NPG1,NCMP
      INTEGER  I,J,K,KK,L,M,KP,NDIM,COMPT
      INTEGER  IDEPSE,IJ,IJ1,MATCOD,I1,IRET,IRET1,IDEFSE,ISIGSE

      LOGICAL  GRAND,AXI,CP,DP,FONC,INCR,EPSINI,
     &         DERIVE,DERIVF,DERFOR,LTEATT

C =====================================================================
C INITIALISATIONS
C =====================================================================
      DATA NOMRES/'E','NU','ALPHA'/
      CALL ELREF1(ELREFE)
      CALL JEMARQ()
      EPSI   = R8PREM()
      RAC2   = SQRT(2.D0)
      OPRUPT = 'RUPTURE'
      AXI    = .FALSE.
      CP     = .FALSE.
      DP     = .FALSE.
      EPSINI = .FALSE.
      DERFOR = .FALSE.
      BLAN8  = '        '
      TYPMOD(2) = BLAN8

      IF (LTEATT(' ','AXIS','OUI')) THEN
        TYPMOD(1) = 'AXIS    '
        AXI = .TRUE.
      ELSEIF (LTEATT(' ','C_PLAN','OUI')) THEN
        TYPMOD(1) = 'C_PLAN  '
        CP  = .TRUE.
      ELSEIF (LTEATT(' ','D_PLAN','OUI')) THEN
        TYPMOD(1) = 'D_PLAN  '
        DP  = .TRUE.
      ENDIF

      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDE,JGANO)
C NOMBRE DE COMPOSANTES DES TENSEURS
      NCMP = 2*NDIM

C INIT. POUR LE CALCUL DES DERIVEES DE G
      TCLA  = 0.D0
      TTHE  = 0.D0
      TFOR  = 0.D0
      TINI  = 0.D0
      CALL JEVECH('PGTHETA','E',IGTHET)
      CALL JEVECH('PTHETAR','L',ITHET)

C TEST SUR LA NULLITE DE THETA_FISSURE
      DERIVE = .FALSE.
      DERIVF = .FALSE.
      COMPT = 0
      DO 3 I=1,NNO
        THET = 0.D0
        DO 2 J=1,NDIM
          THET = THET + ABS(ZR(ITHET+NDIM*(I-1)+J-1))
2      CONTINUE
        IF (THET.LT.EPSI) COMPT = COMPT+1
3     CONTINUE
      IF(COMPT.EQ.NNO) GOTO 9999


C =====================================================================
C RECUPERATION DES CHAMPS LOCAUX
C =====================================================================

      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PDEPLAR','L',IDEPL)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PCOMPOR','L',ICOMP)
      MATCOD = ZI(IMATE)
      DO 10 I = 1,4
        COMPOR(I)= ZK16(ICOMP+I-1)
10    CONTINUE

C TEST SUR LA LOI DE COMPORTEMENT
      IF (OPTION(1:9).EQ.'CALC_DG_E') THEN
        DERIVE = .TRUE.
        IF (COMPOR(1)(1:4).NE.'ELAS')
     &    CALL U2MESK('F','SENSIBILITE_15', 1, COMPOR(1))
      ENDIF
      IF (OPTION(1:12).EQ.'CALC_DG_FORC') THEN
        DERIVF = .TRUE.
        IF (COMPOR(1)(1:4).NE.'ELAS')
     &    CALL U2MESK('F','SENSIBILITE_15', 1, COMPOR(1))
      ENDIF
        CALL JEVECH('PDEPLSE','L',IDEPSE)
        CALL JEVECH('PDEFOSE','L',IDEFSE)
        CALL JEVECH('PSIGMSE','L',ISIGSE)

      IF (OPTION.EQ.'CALC_DG_E_F'
     &      .OR.OPTION.EQ.'CALC_DG_FORC_F') THEN
        FONC = .TRUE.
        CALL JEVECH('PFFVOLU','L',IFORF)
        CALL JEVECH('PTEMPSR','L',ITEMPS)
        NOMPAR(1) = 'X'
        NOMPAR(2) = 'Y'
        NOMPAR(3) = 'INST'
        VALPAR(3) = ZR(ITEMPS)
        CALL TECACH('ONN','PEPSINF',1,IEPSF,IRET)
        IF (IEPSF.NE.0) EPSINI = .TRUE.
        IF (OPTION.EQ.'CALC_DG_FORC_F') THEN
          CALL JEVECH('PFFVOSS','L',IFOSSF)
          DERFOR = .TRUE.
        ENDIF
      ELSE IF (OPTION.EQ.'CALC_DG_E'.OR.OPTION.EQ.'CALC_DG_FORC') THEN
        FONC = .FALSE.
        CALL JEVECH('PFRVOLU','L',IFORC)
        CALL TECACH('ONN','PEPSINR',1,IEPSR,IRET)
        IF (IEPSR.NE.0) EPSINI = .TRUE.
        IF (OPTION.EQ.'CALC_DG_FORC') THEN
          CALL JEVECH('PFRVOSS','L',IFOSSR)
          DERFOR = .TRUE.
        ENDIF
      ENDIF
C LOI DE COMPORTEMENT
      GRAND = COMPOR(3)(1:5).EQ.'GREEN'
      INCR  = COMPOR(4)(1:9).EQ.'COMP_INCR'
      CALL TECACH('ONN','PPESANR',1,IPESA,IRET)
      CALL TECACH('ONN','PROTATR',1,IROTA,IRET)
      CALL TECACH('ONN','PSIGINR',1,ISIGI,IRET)
      CALL TECACH('ONN','PDEPINR',1,IDEPI,IRET)

      DO 20 I=1,NCMP*NNO
        EPSINO(I) = 0.D0
20    CONTINUE

C =====================================================================
C MESSAGES D'ERREURS
C =====================================================================

C ON NE PEUT AVOIR SIMULTANEMENT DEFORMATIONS ET CONTRAINTES INIT.
      IF ((ISIGI.NE.0).AND.EPSINI) THEN
        CALL U2MESS('F','RUPTURE1_20')
      ENDIF

      IF (INCR) THEN
        CALL U2MESS('F','RUPTURE1_22')
      ENDIF

C =====================================================================
C RECUPERATION DES CHARGES ET DEFORMATIONS INITIALES
C =====================================================================
      IF (FONC) THEN
        DO 50 I=1,NNO
          I1 = I-1
          IJ = IGEOM+NDIM*I1-1
          DO 30 J=1,NDIM
            VALPAR(J) = ZR(IJ+J)
30        CONTINUE
          DO 40 J=1,NDIM
            KK = NDIM*I1+J
            CALL FOINTE('FM',ZK8(IFORF+J-1),3,NOMPAR,VALPAR,FNO(KK),IER)
40        CONTINUE
          IF (EPSINI) THEN
            DO 45 J=1,NCMP
              KK = NCMP*I1+J
              CALL FOINTE('FM',ZK8(IEPSF+J-1),3,NOMPAR,VALPAR,
     &                     EPSINO(KK),IER)
45          CONTINUE
          ENDIF
          IF (DERFOR) THEN
            DO 41 J=1,NDIM
              KK = NDIM*I1+J
              CALL FOINTE('FM',ZK8(IFOSSF+J-1),3,NOMPAR,VALPAR,DFNO(KK)
     &                   ,IER)
41          CONTINUE
          ENDIF
50      CONTINUE
      ELSE
        DO 80 I=1,NNO
          I1 = I-1
          IJ = NDIM*I1
          IJ1 = IFORC+IJ-1
          DO 60 J=1,NDIM
            FNO(IJ+J)= ZR(IJ1+J)
60        CONTINUE
          IF (EPSINI) THEN
            IJ = NCMP*I1
            IJ1 = IEPSR+IJ-1
            DO 70 J=1,3
              EPSINO(IJ+J) = ZR(IJ1+J)
70          CONTINUE
            EPSINO(IJ+4) = ZR(IJ1+4)*RAC2
          ENDIF
          IF (DERFOR) THEN
            IJ = NDIM*I1
            IJ1 = IFOSSR+IJ-1
            DO 42 J=1,NDIM
              DFNO(IJ+J)= ZR(IJ1+J)
42          CONTINUE
          ENDIF
80      CONTINUE
      ENDIF
C
C PESANTEUR ET ROTATION
C
      IF ((IPESA.NE.0).OR.(IROTA.NE.0)) THEN
        CALL RCCOMA(MATCOD,'ELAS',PHENOM,CODRET)
       CALL RCVALB('RIGI',1,1,'+',MATCOD,' ',PHENOM,
     &             1,' ',RBID,1,'RHO',RHO,CODRET,'FM')
        IF (IPESA.NE.0) THEN
          DO 95 I=1,NNO
            IJ = NDIM*(I-1)
            DO 90 J=1,NDIM
              KK = IJ + J
              FNO(KK)=FNO(KK)+RHO*ZR(IPESA)*ZR(IPESA+J)
90          CONTINUE
95        CONTINUE
        ENDIF
        IF (IROTA.NE.0) THEN
          OM = ZR(IROTA)
          DO 105 I=1,NNO
            OMO = 0.D0
            IJ =  NDIM*(I-1)
            DO 100 J=1,NDIM
              OMO = OMO + ZR(IROTA+J)* ZR(IGEOM+IJ+J-1)
100         CONTINUE
            DO 103 J=1,NDIM
              KK = IJ + J
              FNO(KK)=FNO(KK)+RHO*OM*OM*(ZR(IGEOM+KK-1)-OMO*ZR(IROTA+J))
103         CONTINUE
105       CONTINUE
        ENDIF
      ENDIF

C =====================================================================
C TRAITEMENTS PARTICULIERS LIES A UN DEPLACEMENT INITIAL
C =====================================================================

      IF (IDEPI.NE.0) THEN
        IF(NOMTE(5:8).EQ.'TR3 '.OR. NOMTE(5:8).EQ.'QU4 ') THEN
          CALL ELREF4(' ','MASS',NDIM,NNO,NNOS,NPG,IPOI1,IVF1,
     &                           IDFDE1,JGANO)
        ELSE
          CALL ELREF4(' ','NOEU_S',NDIM,NNO,NNOS,NPG,IPOI1,IVF1,
     &                           IDFDE1,JGANO)
        ENDIF
        DO 120 KP=1,NPG
          L    = (KP-1)*NNO
          CALL NMGEOM (NDIM,NNO,AXI,GRAND,ZR(IGEOM),KP,IPOI1,
     &                 IVF1,IDFDE1,ZR(IDEPI),
     &                 RBID,DFDI,F,EPS,RBID)
          DO 110 I=1,NCMP
            EPSIPG((KP-1)*NCMP+I)= EPS(I)
110       CONTINUE
120     CONTINUE
C
        CALL PPGAN2 (JGANO,NCMP,EPSIPG,EPSNO)
        DO 121 I=1,NNO*NCMP
          EPSINO(I) = EPSINO(I)+EPSNO(I)
121     CONTINUE
      ENDIF

C ======================================================================
C BOUCLE PRINCIPALE SUR LES POINTS DE GAUSS
C ======================================================================
      IRET=0
      DO 645 KP = 1,NPG1
        CALL RCVARC(' ','TEMP','+','RIGI',KP,1,TG(KP),IRET1)
        IRET=IRET+IRET1
  645 CONTINUE

      DO 646 KP = 1,NNO
        CALL RCVARC(' ','TEMP','+','NOEU',KP,1,TGD(KP),IRET1)
        IRET=IRET+IRET1
  646 CONTINUE

      CALL RCVARC(' ','TEMP','REF','RIGI',1,1,TREF,IRET1)
      IRET=IRET+IRET1

      DO 800 KP=1,NPG1

C INITIALISATIONS
        L   = (KP-1)*NNO
        TEMSEG = 0.D0
        DO 220 I=1,3
          FLAG(I) = 0.D0
          TGDM(I) = 0.D0
          DUDMDE(I) = 0.D0
          DO 200 J=1,3
            SR(I,J) = 0.D0
            DUDPDE(I,J) = 0.D0
            DSRDE(I,J) = 0.D0
200       CONTINUE
          DO 210 J=1,4
            DUDM(I,J) = 0.D0
            DTDM(I,J) = 0.D0
            DFDM(I,J) = 0.D0
210       CONTINUE
          DFDM(I,5) = 0.D0
          DTDM(I,5) = 0.D0
220     CONTINUE
        DO 240 I=1,6
          SIGL (I) = 0.D0
          SIGIN(I) = 0.D0
          EPSIN(I) = 0.D0
          EPS (I)  = 0.D0
          DEPS(I)  = 0.D0
          DSIGL (I) = 0.D0
          DO 230 J=1,3
            DSIGIN(I,J) = 0.D0
            DEPSIN(I,J) = 0.D0
230       CONTINUE
240     CONTINUE

C ================================
C CALCUL DES ELEMENTS GEOMETRIQUES
C ================================
        CALL NMGEOM (NDIM,NNO,AXI,GRAND,ZR(IGEOM),KP,IPOIDS,
     &               IVF,IDFDE,ZR(IDEPL),
     &               POIDS,DFDI,F,EPS,R)
C =========================================================
C STOCKAGE DES DERIVEES DES CONTRAINTES ET DES DEFORMATIONS
C =========================================================
         DO 313 J=1,NCMP
           DSIGDE(J) = ZR(ISIGSE+NCMP*(KP-1)+J-1)
           DEPSDE(J) = ZR(IDEFSE+NCMP*(KP-1)+J-1)
313      CONTINUE
C - CALCULS DES GRADIENTS DE U (DUDM), DE THETA FISSURE (DTDM) ET DE
C   LA FORCE VOLUMIQUE (DFDM),
C   DE LA TEMPERATURE AUX POINTS DE GAUSS (TG) ET SON GRADIENT (TGDM)
        DO 320 I=1,NNO
          I1 = I-1
          DER(1) = DFDI(I)
          DER(2) = DFDI(I+NNO)
          DER(4) = ZR(IVF+L+I1)
          IF (IRET.EQ.0) THEN
            DO 301 J=1,NDIM
              TGDM(J)     = TGDM(J)   + TGD(I)*DER(J)
301         CONTINUE
          ELSE
            DO 302 J=1,NDIM
              TGDM(J)=0.D0
302         CONTINUE
          ENDIF     
          DO 310 J=1,NDIM
            IJ1 = NDIM*I1+J
            IJ = IJ1 - 1
            DO 300 K=1,NDIM
              DUDM(J,K) = DUDM(J,K) + ZR(IDEPL+IJ)*DER(K)
              DTDM(J,K) = DTDM(J,K) + ZR(ITHET+IJ)*DER(K)
              DFDM(J,K) = DFDM(J,K) + FNO(IJ1)*DER(K)
300         CONTINUE
              DUDM(J,4) = DUDM(J,4) + ZR(IDEPL+IJ)*DER(4)
              DTDM(J,4) = DTDM(J,4) + ZR(ITHET+IJ)*DER(4)
              DFDM(J,4) = DFDM(J,4) + FNO(IJ1)*DER(4)
              IF(DERFOR) FLAG(J) = FLAG(J) + DFNO(IJ1)*DER(4)
310       CONTINUE
            DO 311 J=1,NDIM
              DUDMDE(J) = DUDMDE(J) + ZR(IDEPSE+NDIM*(I-1)+J-1)*DER(4)
              DO 312 K=1,NDIM
                DUDPDE(J,K)=DUDPDE(J,K)+ZR(IDEPSE+NDIM*(I-1)+J-1)*DER(K)
  312         CONTINUE
  311       CONTINUE
320     CONTINUE

C =======================================================
C DEFORMATIONS INITIALES
C =======================================================

        IF ((IDEPI.NE.0).OR.EPSINI) THEN
          DO 420 I=1,NNO
            I1 = I-1
            DER(1) = DFDI(I)
            DER(2) = DFDI(I+NNO)
            DER(3) = 0.D0
            DER(4) = ZR(IVF+L+I1)
            IJ = NCMP*I1
            DO 400 J=1,NCMP
              EPSIN(J) = EPSIN(J)+ EPSINO(IJ+J)*DER(4)
400         CONTINUE
            DO 415 J=1,NCMP
              IJ1 = IJ+J
              DO 410 K=1,NDIM
                DEPSIN(J,K) = DEPSIN(J,K)+EPSINO(IJ1)*DER(K)
410           CONTINUE
415         CONTINUE
420       CONTINUE
          DO 430 I=1,NCMP
            EPS(I) = EPS(I)-EPSIN(I)
430       CONTINUE

        ENDIF

C EN ELASTICITE
C CALCUL DE SIGMA (SIGL)
          CRIT(1) = 300
          CRIT(2) = 0.D0
          CRIT(3) = 1.D-3
          CALL NMELNL('RIGI',KP,1,'+',NDIM,TYPMOD,MATCOD,COMPOR,CRIT,
     &                OPRUPT,EPS,SIGL,RBID,RBID,ENERGI,.FALSE.,
     &                TEMSEG,DEPS,DENERG,DSIGL)


C TRAITEMENTS DEPENDANT DE LA MODELISATION
        IF(CP) THEN
          DUDM(3,3)= EPS(3)
        ENDIF
        IF (AXI) THEN
          DUDM(3,3)= DUDM(1,4)/R
          DTDM(3,3)= DTDM(1,4)/R
          DFDM(3,3)= DFDM(1,4)/R
        ENDIF

C CALCUL DE LA DIVERGENCE DU THETA FISSURE (DIVT)
        DIVT = 0.D0
        DO 437 I=1,3
          DIVT  = DIVT + DTDM(I,I)
437     CONTINUE

C =======================================================
C CONTRAINTES INITIALES
C =======================================================

        IF (ISIGI.NE.0) THEN
          DO 460 I=1,NNO
            I1 = I-1
            DER(1) = DFDI(I)
            DER(2) = DFDI(I+NNO)
            DER(3) = 0.D0
            DER(4) = ZR(IVF+L+I1)

C CALCUL DE SIGMA INITIAL
            IJ = ISIGI+NCMP*I1-1
            DO 440 J=1,NCMP
              SIGIN(J) = SIGIN(J)+ ZR(IJ+J)*DER(4)
440         CONTINUE

C CALCUL DU GRADIENT DE SIGMA INITIAL
            DO 455 J=1,NCMP
              DO 450 K=1,NDIM
                DSIGIN(J,K)=DSIGIN(J,K)+ZR(IJ+J)*DER(K)
450           CONTINUE
455         CONTINUE
460       CONTINUE

C TRAITEMENTS PARTICULIERS DES TERMES CROISES
          DO 463 I=4,NCMP
            SIGIN(I) = SIGIN(I)*RAC2
            DO  462 J=1,NDIM
              DSIGIN(I,J) = DSIGIN(4,1)*RAC2
462         CONTINUE
463       CONTINUE

C CORRECTION DE SIGMA ET DE L'ENERGIE LIBRE TOTALE
          DO 464 I=1,NCMP
            SIGL(I) = SIGL(I)+ SIGIN(I)
464       CONTINUE
          DO 465 I=1,NCMP
            ENERGI(1) = ENERGI(1) + (EPS(I)+0.5D0*EPSIN(I))*SIGIN(I)
465       CONTINUE
        ENDIF

C =======================================================
C STOCKAGE DE D(SIGMA)/DE  ET TRAITEMENTS DES TERMES CROISES
C =======================================================
        SR(1,1)= SIGL(1)
        SR(2,2)= SIGL(2)
        SR(3,3)= SIGL(3)
        SR(1,2)= SIGL(4)/RAC2
        SR(2,1)= SR(1,2)
        SR(1,3)= 0.D0
        SR(2,3)= 0.D0
        SR(3,1)= 0.D0
        SR(3,2)= 0.D0
        DSRDE(1,1)= DSIGDE(1)
        DSRDE(2,2)= DSIGDE(2)
        DSRDE(3,3)= DSIGDE(3)
        DSRDE(1,2)= DSIGDE(4)
        DSRDE(2,1)= DSRDE(1,2)
        DSRDE(1,3)= 0.D0
        DSRDE(2,3)= 0.D0
        DSRDE(3,1)= 0.D0
        DSRDE(3,2)= 0.D0
        EPS(4)= EPS(4)/RAC2

C CALCUL DE LA DERIVEE DE G PAR RAPPORT A E (SI DERIVE)
C                                      OU F (SI DERIVF)

C =======================================================
C DERIVEE DU TERME THERMOELASTIQUE CLASSIQUE
C =======================================================
          PROD1 = 0.D0
          PROD2 = 0.D0
          DO 550 I = 1,3
            DO 549 J = 1,3
              DO 548 K = 1,3
                DO 547 M = 1,3
                  PROD1 = PROD1+F(I,J)*DSRDE(J,K)*DUDM(I,M)*DTDM(M,K)
                  PROD2 = PROD2+F(I,J)*SR(J,K)*DUDPDE(I,M)*DTDM(M,K)
  547           CONTINUE
  548         CONTINUE
  549       CONTINUE
  550     CONTINUE
          PROD=PROD1+PROD2
C - CALCUL DE DPSI/DE
          CALL RCVAD2 ('RIGI',KP,1,'+',ZI(IMATE),'ELAS',3,
     &                 NOMRES,VALRES,DEVRES,CODRES)
C
          IF (CODRES(3).NE.'OK') THEN
            VALRES(3)= 0.D0
            DEVRES(3)= 0.D0
          ENDIF
          E     = VALRES(1)
          NU    = VALRES(2)
          ALPHA = VALRES(3)
          IF (ALPHA.NE.0.D0) THEN
            CALL U2MESS('F','SENSIBILITE_53')
          ENDIF
C DERIVEES PAR RAPPORT A LA TEMPERATURE
          DE    = DEVRES(1)
          DNU   = DEVRES(2)
          DALPHA= DEVRES(3)
          TRA = EPS(1)+EPS(2)+EPS(3)
          TRADE = DEPSDE(1)+DEPSDE(2)+DEPSDE(3)
C
          IF(AXI) THEN
            LAM=NU*E/((1.D0+NU)*(1.D0-2.D0*NU))
            MU=E/((1.D0+NU)*2.D0)
            DPSIDE=0.D0
            TERM1 = TRA*TRA*LAM/(2.D0*E)
            TERM2 = TRA*TRADE*LAM
            EPSEPS = 0.D0
            EPSDEP = 0.D0
            DO 545 I=1,3
              EPSEPS = EPSEPS + EPS(I)*EPS(I)
              EPSDEP = EPSDEP + EPS(I)*DEPSDE(I)
  545       CONTINUE
            EPSEPS = EPSEPS + 2.D0*EPS(4)*EPS(4)
            EPSDEP = EPSDEP + 2.D0*EPS(4)*DEPSDE(4)
            TERM3 = EPSEPS*MU/E
            TERM4 = EPSDEP*MU*2.D0
            IF (IRET.EQ.0) THEN
              TERM5=-ALPHA*(TG(KP)-TREF)*E*TRADE/(1.D0-2.D0*NU)
              TERM6=-ALPHA*(TG(KP)-TREF)*(TRA-1.5D0*ALPHA*(TG(KP)-TREF))
     &                /(1.D0-2.D0*NU)
            ELSE
              TERM5=0.D0
              TERM6=0.D0
            ENDIF
            IF(DERIVE) DPSIDE = TERM1 + TERM2 + TERM3 + TERM4 + TERM5
     &                         +TERM6
            IF(DERIVF) DPSIDE = TERM2 + TERM4 + TERM5
           ELSE
             CALL ASSERT(DP.OR.CP)
             IF(DP) THEN
              COE1 = (1.D0-NU)*E/(2.D0*(1.D0+NU)*(1.D0-2.D0*NU))
              COE2 = NU*E/((1.D0+NU)*(1.D0-2.D0*NU))
              COE3 = E/(1.D0+NU)
             ELSE IF(CP) THEN
              COE1 = E/(2.D0*(1.D0+NU)*(1.D0-NU))
              COE2 = NU*E/((1.D0+NU)*(1.D0-NU))
              COE3 = E/(1.D0+NU)
             ENDIF
             IF (IRET.EQ.0) THEN
               DPSIDE=COE2*(EPS(1)*DEPSDE(2)+EPS(2)*DEPSDE(1))
     &                 +2.D0*COE1*(EPS(1)*DEPSDE(1)+EPS(2)*DEPSDE(2))
     &                 +2.D0*COE3*EPS(4)*DEPSDE(4)
     &                 -ALPHA*(TG(KP)-TREF)*E*TRADE/(1.D0-2.D0*NU)
               IF(DERIVE) THEN
                 DPSIDE=DPSIDE +(COE1/E)*(EPS(1)*EPS(1)+EPS(2)*EPS(2))
     &                    +(COE2/E)*EPS(1)*EPS(2)+(COE3/E)*EPS(4)*EPS(4)
     &                    -ALPHA*(TG(KP)-TREF)*
     &                    (TRA-1.5D0*ALPHA*(TG(KP)-TREF))/(1.D0-2.D0*NU)
               ENDIF
             ELSE
               DPSIDE=COE2*(EPS(1)*DEPSDE(2)+EPS(2)*DEPSDE(1))
     &                 +2.D0*COE1*(EPS(1)*DEPSDE(1)+EPS(2)*DEPSDE(2))
     &                 +2.D0*COE3*EPS(4)*DEPSDE(4)
               IF(DERIVE) THEN
                 DPSIDE=DPSIDE +(COE1/E)*(EPS(1)*EPS(1)+EPS(2)*EPS(2))
     &                  +(COE2/E)*EPS(1)*EPS(2)+(COE3/E)*EPS(4)*EPS(4)
               ENDIF
             ENDIF
C
           TCLA = TCLA + POIDS* (PROD-DPSIDE*DIVT)
        ENDIF
C =======================================================
C DERIVEE DU TERME THERMIQUE :   -(D(ENER)/DT)(GRAD(T).THETA)
C =======================================================
        NU1 = 1.D0+NU
        NU1M = 1.D0-2.D0*NU
        NU12 = NU1*NU1
        IF (IRET.EQ.0) THEN
          TERMT1 = -(ALPHA + DALPHA*(TG(KP)-TREF))*(TRA+E*TRADE)/NU1M
     &             -(ALPHA*(TG(KP)-TREF)/NU1M)*(2.D0*DNU*TRA/NU1M
     &             +(DE+2.D0*E*DNU/NU1M)*TRADE)
          TERMT2 = ((3.D0*ALPHA*(TG(KP)-TREF))/NU1M)*
     &           (ALPHA + DALPHA*(TG(KP)-TREF) + ALPHA*(TG(KP)-TREF)*DNU
     &           /NU1M)
        ELSE
          TERMT1 = -(ALPHA )*(TRA+E*TRADE)/NU1M
          TERMT2 = 0.D0
        ENDIF
        IF(AXI) THEN
          COEF = NU1*NU1M
          COEF = COEF*COEF
          COEF = (1.D0+2.D0*NU*NU)/COEF
          TERM1 = COEF*TRA*TRA*DNU/2.D0
          TERM2 = TRA*TRADE*(DE*LAM/E+E*COEF*DNU)
C
          TERM3 = -EPSEPS*DNU/(NU12*2.D0)
          TERM4 = EPSDEP*2.D0*(DE/(2.D0*NU1)-DNU*E/(NU12*2.D0))
C
          IF(DERIVE) DDPSI = TERM1 + TERM2 + TERM3 + TERM4
     &                      +TERMT1+ TERMT2
          IF(DERIVF) DDPSI = TERM2 + TERM4 + TERMT1 + TERMT2
        ELSE
          CALL ASSERT(DP.OR.CP)
          IF(DP) THEN
            COEF = NU12*NU1M*NU1M
            COE1 = (2.D0-NU)*NU/COEF
            COE2 = (1.D0-NU)/(2.D0*NU1*NU1M)
            COE3 = E*COE1
            COE4 = (2.D0*NU*NU+1.D0)/COEF
            COE5 = NU/(NU1*NU1M)
            COE6 = E*COE4
          ELSE IF(CP) THEN
            COEF = (1.D0-NU*NU)*(1.D0-NU*NU)
            COE1 = NU/COEF
            COE2 = 0.5D0/((1.D0+NU)*(1.D0-NU))
            COE3 = E*NU/COEF
            COE4 = (NU*NU+1.D0)/COEF
            COE5 = NU/((1.D0+NU)*(1.D0-NU))
            COE6 = E*COE4
          ENDIF
          TERM1 = (EPS(1)*EPS(1)+EPS(2)*EPS(2))*COE1*DNU
          TERM2 = 2.D0*(EPS(1)*DEPSDE(1)+EPS(2)*DEPSDE(2))
     &            *(COE2*DE+COE3*DNU)
          TERM3 = EPS(1)*EPS(2)*COE4*DNU
          TERM4 = (EPS(1)*DEPSDE(2)+EPS(2)*DEPSDE(1))
     &            *(COE5*DE+COE6*DNU)
          TERM5 = -EPS(4)*EPS(4)*DNU/NU12
          TERM6 = 2.D0*EPS(4)*DEPSDE(4)*(DE/NU1-E*DNU/NU12)
C
          IF(DERIVE) DDPSI = TERM1 + TERM2 + TERM3  + TERM4
     &                     + TERM5 + TERM6 + TERMT1 + TERMT2
          IF(DERIVF) DDPSI = TERM2 + TERM4 + TERM6
     &                     + TERMT1 + TERMT2
        ENDIF
C
        PROD = 0.D0
        DO 560 I = 1,NDIM
          PROD = PROD + TGDM(I)*DTDM(I,4)
  560   CONTINUE
        TTHE = TTHE - POIDS*PROD*DDPSI
C =======================================================
C DERIVEE DU TERME FORCE VOLUMIQUE
C =======================================================
          DO 520 I=1,NDIM
            PROD=0.D0
            DO 510 J=1,NDIM
              PROD = PROD + DFDM(I,J)*DTDM(J,4)
510         CONTINUE
            TFOR = TFOR + DUDMDE(I)*(PROD+DFDM(I,4)*DIVT)*POIDS
520       CONTINUE
          IF(DERIVF) THEN
            TFOR = TFOR+POIDS*DIVT*
     &                 (DUDM(1,4)*FLAG(1)+DUDM(2,4)*FLAG(2))
          ENDIF
C
C =======================================================
C DERIVEE DU TERME INITIAL:
C =======================================================
        IF ((ISIGI.NE.0).OR.(IDEPI.NE.0).OR.EPSINI) THEN
          CALL U2MESS('F','SENSIBILITE_54')
          PROD=0.D0
          DO 670 I=1,NCMP
            DO 660 J=1,NDIM
              PROD=PROD+(DSIGDE(I)*DEPSIN(I,J)
     &                  -DEPSDE(I)*DSIGIN(I,J))*DTDM(J,4)
660         CONTINUE
670       CONTINUE
          TINI = TINI + PROD*POIDS
        ENDIF
C ==================================================================
C FIN DE BOUCLE PRINCIPALE SUR LES POINTS DE GAUSS
C ==================================================================
800   CONTINUE

C EXIT EN CAS DE THETA FISSURE NUL PARTOUT
9999  CONTINUE

C ASSEMBLAGE FINAL DES TERMES DE DG
      ZR(IGTHET) = TTHE  + TCLA  + TFOR   + TINI

      CALL JEDEMA()
      END

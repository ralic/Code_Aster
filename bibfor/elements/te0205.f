      SUBROUTINE TE0205(OPTION,NOMTE)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 08/12/2009   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------
C FONCTION REALISEE:

C   CALCUL DE LA DERIVEE DU TAUX DE RESTITUTION D'ENERGIE ELEMENTAIRE
C      GLOBAL PAR RAPPORT AU MODULE D'YOUNG ET AU CHARGEMENT
C      EN ELASTICITE LINEAIRE ET NON LINEAIRE
C      ELEMENTS ISOPARAMETRIQUES 3D

C   OPTION : 'CALC_DGG_E'    (DG/DE AVEC CHARGES REELLES)
C            'CALC_DGG_E_F'  (DG/DE AVEC CHARGES FONCTIONS)
C            'CALC_DGG_FORC'    (DG/DF AVEC CHARGES REELLES)
C            'CALC_DGG_FORC_F'  (DG/DF AVEC CHARGES FONCTIONS)

C ENTREES  ---> OPTION : OPTION DE CALCUL
C          ---> NOMTE  : NOM DU TYPE ELEMENT
C   -------------------------------------------------------------------
C CORPS DU PROGRAMME
C TOLE CRP_20
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      CHARACTER*16 OPTION,NOMTE

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

C DECLARATION VARIABLES LOCALES
      INTEGER IPOIDS,IVF,IDFDE,IJ,IJ1
      INTEGER ICOMP,IGEOM,ITEMPS,IDEPL,IMATE
      INTEGER IEPSR,IEPSF,ISIGI,IDEPI,IDEPSE,IDEFSE,IFOSSR,IFOSSF
      INTEGER IFORC,IFORF,ITHET,IGTHET,IROTA,IPESA,IER,ISIGSE
      INTEGER JGANO,NNO,NNOS,NPG,NCMP
      INTEGER I,J,K,KK,L,M,KP,NDIM,COMPT,IRET,IRET1,IRET2

      REAL*8 EPSI,RAC2,R8PREM,CRIT(3),E,NU,DPSIDE,LAM,MU,NU1M
      REAL*8 DFDI(81),F(3,3),SR(3,3),DSRDE(3,3),DUDPDE(3,3)
      REAL*8 EPS(6),EPSIN(6),DEPSIN(6,3)
      REAL*8 EPSINO(162),EPSIPG(162),FNO(81),DFNO(81),EPSNO(162)
      REAL*8 SIGL(6),SIGIN(6),DSIGIN(6,3),DSIGDE(6),DEPSDE(6)
      REAL*8 THET,TREF,TG(27),EPSTHE(27),TGD(20),TGDM(3),DUDMDE(3)
      REAL*8 FLAG(3),PROD,PROD1,PROD2,DIVT,VALPAR(4),VALRES(3),DEVRES(3)
      REAL*8 TCLA,TTHE,TFOR,TINI,POIDS,RBID,DE,DNU,DALPHA,DDPSI
      REAL*8 TRA,TRADE,TERM1,TERM2,TERM3,TERM4,TERM5,TERM6,ALPHA
      REAL*8 DUDM(3,4),DFDM(3,4),DTDM(3,4),DER(4),NU1,NU12,COEF
      REAL*8 ENERGI(2),RHO,OM,OMO,EPSEPS,EPSDEP
      REAL*8 RPIPO,T1PIPO(6),T2PIPO(2),T3PIPO(6)

      LOGICAL GRAND,FONC,INCR,EPSINI,LPIPO,DERIVE,DERIVF,DERFOR

      CHARACTER*2 CODRET,CODRES(3)
      CHARACTER*4 FAMI
      CHARACTER*8 NOMPAR(4),TYPMOD(2),NOMRES(3)
      CHARACTER*16 COMPOR(4),OPRUPT,PHENOM

C DEB ------------------------------------------------------------------
      DATA NOMRES/'E','NU','ALPHA'/

C INITIALISATIONS POUR APPEL A NMELNL
      LPIPO = .FALSE.
      RPIPO = 0.D0
      T2PIPO(1) = 0.D0
      T2PIPO(2) = 0.D0
      DO 10 I = 1,6
        T1PIPO(I) = 0.D0
        T3PIPO(I) = 0.D0
   10 CONTINUE

      CALL JEMARQ()
      EPSI = R8PREM()
      RAC2 = SQRT(2.D0)
      OPRUPT = 'RUPTURE'
      DERFOR = .FALSE.
      EPSINI = .FALSE.
      TYPMOD(1) = '3D      '

      FAMI = 'RIGI'
      CALL ELREF4(' ',FAMI,NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
      DO 20 I = 1,NDIM
   20 CONTINUE
      NCMP = 2*NDIM

C - PAS DE CALCUL DE G POUR LES ELEMENTS OU LA VALEUR DE THETA EST NULLE
      CALL JEVECH('PTHETAR','L',ITHET)
      CALL JEVECH('PGTHETA','E',IGTHET)
      TCLA = 0.D0
      TTHE = 0.D0
      TFOR = 0.D0
      TINI = 0.D0
      COMPT = 0
      DO 40 I = 1,NNO
        THET = 0.D0
        DO 30 J = 1,NDIM
          THET = THET + ABS(ZR(ITHET+NDIM* (I-1)+J-1))
   30   CONTINUE
        IF (THET.LT.EPSI) COMPT = COMPT + 1
   40 CONTINUE

      IF (COMPT.EQ.NNO) GO TO 650

      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PDEPLAR','L',IDEPL)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PCOMPOR','L',ICOMP)
      DO 50 I = 1,4
        COMPOR(I) = ZK16(ICOMP+I-1)
   50 CONTINUE
C
C   SI CALCUL DERIVEE
C
      DERIVE = .FALSE.
      DERIVF = .FALSE.
      IF (OPTION(1:10).EQ.'CALC_DGG_E') THEN
        DERIVE = .TRUE.
        IF (COMPOR(1)(1:4).NE.'ELAS')
     &    CALL U2MESK('F','SENSIBILITE_15', 1, COMPOR(1))
      ENDIF
      IF (OPTION(1:13).EQ.'CALC_DGG_FORC') THEN
        DERIVF = .TRUE.
        IF (COMPOR(1)(1:4).NE.'ELAS')
     &    CALL U2MESK('F','SENSIBILITE_15', 1, COMPOR(1))
      ENDIF
        CALL JEVECH('PDEPLSE','L',IDEPSE)
        CALL JEVECH('PDEFOSE','L',IDEFSE)
        CALL JEVECH('PSIGMSE','L',ISIGSE)
C
      IF (OPTION.EQ.'CALC_DGG_E_F'
     &      .OR.OPTION.EQ.'CALC_DGG_FORC_F') THEN
        FONC = .TRUE.
        CALL JEVECH('PFFVOLU','L',IFORF)
        CALL JEVECH('PTEMPSR','L',ITEMPS)
        NOMPAR(1) = 'X'
        NOMPAR(2) = 'Y'
        NOMPAR(3) = 'Z'
        NOMPAR(4) = 'INST'
        VALPAR(4) = ZR(ITEMPS)
        CALL TECACH('ONN','PEPSINF',1,IEPSF,IRET)
        IF (IEPSF.NE.0) EPSINI = .TRUE.
        IF(OPTION.EQ.'CALC_DGG_FORC_F') THEN
          CALL JEVECH('PFFVOSS','L',IFOSSF)
          DERFOR = .TRUE.
        ENDIF
      ELSE
        FONC = .FALSE.
        CALL JEVECH('PFRVOLU','L',IFORC)
        CALL TECACH('ONN','PEPSINR',1,IEPSR,IRET)
        IF (IEPSR.NE.0) EPSINI = .TRUE.
        IF(OPTION.EQ.'CALC_DGG_FORC_F') THEN
          CALL JEVECH('PFRVOSS','L',IFOSSR)
          DERFOR = .TRUE.
        ENDIF
      END IF
      GRAND = COMPOR(3).EQ. 'GROT_GDEP'
      INCR = COMPOR(4) (1:9) .EQ. 'COMP_INCR'
      CALL TECACH('ONN','PPESANR',1,IPESA,IRET)
      CALL TECACH('ONN','PROTATR',1,IROTA,IRET)
      CALL TECACH('ONN','PSIGINR',1,ISIGI,IRET)
      CALL TECACH('ONN','PDEPINR',1,IDEPI,IRET)

      IF ((ISIGI.NE.0) .AND. EPSINI) THEN
        CALL U2MESS('F','RUPTURE1_20')
      END IF

      IF (INCR) THEN
        CALL U2MESS('F','SENSIBILITE_55')
      ENDIF

      DO 60 I = 1,NCMP*NNO
        EPSINO(I) = 0.D0
   60 CONTINUE

C - RECUPERATION DES CHARGES ET DEFORMATIONS INITIALES ----------------

      IF (FONC) THEN
        DO 100 I = 1,NNO
          DO 70 J = 1,NDIM
            VALPAR(J) = ZR(IGEOM+NDIM* (I-1)+J-1)
   70     CONTINUE
          DO 80 J = 1,NDIM
            KK = NDIM* (I-1) + J
            CALL FOINTE('FM',ZK8(IFORF+J-1),4,NOMPAR,VALPAR,FNO(KK),IER)
   80     CONTINUE
          IF (EPSINI) THEN
            DO 90 J = 1,NCMP
              KK = NCMP* (I-1) + J
              CALL FOINTE('FM',ZK8(IEPSF+J-1),4,NOMPAR,VALPAR,
     &                    EPSINO(KK),IER)
   90       CONTINUE
          END IF
          IF (DERFOR) THEN
            DO 41 J=1,NDIM
              KK = NDIM*(I-1)+J
C DFNO :INDICATEUR DE LA(OU DES) CMP DES DERIVEES SENSIBLES % AUX FORCES
C       DFNO(KK) = 0 OU 1
              CALL FOINTE('FM',ZK8(IFOSSF+J-1),3,NOMPAR,VALPAR,DFNO(KK)
     &                    ,IER)
41          CONTINUE
          ENDIF
  100   CONTINUE
      ELSE
        DO 130 I = 1,NNO
          DO 110 J = 1,NDIM
            FNO(NDIM* (I-1)+J) = ZR(IFORC+NDIM* (I-1)+J-1)
  110     CONTINUE
          IF (EPSINI) THEN
            DO 120 J = 1,3
              EPSINO(NCMP* (I-1)+J) = ZR(IEPSR+NCMP* (I-1)+J-1)
              EPSINO(NCMP* (I-1)+J+3) = ZR(IEPSR+NCMP* (I-1)+J-1+3)*RAC2
  120       CONTINUE
          END IF
          IF (DERFOR) THEN
            IJ = NDIM*(I-1)
            IJ1 = IFOSSR+IJ-1
            DO 42 J=1,NDIM
              DFNO(IJ+J)= ZR(IJ1+J)
42          CONTINUE
          ENDIF
  130   CONTINUE
      END IF

C - PESANTEUR ET ROTATION ----------------

      IF ((IPESA.NE.0) .OR. (IROTA.NE.0)) THEN
        CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRET)
        CALL RCVALB('RIGI',1,1,'+',ZI(IMATE),' ',PHENOM,
     &              1,' ',RBID,1,'RHO',RHO,
     &              CODRET,'FM')
        IF (IPESA.NE.0) THEN
          DO 150 I = 1,NNO
            DO 140 J = 1,NDIM
              KK = NDIM* (I-1) + J
              FNO(KK) = FNO(KK) + RHO*ZR(IPESA)*ZR(IPESA+J)
  140       CONTINUE
  150     CONTINUE
        END IF
        IF (IROTA.NE.0) THEN
          OM = ZR(IROTA)
          DO 180 I = 1,NNO
            OMO = 0.D0
            DO 160 J = 1,NDIM
              OMO = OMO + ZR(IROTA+J)*ZR(IGEOM+NDIM* (I-1)+J-1)
  160       CONTINUE
            DO 170 J = 1,NDIM
              KK = NDIM* (I-1) + J
              FNO(KK) = FNO(KK) + RHO*OM*OM*
     &                  (ZR(IGEOM+KK-1)-OMO*ZR(IROTA+J))
  170       CONTINUE
  180     CONTINUE
        END IF
      END IF

C - DEPLACEMENT INITIAL ----------------

      IF (IDEPI.NE.0) THEN
        DO 200 KP = 1,NPG
          L = (KP-1)*NNO
          CALL NMGEOM(NDIM,NNO,.FALSE.,GRAND,ZR(IGEOM),KP,
     &                IPOIDS,IVF,IDFDE,
     &                ZR(IDEPI),RBID,DFDI,F,EPS,RBID)
          DO 190 I = 1,NCMP
            EPSIPG((KP-1)*NCMP+I) = EPS(I)
  190     CONTINUE
  200   CONTINUE

        CALL PPGAN2(JGANO,NCMP,EPSIPG,EPSNO)
        DO 210 I = 1,NNO*NCMP
          EPSINO(I) = EPSINO(I) + EPSNO(I)
  210   CONTINUE
      END IF

C ======================================================================

C - BOUCLE SUR LES POINTS DE GAUSS ----------------
      CALL RCVARC(' ','TEMP','REF','RIGI',1,1,TREF,IRET)

      DO 645 KP = 1,NPG
        CALL VERIFT('RIGI',KP,1,'+',ZI(IMATE),'ELAS',1,EPSTHE(KP),IRET)
        CALL RCVARC(' ','TEMP','+','RIGI',KP,1,TG(KP),IRET1)
  645 CONTINUE

      DO 646 KP = 1,NNO
        CALL RCVARC(' ','TEMP','+','NOEU',KP,1,TGD(KP),IRET2)
  646 CONTINUE

      DO 640 KP = 1,NPG
        L = (KP-1)*NNO
        DO 240 I = 1,3
          FLAG(I) = 0.D0
          TGDM(I) = 0.D0
          DUDMDE(I) = 0.D0
          DO 220 J = 1,3
            SR(I,J) = 0.D0
            DUDPDE(I,J) = 0.D0
  220     CONTINUE
          DO 230 J = 1,4
            DUDM(I,J) = 0.D0
            DTDM(I,J) = 0.D0
            DFDM(I,J) = 0.D0
  230     CONTINUE
  240   CONTINUE
        DO 260 I = 1,6
          DSIGDE(I) = 0.D0
          DEPSDE(I) = 0.D0
          SIGL(I) = 0.D0
          SIGIN(I) = 0.D0
          EPSIN(I) = 0.D0
          EPS(I) = 0.D0
          DO 250 J = 1,3
            DSIGIN(I,J) = 0.D0
            DEPSIN(I,J) = 0.D0
  250     CONTINUE
  260   CONTINUE
C ===========================================
C STOCKAGE DES DERIVEES DES CONTRAINTES ET DES DEFORMATIONS
C ===========================================
         DO 313 J=1,NCMP
           DSIGDE(J) =  ZR(ISIGSE+NCMP*(KP-1)+J-1)
           DEPSDE(J) =  ZR(IDEFSE+NCMP*(KP-1)+J-1)
313      CONTINUE

C - CALCUL DES ELEMENTS GEOMETRIQUES
        CALL NMGEOM(NDIM,NNO,.FALSE.,GRAND,ZR(IGEOM),KP,IPOIDS,
     &              IVF,IDFDE,ZR(IDEPL),
     &              POIDS,DFDI,F,EPS,RBID)

C - CALCULS DES GRADIENTS DE U (DUDM),THETA (DTDM) ET FORCE(DFDM)
C   DE LA TEMPERATURE AUX POINTS DE GAUSS (TG) ET SON GRADIENT (TGDM)

        DO 290 I = 1,NNO
          DER(1) = DFDI(I)
          DER(2) = DFDI(I+NNO)
          DER(3) = DFDI(I+2*NNO)
          DER(4) = ZR(IVF+L+I-1)
          DO 280 J = 1,NDIM
            TGDM(J) = TGDM(J) + TGD(I)*DER(J)
            DO 270 K = 1,NDIM
              DUDM(J,K) = DUDM(J,K) + ZR(IDEPL+NDIM* (I-1)+J-1)*DER(K)
              DTDM(J,K) = DTDM(J,K) + ZR(ITHET+NDIM* (I-1)+J-1)*DER(K)
              DFDM(J,K) = DFDM(J,K) + FNO(NDIM* (I-1)+J)*DER(K)
  270       CONTINUE
            DUDM(J,4) = DUDM(J,4) + ZR(IDEPL+NDIM* (I-1)+J-1)*DER(4)
            DTDM(J,4) = DTDM(J,4) + ZR(ITHET+NDIM* (I-1)+J-1)*DER(4)
            DFDM(J,4) = DFDM(J,4) + FNO(NDIM* (I-1)+J)*DER(4)
            IF(DERFOR) FLAG(J) = FLAG(J) + DFNO(NDIM* (I-1)+J)*DER(4)
  280     CONTINUE
          DO 311 J=1,NDIM
            DUDMDE(J) = DUDMDE(J) + ZR(IDEPSE+NDIM*(I-1)+J-1)*DER(4)
            DO 312 K=1,NDIM
              DUDPDE(J,K)=DUDPDE(J,K)+ZR(IDEPSE+NDIM*(I-1)+J-1)*DER(K)
  312       CONTINUE
  311     CONTINUE
  290   CONTINUE

C -  DEFORMATIONS INITIALES

        IF ((IDEPI.NE.0) .OR. EPSINI) THEN
          DO 410 I = 1,NNO
            DER(1) = DFDI(I)
            DER(2) = DFDI(I+NNO)
            DER(3) = DFDI(I+2*NNO)
            DER(4) = ZR(IVF+L+I-1)
            DO 380 J = 1,NCMP
              EPSIN(J) = EPSIN(J) + EPSINO(NCMP* (I-1)+J)*DER(4)
  380       CONTINUE
            DO 400 J = 1,NCMP
              DO 390 K = 1,NDIM
                DEPSIN(J,K) = DEPSIN(J,K) + EPSINO(NCMP* (I-1)+J)*DER(K)
  390         CONTINUE
  400       CONTINUE
  410     CONTINUE
          DO 420 I = 1,NCMP
            EPS(I) = EPS(I) - EPSIN(I)
  420     CONTINUE
        END IF

C - CALCUL DES CONTRAINTES

          CRIT(1) = 300
          CRIT(2) = 0.D0
          CRIT(3) = 1.D-3
          CALL NMELNL(FAMI,KP,1,'+',NDIM,TYPMOD,ZI(IMATE),COMPOR,CRIT,
     &                OPRUPT,EPS,SIGL,RBID,RBID,ENERGI,LPIPO,RPIPO,
     &                T1PIPO,T2PIPO,T3PIPO)
          DIVT = DTDM(1,1) + DTDM(2,2) + DTDM(3,3)

C  - CONTRAINTES INITIALES

        IF (ISIGI.NE.0) THEN
          DO 470 I = 1,NNO
            DER(1) = DFDI(I)
            DER(2) = DFDI(I+NNO)
            DER(3) = DFDI(I+2*NNO)
            DER(4) = ZR(IVF+L+I-1)
            DO 440 J = 1,NCMP
              SIGIN(J) = SIGIN(J) + ZR(ISIGI+NCMP* (I-1)+J-1)*DER(4)
  440       CONTINUE
            DO 460 J = 1,NCMP
              DO 450 K = 1,NDIM
                DSIGIN(J,K) = DSIGIN(J,K) +
     &                        ZR(ISIGI+NCMP* (I-1)+J-1)*DER(K)
  450         CONTINUE
  460       CONTINUE
  470     CONTINUE
          DO 490 I = 4,NCMP
            SIGIN(I) = SIGIN(I)*RAC2
            DO 480 J = 1,NDIM
              DSIGIN(I,J) = DSIGIN(4,1)*RAC2
  480       CONTINUE
  490     CONTINUE
          DO 500 I = 1,NCMP
            SIGL(I) = SIGL(I) + SIGIN(I)
  500     CONTINUE
          DO 510 I = 1,NCMP
            ENERGI(1) = ENERGI(1) + (EPS(I)+0.5D0*EPSIN(I))*SIGIN(I)
  510     CONTINUE
        END IF

C  - STOCKAGE DES CONTRAINTES ET DES DERIVEES DES CONTRAINTES

          DSRDE(1,1)= DSIGDE(1)
          DSRDE(2,2)= DSIGDE(2)
          DSRDE(3,3)= DSIGDE(3)
          DSRDE(1,2)= DSIGDE(4)
          DSRDE(1,3)= DSIGDE(5)
          DSRDE(2,3)= DSIGDE(6)
          DSRDE(2,1)= DSRDE(1,2)
          DSRDE(3,1)= DSRDE(1,3)
          DSRDE(3,2)= DSRDE(2,3)
          SR(1,1)= SIGL(1)
          SR(2,2)= SIGL(2)
          SR(3,3)= SIGL(3)
          SR(1,2)= SIGL(4)/RAC2
          SR(1,3)= SIGL(5)/RAC2
          SR(2,3)= SIGL(6)/RAC2
          SR(2,1)= SR(1,2)
          SR(3,1)= SR(1,3)
          SR(3,2)= SR(2,3)
          EPS(4)= EPS(4)/RAC2
          EPS(5)= EPS(5)/RAC2
          EPS(6)= EPS(6)/RAC2

C ++++++++++++++++++++++++++++++++++++++++++++++++++++
C + CALCUL DE LA DERIVEE DE G PAR RAPPORT A E (OU F) +
C ++++++++++++++++++++++++++++++++++++++++++++++++++++

C DERIVEE DU TERME THERMOELASTIQUE CLASSIQUE
C ==========================================

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
          CALL RCVAD2 (FAMI,KP,1,'+',ZI(IMATE),'ELAS',
     &                 3,NOMRES,VALRES,DEVRES,CODRES)
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
C
          LAM=NU*E/((1.D0+NU)*(1.D0-2.D0*NU))
          MU=E/((1.D0+NU)*2.D0)
          TRA = EPS(1)+EPS(2)+EPS(3)
          TRADE = DEPSDE(1)+DEPSDE(2)+DEPSDE(3)
          TERM1 = TRA*TRA*LAM/(2.D0*E)
          TERM2 = TRA*TRADE*LAM
          EPSEPS = 0.D0
          EPSDEP = 0.D0
          DO 545 I=1,3
            EPSEPS = EPSEPS + EPS(I)*EPS(I)
            EPSDEP = EPSDEP + EPS(I)*DEPSDE(I)
  545    CONTINUE
          DO 546 I=4,6
            EPSEPS = EPSEPS + 2.D0*EPS(I)*EPS(I)
            EPSDEP = EPSDEP + 2.D0*EPS(I)*DEPSDE(I)
  546     CONTINUE
          TERM3 = EPSEPS*MU/E
          TERM4 = EPSDEP*MU*2.D0
          TERM5 = -EPSTHE(KP)*E*TRADE/(1.D0-2.D0*NU)
          TERM6 = -EPSTHE(KP)*(TRA-1.5D0*EPSTHE(KP))
     &            /(1.D0-2.D0*NU)
          IF(DERIVE) DPSIDE = TERM1 + TERM2 + TERM3 + TERM4 + TERM5
     &                       +TERM6
          IF(DERIVF) DPSIDE = TERM2 + TERM4 + TERM5
C
          TCLA = TCLA + POIDS* (PROD-DPSIDE*DIVT)

C DERIVEE DU TERME THERMIQUE :   -(D(ENER)/DT)(GRAD(T).THETA)
C ==========================

          NU1 = 1.D0+NU
          NU1M = 1.D0-2.D0*NU
          NU12 = NU1*NU1
          COEF = NU1*NU1M
          COEF = COEF*COEF
          COEF = (1.D0+2.D0*NU*NU)/COEF
          TERM1 = COEF*TRA*TRA*DNU/2.D0
          TERM2 = TRA*TRADE*(DE*LAM/E+E*COEF*DNU)
C
          TERM3 = -EPSEPS*DNU/(NU12*2.D0)
          TERM4 = EPSDEP*2.D0*(DE/(2.D0*NU1)-DNU*E/(NU12*2.D0))
C
          IF (IRET1.EQ.0) THEN
              TERM5 = -(ALPHA + DALPHA*(TG(KP)-TREF))*(TRA+E*TRADE)/NU1M
     &           -(EPSTHE(KP)/NU1M)*(2.D0*DNU*TRA/NU1M
     &           +(DE+2.D0*E*DNU/NU1M)*TRADE)
              TERM6 = ((3.D0*EPSTHE(KP))/NU1M)*
     &             (ALPHA + DALPHA*(TG(KP)-TREF) + EPSTHE(KP)
     &             *DNU/NU1M)
          ELSE
            TERM5 = 0.D0
            TERM6 = 0.D0
          ENDIF
          IF(DERIVE) DDPSI = TERM1 + TERM2 + TERM3 + TERM4 +TERM5 +TERM6
          IF(DERIVF) DDPSI = TERM2 + TERM4 + TERM5 + TERM6
C
          PROD = 0.D0
          IF (IRET.EQ.0) THEN
            DO 560 I = 1,NDIM
              PROD = PROD + TGDM(I)*DTDM(I,4)
  560       CONTINUE
          ENDIF
          TTHE = TTHE - POIDS*PROD*DDPSI

C DERIVEE DU TERME FORCE VOLUMIQUE
C ================================

        DO 580 I = 1,NDIM
          PROD = 0.D0
          DO 570 J = 1,NDIM
            PROD = PROD + DFDM(I,J)*DTDM(J,4)
  570     CONTINUE
          TFOR = TFOR + DUDMDE(I)*(PROD+DFDM(I,4)*DIVT)*POIDS
          IF(DERIVF) THEN
            TFOR = TFOR+POIDS*DIVT*
     &           (DUDM(1,4)*FLAG(1)+DUDM(2,4)*FLAG(2)+DUDM(3,4)*FLAG(3))
          ENDIF
  580   CONTINUE

C DERIVEE DU TERME DEFORMATIONS INITIALES
C =======================================

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

  640 CONTINUE
  650 CONTINUE

      ZR(IGTHET) = TTHE + TCLA + TFOR + TINI

      CALL JEDEMA()
      END

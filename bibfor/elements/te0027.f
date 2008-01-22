      SUBROUTINE TE0027(OPTION,NOMTE)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 22/01/2008   AUTEUR REZETTE C.REZETTE 
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
C TOLE CRP_20
C-----------------------------------------------------------------------
C FONCTION REALISEE:

C      CALCUL DU TAUX DE RESTITUTION D'ENERGIE ELEMENTAIRE
C      EN ELASTICITE LINEAIRE ET NON LINEAIRE
C      ELEMENTS ISOPARAMETRIQUES 3D

C      OPTION : 'CALC_G'          (LOCAL,CHARGES REELLES)
C               'CALC_G_F'        (LOCAL,CHARGES FONCTIONS)
C               'CALC_G_GLOB'     (GLOBAL,CHARGES REELLES)
C               'CALC_G_GLOB_F'   (GLOBAL,CHARGES FONCTIONS)

C ENTREES  ---> OPTION : OPTION DE CALCUL
C          ---> NOMTE  : NOM DU TYPE ELEMENT
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       11/12/00 (OB): DEPLACEMENT DU TEST DE LA NULLITE DU THETAFISS,
C                      TOILETTAGE FORTRAN,
C                      RAJOUT DE PARAMETRE DANS L'APPEL A NMELNL POUR
C                      COHERENCE AVEC TE0096 ET OPTION='CALC_DG' EN 2D.
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
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
      INTEGER IPOIDS,IVF,IDFDE
      INTEGER ICOMP,IGEOM,ITEMPS,IDEPL,IMATE
      INTEGER IEPSR,IEPSF,ISIGI,IDEPI,ISIGM,IEPSP,IVARI
      INTEGER IFORC,IFORF,ITHET,IGTHET,IROTA,IPESA,IER
      INTEGER JGANO,NNO,NNOS,NPG,NCMP
      INTEGER I,J,K,KK,L,M,KP,NDIM,COMPT,NBVARI,IRET
      INTEGER IVITES,IACCEL,J1,J2

      REAL*8 EPSI,RAC2,R8PREM,CRIT(3)
      REAL*8 DFDI(81),F(3,3),SR(3,3)
      REAL*8 EPS(6),EPSIN(6),DEPSIN(6,3),EPSP(6),DEPSP(6,3)
      REAL*8 EPSINO(162),EPSIPG(162),FNO(81),EPSNO(162)
      REAL*8 SIGL(6),SIGIN(6),DSIGIN(6,3)
      REAL*8 THET,TGDM(3),TGD(20)
      REAL*8 PROD,PROD1,PROD2,DIVT,VALPAR(4)
      REAL*8 TCLA,TTHE,TFOR,TPLAS,TINI,POIDS,RBID
      REAL*8 DUDM(3,4),DFDM(3,4),DTDM(3,4),DER(4),DVDM(3,4)
      REAL*8 P,PPG,DPDM(3),RP,ENERGI(2),RHO,OM,OMO
      REAL*8 RPIPO,T1PIPO(6),T2PIPO(2),T3PIPO(6)
      REAL*8 ECIN,PROD3,PROD4,ACCELE(3)

      LOGICAL GRAND,FONC,INCR,EPSINI,LPIPO

      CHARACTER*2 CODRET
      CHARACTER*4 FAMI
      CHARACTER*8 NOMPAR(4),TYPMOD(2)
      CHARACTER*16 COMPOR(4),OPRUPT,PHENOM

C DEB ------------------------------------------------------------------


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
      EPSINI = .FALSE.
      TYPMOD(1) = '3D      '

      FAMI = 'RIGI'
      CALL ELREF4(' ',FAMI,NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)

      NCMP = 2*NDIM

C - PAS DE CALCUL DE G POUR LES ELEMENTS OU LA VALEUR DE THETA EST NULLE
      CALL JEVECH('PTHETAR','L',ITHET)
      CALL JEVECH('PGTHETA','E',IGTHET)
      TCLA = 0.D0
      TTHE = 0.D0
      TFOR = 0.D0
      TPLAS = 0.D0
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

      IVITES = 0
      IACCEL = 0

      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PDEPLAR','L',IDEPL)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PCOMPOR','L',ICOMP)
      IF (OPTION.EQ.'CALC_G_F'.OR.
     &    OPTION.EQ.'CALC_G_GLOB_F') THEN
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
      ELSE
        FONC = .FALSE.
        CALL JEVECH('PFRVOLU','L',IFORC)
        CALL TECACH('ONN','PEPSINR',1,IEPSR,IRET)
        IF (IEPSR.NE.0) EPSINI = .TRUE.
      END IF
      DO 50 I = 1,4
        COMPOR(I) = ZK16(ICOMP+I-1)
   50 CONTINUE
      GRAND = COMPOR(3) (1:5) .EQ. 'GREEN'
      INCR = COMPOR(4) (1:9) .EQ. 'COMP_INCR'
      READ (ZK16(ICOMP+1),'(I16)') NBVARI
      IF (INCR) THEN
        CALL JEVECH('PCONTRR','L',ISIGM)
        CALL JEVECH('PDEFOPL','L',IEPSP)
        CALL JEVECH('PVARIPR','L',IVARI)
      END IF
      CALL TECACH('ONN','PPESANR',1,IPESA,IRET)
      CALL TECACH('ONN','PROTATR',1,IROTA,IRET)
      CALL TECACH('ONN','PSIGINR',1,ISIGI,IRET)
      CALL TECACH('ONN','PDEPINR',1,IDEPI,IRET)
      IF (OPTION.EQ.'CALC_G'     .OR. OPTION.EQ.'CALC_G_F' .OR.
     &    OPTION.EQ.'CALC_G_GLOB'.OR. OPTION.EQ.'CALC_G_GLOB_F') THEN
        CALL TECACH('ONN','PVITESS',1,IVITES,IRET)
        CALL TECACH('ONN','PACCELE',1,IACCEL,IRET)
      ENDIF

      IF ((ISIGI.NE.0) .AND. EPSINI) THEN
        CALL U2MESS('F','RUPTURE1_20')
      END IF

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
  130   CONTINUE
      END IF

      IF (IVITES.NE.0) THEN
        CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRET)
        CALL RCVALB('RIGI',1,1,'+',ZI(IMATE),' ',PHENOM,
     &              1,' ',RBID,1,'RHO',RHO,
     &              CODRET,'FM')
      ENDIF

      IF ((IPESA.NE.0) .OR. (IROTA.NE.0)) THEN
        CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRET)
        CALL RCVALB('RIGI',1,1,'+',ZI(IMATE),' ',PHENOM,
     &               1,' ',RBID,1,'RHO',RHO,
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

C CALCUL DU GRADIENT DE TEMPERATURE :
C SI LA TEMPERATURE N'EXISTE PAS, ON LUI IMPOSE UNE VALEUR NULLE
      DO 645 KP = 1,NNO
        CALL RCVARC(' ','TEMP','+','NOEU',KP,1,TGD(KP),IRET)
        IF (IRET.EQ.1) TGD(KP) = 0.D0
  645 CONTINUE

      DO 640 KP = 1,NPG
        L = (KP-1)*NNO
        PPG = 0.D0
        DO 240 I = 1,3
          TGDM(I) = 0.D0
          DPDM(I) = 0.D0
          ACCELE(I) = 0.D0
          DO 220 J = 1,3
            SR(I,J) = 0.D0
  220     CONTINUE
          DO 230 J = 1,4
            DUDM(I,J) = 0.D0
            DVDM(I,J) = 0.D0
            DTDM(I,J) = 0.D0
            DFDM(I,J) = 0.D0
  230     CONTINUE
  240   CONTINUE
        DO 260 I = 1,6
          SIGL(I) = 0.D0
          SIGIN(I) = 0.D0
          EPSIN(I) = 0.D0
          EPSP(I) = 0.D0
          EPS(I) = 0.D0
          DO 250 J = 1,3
            DSIGIN(I,J) = 0.D0
            DEPSIN(I,J) = 0.D0
            DEPSP(I,J) = 0.D0
  250     CONTINUE
  260   CONTINUE

C - CALCUL DES ELEMENTS GEOMETRIQUES

        CALL NMGEOM(NDIM,NNO,.FALSE.,GRAND,ZR(IGEOM),KP,IPOIDS,
     &              IVF,IDFDE,ZR(IDEPL),
     &              POIDS,DFDI,F,EPS,RBID)

C - CALCULS DES GRADIENTS DE U (DUDM),THETA (DTDM) ET FORCE(DFDM)
C   DU GRADIENT (TGDM) DE LA TEMPERATURE AUX POINTS DE GAUSS (TG)

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
            IF (IVITES.NE.0) THEN
              DO 305 K=1,NDIM
                DVDM(J,K) = DVDM(J,K) + ZR(IVITES+NDIM*(I-1)+J-1)*DER(K)
  305         CONTINUE
              DVDM(J,4) = DVDM(J,4) + ZR(IVITES+NDIM*(I-1)+J-1)*DER(4)
              ACCELE(J) = ACCELE(J) + ZR(IACCEL+NDIM*(I-1)+J-1)*DER(4)
            ENDIF
            DUDM(J,4) = DUDM(J,4) + ZR(IDEPL+NDIM* (I-1)+J-1)*DER(4)
            DTDM(J,4) = DTDM(J,4) + ZR(ITHET+NDIM* (I-1)+J-1)*DER(4)
            DFDM(J,4) = DFDM(J,4) + FNO(NDIM* (I-1)+J)*DER(4)
  280     CONTINUE
  290   CONTINUE

C - CALCULS DES GRADIENTS DE P (DPDM) ET EPSP (DEPSP) EN PLASTICITE

        IF (INCR) THEN
          DO 340 I = 1,NNO
            DER(1) = DFDI(I)
            DER(2) = DFDI(I+NNO)
            DER(3) = DFDI(I+2*NNO)
            DER(4) = ZR(IVF+L+I-1)
            P = ZR(IVARI+ (I-1)*NBVARI)
            PPG = PPG + P*DER(4)
            DO 300 J = 1,NCMP
              EPSP(J) = EPSP(J) + ZR(IEPSP+NCMP* (I-1)+J-1)*DER(4)
  300       CONTINUE
            IF (P.GE.EPSI) THEN
              DO 310 J = 1,NDIM
                DPDM(J) = DPDM(J) + ZR(IVARI+ (I-1)*NBVARI)*DER(J)
  310         CONTINUE
              DO 330 K = 1,NDIM
                DO 320 J = 1,NCMP
                  DEPSP(J,K) = DEPSP(J,K) +
     &                         ZR(IEPSP+NCMP* (I-1)+J-1)*DER(K)
  320           CONTINUE
  330         CONTINUE
            END IF
  340     CONTINUE
          DO 360 I = 4,NCMP
            EPSP(I) = EPSP(I)*RAC2
            DO 350 J = 1,NDIM
              DEPSP(I,J) = DEPSP(I,J)*RAC2
  350       CONTINUE
  360     CONTINUE
          IF (PPG.LT.EPSI) THEN
            PPG = 0.D0
            DO 370 J = 1,NCMP
              EPSP(J) = 0.D0
  370       CONTINUE
          END IF
        END IF

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

C -  CONTRAINTES LAGRANGIENNES (SIGL),ENERGIE LIBRE ET DERIVEE / T

        IF (INCR) THEN
          CALL NMPLRU(FAMI,KP,1,'+',NDIM,TYPMOD,ZI(IMATE),COMPOR,
     &                PPG,EPS,EPSP,RP,ENERGI)
          DO 430 I = 1,3
            SIGL(I) = ZR(ISIGM+NCMP* (KP-1)+I-1)
            SIGL(I+3) = ZR(ISIGM+NCMP* (KP-1)+I-1+3)*RAC2
  430     CONTINUE
        ELSE
          CRIT(1) = 300
          CRIT(2) = 0.D0
          CRIT(3) = 1.D-3
          CALL NMELNL(FAMI,KP,1,'+',NDIM,TYPMOD,ZI(IMATE),COMPOR,CRIT,
     &                OPRUPT,EPS,SIGL,RBID,RBID,ENERGI,LPIPO,RPIPO,
     &                T1PIPO,T2PIPO,T3PIPO)
        END IF
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

        SR(1,1) = SIGL(1)
        SR(2,2) = SIGL(2)
        SR(3,3) = SIGL(3)
        SR(1,2) = SIGL(4)/RAC2
        SR(1,3) = SIGL(5)/RAC2
        SR(2,3) = SIGL(6)/RAC2
        SR(2,1) = SR(1,2)
        SR(3,1) = SR(1,3)
        SR(3,2) = SR(2,3)

C - CALCUL DE G

C - TERME THERMOELASTIQUE CLASSIQUE F.SIG:(GRAD(U).GRAD(THET))-ENER*DIVT

        ECIN  = 0.D0
        PROD3 = 0.D0
        PROD4 = 0.D0
        IF (IVITES.NE.0) THEN
          DO 487 J1 = 1, NDIM
            ECIN = ECIN + DVDM(J1,4)*DVDM(J1,4)
  487     CONTINUE
          DO 496 J1 = 1, NDIM
            DO 497 J2 = 1, NDIM
              PROD3 = PROD3 + ACCELE(J1)*DUDM(J1,J2)*DTDM(J2,4)
              PROD4 = PROD4 + DVDM(J1,4)*DVDM(J1,J2)*DTDM(J2,4)
  497       CONTINUE
  496     CONTINUE
          ECIN  = 0.5D0*RHO*ECIN
          PROD3 = RHO*PROD3
          PROD4 = RHO*PROD4
        ENDIF

        PROD = 0.D0
        DO 550 I = 1,3
          DO 540 J = 1,3
            DO 530 K = 1,3
              DO 520 M = 1,3
                PROD = PROD + F(I,J)*SR(J,K)*DUDM(I,M)*DTDM(M,K)
  520         CONTINUE
  530       CONTINUE
  540     CONTINUE
  550   CONTINUE
        PROD = PROD - ECIN*DIVT + PROD3 - PROD4
        TCLA = TCLA + POIDS* (PROD-ENERGI(1)*DIVT)

C - TERME THERMIQUE :   -(D(ENER)/DT)(GRAD(T).THETA)

        PROD = 0.D0
        DO 560 I = 1,NDIM
          PROD = PROD + TGDM(I)*DTDM(I,4)
  560   CONTINUE
        TTHE = TTHE - POIDS*PROD*ENERGI(2)

C - TERME FORCE VOLUMIQUE

        DO 580 I = 1,NDIM
          PROD = 0.D0
          DO 570 J = 1,NDIM
            PROD = PROD + DFDM(I,J)*DTDM(J,4)
  570     CONTINUE
          TFOR = TFOR + DUDM(I,4)* (PROD+DFDM(I,4)*DIVT)*POIDS
  580   CONTINUE

C - TERME PLASTIQUE :   SIG:(GRAD(EPSP).THETA)- R(P).GRAD(P).THETA

        IF (INCR) THEN
          PROD1 = 0.D0
          PROD2 = 0.D0
          DO 600 I = 1,NCMP
            DO 590 J = 1,NDIM
              PROD1 = PROD1 + SIGL(I)*DEPSP(I,J)*DTDM(J,4)
  590       CONTINUE
  600     CONTINUE
          DO 610 I = 1,NDIM
            PROD2 = PROD2 + RP*DPDM(I)*DTDM(I,4)
  610     CONTINUE
          TPLAS = TPLAS + (PROD1-PROD2)*POIDS
        END IF

C - TERME INITIAL:  SIG:GRAD(EPSIN).THETA-(EPS-EPSIN):GRAD(SIGIN).THETA

        IF ((ISIGI.NE.0) .OR. (IDEPI.NE.0) .OR. EPSINI) THEN
          PROD1 = 0.D0
          PROD2 = 0.D0
          DO 630 I = 1,NCMP
            DO 620 J = 1,NDIM
              PROD1 = PROD1 + (SIGL(I)-0.5D0*SIGIN(I))*DEPSIN(I,J)*
     &                DTDM(J,4)
              PROD2 = PROD2 + (EPS(I)+0.5D0*EPSIN(I))*DSIGIN(I,J)*
     &                DTDM(J,4)
  620       CONTINUE
  630     CONTINUE
          TINI = TINI + (PROD1-PROD2)*POIDS
        END IF

  640 CONTINUE
  650 CONTINUE

      ZR(IGTHET) = TTHE + TCLA + TFOR + TPLAS + TINI
      CALL JEDEMA()
      END

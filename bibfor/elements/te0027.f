      SUBROUTINE TE0027(OPTION,NOMTE)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 28/08/2012   AUTEUR TRAN V-X.TRAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C ----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      INCLUDE 'jeveux.h'
      CHARACTER*16 OPTION,NOMTE


C DECLARATION VARIABLES LOCALES
      INTEGER IPOIDS,IVF,IDFDE
      INTEGER ICOMP,IGEOM,ITEMPS,IDEPL,IMATE
      INTEGER IEPSR,IEPSF,ISIGI,ISIGM,IEPSP,IVARI
      INTEGER IFORC,IFORF,ITHET,IGTHET,IROTA,IPESA,IER
      INTEGER JGANO,NNO,NNOS,NPG,NCMP
      INTEGER I,J,K,KK,L,M,KP,NDIM,COMPT,NBVARI,IRET
      INTEGER IVITES,IACCEL,J1,J2,IRETH,MATCOD

      REAL*8 EPSREF(6),EPSI,RAC2,R8PREM,CRIT(3)
      REAL*8 DFDI(81),F(3,3),SR(3,3)
      REAL*8 EPS(6),EPSIN(6),DEPSIN(6,3),EPSP(6),DEPSP(6,3)
      REAL*8 EPSINO(162),EPSIPG(162),FNO(81),EPSNO(162)
      REAL*8 SIGL(6),SIGIN(6),DSIGIN(6,3)
      REAL*8 THET,TGDM(3),TGD(20)
      REAL*8 PROD,PROD1,PROD2,DIVT,VALPAR(4)
      REAL*8 TCLA,TTHE,TFOR,TPLAS,TINI,POIDS,RBID
      REAL*8 DUDM(3,4),DFDM(3,4),DTDM(3,4),DER(4),DVDM(3,4)
      REAL*8 P,PPG,DPDM(3),RP,ENERGI(2),RHO,OM,OMO
      REAL*8 ECIN,PROD3,PROD4,ACCELE(3),E,NU,MU

      LOGICAL GRAND,FONC,INCR,EPSINI

      INTEGER ICODRE
      CHARACTER*4 FAMI
      CHARACTER*8 NOMPAR(4),TYPMOD(2)
      CHARACTER*16 COMPOR(4),OPRUPT,PHENOM

C DEB ------------------------------------------------------------------

      CALL JEMARQ()

      EPSI = R8PREM()
      RAC2 = SQRT(2.D0)
      OPRUPT = 'RUPTURE'
      EPSINI = .FALSE.
      TYPMOD(1) = '3D      '
      TYPMOD(2) = ' '
C
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

      IF (COMPT.EQ.NNO) GO TO 9999

      IVITES = 0
      IACCEL = 0

      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PDEPLAR','L',IDEPL)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PCOMPOR','L',ICOMP)
      MATCOD = ZI(IMATE)
C RECUPERATION DU CHAMP LOCAL (CARTE) ASSOCIE AU PRE-EPSI
C CE CHAMP EST ISSU D UN CHARGEMENT PRE-EPSI
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
      
C LOI DE COMPORTEMENT      
      DO 50 I = 1,4
        COMPOR(I) = ZK16(ICOMP+I-1)
   50 CONTINUE
      GRAND = COMPOR(3).EQ. 'GROT_GDEP'
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
      IF (OPTION.EQ.'CALC_G'     .OR. OPTION.EQ.'CALC_G_F' .OR.
     &    OPTION.EQ.'CALC_G_GLOB'.OR. OPTION.EQ.'CALC_G_GLOB_F') THEN
        CALL TECACH('ONN','PVITESS',1,IVITES,IRET)
        CALL TECACH('ONN','PACCELE',1,IACCEL,IRET)
      ENDIF
      
      DO 60 I = 1,NCMP*NNO
        EPSINO(I) = 0.D0
   60 CONTINUE
   
C =====================================================================
C MESSAGES D'ERREURS
C =====================================================================

C ON NE PEUT AVOIR SIMULTANEMENT PRE-DEFORMATIONS ET CONTRAINTES INIT.
      IF ((ISIGI.NE.0) .AND. EPSINI) THEN
        CALL U2MESS('F','RUPTURE1_20')
      END IF

C =====================================================================
C RECUPERATION DES CHARGES ET PRE-DEFORMATIONS (CHARGEMENT PRE-EPSI)
C =====================================================================
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
        CALL RCCOMA(MATCOD,'ELAS',PHENOM,ICODRE)
        CALL RCVALB('RIGI',1,1,'+',MATCOD,' ',PHENOM,
     &              1,' ',RBID,1,'RHO',RHO,
     &              ICODRE,1)
      ENDIF


C CORRECTION DES FORCES VOLUMIQUES
      IF ((IPESA.NE.0) .OR. (IROTA.NE.0)) THEN
        CALL RCCOMA(MATCOD,'ELAS',PHENOM,ICODRE)
        CALL RCVALB('RIGI',1,1,'+',MATCOD,' ',PHENOM,
     &               1,' ',RBID,1,'RHO',RHO,
     &              ICODRE,1)
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

      

C ======================================================================

C CALCUL DU GRADIENT DE TEMPERATURE :
C SI LA TEMPERATURE N'EXISTE PAS, ON LUI IMPOSE UNE VALEUR NULLE
      DO 645 KP = 1,NNO
        CALL RCVARC(' ','TEMP','+','NOEU',KP,1,TGD(KP),IRETH)
        IF (IRETH.EQ.1) TGD(KP) = 0.D0
  645 CONTINUE


C ======================================================================
C BOUCLE PRINCIPALE SUR LES POINTS DE GAUSS
C ======================================================================

      DO 800 KP = 1,NPG
      
C INITIALISATIONS
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
          SIGL(I)   = 0.D0
          SIGIN(I)  = 0.D0
          EPSIN(I)  = 0.D0
          EPSP(I)   = 0.D0
          EPS(I)    = 0.D0
          EPSREF(I) =0.D0
          DO 250 J = 1,3
            DSIGIN(I,J) = 0.D0
            DEPSIN(I,J) = 0.D0
            DEPSP(I,J) = 0.D0
  250     CONTINUE
  260   CONTINUE

C - CALCUL DES ELEMENTS GEOMETRIQUES

        CALL NMGEOM(NDIM,NNO,.FALSE.,GRAND,ZR(IGEOM),KP,IPOIDS,
     &              IVF,IDFDE,ZR(IDEPL),
     &              .TRUE.,POIDS,DFDI,F,EPS,RBID)

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
C =======================================================
C PLASTICITE
C =======================================================
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

C =======================================================
C PRE DEFORMATIONS ET LEUR GRADIENT DEPSIN
C (seule intervenant dans le calcul de G)
C =======================================================

        IF (EPSINI) THEN
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

C =======================================================
C CALCUL DES CONTRAINTES LAGRANDIENNES SIGL ET DE L'ENERGIE LIBRE
C =======================================================

        IF (INCR) THEN
C EN PLASTICITE
          CALL NMPLRU(FAMI,KP,1,'+',NDIM,TYPMOD,MATCOD,COMPOR,
     &                PPG,EPS,EPSP,RP,ENERGI)
          DO 430 I = 1,3
            SIGL(I) = ZR(ISIGM+NCMP* (KP-1)+I-1)
            SIGL(I+3) = ZR(ISIGM+NCMP* (KP-1)+I-1+3)*RAC2
  430     CONTINUE

        ELSE

          CRIT(1) = 300
          CRIT(2) = 0.D0
          CRIT(3) = 1.D-3
          CALL NMELNL(FAMI,KP,1,'+',NDIM,TYPMOD,MATCOD,COMPOR,CRIT,
     &                OPRUPT,EPS,SIGL,RBID,RBID,ENERGI)
          CALL TECACH('NNN','PCONTGR',1,ISIGM,IRET)
          IF(IRET.EQ.0)THEN
            CALL JEVECH('PCONTGR','L',ISIGM)
            DO 401 I = 1,3
              SIGL(I) = ZR(ISIGM+NCMP* (KP-1)+I-1)
              SIGL(I+3) = ZR(ISIGM+NCMP* (KP-1)+I-1+3)*RAC2
  401       CONTINUE
          ENDIF
        END IF
        DIVT = DTDM(1,1) + DTDM(2,2) + DTDM(3,3)

C =======================================================
C CORRECTIONS LIEES A LA CONTRAINTE INITIALE (SIGM_INIT DE CALC_G)
C CONTRAINTE, DEFORMATION DE REFERENCE, ENERGIE ELASTIQUE
C =======================================================

        IF (ISIGI.NE.0) THEN
          DO 470 I = 1,NNO
            DER(1) = DFDI(I)
            DER(2) = DFDI(I+NNO)
            DER(3) = DFDI(I+2*NNO)
            DER(4) = ZR(IVF+L+I-1)
C CALCUL DE SIGMA INITIAL
            DO 440 J = 1,NCMP
              SIGIN(J) = SIGIN(J) + ZR(ISIGI+NCMP* (I-1)+J-1)*DER(4)
  440       CONTINUE
C CALCUL DU GRADIENT DE SIGMA INITIAL
            DO 460 J = 1,NCMP
              DO 450 K = 1,NDIM
                DSIGIN(J,K) = DSIGIN(J,K) +
     &                        ZR(ISIGI+NCMP* (I-1)+J-1)*DER(K)
  450         CONTINUE
  460       CONTINUE
  470     CONTINUE

C TRAITEMENTS PARTICULIERS DES TERMES CROISES  
          DO 490 I = 4,NCMP
            SIGIN(I) = SIGIN(I)*RAC2
            DO 480 J = 1,NDIM
              DSIGIN(I,J) = DSIGIN(4,1)*RAC2
  480       CONTINUE
  490     CONTINUE

C CALCUL DE LA DEFORMATION DE REFERENCE
          CALL RCCOMA(MATCOD,'ELAS',PHENOM,ICODRE)
          CALL RCVALA(MATCOD,' ',PHENOM,1,' ',RBID,1,'NU',NU,
     &              ICODRE,1)
          CALL RCVALA(MATCOD,' ',PHENOM,1,' ',RBID,1,'E',E,
     &              ICODRE,1)
        
          MU = E/(2.D0*(1.D0+NU))
          
          EPSREF(1)=-(1.D0/E)*(SIGIN(1)-(NU*(SIGIN(2)+SIGIN(3))))
          EPSREF(2)=-(1.D0/E)*(SIGIN(2)-(NU*(SIGIN(3)+SIGIN(1))))    
          EPSREF(3)=-(1.D0/E)*(SIGIN(3)-(NU*(SIGIN(1)+SIGIN(2)))) 
          EPSREF(4)=-(1.D0/MU)*SIGIN(4)
          EPSREF(5)=-(1.D0/MU)*SIGIN(5)
          EPSREF(6)=-(1.D0/MU)*SIGIN(6)

C ENERGIE ELASTIQUE (expression WADIER)
          DO 465 I=1,NCMP
            ENERGI(1) = ENERGI(1) + (EPS(I)-0.5D0*EPSREF(I))*SIGIN(I)
465       CONTINUE
        ENDIF
        
C =======================================================
C STOCKAGE DE SIGMA ET TRAITEMENTS DES TERMES CROISES
C =======================================================
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

C =======================================================
C - TERME THERMOELASTIQUE CLASSIQUE F.SIG:(GRAD(U).GRAD(THET))-ENER*DIVT
C =======================================================
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

C =======================================================
C - TERME THERMIQUE :   -(D(ENER)/DT)(GRAD(T).THETA)
C =======================================================
        IF (IRETH.EQ.0) THEN
          PROD = 0.D0
          DO 560 I = 1,NDIM
            PROD = PROD + TGDM(I)*DTDM(I,4)
  560     CONTINUE
          TTHE = TTHE - POIDS*PROD*ENERGI(2)
        ENDIF
C =======================================================
C - TERME FORCE VOLUMIQUE
C =======================================================
        DO 580 I = 1,NDIM
          PROD = 0.D0
          DO 570 J = 1,NDIM
            PROD = PROD + DFDM(I,J)*DTDM(J,4)
  570     CONTINUE
          TFOR = TFOR + DUDM(I,4)* (PROD+DFDM(I,4)*DIVT)*POIDS
  580   CONTINUE

C =======================================================
C - TERME PLASTIQUE :   SIG:(GRAD(EPSP).THETA)- R(P).GRAD(P).THETA
C =======================================================
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

C =======================================================
C TERME INITIAL:PROD1 LIE A LA CONTRAINTE (EPS-EPSREF):GRAD(SIGIN).THETA
C               PROD2 LIE A LA PREDEFORMATION SIG:GRAD(EPSIN).THETA
C =======================================================

        IF ((ISIGI.NE.0) .OR. EPSINI) THEN
          PROD1 = 0.D0
          PROD2 = 0.D0
          IF (ISIGI.NE.0) THEN
            DO 630 I=1,NCMP
              DO 620 J=1,NDIM
              PROD1=PROD1-(EPS(I)-EPSREF(I))*DSIGIN(I,J)*DTDM(J,4)
620           CONTINUE
630         CONTINUE
          ELSE IF (EPSINI) THEN
            DO 631 I=1,NCMP
              DO 621 J=1,NDIM
              PROD2=PROD2+SIGL(I)*DEPSIN(I,J)*DTDM(J,4)
621           CONTINUE
631         CONTINUE
          END IF 
        END IF
C ==================================================================
C FIN DE BOUCLE PRINCIPALE SUR LES POINTS DE GAUSS
C ==================================================================
800   CONTINUE
C EXIT EN CAS DE THETA FISSURE NUL PARTOUT
9999   CONTINUE

C ASSEMBLAGE FINAL DES TERMES DE G OU DG
      ZR(IGTHET) = TTHE + TCLA + TFOR + TPLAS + TINI
      CALL JEDEMA()
      END

      SUBROUTINE TE0096(OPTION,NOMTE)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 28/08/2012   AUTEUR TRAN V-X.TRAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C
C FONCTION REALISEE:
C
C   TAUX DE RESTITUTION D'ENERGIE ELEMENTAIRE EN ELASTICITE
C                                             EN ELASTICITE NON LINEAIRE
C                                             EN GRANDS DEPLACEMENTS
C
C   ELEMENTS ISOPARAMETRIQUES 2D
C
C   OPTION : 'CALC_G'     (G AVEC CHARGES REELLES)
C            'CALC_G_F'   (G AVEC CHARGES FONCTIONS)
C
C ----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      INCLUDE 'jeveux.h'
      CHARACTER*16      OPTION,NOMTE


C DECLARATION VARIABLES LOCALES
C
      INTEGER       ICODRE,KPG,SPT
      CHARACTER*4   FAMI
      CHARACTER*8   NOMPAR(3),TYPMOD(2),FAMIL,POUM
      CHARACTER*16  COMPOR(4),OPRUPT,PHENOM

      REAL*8   EPSREF(6), E, TROISK, MU, ALPHA
      REAL*8   EPSI,RAC2,R8PREM,CRIT(3)
      REAL*8   DFDI(27),F(3,3),SR(3,3),SIGL(6),SIGIN(6),DSIGIN(6,3)
      REAL*8   EPS(6),EPSIN(6),DEPSIN(6,3),EPSP(6),DEPSP(6,3)
      REAL*8   EPSINO(36),FNO(18)
      REAL*8   THET,TN(20),TGDM(3),PROD,PROD1,PROD2,DIVT
      REAL*8   VALPAR(3),TCLA,TTHE,TFOR,TPLAS,TINI,POIDS,R,RBID
      REAL*8   P,PPG,DPDM(3),RP,ENERGI(2),RHO,OM,OMO
      REAL*8   DTDM(3,5),DER(6),DFDM(3,5),DUDM(3,4),DVDM(3,4),VEPSCP
      REAL*8   ECIN,PROD3,PROD4,NU,ACCELE(3)

      INTEGER  IPOIDS,IVF,IDFDE,IPOI1,IVF1,IDFDE1
      INTEGER  ICOMP,IGEOM,ITEMPS,IDEPL,IMATE
      INTEGER  IEPSR,IEPSF,ISIGI,ISIGM,IEPSP,IVARI
      INTEGER  IFORC,IFORF,ITHET,IGTHET,IROTA,IPESA,IER
      INTEGER  IVITES,IACCEL,J1,J2
      INTEGER  NNO,NNOS,NPG,NCMP,JGANO
      INTEGER  I,J,K,KK,L,M,KP,NDIM,COMPT,NBVARI
      INTEGER  IJ,IJ1,MATCOD,I1,IRET,IRET1,NPG1

      LOGICAL  GRAND,AXI,CP,FONC,INCR,EPSINI,LTEATT

C =====================================================================
C INITIALISATIONS
C =====================================================================
      CALL JEMARQ()
      FAMI = 'RIGI'
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDE,JGANO)

      EPSI   = R8PREM()
      RAC2   = SQRT(2.D0)
      OPRUPT = 'RUPTURE'
      AXI    = .FALSE.
      CP     = .FALSE.
      EPSINI = .FALSE.
      TYPMOD(2) = ' '
      FAMIL='FPG1'
      KPG=1
      SPT=1
      POUM='+'

      IF (LTEATT(' ','AXIS','OUI')) THEN
        TYPMOD(1) = 'AXIS'
        AXI = .TRUE.
      ELSEIF (LTEATT(' ','C_PLAN','OUI')) THEN
        TYPMOD(1) = 'C_PLAN'
        CP  = .TRUE.
      ELSEIF (LTEATT(' ','D_PLAN','OUI')) THEN
        TYPMOD(1) = 'D_PLAN'
      ENDIF
C
C NOMBRE DE COMPOSANTES DES TENSEURS
      NCMP = 2*NDIM

C INIT. POUR LE CALCUL DE G
      TCLA  = 0.D0
      TTHE  = 0.D0
      TFOR  = 0.D0
      TPLAS = 0.D0
      TINI  = 0.D0
      CALL JEVECH('PGTHETA','E',IGTHET)
      CALL JEVECH('PTHETAR','L',ITHET)

      IVITES = 0
      IACCEL = 0

C TEST SUR LA NULLITE DE THETA_FISSURE
      COMPT = 0
      DO 3 I=1,NNO
        THET = 0.D0
        DO 2 J=1,NDIM
          THET = THET + ABS(ZR(ITHET+NDIM*(I-1)+J-1))
2      CONTINUE
        IF (THET.LT.EPSI) COMPT = COMPT+1
3     CONTINUE
      IF(COMPT.EQ.NNO) GOTO 9999
C
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

C RECUPERATION DU CHAMP LOCAL (CARTE) ASSOCIE AU PRE-EPSI
C CE CHAMP EST ISSU D UN CHARGEMENT PRE-EPSI
      IF (OPTION.EQ.'CALC_G_F') THEN
        FONC = .TRUE.
        CALL JEVECH('PFFVOLU','L',IFORF)
        CALL JEVECH('PTEMPSR','L',ITEMPS)
        NOMPAR(1) = 'X'
        NOMPAR(2) = 'Y'
        NOMPAR(3) = 'INST'
        VALPAR(3) = ZR(ITEMPS)
        CALL TECACH('ONN','PEPSINF',1,IEPSF,IRET)
        IF (IEPSF.NE.0) EPSINI = .TRUE.
      ELSE
        FONC = .FALSE.
        CALL JEVECH('PFRVOLU','L',IFORC)
        CALL TECACH('ONN','PEPSINR',1,IEPSR,IRET)
        IF (IEPSR.NE.0) EPSINI = .TRUE.
      ENDIF


C LOI DE COMPORTEMENT
      GRAND = COMPOR(3).EQ.'GROT_GDEP'
      INCR  = COMPOR(4)(1:9).EQ.'COMP_INCR'
      READ(ZK16(ICOMP+1),'(I16)') NBVARI
      IF (INCR) THEN
        CALL JEVECH('PCONTRR','L',ISIGM)
        CALL JEVECH('PDEFOPL','L',IEPSP)
        CALL JEVECH('PVARIPR','L',IVARI)
      ENDIF
      CALL TECACH('ONN','PPESANR',1,IPESA,IRET)
      CALL TECACH('ONN','PROTATR',1,IROTA,IRET)
      CALL TECACH('ONN','PSIGINR',1,ISIGI,IRET)
C      WRITE(6,*)'ISIGI',ISIGI
C      WRITE(6,*)'IDEPI',IDEPI
C      WRITE(6,*)'EPSINI',EPSINI
C      WRITE(6,*)'IEPSF',IEPSF
C      WRITE(6,*)'IEPSR',IEPSR
      IF (OPTION.EQ.'CALC_G'.OR.OPTION.EQ.'CALC_G_F') THEN
        CALL TECACH('ONN','PVITESS',1,IVITES,IRET)
        CALL TECACH('ONN','PACCELE',1,IACCEL,IRET)
      ENDIF

      DO 20 I=1,NCMP*NNO
        EPSINO(I) = 0.D0
20    CONTINUE

C =====================================================================
C MESSAGES D'ERREURS
C =====================================================================

C ON NE PEUT AVOIR SIMULTANEMENT PRE-DEFORMATIONS ET CONTRAINTES INIT.
      IF ((ISIGI.NE.0).AND.EPSINI) THEN
        CALL U2MESS('F','RUPTURE1_20')
      ENDIF

C =====================================================================
C RECUPERATION DES CHARGES ET PRE-DEFORMATIONS (CHARGEMENT PRE-EPSI)
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
80      CONTINUE
      ENDIF

      IF (IVITES.NE.0) THEN
        CALL RCCOMA(MATCOD,'ELAS',PHENOM,ICODRE)
        CALL RCVALB(FAMIL,KPG,SPT,POUM,MATCOD,' ',PHENOM,1,' ',RBID,1,
     &              'RHO',RHO,ICODRE,1)
        CALL RCVALB(FAMIL,KPG,SPT,POUM,MATCOD,' ',PHENOM,1,' ',RBID,1,
     &              'NU',NU,ICODRE,1)
      ENDIF

C CORRECTION DES FORCES VOLUMIQUES
      IF ((IPESA.NE.0).OR.(IROTA.NE.0)) THEN
        CALL RCCOMA(MATCOD,'ELAS',PHENOM,ICODRE)
        CALL RCVALB(FAMIL,KPG,SPT,POUM,MATCOD,' ',PHENOM,1,' ',RBID,1,
     &              'RHO',RHO,ICODRE,1)
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


C ======================================================================
C CALCUL DE LA TEMPERATURE AUX NOEUDS ET RECUPERATION DE LA TEMPERATURE
C DE REFERENCE
C ======================================================================

      DO 645 KP = 1,NNO
        CALL RCVARC(' ','TEMP','+','NOEU',KP,1,TN(KP),IRET1)

  645 CONTINUE

C ======================================================================
C BOUCLE PRINCIPALE SUR LES POINTS DE GAUSS
C ======================================================================

      DO 800 KP=1,NPG1

C INITIALISATIONS
        L   = (KP-1)*NNO
        PPG = 0.D0
        DO 220 I=1,3
          TGDM(I) = 0.D0
          DPDM(I) = 0.D0
          ACCELE(I) = 0.D0
          DO 200 J=1,3
            SR(I,J) = 0.D0
200       CONTINUE
          DO 210 J=1,4
            DUDM(I,J) = 0.D0
            DVDM(I,J) = 0.D0
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
          EPSP(I)  = 0.D0
          EPS (I)  = 0.D0
          EPSREF(I)= 0.D0
          DO 230 J=1,3
            DSIGIN(I,J) = 0.D0
            DEPSIN(I,J) = 0.D0
            DEPSP(I,J)  = 0.D0
230       CONTINUE
240     CONTINUE

C ===========================================
C CALCUL DES ELEMENTS GEOMETRIQUES
C ===========================================

        CALL NMGEOM (NDIM,NNO,AXI,GRAND,ZR(IGEOM),KP,IPOIDS,
     &               IVF,IDFDE,ZR(IDEPL),
     &               .TRUE.,POIDS,DFDI,F,EPS,R)
C - CALCULS DES GRADIENTS DE U (DUDM), DE THETA FISSURE (DTDM) ET DE
C   LA FORCE VOLUMIQUE (DFDM),
C   DE LA TEMPERATURE AUX POINTS DE GAUSS (TG) ET SON GRADIENT (TGDM)
        DO 320 I=1,NNO
          I1 = I-1
          DER(1) = DFDI(I)
          DER(2) = DFDI(I+NNO)
          DER(4) = ZR(IVF+L+I1)
          IF (IRET1.EQ.0) THEN
            DO 309 J=1,NDIM
              TGDM(J)     = TGDM(J)   + TN(I)*DER(J)
309         CONTINUE
          ENDIF
          DO 310 J=1,NDIM
            IJ1 = NDIM*I1+J
            IJ = IJ1 - 1
            DO 300 K=1,NDIM
              DUDM(J,K) = DUDM(J,K) + ZR(IDEPL+IJ)*DER(K)
              DTDM(J,K) = DTDM(J,K) + ZR(ITHET+IJ)*DER(K)
              DFDM(J,K) = DFDM(J,K) + FNO(IJ1)*DER(K)
300         CONTINUE
            IF (IVITES.NE.0) THEN
              DO 305 K=1,NDIM
                DVDM(J,K) = DVDM(J,K) + ZR(IVITES+IJ)*DER(K)
305           CONTINUE
              DVDM(J,4) = DVDM(J,4) + ZR(IVITES+IJ)*DER(4)
              ACCELE(J) = ACCELE(J) + ZR(IACCEL+IJ)*DER(4)
              IF (CP) THEN
                VEPSCP = -NU/(1.D0-NU)*(DVDM(1,1)+DVDM(2,2))
              ENDIF
            ENDIF
            DUDM(J,4) = DUDM(J,4) + ZR(IDEPL+IJ)*DER(4)
            DTDM(J,4) = DTDM(J,4) + ZR(ITHET+IJ)*DER(4)
            DFDM(J,4) = DFDM(J,4) + FNO(IJ1)*DER(4)
310       CONTINUE
320     CONTINUE

C =======================================================
C PLASTICITE
C =======================================================

C CALCULS DES GRADIENTS DE P (DPDM) ET EPSP (DEPSP) EN PLASTICITE

        IF (INCR) THEN
          DO 380 I=1,NNO
            DER(1) = DFDI(I)
            DER(2) = DFDI(I+NNO)
            DER(3) = 0.D0
            DER(4) = ZR(IVF+L+I-1)
            P   = ZR(IVARI+(I-1)*NBVARI)
            PPG = PPG + P*DER(4)
            DO 350 J=1,NCMP
              EPSP(J) = EPSP(J)+ ZR(IEPSP+NCMP*(I-1)+J-1)*DER(4)
350         CONTINUE
            IF (P.GE.EPSI) THEN
              DO 360 J=1,NDIM
                DPDM(J)= DPDM(J) + ZR(IVARI+(I-1)*NBVARI)*DER(J)
360           CONTINUE
              DO 370 K=1,NDIM
                DO 365 J=1,NCMP
                  DEPSP(J,K)=DEPSP(J,K)+ZR(IEPSP+NCMP*(I-1)+J-1)*DER(K)
365             CONTINUE
370           CONTINUE
            ENDIF
380       CONTINUE
          DO 382 I=4,NCMP
            EPSP(I)=EPSP(I)*RAC2
            DO  381 J=1,NDIM
              DEPSP(I,J)=DEPSP(I,J)*RAC2
381         CONTINUE
382       CONTINUE
          IF (PPG.LT.EPSI)THEN
            PPG = 0.D0
            DO 390 J=1,NCMP
              EPSP(J) =  0.D0
390         CONTINUE
          ENDIF
        ENDIF

C =======================================================
C PRE DEFORMATIONS ET LEUR GRADIENT DEPSIN
C (seule intervenant dans le calcul de G)
C =======================================================

        IF (EPSINI) THEN
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

C =======================================================
C CALCUL DES CONTRAINTES LAGRANGIENNES SIGL ET DE L'ENERGIE LIBRE
C =======================================================

        IF (INCR) THEN

C EN PLASTICITE
          CALL NMPLRU(FAMI,KP,1,'+',NDIM,TYPMOD,MATCOD,COMPOR,PPG,
     &                EPS,EPSP,RP,ENERGI)
          DO 435 I = 1,3
            SIGL(I)= ZR(ISIGM+NCMP*(KP-1)+I-1)
435       CONTINUE
          SIGL(4)= ZR(ISIGM+NCMP*(KP-1)+3)*RAC2

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
               SIGL(I)= ZR(ISIGM+NCMP*(KP-1)+I-1)
401          CONTINUE
             SIGL(4)= ZR(ISIGM+NCMP*(KP-1)+3)*RAC2
          ENDIF
        ENDIF

C =======================================================
C DIVERS (DIVERGENCE, MODELISATION...)
C =======================================================

C TRAITEMENTS DEPENDANT DE LA MODELISATION
        IF(CP) THEN
          DUDM(3,3)= EPS(3)
          IF (IVITES.NE.0) THEN
            DVDM(3,3)= VEPSCP
          ENDIF
        ENDIF
        IF (AXI) THEN
          DUDM(3,3)= DUDM(1,4)/R
          DTDM(3,3)= DTDM(1,4)/R
          DFDM(3,3)= DFDM(1,4)/R
          IF (IVITES.NE.0) THEN
            DVDM(3,3)= DVDM(1,4)/R
          ENDIF
        ENDIF

C CALCUL DE LA DIVERGENCE DU THETA FISSURE (DIVT)
        DIVT = 0.D0
        DO 437 I=1,3
          DIVT  = DIVT + DTDM(I,I)
437     CONTINUE


C =======================================================
C CORRECTIONS LIEES A LA CONTRAINTE INITIALE (SIGM_INIT DE CALC_G)
C CONTRAINTE, DEFORMATION DE REFERENCE, ENERGIE ELASTIQUE
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
          
C 

C ENERGIE ELASTIQUE (expression WADIER)

          DO 465 I=1,NCMP
            ENERGI(1) = ENERGI(1) + (EPS(I)-0.5D0*EPSREF(I))*SIGIN(I)
465       CONTINUE
        ENDIF




C =======================================================
C STOCKAGE DE SIGMA ET TRAITEMENTS DES TERMES CROISES
C =======================================================
        SR(1,1)= SIGL(1)
        SR(2,2)= SIGL(2)
        SR(3,3)= SIGL(3)
        SR(1,2)= SIGL(4)/RAC2
        SR(2,1)= SR(1,2)
        SR(1,3)= SIGL(5)/RAC2
        SR(3,1)= SR(1,3)
        SR(2,3)= SIGL(6)/RAC2
        SR(3,2)= SR(2,3)

C CALCUL DE G

C =======================================================
C TERME THERMOELASTIQUE CLASSIQUE F.SIG:(GRAD(U).GRAD(THET))-ENER*DIVT
C REMARQUE : POUR LA DERIVEE, TCLA EST INUTILE.
C            MAIS ON A BESOIN DE PROD2 SI TSENUL EST FAUX.
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

        PROD  = 0.D0
        PROD2 = 0.D0
        DO 490 I=1,3
          DO 480 J=1,3
            DO 475 K=1,3
              DO 470 M=1,3
                PROD =PROD+F(I,J)*SR(J,K)*DUDM(I,M)*DTDM(M,K)
470           CONTINUE
475         CONTINUE
480       CONTINUE
490     CONTINUE
        PROD  = PROD - ECIN*DIVT + PROD3 - PROD4
        PROD2 = POIDS*( PROD - ENERGI(1)*DIVT)

        TCLA  = TCLA + PROD2

C =======================================================
C TERME THERMIQUE :   -(D(ENER)/DT)(GRAD(T).THETA)
C =======================================================
        IF (IRET1.EQ.0) THEN
          PROD = 0.D0
          PROD2 = 0.D0
          DO 500 I=1,NDIM
            PROD = PROD + TGDM(I)*DTDM(I,4)
500       CONTINUE
          PROD2 = - POIDS*PROD*ENERGI(2)
C
          TTHE = TTHE + PROD2
        ELSE
          TTHE = 0.D0
        ENDIF
C =======================================================
C TERME FORCE VOLUMIQUE
C REMARQUE : POUR LA DERIVEE, TFOR EST INUTILE.
C            MAIS ON A BESOIN DE PROD2 SI TSENUL EST FAUX.
C =======================================================
C
        PROD2 = 0.D0
        DO 520 I=1,NDIM
          PROD=0.D0
          DO 510 J=1,NDIM
            PROD = PROD + DFDM(I,J)*DTDM(J,4)
510       CONTINUE
          PROD2 = PROD2 + DUDM(I,4)*(PROD+DFDM(I,4)*DIVT)*POIDS
520     CONTINUE
C
        TFOR = TFOR + PROD2

C =======================================================
C TERME PLASTIQUE :   SIG:(GRAD(EPSP).THETA)- R(P).GRAD(P).THETA
C =======================================================
C
        IF (INCR) THEN
          PROD1=0.D0
          PROD2=0.D0
          DO 620 I=1,NCMP
            DO 610 J=1,NDIM
              PROD1 = PROD1 + SIGL(I)*DEPSP(I,J)*DTDM(J,4)
610         CONTINUE
620       CONTINUE
          DO 650 I=1,NDIM
            PROD2 = PROD2 + RP*DPDM(I)*DTDM(I,4)
650       CONTINUE
          TPLAS = TPLAS + (PROD1-PROD2)*POIDS
        ENDIF

C =======================================================
C TERME INITIAL:PROD1 LIE A LA CONTRAINTE (EPS-EPSREF):GRAD(SIGIN).THETA
C               PROD2 LIE A LA PREDEFORMATION SIG:GRAD(EPSIN).THETA
C =======================================================
C
        IF ((ISIGI.NE.0).OR.EPSINI) THEN
          PROD1=0.D0
          PROD2=0.D0
          IF (ISIGI.NE.0) THEN
            DO 670 I=1,NCMP
              DO 660 J=1,NDIM
              PROD1=PROD1-(EPS(I)-EPSREF(I))*DSIGIN(I,J)*DTDM(J,4)
660           CONTINUE
670         CONTINUE
          ELSE IF (EPSINI) THEN
            DO 671 I=1,NCMP
              DO 661 J=1,NDIM
              PROD2=PROD2+SIGL(I)*DEPSIN(I,J)*DTDM(J,4)
661           CONTINUE
671         CONTINUE
          END IF 
          TINI = TINI + (PROD1+PROD2)*POIDS
C
        ENDIF

C ==================================================================
C FIN DE BOUCLE PRINCIPALE SUR LES POINTS DE GAUSS
C ==================================================================
800   CONTINUE

C EXIT EN CAS DE THETA FISSURE NUL PARTOUT
9999  CONTINUE

C ASSEMBLAGE FINAL DES TERMES DE G OU DG
      ZR(IGTHET) = TTHE  + TCLA  + TFOR  + TPLAS  + TINI
      CALL JEDEMA()
      END

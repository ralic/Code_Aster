      SUBROUTINE NMELNL (FAMI,KPG,KSP,POUM,NDIM, TYPMOD, IMATE, COMPOR,
     &                   CRIT,OPTION, EPS, SIG, VI, DSIDEP, ENERGI,
     &                   DERIVL,DLAGTG, DEPS, DENERG, DSIG)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 28/11/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_7
C-----------------------------------------------------------------------
C
C     REALISE LA LOI DE HENCKY POUR LES ELEMENTS ISOPARAMETRIQUES
C     ET CALCUL SA DERIVEE LAGRANGIENNE PAR RAPPORT A UNE VARIATION
C     DE DOMAINE (EN DP ELASTIQUE ISOTROPE LINEAIRE).
C
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  TYPMOD  : TYPE DE MODELISATION
C IN  IMATE   : NATURE DU MATERIAU
C IN  COMPOR  : COMPORTEMENT
C IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
C IN  TEMP    : TEMPERATURE.
C IN  TREF    : TEMPERATURE DE REFERENCE.
C IN  EPS     : DEFORMATION (SI C_PLAN EPS(3) EST EN FAIT CALCULE)
C IN  OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG -> SIG    DSIDEP
C                                 FULL_MECA      -> SIG VI DSIDEP
C                                 RAPH_MECA      -> SIG VI
C                                 RUPTURE        -> SIG VI ENERGI
C IN  DERIVL : FLAG POUR LE CALCUL DE LA DERIVEE LAGRANGIENNE (SI DG).
C IN  DLAGTG : DERIVEE LAGRANGIENNE DE LA TEMPERATURE (SI DG).
C IN  DEPS   : DERIVEE LAGRANGIENNE DE EPS-EPSINIT (SI DG).
C OUT SIG    : CONTRAINTES LAGRANGIENNES
C OUT VI     : VARIABLE INTERNE (AUXILIAIRE DE CALCUL)
C OUT DSIDEP : MATRICE CARREE
C OUT ENERGI(1)  : ENERGIE LIBRE (POUR LE CALCUL DE G)
C OUT ENERGI(2)  : DERIVEE DE L'ENERGIE LIBRE / TEMPERATURE
C OUT DENERG(1)  : DERIVEE LAGRANGIENNE DE L'ENERGIE LIBRE (SI DG).
C OUT DENERG(2)  : DERIVEE LAGRANGIENNE DE LA DERIVEE EN TEMPERATURE.
C OUT DSIG : DERIVEE LAGRANGIENNE DU TENSEUR DES CONTRAINTES (SI DG).
C   -------------------------------------------------------------------
C     SUBROUTINES APPELLEES:
C       ENVIMA: R8MIEM, R8PREM.
C       MATERIAUX: RCVALB, RCTRAC, RCFONC.
C       DIVERS: NMCRI1, ZEROFO, NMELRU.
C
C     FONCTIONS INTRINSEQUES:
C       SQRT, INT, ABS.
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       11/12/00 (OB): TOILETTAGE FORTRAN,
C                      MISE EN PLACE DE LA DERIVEE LAGRANGIENNE PAR
C                      RAPPORT A UNE VARIATION DE DOMAINE.
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      INTEGER      KPG,KSP,NDIM,IMATE,IRET,ISEC,IHYD
      CHARACTER*(*) FAMI,POUM
      CHARACTER*8  TYPMOD(*)
      CHARACTER*16 COMPOR(*),OPTION
      REAL*8      CRIT(3),TEMP,PTOT,DLAGTG,DEPS(6),DSIG(6),HYDR, SECH
      REAL*8      SECREF
      REAL*8      EPS(6),SIG(6),VI,DSIDEP(6,6),ENERGI(2),DENERG(2)

      LOGICAL     DERIVL

C DECLARATION VARIABLES LOCALES
      LOGICAL     CPLAN,ELAS,VMIS,LINE,NONLIN,INCO,PUIS
      INTEGER ICODRE(5)
      CHARACTER*8 NOMRES(5)
      CHARACTER*16 PHENOM
      INTEGER     JPROL, JVALE, NBVALE, NDIMSI, NITER, K, L, IBID

      REAL*8 VALRES(5), E, NU, TROISK, DEUXMU, ALPHA, SIGY, DSDE
      REAL*8 KDESS,BENDO, THER, EPSTH(6), EPSMO, EPSDV(6), EPSEQ, SIELEQ
      REAL*8 P, RP, RPRIM, G, COEF, EPSI, AIRERP,DTHER
      REAL*8 F0,APPROX,PREC,X,KRON(6), DUM,DIVU,R8PREM,BIOT
      REAL*8 DEPSTH(6),DEPSDV(6),DEPSEQ,DEPSMO,DDIVU,R8MIEM,EPSTES
      REAL*8 COCO,DP0,RPRIM0,XAP,VAL0,PRECR

C====================================================================
C---COMMONS NECESSAIRES A HENCKY C_PLAN (NMCRI1)
C====================================================================
      INTEGER  IMATE2, JPROL2, JVALE2, NBVAL2
      REAL*8   PM, SIGEL(6), LIN, EPSTHE,EPSPTO
      COMMON /RCONM1/ DEUXMU, NU, E, SIGY, RPRIM, PM, SIGEL, LIN
      COMMON /KCONM1/ IMATE2, JPROL2, JVALE2, NBVAL2
      REAL*8   NMCRI1
      EXTERNAL NMCRI1
C====================================================================
C---COMMONS NECESSAIRES A ELAS_VMIS_PUIS
C====================================================================
      COMMON /RCONM2/ALFAFA,UNSURN,SIELEQ
      REAL*8         ALFAFA,UNSURN
      REAL*8   NMCRI2
      EXTERNAL NMCRI2
C====================================================================
C - INITIALISATIONS
C====================================================================
      DATA  KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
      CPLAN = TYPMOD(1) .EQ. 'C_PLAN'
      INCO  = TYPMOD(2) .EQ. 'INCO'
      ELAS  = (COMPOR(1)(1:5) .EQ. 'ELAS ')
      VMIS  = (COMPOR(1)(1:9) .EQ. 'ELAS_VMIS')
      LINE  = (COMPOR(1)(1:14).EQ. 'ELAS_VMIS_LINE')
      PUIS  = (COMPOR(1)(1:14).EQ. 'ELAS_VMIS_PUIS')
      EPSI  = R8PREM()
C====================================================================
C INITIALISATIONS LIEES AU CALCUL DE DERIVEES LAGRANGIENNE
C====================================================================
      IF (DERIVL) THEN
        IF (.NOT.ELAS)
     &    CALL U2MESS('F','RUPTURE1_21')
        EPSTES = R8MIEM()
        DENERG(1) = 0.D0
        DENERG(2) = 0.D0
        DO 2 K=1,6
          DEPSTH(K) = 0.D0
          DEPSDV(K) = 0.D0
2       CONTINUE
      ENDIF
      ENERGI(1) = 0.D0
      ENERGI(2) = 0.D0

      IF (.NOT.(ELAS .OR. VMIS))
     &   CALL U2MESK('F','ALGORITH4_50',1,COMPOR(1))
      NDIMSI = 2*NDIM
C====================================================================
C - LECTURE DES CARACTERISTIQUES ELASTIQUES
C====================================================================
      NOMRES(1)='E'
      NOMRES(2)='NU'
      NOMRES(3)='ALPHA'

C TEST SUR LA COHERENCE DES INFORMATIONS CONCERNANT LA TEMPERATURE
      CALL VERIFT(FAMI,KPG,KSP,POUM,IMATE,'ELAS',1,EPSTHE,IRET)

      CALL RCVARC(' ','TEMP',POUM,FAMI,KPG,KSP,TEMP,IRET)
      CALL RCVARC(' ','HYDR',POUM,FAMI,KPG,KSP,HYDR,IHYD)
      IF (IHYD.NE.0) HYDR=0.D0
      CALL RCVARC(' ','SECH',POUM,FAMI,KPG,KSP,SECH,ISEC)
      IF (ISEC.NE.0) SECH=0.D0
      CALL RCVARC(' ','SECH','REF',FAMI,KPG,KSP,SECREF,IRET)
      IF (IRET.NE.0) SECREF=0.D0
C SI CALCUL DE LA DERIVEE LAGRANGIENNE DERIVL=TRUE
C ET SECHAGE ou HYDRATATION EN VARIABLE DE COMMANDE
C => ERREUR CAS NON PRÉVU
      IF (DERIVL .AND. (IHYD .EQ.0))
     &   CALL U2MESS('F','SENSIBILITE_57')
      IF (DERIVL .AND. (ISEC .EQ.0))
     &   CALL U2MESS('F','SENSIBILITE_57')
      IF (ELAS .OR. LINE .OR. PUIS) THEN
         CALL RCVALB (FAMI,KPG,KSP,POUM,IMATE,' ','ELAS',0,' ',0.D0,2,
     &                 NOMRES,VALRES,ICODRE, 2)
         CALL RCVALB (FAMI,KPG,KSP,POUM,IMATE,' ','ELAS',0,' ',0.D0,1,
     &                 NOMRES(3),VALRES(3),ICODRE(3), 0)
         IF (ICODRE(3).NE.0) VALRES(3)=0.D0
      ELSE
         CALL RCTRAC(IMATE,1,'SIGM',TEMP,JPROL,
     &               JVALE,NBVALE,VALRES(1))
         CALL RCVALB (FAMI,KPG,KSP,POUM,IMATE,' ','ELAS',0,' ',0.D0,1,
     &                 NOMRES(2),VALRES(2),ICODRE(2), 2)

         CALL RCVALB (FAMI,KPG,KSP,POUM,IMATE,' ','ELAS',0,' ',0.D0,1,
     &                 NOMRES(3),VALRES(3),ICODRE(3), 0)
         IF (ICODRE(3).NE.0) VALRES(3)=0.D0
      ENDIF

      E     = VALRES(1)
      NU    = VALRES(2)
      ALPHA = VALRES(3)

      DEUXMU = E/(1.D0+NU)
      IF (ABS(NU- 0.5D0).GE.EPSI) THEN
        TROISK = E/(1.D0-2.D0*NU)
      ELSE
        TROISK = DEUXMU
      ENDIF
C
C --- RETRAIT ENDOGENE ET RETRAIT DE DESSICCATION
C
      NOMRES(4)='B_ENDOGE'
      NOMRES(5)='K_DESSIC'
      CALL RCVALB(FAMI,KPG,KSP,POUM,IMATE,' ','ELAS',0,' ',0.D0,1,
     &            NOMRES(4),VALRES(4),ICODRE(4), 0)
      IF ( ICODRE(4) .NE.0    ) VALRES(4) = 0.D0
      BENDO = VALRES(4)
C
      CALL RCVALB(FAMI,KPG,KSP,POUM,IMATE,' ','ELAS',0,' ',0.D0,1,
     &            NOMRES(5),VALRES(5),ICODRE(5), 0)
      IF ( ICODRE(5) .NE.0    ) VALRES(5) = 0.D0
      KDESS = VALRES(5)
C
C====================================================================
C - LECTURE DES CARACTERISTIQUES DE NON LINEARITE DU MATERIAU
C====================================================================
      IF (LINE) THEN
        NOMRES(1)='D_SIGM_EPSI'
        NOMRES(2)='SY'
        CALL RCVALB (FAMI,KPG,KSP,POUM,IMATE,' ','ECRO_LINE',0,' ',0.D0,
     &               2,NOMRES,VALRES,ICODRE , 2)
        DSDE  = VALRES(1)
        SIGY  = VALRES(2)

      ELSE IF (PUIS) THEN
        NOMRES(1)='SY'
        NOMRES(2)='A_PUIS'
        NOMRES(3)='N_PUIS'
        CALL RCVALA(IMATE,' ','ECRO_PUIS',1,'TEMP',TEMP,3,NOMRES,VALRES,
     &              ICODRE , 2)
        SIGY   = VALRES(1)
        ALFAFA = VALRES(2)
        COCO   = E/ALFAFA/SIGY
        UNSURN = 1.D0/VALRES(3)

      ELSE IF (VMIS) THEN
        CALL RCFONC('S',1,JPROL,JVALE,NBVALE,SIGY,DUM,DUM,
     &               DUM,DUM,DUM,DUM,DUM,DUM)
      ENDIF
C====================================================================
C CALCULS DIVERS
C====================================================================
C - CALCUL DE EPSMO ET EPSDV
      THER = EPSTHE - KDESS*(SECREF-SECH)- BENDO*HYDR

C CALCUL DE LA DERIVEE LAGRANGIENNE DE THER (DTHER) CAR DL(ALPHA)=0
C ET DL(TREF) = 0 PAR ELEMENT
      IF (DERIVL) DTHER = ALPHA * DLAGTG

C TRAITEMENT PARTICULIER EN CONTRAINTE PLANE
      IF (CPLAN) THEN
        EPS(3)=-NU/(1.D0-NU)*(EPS(1)+EPS(2))
     &                +(1.D0+NU)/(1.D0-NU)*THER

C CALCUL DE LA DERIVEE LAGRANGIENNE DE EPS(3) (DEPS(3)) CAR DL(NU)=0
C PAR ELEMENT
        IF (DERIVL) DEPS(3)=-NU/(1.D0-NU)*(DEPS(1)+DEPS(2))
     &                +(1.D0+NU)/(1.D0-NU)*DTHER
      ENDIF
      EPSMO = 0.D0
C
C SI ON EST SUR UNE LOI ELASTIQUE, SI ON EST EN D_PLAN OU EN 3D,
C ON REGARDE LA VARIABLE DE COMMANDE PRESSION POUR LE CHAINAGE HM
C
      EPSPTO = 0.D0
      IF (ELAS.AND.(.NOT.CPLAN)) THEN
        CALL RCVARC(' ','PTOT',POUM,FAMI,KPG,KSP,PTOT,IRET)

        IF (IRET.EQ.0) THEN
          PHENOM = 'THM_DIFFU'
          NOMRES(1) = 'BIOT_COE'

          CALL RCVALB(FAMI,KPG,KSP,POUM,IMATE,' ',PHENOM,0,' ',
     &                    0.D0,1,NOMRES,VALRES, ICODRE,1 )
C
          BIOT = VALRES(1)
C
          EPSPTO = BIOT/TROISK*PTOT
C
        ENDIF
C
      ENDIF
C
      DO 10 K=1,3
        EPSTH(K)   = EPS(K) - THER - EPSPTO
        EPSTH(K+3) = EPS(K+3)
        EPSMO      = EPSMO + EPSTH(K)
10    CONTINUE
      EPSMO = EPSMO/3.D0

C CALCUL DES DERIVEES LAGRANGIENNES: EPSTH (DEPSTH), EPSMO (DEPSMO)
      IF (DERIVL) THEN
        DEPSMO = 0.D0
        DO 11 K=1,3
          DEPSTH(K) = DEPS(K) - DTHER
          DEPSTH(3+K) = DEPS(K+3)
          DEPSMO = DEPSMO + DEPSTH(K)
11      CONTINUE
        DEPSMO = DEPSMO/3.D0
      ENDIF

      DO 20 K=1,NDIMSI
        EPSDV(K) = EPSTH(K) - EPSMO * KRON(K)
C CALCUL DE LA DERIVEE LAGRANGIENNE DU TENSEUR DEVIATORIQUE (DEPSDV)
        IF (DERIVL) DEPSDV(K) = DEPSTH(K) - DEPSMO * KRON(K)
20    CONTINUE
C - CALCUL DE LA CONTRAINTE ELASTIQUE EQUIVALENTE
      EPSEQ = 0.D0
      DEPSEQ = 0.D0
      DO 30 K=1,NDIMSI
        EPSEQ = EPSEQ + EPSDV(K)*EPSDV(K)
C CALCUL DE SA DERIVEE LAGRANGIENNE (PART I)
        IF (DERIVL) DEPSEQ = DEPSEQ + 2.D0*EPSDV(K)*DEPSDV(K)
30    CONTINUE
      EPSEQ = SQRT(1.5D0*EPSEQ)
C CALCUL DE LA DERIVEE LAGRANGIENNE DU EPS EQUIVALENT (PART II)
      IF (DERIVL) THEN
        IF (EPSEQ.GT.EPSTES) THEN
          DEPSEQ = 0.75D0 * DEPSEQ/EPSEQ
        ELSE
          CALL U2MESS('A','ALGORITH7_73')
          DEPSEQ = 0.75D0 * DEPSEQ/EPSTES
        ENDIF
      ENDIF
      SIELEQ = DEUXMU * EPSEQ
      NONLIN = .FALSE.
      IF (VMIS) NONLIN = (SIELEQ.GE.SIGY)
C====================================================================
C CAS NON LINEAIRE
C====================================================================
C - CALCUL DE P, RP, RPRIM ET AIRERP
      IF (NONLIN) THEN
         IRET=0
C===========================================
C      CAS DES CONTRAINTES PLANES
C===========================================
        IF (CPLAN) THEN
C        REMPLISSAGE DU COMMON
          PM = 0.D0
          DO 40 K=1,4
            SIGEL(K) = DEUXMU*EPSDV(K)
40        CONTINUE
          IMATE2 = IMATE
          IF (LINE) THEN
            RPRIM = E*DSDE/(E-DSDE)
            LIN = 1.D0
          ELSE IF (PUIS) THEN
            CALL U2MESS('F','ALGORITH_1')
          ELSE
            JPROL2 = JPROL
            JVALE2 = JVALE
            NBVAL2 = NBVALE
            CALL RCFONC('V',1,JPROL,JVALE,NBVALE,DUM,E,NU,
     &                  0.D0,RP,RPRIM,AIRERP,DUM,DUM)
            LIN = 0.D0
          ENDIF
C        CALCUL DE P (EQUATION PROPRE AUX CONTRAINTES PLANES)
          F0=NMCRI1(0.D0)
          APPROX = 2.D0*EPSEQ/3.D0 - SIGY/1.5D0/DEUXMU
          PREC= CRIT(3) * SIGY
          NITER = INT(CRIT(1))
          CALL ZEROFO(NMCRI1,F0,APPROX,PREC,NITER,P,IRET,IBID)
          IF(IRET.EQ.1) CALL U2MESS('F','ALGORITH8_65')
          IF (LINE) THEN
            RP = SIGY +RPRIM*P
            AIRERP = 0.5D0*(SIGY+RP)*P
          ELSE IF (PUIS) THEN
            CALL U2MESS('F','ALGORITH_1')
          ELSE
            CALL RCFONC('V',1,JPROL,JVALE,NBVALE,DUM,E,
     &                   NU,P,RP,RPRIM,AIRERP,DUM,DUM)
          ENDIF
C
          EPSEQ = 1.5D0*P + RP/DEUXMU
          G = RP/EPSEQ
          X = 3*(DEUXMU-G)/(TROISK+2*G)*EPSDV(3)
          EPSMO   = EPSMO   +X/3.D0
          EPS(3)  = EPS(3)  + X
          EPSDV(1)= EPSDV(1)-X/3.D0
          EPSDV(2)= EPSDV(2)-X/3.D0
          EPSDV(3)= EPSDV(3)+X*2.D0/3.D0
C      CAS 2D OU 3D
        ELSE
C===========================================
C NON CONTRAINTE PLANE
C===========================================
          PM=0.D0
          IF (LINE) THEN
            RPRIM = E*DSDE/(E-DSDE)
            P = (SIELEQ - SIGY) / (RPRIM+1.5D0*DEUXMU)
            RP = SIGY +RPRIM*P
            AIRERP = 0.5D0*(SIGY+RP)*P
          ELSE IF (PUIS) THEN
C           AMELIORATION DE LA PREDICTION EN ESTIMANT RPRIM(PM+DP0)
            DP0 = ( SIELEQ - SIGY)/1.5D0/DEUXMU
            RPRIM0 = UNSURN*SIGY*COCO * (COCO*DP0)**(UNSURN-1.D0)
            DP0 = DP0 / (1+RPRIM0/1.5D0/DEUXMU)
            XAP   = DP0
            VAL0  = NMCRI2(0.D0)
            PRECR = CRIT(3) * SIGY
            NITER = NINT(CRIT(1))
            CALL ZEROFO(NMCRI2,VAL0,XAP,PRECR,NITER,P,IRET,IBID)
            IF(IRET.EQ.1) CALL U2MESS('F','ALGORITH8_65')
            CALL ECPUIS(E,SIGY,ALFAFA,UNSURN,PM,P,RP,RPRIM)
          ELSE
            CALL RCFONC('E',1,JPROL,JVALE,NBVALE,DUM,E,NU,
     &                  0.D0,RP,RPRIM,AIRERP,SIELEQ,P)
          ENDIF
          G = RP/EPSEQ
        ENDIF
C====================================================================
C CAS LINEAIRE
C====================================================================
      ELSE
        G = DEUXMU
      ENDIF
C====================================================================
C - CALCUL DES CONTRAINTES ET DES PSEUDO VARIABLES INTERNES
C====================================================================
      IF (INCO) THEN
        DO 50 K = 1,NDIMSI
          SIG(K) =  G*EPSDV(K)
 50     CONTINUE
      ELSE
        DO 55 K = 1,NDIMSI
          SIG(K) = TROISK*EPSMO*KRON(K) + G*EPSDV(K)
 55     CONTINUE
C CALCUL DE LA DERIVEE LAGRANGIENNE DU TENSEUR DES CONTRAINTES
C  (DSIG) CAR DL(TROISK)=0 ET DL(G)=0 PAR ELEMENT
        IF (DERIVL) THEN
          DO 56 K = 1,NDIMSI
            DSIG(K) = TROISK*DEPSMO*KRON(K) + G*DEPSDV(K)
 56       CONTINUE
        ENDIF
      ENDIF
C====================================================================
C TRAITEMENTS PARTICULIERS
C====================================================================
      IF ( OPTION(1:9) .EQ. 'RAPH_MECA' .OR.
     &     OPTION(1:9) .EQ. 'FULL_MECA' .OR.
     &     OPTION(1:7) .EQ. 'RUPTURE'  ) THEN
        IF (NONLIN) THEN
          VI = P
        ELSE IF (VMIS) THEN
          VI = 0.D0
        ENDIF
      ENDIF
C - CALCUL DE LA MATRICE DE RIGIDITE TANGENTE
      IF ( OPTION(1:10) .EQ. 'RIGI_MECA_' .OR.
     &     OPTION       .EQ. 'RIGI_MECA' .OR.
     &     OPTION(1:9)  .EQ. 'FULL_MECA' ) THEN

        DO 60, K=1,NDIMSI
          DO 70, L=1,NDIMSI
            DSIDEP(K,L) =  0.D0
 70       CONTINUE
 60     CONTINUE
C      TERME LINEAIRE
        DO 80 K=1,3
          DO 90 L=1,3
            DSIDEP(K,L) = (TROISK-G)/3.D0
 90       CONTINUE
 80     CONTINUE
        DO 100 K=1,NDIMSI
          DSIDEP(K,K) = DSIDEP(K,K) + G
 100    CONTINUE
C      TERME NON LINEAIRE
        IF (NONLIN.AND.(OPTION(11:14).NE.'ELAS')) THEN
          COEF = DEUXMU*RPRIM/(1.5D0*DEUXMU+RPRIM) - G
          COEF = COEF * 3.D0 / (2.D0*EPSEQ*EPSEQ)
          DO 110 K = 1,NDIMSI
            DO 120 L= 1,NDIMSI
              DSIDEP(K,L) = DSIDEP(K,L) + COEF*EPSDV(K)*EPSDV(L)
120         CONTINUE
110       CONTINUE
        ENDIF
C      CORRECTION POUR LES CONTRAINTES PLANES
        IF (CPLAN) THEN
          DO 130 K=1,NDIMSI
            IF (K.EQ.3) GOTO 130
            DO 140 L=1,NDIMSI
              IF (L.EQ.3) GO TO 140
              DSIDEP(K,L)=DSIDEP(K,L)
     &        - 1.D0/DSIDEP(3,3)*DSIDEP(K,3)*DSIDEP(3,L)
 140        CONTINUE
 130      CONTINUE
        ENDIF
      ENDIF
C====================================================================
C CALCUL DE L'ENERGIE LIBRE
C====================================================================
C CALCUL DE L'ENERGIE LIBRE (ENERGI(1)) ET DE SA DERIVEE / T
C (ENERGI(2)) ET DE LEURS DERIVEES LAGRANGIENNES (DENER)
      IF (OPTION(1:7) .EQ. 'RUPTURE') THEN
        DIVU   = 3.D0*EPSMO
C CALCUL INTERMEDIAIRE POUR LA DERIVEE LAGRANGIENNE
        IF (DERIVL) DDIVU = 3.D0*DEPSMO
        CALL NMELRU(FAMI,KPG,KSP,POUM,IMATE,COMPOR,EPSEQ,P,DIVU,
     &              NONLIN,ENERGI,
     &              DERIVL,DDIVU,DEPSEQ,DENERG,DLAGTG)
      ENDIF
      END

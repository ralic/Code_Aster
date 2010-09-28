      SUBROUTINE TE0533(OPTION,NOMTE)
      IMPLICIT   NONE
      CHARACTER*16 OPTION,NOMTE

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 28/09/2010   AUTEUR MASSIN P.MASSIN 
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
C RESPONSABLE GENIAUT S.GENIAUT
C
C.......................................................................
C
C         CALCUL DES MATRICES DE CONTACT FROTTEMENT POUR X-FEM
C                       (METHODE CONTINUE)
C
C
C  OPTION : 'RIGI_CONT' (CALCUL DES MATRICES DE CONTACT)
C  OPTION : 'RIGI_FROT' (CALCUL DES MATRICES DE FROTTEMENT)

C  ENTREES  ---> OPTION : OPTION DE CALCUL
C           ---> NOMTE  : NOM DU TYPE ELEMENT
C
C......................................................................
C TOLE CRP_20
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX --------------------
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

C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX --------------------

      INTEGER      I,J,K,L,IJ,IFA,IPGF,INO,ISSPG,NI,NJ,NLI,NLJ,PLI,PLJ
      INTEGER      JINDCO,JDONCO,JLSN,IPOIDS,IVF,IDFDE,JGANO,IGEOM
      INTEGER      IDEPM,IDEPD,IMATT,JLST,JPTINT,JAINT,JCFACE,JLONCH
      INTEGER      IPOIDF,IVFF,IDFDEF,IADZI,IAZK24,IBID,JBASEC,JSEUIL
      INTEGER      NDIM,NFH,DDLC,DDLS,NDDL,NNO,NNOS,NNOM,NNOF,DDLM
      INTEGER      NPG,NPGF,XOULA,FAC(6,4),NBF
      INTEGER      INDCO(60),NINTER,NFACE,CFACE(5,3),IBID2(12,3),CPT
      INTEGER      INTEG,NFE,SINGU,JSTNO,NVIT
      INTEGER      NNOL,PLA(27),LACT(8),NLACT
      INTEGER      IER,IN,JN,NCONTA
      CHARACTER*8  ELREF,ELREFC,TYPMA,FPG,ELC,LAG
      REAL*8       FFI,FFJ,FFP(27),FFC(8),KNP(3,3)
      REAL*8       MMAT(204,204),JAC,RHON,MU,RHOTK,COEFBU,COEFFR
      REAL*8       NDN(3,6),TAU1(3),TAU2(3),LAMB1(3)
      REAL*8       ND(3),METR(2,2),P(3,3),KN(3,3),R3(3),SEUIL(60),DDOT
      REAL*8       PTKNP(3,3),TAUKNP(2,3),TAIKTA(2,2),IK(3,3)
      REAL*8       LSN,LST,R,RR,E,G(3),RBID,ID(3,3),R8PREM
      REAL*8       CSTACO,CSTAFR,CPENCO,CPENFR,VITANG(3),GT(3),X(4)
      REAL*8       FFPC(27),DFBID(27,3),R3BID(3),R2BID(2)
      INTEGER      ZXAIN,XXMMVD
      LOGICAL      LPENAF,MALIN,LPENAC,ADHER,ISMALI

      INTEGER      JCOHES
      CHARACTER*2  CODRET(3)
      CHARACTER*8  NOMRES(3)
      REAL*8       VALRES(3),PENADH,RELA
      INTEGER      IMATE,III,IA,IB
      REAL*8       VIM(9),VIP(9)
      REAL*8       SAUT(3),DSAUT(3),TAU11(3),TAU22(3)              
      REAL*8       SIGMA(3,3),DSIDEP(3,3)
      REAL*8       AM(3),DAM(3),TSELAS
      REAL*8       COEFF1,COEFF2,COEFF3,COEFF4,UNITY(3,3),PP(3,3)     
      CHARACTER*16 OPTIO2,ENR
      REAL*8       AM2D(2),DAM2D(2),DSID2D(2,2)
      REAL*8       DEPEQI,SQRNOR,SQRTAN,DTANG(3),DNOR(3)
      REAL*8       BETA,BETASQ, GC,SIGMC,ALPHA0,ALPHA
      REAL*8       DDT1(3,3),DDT2(3,3),DDT3(3,3),DDT4(3,3),COHES(60)
      INTEGER      NPTF,NFISS,JFISNO
C......................................................................

      CALL JEMARQ()
C
C-----------------------------------------------------------------------
C     INITIALISATIONS
C-----------------------------------------------------------------------
C
      ZXAIN=XXMMVD('ZXAIN')
      CALL ELREF1(ELREF)
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
C
C     INITIALISATION DES DIMENSIONS DES DDLS X-FEM
      CALL XTEINI(NOMTE,NFH,NFE,SINGU,DDLC,NNOM,DDLS,NDDL,DDLM,NFISS)
C
      CALL TECAEL(IADZI,IAZK24)
      TYPMA=ZK24(IAZK24-1+3+ZI(IADZI-1+2)+3)

      IF (NDIM .EQ. 3) THEN
         CALL CONFAC(TYPMA,IBID2,IBID,FAC,NBF)
      ENDIF

C     INITIALISATION DE LA MATRICE DE TRAVAIL
      CALL MATINI(204,204,0.D0,MMAT)

C     INITIALISATION DES VARIABLES INTERNES POUR CZM-XFEM
      DO 265 III=1,9
      VIM(III)=0.0D0
      VIP(III)=0.0D0
 265  CONTINUE 
C
C------------RECUPERATION DU TYPE DE CONTACT----------------------------
C
      NCONTA=0
      CALL TEATTR (NOMTE,'C','XLAG',LAG,IBID)
      IF (IBID.EQ.0.AND.LAG.EQ.'NOEUD') THEN
        MALIN=.TRUE.
        IF(ISMALI(ELREF))      NCONTA=1
        IF(.NOT.ISMALI(ELREF)) NCONTA=3
      ELSE
        MALIN=.FALSE.
      ENDIF
      CALL ELELIN(NCONTA,ELREF,ELREFC,IBID,IBID)
C
C-----------------------------------------------------------------------
C     RECUPERATION DES ENTREES / SORTIE
C-----------------------------------------------------------------------
C
      CALL JEVECH('PGEOMER','E',IGEOM)
C     DEPMOI
      CALL JEVECH('PDEPL_M','L',IDEPM)
C     DEPDEL
      CALL JEVECH('PDEPL_P','L',IDEPD)
      CALL JEVECH('PINDCOI','L',JINDCO)
      CALL JEVECH('PDONCO','L',JDONCO)
      CALL JEVECH('PSEUIL','L',JSEUIL)
      CALL JEVECH('PLSN','L',JLSN)
      CALL JEVECH('PLST','L',JLST)
      CALL JEVECH('PPINTER','L',JPTINT)
      CALL JEVECH('PAINTER','L',JAINT)
      CALL JEVECH('PCFACE','L',JCFACE)
      CALL JEVECH('PLONCHA','L',JLONCH)
      CALL JEVECH('PBASECO','L',JBASEC)


      CALL TEATTR(NOMTE,'S','XFEM',ENR,IBID)          
      IF (ENR.EQ.'XHC') THEN
        RELA =  ZR(JDONCO-1+10)
      ELSE
        RELA=0.0D0
      ENDIF

      IF(RELA.EQ.1.D0) THEN
        CALL JEVECH('PMATERC','L',IMATE)
        CALL JEVECH('PCOHES' ,'L',JCOHES)
      ENDIF         

C     RECUPERATIONS DES DONNEES SUR LE CONTACT ET
C     SUR LA TOPOLOGIE DES FACETTES
      NINTER=ZI(JLONCH-1+1)
      NFACE=ZI(JLONCH-1+2)
      NPTF=ZI(JLONCH-1+3)

      DO 10 I=1,60
        INDCO(I) = ZI(JINDCO-1+I)
        SEUIL(I) = ZR(JSEUIL-1+I)
      IF(RELA.EQ.1.D0)COHES(I) = ZR(JCOHES-1+I)
 10   CONTINUE

      RHON = ZR(JDONCO-1+1)
      MU = ZR(JDONCO-1+2)
      RHOTK = ZR(JDONCO-1+3)

C     COEFFICIENTS DE STABILISATION
      CSTACO=ZR(JDONCO-1+6)
      CSTAFR=ZR(JDONCO-1+7)

C     COEFFICIENTS DE PENALISATION
      CPENCO=ZR(JDONCO-1+8)
      CPENFR=ZR(JDONCO-1+9)

      IF (CSTACO.EQ.0.D0) CSTACO=RHON
      IF (CSTAFR.EQ.0.D0) CSTAFR=RHOTK

      IF (CPENCO.EQ.0.D0) CPENCO=RHON
      IF (CPENFR.EQ.0.D0) CPENFR=RHOTK

C     PENALISATION PURE
C     PENALISATION DU CONTACT
      LPENAC=.FALSE.
      IF (CSTACO.EQ.0.D0) THEN
        CSTACO=CPENCO
        IF(CPENCO.NE.0.0D0)LPENAC=.TRUE.
      ENDIF
C     PENALISATION DU FROTTEMENT
      LPENAF=.FALSE.
      IF (CSTAFR.EQ.0.D0) THEN
        IF(CPENFR.NE.0.0D0)LPENAF=.TRUE.
      ENDIF
      
C     SCHEMA D'INTEGRATION NUMERIQUE ET ELEMENT DE REFERENCE DE CONTACT
C     DISCUSSION VOIR BOOK IV 18/10/2004 ET BOOK VI 06/07/2005
      INTEG = NINT(ZR(JDONCO-1+4))
      IF (NDIM .EQ. 3) THEN
        IF (INTEG.EQ.1) FPG='NOEU'
        IF (INTEG.EQ.2) FPG='GAUSS'
        IF (INTEG.EQ.3) FPG='SIMP'
        IF (INTEG.EQ.4) FPG='SIMP1'
        IF (INTEG.EQ.6) FPG='COTES'
        IF (INTEG.EQ.10) FPG='XCON'
        IF (INTEG.EQ.14) FPG='FPG4'
        IF (INTEG.EQ.16) FPG='FPG6'
        IF (INTEG.EQ.17) FPG='FPG7'
        ELC='TR3'
      ELSEIF (NDIM.EQ.2) THEN
        IF (INTEG.EQ.1) FPG='NOEU'
        IF (INTEG.EQ.2) FPG='GAUSS'
        IF (INTEG.EQ.3) FPG='SIMP'
        IF (INTEG.EQ.4) FPG='SIMP1'
        IF (INTEG.EQ.6) FPG='COTES'
        IF (INTEG.EQ.7) FPG='COTES1'
        IF (INTEG.EQ.8) FPG='COTES2'
        IF (INTEG.EQ.12) FPG='FPG2'
        IF (INTEG.EQ.13) FPG='FPG3'
        IF (INTEG.EQ.14) FPG='FPG4'
        IF(ISMALI(ELREF)) THEN
          ELC='SE2'
        ELSE
          ELC='SE3'
        ENDIF
      ENDIF

      CALL ELREF4(ELC,FPG,IBID,NNOF,IBID,NPGF,IPOIDF,IVFF,IDFDEF,IBID)

      DO 11 I=1,NFACE
        DO 12 J=1,NPTF
          CFACE(I,J)=ZI(JCFACE-1+NDIM*(I-1)+J)
 12      CONTINUE
 11   CONTINUE

C     RECUPERATION DU COEFFICIENT DE MISE À L'ECHELLE DES PRESSIONS
      E=ZR(JDONCO-1+5)
C
C     LISTE DES LAMBDAS ACTIFS
C
      IF(MALIN) CALL XLACTI(TYPMA,NINTER,JAINT,LACT,NLACT)

C-----------------------------------------------------------------------
C
C     BOUCLE SUR LES FACETTES
      DO 100 IFA=1,NFACE

C       NOMBRE DE LAMBDAS ET LEUR PLACE DANS LA MATRICE
        IF (MALIN) THEN
          IF (NCONTA.EQ.1) NNOL=NNO
          IF (NCONTA.EQ.3) NNOL=NNOS
          DO 15 I=1,NNOL
            CALL XPLMAT(NDIM,NFH,NFE,DDLC,DDLM,NNO,NNOM,I,PLI)
            PLA(I)=PLI
 15       CONTINUE
        ELSE
          NNOL=NNOF
          DO 16 I=1,NNOF
C           XOULA  : RENVOIE LE NUMERO DU NOEUD PORTANT CE LAMBDA
            NI=XOULA(CFACE,IFA,I,JAINT,TYPMA)
C           PLACE DU LAMBDA DANS LA MATRICE
            CALL XPLMAT(NDIM,NFH,NFE,DDLC,DDLM,NNO,NNOM,NI,PLI)
            PLA(I)=PLI
 16       CONTINUE
        ENDIF

C       BOUCLE SUR LES POINTS DE GAUSS DES FACETTES
        DO 110 IPGF=1,NPGF
C
C         INDICE DE CE POINT DE GAUSS DANS INDCO
          ISSPG=NPGF*(IFA-1)+IPGF

C         CALCUL DE JAC (PRODUIT DU JACOBIEN ET DU POIDS)
C         ET DES FF DE L'ELEMENT PARENT AU POINT DE GAUSS
C         ET LA NORMALE ND ORIENTÉE DE ESCL -> MAIT

          IF (NDIM.EQ.3) THEN
            CALL XJACFF(ELREF,ELREFC,ELC,NDIM,FPG,JPTINT,IFA,CFACE,IPGF,
     &                  NNO,IGEOM,JBASEC,G,'NON',JAC,FFP,FFPC,DFBID,ND,
     &                  TAU1,TAU2)
          ELSEIF (NDIM.EQ.2) THEN
            CALL XJACF2(ELREF,ELREFC,ELC,NDIM,FPG,JPTINT,IFA,CFACE,NPTF,
     &                  IPGF,NNO,IGEOM,JBASEC,G,'NON',JAC,FFP,FFPC,
     &                  DFBID,ND,TAU1)
          ENDIF

C        CALCUL DES FONCTIONS DE FORMES DE CONTACT DANS LE CAS LAG NOEUD
          IF (NCONTA.EQ.1) THEN
            CALL XMOFFC(LACT,NLACT,NNO,FFP,FFC)
          ELSEIF (NCONTA.EQ.3) THEN
            CALL XMOFFC(LACT,NLACT,NNOS,FFPC,FFC)
          ENDIF

C         CE POINT DE GAUSS EST-IL SUR UNE ARETE?
          K=0
          DO 17 I=1,NINTER
            IF (K.EQ.0) THEN
              X(4)=0.D0
              DO 20 J=1,NDIM
                X(J)=ZR(JPTINT-1+NDIM*(I-1)+J)
 20           CONTINUE
              DO 21 J=1,NDIM
                X(4) = X(4) + (X(J)-G(J))*(X(J)-G(J))
 21           CONTINUE
              X(4) = SQRT(X(4))
              IF (X(4).LT.1.D-12) THEN
                K=I
                GOTO 17
              ENDIF
            ENDIF
 17       CONTINUE

          IF (K.NE.0) THEN
            NVIT = ZR(JAINT-1+ZXAIN*(K-1)+5)
          ELSE
            NVIT = 0
          ENDIF
C         IL NE FAUT PAS UTILISER NVIT SI LE SCHEMA D'INTEGRATION
C         NE CONTIENT PAS DE NOEUDS
          IF ((FPG(1:3).EQ.'FPG').OR.(FPG.EQ.'GAUSS')
     &             .OR.(FPG.EQ.'XCON')) NVIT=1

C         CALCUL DE RR = SQRT(DISTANCE AU FOND DE FISSURE)
          IF (SINGU.EQ.1) THEN
            LSN=0.D0
            LST=0.D0
            DO 112 I=1,NNO
              LSN=LSN+ZR(JLSN-1+I)*FFP(I)
              LST=LST+ZR(JLST-1+I)*FFP(I)
 112        CONTINUE
C           LSN NON NUL SUR LA SURFACE.
            CALL ASSERT(ABS(LSN).LE.1.D-3)
            R=ABS(LST)
            RR=SQRT(R)
          ENDIF

C         I) CALCUL DES MATRICES DE CONTACT
C         ..............................

          IF (OPTION.EQ.'RIGI_CONT') THEN
C
C           SI PAS DE CONTACT POUR CE PG : ON REMPLIT LA MATRICE C
            IF (INDCO(ISSPG).EQ.0) THEN

              IF (NVIT.NE.0) THEN

                DO 120 I = 1,NNOL

                  PLI=PLA(I)
                  IF (MALIN) THEN
                    FFI=FFC(I)
                  ELSE
                    FFI=ZR(IVFF-1+NNOF*(IPGF-1)+I)
                  ENDIF

                  DO 121 J = 1,NNOL

                    PLJ=PLA(J)
                    IF (MALIN) THEN
                      FFJ=FFC(J)
                    ELSE
                      FFJ=ZR(IVFF-1+NNOF*(IPGF-1)+J)
                    ENDIF

                    MMAT(PLI,PLJ) = MMAT(PLI,PLJ)
     &                       - FFJ * FFI * JAC / CSTACO * E * E
 121              CONTINUE
 120            CONTINUE

              ENDIF
C
C           SI CONTACT POUR CE PG : ON REMPLIT LES MATRICES A, AT ET A_U
            ELSE IF (INDCO(ISSPG).EQ.1) THEN

C             I.1. CALCUL DE A ET DE AT
              DO 130 I = 1,NNOL

                PLI=PLA(I)
                IF (MALIN) THEN
                  FFI=FFC(I)
                ELSE
                  FFI=ZR(IVFF-1+NNOF*(IPGF-1)+I)
                ENDIF

                DO 131 J = 1,NNO
                  CALL INDENT(J,DDLS,DDLM,NNOS,JN)

                  DO 132 L = 1,NFH*NDIM
                    MMAT(PLI,JN+NDIM+L)=
     &              MMAT(PLI,JN+NDIM+L)+
     &              2.D0 * FFI * FFP(J) * ND(L) * JAC * E
C
                    IF(.NOT.LPENAC)THEN
                      MMAT(JN+NDIM+L,PLI)=
     &                MMAT(JN+NDIM+L,PLI)+
     &                2.D0 * FFI * FFP(J) * ND(L) * JAC * E
                    ENDIF
 132              CONTINUE
C
                  DO 133 L = 1,SINGU*NDIM
                    MMAT(PLI,JN+NDIM*(1+NFH)+L)=
     &              MMAT(PLI,JN+NDIM*(1+NFH)+L)+
     &              2.D0 * FFI * FFP(J) * RR * ND(L) * JAC * E
C
                    IF(.NOT.LPENAC)THEN
                      MMAT(JN+NDIM*(1+NFH)+L,PLI)=
     &                MMAT(JN+NDIM*(1+NFH)+L,PLI)+
     &                2.D0 * FFI * FFP(J) * RR * ND(L) * JAC * E
                    ENDIF
 133              CONTINUE

 131            CONTINUE

 130          CONTINUE
C
C             I.2. CALCUL DE A_U
              DO 140 I = 1,NNO
                CALL INDENT(I,DDLS,DDLM,NNOS,IN)

                DO 141 J = 1,NNO
                  CALL INDENT(J,DDLS,DDLM,NNOS,JN)

                  DO 142 K = 1,NFH*NDIM
                    DO 143 L = 1,NFH*NDIM
                      MMAT(IN+NDIM+K,JN+NDIM+L) =
     &                  MMAT(IN+NDIM+K,JN+NDIM+L) +
     &                  4.D0*CPENCO*FFP(I)*FFP(J)*ND(K)*ND(L)*JAC
 143                CONTINUE
C
                    DO 144 L = 1,SINGU*NDIM
                      MMAT(IN+NDIM+K,JN+NDIM*(1+NFH)+L) =
     &                  MMAT(IN+NDIM+K,JN+NDIM*(1+NFH)+L) +
     &                  4.D0*CPENCO*FFP(I)*FFP(J)*RR*ND(K)*ND(L)*JAC
 144                CONTINUE
C
 142              CONTINUE

                  DO 145 K = 1,SINGU*NDIM
                    DO 146 L = 1,NFH*NDIM
                      MMAT(IN+NDIM*(1+NFH)+K,JN+NDIM+L) =
     &                  MMAT(IN+NDIM*(1+NFH)+K,JN+NDIM+L) +
     &                  4.D0*CPENCO*FFP(I)*FFP(J)*RR*ND(K)*ND(L)*JAC
 146                CONTINUE
C
                    DO 147 L = 1,SINGU*NDIM
                      MMAT(IN+NDIM*(1+NFH)+K,JN+NDIM*(1+NFH)+L) =
     &                  MMAT(IN+NDIM*(1+NFH)+K,JN+NDIM*(1+NFH)+L) +
     &                  4.D0*CPENCO*FFP(I)*FFP(J)*RR*RR*ND(K)*ND(L)
     &                                                    *JAC
 147                CONTINUE
 145              CONTINUE

 141            CONTINUE
 140          CONTINUE
C
C             I.3. SI PENALISATION PURE CALCUL DE C
              IF (LPENAC) THEN
                DO 220 I = 1,NNOL

                  PLI=PLA(I)
                  IF (MALIN) THEN
                    FFI=FFC(I)
                  ELSE
                    FFI=ZR(IVFF-1+NNOF*(IPGF-1)+I)
                  ENDIF

                  DO 221 J = 1,NNOL

                    PLJ=PLA(J)
                    IF (MALIN) THEN
                      FFJ=FFC(J)
                    ELSE
                      FFJ=ZR(IVFF-1+NNOF*(IPGF-1)+J)
                    ENDIF
C
                    MMAT(PLI,PLJ) = MMAT(PLI,PLJ)
     &                       - FFJ * FFI * JAC / CSTACO * E * E

 221              CONTINUE
 220            CONTINUE
              ENDIF
C
            ELSE
C             SI INDCO N'EST NI EGAL A 0 NI EGAL A 1
C             PROBLEME DE STATUT DE CONTACT.
              CALL ASSERT(INDCO(ISSPG).EQ.0 .OR. INDCO(ISSPG).EQ.1)
            END IF
C
C         II) CALCUL DES MATRICES DE FROTTEMENT
C         ..............................

          ELSEIF (OPTION.EQ.'RIGI_FROT') THEN

            IF (MU.EQ.0.D0.OR.SEUIL(ISSPG).EQ.0.D0) INDCO(ISSPG) = 0

C           SI PAS DE CONTACT POUR CE PG : ON REMPLIT QUE LA MATRICE F
            IF (INDCO(ISSPG).EQ.0) THEN

              IF (NVIT.NE.0) THEN

                DO 150 I = 1,NNOL
                  PLI=PLA(I)
                  IF (MALIN) THEN
                    FFI=FFC(I)
                    NLI=LACT(I)
                    IF (NLI.EQ.0) GOTO 150
                  ELSE
                    FFI=ZR(IVFF-1+NNOF*(IPGF-1)+I)
                    NLI=CFACE(IFA,I)
                  ENDIF

                  DO 151 J = 1,NNOL
                    PLJ=PLA(J)
                    IF (MALIN) THEN
                      FFJ=FFC(J)
                      NLJ=LACT(J)
                      IF (NLJ.EQ.0) GOTO 151
                    ELSE
                      FFJ=ZR(IVFF-1+NNOF*(IPGF-1)+J)
                      NLJ=CFACE(IFA,J)
                    ENDIF

C                   MÉTRIQUE DE LA BASE COVARIANTE AUX PTS D'INTERSECT
                    METR(1,1)=DDOT(NDIM,TAU1(1),1,TAU1(1),1)
                    IF (NDIM.EQ.3) THEN
                      METR(1,2)=DDOT(NDIM,TAU1(1),1,TAU2(1),1)
                      METR(2,1)=DDOT(NDIM,TAU2(1),1,TAU1(1),1)
                      METR(2,2)=DDOT(NDIM,TAU2(1),1,TAU2(1),1)
                    ENDIF

                    DO 152 K = 1,NDIM-1
                      DO 153 L = 1,NDIM-1
                        MMAT(PLI+K,PLJ+L) = MMAT(PLI+K,PLJ+L)
     &                           + FFI * FFJ * METR(K,L) * JAC
 153                  CONTINUE
 152                CONTINUE
 151              CONTINUE
 150            CONTINUE

             ENDIF

C           II.2. CALCUL DES MATRICES DE COHESION

C           II.2.1. ACTIVATION DE LA LOI COHESIVE 
C                   ET RECUPERATION DES PARAMETRES MATERIAUX :
C        .............................. 
              IF(RELA.NE.1.0D0) THEN
                GO TO 53
              ENDIF
                            
              NOMRES(1) = 'GC'
              NOMRES(2) = 'SIGM_C'
              NOMRES(3) = 'PENA_ADH'
             
              CALL RCVALA( ZI(IMATE),' ','RUPT_FRAG',0,' ',0.D0,3,
     &                     NOMRES,VALRES,CODRET, 'FM' )
              GC   = VALRES(1)              
              SIGMC  = VALRES(2)               
              PENADH = VALRES(3)
              BETA=1.0D0                
              BETASQ=BETA*BETA
              ALPHA0=(GC/SIGMC)*PENADH
C
C           II.2.2. CALCUL DU SAUT DE DEPLACEMENT EQUIVALENT [[UEG]]
C        .............................. 
              DEPEQI=0.0D0
              CALL MATINI(3,3,0.D0,DSIDEP)
              CALL MATINI(2,2,0.D0,DSID2D)
              CALL MATINI(3,3,0.D0,SIGMA)
              CALL MATINI(3,3,0.D0,UNITY)
              CALL VECINI(3,0.D0,AM)
              CALL VECINI(3,0.D0,DAM)
              CALL MATINI(3,3,0.D0,PP)
              CALL VECINI(3,0.D0,TAU11)
              CALL VECINI(3,0.D0,TAU22)

              DO 215 I = 1,NDIM
                UNITY(I,I)= 1.0D0
 215          CONTINUE  
        
C             P : OPERATEUR DE PROJECTION 3D
              CALL XMAFR1(3,ND,P)                
              CALL VECINI(3,0.D0,SAUT)
C 
              DO 204 INO=1,NNO
                DO 205 J=1,NFH*NDIM
                  SAUT(J) = SAUT(J) - 2.D0 * FFP(INO) *
     &                       (ZR(IDEPD-1+DDLS*(INO-1)+NDIM+J)
     &                       +ZR(IDEPM-1+DDLS*(INO-1)+NDIM+J))
 205            CONTINUE                            
                DO 206 J = 1,SINGU*NDIM
                  SAUT(J) = SAUT(J) - 2.D0 * FFP(INO) * RR *
     &                       (ZR(IDEPD-1+DDLS*(INO-1)+NDIM*(1+NFH)+J)
     &                       +ZR(IDEPM-1+DDLS*(INO-1)+NDIM*(1+NFH)+J))
 206            CONTINUE
 204          CONTINUE
              DO 310 I = 1,NNOL
                PLI=PLA(I)
                IF (MALIN) THEN
                    FFI=FFC(I)
                    NLI=LACT(I)
                    IF (NLI.EQ.0) GOTO 310
                ELSE
                    FFI=ZR(IVFF-1+NNOF*(IPGF-1)+I)
                    NLI=CFACE(IFA,I)
                 ENDIF
                 DO 311 J=1,NDIM
                   IF (NDIM .EQ.3) THEN
                       TAU11(J)=TAU11(J)+FFI*TAU1(J)
                       TAU22(J)=TAU22(J)+FFI*TAU2(J)
                   ELSEIF (NDIM.EQ.2) THEN
                       TAU11(J)=TAU11(J)+FFI*TAU1(J)
                   ENDIF
 311             CONTINUE
 310          CONTINUE 

              DO 218 I=1,NDIM
                DTANG(I) = 0.D0
                DNOR(I) = 0.D0

                DO 219  K=1,NDIM
                  PP(I,K)=ND(I)*ND(K)
                  DTANG(I)=DTANG(I)+P(I,K)*SAUT(K)
                  DNOR(I) = DNOR(I) +PP(I,K)*SAUT(K)                    
 219            CONTINUE 
                AM(1) = AM(1) + DNOR(I)*ND(I)
                AM(2) = AM(2) + DTANG(I)*TAU11(I)
                AM(3) = AM(3) + DTANG(I)*TAU22(I)
                DAM(1) = AM(1)
                DAM(2) = AM(2)
                DAM(3) = AM(3)  
 218          CONTINUE
 
              SQRTAN=0.D0
              SQRNOR=0.D0
                
              DO 214 I=1,NDIM
                SQRTAN=SQRTAN+DTANG(I)**2
                SQRNOR=SQRNOR+DNOR(I)**2
 214          CONTINUE 
 
              DEPEQI = SQRT(SQRNOR+BETASQ*SQRTAN)                 
              ALPHA = DEPEQI
C                
C VIM = VARIABLES INTERNES UTILISEES DANS LCEJEX
C.............VIM(1): SEUIL, PLUS GRANDE NORME DU SAUT
C.............VIM(2): INDICATEUR DE DISSIPATION (0 : NON, 1 : OUI)
              IF ( ( ALPHA . GE . ALPHA0) . AND . 
     &                ( ALPHA . GE . COHES(ISSPG)*1.001D0) ) THEN
                VIM(1) = ALPHA
                VIM(2) = 1
              ELSE
                VIM(1) = COHES(ISSPG)   
                VIM(2) = 0
              ENDIF
C                
C L'INTERPENETRATION DES LEVRES DE LA FISSURE CORRESPOND
C        A UN SAUT NEGATIF DANS LCEJEX                  
C
              DO 777 I=1,NDIM
                AM(I)=-AM(I)
                DAM(I)=-DAM(I)
 777          CONTINUE
 
              OPTIO2=OPTION
              OPTION='RIGI_MECA'
              
              IF (NDIM.EQ.2)   THEN
                           
                AM2D(1)=AM(1)
                AM2D(2)=AM(2)              
                DAM2D(1)=DAM(1)
                DAM2D(2)=DAM(2)
                CALL LCEJEX('RIGI',IPGF,1,2,ZI(IMATE),OPTION,AM2D, 
     &                      DAM2D,SIGMA,DSID2D,VIM,VIP)
                DSIDEP(1,1)=DSID2D(1,1)                
                DSIDEP(1,2)=DSID2D(1,2)                
                DSIDEP(2,1)=DSID2D(2,1)                
                DSIDEP(2,2)=DSID2D(2,2)                
              
              ELSE IF (NDIM.EQ.3)   THEN                         
                CALL LCEJEX('RIGI',IPGF,1,3,ZI(IMATE),OPTION,AM, 
     &                      DAM,SIGMA,DSIDEP,VIM,VIP)
              ENDIF     
     
              OPTION=OPTIO2

              IF (ABS(DEPEQI) . LE . R8PREM() ) THEN
                  GOTO 110
              ELSE
C
C           II.2.3. CALCUL DES MATRICES DE COHESION 
C        .............................. 
                CALL MATINI(3,3,0.D0,DDT1)
                CALL MATINI(3,3,0.D0,DDT2)
                CALL MATINI(3,3,0.D0,DDT3)
                CALL MATINI(3,3,0.D0,DDT4)    
C
                IF ( ( ALPHA . GE . ALPHA0) . AND . 
     &                  ( ALPHA . GE . COHES(ISSPG)*1.001D0) ) THEN
C                 CHARGE COHESIVE-ENDOMAGEMENT 
                  COEFF1 = (DSIDEP(1,1)-DSIDEP(2,2))/
     &                         ((AM(1))**2-(AM(2))**2)
                  COEFF2 = DSIDEP(1,1) - COEFF1*(AM(1))**2 
                  COEFF3 = -(SIGMC*COEFF2)/DEPEQI                
                  COEFF4 = (SIGMC*DEPEQI+GC)/(DEPEQI*SIGMC*GC)
                  
                  DO 216 I = 1,NDIM
                    DO 217 J = 1,NDIM
                      DDT1(I,J)= COEFF1*DNOR(I)*DNOR(J)+
     &                              COEFF2*UNITY(I,J)
                      DDT2(I,J)=(COEFF3*DNOR(I)*DTANG(J)*COEFF4)
                      DDT3(I,J)=(COEFF3*DTANG(I)*DNOR(J)*COEFF4)
                      DDT4(I,J)= COEFF1*DTANG(I)*DTANG(J)+
     &                              COEFF2*UNITY(I,J)
 217                CONTINUE
 216              CONTINUE
              
                ELSE IF ( ( ALPHA . LT . ALPHA0) . AND . 
     &                  ( ALPHA . GE . COHES(ISSPG)*1.001D0) ) THEN
C                 CHARGE ELASTIQUE AVANT ENDOMAGEMENT     
                  TSELAS = DSIDEP(1,1)
                  DO 316 I = 1,NDIM
                    DO 317 J = 1,NDIM                        
                      DDT1(I,J)=TSELAS*UNITY(I,J)
                      DDT2(I,J)=0.0D0
                      DDT3(I,J)=0.0D0
                      DDT4(I,J)=TSELAS*UNITY(I,J)
317                 CONTINUE
316               CONTINUE

                ELSE IF ( ALPHA . LT . COHES(ISSPG)*1.001D0)  THEN
C                 CHARGE ET DECHARGE ELASTIQUE APRES ENDOMAGEMENT
                  ALPHA = COHES(ISSPG)
                  TSELAS = DSIDEP(1,1)

                  DO 416 I = 1,NDIM
                    DO 417 J = 1,NDIM
                      DDT1(I,J)=TSELAS*UNITY(I,J)
                      DDT2(I,J)=0.0D0
                      DDT3(I,J)=0.0D0
                      DDT4(I,J)=TSELAS*UNITY(I,J)
 417                CONTINUE
 416              CONTINUE 

                ELSE
C                ALPHA NE CORRESPOND A AUCUN CAS
                 CALL ASSERT(.FALSE.)
                ENDIF

                CALL MATCOX(NDIM,PP,DDT1,DDT2,DDT3,DDT4,P,
     &                  NNO,NFH*NDIM,DDLS,JAC,FFP,SINGU,RR,
     &                  MMAT)  
              ENDIF
C 
 53           CONTINUE


C           II.3. SI CONTACT POUR CE PG : ON REMPLIT B, BT, B_U ET F
            ELSE IF (INDCO(ISSPG).EQ.1) THEN

C             INITIALISATIONS DES MATRICES DE TRAVAIL 3X3, EN 2D ET 3D
              CALL MATINI(3,3,0.D0,P)
              CALL MATINI(3,3,0.D0,PTKNP)
              CALL MATINI(2,3,0.D0,TAUKNP)

C             P : OPÉRATEUR DE PROJECTION

              CALL XMAFR1(NDIM,ND,P)

C             ON TESTE L'ETAT D'ADHERENCE DU PG (AVEC DEPDEL)
              CALL VECINI(3,0.D0,SAUT)
              CALL VECINI(3,0.D0,LAMB1)

              DO 154 INO=1,NNO
                CALL INDENT(INO,DDLS,DDLM,NNOS,IN)

                DO 155 J=1,NFH*NDIM
                  SAUT(J) = SAUT(J) - 2.D0 * FFP(INO) *
     &             ZR(IDEPD-1+IN+NDIM+J)
 155            CONTINUE
                DO 156 J = 1,SINGU*NDIM
                  SAUT(J) = SAUT(J) - 2.D0 * FFP(INO) * RR *
     &             ZR(IDEPD-1+IN+NDIM*(1+NFH)+J)

 156            CONTINUE
 154          CONTINUE

              DO 158 I=1,NNOL
                PLI=PLA(I)

                IF (MALIN) THEN
                  FFI=FFC(I)
                  NLI=LACT(I)
                  IF (NLI.EQ.0) GOTO 158
                ELSE
                  FFI=ZR(IVFF-1+NNOF*(IPGF-1)+I)
                  NLI=CFACE(IFA,I)
                ENDIF

                DO 159 J=1,NDIM
                  LAMB1(J)=LAMB1(J) + FFI * TAU1(J) *
     &                    (ZR(IDEPD-1+PLI+1)+ZR(IDEPM-1+PLI+1))

                  IF (NDIM.EQ.3)
     &              LAMB1(J)=LAMB1(J) + FFI * TAU2(J) *
     &                       (ZR(IDEPD-1+PLI+2) + ZR(IDEPM-1+PLI+2))
 159            CONTINUE
 158          CONTINUE

              CALL XADHER(P,SAUT,LAMB1,RHOTK,CSTAFR,CPENFR,LPENAF,
     &                     VITANG,R3,KN,PTKNP,IK,ADHER)

              CALL PROMAT(KN,3,NDIM,NDIM,P,3,NDIM,NDIM,KNP)

C             II.3.1. CALCUL DE B ET DE BT

              DO 160 I = 1,NNOL

                PLI=PLA(I)
                IF (MALIN) THEN
                  FFI=FFC(I)
                  NLI=LACT(I)
                  IF (NLI.EQ.0) GOTO 160
                ELSE
                  FFI=ZR(IVFF-1+NNOF*(IPGF-1)+I)
                  NLI=CFACE(IFA,I)
                ENDIF

C               CALCUL DE TAU.KN.P
                DO 161 J = 1,NDIM
                  TAUKNP(1,J) = 0.D0
                  DO 162 K = 1,NDIM
                    TAUKNP(1,J) = TAUKNP(1,J) + TAU1(K) * KNP(K,J)
 162              CONTINUE
 161            CONTINUE

                IF (NDIM.EQ.3) THEN
                  DO 163 J = 1,NDIM
                    TAUKNP(2,J) = 0.D0
                    DO 164 K = 1,NDIM
                      TAUKNP(2,J) = TAUKNP(2,J) + TAU2(K) * KNP(K,J)
 164                CONTINUE
 163              CONTINUE
                ENDIF

                DO 165 J = 1,NNO
                  CALL INDENT(J,DDLS,DDLM,NNOS,JN)
                  DO 166 K = 1,NDIM-1
                    DO 167 L = 1,NFH*NDIM
                      MMAT(PLI+K,JN+NDIM+L) =
     &                MMAT(PLI+K,JN+NDIM+L) +
     &              2.D0*MU*SEUIL(ISSPG)*FFI*FFP(J)*TAUKNP(K,L)*JAC
C
                      IF(.NOT.LPENAF)THEN
                        MMAT(JN+NDIM+L,PLI+K) =
     &                  MMAT(JN+NDIM+L,PLI+K) +
     &              2.D0*MU*SEUIL(ISSPG)*FFI*FFP(J)*TAUKNP(K,L)*JAC
                      ENDIF
C
 167                CONTINUE
C
                    DO 168 L = 1,SINGU*NDIM
C
                      MMAT(PLI+K,JN+NDIM*(1+NFH)+L) =
     &                MMAT(PLI+K,JN+NDIM*(1+NFH)+L) +
     &           2.D0*RR*MU*SEUIL(ISSPG)*FFI*FFP(J)*TAUKNP(K,L)*JAC
C
                      IF(.NOT.LPENAF)THEN
                        MMAT(JN+NDIM*(1+NFH)+L,PLI+K) =
     &                  MMAT(JN+NDIM*(1+NFH)+L,PLI+K) +
     &           2.D0*RR*MU*SEUIL(ISSPG)*FFI*FFP(J)*TAUKNP(K,L)*JAC
                      ENDIF
C
 168                CONTINUE
 166              CONTINUE
 165            CONTINUE
 160          CONTINUE

C             II.3.2. CALCUL DE B_U

              IF (ADHER) THEN

C               CAS ADHERENT, TERME DE PENALISATION:
C               ON A ALORS PTKNP=PT.ID.P
                COEFBU=CPENFR

              ELSE

                IF (LPENAF) THEN
C                 CAS GLISSANT, PENALISATION SEULE
                  COEFBU=CPENFR
                ELSE
C                 CAS GLISSANT
                  COEFBU=CSTAFR
                ENDIF

              ENDIF

              DO 170 I = 1,NNO
                CALL INDENT(I,DDLS,DDLM,NNOS,IN)

                DO 171 J = 1,NNO
                  CALL INDENT(J,DDLS,DDLM,NNOS,JN)

                  DO 172 K = 1,NFH*NDIM
                    DO 173 L = 1,NFH*NDIM
C
                      MMAT(IN+NDIM+K,JN+NDIM+L) =
     &                MMAT(IN+NDIM+K,JN+NDIM+L) -
     &                4.D0*MU*SEUIL(ISSPG)*COEFBU*FFP(I)*FFP(J)*
     &                                        PTKNP(K,L)*JAC
C
 173                CONTINUE
C
                    DO 174 L = 1,SINGU*NDIM
C
                      MMAT(IN+NDIM+K,JN+NDIM*(1+NFH)+L) =
     &                MMAT(IN+NDIM+K,JN+NDIM*(1+NFH)+L) -
     &                4.D0*RR*MU*SEUIL(ISSPG)*COEFBU*FFP(I)*FFP(J)*
     &                                          PTKNP(K,L)*JAC
C
 174                CONTINUE
 172              CONTINUE

                  DO 175 K = 1,SINGU*NDIM
                    DO 176 L = 1,NFH*NDIM
C
                      MMAT(IN+NDIM*(1+NFH)+K,JN+NDIM+L) =
     &                MMAT(IN+NDIM*(1+NFH)+K,JN+NDIM+L) -
     &                4.D0*RR*MU*SEUIL(ISSPG)*COEFBU*FFP(I)*FFP(J)*
     &                                        PTKNP(K,L)*JAC
C
 176                CONTINUE
                    DO 177 L = 1,SINGU*NDIM
C
                     MMAT(IN+NDIM*(1+NFH)+K,JN+NDIM*(1+NFH)+L)
     &            =  MMAT(IN+NDIM*(1+NFH)+K,JN+NDIM*(1+NFH)+L)
     &            -  4.D0*RR*RR*MU*SEUIL(ISSPG)*COEFBU*FFP(I)*FFP(J)*
     &                                       PTKNP(K,L)*JAC
C
 177                CONTINUE
 175              CONTINUE

 171            CONTINUE
 170          CONTINUE

C
C             II.3.3. CALCUL DE F

C             F EST NULLE SI LE POINT EST ADHERENT, ET
C             SI ON N'EST PAS EN PENALISATION SEULE
              IF ((.NOT.ADHER).OR.LPENAF) THEN

C               LE COEFFICIENT DE F_R EST CELUI DE STABILISATION
                COEFFR=CSTAFR
C               SAUF EN CAS DE PENALISATION SEULE
                IF (LPENAF) COEFFR=CPENFR

                DO 180 I = 1,NNOL
                  PLI=PLA(I)
                  IF (MALIN) THEN
                    FFI=FFC(I)
                    NLI=LACT(I)
                    IF (NLI.EQ.0) GOTO 180
                  ELSE
                    FFI=ZR(IVFF-1+NNOF*(IPGF-1)+I)
                    NLI=CFACE(IFA,I)
                  ENDIF
                  DO 181 J = 1,NNOL
                    PLJ=PLA(J)
                    IF (MALIN) THEN
                      FFJ=FFC(J)
                      NLJ=LACT(J)
                      IF (NLJ.EQ.0) GOTO 181
                    ELSE
                      FFJ=ZR(IVFF-1+NNOF*(IPGF-1)+J)
                      NLJ=CFACE(IFA,J)
                    ENDIF

                    IF (LPENAF) THEN
C                     PENALISATION SEULE, TAIKTA=TAUT.ID.TAU
                      CALL MATINI(3,3,0.D0,ID)
                      ID(1,1)=1.0D0
                      ID(2,2)=1.0D0
                      ID(3,3)=1.0D0
                      CALL XMAFR2(TAU1,TAU2,ID,TAIKTA)
                    ELSE
C                     CALCUL DE TAIKTA = TAUT.(ID-KN).TAU
                      CALL XMAFR2(TAU1,TAU2,IK,TAIKTA)
                    ENDIF

                    DO 182 K = 1,NDIM-1
                      DO 183 L = 1,NDIM-1
                        MMAT(PLI+K,PLJ+L) = MMAT(PLI+K,PLJ+L)
     &                           + (MU*SEUIL(ISSPG)/COEFFR)*
     &                           FFI*FFJ*TAIKTA(K,L)*JAC

 183                  CONTINUE
 182                CONTINUE
 181              CONTINUE
 180            CONTINUE
              ENDIF

            ELSE
C             SI INDCO N'EST NI ÉGAL À 0 NI ÉGAL À 1
C             PROBLEME DE STATUT DE CONTACT.
              CALL ASSERT(INDCO(ISSPG).EQ.0 .OR. INDCO(ISSPG).EQ.1)
            END IF

          ELSE
C           SI OPTION NI 'RIGI_CONT' NI 'RIGI_FROT'
            CALL ASSERT(OPTION.EQ.'RIGI_FROT' .OR.
     &                  OPTION.EQ.'RIGI_CONT')
          ENDIF

C         FIN DE BOUCLE SUR LES POINTS DE GAUSS
 110    CONTINUE

C       FIN DE BOUCLE SUR LES FACETTES
 100  CONTINUE
C
C-----------------------------------------------------------------------
C     COPIE DES CHAMPS DE SORTIES ET FIN
C-----------------------------------------------------------------------
C
      IF((LPENAC.AND.(OPTION.EQ.'RIGI_CONT'))
     &    .OR.(LPENAF.AND.(OPTION.EQ.'RIGI_FROT')))THEN
C --- RECUPERATION DE LA MATRICE 'OUT' NON SYMETRIQUE
        CALL JEVECH('PMATUNS','E',IMATT)
        DO 201 J = 1,NDDL
          DO 211 I = 1,NDDL
            IJ = J+NDDL*(I-1)
            ZR(IMATT+IJ-1) = MMAT(I,J)
 211      CONTINUE
 201    CONTINUE
      ELSE
C --- RECUPERATION DE LA MATRICE 'OUT' SYMETRIQUE
        CALL JEVECH('PMATUUR','E',IMATT)
        DO 200 J = 1,NDDL
          DO 210 I = 1,J
            IJ = (J-1)*J/2 + I
            ZR(IMATT+IJ-1) = MMAT(I,J)
 210      CONTINUE
 200    CONTINUE
      ENDIF
      CALL TEATTR (NOMTE,'C','XLAG',LAG,IBID)
      IF (IBID.EQ.0.AND.LAG.EQ.'ARETE') THEN
        NNO = NNOS
      ENDIF
C     SUPPRESSION DES DDLS DE DEPLACEMENT SEULEMENT POUR LES XHTC
      IF (NFH*NFE.NE.0) THEN
        CALL JEVECH('PSTANO' ,'L',JSTNO)
        CALL XTEDDL(NDIM,NFH,NFE,DDLS,NDDL,NNO,NNOS,ZI(JSTNO),
     &              .FALSE.,.TRUE.,OPTION,NOMTE,ZR(IMATT),RBID,DDLM,
     &              NFISS,JFISNO)
      ENDIF
C     SUPPRESSION DES DDLS DE CONTACT
      IF (MALIN.AND.NLACT.LT.NNO) THEN
        CALL XTEDDL(NDIM,NFH,NFE,DDLS,NDDL,NNO,NNOS,LACT,
     &              .TRUE.,.TRUE.,OPTION,NOMTE,ZR(IMATT),RBID,DDLM,
     &              NFISS,JFISNO)
      ENDIF

      CALL JEDEMA()
      END

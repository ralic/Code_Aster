      SUBROUTINE TE0534(OPTION,NOMTE)
      IMPLICIT   NONE
      CHARACTER*16 OPTION,NOMTE

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/04/2010   AUTEUR PELLET J.PELLET 
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

C.......................................................................
C
C               CALCUL DES SECONDS MEMBRES DE CONTACT FROTTEMENT
C                   POUR X-FEM  (METHODE CONTINUE)
C
C
C  OPTION : 'CHAR_MECA_CONT' (CALCUL DU SECOND MEMBRE DE CONTACT)
C  OPTION : 'CHAR_MECA_FROT' (CALCUL DU SECOND MEMBRE DE FROTTEMENT)
C
C  ENTREES  ---> OPTION : OPTION DE CALCUL
C           ---> NOMTE  : NOM DU TYPE ELEMENT
C
C.......................................................................
C TOLE CRP_20
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

      INTEGER     I,J,K,L,IJ,IFA,IPGF,INO,ISSPG,NI,NJ,NLI,PLI,PLJ
      INTEGER     JINDCO,JDONCO,JLST,IPOIDS,IVF,IDFDE,JGANO,IGEOM,JSEUIL
      INTEGER     IDEPM,IDEPL,IMATT,JSTANO,JPTINT,JAINT,JCFACE,JLONCH
      INTEGER     IPOIDF,IVFF,IDFDEF,IADZI,IAZK24,IBID,IVECT,JBASEC
      INTEGER     NDIM,DDLH,DDLC,DDLS,NDDL,NNO,NNOS,NNOM,NNOF
      INTEGER     NPG,NPGF,XOULA,IN(3),FAC(6,4),NBF
      INTEGER     INDCO(60),NINTER,NFACE,CFACE(5,3),IBID2(12,3),CPT
      INTEGER     INTEG,NFE,SINGU,JSTNO,NVIT
      INTEGER     NNOL,PLA(27),LACT(8),NLACT
      CHARACTER*8 ELREF,TYPMA,FPG,ELC,LAG
      REAL*8      HE,SIGN,VTMP(400),REAC,REAC12(3),FFI,LAMBDA,JAC,JACN
      REAL*8      ND(3),DN,SAUT(3),FFP(27),FFC(8),PTPB(3),PADIST
      REAL*8      METR(2),AL,RHON,MU,RHOTK,P(3,3),SEUIL(60),FFN(27)
      REAL*8      NDN(3,6),TAU1(3,6),TAU2(3,6),PB(3),RPB(3)
      REAL*8      RBID1(3,3),RBID2(3,3),RBID3(3,3),NBARY(3),DDOT
      REAL*8      LST,R,RR,E,G(3),TT(3),RBID
      LOGICAL     LBID,ADHER
      REAL *8     CSTACO,CSTAFR,CPENCO,CPENFR,VITANG(3),X(4)
      REAL *8     VTANG2(3),NVTNG2
      INTEGER     ZXAIN,XXMMVD
      LOGICAL     LPENAF,MALIN,LPENAC
C.......................................................................

      CALL JEMARQ()
C
C-----------------------------------------------------------------------
C     INITIALISATIONS
C-----------------------------------------------------------------------
      ZXAIN=XXMMVD('ZXAIN')
      CALL ELREF1(ELREF)
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
C
C     INITIALISATION DES DIMENSIONS DES DDLS X-FEM
      CALL XTEINI(NOMTE,DDLH,NFE,SINGU,DDLC,NNOM,DDLS,NDDL)
C
C
      CALL TECAEL(IADZI,IAZK24)
      TYPMA=ZK24(IAZK24-1+3+ZI(IADZI-1+2)+3)

      IF (NDIM .EQ. 3) THEN
         CALL CONFAC(TYPMA,IBID2,IBID,FAC,NBF)
      ENDIF
C
C     INITIALISATION DU VECTEUR DE TRAVAIL
      DO 40 J=1,NDDL
        VTMP(J)=0.D0
40    CONTINUE
C
C-----------------------------------------------------------------------
C     RECUPERATION DES ENTRÉES / SORTIE
C-----------------------------------------------------------------------
      CALL JEVECH('PGEOMER','E',IGEOM)
C     DEPLACEMENT A L'EQUILIBRE PRECEDENT  (DEPMOI)       : 'PDEPL_M'
      CALL JEVECH('PDEPL_M','L',IDEPM)
C     INCREMENT DE DEP DEPUIS L'EQUILIBRE PRECEDENT (DEPDEL) : 'PDEPL_P'
      CALL JEVECH('PDEPL_P','L',IDEPL)
      CALL JEVECH('PINDCOI','L',JINDCO)
      CALL JEVECH('PDONCO','L',JDONCO)
      CALL JEVECH('PSEUIL','L',JSEUIL)
      CALL JEVECH('PLST','L',JLST)
      CALL JEVECH('PPINTER','L',JPTINT)
      CALL JEVECH('PAINTER','L',JAINT)
      CALL JEVECH('PCFACE','L',JCFACE)
      CALL JEVECH('PLONCHA','L',JLONCH)
      CALL JEVECH('PBASECO','L',JBASEC)

      CALL JEVECH('PVECTUR','E',IVECT)

C     RÉCUPÉRATIONS DES DONNÉES SUR LE CONTACT ET
C     SUR LA TOPOLOGIE DES FACETTES

      NINTER=ZI(JLONCH-1+1)
      NFACE=ZI(JLONCH-1+2)

      DO 10 I=1,60
        INDCO(I) = ZI(JINDCO-1+I)
        SEUIL(I) = ZR(JSEUIL-1+I)
 10   CONTINUE
      RHON = ZR(JDONCO-1+1)
      MU = ZR(JDONCO-1+2)
      RHOTK = ZR(JDONCO-1+3)

      INTEG = NINT(ZR(JDONCO-1+4))
C     SCHEMA D'INTEGRATION NUMERIQUE ET ELEMENT DE REFERENCE DE CONTACT
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
        ELC='SE2'
      ENDIF
      CALL ELREF4(ELC,FPG,IBID,NNOF,IBID,NPGF,IPOIDF,IVFF,IDFDEF,IBID)
C
      DO 11 I=1,NFACE
        DO 12 J=1,NDIM
          CFACE(I,J)=ZI(JCFACE-1+NDIM*(I-1)+J)
 12     CONTINUE
 11   CONTINUE

C     RECUPERATION DU COEFFICIENT DE MISE À L'ECHELLE DES PRESSIONS
      E=ZR(JDONCO-1+5)

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
        LPENAC=.TRUE.
      ENDIF
C     PENALISATION DU FROTTEMENT
      LPENAF=.FALSE.
      IF (CSTAFR.EQ.0.D0) THEN
        LPENAF=.TRUE.
      ENDIF

C     RÉCUPÉRATION DE LA BASE COVARIANTE AUX POINTS D'INTERSECTION
      DO 13 NLI=1,NINTER
        DO 14 J=1,NDIM
          NDN(J,NLI)  =ZR(JBASEC-1+NDIM*NDIM*(NLI-1)+J)
          TAU1(J,NLI)=ZR(JBASEC-1+NDIM*NDIM*(NLI-1)+J+NDIM)
          IF (NDIM .EQ. 3)
     &      TAU2(J,NLI)=ZR(JBASEC-1+NDIM*NDIM*(NLI-1)+J+2*NDIM)
 14     CONTINUE
 13   CONTINUE

C
C     L'ELEMENT EST-IL LINEAIRE OU QUADRATIQUE
C
      CALL TEATTR (NOMTE,'C','XLAG',LAG,IBID)
      IF (IBID.EQ.0.AND.LAG.EQ.'NOEUD') THEN
        MALIN=.TRUE.
      ELSE
        MALIN=.FALSE.
      ENDIF
C
C     LISTE DES LAMBDAS ACTIFS
C
      IF (MALIN) THEN
        CALL XLACTI(TYPMA,NINTER,JAINT,LACT,NLACT)
      ENDIF
C
C-----------------------------------------------------------------------
C
C     BOUCLE SUR LES FACETTES
      DO 100 IFA=1,NFACE
C       NOMBRE DE LAMBDAS ET LEUR PLACE DANS LA MATRICE
        IF (MALIN) THEN
          NNOL=NNO
          DO 15 I=1,NNOL
            CALL XPLMAT(NDIM,DDLH,NFE,DDLC,NNO,NNOM,I,PLI)
            PLA(I)=PLI
 15       CONTINUE
        ELSE
          NNOL=NNOF
          DO 16 I=1,NNOF
C           XOULA  : RENVOIE LE NUMERO DU NOEUD PORTANT CE LAMBDA
            NI=XOULA(CFACE,IFA,I,JAINT,TYPMA)
C           PLACE DU LAMBDA DANS LA MATRICE
            CALL XPLMAT(NDIM,DDLH,NFE,DDLC,NNO,NNOM,NI,PLI)
            PLA(I)=PLI
 16       CONTINUE
        ENDIF

C       BOUCLE SUR LES POINTS DE GAUSS DES FACETTES
        DO 110 IPGF=1,NPGF
C
C         INDICE DE CE POINT DE GAUSS DANS INDCO
          ISSPG=NPGF*(IFA-1)+IPGF

C         CALCUL DE JAC (PRODUIT DU JACOBIEN ET DU POIDS)
C         ET DES FF DE L'ÉLÉMENT PARENT AU POINT DE GAUSS
C         ET LA NORMALE ND ORIENTÉE DE ESCL -> MAIT
          IF (NDIM .EQ. 3) THEN
            CALL XJACFF(ELREF,FPG,JPTINT,IFA,CFACE,IPGF,NNO,IGEOM,G,
     &                                         'NON',JAC,FFP,RBID,ND)
          ELSEIF (NDIM.EQ.2) THEN
            CALL XJACF2(ELREF,FPG,JPTINT,IFA,CFACE,IPGF,NNO,IGEOM,G,
     &                                         'NON',JAC,FFP,RBID,ND)
          ENDIF
C        CALCUL DES FONCTIONS DE FORMES DE CONTACT DANS LE CAS LINEAIRE
          IF (MALIN) THEN
            CALL XMOFFC(LACT,NLACT,NNO,FFP,FFC)
          ENDIF
C
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
C         SI OUI, L'ARETE EST-ELLE VITALE?
          IF (K.NE.0) THEN
            NVIT = ZR(JAINT-1+ZXAIN*(K-1)+5)
          ELSE
            NVIT = 0
          ENDIF
C         IL NE FAUT PAS UTILISER NVIT SI LE SCHEMA D'INTEGRATION
C         NE CONTIENT PAS DE NOEUDS
          IF ((FPG(1:3).EQ.'FPG').OR.(FPG.EQ.'GAUSS')
     &             .OR.(FPG.EQ.'XCON')) NVIT=1
C
C         RÉACTION CONTACT = SOMME DES FF(I).LAMBDA(I) POUR I=1,NNOL
C         RQ : LA VALEUR DANS IDEPPL EST LA PRESSION DIVISÉE PAR E
C         RÉACTION FROTT = SOMME DES FF(I).(LAMB1(I).TAU1+LAMB2(I).TAU2)
C        (DEPDEL+DEPMOI)
          REAC=0.D0
          CALL VECINI(NDIM,0.D0,REAC12)
          DO 120 I = 1,NNOL
            PLI=PLA(I)
            IF (MALIN) THEN
              FFI=FFC(I)
              NLI=LACT(I)
              IF (NLI.EQ.0) GOTO 120
            ELSE
              FFI=ZR(IVFF-1+NNOF*(IPGF-1)+I)
              NLI=CFACE(IFA,I)
            ENDIF
            REAC = REAC + FFI * (ZR(IDEPL-1+PLI)+ZR(IDEPM-1+PLI)) * E
            DO 121 J=1,NDIM
              IF (NDIM .EQ.3) THEN
                REAC12(J)=REAC12(J)+FFI*(ZR(IDEPL-1+PLI+1)*TAU1(J,NLI)
     &                                  +ZR(IDEPM-1+PLI+1)*TAU1(J,NLI)
     &                                  +ZR(IDEPM-1+PLI+2)*TAU2(J,NLI)
     &                                  +ZR(IDEPL-1+PLI+2)*TAU2(J,NLI))
              ELSEIF (NDIM.EQ.2) THEN
                REAC12(J)=REAC12(J)+FFI*(ZR(IDEPL-1+PLI+1)*TAU1(J,NLI)
     &                                  +ZR(IDEPM-1+PLI+1)*TAU1(J,NLI))
              ENDIF
 121        CONTINUE
 120      CONTINUE

C         NORMALE AU CENTRE DE LA FACETTE
          CALL VECINI(NDIM,0.D0,NBARY)
          DO 122 I=1,NNOF
            NBARY(1)=NBARY(1)+NDN(1,CFACE(IFA,I))/NNOF
            NBARY(2)=NBARY(2)+NDN(2,CFACE(IFA,I))/NNOF
            IF (NDIM .EQ. 3)
     &        NBARY(3)=NBARY(3)+NDN(3,CFACE(IFA,I))/NNOF
 122      CONTINUE

C         CALCUL DE RR = SQRT(DISTANCE AU FOND DE FISSURE)
          IF (SINGU.EQ.1) THEN
            LST=0.D0
            DO 112 I=1,NNO
              LST=LST+ZR(JLST-1+I)*FFP(I)
 112        CONTINUE
            R=ABS(LST)
            RR=SQRT(R)
          ENDIF

C         I) CALCUL DES SECONDS MEMBRES DE CONTACT
C         ..............................

          IF (OPTION.EQ.'CHAR_MECA_CONT') THEN


C           SI PAS DE CONTACT POUR CE PG : ON REMPLIT LE VECTEUR LN2
            IF (INDCO(ISSPG).EQ.0) THEN
C
              IF (NVIT.NE.0) THEN

                DO 130 I = 1,NNOL

                  PLI=PLA(I)
                  IF (MALIN) THEN
                    FFI=FFC(I)
                  ELSE
                    FFI=ZR(IVFF-1+NNOF*(IPGF-1)+I)
                  ENDIF

                  VTMP(PLI) = VTMP(PLI) - REAC*FFI*JAC/CSTACO*E
 130            CONTINUE

              ENDIF
C
C           SI CONTACT POUR CE PG : ON REMPLIT LES VECTEURS LN1 ET LN2
            ELSE IF (INDCO(ISSPG).EQ.1) THEN

C
C             CALCUL DU SAUT ET DE DN EN CE PG (DEPMOI + DEPDEL)
              CALL VECINI(NDIM,0.D0,SAUT)
              DO 140 I = 1,NNO
                DO 141 J = 1,DDLH
                  SAUT(J) = SAUT(J) - 2.D0 * FFP(I) *
     &                             (   ZR(IDEPM-1+DDLS*(I-1)+NDIM+J)
     &                               + ZR(IDEPL-1+DDLS*(I-1)+NDIM+J) )
 141            CONTINUE
                DO 142 J = 1,SINGU*NDIM
                  SAUT(J) = SAUT(J) - 2.D0 * FFP(I) * RR *
     &                          (   ZR(IDEPM-1+DDLS*(I-1)+NDIM+DDLH+J)
     &                            + ZR(IDEPL-1+DDLS*(I-1)+NDIM+DDLH+J) )
 142            CONTINUE
 140          CONTINUE
              DN = 0.D0
              DO 143 J = 1,NDIM
                DN = DN + SAUT(J)*ND(J)
 143          CONTINUE

C
C             TERME LN1
              IF (LPENAC) THEN
C               PENALISATION DU CONTACT
                DO 153 I = 1,NNO
                  DO 154 J = 1,DDLH
                    VTMP(DDLS*(I-1)+NDIM+J) =
     &              VTMP(DDLS*(I-1)+NDIM+J) -
     &              (CPENCO*DN)*2.D0*FFP(I)*ND(J)*JAC
 154              CONTINUE
                  DO 155 J = 1,SINGU*NDIM
                    VTMP(DDLS*(I-1)+NDIM+DDLH+J) =
     &              VTMP(DDLS*(I-1)+NDIM+DDLH+J) -
     &              (CPENCO*DN)*2.D0*FFP(I)*RR*ND(J)*JAC
 155              CONTINUE
 153            CONTINUE
              ELSE
                DO 150 I = 1,NNO
                  DO 151 J = 1,DDLH
                    VTMP(DDLS*(I-1)+NDIM+J) =
     &              VTMP(DDLS*(I-1)+NDIM+J) +
     &              (REAC-CPENCO*DN)*2.D0*FFP(I)*ND(J)*JAC
 151              CONTINUE
                  DO 152 J = 1,SINGU*NDIM
                    VTMP(DDLS*(I-1)+NDIM+DDLH+J) =
     &              VTMP(DDLS*(I-1)+NDIM+DDLH+J) +
     &              (REAC-CPENCO*DN)*2.D0*FFP(I)*RR*ND(J)*JAC
 152              CONTINUE
 150            CONTINUE
              ENDIF
C
C             TERME LN2
              DO 160 I = 1,NNOL

                PLI=PLA(I)
                IF (MALIN) THEN
                  FFI=FFC(I)
                ELSE
                  FFI=ZR(IVFF-1+NNOF*(IPGF-1)+I)
                ENDIF

                VTMP(PLI) = VTMP(PLI) - DN * FFI * JAC * E
                IF (LPENAC) THEN
                  VTMP(PLI) = VTMP(PLI) - REAC*FFI*JAC/CPENCO*E
                ENDIF
 160          CONTINUE

            ELSE
C             SI INDCO N'EST NI ÉGAL À 0 NI ÉGAL À 1
C             PROBLEME DE STATUT DE CONTACT.
              CALL ASSERT(INDCO(ISSPG).EQ.0 .OR. INDCO(ISSPG).EQ.1)
            END IF
C

C         II) CALCUL DES SECONDS MEMBRES DE FROTTEMENT
C         ..............................

          ELSEIF (OPTION.EQ.'CHAR_MECA_FROT') THEN

            IF (MU.EQ.0.D0.OR.SEUIL(ISSPG).EQ.0.D0) INDCO(ISSPG) = 0

C           SI PAS DE CONTACT POUR CE PG : ON REMPLIT QUE LN3
            IF (INDCO(ISSPG).EQ.0) THEN
              IF (NVIT.NE.0) THEN

                CALL VECINI(NDIM,0.D0,SAUT)
                DO 161 I = 1,NNO
                  DO 162 J = 1,DDLH
                    SAUT(J) = SAUT(J) - 2.D0 * FFP(I) *
     &                       (   ZR(IDEPM-1+DDLS*(I-1)+NDIM+J)
     &                       + ZR(IDEPL-1+DDLS*(I-1)+NDIM+J) )
 162              CONTINUE
                  DO 163 J = 1,SINGU*NDIM
                    SAUT(J) = SAUT(J) - 2.D0 * FFP(I) * RR *
     &                       (   ZR(IDEPM-1+DDLS*(I-1)+NDIM+DDLH+J)
     &                       + ZR(IDEPL-1+DDLS*(I-1)+NDIM+DDLH+J) )
 163              CONTINUE
 161            CONTINUE

                CALL VECINI(NDIM-1,0.D0,TT)
                DO 165 I = 1,NNOL
                  PLI=PLA(I)
                  IF (MALIN) THEN
                    FFI=FFC(I)
                    NLI=LACT(I)
                    IF (NLI.EQ.0) GOTO 165
                  ELSE
                    FFI=ZR(IVFF-1+NNOF*(IPGF-1)+I)
                    NLI=CFACE(IFA,I)
                  ENDIF

                  METR(1)=DDOT(NDIM,TAU1(1,NLI),1,SAUT,1)
                  IF (NDIM.EQ.3) METR(2)=DDOT(NDIM,TAU2(1,NLI),1,
     &                                        SAUT,1)
                  TT(1)=DDOT(NDIM,TAU1(1,NLI),1,REAC12,1)
                  IF (NDIM .EQ.3) TT(2)=DDOT(NDIM,TAU2(1,NLI),1,
     &                                       REAC12,1)
                  DO 167 K=1,NDIM-1
                    VTMP(PLI+K) = VTMP(PLI+K) + TT(K)*FFI*JAC
 167              CONTINUE
 165            CONTINUE

              ENDIF

C           SI CONTACT POUR CE PG : ON REMPLIT LN1 ET LN3
            ELSE IF (INDCO(ISSPG).EQ.1) THEN
C             P : OPÉRATEUR DE PROJECTION
              CALL XMAFR1(NDIM,ND,P)

C             PBOUL SELON L'ÉTAT D'ADHERENCE DU PG (AVEC DEPDEL)
              CALL VECINI(NDIM,0.D0,SAUT)
              DO 175 INO=1,NNO
                DO 176 J=1,DDLH
                  SAUT(J) = SAUT(J) - 2.D0 * FFP(INO) *
     &                                ZR(IDEPL-1+DDLS*(INO-1)+NDIM+J)
 176            CONTINUE
                DO 177 J = 1,SINGU*NDIM
                  SAUT(J) = SAUT(J) - 2.D0 * FFP(INO) * RR *
     &                              ZR(IDEPL-1+DDLS*(INO-1)+NDIM+DDLH+J)

 177            CONTINUE
 175          CONTINUE

              CALL XADHER(P,SAUT,REAC12,RHOTK,CSTAFR,CPENFR,LPENAF,
     &                 VITANG,PB,RBID1,RBID2,RBID3,ADHER)
C             TERME LN1

              IF (ADHER) THEN
C               CALCUL DE PT.REAC12
                DO 188 I=1,NDIM
                  PTPB(I)=0.D0
                  IF (LPENAF) THEN
                    DO 190 K=1,NDIM
                      PTPB(I)=PTPB(I)+P(K,I)*CPENFR*VITANG(K)
 190                CONTINUE
                  ELSE
                    DO 189 K=1,NDIM
                      PTPB(I)=PTPB(I)+P(K,I)*(REAC12(K)
     &                         +CPENFR*VITANG(K))
 189                CONTINUE
                  ENDIF
 188            CONTINUE
              ELSE
C               CALCUL DE PT.PBOUL
                DO 182 I=1,NDIM
                  PTPB(I)=0.D0
                  DO 183 K=1,NDIM
                    PTPB(I)=PTPB(I) + P(K,I)*PB(K)
 183              CONTINUE
 182            CONTINUE
              ENDIF

              DO 185 I = 1,NNO
                DO 186 J = 1,DDLH
                  VTMP(DDLS*(I-1)+NDIM+J) =
     &            VTMP(DDLS*(I-1)+NDIM+J) +
     &            2.D0*MU*SEUIL(ISSPG)* PTPB(J)*FFP(I)*JAC
 186            CONTINUE
                DO 187 J = 1,SINGU*NDIM
                  VTMP(DDLS*(I-1)+NDIM+DDLH+J) =
     &            VTMP(DDLS*(I-1)+NDIM+DDLH+J) +
     &            2.D0*RR*MU*SEUIL(ISSPG)* PTPB(J)*FFP(I)*JAC
 187            CONTINUE
 185          CONTINUE

C             TERME LN3

C             CALCUL DE REAC12-PBOUL
              IF (LPENAF) CSTAFR=CPENFR
              DO 180 I=1,NDIM
                RPB(I)=REAC12(I)-PB(I)
 180          CONTINUE
  
              DO 194 I = 1,NNOL
                PLI=PLA(I)
                IF (MALIN) THEN
                  FFI=FFC(I)
                  NLI=LACT(I)
                  IF (NLI.EQ.0) GOTO 194
                ELSE
                  FFI=ZR(IVFF-1+NNOF*(IPGF-1)+I)
                  NLI=CFACE(IFA,I)
                ENDIF

                METR(1)=DDOT(NDIM,TAU1(1,NLI),1,RPB,1)
                IF(NDIM.EQ.3)
     &            METR(2)=DDOT(NDIM,TAU2(1,NLI),1,RPB,1)
                DO 195 K=1,NDIM-1
                  VTMP(PLI+K) = VTMP(PLI+K)
     &                  + MU*SEUIL(ISSPG)/CSTAFR * METR(K)*FFI*JAC

 195            CONTINUE
 194          CONTINUE

            ELSE
C             SI INDCO N'EST NI ÉGAL À 0 NI ÉGAL À 1
C             PROBLEME DE STATUT DE CONTACT.
              CALL ASSERT(INDCO(ISSPG).EQ.0 .OR. INDCO(ISSPG).EQ.1)
            END IF
          ELSE
C           SI OPTION NI 'CHAR_MECA_CONT' NI 'CHAR_MECA_FROT'
            CALL ASSERT(OPTION.EQ.'CHAR_MECA_FROT' .OR.
     &                  OPTION.EQ.'CHAR_MECA_CONT')
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
      DO 900 I=1,NDDL
        ZR(IVECT-1+I)=VTMP(I)
 900  CONTINUE

      CALL TEATTR (NOMTE,'C','XLAG',LAG,IBID)
      IF (IBID.EQ.0.AND.LAG.EQ.'ARETE') THEN
        NNO = NNOS
      ENDIF
C     SUPPRESSION DES DDLS DE DEPLACEMENT SEULEMENT POUR LES XHTC
      IF (DDLH*NFE.NE.0) THEN
        CALL JEVECH('PSTANO' ,'L',JSTNO)
        CALL XTEDDL(NDIM,DDLH,NFE,DDLS,NDDL,NNOS,ZI(JSTNO),
     &               .FALSE.,LBID,OPTION,NOMTE,RBID,ZR(IVECT))
      ENDIF
C     SUPPRESSION DES DDLS DE CONTACT
      IF (MALIN.AND.NLACT.LT.NNO) THEN
        CALL XTEDDL(NDIM,DDLH,NFE,DDLS,NDDL,NNOS,LACT,
     &              .TRUE.,.TRUE.,OPTION,NOMTE,RBID,ZR(IVECT))
      ENDIF
      CALL JEDEMA()
      END

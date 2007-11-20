      SUBROUTINE TE0534(OPTION,NOMTE)
      IMPLICIT   NONE
      CHARACTER*16 OPTION,NOMTE

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 19/11/2007   AUTEUR GENIAUT S.GENIAUT 
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

      INTEGER     I,J,K,L,IJ,IFA,IPGF,INO,ISSPG,NI,NJ,NLI,NLJ,PLI,PLJ
      INTEGER     JINDCO,JDONCO,JLST,IPOIDS,IVF,IDFDE,JGANO,IGEOM,JSEUIL
      INTEGER     IDEPM,IDEPL,IMATT,JSTANO,JPTINT,JAINT,JCFACE,JLONCH
      INTEGER     IPOIDF,IVFF,IDFDEF,IADZI,IAZK24,IBID,IVECT,JBASEC
      INTEGER     NDIM,DDLH,DDLC,DDLS,NDDL,NNO,NNOS,NNOM,NNOF
      INTEGER     NPG,NPGF,AR(12,2),NBAR,XOULA,IN(3),FAC(6,4),NBF
      INTEGER     INDCO(60),NINTER,NFACE,CFACE(5,3),IBID2(12,3),CPT
      INTEGER     INTEG,NFE,SINGU
      CHARACTER*8 ELREF,TYPMA,FPG,ELC
      REAL*8      HE,SIGN,VTMP(400),REAC,REAC12(3),FFI,LAMBDA,JAC,JACN
      REAL*8      ND(3),DN,SAUT(3),FFP(27),PTPB(3),PADIST,MULT
      REAL*8      METR(2),AL,RHON,MU,RHOTK,P(3,3),SEUIL(60),FFN(27)
      REAL*8      NDN(3,6),TAU1(3,6),TAU2(3,6),PB(3),RPB(3)
      REAL*8      RBID1(3,3),RBID2(3,3),RBID3(3,3),NBARY(3),DDOT
      REAL*8      LST,R,RR,E,G(3),TT(3),RBID
C.......................................................................

      CALL JEMARQ()
C
C-----------------------------------------------------------------------
C     INITIALISATIONS
C-----------------------------------------------------------------------
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
      ELSE
         CALL CONARE(TYPMA,AR,NBAR)
      ENDIF
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
      IF (NINTER.LT.NDIM) GOTO 9999

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
        IF (INTEG.EQ.1) FPG='XCON'
        IF (INTEG.EQ.4) FPG='FPG4'
        IF (INTEG.EQ.6) FPG='FPG6'
        IF (INTEG.EQ.7) FPG='FPG7'
        ELC='TR3'
      ELSEIF (NDIM.EQ.2) THEN
        FPG='MASS'
        ELC='SE2'
      ENDIF
C
      NFACE=ZI(JLONCH-1+2)
      DO 11 I=1,NFACE
        DO 12 J=1,NDIM
          CFACE(I,J)=ZI(JCFACE-1+NDIM*(I-1)+J)
 12     CONTINUE
 11   CONTINUE

C     RECUPERATION DU COEFFICIENT DE MISE À L'ECHELLE DES PRESSIONS
      E=ZR(JDONCO-1+5)

C     RÉCUPÉRATION DE LA BASE COVARIANTE AUX POINTS D'INTERSECTION
      DO 13 NLI=1,NINTER
        DO 14 J=1,NDIM
          NDN(J,NLI)  =ZR(JBASEC-1+NDIM*NDIM*(NLI-1)+J)
          TAU1(J,NLI)=ZR(JBASEC-1+NDIM*NDIM*(NLI-1)+J+NDIM)
          IF (NDIM .EQ. 3)
     &      TAU2(J,NLI)=ZR(JBASEC-1+NDIM*NDIM*(NLI-1)+J+2*NDIM)
 14     CONTINUE
 13   CONTINUE

C     INITIALISATION DU VECTEUR DE TRAVAIL
C     IL FAUT LE FAIRE APRES AVOIR RÉCUPERER NINTER
C     POUR ETRE SUR QU'ON SOIT BIEN SUR UN ELEMENT LINEIARE
C     CAR POUR LE MOMENT, ON EST AUSSI SUR DES ELEMENTS QUADRATIQUES
C     MAIS NINTER VAUT ALORS -1 ET ON SORT DIRECT
C     SAUF QUE VTMP EST DIMENTIONNE POUR DES ELTS LINEAIRES
      DO 40 J=1,NDDL
        VTMP(J)=0.D0
40    CONTINUE
C
C-----------------------------------------------------------------------
C
C     BOUCLE SUR LES FACETTES

      DO 100 IFA=1,NFACE
C
C       PETIT TRUC EN PLUS POUR LES FACES EN DOUBLE
        MULT=1.D0
        DO 101 I=1,NDIM
          NLI=CFACE(IFA,I)
          IN(I)=NINT(ZR(JAINT-1+4*(NLI-1)+2))
101     CONTINUE
C       SI LES 2/3 SOMMETS DE LA FACETTE SONT DES NOEUDS DE L'ELEMENT
        IF (NDIM .EQ. 3) THEN
          IF (IN(1).NE.0.AND.IN(2).NE.0.AND.IN(3).NE.0) THEN
            DO 102 I=1,NBF
              CPT=0
              DO 103 INO=1,4
                IF (IN(1).EQ.FAC(I,INO).OR.IN(2).EQ.FAC(I,INO).OR.
     &            IN(3).EQ.FAC(I,INO))    CPT=CPT+1
 103          CONTINUE
              IF (CPT.EQ.3) THEN
                 MULT=0.5D0
                 GOTO 104
              ENDIF
 102        CONTINUE
          ENDIF
        ELSEIF (NDIM .EQ. 2) THEN
          IF (IN(1).NE.0.AND.IN(2).NE.0) THEN
            DO 1021 I=1,NBAR
              CPT=0
              DO 1031 INO=1,2
                IF (IN(1).EQ.AR(I,INO).OR.IN(2).EQ.AR(I,INO))
     &          CPT=CPT+1
 1031         CONTINUE
              IF (CPT.EQ.2) THEN
                MULT=0.5D0
                GOTO 104
              ENDIF
 1021       CONTINUE
          ENDIF
        ENDIF
 104    CONTINUE
C
        CALL ELREF4(ELC,FPG,IBID,NNOF,IBID,NPGF,
     &      IPOIDF,IVFF,IDFDEF,IBID)

C       BOUCLE SUR LES POINTS DE GAUSS DES FACETTES
        DO 110 IPGF=1,NPGF
C
C         INDICE DE CE POINT DE GAUSS DANS INDCO
          ISSPG=NPGF*(IFA-1)+IPGF
C
C         RÉACTION CONTACT = SOMME DES FF(I).LAMBDA(I) POUR I=1,NNOF
C         RQ : LA VALEUR DANS IDEPPL EST LA PRESSION DIVISÉE PAR E
C         RÉACTION FROTT = SOMME DES FF(I).(LAMB1(I).TAU1+LAMB2(I).TAU2)
C        (DEPDEL+DEPMOI)
          REAC=0.D0
          CALL LCINVN(NDIM,0.D0,REAC12)
          DO 120 I = 1,NNOF
            FFI=ZR(IVFF-1+NNOF*(IPGF-1)+I)
            NLI=CFACE(IFA,I)
            NI=XOULA(CFACE,IFA,I,JAINT,TYPMA)
            CALL XPLMAT(NDIM,DDLH,NFE,DDLC,NNO,NNOM,NI,PLI)
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

C         NORMALE AU CENTRE DE LA FACETTE
          CALL LCINVN(NDIM,0.D0,NBARY)
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
              DO 130 I = 1,NNOF

                FFI=ZR(IVFF-1+NNOF*(IPGF-1)+I)
                NI=XOULA(CFACE,IFA,I,JAINT,TYPMA)
                CALL XPLMAT(NDIM,DDLH,NFE,DDLC,NNO,NNOM,NI,PLI)

                 VTMP(PLI) = VTMP(PLI) - REAC*FFI*JAC*MULT/RHON*E
 130          CONTINUE
C
C           SI CONTACT POUR CE PG : ON REMPLIT LES VECTEURS LN1 ET LN2
            ELSE IF (INDCO(ISSPG).EQ.1) THEN
C
C             CALCUL DU SAUT ET DE DN EN CE PG (DEPMOI + DEPDEL)
              CALL LCINVN(NDIM,0.D0,SAUT)
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
              DO 150 I = 1,NNO
                DO 151 J = 1,DDLH
                  VTMP(DDLS*(I-1)+NDIM+J) =
     &            VTMP(DDLS*(I-1)+NDIM+J) +
     &            (REAC-RHON*DN)*2.D0*FFP(I)*ND(J)*JAC*MULT
 151            CONTINUE
                DO 152 J = 1,SINGU*NDIM
                  VTMP(DDLS*(I-1)+NDIM+DDLH+J) =
     &            VTMP(DDLS*(I-1)+NDIM+DDLH+J) +
     &            (REAC-RHON*DN)*2.D0*FFP(I)*RR*ND(J)*JAC*MULT
 152            CONTINUE
 150          CONTINUE
C
C             TERME LN2
              DO 160 I = 1,NNOF

                FFI=ZR(IVFF-1+NNOF*(IPGF-1)+I)
                NI=XOULA(CFACE,IFA,I,JAINT,TYPMA)
                CALL XPLMAT(NDIM,DDLH,NFE,DDLC,NNO,NNOM,NI,PLI)

                VTMP(PLI) = VTMP(PLI) - DN * FFI * JAC * MULT * E

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
              CALL LCINVN(NDIM,0.D0,SAUT)
              DO 161 I = 1,NNO
                DO 162 J = 1,DDLH
                  SAUT(J) = SAUT(J) - 2.D0 * FFP(I) *
     &                             (   ZR(IDEPM-1+DDLS*(I-1)+NDIM+J)
     &                               + ZR(IDEPL-1+DDLS*(I-1)+NDIM+J) )
 162            CONTINUE
                DO 163 J = 1,SINGU*NDIM
                  SAUT(J) = SAUT(J) - 2.D0 * FFP(I) * RR *
     &                          (   ZR(IDEPM-1+DDLS*(I-1)+NDIM+DDLH+J)
     &                            + ZR(IDEPL-1+DDLS*(I-1)+NDIM+DDLH+J) )
 163            CONTINUE
 161          CONTINUE
              DN = 0.D0
              DO 164 J = 1,NDIM
                DN = DN + SAUT(J)*ND(J)
 164          CONTINUE

          CALL LCINVN(NDIM-1,0.D0,TT)
          DO 165 I = 1,NNOF
            FFI=ZR(IVFF-1+NNOF*(IPGF-1)+I)
            NLI=CFACE(IFA,I)
            NI=XOULA(CFACE,IFA,I,JAINT,TYPMA)

            CALL XPLMAT(NDIM,DDLH,NFE,DDLC,NNO,NNOM,NI,PLI)

            METR(1)=DDOT(NDIM,TAU1(1,NLI),1,SAUT,1)
            IF (NDIM.EQ.3) METR(2)=DDOT(NDIM,TAU2(1,NLI),1,SAUT,1)
            TT(1)=DDOT(NDIM,TAU1(1,NLI),1,REAC12,1)
            IF (NDIM .EQ.3) TT(2)=DDOT(NDIM,TAU2(1,NLI),1,REAC12,1)
           DO 167 K=1,NDIM-1
               VTMP(PLI+K) = VTMP(PLI+K) + TT(K)*FFI*JAC*MULT
 167       CONTINUE
 165      CONTINUE

C           SI CONTACT POUR CE PG : ON REMPLIT LN1 ET LN3
            ELSE IF (INDCO(ISSPG).EQ.1) THEN

C             P : OPÉRATEUR DE PROJECTION
              CALL XMAFR1(NDIM,ND,P)

C             PBOUL SELON L'ÉTAT D'ADHERENCE DU PG (AVEC DEPDEL)
              CALL LCINVN(NDIM,0.D0,SAUT)
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

              CALL XADHER(P,SAUT,REAC12,RHOTK,PB,RBID1,RBID2,RBID3)

C             CALCUL DE Pt.PBOUL ET REAC12-PBOUL
              DO 182 I=1,NDIM
                PTPB(I)=0.D0
                DO 183 K=1,NDIM
                  PTPB(I)=PTPB(I) + P(K,I)*PB(K)
 183            CONTINUE
                RPB(I)=REAC12(I)-PB(I)
 182          CONTINUE

C             TERME LN1
              DO 185 I = 1,NNO
                DO 186 J = 1,DDLH
                  VTMP(DDLS*(I-1)+NDIM+J) =
     &            VTMP(DDLS*(I-1)+NDIM+J) +
     &            2.D0*MU*SEUIL(ISSPG)* PTPB(J)*FFP(I)*JAC*MULT
 186            CONTINUE
                DO 187 J = 1,SINGU*NDIM
                  VTMP(DDLS*(I-1)+NDIM+DDLH+J) =
     &            VTMP(DDLS*(I-1)+NDIM+DDLH+J) +
     &            2.D0*RR*MU*SEUIL(ISSPG)* PTPB(J)*FFP(I)*JAC*MULT
 187            CONTINUE
 185          CONTINUE

C             TERME LN3
              DO 194 I = 1,NNOF
                FFI=ZR(IVFF-1+NNOF*(IPGF-1)+I)
                NLI=CFACE(IFA,I)
                NI=XOULA(CFACE,IFA,I,JAINT,TYPMA)
                CALL XPLMAT(NDIM,DDLH,NFE,DDLC,NNO,NNOM,NI,PLI)

                METR(1)=DDOT(NDIM,TAU1(1,NLI),1,RPB,1)
                IF(NDIM.EQ.3)
     &            METR(2)=DDOT(NDIM,TAU2(1,NLI),1,RPB,1)
                DO 195 K=1,NDIM-1
                  VTMP(PLI+K) = VTMP(PLI+K)
     &                  + MU*SEUIL(ISSPG)/RHOTK * METR(K)*FFI*JAC*MULT

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

 9999 CONTINUE

      CALL JEDEMA()
      END

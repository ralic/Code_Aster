      SUBROUTINE TE0533(OPTION,NOMTE)
      IMPLICIT   NONE
      CHARACTER*16 OPTION,NOMTE

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 18/04/2005   AUTEUR GENIAUT S.GENIAUT 
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
      INTEGER      IDEPM,IDEPD,IMATT,JSTANO,JPTINT,JAINT,JCFACE,JLONCH
      INTEGER      IPOIDF,IVFF,IDFDEF,IADZI,IAZK24,IBID,JBASEC,JSEUIL
      INTEGER      NDIM,DDLH,DDLC,DDLE,DDLS,NDDL,NNO,NNOS,NNOM,NNOF
      INTEGER      NPG,NPGF,AR(12,2),NBAR,XOULA,IN(3),FAC(6,4),NBF
      INTEGER      INDCO(60),NINTER,NFACE,CFACE(5,3),IBID2(12,3),CPT

      CHARACTER*8  ELREF,TYPMA
      REAL*8       LSN(27),HE,SIGN,XG,FFI,FFJ,FFP(27),SAUT(3),KNP(3,3)
      REAL*8       MMAT(27*7,27*7),JAC,AL,RHON,MU,RHOTK,PADIST,MULT
      REAL*8       NDN(3,6),TAU1(3,6),TAU2(3,6),LAMB1(3),LAMB2(3)
      REAL*8       ND(3),METR(2,2),P(3,3),KN(3,3),R3(3),SEUIL(60)
      REAL*8       PTKNP(3,3),TAUKNP(2,3),TAIKTA(2,2),IK(3,3),NBARY(3)

C......................................................................

      CALL JEMARQ()
C
C-----------------------------------------------------------------------
C     INITIALISATIONS
C-----------------------------------------------------------------------



      CALL ELREF1(ELREF)
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
C
C     DDL PAR NOEUD SOMMET : HEAVISIDE, ENRICHIS (FOND), CONTACT
      DDLH=3
      DDLE=0
      DDLC=3
C     NB DE 'NOEUDS MILIEU' SERVANT À PORTER DES DDL DE CONTACT
      NNOM=3*(NNO/2)

C     NOMBRE DE DDL À CHAQUE NOEUD SOMMET 
      DDLS=NDIM+DDLH+DDLE+DDLC
C
C     NOMBRE DE DDL TOTAL DE L'ÉLÉMENT
      NDDL=(NNO*DDLS)+(NNOM*DDLC)

C     INITIALISATION DE LA MATRICE
      CALL MATINI(27*7,27*7,0.D0,MMAT)
C
      CALL TECAEL(IADZI,IAZK24)
      TYPMA=ZK24(IAZK24-1+3+ZI(IADZI-1+2)+3)
      CALL CONARE(TYPMA,AR,NBAR)
      CALL CONFAC(TYPMA,IBID2,IBID,FAC,NBF)
C
C-----------------------------------------------------------------------
C     RECUPERATION DES ENTRÉES / SORTIE
C-----------------------------------------------------------------------
      CALL JEVECH('PGEOMER','E',IGEOM)
C     DEPMOI
      CALL JEVECH('PDEPL_M','L',IDEPM)
C     DEPDEL
      CALL JEVECH('PDEPL_P','L',IDEPD)
      CALL JEVECH('PINDCOI','L',JINDCO)
      CALL JEVECH('PDONCO','L',JDONCO)
      CALL JEVECH('PSEUIL','L',JSEUIL)
      CALL JEVECH('PLEVSET','L',JLSN)
      CALL JEVECH('PSTANO','L',JSTANO)
      CALL JEVECH('PPINTER','L',JPTINT)
      CALL JEVECH('PAINTER','L',JAINT)
      CALL JEVECH('PCFACE','L',JCFACE)
      CALL JEVECH('PLONCHA','L',JLONCH)
      CALL JEVECH('PBASECO','L',JBASEC)

      CALL JEVECH('PMATUUR','E',IMATT)

C     RÉCUPÉRATIONS DES DONNÉES SUR LE CONTACT ET
C     SUR LA TOPOLOGIE DES FACETTES
      DO 10 I=1,60
        INDCO(I) = ZI(JINDCO-1+I)
        SEUIL(I) = ZR(JSEUIL-1+I)        
 10   CONTINUE
 
      RHON = ZR(JDONCO-1+1)
      MU = ZR(JDONCO-1+2)
      RHOTK = ZR(JDONCO-1+3)

      NINTER=ZI(JLONCH-1+1)
      IF (NINTER.LT.3) GOTO 9999 
      NFACE=ZI(JLONCH-1+2)
      DO 11 I=1,NFACE
        DO 12 J=1,3
          CFACE(I,J)=ZI(JCFACE-1+3*(I-1)+J)
 12      CONTINUE
 11   CONTINUE
C

      IF (OPTION.EQ.'RIGI_FROT') WRITE(6,*)'SEUIL ',ZR(JSEUIL)

C     RÉCUPÉRATION DE LA BASE COVARIANTE AUX POINTS D'INTERSECTION
      DO 13 NLI=1,NINTER
        DO 14 J=1,3
          NDN(J,NLI)  =ZR(JBASEC-1+9*(NLI-1)+J)  
          TAU1(J,NLI)=ZR(JBASEC-1+9*(NLI-1)+J+3)
          TAU2(J,NLI)=ZR(JBASEC-1+9*(NLI-1)+J+6)
 14     CONTINUE
C        write(6,*)'tau2',TAU2(1,NLI),TAU2(2,NLI),TAU2(3,NLI)
C           CALL ASSERT(ABS(NDN(1,NLI)).LE.1.D-12)
C           CALL ASSERT(ABS(NDN(2,NLI)).LE.1.D-12)           
C           CALL ASSERT(ABS(NDN(3,NLI)-1.D0).LE.1.D-12)           
           
 13   CONTINUE
C    
C     REACTUALISATION DE LA GEOMETRIE AVEC DEPMOI
      DO 20 I = 1,NNO       
        HE=SIGN(1.D0,ZR(JLSN-1+I))
        DO 30 J = 1,NDIM
          ZR(IGEOM-1+(I-1)*NDIM+J) = ZR(IGEOM-1+(I-1)*NDIM+J) +
     &    ZR(IDEPM-1+(I-1)*DDLS+J) + HE * ZR(IDEPM-1+(I-1)*DDLS+J+3)
 30     CONTINUE
 20   CONTINUE
C
C-----------------------------------------------------------------------
C
C     BOUCLE SUR LES FACETTES
      DO 100 IFA=1,NFACE
C
C       PETIT TRUC EN PLUS POUR LES FACES EN DOUBLE
        MULT=1.D0
        DO 101 I=1,3
          NLI=CFACE(IFA,I)
          IN(I)=NINT(ZR(JAINT-1+4*(NLI-1)+2))
101     CONTINUE
C       SI LES 3 SOMMETS DE LA FACETTE SONT DES NOEUDS DE L'ÉLÉMENT
        IF (IN(1).NE.0.AND.IN(2).NE.0.AND.IN(3).NE.0) THEN
          DO 102 I=1,NBF
            CPT=0
            DO 103 INO=1,4
              IF (IN(1).EQ.FAC(I,INO).OR.IN(2).EQ.FAC(I,INO).OR.
     &            IN(3).EQ.FAC(I,INO))    CPT=CPT+1     
 103        CONTINUE
            IF (CPT.EQ.3) THEN
              MULT=0.5D0

              GOTO 104
            ENDIF  
 102      CONTINUE            
        ENDIF
 104    CONTINUE
C
C       LA FAMILLE 'XCON' A 12 PG INTEGRE ORDRE I+J=6
        CALL ELREF4('TR3','XCON',IBID,NNOF,IBID,NPGF,IPOIDF,IVFF,
     &                                                     IDFDEF,IBID)
C
C       BOUCLE SUR LES POINTS DE GAUSS DES FACETTES
        DO 110 IPGF=1,NPGF
C
C         INDICE DE CE POINT DE GAUSS DANS INDCO
          ISSPG=NPGF*(IFA-1)+IPGF
C
C         CALCUL DE JAC (PRODUIT DU JACOBIEN ET DU POIDS)        
C         ET DES FF DE L'ÉLÉMENT PARENT AU POINT DE GAUSS
C         ET LA NORMALE ND ORIENTÉE DE ESCL -> MAIT
          CALL XJACFF(ELREF,JPTINT,IFA,CFACE,IPGF,NNO,IGEOM,JAC,FFP,ND)

C         NORMALE AU CENTRE DE LA FACETTE
          CALL LCINVN(NDIM,0.D0,NBARY)
          DO 122 I=1,NNOF
            NBARY(1)=NBARY(1)+NDN(1,CFACE(IFA,I))/3.D0
            NBARY(2)=NBARY(2)+NDN(2,CFACE(IFA,I))/3.D0
            NBARY(3)=NBARY(3)+NDN(3,CFACE(IFA,I))/3.D0
 122      CONTINUE          

C          IF (PADIST(3,ND,NBARY).GT.1.D-12) CALL UTMESS('F','TE0533',
C     &                       'PB DE NORMALE. FISSURE PLANE ?') 
 
C         I) CALCUL DES MATRICES DE CONTACT
C         ..............................

          IF (OPTION.EQ.'RIGI_CONT') THEN
C
C           SI PAS DE CONTACT POUR CE PG : ON REMPLIT LA MATRICE C 
            IF (INDCO(ISSPG).EQ.0) THEN
C
              DO 120 I = 1,NNOF

                FFI=ZR(IVFF-1+NNOF*(IPGF-1)+I)
C               XOULA  : RENVOIE LE NUMÉRO DU NOEUD PORTANT CE LAMBDA
                NI=XOULA(CFACE,IFA,I,JAINT,TYPMA)
                CALL XPLMAT(NDIM,DDLH,DDLE,DDLC,NNO,NNOM,NI,PLI)

                DO 121 J = 1,NNOF

                  FFJ=ZR(IVFF-1+NNOF*(IPGF-1)+J)
                  NJ=XOULA(CFACE,IFA,J,JAINT,TYPMA)
                  CALL XPLMAT(NDIM,DDLH,DDLE,DDLC,NNO,NNOM,NJ,PLJ)
C
                  MMAT(PLI,PLJ) = MMAT(PLI,PLJ)
     &                     - FFJ * FFI * JAC * MULT / RHON

 121            CONTINUE
 120          CONTINUE
C
C           SI CONTACT POUR CE PG : ON REMPLIT LES MATRICES A, At ET A_U
            ELSE IF (INDCO(ISSPG).EQ.1) THEN
C
C             I.1. CALCUL DE A ET DE At
              DO 130 I = 1,NNOF

                FFI=ZR(IVFF-1+NNOF*(IPGF-1)+I)
                NI=XOULA(CFACE,IFA,I,JAINT,TYPMA)
                CALL XPLMAT(NDIM,DDLH,DDLE,DDLC,NNO,NNOM,NI,PLI)

                DO 131 J = 1,NNO
                  DO 132 L = 1,DDLH
C
                 MMAT(PLI,DDLS*(J-1)+NDIM+L)=MMAT(PLI,DDLS*(J-1)+NDIM+L)
     &             + 2.D0 * FFI * FFP(J) * ND(L) * JAC * MULT
C
                 MMAT(DDLS*(J-1)+NDIM+L,PLI)=MMAT(DDLS*(J-1)+NDIM+L,PLI)
     &             + 2.D0 * FFI * FFP(J) * ND(L) * JAC * MULT
C
 132              CONTINUE
 131            CONTINUE

 130          CONTINUE
C
C             I.2. CALCUL DE A_U
              DO 140 I = 1,NNO
                DO 141 J = 1,NNO
                  DO 142 K = 1,DDLH
                    DO 143 L = 1,DDLH
C
                      MMAT(DDLS*(I-1)+NDIM+K,DDLS*(J-1)+NDIM+L) =  
     &                MMAT(DDLS*(I-1)+NDIM+K,DDLS*(J-1)+NDIM+L)+
     &                4.D0*RHON*FFP(I)*FFP(J)*ND(K) * ND(L) * JAC * MULT
C
 143                CONTINUE
 142              CONTINUE
 141            CONTINUE
 140          CONTINUE
C
            ELSE
C             SI INDCO N'EST NI ÉGAL À 0 NI ÉGAL À 1
              CALL UTMESS('F','TE0533','PB DE STATUT DE CONTACT') 
            END IF
C

C         II) CALCUL DES MATRICES DE FROTTEMENT
C         ..............................

          ELSEIF (OPTION.EQ.'RIGI_FROT') THEN

            IF (MU.EQ.0.D0.OR.SEUIL(ISSPG).EQ.0.D0) INDCO(ISSPG) = 0

C           SI PAS DE CONTACT POUR CE PG : ON REMPLIT QUE LA MATRICE F 
            IF (INDCO(ISSPG).EQ.0) THEN

              DO 150 I = 1,NNOF
                FFI=ZR(IVFF-1+NNOF*(IPGF-1)+I)
                NLI=CFACE(IFA,I)
                NI=XOULA(CFACE,IFA,I,JAINT,TYPMA)
                CALL XPLMAT(NDIM,DDLH,DDLE,DDLC,NNO,NNOM,NI,PLI)
                DO 151 J = 1,NNOF
                  FFJ=ZR(IVFF-1+NNOF*(IPGF-1)+J)
                  NLJ=CFACE(IFA,J)
                  NJ=XOULA(CFACE,IFA,J,JAINT,TYPMA)
                  CALL XPLMAT(NDIM,DDLH,DDLE,DDLC,NNO,NNOM,NJ,PLJ)

C             METR : MÉTRIQUE DE LA BASE COVARIANTE AUX PTS D'INTERSECT
              CALL PSCAL(3,TAU1(1,NLI),TAU1(1,NLJ),METR(1,1))
              CALL PSCAL(3,TAU1(1,NLI),TAU2(1,NLJ),METR(1,2))
              CALL PSCAL(3,TAU2(1,NLI),TAU1(1,NLJ),METR(2,1))
              CALL PSCAL(3,TAU2(1,NLI),TAU2(1,NLJ),METR(2,2))

                  DO 152 K = 1,2
                    DO 153 L = 1,2
                      
                      MMAT(PLI+K,PLJ+L) = MMAT(PLI+K,PLJ+L)
     &                           + FFI * FFJ * METR(K,L) * JAC * MULT

 153                CONTINUE
 152              CONTINUE
 151            CONTINUE
 150          CONTINUE

C           SI CONTACT POUR CE PG : ON REMPLIT B, Bt, B_U et F
            ELSE IF (INDCO(ISSPG).EQ.1) THEN

C             P : OPÉRATEUR DE PROJECTION
              CALL XMAFR1(ND,P)

C             ON TESTE L'ETAT D'ADHERENCE DU PG (AVEC DEPDEL)
              CALL LCINVN(NDIM,0.D0,SAUT)
              CALL LCINVN(NDIM,0.D0,LAMB1)
              DO 156 INO=1,NNO
                DO 157 J=1,NDIM
              SAUT(J)=SAUT(J)-2.D0*ZR(IDEPD-1+DDLS*(INO-1)+3+J)*FFP(INO)
 157            CONTINUE
 156          CONTINUE
              DO 158 I=1,NNOF
                FFI=ZR(IVFF-1+NNOF*(IPGF-1)+I)
                NLI=CFACE(IFA,I)
                NI=XOULA(CFACE,IFA,I,JAINT,TYPMA)
                CALL XPLMAT(NDIM,DDLH,DDLE,DDLC,NNO,NNOM,NI,PLI)       
                DO 159 J=1,NDIM
                  LAMB1(J)=LAMB1(J)+FFI*(ZR(IDEPD-1+PLI+1)*TAU1(J,NLI)
     &                                  +ZR(IDEPD-1+PLI+2)*TAU2(J,NLI))
 159            CONTINUE
 158          CONTINUE
              CALL XADHER(P,SAUT,LAMB1,RHOTK,R3,KN,PTKNP,IK)
              CALL PROMAT(KN,NDIM,NDIM,NDIM,P,NDIM,NDIM,NDIM,KNP)

C             II.1. CALCUL DE B ET DE Bt
              DO 160 I = 1,NNOF

                FFI=ZR(IVFF-1+NNOF*(IPGF-1)+I)
                NLI=CFACE(IFA,I)
                NI=XOULA(CFACE,IFA,I,JAINT,TYPMA)
                CALL XPLMAT(NDIM,DDLH,DDLE,DDLC,NNO,NNOM,NI,PLI)

C               CALCUL DE TAU.KN.P
                DO 161 J = 1,NDIM
                  TAUKNP(1,J) = 0.D0
                  DO 162 K = 1,NDIM
                    TAUKNP(1,J) = TAUKNP(1,J) + TAU1(K,NLI) * KNP(K,J)
 162              CONTINUE
 161            CONTINUE
                DO 163 J = 1,NDIM
                  TAUKNP(2,J) = 0.D0
                  DO 164 K = 1,NDIM
                    TAUKNP(2,J) = TAUKNP(2,J) + TAU2(K,NLI) * KNP(K,J)
 164              CONTINUE
 163            CONTINUE

                DO 165 J = 1,NNO

                  DO 166 K = 1,2
                    DO 167 L = 1,DDLH
C
             MMAT(PLI+K,DDLS*(J-1)+NDIM+L)=MMAT(PLI+K,DDLS*(J-1)+NDIM+L)
     &           +  2.D0*MU*SEUIL(ISSPG)*FFI*FFP(J)*TAUKNP(K,L)*JAC*MULT
C
             MMAT(DDLS*(J-1)+NDIM+L,PLI+K)=MMAT(DDLS*(J-1)+NDIM+L,PLI+K)
     &           +  2.D0*MU*SEUIL(ISSPG)*FFI*FFP(J)*TAUKNP(K,L)*JAC*MULT
C
 167                CONTINUE
 166              CONTINUE
 165            CONTINUE
 160          CONTINUE

C             II.2. CALCUL DE B_U
              DO 170 I = 1,NNO
                DO 171 J = 1,NNO
                  DO 172 K = 1,DDLH
                    DO 173 L = 1,DDLH
C
                      MMAT(DDLS*(I-1)+NDIM+K,DDLS*(J-1)+NDIM+L) =  
     &                MMAT(DDLS*(I-1)+NDIM+K,DDLS*(J-1)+NDIM+L) -
     &      4.D0*MU*SEUIL(ISSPG)*RHOTK*FFP(I)*FFP(J)*PTKNP(K,L)*JAC*MULT
C
 173                CONTINUE
 172              CONTINUE
 171            CONTINUE
 170          CONTINUE
C
C             II.3. CALCUL DE F
              DO 180 I = 1,NNOF
                FFI=ZR(IVFF-1+NNOF*(IPGF-1)+I)
                NLI=CFACE(IFA,I)
                NI=XOULA(CFACE,IFA,I,JAINT,TYPMA)
                CALL XPLMAT(NDIM,DDLH,DDLE,DDLC,NNO,NNOM,NI,PLI)
                DO 181 J = 1,NNOF
                  FFJ=ZR(IVFF-1+NNOF*(IPGF-1)+J)
                  NLJ=CFACE(IFA,J)
                  NJ=XOULA(CFACE,IFA,J,JAINT,TYPMA)
                  CALL XPLMAT(NDIM,DDLH,DDLE,DDLC,NNO,NNOM,NJ,PLJ)

C                 CALCUL DE TAIKTA = TAUt.(Id-KN).TAU
                  CALL XMAFR2(NLI,NLJ,TAU1,TAU2,IK,TAIKTA)
      
                  DO 182 K = 1,2
                    DO 183 L = 1,2
                      
                      MMAT(PLI+K,PLJ+L) = MMAT(PLI+K,PLJ+L)
     &              - MU*SEUIL(ISSPG)/RHOTK*FFI*FFJ*TAIKTA(K,L)*JAC*MULT

 183                CONTINUE
 182              CONTINUE
 181            CONTINUE
 180          CONTINUE

            ELSE
C             SI INDCO N'EST NI ÉGAL À 0 NI ÉGAL À 1
              CALL UTMESS('F','TE0533','PB DE STATUT DE CONTACT') 
            END IF

          ELSE
C           SI OPTION NI 'RIGI_CONT' NI 'RIGI_FROTT'
            CALL UTMESS('F','TE0533','OPTION INCONNUE')           
          ENDIF

C         FIN DE BOUCLE SUR LES POINTS DE GAUSS
 110    CONTINUE

C       FIN DE BOUCLE SUR LES FACETTES
 100  CONTINUE
C-----------------------------------------------------------------------
C
C     FIN DE CHANGEMENT ET COPIE
      DO 200 J = 1,NDDL
        DO 210 I = 1,J
          IJ = (J-1)*J/2 + I
          ZR(IMATT+IJ-1) = MMAT(I,J)
 210    CONTINUE
 200  CONTINUE
C
 9999 CONTINUE

      CALL JEDEMA()
      END

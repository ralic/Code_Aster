      SUBROUTINE TE0339 (OPTION,NOMTE)
      IMPLICIT NONE
      CHARACTER*(*)     OPTION,NOMTE
C     -----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 18/06/2002   AUTEUR F6BHHBO P.DEBONNIERES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     -----------------------------------------------------------------
C     FONCTION REALISEE :
C
C         CALCUL DU TAUX DE CROISSANCE DE CAVITES SELON UNE LOI DE
C         RICE ET TRACEY EN COMPORTEMENT NON-LINEAIRE.
C         ELEMENTS ISOPARAMETRIQUES 3D.
C
C         OPTION : 'RICE_TRACEY'
C
C ENTREE  --->  OPTION : NOM DE L'OPTION DE CALCUL
C         --->  NOMTE  : NOM DU TYPE D'ELEMENT
C
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
C-DEL CHARACTER*32 JEXNUM,JEXNOM,JEXATR,JEXR8
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
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) ,ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CHARACTER*24 CARAC,FF
      CHARACTER*16 OPTCAL(2)
      CHARACTER*8  ELREFE
      REAL*8       SIG(6),TRIAX,VOLU,RSR0,NUMEMA,DEPSEQ
      REAL*8       POIDS,DVPG,SIGM,SIGEQ,LRSR0M,LRSR0P
      REAL*8       CONG(6),VARIGP,VARIGM,CROIS,VK,DFDBID(30)
      INTEGER      NNO,KP,NPG,I,IRITRA,NDIM
      INTEGER      ISSOPT,IMA,K,IADZI,IAZK24,NBVARI,IPOPP,ICOMPO
      INTEGER      ICARAC,IFF,IPOIDS,IVF,IDFDE,IDFDK,IDFDN
      INTEGER      IGEOM,ICONG,IVARPG,IVARMG,ISDRMR,ISDRPR,JTAB(7)
C
C======================== CORPS DU PROGRAMME ===========================
C
C-DBG WRITE(6,*)'========== TE0339 : IN  ==========='
C     1. RECUPERATION DES INFOS
C     -------------------------
C     1.1 NOMBRE DE NOEUDS ET DE POINTS DE GAUSS
C     ------------------------------------------
      CALL ELREF1(ELREFE)

      CARAC='&INEL.'//ELREFE//'.CARACTE'
      CALL JEVETE(CARAC,'L',ICARAC)
      NDIM = ZI(ICARAC)
      NNO  = ZI(ICARAC+1)
      NPG  = ZI(ICARAC+3)
C
      FF ='&INEL.'//ELREFE//'.FFORMES'
      CALL JEVETE(FF,'L',IFF)
      IPOIDS = IFF + (NDIM+1)*NNO*NNO
      IVF    = IPOIDS + NPG
      IDFDE  = IVF    + NPG*NNO
      IDFDN  = IDFDE  + 1
      IDFDK  = IDFDN  + 1
C
C     1.2 NUMERO DE LA MAILLE
C     -----------------------
      CALL TECAEL(IADZI,IAZK24)
      IMA =ZI(IADZI)
      NUMEMA= DBLE(IMA)
C
C     1.3 CHAMPS IN
C     -------------
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PCONTPR','L',ICONG)
      CALL JEVECH('PVARIMR','L',IVARMG)
      CALL JEVECH('PVARIPR','L',IVARPG)
      CALL JEVECH('PSDRMR', 'L',ISDRMR)
      CALL JEVECH('PSOUSOP','L',ISSOPT)
      CALL TECACH(.TRUE.,.TRUE.,'PVARIPR',7,JTAB)     
      NBVARI = MAX(JTAB(6),1)*JTAB(7)
      CALL JEVECH('PCOMPOR','L',ICOMPO)
C     READ (ZK16(ICOMPO+1),'(I16)') NBVARI
C
      IF (     (ZK16(ICOMPO).EQ.'VMIS_ISOT_TRAC')
     &     .OR.(ZK16(ICOMPO).EQ.'VMIS_ISOT_LINE')
     &     .OR.(ZK16(ICOMPO).EQ.'LEMAITRE')
     &     .OR.(ZK16(ICOMPO).EQ.'VMIS_ECMI_TRAC')
     &     .OR.(ZK16(ICOMPO).EQ.'VMIS_ECMI_LINE')
     &     .OR.(ZK16(ICOMPO).EQ.'VISC_CIN1_CHAB')
     &     .OR.(ZK16(ICOMPO).EQ.'VISC_CIN2_CHAB') ) THEN
        IPOPP  = 1
      ELSE IF  (ZK16(ICOMPO).EQ.'CHABOCHE') THEN
        IPOPP  = 13
      ELSE 
        CALL UTMESS('F','TE0339','POUR L''OPTION '//
     +          '"RICE_TRACEY", LA RELATION "'//ZK16(ICOMPO)//
     +          '" N''EST PAS ADMISE') 
      END IF
C   /* ========================================================= */
C   /* PVARIMR = DEF PLAST EQ A L'INSTANT PRECEDENT              */
C   /* PVARIPR = DEF PLAST EQ A L'INSTANT COURRANT               */
C   /* PSDRMR  = LOG DU TAUX DE CROISSANCE A L'INSTANT PRECEDENT */
C   /* ========================================================= */
C
C     1.4 CHAMPS OUT
C     --------------
      CALL JEVECH('PRICTRA','E',IRITRA)
      CALL JEVECH('PSDRPR', 'E',ISDRPR)
C   /* ========================================================= */
C   /* PRICTRA = CHAM_ELEM RICE-TRACEY (5 CMP)                   */
C   /* PSDRPR  = LOG DU TAUX DE CROISSANCE A L'INSTANT COURRANT  */
C   /* ========================================================= */
C
C     1.5 OPTIONS DE CALCUL
C     ---------------------
      OPTCAL(1) = ZK24(ISSOPT)(1:16)
      OPTCAL(2) = ZK24(ISSOPT)(17:19)
C
C     1.6 INITIALISATION
C     ------------------
      POIDS  =0.D0
      TRIAX  =0.D0
      RSR0   =0.D0
      VOLU   =0.D0
      VK     =0.D0
      DVPG   =0.D0
      DEPSEQ = 0.D0
      DO 10, I = 1, 6, 1
         CONG(I) = 0.D0
10    CONTINUE
      VARIGM = 0.D0
      VARIGP = 0.D0
C
C
C     2. BOUCLE SUR POINTS DE GAUSS SUIVANT OPTIONS DE CALCUL
C     -------------------------------------------------------
C   /* ============================================================== */
C   /* CMP ACTIVES DU CHAM_ELEM RICE-TRACEY SUIVANT LES OPTIONS       */
C   /* -------------------------------------------------------------- */
C   /* 1. CALCUL DU TAUX MOYEN : PREPRATION POUR INTEGRATION DE RT    */
C   /*       TAUX DE TRIAXIALITE SUR LA MAILLE               (TRIAX ) */
C   /*       VARIATION DE DEF PLAST EQ                       (DEPSEQ) */
C   /*       VOLUME PRIS EN COMPTE                           (VOLU  ) */
C   /*       NUMERO DE LA MAILLE                             (NUMEMA) */
C   /*    IE. CE QU'IL FAUDRA MOYENNER AVANT D'INTEGRER RT            */
C   /*                                                                */
C   /* 2. CALCUL DU TAUX MAX : INTEGRATION DE RT SUR LA MAILLE        */
C   /*       VOLUME PRIS EN COMPTE                           (VOLU  ) */
C   /*       TAUX DE CROISSANCE SUR LA MAILLE INSTANT COURRANT (RSR0) */
C   /*       NUMERO DE LA MAILLE                             (NUMEMA) */
C   /*    DANS CE CAS, LE PARAM OUT PSDRPR JOUE VRAIMENT SON ROLE     */
C   /* ============================================================== */
C
C     2.1 CHAM_ELEM POUR LE CALCUL DU TAUX MOYEN AVEC CHAMPS IN MOYENNES
C     ------------------------------------------------------------------
      IF ((OPTCAL(1).EQ.'SIGM_ELMOY').AND.(OPTCAL(2).EQ.'NON')) THEN
C        2.1.1 INTEGRATION PAR QUADRATURE DES CHAMPS IN
C        ----------------------------------------------
         DO 100, KP = 1, NPG, 1
            K = (KP - 1)*NNO*3
            CALL DFDM3D (NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDN+K),
     &              ZR(IDFDK+K),ZR(IGEOM),DFDBID,DFDBID,DFDBID,POIDS)
            DVPG = POIDS
            VK = VK + DVPG
            DO 110, I = 1, 6, 1
               CONG(I)=CONG(I)+DVPG*ZR(ICONG+6*KP+I-7)
110         CONTINUE
            VARIGM = VARIGM+DVPG*ZR(IVARMG+NBVARI*(KP-1)+IPOPP-1)
            VARIGP = VARIGP+DVPG*ZR(IVARPG+NBVARI*(KP-1)+IPOPP-1)
100      CONTINUE
C
C        2.1.2 VALEUR MOYENNE DES CHAMPS IN SUR LA MAILLE
C        ------------------------------------------------
         DO 120, I = 1, 6, 1
            SIG(I) = CONG(I)/VK
120      CONTINUE
         VARIGM  =VARIGM/VK
         VARIGP  =VARIGP/VK
C
C        2.1.3 INVARIANTS
C        ----------------
         SIGM  = (SIG(1)+SIG(2)+SIG(3))/3.D0
         SIGEQ =  SIG(4)*SIG(4)+SIG(5)*SIG(5)+SIG(6)*SIG(6)
         SIGEQ =  SIGEQ + SIGEQ
         SIGEQ =  SIGEQ + (SIG(1)-SIGM)*(SIG(1)-SIGM) +
     &                    (SIG(2)-SIGM)*(SIG(2)-SIGM) +
     &                    (SIG(3)-SIGM)*(SIG(3)-SIGM)
         SIGEQ = SQRT(1.5D0*SIGEQ)
C
C        2.1.4 CHAMPS OUT
C        ----------------
         TRIAX  = SIGM/SIGEQ
         VOLU   = VK
         DEPSEQ = VARIGP-VARIGM
         DO 130, I = 1, NPG, 1
            ZR(ISDRPR+I-1) = ZR(ISDRMR+I-1)
130      CONTINUE
C
C     2.2 CHAM_ELEM POUR CALCUL DU TAUX MOYEN AVEC CHAMPS IN ORIGINAUX
C     ----------------------------------------------------------------
      ELSEIF ((OPTCAL(1).EQ.'SIGM_ELGA').AND.(OPTCAL(2).EQ.'NON')) THEN
         DO 200, KP = 1, NPG, 1
C           2.2.1 RECUPERATION DES CHAMPS IN
C           --------------------------------
            DO 210, I = 1, 6, 1
               CONG(I)= ZR(ICONG+6*KP+I-7)
210         CONTINUE
            VARIGM = ZR(IVARMG+NBVARI*(KP-1)+IPOPP-1)
            VARIGP = ZR(IVARPG+NBVARI*(KP-1)+IPOPP-1)
C
C           2.2.2 CALCUL DE LA TRIAXIALITE LOCALE
C           -------------------------------------
            SIGM  = (CONG(1)+CONG(2)+CONG(3))/3.D0
            SIGEQ =  CONG(4)*CONG(4)+CONG(5)*CONG(5)+CONG(6)*CONG(6)
            SIGEQ =  SIGEQ + SIGEQ
            SIGEQ =  SIGEQ + (CONG(1)-SIGM)*(CONG(1)-SIGM) +
     &                       (CONG(2)-SIGM)*(CONG(2)-SIGM) +
     &                       (CONG(3)-SIGM)*(CONG(3)-SIGM)
            SIGEQ = SQRT(1.5D0*SIGEQ)
C
C           2.2.3 INTEGRATION PAR QUADRATURE
C           --------------------------------
            K = (KP - 1)*NNO*3
            CALL DFDM3D (NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDN+K),
     &              ZR(IDFDK+K),ZR(IGEOM),DFDBID,DFDBID,DFDBID,POIDS)
            DVPG   = POIDS
            VK     = VK + DVPG
            TRIAX  = TRIAX  + DVPG*(SIGM/SIGEQ)
            DEPSEQ = DEPSEQ + DVPG*(VARIGP - VARIGM)
200      CONTINUE
C
C        2.2.4 CHAMPS OUT
C        ----------------
         TRIAX  = TRIAX/VK
         VOLU   = VK
         DEPSEQ = DEPSEQ/VK
         DO 220, I = 1, NPG, 1
            ZR(ISDRPR+I-1) = ZR(ISDRMR+I-1)
220      CONTINUE
C
C     2.3 CHAM_ELEM POUR LE CALCUL DU TAUX MAX AVEC CHAMPS IN MOYENNES
C     ----------------------------------------------------------------
      ELSEIF ((OPTCAL(1).EQ.'SIGM_ELMOY').AND.(OPTCAL(2).EQ.'OUI')) THEN
C        2.3.1 INTEGRATION PAR QUADRATURE DES CHAMPS IN
C        ----------------------------------------------
         DO 300, KP = 1, NPG, 1
            K = (KP - 1)*NNO*3
            CALL DFDM3D (NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDN+K),
     &              ZR(IDFDK+K),ZR(IGEOM),DFDBID,DFDBID,DFDBID,POIDS)
            DVPG = POIDS
            VK = VK + DVPG
            DO 310, I = 1, 6, 1
               CONG(I)=CONG(I)+DVPG*ZR(ICONG+6*KP+I-7)
310         CONTINUE
            VARIGM = VARIGM+DVPG*ZR(IVARMG+NBVARI*(KP-1)+IPOPP-1)
            VARIGP = VARIGP+DVPG*ZR(IVARPG+NBVARI*(KP-1)+IPOPP-1)
300      CONTINUE
C
C        2.3.2 VALEUR MOYENNE DES CHAMPS IN SUR LA MAILLE
C        ------------------------------------------------
         DO 320, I = 1, 6, 1
            SIG(I) = CONG(I)/VK
320      CONTINUE
         VARIGM  =VARIGM/VK
         VARIGP  =VARIGP/VK
C
C        2.3.3 INVARIANTS
C        ----------------
         SIGM  = (SIG(1)+SIG(2)+SIG(3))/3.D0
         SIGEQ =  SIG(4)*SIG(4)+SIG(5)*SIG(5)+SIG(6)*SIG(6)
         SIGEQ =  SIGEQ + SIGEQ
         SIGEQ =  SIGEQ + (SIG(1)-SIGM)*(SIG(1)-SIGM) +
     &                    (SIG(2)-SIGM)*(SIG(2)-SIGM) +
     &                    (SIG(3)-SIGM)*(SIG(3)-SIGM)
         SIGEQ = SQRT(1.5D0*SIGEQ)
C
C        2.3.4 INTEGRATION DE LA LOI RT
C        ------------------------------
         TRIAX  = SIGM/SIGEQ
         VOLU   = VK
         DEPSEQ = VARIGP-VARIGM
         LRSR0M = ZR(ISDRMR)
         LRSR0P = LRSR0M +
     &            0.283D0*SIGN(1.0D0,TRIAX)*EXP(1.5D0*ABS(TRIAX))*DEPSEQ
C
C        2.3.5 CHAMPS OUT
C        ----------------
         RSR0 = EXP(LRSR0P)
         DO 330, I = 1, NPG, 1
            ZR(ISDRPR+I-1) = LRSR0P
330      CONTINUE
C
C     2.4 CHAM_ELEM POUR LE CALCUL DU TAUX MAX AVEC CHAMPS IN ORIGINAUX
C     -----------------------------------------------------------------
      ELSEIF ((OPTCAL(1).EQ.'SIGM_ELGA').AND.(OPTCAL(2).EQ.'OUI')) THEN
         DO 400, KP = 1, NPG, 1
C           2.4.1 RECUPERATION DES CHAMPS IN
C           --------------------------------
            DO 410, I = 1, 6, 1
               CONG(I)= ZR(ICONG+6*KP+I-7)
410         CONTINUE
            VARIGM = ZR(IVARMG+NBVARI*(KP-1)+IPOPP-1)
            VARIGP = ZR(IVARPG+NBVARI*(KP-1)+IPOPP-1)
            K = (KP - 1)*NNO*3
            CALL DFDM3D (NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDN+K),
     &               ZR(IDFDK+K),ZR(IGEOM),DFDBID,DFDBID,DFDBID,POIDS)
            DVPG   = POIDS
C
C           2.4.2 CALCUL DE LA TRIAXIALITE LOCALE
C           -------------------------------------
            SIGM  = (CONG(1)+CONG(2)+CONG(3))/3.D0
            SIGEQ =  CONG(4)*CONG(4)+CONG(5)*CONG(5)+CONG(6)*CONG(6)
            SIGEQ =  SIGEQ + SIGEQ
            SIGEQ =  SIGEQ + (CONG(1)-SIGM)*(CONG(1)-SIGM) +
     &                       (CONG(2)-SIGM)*(CONG(2)-SIGM) +
     &                       (CONG(3)-SIGM)*(CONG(3)-SIGM)
            SIGEQ = SQRT(1.5D0*SIGEQ)
            TRIAX = SIGM/SIGEQ
C
C           2.4.3 INTEGRATION DE LA LOI RT AU PG COURRANT
C           ---------------------------------------------
            DEPSEQ = VARIGP - VARIGM
            LRSR0M = ZR(ISDRMR + KP-1)
            LRSR0P = LRSR0M +
     &            0.283D0*SIGN(1.0D0,TRIAX)*EXP(1.5D0*ABS(TRIAX))*DEPSEQ
            CROIS = EXP(LRSR0P)
C
C           2.4.4 CHAMPS OUT
C           ----------------
            ZR(ISDRPR + KP-1) = LRSR0P
            IF (CROIS .GT. RSR0) THEN
               RSR0 = CROIS
               VOLU = DVPG
            ENDIF
400      CONTINUE
C
C     2.5 TRAITEMENT DES OPTIONS INVALIDES
C     ------------------------------------
      ELSE
         CALL UTMESS('F','TE0339','OPTION DE CALCUL NON VALIDE')
      ENDIF
C
C
C     3. ECRITURE DES CMP DU CHAM_ELEM OUT DE TYPE RICE-TRACEY
C     --------------------------------------------------------
      ZR(IRITRA)   = TRIAX
      ZR(IRITRA+1) = RSR0
      ZR(IRITRA+2) = VOLU
      ZR(IRITRA+3) = NUMEMA
      ZR(IRITRA+4) = DEPSEQ
C
C-DBG WRITE(6,*)'========== TE0339 : OUT ==========='
      END

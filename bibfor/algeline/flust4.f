      SUBROUTINE FLUST4(MELFLU,TYPFLU,BASE,NOMA,NUOR,AMOR,FREQ,MASG,
     &                  FACT,VITE,NBM,NPV,NIVPAR,NIVDEF)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C-----------------------------------------------------------------------
C  CALCUL DES PARAMETRES DE COUPLAGE FLUIDE-STRUCTURE POUR UNE
C  CONFIGURATION DE TYPE "COQUES CYLINDRIQUES COAXIALES"
C  OPERATEUR APPELANT : CALC_FLUI_STRU , OP0144
C-----------------------------------------------------------------------
C  IN : MELFLU : NOM DU CONCEPT DE TYPE MELASFLU PRODUIT
C  IN : TYPFLU : NOM DU CONCEPT DE TYPE TYPE_FLUI_STRU DEFINISSANT LA
C                CONFIGURATION ETUDIEE
C  IN : BASE   : NOM DU CONCEPT DE TYPE MODE_MECA DEFINISSANT LA BASE
C                MODALE DU SYSTEME AVANT PRISE EN COMPTE DU COUPLAGE
C  IN : NOMA   : NOM DU CONCEPT DE TYPE MAILLAGE
C  IN : NUOR   : LISTE DES NUMEROS D'ORDRE DES MODES SELECTIONNES POUR
C                LE COUPLAGE (PRIS DANS LE CONCEPT MODE_MECA)
C  IN : AMOR   : LISTE DES AMORTISSEMENTS REDUITS MODAUX INITIAUX
C  IN : VITE   : LISTE DES VITESSES D'ECOULEMENT ETUDIEES
C  IN : NBM    : NOMBRE DE MODES PRIS EN COMPTE POUR LE COUPLAGE
C  IN : NPV    : NOMBRE DE VITESSES D'ECOULEMENT
C  IN : NIVPAR : NIVEAU D'IMPRESSION DANS LE FICHIER RESULTAT POUR LES
C                PARAMETRES DU COUPLAGE (FREQ,AMOR)
C  IN : NIVDEF : NIVEAU D'IMPRESSION DANS LE FICHIER RESULTAT POUR LES
C                DEFORMEES MODALES
C  OUT: FREQ   : LISTE DES FREQUENCES ET AMORTISSEMENTS REDUITS MODAUX
C                PERTURBES PAR L'ECOULEMENT
C  OUT: MASG   : MASSES GENERALISEES DES MODES PERTURBES
C  OUT: FACT   : PSEUDO FACTEUR DE PARTICIPATION
C-----------------------------------------------------------------------
C
      INCLUDE 'jeveux.h'
      INTEGER      NBM, NPV, NIVPAR, NIVDEF, NUOR(*)
      REAL*8       AMOR(*), FREQ(*), MASG(*), VITE(*), FACT(*)
      CHARACTER*8  TYPFLU, BASE, NOMA
      CHARACTER*19 MELFLU
C
      LOGICAL      VNEG,VPOS,CALCUL(2)
      REAL*8       MCF0,KSI,CARAC(2)
      CHARACTER*8  CAELEM,MATER1,MATER2, K8B
      CHARACTER*24 FSVI,FSVR,FSVK,FSGM
      COMPLEX*16   BII
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER IAMFR ,IAXE ,ICOEF ,ICOMP ,IER ,IFR ,IFREQI 
      INTEGER IGEOM ,IICOQ ,IM ,IMAJ ,IMASSE ,IMOD ,IOR 
      INTEGER IORCO ,IUNIFI ,IV ,IVABS ,IVCPR ,IWORK ,JMOD 
      INTEGER KEC ,LFACT ,LFSGM ,LFSVI ,LFSVK ,LFSVR ,LMASG 
      INTEGER LWORK ,N1 ,NT ,NUMOD 
      REAL*8 CF0 ,FI ,HMOY ,PI ,R8PI ,RBID ,S0 
      REAL*8 U0 
C-----------------------------------------------------------------------
      CALL JEMARQ()
      IFR = IUNIFI('RESULTAT')
C
      PI = R8PI()
C
C
C --- 1.VERIFICATION DU SIGNE DES VITESSES
C ---   LES VITESSES ETUDIEES DOIVENT TOUTES ETRE DU MEME SIGNE
C
      VNEG = .FALSE.
      VPOS = .FALSE.
      DO 10 IV = 1,NPV
        IF (VITE(IV).LT.0.D0) THEN
          VNEG = .TRUE.
        ELSE IF (VITE(IV).GT.0.D0) THEN
          VPOS = .TRUE.
        ENDIF
  10  CONTINUE
      IF (VNEG .AND. VPOS) THEN
        CALL U2MESS('F','ALGELINE_48')
      ELSE IF (VNEG) THEN
        KEC = -1
      ELSE
        KEC = 1
      ENDIF
C
      CALL WKVECT('&&FLUST4.TEMP.VABS','V V R',NPV,IVABS)
      IF (VNEG) THEN
        DO 11 IV = 1,NPV
          ZR(IVABS+IV-1) = DBLE(ABS(VITE(IV)))
  11    CONTINUE
      ELSE
        DO 12 IV = 1,NPV
          ZR(IVABS+IV-1) = VITE(IV)
  12    CONTINUE
      ENDIF
C
C
C --- 2.RECUPERATION DES INFORMATIONS APPORTEES PAR LE CONCEPT  ---
C ---   TYPE_FLUI_STRU                                          ---
C
      FSVI = TYPFLU//'           .FSVI'
      CALL JEVEUO(FSVI,'L',LFSVI)
      IMASSE = ZI(LFSVI)
      IAXE   = ZI(LFSVI+1)
C
      FSVR = TYPFLU//'           .FSVR'
      CALL JEVEUO(FSVR,'L',LFSVR)
C
      FSVK = TYPFLU//'           .FSVK'
      CALL JEVEUO(FSVK,'L',LFSVK)
      CAELEM = ZK8(LFSVK)
      MATER1 = ZK8(LFSVK+1)
      MATER2 = ZK8(LFSVK+2)
C
      FSGM = TYPFLU//'           .FSGM'
      CALL JEVEUO(FSGM,'L',LFSGM)
C
C
C --- 3.CREATION DES GROUPES DE NOEUDS CORRESPONDANT AUX COQUES ---
C ---   INTERNE ET EXTERNE, A PARTIR DES GROUPES DE MAILLES     ---
C
      CALL MEFGMN(NOMA,2,ZK8(LFSGM))
C
C
C --- 4.DETERMINATION DES GRANDEURS GEOMETRIQUES CARACTERISTIQUES ---
C ---   DE LA CONFIGURATION                                       ---
C
      CALL WKVECT('&&FLUST4.TEMP.GEOM','V V R',9,IGEOM)
      CALL GEOCOQ(NOMA,ZK8(LFSGM),CAELEM,IAXE,ZR(IGEOM))
C
      HMOY = ZR(IGEOM)
C
C
C --- 5.CARACTERISATION DES DEFORMEES MODALES AVANT PRISE EN COMPTE ---
C ---   DU COUPLAGE                                                 ---
C
      CALL WKVECT('&&FLUST4.TEMP.ICOQ','V V I',NBM   ,IICOQ)
      CALL WKVECT('&&FLUST4.TEMP.ORCO','V V R',4*NBM ,IORCO)
      CALL WKVECT('&&FLUST4.TEMP.COEF','V V R',10*NBM,ICOEF)

      CALL RSLIPA(BASE,'FREQ','&&FLUST4.LIFREQ',IFREQI,N1)
      CALL MODCOQ(BASE,NUOR,NBM,MATER1,MATER2,NOMA,ZK8(LFSGM),IAXE,KEC,
     &            ZR(IGEOM),ZI(IICOQ),ZR(IORCO),ZR(ICOEF),IFREQI)
C
C
C --- 6.PRISE EN COMPTE DU COUPLAGE FLUIDELASTIQUE
C
      WRITE(IFR,*) '<FLUST4> COUPLAGE FLUIDE-STRUCTURE POUR COQUE_COAX'
      WRITE(IFR,*)
      CALL WKVECT('&&FLUST4.TEMP.MAJ' ,'V V R',NBM  ,IMAJ )
      CALL WKVECT('&&FLUST4.TEMP.AMFR','V V R',2*NBM,IAMFR)
      NT = 2
      LWORK = 2*NT*NT + 10*NT + 2
      CALL WKVECT('&&FLUST4.TEMP.WORK','V V R',LWORK,IWORK)
C
C
      U0 = 0.D0
      CF0 = 0.D0
      MCF0 = 1.D0
      S0 = 0.D0
C     =================================================================
C
C --- 6.1.CAS OU LES EFFETS DE MASSE AJOUTEE ONT DEJA ETE PRIS EN COMPTE
C
C     =================================================================
      IF ( IMASSE .EQ. 0 ) THEN
C
C-------6.1.1.ON RECOPIE LES MASSES GENERALISEES ET LES DEFORMEES
C
        DO 50 IM = 1,NBM
          IOR = NUOR(IM)
          CALL RSADPA ( BASE,'L',1,'MASS_GENE'      ,IOR,0,LMASG,K8B)
          CALL RSADPA ( BASE,'L',1,'FACT_PARTICI_DX',IOR,0,LFACT,K8B)
          MASG(IM) = ZR(LMASG)
          FACT(3*(IM-1)+1) = ZR(LFACT  ) * MASG(IM)
          FACT(3*(IM-1)+2) = ZR(LFACT+1) * MASG(IM)
          FACT(3*(IM-1)+3) = ZR(LFACT+2) * MASG(IM)
 50     CONTINUE
        CALL CPDEPL(MELFLU,BASE,NUOR,NBM)
C
C-------6.1.2.CALCUL DE LA MATRICE DE MASSE AJOUTEE A RETRANCHER AUX
C             EXCITATIONS MODALES DUES AUX FORCES FLUIDELASTIQUES
C
        WRITE(IFR,*) 'CALCUL DES MASSES MODALES AJOUTEES PAR LE FLUIDE'
        WRITE(IFR,*)
        DO 20 IMOD = 1,NBM
C
          NUMOD = NUOR(IMOD)
          WRITE(IFR,'(A9,I3)') 'NUMOD = ',NUMOD
          FI  = ZR(IFREQI+NUMOD-1)
          KSI = AMOR(IMOD)
C
          CALL BIJMOC(U0,ZR(IGEOM),CF0,MCF0,ZR(LFSVR),IMOD,IMOD,NBM,
     &                ZI(IICOQ),ZR(IORCO),ZR(ICOEF),S0,S0,BII)
C
          ZR(IMAJ+IMOD-1) = -1.D0*DBLE(BII)
          WRITE(IFR,'(A5,G23.16)') 'MI = ',ZR(IMAJ+IMOD-1)
          WRITE(IFR,*)
C
          ZR(IAMFR+IMOD-1) = 4.D0*PI*FI*KSI*MASG(IMOD)
          ZR(IAMFR+NBM+IMOD-1) = FI
C
  20    CONTINUE
C
C-------6.1.3.CALCUL DES NOUVEAUX PARAMETRES MODAUX SOUS ECOULEMENT
C
        CALL PACOUC(TYPFLU,ZR(IMAJ),ZR(IORCO),ZR(IVABS),ZR(ICOEF),MASG,
     &              FREQ,ZR(IAMFR),NBM,IMASSE,NPV,ZR(IWORK),
     &              ZI(IICOQ),ZR(IGEOM),RBID,IER)
C
C-------6.1.4.CALCUL D'UN CRITERE DE POIDS DES TERMES EXTRADIAGONAUX
C             DE LA MATRICE B(S) PAR RAPPORT AUX TERMES DIAGONAUX
C
        IF (NBM.GT.1) CALL POIBIJ(NPV,ZR(IVABS),ZR(IGEOM),ZR(LFSVR),
     &                            NBM,ZI(IICOQ),ZR(IORCO),ZR(ICOEF),
     &                            FREQ,IMASSE,ZR(IMAJ),RBID)
C     =================================================================
C
C --- 6.2.CAS GENERAL
C
C     =================================================================
      ELSE
C
C-------6.2.1.CALCUL DES MODES EN EAU AU REPOS
C
        CALL WKVECT('&&FLUST4.TEMP.VCPR','V V R',NBM*NBM,IVCPR)
C
        CALL MODEAU(MELFLU,NOMA,ZR(IGEOM),ZR(LFSVR),BASE,ZR(IFREQI),
     &              NBM,NUOR,ZI(IICOQ),ZR(IORCO),ZR(ICOEF),AMOR,MASG,
     &              FACT,ZR(IAMFR),ZR(IVCPR),ZR(IMAJ))
C
        WRITE(IFR,*) 'RESULTATS DU CALCUL DES MODES EN EAU AU REPOS'
        WRITE(IFR,*)
        WRITE(IFR,*) 'FREQUENCES PROPRES'
        DO 30 IMOD = 1,NBM
          WRITE(IFR,'(I3,1X,G23.16)') IMOD,ZR(IAMFR+NBM+IMOD-1)
  30    CONTINUE
        WRITE(IFR,*)
        WRITE(IFR,*) 'DECOMPOSITION MODES EN EAU AU REPOS/MODES EN AIR'
        DO 31 JMOD = 1,NBM
          WRITE(IFR,'(A24,I3)') 'MODE EN EAU AU REPOS NO ',JMOD
          ICOMP = IVCPR + NBM*(JMOD-1)
          DO 32 IMOD = 1,NBM
            WRITE(IFR,'(G23.16,1X,A23,I3)') ZR(ICOMP+IMOD-1),
     &        'SUIVANT MODE EN AIR NO ',IMOD
  32      CONTINUE
          WRITE(IFR,*)
  31    CONTINUE
        WRITE(IFR,*)
        WRITE(IFR,*) 'MASSES MODALES'
        DO 33 IMOD = 1,NBM
          WRITE(IFR,'(I3,1X,G23.16)') IMOD,MASG(IMOD)
  33    CONTINUE
        WRITE(IFR,*)
C
C-------6.2.2.CALCUL DES NOUVEAUX PARAMETRES MODAUX SOUS ECOULEMENT
C
        CALL PACOUC(TYPFLU,ZR(IMAJ),ZR(IORCO),ZR(IVABS),ZR(ICOEF),MASG,
     &              FREQ,ZR(IAMFR),NBM,IMASSE,NPV,ZR(IWORK),
     &              ZI(IICOQ),ZR(IGEOM),ZR(IVCPR),IER)
C
C-------6.2.3.CALCUL D'UN CRITERE DE POIDS DES TERMES EXTRADIAGONAUX
C             DE LA MATRICE B(S) PAR RAPPORT AUX TERMES DIAGONAUX
C
        IF (NBM.GT.1) CALL POIBIJ(NPV,ZR(IVABS),ZR(IGEOM),ZR(LFSVR),
     &                            NBM,ZI(IICOQ),ZR(IORCO),ZR(ICOEF),
     &                            FREQ,IMASSE,ZR(IMAJ),ZR(IVCPR))
C
      ENDIF
C
C
C --- 7.IMPRESSIONS DANS LE FICHIER RESULTAT SI DEMANDEES ---
C
      IF (NIVPAR.EQ.1 .OR. NIVDEF.EQ.1) THEN
        CARAC(1)=2.D0*HMOY
        CARAC(2)=0.D0
        CALCUL(1)=.TRUE.
        CALCUL(2)=.FALSE.
        CALL FLUIMP(4,NIVPAR,NIVDEF,MELFLU,TYPFLU,NUOR,FREQ,
     &              ZR(IFREQI),NBM,VITE,NPV,CARAC,CALCUL,RBID)
      ENDIF
C
      CALL JEDETC('V','&&MEFGMN',1)
      CALL JEDETC('V','&&FLUST4',1)
      CALL JEDEMA()
C
      END

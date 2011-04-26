      SUBROUTINE MEFIST(MELFLU,NDIM,SOM,ALPHA,RU,PROMAS,PROVIS,MATMA,
     &                  NUMGRP,NUOR,FREQ,MASG,FACT,FACPAR,VITE,XINT,
     &                  YINT,RINT,Z,PHIX,PHIY,DEFM,
     &                  ITYPG,ZG,HG,DG,TG,CDG,CPG,RUGG, BASE )
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER      NDIM(14),NUMGRP(*),NUOR(*)
      REAL*8       FACT(*), FACPAR(*), RTBLOC,JEVTBL
      REAL*8       SOM(9),ALPHA,RU,MATMA(*),FREQ(*),MASG(*),VITE(*)
      REAL*8       XINT(*),YINT(*),RINT(*),Z(*),PHIX(*),PHIY(*),DEFM(*)
      CHARACTER*8  PROMAS, PROVIS, BASE
      CHARACTER*19 MELFLU
      INTEGER      ITYPG(*)
      REAL*8       ZG(*),HG(*),DG(*),TG(*),CDG(*),CPG(*),RUGG(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C TOLE CRP_20
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
C TOLE CRP_21
C ----------------------------------------------------------------------
C     AFFECTATION
C     OPERATEUR APPELANT : OP0144 , FLUST3
C ----------------------------------------------------------------------
C     OPTION DE CALCUL   : CALC_FLUI_STRU , CALCUL DES PARAMETRES DE
C     COUPLAGE FLUIDE-STRUCTURE POUR UNE CONFIGURATION DE TYPE "FAISCEAU
C     DE TUBES SOUS ECOULEMENT AXIAL"
C ----------------------------------------------------------------------
C IN  : MELFLU : NOM DU CONCEPT DE TYPE MELASFLU PRODUIT
C IN  : NDIM   : TABLEAU DES DIMENSIONS
C IN  : SOM    : COORDONNEES DES SOMMETS DE L'ENCEINTE RECTANGULAIRE
C                OU XEXT,YEXT,REXT
C IN  : ALPHA  : COEFFICIENT DE PROPORTIONALITE DE LA PESENTEUR PAR
C                RAPPORT A LA VALEUR STANDARD (9.81). LA PROJECTION DU
C                VECTEUR V SUIVANT Z VAUT 9.81*ALPHA.
C IN  : RU     : RUGOSITE DES CYLINDRES
C IN  : PROMAS : PROFIL DE MASSE VOLUMIQUE DU FLUIDE, DE TYPE FONCTION
C IN  : PROVIS : PROFIL DE VISCOSITE DU FLUIDE, DE TYPE FONCTION
C IN  : MATMA  : VECTEUR CONTENANT LES MATRICES MODALES, MASSE,RIGIDITE,
C                AMORTISSEMENT
C IN  : NUMGRP : INDICES DES GROUPES D EQUIVALENCE
C IN  : NUOR   : LISTE DES NUMEROS D'ORDRE DES MODES SELECTIONNES POUR
C                LE COUPLAGE (PRIS DANS LE CONCEPT MODE_MECA)
C IN  : FREQ   : FREQUENCES ET AMORTISSEMENTS REDUITS MODAUX PERTURBES
C                PAR L'ECOULEMENT
C IN  : MASG   : MASSES GENERALISEES DES MODES PERTURBES, SUIVANT LA
C                DIRECTION CHOISIE PAR L'UTILISATEUR
C IN  : VITE   : LISTE DES VITESSES D'ECOULEMENT ETUDIEES
C IN  : XINT   : COORDONNEES 'X' DES CENTRES DES CYLINDRES DANS
C                LE REPERE AXIAL
C IN  : YINT   : COORDONNEES 'Y' DES CENTRES DES CYLINDRES DANS
C                LE REPERE AXIAL
C IN  : RINT   : RAYONS DES CYLINDRES
C IN  : Z      : COORDONNEES 'Z'  DES DES POINTS DE DISCRETISATION DANS
C                LE REPERE AXIAL
C IN  : PHIX   : DEFORMEES MODALES INTERPOLEES DANS LE REPERE AXIAL
C IN  : PHIY   : DEFORMEES MODALES INTERPOLEES DANS LE REPERE AXIAL
C IN  : DEFM   : DEFORMEES MODALES DANS LE REPERE PHYSIQUE
C
C IN  : ITYPG  : VECTEUR DES TYPES DE GRILLE
C IN  : ZG     : COORDONNEES 'Z' DES POINTS DE DISCRETISATION
C                DES GRILLES
C IN  : HG     : LONGUEURS, DANS LA DIRECTION AXIALE, DE CHAQUE TYPE
C                DE GRILLE
C IN  : DG     : LARGEURS (OU COTES), DANS LE PLAN PERPENDICULAIRE
C                A L'AXE DU FAISCEAU, DE CHAQUE TYPE DE GRILLE
C IN  : TG     : EPAISSEURS, DANS LE PLAN PERPENDICULAIRE A L'AXE
C                DU FAISCEAU, DE CHAQUE TYPE DE GRILLE
C IN  : CDG    : COEFFICIENTS DE TRAINEE DE CHAQUE TYPE DE GRILLE
C IN  : CPG    : PENTES DU COEFFICIENT DE PORTANCE DE CHAQUE TYPE
C                DE GRILLE
C IN  : RUGG   : RUGOSITES DE CHAQUE TYPE DE GRILLE
C  IN : BASE   : NOM DU CONCEPT DE TYPE MODE_MECA DEFINISSANT LA BASE
C                MODALE DU SYSTEME AVANT PRISE EN COMPTE DU COUPLAGE
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C ----------------------------------------------------------------------
      INTEGER       I, J, NBPARA, IRET
      PARAMETER    ( NBPARA = 5 )
      COMPLEX*16    C16B
      CHARACTER*8   TYPARA(NBPARA)
      CHARACTER*14  NUGENE
      CHARACTER*24  NOOBJ
      CHARACTER*19  RIGGEN, MASGEN, AMOGEN, VALEK(3)
      CHARACTER*16  NOPARA(NBPARA)
      CHARACTER*19  NOMT19
      CHARACTER*24  NOMCHA
C
      DATA NOPARA / 'NUME_VITE', 'VITE_FLUI',
     &              'MATR_MASS', 'MATR_AMOR', 'MATR_RIGI' /
      DATA TYPARA / 'I', 'R', 'K24', 'K24', 'K24' /
C ----------------------------------------------------------------------
      CALL JEMARQ()
C
C --- LECTURE DES DIMENSIONS
      NBZ    = NDIM(1)
      NBMOD  = NDIM(2)
      NBCYL  = NDIM(3)
      NBGRP  = NDIM(4)
      IENCEI = NDIM(6)
      NIMA   = NDIM(7)
      NIMA2  = NDIM(8)
      NBV    = NDIM(9)
      NBNOE  = NDIM(11)
      NBTOT  = NBCYL*(2*NIMA+1)*(2*NIMA+1)
      NBFIN  = NBTOT + 4*(NIMA2)*(NIMA2+2*NIMA+1)
      NTYPG  = NDIM(13)
      NBGTOT = NDIM(14)
C
C --- ON CREE UN NUMME_DDL_GENE POUR STOCKER LES MATRICES
C

C     DETERMINATION DU NOM DE LA SD CACHEE NUME_DDL_GENE
      NOOBJ ='12345678.NU000.NUME.PRNO'
      CALL GNOMSD(NOOBJ,12,14)
      NUGENE=NOOBJ(1:14)

C     -- FABRICATION DU STOCKAGE MORSE :
      CALL NUMMO1 ( NUGENE, BASE, NBMOD, 'PLEIN' )

C     -- FABRICATION DU STOCKAGE LIGN_CIEL :
      RTBLOC=JEVTBL()
      CALL SMOSLI(NUGENE//'.SMOS',NUGENE//'.SLCS','G',RTBLOC)

C     -- SOLVEUR PAR DEFAUT :
      CALL CRNSLV(NUGENE,'LDLT','SANS','G')
C
C --- ON GREFFE UNE STRUCTURE TABLE AU CONCEPT "MELFLU" POUR
C     STOCKER LES MATRICES CREEES
C
      NOMT19 = ' '
      CALL JEEXIN ( MELFLU//'.LTNT', IRET )
      IF ( IRET .NE. 0 ) THEN
         CALL LTNOTB ( MELFLU , 'MATR_GENE' , NOMT19 )
         CALL DETRSD ( 'TABLE' , NOMT19 )
      ELSE
         CALL LTCRSD ( MELFLU , 'G' )
      ENDIF
      CALL LTNOTB ( MELFLU , 'MATR_GENE' , NOMT19 )
C
      CALL JEEXIN ( NOMT19//'.TBBA', IRET )
      IF ( IRET .NE. 0 )  CALL DETRSD ( 'TABLE' , NOMT19 )
C
      CALL TBCRSD ( NOMT19, 'G' )
      CALL TBAJPA ( NOMT19, NBPARA, NOPARA, TYPARA )
C
C --- TABLEAUX DE TRAVAIL - ALLOCATION MEMOIRE
      CALL WKVECT('&&MEFIST.TMP.PST','V V R',NBZ*5,IPST)
      IDPST = IPST + NBZ
      ICP = IDPST + NBZ
      ICF = ICP + NBZ
      IRE = ICF + NBZ
C
      NN = NBCYL*(2+2*NBCYL+8*NBGRP) + 4*NBGRP
      CALL WKVECT('&&MEFIST.TMP.DIV','V V R',NN,IDCENT)
      IFICEN = IDCENT + NBCYL
      ID = IFICEN + NBCYL
      IFI = ID + NBCYL*NBCYL
      IPPXX = IFI + NBCYL*NBCYL
      IPPXY = IPPXX + NBCYL*NBGRP
      IPPYX = IPPXY + NBCYL*NBGRP
      IPPYY = IPPYX + NBCYL*NBGRP
      IVNXX = IPPYY + NBCYL*NBGRP
      IVNXY = IVNXX + NBCYL*NBGRP
      IVNYX = IVNXY + NBCYL*NBGRP
      IVNYY = IVNYX + NBCYL*NBGRP
      ITMP  = IVNYY + NBCYL*NBGRP
C
      CALL WKVECT('&&MEFIST.TMP.BET','V V R',NBFIN,IBETA)
      CALL WKVECT('&&MEFIST.TMP.SGN','V V I',NBFIN*2,ISGN)
      IORIG = ISGN + NBFIN
C
      CALL WKVECT('&&MEFIST.TMP.VIT','V V R',(NBZ+1)*4,IVIT)
      IRHO = IVIT + NBZ + 1
      IVISC = IRHO + NBZ + 1
      IDVIT = IVISC + NBZ + 1
C
      CALL WKVECT('&&MEFIST.TMP.VVV','V V R',NBMOD,IVEC)
      CALL WKVECT('&&MEFIST.TMP.MAT','V V R',3*NBMOD*NBMOD,IMATM)
      IMATR = IMATM + NBMOD*NBMOD
      IMATA = IMATR + NBMOD*NBMOD
C
      NN = 2*NBMOD*(7+5*2*NBMOD)
      CALL WKVECT('&&MEFIST.TMP.VEC','V V R',NN,IREEL)
      IIMAG = IREEL + 2*NBMOD
      IALFR = IIMAG + 2*NBMOD
      IALFI = IALFR + 2*NBMOD
      IWCT  = IALFI + 2*NBMOD
      IMAT1 = IWCT  + 4*NBMOD
      IMAT2 = IMAT1 + 2*NBMOD*2*NBMOD
      IMATV = IMAT2 + 2*NBMOD*2*NBMOD
      IMATC = IMATV + 2*NBMOD*2*NBMOD
C
      CALL WKVECT('&&MEFIST.TMP.IND','V V I',NBMOD*2,IIND)
      CALL WKVECT('&&MEFIST.TMP.FRE','V V R',NBMOD*2,IFRE)
      IKSI = IFRE + NBMOD
C
      IF(NTYPG.NE.0) THEN
        CALL WKVECT('&&MEFIST.TMP.COEFG','V V R',4*NBGTOT,ICFG)
        IRHOG  = ICFG + NBGTOT
        IVISCG = IRHOG + NBGTOT
        IVITG  = IVISCG + NBGTOT
        CALL WKVECT('&&MEFIST.TMP.SECTG','V V R',2*NTYPG+2,IAXG)
        IXIG  = IAXG + NTYPG
        IAFLU = IXIG + NTYPG
        IPM   = IAFLU + 1
      ELSE
        ICFG   = 1
        IRHOG  = 1
        IVISCG = 1
        IVITG  = 1
        IAXG   = 1
        IXIG   = 1
        IAFLU  = 1
        IPM    = 1
      ENDIF
C
C
C --- ENCEINTE CIRCULAIRE
C --- CALCUL DES COORDONNEES POLAIRES ABSOLUES ET RELATIVES DES CENTRES
C --- DES CYLINDRES ET VERIFICATION DE L INCLUSION DES FAISCEAUX DANS
C --- L ENCEINTE
      IF(IENCEI.EQ.1) THEN
          CALL MEFVER(NDIM,SOM,XINT,YINT,RINT)
          CALL MEFGEC(NDIM,NBCYL,SOM,XINT,YINT,RINT,ZR(IDCENT),
     &                ZR(IFICEN),ZR(ID),ZR(IFI))
C
C --- ENCEINTE RECTANGULAIRE
C --- VERIFICATION DE L'ORDRE ET DE LA BONNE DISPOSITION DES SOMMETS DE
C --- L ENCEINTE, ET MISE EN FORME DES DONNEES POUR LA PRISE EN COMPTE
C --- DES CONDITIONS AUX LIMITES PAR UNE METHODE DERIVEE DE LA METHODE
C --- DES IMAGES
      ELSE IF(IENCEI.EQ.2) THEN
          CALL MEFVER(NDIM,SOM,XINT,YINT,RINT)
          CALL MEFGER(NDIM,SOM,XINT,YINT,RINT,ZI(ISGN),ZI(IORIG),
     &                ZR(IBETA))
C ---
      ELSE
          CALL U2MESS('F','ALGELINE_85')
      ENDIF
C
C --- CALCUL DES COEFFICIENTS INTERVENANT DANS L EXPRESSION DES
C --- FORCES DE PRESSION PERTURBEE, ET DES FORCES NORMALES DE
C --- FROTTEMENTS SUR CHAQUE CYLINDRES
C
      IF(IENCEI.EQ.1) THEN
         CALL MEFCIR(NDIM,NBCYL,NBGRP,NUMGRP,SOM,RINT,ZR(IDCENT),
     &               ZR(IFICEN),ZR(ID),ZR(IFI),
     &               ZR(IPPXX),ZR(IPPXY),ZR(IPPYX),ZR(IPPYY),
     &               ZR(IVNXX),ZR(IVNXY),ZR(IVNYX),ZR(IVNYY),
     &               ZR(ITMP))
      ELSE IF(IENCEI.EQ.2.OR.IENCEI.EQ.0) THEN
         CALL MEFREC(NDIM,NBCYL,NBGRP,NUMGRP,XINT,YINT,RINT,
     &               ZI(ISGN),ZI(IORIG),ZR(IBETA),
     &               ZR(IPPXX),ZR(IPPXY),ZR(IPPYX),ZR(IPPYY),
     &               ZR(IVNXX),ZR(IVNXY),ZR(IVNYX),ZR(IVNYY),
     &               ZR(ITMP))
      ENDIF
C
C
      IFM = IUNIFI('MESSAGE')
C
C
C --- CALCUL DES CARACTERISTIQUES MODALES EN FLUIDE AU REPOS
C
      NV0 = 0
      DO 10 NV = 1, NBV
         IF (VITE(NV).EQ.0.0D0) THEN
            NV0 = NV
            GOTO 11
         ENDIF
  10  CONTINUE
  11  CONTINUE
C
      VIT0 = 0.0D0
      WRITE (IFM,6001) '<MEFIST> TRAITEMENT DE LA VITESSE D '
     &   ,'ECOULEMENT: VIT0 = ',VIT0,' M/S'
C
C.....CALCUL DU DIAMETRE HYDRAULIQUE, ET DES NOMBRES DE REYNOLDS
      CALL MEFROT(NDIM,SOM,VIT0,PROMAS,PROVIS,Z,RU,RINT,ZR(IRE),
     &            ZR(ICP),ZR(ICF),DH,ZR(IVIT),ZR(IRHO),ZR(IVISC),
     &            ITYPG,ZG,TG,DG,RUGG,ZR(IAXG),ZR(IXIG),ZR(IAFLU),
     &            ZR(IPM),ZR(ICFG),ZR(IVITG),ZR(IRHOG),ZR(IVISCG))
      SOM(9) = DH
C
C.....CALCUL DE LA PRESSION ET DU GRADIENT DE PRESSION STATIONNAIRE
      CALL MEFPRE(NDIM,ALPHA,Z,ZR(ICF),DH,ZR(IVIT+1),ZR(IRHO+1),
     &            ZR(IPST),ZR(IDPST),ZR(IDVIT),
     &            ITYPG,ZG,HG,ZR(IAXG),ZR(IPM),ZR(IXIG),ZR(IAFLU),
     &            CDG,ZR(ICFG),ZR(IVITG),ZR(IRHOG))
C
C.....CALCUL DES MATRICES DE MASSE, DE RAIDEUR, D AMORTISSEMENT SOUS
C.....ECOULEMENT (PROJECTION DES EFFORTS FLUIDES SUR BASE MODALE EN
C.....AIR)
      CALL MEFMAT(NDIM,NUMGRP,NBZ,NBGRP,NBMOD,MATMA,ZR(IDCENT),ZR(ICP),
     &            ZR(ICF),ZR(IVIT),ZR(IRHO),ZR(IPST),
     &            ZR(IDPST),RINT,PHIX,PHIY,Z,ZR(IMATM),ZR(IMATR),
     &            ZR(IMATA),ITYPG,ZR(IAXG),ZG,ZR(IRHOG),ZR(IVITG),
     &            CDG,CPG)
C
C.....RESOLUTION DU PROBLEME GENERALISE SOUS ECOULEMENT - CALCUL DES
C.....VALEURS ET VECTEURS PROPRES
      CALL MEFEIG(NDIM,NBMOD,ZR(IMATM),ZR(IMATR),ZR(IMATA),ZR(IFRE),
     &            ZR(IKSI),ZR(IMATV),ZR(IALFR),ZR(IALFI),ZR(IMAT1),
     &            ZR(IMAT2),ZR(IWCT),ZR(IMATC),ZI(IIND))
C
C.....PRISE EN COMPTE DE L'AMORTISSEMENT FLUIDE AU REPOS
      CALL MEFREP(NBZ,NBMOD,NBCYL,NBGRP,NUMGRP,Z,ZR(IFRE),ZR(IRHO+1),
     &            ZR(IVISC),RINT,PHIX,PHIY,ZR(IDCENT),MATMA)
      CALL MEFMAT(NDIM,NUMGRP,NBZ,NBGRP,NBMOD,MATMA,ZR(IDCENT),ZR(ICP),
     &            ZR(ICF),ZR(IVIT),ZR(IRHO),ZR(IPST),
     &            ZR(IDPST),RINT,PHIX,PHIY,Z,ZR(IMATM),ZR(IMATR),
     &            ZR(IMATA),ITYPG,ZR(IAXG),ZG,ZR(IRHOG),ZR(IVITG),
     &            CDG,CPG)
      CALL MEFEIG(NDIM,NBMOD,ZR(IMATM),ZR(IMATR),ZR(IMATA),ZR(IFRE),
     &            ZR(IKSI),ZR(IMATV),ZR(IALFR),ZR(IALFI),ZR(IMAT1),
     &            ZR(IMAT2),ZR(IWCT),ZR(IMATC),ZI(IIND))
C
C.....STOCKAGE DES RESULTATS EN FLUIDE AU REPOS LE CAS ECHEANT
C.....MATRICES DE MASSES GENERALISEES, FREQUENCE, AMORTISSEMENT
      IF (NV0.NE.0) THEN
C
         DO 20 N = 1, NBMOD
            NN = ZI(IIND+N-1)
            II = (NN-1)*2*NBMOD
            JJ = N + NBMOD*(NV0-1)
            KK = 3*(N-1) + NBMOD*(NV0-1)
            CALL PMAVEC('ZERO',NBMOD,ZR(IMATM),ZR(IMATV+II),ZR(IVEC))
            MASG(JJ) = DDOT(NBMOD,ZR(IMATV+II),1,ZR(IVEC),1)
            FACT(KK+1) = DDOT(NBMOD,ZR(IVEC),1,FACPAR,1)
            FACT(KK+2) = DDOT(NBMOD,ZR(IVEC),1,FACPAR(NBMOD+1),1)
            FACT(KK+3) = DDOT(NBMOD,ZR(IVEC),1,FACPAR(2*NBMOD+1),1)
  20     CONTINUE
C
         DO 30 J = 1, NBMOD
            IND = 2*NBMOD*(NV0-1) + 2*(J-1) +1
            FREQ(IND)   = ZR(IFRE+J-1)
            FREQ(IND+1) = ZR(IKSI+J-1)
  30     CONTINUE
C
C........STOCKAGE DES DEFORMEES MODALES APRES REPROJECTION SUR BASE
C........PHYSIQUE
         NOMCHA(1:13)   = MELFLU(1:8)//'.C01.'
         NOMCHA(20:24) = '.VALE'
         DO 40 K = 1, NBMOD
            KK = ZI(IIND+K-1)
            WRITE(NOMCHA(14:16),'(I3.3)') NUOR(K)
            WRITE(NOMCHA(17:19),'(I3.3)') NV0
            CALL JEVEUO(NOMCHA,'E',ICHAM)
            DO 41 J = 1, NBNOE*6
               IND = ICHAM+J-1
               ZR(IND) = 0.D0
               DO 42 M = 1, NBMOD
                  ZR(IND) = ZR(IND) + DEFM(J+(M-1)*NBNOE*6)
     &                    * ZR(IMATV+M-1+(KK-1)*2*NBMOD)
  42           CONTINUE
  41        CONTINUE
            CALL JELIBE(NOMCHA)
  40     CONTINUE
C
C........IMPRESSIONS
C
      WRITE (IFM,*)
      WRITE (IFM,*) '         RESULTAT MODULE COUPLAGE FLUIDE-STRUCTURE'
      WRITE (IFM,7001) '         VITESSE GAP(M/S) : ',VIT0
      WRITE (IFM,*) ' ************************************************'
     &        ,'*******************'
      WRITE (IFM,*) ' *                   *      FREQUENCES      *    '
     &     ,'                  *'
      WRITE (IFM,*) ' *        MODE       *         SOUS         *    '
     &     ,'AMORTISSEMENT     *'
      WRITE (IFM,*) ' *                   *    ECOULEMENT(HZ)    *    '
     &     ,'     ( % )        *'
      WRITE (IFM,*) ' ************************************************'
     &     ,'*******************'
      DO 50 I = 1, NBMOD
        WRITE (IFM,7002) ' *      ',NUOR(I),'         *    ',
     &           ZR(IFRE+I-1),'     *    ',100.D0*ZR(IKSI+I-1),'     *'
  50  CONTINUE
      WRITE (IFM,*) ' ************************************************'
     &     ,'*******************'
      WRITE (IFM,*)
C
      ENDIF
C
C --- FIN DU CALCUL DES CARACTERISTIQUES EN FLUIDE AU REPOS
C
C --- BOUCLE SUR LES VITESSES D'ECOULEMENT
C
      DO 100 NV = 1, NBV
C
         IF (NV.EQ.NV0) GOTO 100
C
         VIT0 = VITE(NV)
         WRITE (IFM,6001) '<MEFIST> TRAITEMENT DE LA VITESSE D '
     &   ,'ECOULEMENT: VIT0 = ',VIT0,' M/S'
C
C........CALCUL DU DIAMETRE HYDRAULIQUE, ET DES NOMBRES DE REYNOLDS
         CALL MEFROT(NDIM,SOM,VIT0,PROMAS,PROVIS,Z,RU,RINT,ZR(IRE),
     &               ZR(ICP),ZR(ICF),DH,ZR(IVIT),ZR(IRHO),ZR(IVISC),
     &               ITYPG,ZG,TG,DG,RUGG,ZR(IAXG),ZR(IXIG),ZR(IAFLU),
     &               ZR(IPM),ZR(ICFG),ZR(IVITG),ZR(IRHOG),ZR(IVISCG))
         SOM(9) = DH
C
C........CALCUL DE LA PRESSION ET DU GRADIENT DE PRESSION STATIONNAIRE
         CALL MEFPRE(NDIM,ALPHA,Z,ZR(ICF),DH,ZR(IVIT+1),ZR(IRHO+1),
     &               ZR(IPST),ZR(IDPST),ZR(IDVIT),
     &               ITYPG,ZG,HG,ZR(IAXG),ZR(IPM),ZR(IXIG),ZR(IAFLU),
     &               CDG,ZR(ICFG),ZR(IVITG),ZR(IRHOG))
C
C........CALCUL DES MATRICES DE MASSE, DE RAIDEUR, D AMORTISSEMENT SOUS
C........ECOULEMENT (PROJECTION DES EFFORTS FLUIDES SUR BASE MODALE EN
C........AIR)
         CALL MEFMAT(NDIM,NUMGRP,NBZ,NBGRP,NBMOD,MATMA,ZR(IDCENT),
     &               ZR(ICP),ZR(ICF),ZR(IVIT),ZR(IRHO),
     &               ZR(IPST),ZR(IDPST),RINT,PHIX,PHIY,Z,ZR(IMATM),
     &               ZR(IMATR),ZR(IMATA),ITYPG,ZR(IAXG),ZG,ZR(IRHOG),
     &               ZR(IVITG),CDG,CPG)
C
C........RESOLUTION DU PROBLEME GENERALISE SOUS ECOULEMENT - CALCUL DES
C........VALEURS ET VECTEURS PROPRES
         CALL MEFEIG(NDIM,NBMOD,ZR(IMATM),ZR(IMATR),ZR(IMATA),ZR(IFRE),
     &               ZR(IKSI),ZR(IMATV),ZR(IALFR),ZR(IALFI),ZR(IMAT1),
     &               ZR(IMAT2),ZR(IWCT),ZR(IMATC),ZI(IIND))
C
C........STOCKAGE DES RESULTATS POUR LA VITESSE D'ECOULEMENT COURANTE
C........MATRICES DE MASSES GENERALISEES, FREQUENCE, AMORTISSEMENT
         DO 60 N = 1,NBMOD
            NN = ZI(IIND+N-1)
            II = (NN-1)*2*NBMOD
            JJ = N + NBMOD*(NV-1)
            KK = 3*(N-1) + NBMOD*(NV-1)
            CALL PMAVEC('ZERO',NBMOD,ZR(IMATM),ZR(IMATV+II),ZR(IVEC))
            MASG(JJ) = DDOT(NBMOD,ZR(IMATV+II),1,ZR(IVEC),1)
            FACT(KK+1) = DDOT(NBMOD,ZR(IVEC),1,FACPAR,1)
            FACT(KK+2) = DDOT(NBMOD,ZR(IVEC),1,FACPAR(NBMOD+1),1)
            FACT(KK+3) = DDOT(NBMOD,ZR(IVEC),1,FACPAR(2*NBMOD+1),1)
  60     CONTINUE
C
         DO 70 J = 1,NBMOD
            IND = 2*NBMOD*(NV-1) + 2*(J-1) +1
            FREQ(IND)   = ZR(IFRE+J-1)
            FREQ(IND+1) = ZR(IKSI+J-1)
  70     CONTINUE
C
C........STOCKAGE DES DEFORMEES MODALES APRES REPROJECTION SUR BASE
C........PHYSIQUE
         NOMCHA(1:13)   = MELFLU(1:8)//'.C01.'
         NOMCHA(20:24) = '.VALE'
         DO 80 K = 1, NBMOD
            KK = ZI(IIND+K-1)
            WRITE(NOMCHA(14:16),'(I3.3)') NUOR(K)
            WRITE(NOMCHA(17:19),'(I3.3)') NV
            CALL JEVEUO(NOMCHA,'E',ICHAM)
            DO 81 J = 1, NBNOE*6
               IND = ICHAM+J-1
               ZR(IND) = 0.D0
               DO 82 M = 1, NBMOD
                  ZR(IND) = ZR(IND) + DEFM(J+(M-1)*NBNOE*6)
     &                    * ZR(IMATV+M-1+(KK-1)*2*NBMOD)
  82           CONTINUE
  81        CONTINUE
            CALL JELIBE(NOMCHA)
  80     CONTINUE
C
C........IMPRESSIONS
C
      WRITE (IFM,*)
      WRITE (IFM,*) '         RESULTAT MODULE COUPLAGE FLUIDE-STRUCTURE'
      WRITE (IFM,7001) '         VITESSE GAP(M/S) : ',VIT0
      WRITE (IFM,*) ' ************************************************'
     &        ,'*******************'
      WRITE (IFM,*) ' *                   *      FREQUENCES      *    '
     &     ,'                  *'
      WRITE (IFM,*) ' *        MODE       *         SOUS         *    '
     &     ,'AMORTISSEMENT     *'
      WRITE (IFM,*) ' *                   *    ECOULEMENT(HZ)    *    '
     &     ,'     ( % )        *'
      WRITE (IFM,*) ' ************************************************'
     &     ,'*******************'
      DO 90 I = 1, NBMOD
        WRITE (IFM,7002) ' *      ',NUOR(I),'         *    ',
     &           ZR(IFRE+I-1),'     *    ',100.D0*ZR(IKSI+I-1),'     *'
  90  CONTINUE
      WRITE (IFM,*) ' ************************************************'
     &     ,'*******************'
      WRITE (IFM,*)
C
C ------ ON STOCKE LES MATRICES  ZR(IMATM), ZR(IMATA), ZR(IMATR)
C
C        DETERMINATION DU NOM DES SD CACHEES MATR_ASSE_GENE
         NOOBJ ='12345678.RIGGEN0000.REFA'
         CALL GNOMSD(NOOBJ,16,19)
         RIGGEN=NOOBJ(1:19)

         NOOBJ ='12345678.MASGEN0000.REFA'
         CALL GNOMSD(NOOBJ,16,19)
         MASGEN=NOOBJ(1:19)

         NOOBJ ='12345678.AMOGEN0000.REFA'
         CALL GNOMSD(NOOBJ,16,19)
         AMOGEN=NOOBJ(1:19)

         VALEK(1) = MASGEN
         VALEK(2) = AMOGEN
         VALEK(3) = RIGGEN

         CALL MEFSMA ( ZR(IMATM), ZR(IMATA), ZR(IMATR), NUGENE,
     &                                         MASGEN, AMOGEN, RIGGEN )
C
         CALL TBAJLI ( NOMT19, NBPARA, NOPARA,
     &                                       NV, VIT0, C16B, VALEK, 0 )
C
C --- FIN DE BOUCLE SUR LES VITESSES D ECOULEMENT
 100  CONTINUE
C
C --- FORMATS D'IMPRESSION
C
 6001 FORMAT (1P,1X,A,A,D13.6,A)
 7001 FORMAT (1P,1X,A,D13.6)
 7002 FORMAT (1P,1X,A,I4,A,D13.6,A,D13.6,A)
C
      CALL JEDETC('V','&&MEFIST',1)
C
      CALL JEDEMA()
      END

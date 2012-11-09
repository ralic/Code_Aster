      SUBROUTINE SPECFF(CASINT,NOMU,SPECTR,BASE,NUOR,IMODI,IMODF,NBM,
     &                  NBPF)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C     PROJECTION D'UN SPECTRE D'EXCITATION TURBULENTE REPARTIE (AVEC
C     FONCTIONS DE FORME) SUR UNE BASE MODALE PERTURBEE PAR PRISE EN
C     COMPTE DU COUPLAGE FLUIDE STRUCTURE
C     APPELANT : OP0146 , OPERATEUR PROJ_SPEC_BASE
C-----------------------------------------------------------------------
C IN  : CASINT  : BOOLEEN, DONNE L'OPTION DE CALCUL
C       CASINT  = .TRUE.  => CALCUL DE TOUS LES INTERSPECTRES
C       CASINT  = .FALSE. => CALCUL DES AUTOSPECTRES UNIQUEMENT
C IN  : NOMU    : NOM UTILISATEUR
C IN  : SPECTR  : NOM DU CONCEPT SPECTRE
C IN  : BASE    : NOM DU CONCEPT MELASFLU
C IN  : NUOR    : NUMEROS D'ORDRE DES MODES DU CONCEPT MELASFLU
C IN  : IMODI   : INDICE DU PREMIER MODE PRIS EN COMPTE
C IN  : IMODF   : INDICE DU DERNIER MODE PRIS EN COMPTE
C IN  : NBM     : NOMBRE DE MODES DU CONCEPT MELASFLU
C IN  : NBPF    : NOMBRE DE POINTS DE LA DISCRETISATION FREQUENTIELLE
C
C     ------------------------------------------------------------------
C
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM
      LOGICAL      CASINT,LTABLE,EXIIND
      INTEGER      IMODI,IMODF,NBM,NUOR(NBM),NBPF,IVAL(2)
      CHARACTER*8  NOMU,K8B,CAELEM,MODELE,TABLE,NOMNOA,NOMA
      CHARACTER*19 SPECTR,BASE,TYPFLU
C
      INTEGER      IBID,DIM,DIMINT,MXVAL,ITAB,NBFREQ,NBVAL,IFREQ
      INTEGER      IRE,IIM,ISRE,ISIM,I1,IND,LNUMI,LNUMJ,IER2,IJ,IPROL
      INTEGER IAXE,ICHREF,IDEB,IDEBIT,IDEC,IDEFM,IDIAX
      INTEGER IDIFE,IDIFF,IFO,IFO1,IFO2,IFOI,IFSIC,IINTE,IL,IM1
      INTEGER IMATA,IMATB,INOFE,INOMF,INUNO,IREMF,IRET,IM1B,IM2,IM2B
      INTEGER ISPIN,ISPNO,ISPTE,ITYPFL,IV,IVAFF,IVALE
      INTEGER LWR,NBFONC,NBMR,NBN,NBNFEN,NBP,NBP1,NBP2
      REAL*8       R8B,LONG,MODULE,PLA180(21),PLA300(21),DET
      REAL*8 BETA11 ,BETA12 ,BETA21 ,BETA22 ,PHASE ,PTF ,S0
      CHARACTER*24 SPVAIN,SPVATE,SPNNOE
      CHARACTER*24 REMF,FSIC,CHREFE,CHVALE,CHNUMI,CHNUMJ,CHTAB
C
      DATA PLA180 / 8.D-1  , 1.D0   , 1.3D0  , 1.5D-1 , 2.D-1  ,
     &              4.D-2  , 2.4D-1 , 3.4D-1 , 6.D-2  , 2.8D-1 ,
     &              1.D-1  , 2.D-2  , 3.D-2  , 1.3D-1 , 4.2D-1 ,
     &              1.D0   , 1.3D0  , 1.8D-1 , 6.1D-1 , 2.5D-1 ,
     &              2.6D0  /
C
      DATA PLA300 / 1.1D0  , 1.6D0  , 2.6D0  , 1.8D-1 , 3.2D-1 ,
     &              4.D-2  , 1.6D-1 , 2.7D-1 , 4.D-2  , 1.D-1  ,
     &              4.9D-1 , 8.3D-1 , 3.D-2  , 7.D-2  , 8.8D-1 ,
     &              1.7D0  , 3.D0   , 3.D-1  , 3.2D-1 , 1.D0   ,
     &              4.5D0  /
C
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- 1.TEST DE COMPATIBILITE TYPE DE SPECTRE/CONFIGURATION ETUDIEE ---
C
      REMF = BASE//'.REMF'
      CALL JEVEUO(REMF,'L',IREMF)
      TYPFLU = ZK8(IREMF)
      FSIC = TYPFLU//'.FSIC'
      CALL JEVEUO(FSIC,'L',IFSIC)
      ITYPFL = ZI(IFSIC)
      IF (ITYPFL.NE.2) THEN
        CALL U2MESS('F','MODELISA7_4')
      ENDIF
C
C --- 2.RECUPERATION DES INFORMATIONS CARACTERISTIQUES DU SPECTRE ---
C
      SPVAIN = SPECTR//'.VAIN'
      SPVATE = SPECTR//'.VATE'
      SPNNOE = SPECTR//'.NNOE'
C
      CALL JEVEUO(SPVAIN,'L',ISPIN)
      LTABLE = .FALSE.
      IF (ZI(ISPIN+1).EQ.0) LTABLE = .TRUE.
C
      CALL JEVEUO(SPVATE,'L',ISPTE)
      CAELEM = ZK16(ISPTE+1)(1:8)
      MODELE = ZK16(ISPTE+2)(1:8)
      IF ( LTABLE ) THEN
        TABLE = ZK16(ISPTE+3)(1:8)
        CHNUMI = TABLE//'.NUMI'
        CHNUMJ = TABLE//'.NUMJ'
        CALL JEVEUO(CHNUMI,'L',LNUMI)
        CALL JEVEUO(CHNUMJ,'L',LNUMJ)
        CALL JELIRA(CHNUMI,'LONMAX',MXVAL,K8B)
        R8B = (-1.D0+SQRT(1.D0+8.D0*MXVAL))/2.D0
        NBFONC = INT(R8B)
        CALL WKVECT('&&SPECFF.TEMP.NOMF','V V K8',NBFONC,INOMF)
        DO 10 IFO = 1,NBFONC
          ZK8(INOMF+IFO-1) = ZK16(ISPTE+3+IFO)(1:8)
  10    CONTINUE
      ELSE
        NBFONC = 12
        DIMINT = 6
        READ(ZK16(ISPTE+4)(7:9),'(I3)') IDEBIT
      ENDIF
C
      CALL JEVEUO(SPNNOE,'L',ISPNO)
      NOMNOA = ZK8(ISPNO)
C
C --- 3.RECUPERATION DU NOM DU CONCEPT MAILLAGE ---
C
      IV = 1
      WRITE(CHREFE,'(A8,A5,2I3.3,A5)') BASE(1:8),'.C01.',NUOR(1),IV,
     &                                 '.REFE'
      CALL JEVEUO(CHREFE,'L',ICHREF)
      NOMA = ZK24(ICHREF)(1:8)
C
C --- 4.DISCRETISATION DES FONCTIONS DE FORME ---
C
      IF ( LTABLE ) THEN
        CALL VERIFF(NBFONC,ZK8(INOMF),NBP1,NBP2,LONG)
        NBP = NBP1 + NBP2
        CALL WKVECT('&&SPECFF.TEMP.DIFF','V V R',NBP,IDIFF)
        CALL WKVECT('&&SPECFF.TEMP.VAFF','V V R',NBP*NBFONC,IVAFF)
        CALL DISCFF(NBFONC,ZK8(INOMF),NBP1,NBP2,ZR(IDIFF),ZR(IVAFF))
      ELSE
        NBP1 = 101
        NBP2 = NBP1
        NBP  = NBP1 + NBP2
        LONG = 0.971D0
        CALL WKVECT('&&SPECFF.TEMP.DIFF','V V R',NBP,IDIFF)
        CALL WKVECT('&&SPECFF.TEMP.VAFF','V V R',NBP*NBFONC,IVAFF)
        CALL FFGRA1(NBFONC,IDEBIT,NBP1,NBP2,LONG,ZR(IDIFF),ZR(IVAFF))
      ENDIF
C
C --- 5.CARACTERISATION DU MAILLAGE DE LA POUTRE ---
C
C --- 5.1.DETERMINATION DE L'AXE DIRECTEUR DE LA POUTRE
C
      CALL AXDIPO(NOMA,CAELEM,MODELE,IAXE)
C
C --- 5.2.CREATION D'UNE LISTE ORDONNEE DE NOEUDS : ORDRE CROISSANT
C ---     DU PARAMETRE LE LONG DE L'AXE DIRECTEUR DE LA POUTRE
C
      CALL JELIRA(NOMA//'.NOMNOE','NOMUTI',NBN,K8B)
      IF (NBN.LT.3) THEN
        CALL U2MESS('F','MODELISA7_6')
      ENDIF
      CALL WKVECT('&&SPECFF.TEMP.NUNO','V V I',NBN,INUNO)
      CALL WKVECT('&&SPECFF.TEMP.DIAX','V V R',NBN,IDIAX)
      CALL DISCAX(NOMA,NBN,IAXE,ZI(INUNO),ZR(IDIAX))
C
C --- 6.EXTRACTION DE LA FENETRE EXCITEE SUR LE MAILLAGE : LISTE DES ---
C ---   NOEUDS ET DISCRETISATION SPATIALE CORRESPONDANTE, TRANSLATEE ---
C ---   POUR ETRE SUPERPOSEE A CELLE DES FONCTIONS DE FORME          ---
C
      CALL WKVECT('&&SPECFF.TEMP.NOFE','V V I',NBN,INOFE)
      CALL WKVECT('&&SPECFF.TEMP.DIFE','V V R',NBN,IDIFE)
      CALL FENEXC(NOMA,NOMNOA,LONG,NBN,ZI(INUNO),ZR(IDIAX),
     &            NBNFEN,ZI(INOFE),ZR(IDIFE))
C
C --- 7.EXTRACTION DES COMPOSANTES DES DEFORMEES MODALES SUR LA ---
C ---   FENETRE EXCITEE, DANS LES DEUX DIRECTIONS ORTHOGONALES  ---
C ---   A LA POUTRE                                             ---
C
      NBMR = IMODF - IMODI + 1
      CALL WKVECT('&&SPECFF.TEMP.DEFM','V V R',NBP*NBMR,IDEFM)
      CALL DEFFEN(BASE,NUOR,IMODI,NBMR,NBM,IAXE,LONG,NBNFEN,ZI(INOFE),
     &            ZR(IDIFE),NBP1,NBP2,ZR(IDIFF),ZR(IDEFM))
C
C --- 8.CALCUL DE LA DECOMPOSITION DES DEFORMEES MODALES SUR LA ---
C ---   FAMILLE DES FONCTIONS DE FORME ASSOCIEES A L'EXCITATION ---
C
      CALL WKVECT('&&SPECFF.TEMP.MATA','V V R',NBFONC*NBFONC,IMATA)
      CALL WKVECT('&&SPECFF.TEMP.MATB','V V R',NBFONC*NBMR,IMATB)
      CALL SCALFF(NBFONC,NBP,ZR(IDIFF),ZR(IVAFF),ZR(IMATA))
      CALL SCALDF(NBFONC,NBP,NBMR,ZR(IDIFF),ZR(IVAFF),ZR(IDEFM),
     &            ZR(IMATB))
      CALL MGAUSS('NFVP',ZR(IMATA),ZR(IMATB),NBFONC,NBFONC,NBMR,
     &            DET,IRET)
C
C --- 9.CALCUL DES INTERSPECTRES D'EXCITATIONS MODALES ---
C ---   BOUCLE SUR LE NOMBRE DE VITESSES               ---
C
      CALL WKVECT('&&SPECFF.TEMP.SWR ','V V R',NBPF,LWR)
      IF ( LTABLE ) THEN
         DIM = NBFONC*(NBFONC+1)/2
      ELSE
         DIM = DIMINT*(DIMINT+1)/2
      ENDIF
      DIM = 2*NBPF*DIM
      CALL WKVECT('&&SPECFF.TEMP.INTE','V V R',DIM,IINTE)
C
C --- 9.1.RECUPERATION DE LA DISCRETISATION FREQUENTIELLE
        CALL JEVEUO(NOMU//'.FREQ','L',LWR)
C
C --- 9.2.INTERPOLATION DES INTERSPECTRES A PROJETER
C
        IF ( LTABLE ) THEN
          CALL WKVECT('&&SPECFF.PROL','V V K24',5,IPROL)
          ZK24(IPROL)   = 'FONCTION'
          ZK24(IPROL+1) = 'LIN LIN '
          ZK24(IPROL+2) =  ' '
          ZK24(IPROL+3) = 'TOUTRESU'
          ZK24(IPROL+4) = 'CC      '
          CALL WKVECT('&&SPECFF.IRE','V V R',NBPF,IRE)
          CALL WKVECT('&&SPECFF.IIM','V V R',NBPF,IIM)
          CHTAB=TABLE//'.VALE'
          CALL JEVEUO(TABLE//'.FREQ','L',IFREQ)
          DO 60 IFO2 = 1,NBFONC
            IVAL(2) = IFO2
            DO 61 IFO1 = 1,IFO2
              IFO = IFO2*(IFO2-1)/2 + IFO1
              IVAL(1) = IFO1
              EXIIND = .FALSE.
              DO 320 I1=1,MXVAL
                IF ((ZI(LNUMI-1+I1) .EQ. IVAL(1)) .AND.
     &            (ZI(LNUMJ-1+I1) .EQ. IVAL(2))) THEN
                  EXIIND = .TRUE.
                  IND = I1
                ENDIF
320           CONTINUE
              IF (.NOT. EXIIND) CALL U2MESS('F','MODELISA2_89')
              CALL JEVEUO(JEXNUM(CHTAB,IND),'L',ITAB)
              CALL JELIRA(JEXNUM(CHTAB,IND),'LONMAX',NBVAL,K8B)
              IF (IFO2 .EQ. IFO1) THEN
                NBFREQ = NBVAL
                CALL JEEXIN('&&SPECFF.SRE',IBID)
                IF (IBID .EQ. 0) THEN
                  CALL WKVECT('&&SPECFF.SRE','V V R',NBFREQ,ISRE)
                  CALL WKVECT('&&SPECFF.SIM','V V R',NBFREQ,ISIM)
                ENDIF
                CALL FOINTR(' ',ZK24(IPROL),NBFREQ,ZR(IFREQ),ZR(ITAB),
     &              NBPF,ZR(LWR),ZR(ISRE),IER2)
                DO 210 I1 = 1,NBFREQ
                  ZR(ISIM-1+I1) = 0.D0
210             CONTINUE
              ELSE
                NBFREQ = NBVAL/2
                CALL JEEXIN('&&SPECFF.SRE',IBID)
                IF (IBID .EQ. 0) THEN
                  CALL WKVECT('&&SPECFF.SRE','V V R',NBFREQ,ISRE)
                  CALL WKVECT('&&SPECFF.SIM','V V R',NBFREQ,ISIM)
                ENDIF
                DO 310 I1 = 1,NBFREQ
                  ZR(IRE-1+I1) = ZR(ITAB+2*(I1-1))
                  ZR(IIM-1+I1) = ZR(ITAB+2*(I1-1)+1)
310             CONTINUE
                CALL FOINTR(' ',ZK24(IPROL),NBFREQ,ZR(IFREQ),ZR(IRE),
     &              NBPF,ZR(LWR),ZR(ISRE),IER2)
                CALL FOINTR(' ',ZK24(IPROL),NBFREQ,ZR(IFREQ),ZR(IIM),
     &              NBPF,ZR(LWR),ZR(ISIM),IER2)
              ENDIF
              DO 62 IL = 1,NBPF
                IDEC = 2*NBPF*(IFO-1)+2*(IL-1)
                ZR(IINTE+IDEC  ) = ZR(ISRE-1+IL)
                ZR(IINTE+IDEC+1) = ZR(ISIM-1+IL)
  62          CONTINUE
  61        CONTINUE
  60      CONTINUE
C
        ELSE IF (IDEBIT.EQ.180) THEN
C
          DO 70 IFO2 = 1,DIMINT
            IFO = IFO2*(IFO2+1)/2
            S0 = PLA180(IFO)
            DO 71 IL = 1,NBPF
              IDEC = 2*NBPF*(IFO-1)+2*(IL-1)
              PTF = ZR(LWR+IL-1)
              MODULE = 1.D0 + (PTF/15.D0)**(4.6D0)
              MODULE = S0/MODULE
              ZR(IINTE+IDEC) = MODULE
  71        CONTINUE
  70      CONTINUE
          DO 72 IFO2 = 2,DIMINT
            DO 73 IFO1 = 1,IFO2-1
              IFO = IFO2*(IFO2-1)/2 + IFO1
              S0 = PLA180(IFO)
              IFOI = (IFO2-1)*(IFO2-2)/2 + IFO1
              DO 74 IL = 1,NBPF
                IDEC = 2*NBPF*(IFO-1)+2*(IL-1)
                PTF = ZR(LWR+IL-1)
                CALL PHA180(IFOI,PTF,PHASE)
                MODULE = 1.D0 + (PTF/15.D0)**(4.6D0)
                MODULE = S0/MODULE
                ZR(IINTE+IDEC)   = MODULE * DBLE(COS(PHASE))
                ZR(IINTE+IDEC+1) = MODULE * DBLE(SIN(PHASE))
  74          CONTINUE
  73        CONTINUE
  72      CONTINUE
C
        ELSE IF (IDEBIT.EQ.300) THEN
C
          DO 80 IFO2 = 1,DIMINT
            IFO = IFO2*(IFO2+1)/2
            S0 = PLA300(IFO)
            DO 81 IL = 1,NBPF
              IDEC = 2*NBPF*(IFO-1)+2*(IL-1)
              PTF = ZR(LWR+IL-1)
              MODULE = 1.D0 + (PTF/15.D0)**(4.6D0)
              MODULE = S0/MODULE
              ZR(IINTE+IDEC) = MODULE
  81        CONTINUE
  80      CONTINUE
          DO 82 IFO2 = 2,DIMINT
            DO 83 IFO1 = 1,IFO2-1
              IFO = IFO2*(IFO2-1)/2 + IFO1
              S0 = PLA300(IFO)
              IFOI = (IFO2-1)*(IFO2-2)/2 + IFO1
              DO 84 IL = 1,NBPF
                IDEC = 2*NBPF*(IFO-1)+2*(IL-1)
                PTF = ZR(LWR+IL-1)
                CALL PHA300(IFOI,PTF,PHASE)
                MODULE = 1.D0 + (PTF/15.D0)**(4.6D0)
                MODULE = S0/MODULE
                ZR(IINTE+IDEC)   = MODULE * DBLE(COS(PHASE))
                ZR(IINTE+IDEC+1) = MODULE * DBLE(SIN(PHASE))
  84          CONTINUE
  83        CONTINUE
  82      CONTINUE
C
        ENDIF
C
C --- 9.3.PROJECTION DES INTERSPECTRES
C
        IJ = 0
        CHVALE = NOMU//'.VALE'
        DO 90 IM2 = IMODI,IMODF
          IDEB = IM2
          IF (CASINT) IDEB = IMODI
          DO 91 IM1 = IDEB,IM2
            IJ = IJ + 1
            CALL JEVEUO(JEXNUM(CHVALE,IJ),'E',IVALE)
            CALL JELIRA(JEXNUM(CHVALE,IJ),'LONMAX',NBVAL,K8B)
            IM2B = IM2 - IMODI + 1
            IM1B = IM1 - IMODI + 1
C
            IF ( LTABLE ) THEN
              DO 100 IL = 1,NBPF
                DO 101 IFO2 = 1,NBFONC
                  BETA12 = ZR(IMATB+NBFONC*(IM1B-1)+IFO2-1)
                  BETA22 = ZR(IMATB+NBFONC*(IM2B-1)+IFO2-1)
                  IFO = IFO2*(IFO2+1)/2
                  IDEC = 2*NBPF*(IFO-1)+2*(IL-1)
                  IF (NBVAL .EQ. NBPF) THEN
                    ZR(IVALE+IL-1) = ZR(IVALE+IL-1)
     &                           + BETA12*BETA22 * ZR(IINTE+IDEC)
                  ELSE
                    ZR(IVALE+2*(IL-1)) = ZR(IVALE+2*(IL-1))
     &                               + BETA12*BETA22 * ZR(IINTE+IDEC)
                  ENDIF
 101            CONTINUE
                IF (NBFONC.GT.1) THEN
                  DO 102 IFO2 = 2,NBFONC
                    BETA12 = ZR(IMATB+NBFONC*(IM1B-1)+IFO2-1)
                    BETA22 = ZR(IMATB+NBFONC*(IM2B-1)+IFO2-1)
                    DO 103 IFO1 = 1,IFO2-1
                      BETA11 = ZR(IMATB+NBFONC*(IM1B-1)+IFO1-1)
                      BETA21 = ZR(IMATB+NBFONC*(IM2B-1)+IFO1-1)
                      IFO = IFO2*(IFO2-1)/2 + IFO1
                      IDEC = 2*NBPF*(IFO-1)+2*(IL-1)
                      IF (NBVAL .EQ. NBPF) THEN
                        ZR(IVALE+IL-1) =
     & ZR(IVALE+IL-1)+(BETA11*BETA22+BETA12*BETA21)*ZR(IINTE+IDEC)
                      ELSE
                        ZR(IVALE+2*(IL-1)) =
     & ZR(IVALE+2*(IL-1))+(BETA11*BETA22+BETA12*BETA21)*ZR(IINTE+IDEC)
                        ZR(IVALE+2*(IL-1)+1) = ZR(IVALE+2*(IL-1)+1) +
     & (BETA11*BETA22-BETA12*BETA21)*ZR(IINTE+IDEC+1)
                      ENDIF
 103                CONTINUE
 102              CONTINUE
                ENDIF
 100          CONTINUE
C
            ELSE
C
              DO 110 IL = 1,NBPF
                DO 111 IFO2 = 1,DIMINT
                  BETA12 = ZR(IMATB+NBFONC*(IM1B-1)+IFO2-1)
                  BETA22 = ZR(IMATB+NBFONC*(IM2B-1)+IFO2-1)
                  IFO = IFO2*(IFO2+1)/2
                  IDEC = 2*NBPF*(IFO-1)+2*(IL-1)
                  IF (NBVAL .EQ. NBPF) THEN
                    ZR(IVALE+IL-1) = ZR(IVALE+IL-1)
     &                           + BETA12*BETA22 * ZR(IINTE+IDEC)
                  ELSE
                    ZR(IVALE+2*(IL-1)) = ZR(IVALE+2*(IL-1))
     &                               + BETA12*BETA22 * ZR(IINTE+IDEC)
                  ENDIF
 111            CONTINUE
                DO 112 IFO2 = DIMINT+1,NBFONC
                  BETA12 = ZR(IMATB+NBFONC*(IM1B-1)+IFO2-1)
                  BETA22 = ZR(IMATB+NBFONC*(IM2B-1)+IFO2-1)
                  IFO = (IFO2-DIMINT)*(IFO2-DIMINT+1)/2
                  IDEC = 2*NBPF*(IFO-1)+2*(IL-1)
                  IF (NBVAL .EQ. NBPF) THEN
                    ZR(IVALE+IL-1) = ZR(IVALE+IL-1)
     &                             + BETA12*BETA22 * ZR(IINTE+IDEC)
                  ELSE
                    ZR(IVALE+2*(IL-1)) = ZR(IVALE+2*(IL-1))
     &                               + BETA12*BETA22 * ZR(IINTE+IDEC)
                  ENDIF
 112            CONTINUE
                DO 113 IFO2 = 2,DIMINT
                  BETA12 = ZR(IMATB+NBFONC*(IM1B-1)+IFO2-1)
                  BETA22 = ZR(IMATB+NBFONC*(IM2B-1)+IFO2-1)
                  DO 114 IFO1 = 1,IFO2-1
                    BETA11 = ZR(IMATB+NBFONC*(IM1B-1)+IFO1-1)
                    BETA21 = ZR(IMATB+NBFONC*(IM2B-1)+IFO1-1)
                    IFO = IFO2*(IFO2-1)/2 + IFO1
                    IDEC = 2*NBPF*(IFO-1)+2*(IL-1)
                    IF (NBVAL .EQ. NBPF) THEN
                      ZR(IVALE+IL-1) =
     & ZR(IVALE+IL-1)+(BETA11*BETA22+BETA12*BETA21)*ZR(IINTE+IDEC)
                    ELSE
                      ZR(IVALE+2*(IL-1)) =
     & ZR(IVALE+2*(IL-1))+(BETA11*BETA22+BETA12*BETA21)*ZR(IINTE+IDEC)
                      ZR(IVALE+2*(IL-1)+1) = ZR(IVALE+2*(IL-1)+1) +
     & (BETA11*BETA22-BETA12*BETA21)*ZR(IINTE+IDEC+1)
                    ENDIF
 114              CONTINUE
 113            CONTINUE
                DO 115 IFO2 = DIMINT+2,NBFONC
                  BETA12 = ZR(IMATB+NBFONC*(IM1B-1)+IFO2-1)
                  BETA22 = ZR(IMATB+NBFONC*(IM2B-1)+IFO2-1)
                  DO 116 IFO1 = DIMINT+1,IFO2-1
                    BETA11 = ZR(IMATB+NBFONC*(IM1B-1)+IFO1-1)
                    BETA21 = ZR(IMATB+NBFONC*(IM2B-1)+IFO1-1)
                    IFO = (IFO2-DIMINT)*(IFO2-DIMINT-1)/2 + IFO1-DIMINT
                    IDEC = 2*NBPF*(IFO-1)+2*(IL-1)
                    IF (NBVAL .EQ. NBPF) THEN
                      ZR(IVALE+IL-1) =
     & ZR(IVALE+IL-1)+(BETA11*BETA22+BETA12*BETA21)*ZR(IINTE+IDEC)
                    ELSE
                      ZR(IVALE+2*(IL-1)) =
     & ZR(IVALE+2*(IL-1))+(BETA11*BETA22+BETA12*BETA21)*ZR(IINTE+IDEC)
                      ZR(IVALE+2*(IL-1)+1) = ZR(IVALE+2*(IL-1)+1) +
     & (BETA11*BETA22-BETA12*BETA21)*ZR(IINTE+IDEC+1)
                    ENDIF
 116              CONTINUE
 115            CONTINUE
 110          CONTINUE
C
            ENDIF
C
  91      CONTINUE
  90    CONTINUE
C
      CALL JEDETR('&&SPECFF.TEMP.NOMF')
      CALL JEDETR('&&SPECFF.TEMP.DIFF')
      CALL JEDETR('&&SPECFF.TEMP.VAFF')
      CALL JEDETR('&&SPECFF.TEMP.NUNO')
      CALL JEDETR('&&SPECFF.TEMP.DIAX')
      CALL JEDETR('&&SPECFF.TEMP.NOFE')
      CALL JEDETR('&&SPECFF.TEMP.DIFE')
      CALL JEDETR('&&SPECFF.TEMP.DEFM')
      CALL JEDETR('&&SPECFF.TEMP.MATA')
      CALL JEDETR('&&SPECFF.TEMP.MATB')
      CALL JEDETR('&&SPECFF.TEMP.SWR ')
      CALL JEDETR('&&SPECFF.TEMP.INTE')
C
      CALL JEDEMA()
      END

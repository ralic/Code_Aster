      SUBROUTINE SPECFF(CASINT,NOMU,SPECTR,BASE,NUOR,IMODI,IMODF,NBM,
     +                  NBPF,NPV)
      IMPLICIT REAL*8 (A-H,O-Z)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 15/06/2005   AUTEUR VABHHTS J.PELLET 
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
C IN  : NPV     : NOMBRE DE VITESSES ETUDIEES
C
C     ------------------------------------------------------------------
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
C
      LOGICAL      CASINT
      INTEGER      IMODI,IMODF,NBM,NUOR(NBM),NBPF,NPV,IVAL(2)
      CHARACTER*8  NOMU
      CHARACTER*19 SPECTR,BASE
C
      LOGICAL      LTABLE,FAUX
      INTEGER      IBID,DIM,DIMINT
      REAL*8       R8B,LONG,MODULE,PLA180(21),PLA300(21),ZERO
      COMPLEX*16   C16B
      CHARACTER*8  K8B,CAELEM,MODELE,TABLE,NOMNOA,NOMA
      CHARACTER*16 NOPART(2)
      CHARACTER*19 TYPFLU,NOMFON
      CHARACTER*24 SPVAIN,SPVATE,SPNNOE
      CHARACTER*24 REMF,FSIC,CHREFE,VALE
C
      DATA NOPART / 'NUME_ORDRE_I' , 'NUME_ORDRE_J' /
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
      FAUX = .FALSE.
      ZERO = 0.D0
C
      CALL JEMARQ()
C
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
        CALL UTMESS('F','SPECFF','LE TYPE DE SPECTRE EST INCOMPATIBLE'//
     &              ' AVEC LA CONFIGURATION ETUDIEE')
      ENDIF
C
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
        CALL TBLIVA ( TABLE, 0, K8B, IBID, R8B, C16B, K8B, K8B, R8B,
     +              'DIMENSION', K8B, NBFONC, R8B, C16B, K8B, IRET )
        IF ( IRET .NE. 0 ) CALL UTMESS('F','SPECFF','Y A UN BUG 1' )
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
C
C --- 3.RECUPERATION DU NOM DU CONCEPT MAILLAGE ---
C
      IV = 1
      WRITE(CHREFE,'(A8,A5,2I3.3,A5)') BASE(1:8),'.C01.',NUOR(1),IV,
     &                                 '.REFE'
      CALL JEVEUO(CHREFE,'L',ICHREF)
      NOMA = ZK24(ICHREF)(1:8)
C
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
        CALL UTMESS('F','SPECFF','NOMBRE DE NOEUDS INSUFFISANT SUR LE'//
     &              ' MAILLAGE')
      ENDIF
      CALL WKVECT('&&SPECFF.TEMP.NUNO','V V I',NBN,INUNO)
      CALL WKVECT('&&SPECFF.TEMP.DIAX','V V R',NBN,IDIAX)
      CALL DISCAX(NOMA,NBN,IAXE,ZI(INUNO),ZR(IDIAX))
C
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
C
C --- 8.CALCUL DE LA DECOMPOSITION DES DEFORMEES MODALES SUR LA ---
C ---   FAMILLE DES FONCTIONS DE FORME ASSOCIEES A L'EXCITATION ---
C
      CALL WKVECT('&&SPECFF.TEMP.MATA','V V R',NBFONC*NBFONC,IMATA)
      CALL WKVECT('&&SPECFF.TEMP.MATB','V V R',NBFONC*NBMR,IMATB)
      CALL SCALFF(NBFONC,NBP,ZR(IDIFF),ZR(IVAFF),ZR(IMATA))
      CALL SCALDF(NBFONC,NBP,NBMR,ZR(IDIFF),ZR(IVAFF),ZR(IDEFM),
     &            ZR(IMATB))
      CALL MGAUSS(ZR(IMATA),ZR(IMATB),NBFONC,NBFONC,NBMR,ZERO,FAUX)
C
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
      DO 40 IV = 1,NPV
C
C --- 9.1.RECUPERATION DE LA DISCRETISATION FREQUENTIELLE
C        (NBPF PREMIERES VALEURS DE L'OBJET .VALE DE LA PREMIERE
C         FONCTION DE LA TABLE)
C
        WRITE(NOMFON,'(A8,A2,3I3.3)') NOMU,'.S',IV,NUOR(IMODI),
     &                                NUOR(IMODI)
        VALE = NOMFON//'.VALE'
        CALL JEVEUO(VALE,'L',IVALE)
C
        DO 50 IL = 1,NBPF
          ZR(LWR+IL-1) = ZR(IVALE+IL-1)
  50    CONTINUE
C
C --- 9.2.INTERPOLATION DES INTERSPECTRES A PROJETER
C
        IF ( LTABLE ) THEN
C
          DO 60 IFO2 = 1,NBFONC
            IVAL(2) = IFO2
            DO 61 IFO1 = 1,IFO2
              IFO = IFO2*(IFO2-1)/2 + IFO1
              IVAL(1) = IFO1
              CALL TBLIVA ( TABLE, 2, NOPART, IVAL, R8B, C16B, K8B, K8B,
     +             R8B, 'FONCTION', K8B, IBID, R8B, C16B, NOMFON, IRET )
              IF (IRET.NE.0) CALL UTMESS('F','SPECFF','Y A UN BUG 2' )
               K8B = ' '
               DO 62 IL = 1,NBPF
                PTF = ZR(LWR+IL-1)
                CALL FOINTC(NOMFON,0,K8B,PTF,RESURE,RESUIM,IER)
                IF (IER.NE.0) THEN
                  CALL UTMESS('F','SPECFF','PROBLEME RENCONTRE LORS '//
     &                 'DE L INTERPOLATION D UN INTERSPECTRE')
                ENDIF
                IDEC = 2*NBPF*(IFO-1)+2*(IL-1)
                ZR(IINTE+IDEC  ) = RESURE
                ZR(IINTE+IDEC+1) = RESUIM
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
        DO 90 IM2 = IMODI,IMODF
          IDEB = IM2
          IF (CASINT) IDEB = IMODI
          DO 91 IM1 = IDEB,IM2
            WRITE (NOMFON,'(A8,A2,3I3.3)') NOMU,'.S',IV,NUOR(IM1),
     &                                     NUOR(IM2)
            VALE = NOMFON(1:19)//'.VALE'
            CALL JEVEUO(VALE,'E',IVALE)
C
            IM2B = IM2 - IMODI + 1
            IM1B = IM1 - IMODI + 1
C
            IF ( LTABLE ) THEN
C
              DO 100 IL = 1,NBPF
C
                DO 101 IFO2 = 1,NBFONC
                  BETA12 = ZR(IMATB+NBFONC*(IM1B-1)+IFO2-1)
                  BETA22 = ZR(IMATB+NBFONC*(IM2B-1)+IFO2-1)
                  IFO = IFO2*(IFO2+1)/2
                  IDEC = 2*NBPF*(IFO-1)+2*(IL-1)
                  ZR(IVALE+NBPF+2*(IL-1)) = ZR(IVALE+NBPF+2*(IL-1))
     &                                  + BETA12*BETA22 * ZR(IINTE+IDEC)
 101            CONTINUE
C
                IF (NBFONC.GT.1) THEN
                  DO 102 IFO2 = 2,NBFONC
                    BETA12 = ZR(IMATB+NBFONC*(IM1B-1)+IFO2-1)
                    BETA22 = ZR(IMATB+NBFONC*(IM2B-1)+IFO2-1)
                    DO 103 IFO1 = 1,IFO2-1
                      BETA11 = ZR(IMATB+NBFONC*(IM1B-1)+IFO1-1)
                      BETA21 = ZR(IMATB+NBFONC*(IM2B-1)+IFO1-1)
                      IFO = IFO2*(IFO2-1)/2 + IFO1
                      IDEC = 2*NBPF*(IFO-1)+2*(IL-1)
                      ZR(IVALE+NBPF+2*(IL-1))
     &                  = ZR(IVALE+NBPF+2*(IL-1)) + ( BETA11*BETA22
     &                    + BETA12*BETA21 ) * ZR(IINTE+IDEC)
                      ZR(IVALE+NBPF+2*(IL-1)+1)
     &                  = ZR(IVALE+NBPF+2*(IL-1)+1) + ( BETA11*BETA22
     &                    - BETA12*BETA21 ) * ZR(IINTE+IDEC+1)
 103                CONTINUE
 102              CONTINUE
                ENDIF
C
 100          CONTINUE
C
            ELSE
C
              DO 110 IL = 1,NBPF
C
                DO 111 IFO2 = 1,DIMINT
                  BETA12 = ZR(IMATB+NBFONC*(IM1B-1)+IFO2-1)
                  BETA22 = ZR(IMATB+NBFONC*(IM2B-1)+IFO2-1)
                  IFO = IFO2*(IFO2+1)/2
                  IDEC = 2*NBPF*(IFO-1)+2*(IL-1)
                  ZR(IVALE+NBPF+2*(IL-1)) = ZR(IVALE+NBPF+2*(IL-1))
     &                                  + BETA12*BETA22 * ZR(IINTE+IDEC)
 111            CONTINUE
C
                DO 112 IFO2 = DIMINT+1,NBFONC
                  BETA12 = ZR(IMATB+NBFONC*(IM1B-1)+IFO2-1)
                  BETA22 = ZR(IMATB+NBFONC*(IM2B-1)+IFO2-1)
                  IFO = (IFO2-DIMINT)*(IFO2-DIMINT+1)/2
                  IDEC = 2*NBPF*(IFO-1)+2*(IL-1)
                  ZR(IVALE+NBPF+2*(IL-1)) = ZR(IVALE+NBPF+2*(IL-1))
     &                                  + BETA12*BETA22 * ZR(IINTE+IDEC)
 112            CONTINUE
C
                DO 113 IFO2 = 2,DIMINT
                  BETA12 = ZR(IMATB+NBFONC*(IM1B-1)+IFO2-1)
                  BETA22 = ZR(IMATB+NBFONC*(IM2B-1)+IFO2-1)
                  DO 114 IFO1 = 1,IFO2-1
                    BETA11 = ZR(IMATB+NBFONC*(IM1B-1)+IFO1-1)
                    BETA21 = ZR(IMATB+NBFONC*(IM2B-1)+IFO1-1)
                    IFO = IFO2*(IFO2-1)/2 + IFO1
                    IDEC = 2*NBPF*(IFO-1)+2*(IL-1)
                    ZR(IVALE+NBPF+2*(IL-1))
     &                = ZR(IVALE+NBPF+2*(IL-1)) + ( BETA11*BETA22
     &                  + BETA12*BETA21 ) * ZR(IINTE+IDEC)
                    ZR(IVALE+NBPF+2*(IL-1)+1)
     &                = ZR(IVALE+NBPF+2*(IL-1)+1) + ( BETA11*BETA22
     &                  - BETA12*BETA21 ) * ZR(IINTE+IDEC+1)
 114              CONTINUE
 113            CONTINUE
C
                DO 115 IFO2 = DIMINT+2,NBFONC
                  BETA12 = ZR(IMATB+NBFONC*(IM1B-1)+IFO2-1)
                  BETA22 = ZR(IMATB+NBFONC*(IM2B-1)+IFO2-1)
                  DO 116 IFO1 = DIMINT+1,IFO2-1
                    BETA11 = ZR(IMATB+NBFONC*(IM1B-1)+IFO1-1)
                    BETA21 = ZR(IMATB+NBFONC*(IM2B-1)+IFO1-1)
                    IFO = (IFO2-DIMINT)*(IFO2-DIMINT-1)/2 + IFO1-DIMINT
                    IDEC = 2*NBPF*(IFO-1)+2*(IL-1)
                    ZR(IVALE+NBPF+2*(IL-1))
     &                = ZR(IVALE+NBPF+2*(IL-1)) + ( BETA11*BETA22
     &                  + BETA12*BETA21 ) * ZR(IINTE+IDEC)
                    ZR(IVALE+NBPF+2*(IL-1)+1)
     &                = ZR(IVALE+NBPF+2*(IL-1)+1) + ( BETA11*BETA22
     &                  - BETA12*BETA21 ) * ZR(IINTE+IDEC+1)
 116              CONTINUE
 115            CONTINUE
C
 110          CONTINUE
C
            ENDIF
C
  91      CONTINUE
  90    CONTINUE
C
  40  CONTINUE
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

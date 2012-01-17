      SUBROUTINE LISDEF(OPER  ,OPTKZ ,OPTI  ,VALKZ ,VALI  )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 17/01/2012   AUTEUR ABBAS M.ABBAS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C TOLE CRP_20
C
      IMPLICIT      NONE
      CHARACTER*(*) VALKZ,OPTKZ
      CHARACTER*4   OPER
      INTEGER       VALI,OPTI
C
C ----------------------------------------------------------------------
C
C ROUTINE UTILITAIRE (LISTE_CHARGES)
C
C ROUTINE CENTRALE POUR LES CARACTERISTIQUES DES CHARGES
C
C
C ----------------------------------------------------------------------
C
C
C IN  OPER   : TYPE DE DEMANDE
C              'IDGE' - IDENTIFICATION DES GENRES D'UNE CHARGE
C              'OBJE' - NOM DE L'OBJET DEFINI DANS AFFE_CHAR_*
C                       (EN GENERAL, C'EST UNE CARTE)
C              'TYPC' - RETOURNE LE TYPE DE LA CHARGE (REEL, COMP, FONC)
C              'POEC' - POSITION DANS L'ENTIER CODE
C              'OPTI' - NOM DE L'OPTION DE CALCUL
C              'PARA' - NOM DU PARAMETRE POUR LE CALCUL
C              'IDNS' - LISTE DES INDXCH DES CHARGES
C              'LIGC' - RETOURNE LE TYPE DU LIGREL DE CALCUL
C              'MOTC' - RETOURNE L'INDXCH DU MOT-CLEF DONNE
C              'LISG' - LISTE DES GENRES DISPONIBLES
C IN  OPTI   : OPTION (ENTIER)
C              'IDGE' - INUTILISE
C              'OBJE' - INDEX DE LA CHARGE
C              'TYPC' - CODE (ENTIER CODE) CONTENANT LES GENRES
C              'POEC' - INUTILISE
C              'OPTI' - INDEX DE LA CHARGE
C              'PARA' - INDEX DE LA CHARGE
C              'IDNS' - POSITION DANS L'ENTIER CODE
C              'LIGC' - INDEX DE LA CHARGE
C              'MOTC' - INUTILISE
C              'LISG' - INUTILISE
C IN  OPTK   : OPTION (CHAINE)
C              'IDGE' - PREFIXE DE L'OBJET
C              'OBJE' - PREFIXE DE L'OBJET
C              'TYPC' - PREFIXE DE L'OBJET
C              'POEC' - GENRE DE LA CHARGE
C              'OPTI' - TYPE DE LA CHARGE (REEL/COMP/FONC)
C              'PARA' - TYPE DE LA CHARGE (REEL/COMP/FONC)
C              'IDNS' - NOM DE L'OBJET JEVEUX A CREER
C              'LIGC' - INUTILISE
C              'MOTC' - MOT-CLEF A REPERER
C              'LISG' - NOM DE L'OBJET JEVEUX A CREER
C OUT VALI   : REPONSE (ENTIER)
C              'IDGE' - CODE (ENTIER CODE) CONTENANT LES GENRES
C              'OBJE' - 1 SI L'OBJET UNE CARTE
C              'TYPC' - INUTILISE
C              'POEC' - POSITION DANS L'ENTIER CODE
C              'OPTI' - INUTILISE
C              'PARA' - INUTILISE
C              'IDNS' - NOMBRE DE CHARGES
C              'LIGC' - INUTILISE
C              'MOTC' - INDXCH DE LA CHARGE
C              'LISG' - NOMBRE DE GENRES DISPONIBLES
C OUT VALK   : REPONSE (CHAINE)
C              'IDGE' - INUTILISE
C              'OBJE' - NOM DE L'OBJET
C              'TYPC' - TYPE DE LA CHARGE
C                     'REEL'    - CHARGE CONSTANTE REELLE
C                     'COMP'    - CHARGE CONSTANTE COMPLEXE
C                     'FONC_F0' - CHARGE FONCTION QUELCONQUE
C                     'FONC_FT' - CHARGE FONCTION DU TEMPS
C              'POEC' - INUTILISE
C              'OPTI' - NOM DE L'OPTION DE CALCUL
C              'PARA' - NOM DU PARAMETRE POUR LE CALCUL
C              'IDNS' - INUTILISE
C              'LIGC' - TYPE DU LIGREL DE CALCUL: LIGRCH OU LIGRMO
C              'MOTC' - INUTILISE
C              'LISG' - INUTILISE
C
C --------- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C
C ----------- FIN COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER      NBTYTH
      PARAMETER   (NBTYTH = 28)
      CHARACTER*6  NOMOB(NBTYTH)
      CHARACTER*24 MOTCL(NBTYTH)
      CHARACTER*24 GENRE(NBTYTH)
      INTEGER      INDCOD(NBTYTH)
      CHARACTER*16 OPTIOF(NBTYTH),OPTIOR(NBTYTH),OPTIOC(NBTYTH)
      CHARACTER*8  PARAF(NBTYTH),PARAR(NBTYTH),PARAC(NBTYTH)
      CHARACTER*6  TYPLIG(NBTYTH)
C
      INTEGER      INDXCH,IRET,CODCHA,IBID,IPOSIT,NBCH,I,ITYPOB
      CHARACTER*16 OPTION,TYPECO
      CHARACTER*24 TYPCHA,GENCHA,NOMOBJ,PARCHA,GENOLD,MOTCLE
      CHARACTER*8  CHARGE,TYPECH,LPAIN,NOMGD
      INTEGER      TABCOD(30),IDD,INDEX2
      CHARACTER*19 CARTE,CHAMNO
      CHARACTER*24 LISCNS
      CHARACTER*6  LIGCAL
      INTEGER      JLISCI,JLISCK
      LOGICAL      LFIRST,LDOUB
      LOGICAL      LVEAS,LVEAC,LVEAG
      CHARACTER*13 PREFOB
C
C --- OBJETS DEFINISSANT LES CHARGEMENTS: ON NE MET QUE CEUX DEFINIS
C --- PAR UNE CARTE - AUTRES OBJETS: __*
C
      DATA NOMOB/
     &     '.CIMPO'          ,'__ELIM'          ,
     &     '.FORNO'          ,'.EPSIN'          ,'.SIINT'          ,
     &     '.PRESS'          ,'.FLUX'           ,'.VNOR'           ,
     &     '.IMPE'           ,'__EVOC'          ,'.PESAN'          ,
     &     '.ROTAT'          ,'.SIGIN'          ,'.FELEC'          ,
     &     '.FL1'            ,'.ONDE'           ,'.ONDPL'          ,
     &     '.VEASS'          ,'.F1D2D'          ,'.F3D3D'          ,
     &     '.F2D2D'          ,'.F1D3D'          ,'.F2D3D'          ,
     &     '.F1D1D'          ,'.FCO3D'          ,'.FCO2D'          ,
     &     '__VEAS'          ,'__VEAG'          /
C
C --- MOT_CLEF DEFINISSANT LE CHARGEMENT DANS AFFE_CHAR_*
C --- (SAUF DIRICHLET)
C
      DATA MOTCL /
     &     'DIRI_DUAL'       ,'DIRI_ELIM'       ,
     &     'FORCE_NODALE'    ,'EPSI_INIT'       ,'SIGM_INTERNE'    ,
     &     'PRES_REP'        ,'FLUX_THM_REP'    ,'VITE_FACE'       ,
     &     'IMPE_FACE'       ,'EVOL_CHAR'       ,'PESANTEUR'       ,
     &     'ROTATION'        ,'RELA_CINE_BP'    ,'FORCE_ELEC'      ,
     &     'INTE_ELEC'       ,'ONDE_FLUI'       ,'ONDE_PLANE'      ,
     &     'VECT_ASSE'       ,'FORCE_CONTOUR'   ,'FORCE_INTERNE#3D',
     &     'FORCE_INTERNE#2D','FORCE_ARETE'     ,'FORCE_FACE'      ,
     &     'FORCE_POUTRE'    ,'FORCE_COQUE#3D'  ,'FORCE_COQUE#2D'  ,
     &     'VECT_ASSE'       ,'VECT_ASSE_GENE'  /
C
      DATA GENRE /
     &     'DIRI_DUAL'       ,'DIRI_ELIM'       ,
     &     'NEUM_MECA'       ,'NEUM_MECA'       ,'SIGM_INTERNE'    ,
     &     'NEUM_MECA'       ,'NEUM_MECA'       ,'VITE_FACE'       ,
     &     'IMPE_FACE'       ,'EVOL_CHAR'       ,'NEUM_MECA'       ,
     &     'NEUM_MECA'       ,'SIGM_CABLE'      ,'FORCE_ELEC'      ,
     &     'INTE_ELEC'       ,'ONDE_FLUI'       ,'ONDE_PLANE'      ,
     &     'VECT_ASSE_CHAR'  ,'NEUM_MECA'       ,'NEUM_MECA'       ,
     &     'NEUM_MECA'       ,'NEUM_MECA'       ,'NEUM_MECA'       ,
     &     'NEUM_MECA'       ,'NEUM_MECA'       ,'NEUM_MECA'       ,
     &     'VECT_ASSE'       ,'VECT_ASSE_GENE'  /
C
      DATA TYPLIG /
     &     'LIGRCH'          ,'LIGRCH'          ,
     &     'LIGRCH'          ,'LIGRMO'          ,'LIGRMO'          ,
     &     'LIGRMO'          ,'LIGRMO'          ,'LIGRMO'          ,
     &     'LIGRMO'          ,'LIGRMO'          ,'LIGRMO'          ,
     &     'LIGRMO'          ,'LIGRMO'          ,'LIGRMO'          ,
     &     'LIGRMO'          ,'LIGRMO'          ,'LIGRMO'          ,
     &     'LIGRMO'          ,'LIGRMO'          ,'LIGRMO'          ,
     &     'LIGRMO'          ,'LIGRMO'          ,'LIGRMO'          ,
     &     'LIGRMO'          ,'LIGRMO'          ,'LIGRMO'          ,
     &     ' '               ,' '               /
C
      DATA INDCOD /
     &     02                ,01                ,
     &     03                ,03                ,05                ,
     &     03                ,03                ,06                ,
     &     07                ,08                ,03                ,
     &     03                ,09                ,14                ,
     &     10                ,11                ,12                ,
     &     13                ,03                ,03                ,
     &     03                ,03                ,03                ,
     &     03                ,03                ,03                ,
     &     04                ,15                /
C
C --- NOM DE L'OPTION - CAS REEL
C
      DATA OPTIOR /
     &     'MECA_DDLI_R'     ,' '               ,
     &     'CHAR_MECA_FORC_R','CHAR_MECA_EPSI_R',' '               ,
     &     'CHAR_MECA_PRES_R','CHAR_MECA_FLUX_R',' '               ,
     &     ' '               ,' '               ,'CHAR_MECA_PESA_R',
     &     'CHAR_MECA_ROTA_R',' '               ,'CHAR_MECA_FRELEC',
     &     ' '               ,' '               ,' '               ,
     &     ' '               ,'CHAR_MECA_FR1D2D','CHAR_MECA_FR3D3D',
     &     'CHAR_MECA_FR2D2D','CHAR_MECA_FR1D3D','CHAR_MECA_FR2D3D',
     &     'CHAR_MECA_FR1D1D','CHAR_MECA_FRCO3D','CHAR_MECA_FRCO2D',
     &     ' '               ,' '               /
C
C --- NOM DE L'OPTION - CAS COMPLEXE
C
      DATA OPTIOC /
     &     'MECA_DDLI_C'     ,' '               ,
     &     ' '               ,' '               ,' '               ,
     &     ' '               ,' '               ,' '               ,
     &     ' '               ,' '               ,' '               ,
     &     ' '               ,' '               ,' '               ,
     &     ' '               ,' '               ,' '               ,
     &     ' '               ,' '               ,' '               ,
     &     ' '               ,' '               ,' '               ,
     &     'CHAR_MECA_FC1D1D',' '               ,' '               ,
     &     ' '               ,' '               /
C
C --- NOM DE L'OPTION - CAS FONCTION REELLE
C
      DATA OPTIOF /
     &     'MECA_DDLI_F'     ,' '               ,
     &     'CHAR_MECA_FORC_F','CHAR_MECA_EPSI_F',' '               ,
     &     'CHAR_MECA_PRES_F','CHAR_MECA_FLUX_F',' '               ,
     &     ' '               ,' '               ,' '               ,
     &     ' '               ,' '               ,' '               ,
     &     ' '               ,' '               ,' '               ,
     &     ' '               ,'CHAR_MECA_FF1D2D','CHAR_MECA_FF3D3D',
     &     'CHAR_MECA_FF2D2D','CHAR_MECA_FF1D3D','CHAR_MECA_FF2D3D',
     &     'CHAR_MECA_FF1D1D','CHAR_MECA_FFCO3D','CHAR_MECA_FFCO2D',
     &     ' '               ,' '               /
C
C --- NOM DU PARAMETRE - CAS REEL
C
      DATA PARAR /
     &     'PDDLIMR'         ,' '               ,
     &     'PFORNOR'         ,'PEPSINR'         ,' '               ,
     &     'PPRESSR'         ,'PFLUXR'          ,' '               ,
     &     ' '               ,' '               ,'PESANR'          ,
     &     'PROTATR'         ,' '               ,'PFRELEC'         ,
     &     ' '               ,' '               ,' '               ,
     &     ' '               ,'PFR1D2D'         ,'PFR3D3D'         ,
     &     'PFR2D2D'         ,'PFR1D3D'         ,'PFR2D3D'         ,
     &     'PFR1D1D'         ,'PFRCO3D'         ,'PFRCO2D'         ,
     &     ' '               ,' '               /
C
C --- NOM DU PARAMETRE - CAS COMPLEXE
C
      DATA PARAC /
     &     'PDDLIMC'         ,' '               ,
     &     ' '               ,' '               ,' '               ,
     &     ' '               ,' '               ,' '               ,
     &     ' '               ,' '               ,' '               ,
     &     ' '               ,' '               ,' '               ,
     &     ' '               ,' '               ,' '               ,
     &     ' '               ,' '               ,' '               ,
     &     ' '               ,' '               ,' '               ,
     &     'PFC1D1D'         ,' '               ,' '               ,
     &     ' '               ,' '               /
C
C --- NOM DU PARAMETRE - CAS FONCTION REELLE
C
      DATA PARAF /
     &     'PDDLIMF'         ,' '               ,
     &     'PFORNOF'         ,'PEPSINF'         ,' '               ,
     &     'PPRESSF'         ,'PFLUXF'          ,' '               ,
     &     ' '               ,' '               ,'PPESANR'         ,
     &     'PROTATR'         ,' '               ,'PFRELEC'         ,
     &     ' '               ,' '               ,' '               ,
     &     ' '               ,'PFF1D2D'         ,'PFF3D3D'         ,
     &     'PFF2D2D'         ,'PFF1D3D'         ,'PFF2D3D'         ,
     &     'PFF1D1D'         ,'PFFCO3D'         ,'PFFCO2D'         ,
     &     ' '               ,' '               /
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C ----------------------------------------------------------------------
      IF (OPER.EQ.'IDGE') THEN
        CODCHA = 0
        CALL ISDECO(CODCHA,TABCOD,30)
        PREFOB = OPTKZ
        CHARGE = PREFOB(1:8)
C
C ----- REPERAGE DES CHARGEMENTS
C
        DO 10 INDXCH = 1,NBTYTH
          NOMOBJ = NOMOB(INDXCH)
          IPOSIT = 0
          IF (NOMOBJ(1:2).EQ.'__') THEN
            IF (NOMOBJ.EQ.'__ELIM') THEN
              CALL DISMOI('C'   ,'TYPE_CHARGE',CHARGE,'CHARGE',IBID  ,
     &                    TYPCHA,IRET)
              IF ((TYPCHA(1:4).EQ.'CIME').OR.
     &            (TYPCHA(1:4).EQ.'CITH').OR.
     &            (TYPCHA(1:4).EQ.'CIAC')) THEN
                IPOSIT = INDCOD(INDXCH)
              ENDIF
            ELSEIF (NOMOBJ.EQ.'__EVOC') THEN
              NOMOBJ = CHARGE(1:8)//'.CHME.EVOL.CHAR'
              CALL JEEXIN(NOMOBJ,IRET)
              IF (IRET.NE.0) IPOSIT = INDCOD(INDXCH)
            ELSEIF (NOMOBJ.EQ.'__VEAS') THEN
              CHAMNO = CHARGE
              CALL JEEXIN(CHAMNO(1:19)//'.VALE',IRET)
              IF (IRET.NE.0) THEN
                CALL GETTCO(CHARGE,TYPECO)
                IF (TYPECO.EQ.'CHAM_NO_SDASTER') IPOSIT = INDCOD(INDXCH)
              ENDIF
            ELSEIF (NOMOBJ.EQ.'__VEAG') THEN
              CHAMNO = CHARGE
              CALL JEEXIN(CHAMNO(1:19)//'.VALE',IRET)
              IF (IRET.NE.0) THEN
                CALL GETTCO(CHARGE,TYPECO)
                IF (TYPECO.EQ.'VECT_ASSE_GENE') IPOSIT = INDCOD(INDXCH)
              ENDIF
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF
          ELSE
            CARTE  = PREFOB(1:13)//NOMOBJ
            CALL EXISD('CARTE',CARTE,IRET)
            IF (IRET.EQ.1) IPOSIT = INDCOD(INDXCH)
          ENDIF
          IF (IPOSIT.GT.30) CALL ASSERT(.FALSE.)
          IF (IPOSIT.NE.0)  TABCOD(IPOSIT) = 1
 10     CONTINUE
C
C ----- CODAGE DE L'ENTIER
C
        CALL ISCODE(TABCOD,CODCHA,30)
        VALI   = CODCHA
C ----------------------------------------------------------------------
      ELSEIF (OPER.EQ.'OBJE') THEN
        INDXCH = OPTI
        PREFOB = OPTKZ
        ITYPOB = -1
        NOMOBJ = ' '
        IF ((INDXCH.GT.0).AND.(INDXCH.LE.NBTYTH)) THEN
          NOMOBJ = PREFOB(1:13)//NOMOB(INDXCH)
          IF (NOMOB(INDXCH)(1:2).EQ.'__') THEN
            IF (GENRE(INDXCH).EQ.'EVOL_CHAR') THEN
              ITYPOB = 0
              NOMOBJ = PREFOB(1:13)//'.EVOL.CHAR'
            ELSEIF (GENRE(INDXCH).EQ.'DIRI_ELIM') THEN
              ITYPOB = 0
              CHARGE = PREFOB(1:8)
              NOMOBJ = CHARGE(1:8)//'.TYPE'
            ELSEIF (GENRE(INDXCH).EQ.'VECT_ASSE') THEN
              ITYPOB = 0
              CHAMNO = PREFOB(1:8)
              NOMOBJ = CHAMNO(1:19)//'.VALE'
            ELSEIF (GENRE(INDXCH).EQ.'VECT_ASSE_GENE') THEN
              ITYPOB = 0
              CHAMNO = PREFOB(1:8)
              NOMOBJ = CHAMNO(1:19)//'.VALE'
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF
          ELSE
            CARTE  = PREFOB(1:13)//NOMOB(INDXCH)
            ITYPOB = 1
            NOMOBJ = CARTE
          ENDIF
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
        CALL ASSERT(ITYPOB.GE.0)
        VALKZ  = NOMOBJ
        VALI   = ITYPOB
C ----------------------------------------------------------------------
      ELSEIF (OPER.EQ.'TYPC') THEN
        PREFOB = OPTKZ
        TYPECH = ' '
        CODCHA = OPTI
        LVEAS  = .FALSE.
        LVEAC  = .FALSE.
        LVEAG  = .FALSE.
        CALL ISDECO(CODCHA,TABCOD,30)
C
C ----- DETECTION VECT_ASSE_CHAR
C
        DO 20 INDXCH = 1,NBTYTH
          IF (GENRE(INDXCH).EQ.'VECT_ASSE_CHAR') IPOSIT = INDCOD(INDXCH)
 20     CONTINUE
        IF (TABCOD(IPOSIT).EQ.1) LVEAC = .TRUE.
C
C ----- DETECTION VECT_ASSE
C
        DO 21 INDXCH = 1,NBTYTH
          IF (GENRE(INDXCH).EQ.'VECT_ASSE') IPOSIT = INDCOD(INDXCH)
 21     CONTINUE
        IF (TABCOD(IPOSIT).EQ.1) LVEAS = .TRUE.
C
C ----- DETECTION VECT_ASSE_GENE
C
        DO 22 INDXCH = 1,NBTYTH
          IF (GENRE(INDXCH).EQ.'VECT_ASSE_GENE') IPOSIT = INDCOD(INDXCH)
 22     CONTINUE
        IF (TABCOD(IPOSIT).EQ.1) LVEAG = .TRUE.
C
C ----- DETECTION TYPE DU CHAMP
C
        IF (LVEAC.OR.LVEAS.OR.LVEAG) THEN
          CALL LISCVA(PREFOB,CHAMNO)
          CALL JELIRA(CHAMNO//'.VALE','TYPE',IBID,NOMGD)
          IF (NOMGD(1:1).EQ.'R') THEN
            TYPECH = 'REEL'
          ELSEIF (NOMGD(1:1).EQ.'C') THEN
            TYPECH = 'COMP'
          ELSE
            CALL ASSERT(.FALSE.)
          ENDIF
        ELSE
          CHARGE = PREFOB(1:8)
          CALL DISMOI('F'   ,'TYPE_CHARGE',CHARGE,'CHARGE',IBID,
     &                TYPCHA,IRET)
          IF (TYPCHA(5:7).EQ.'_RE') THEN
            TYPECH = 'REEL'
          ELSEIF (TYPCHA(5:7).EQ.'_RI') THEN
            TYPECH = 'COMP'
          ELSEIF (TYPCHA(5:6).EQ.'_F') THEN
            TYPECH = 'FONC'
          ELSE
            CALL ASSERT(.FALSE.)
          ENDIF
        ENDIF
C
C ----- CAS D'UNE FONCTION: EST-ELLE FONCTION DU TEMPS ?
C
        IF (TYPECH.EQ.'FONC') THEN
          TYPECH = 'FONC_F0'
          LFIRST = .FALSE.
          DO 103 INDXCH = 1,NBTYTH
            CARTE  = PREFOB(1:13)//NOMOB(INDXCH)
            IF (NOMOB(INDXCH).NE.' ') THEN
              CALL EXISD('CARTE',CARTE,IRET)
              IF (IRET.EQ.1) THEN
                CALL DISMOI('F'   ,'PARA_INST',CARTE,'CARTE',
     &                      IBID  ,PARCHA,IRET)
C
C ----- PROTECTION: POUR UNE CHARGE, TOUT DOIT ETRE DU MEME TYPE
C
                IF (LFIRST) THEN
                  IF ((PARCHA.EQ.'OUI').AND.(TYPECH.EQ.'FONC_F0'))
     &              CALL ASSERT(.FALSE.)
                  IF ((PARCHA.EQ.'NON').AND.(TYPECH.EQ.'FONC_FT'))
     &              CALL ASSERT(.FALSE.)
                ENDIF
                IF (PARCHA(1:3).EQ.'OUI') THEN
                  TYPECH = 'FONC_FT'
                  LFIRST = .TRUE.
                ENDIF
              ENDIF
            ENDIF
 103      CONTINUE
        ENDIF
        CALL ASSERT(TYPECH.NE.' ')
        VALKZ = TYPECH
C ----------------------------------------------------------------------
      ELSEIF (OPER.EQ.'POEC') THEN
        GENCHA = OPTKZ
        IPOSIT = 0
        DO 15 INDXCH=1,NBTYTH
          IF (GENRE(INDXCH).EQ.GENCHA) IPOSIT = INDCOD(INDXCH)
 15     CONTINUE
        IF ((IPOSIT.LE.0).OR.(IPOSIT.GT.30)) CALL ASSERT(.FALSE.)
        VALI   = IPOSIT
C ----------------------------------------------------------------------
      ELSEIF (OPER.EQ.'OPTI') THEN
        INDXCH  = OPTI
        TYPECH = OPTKZ
        OPTION = ' '
        IF ((INDXCH.GT.0).AND.(INDXCH.LE.NBTYTH)) THEN
          IF (TYPECH.EQ.'REEL') THEN
            OPTION = OPTIOR(INDXCH)
          ELSEIF (TYPECH.EQ.'COMP') THEN
            OPTION = OPTIOC(INDXCH)
          ELSEIF (TYPECH(1:4).EQ.'FONC') THEN
            OPTION = OPTIOF(INDXCH)
          ELSE
            CALL ASSERT(.FALSE.)
          ENDIF
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
        VALKZ  = OPTION
C ----------------------------------------------------------------------
      ELSEIF (OPER.EQ.'PARA') THEN
        INDXCH  = OPTI
        TYPECH = OPTKZ
        LPAIN  = ' '
        IF ((INDXCH.GT.0).AND.(INDXCH.LE.NBTYTH)) THEN
          IF (TYPECH.EQ.'REEL') THEN
            LPAIN = PARAR(INDXCH)
          ELSEIF (TYPECH.EQ.'COMP') THEN
            LPAIN = PARAC(INDXCH)
          ELSEIF (TYPECH(1:4).EQ.'FONC') THEN
            LPAIN = PARAF(INDXCH)
          ELSE
            CALL ASSERT(.FALSE.)
          ENDIF
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
        VALKZ  = LPAIN
C ----------------------------------------------------------------------
      ELSEIF (OPER.EQ.'IDNS') THEN
        LISCNS = OPTKZ
        IPOSIT = OPTI
C
C ----- GENRE DU CHARGEMENT
C
        DO 40 INDXCH = 1,NBTYTH
          IF (IPOSIT.EQ.INDCOD(INDXCH)) GENCHA = GENRE(INDXCH)
 40     CONTINUE
C
C ----- NOMBRE DE CHARGEMENTS DE CE GENRE
C
        NBCH = 0
        DO 45 INDXCH = 1,NBTYTH
          IF (GENRE(INDXCH).EQ.GENCHA) NBCH = NBCH + 1
 45     CONTINUE
        CALL ASSERT(NBCH.GT.0)
C
C ----- CREATION DE L'OBJET
C
        CALL WKVECT(LISCNS,'V V I',NBCH,JLISCI)
C
C ----- REMPLISSAGE DE L'OBJET
C
        I = 0
        DO 50 INDXCH = 1,NBTYTH
          IF (GENRE(INDXCH).EQ.GENCHA) THEN
            I = I+1
            ZI(JLISCI-1+I)   = INDXCH
          ENDIF
 50     CONTINUE
        CALL ASSERT(I.EQ.NBCH)
        VALI  = NBCH
C ----------------------------------------------------------------------
      ELSEIF (OPER.EQ.'LIGC') THEN
        INDXCH  = OPTI
        LIGCAL = ' '
        IF ((INDXCH.GT.0).AND.(INDXCH.LE.NBTYTH)) THEN
          LIGCAL = TYPLIG(INDXCH)
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
        VALKZ  = LIGCAL
C ----------------------------------------------------------------------
      ELSEIF (OPER.EQ.'MOTC') THEN
        INDXCH = OPTI
        MOTCLE = MOTCL(INDXCH)
        VALKZ  = MOTCLE
C ----------------------------------------------------------------------
      ELSEIF (OPER.EQ.'LISG') THEN
        LISCNS = OPTKZ
        IDD    = 1
        CALL WKVECT(LISCNS,'V V K24',NBTYTH,JLISCK)
        DO 70 INDXCH = 1,NBTYTH
          GENCHA = GENRE(INDXCH)
          IF (GENCHA.NE.' ') THEN
            IF (INDXCH.EQ.1) THEN
              ZK24(JLISCK-1+INDXCH) = GENCHA
            ELSE
              LDOUB = .FALSE.
              DO 71 INDEX2 = 1,NBTYTH
                GENOLD = ZK24(JLISCK-1+INDEX2)
                IF ((GENOLD.EQ.GENCHA).AND.(GENOLD.NE.' ')) THEN
                  LDOUB = .TRUE.
                  GOTO 72
                ENDIF
 71           CONTINUE
 72           CONTINUE
              IF (.NOT.LDOUB) THEN
                IDD = IDD + 1
                ZK24(JLISCK-1+IDD) = GENCHA
              ENDIF
            ENDIF
          ENDIF
 70     CONTINUE
        CALL ASSERT(IDD.LE.NBTYTH)
        VALI = IDD
C
      ELSE
        CALL ASSERT(.FALSE.)

      ENDIF
C
      CALL JEDEMA()
      END

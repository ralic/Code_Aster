      SUBROUTINE NTDOTH ( MODELE, MATE, CARELE, FOMULT, MATCST,
     &                    COECST, INFCHA,
     &                    NBPASE, INPSCO)
C
C     THERMIQUE - DONNEES EN THERMIQUE
C     *           **         **
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/12/2002   AUTEUR CIBHHGB G.BERTRAND 
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
C
C ----------------------------------------------------------------------
C     SAISIE ET VERIFICATION DE LA COHERENCE DES DONNEES THERMIQUES DU
C     PROBLEME.
C
C VAR MODELE  : NOM DU MODELE
C OUT MATE    : NOM DE LA CARTE CODEE DU CHAMP MATERIAU
C OUT CARELE  : CARACTERISTIQUES DES POUTRES ET COQUES
C OUT FOMULT  : LISTE DES FONCTIONS MULTIPLICATIVES
C OUT MATCST  : LOGIQUE INDIQUANT SI LE MATERIAU EST CONSTANT / TEMPS
C OUT COECST  : LOGIQUE INDIQUANT SI LES C.L. SONT CONSTANTES / TEMPS
C               POUR LE RECALCUL OU NON DE LA RIGIDITE DANS OP0025
C VAR INFCHA  : CONTIENT LA LISTE DES CHARGES ET DES INFOS SUR
C               SUR LES CHARGES
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       28/01/02 (OB): CREATION DE LA CARTE DERIVEE CODEE.
C----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C 0.1. ==> ARGUMENTS

      INTEGER      NBPASE
      CHARACTER*19 INFCHA
      CHARACTER*24 MODELE,CARELE,FOMULT,MATE
      CHARACTER*(*) INPSCO
      LOGICAL      MATCST,COECST

C 0.2. ==> COMMUNS

C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
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
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------

C 0.3. ==> VARIABLES LOCALES

      CHARACTER*6 NOMPRO
      PARAMETER ( NOMPRO = 'NTDOTH' )

      INTEGER N1,NCHAR,IALICH,IBID,IERD,IALIFC,ICH,IRET,JINF,JPRO,JVAL,
     &        NRPASE,K,NCHCI,ADCHSE,IAUX,JAUX
      CHARACTER*8 LCHCI,MATERS,K8BID,CARA,MODE,TYPCH,PARCHA,REPK,
     &            MATERI,NOPASE
      CHARACTER*16 NOMMOD,NOMEXC,NOMCAR
      CHARACTER*24 LIGRCH,LCHIN,NOMFCT,NOMCHA,CHARSE,K24BID,MATSEN
      LOGICAL FMULT,EXCHSE

C --- NOMBRE MAXIMUM DE TYPE DE CHARGE : NBTYCH

      INTEGER      NBTYCH     
      PARAMETER   (NBTYCH = 10)      
      CHARACTER*6  NOMLIG(NBTYCH)
      CHARACTER*8  TYPEPS(-2:NBTYCH)

      DATA NOMLIG/
     &     '.CIMPO'  ,'.SOURE'  ,'.FLURE'  ,'.FLUR2'  ,
     &     '.T_EXT'  ,'.COEFH'  ,'.HECHP'  ,'.GRAIN'  ,'.FLUNL'  ,
     &     '.RAYO'   /
      DATA TYPEPS/
     &     'MATERIAU','CARAELEM','DIRICHLE','        ',     
     &     'SOURCE  ','FLUX    ','FLUX    ',
     &     'T_EXT   ','COEFH   ','HECHP   ','GRAIN   ','FLUNL   ',
     &     'RAYO    '/

C====
C 1. PREALABLES
C====
      CALL JEMARQ()
      NOMMOD = 'MODELE'
      NOMCAR = 'CARA_ELEM'
      NOMEXC = 'EXCIT'
      COECST = .TRUE.

C====
C 2. RECUPERATIONS
C====

C 2.1. ==> LE MODELE
      CALL GETVID(' ',NOMMOD,0,1,1,MODE,N1)
      MODELE = MODE

C 2.2. ==> LE MATERIAU
      MATERI = ' '
      CALL GETVID (' ', 'CHAM_MATER',0,1,1,MATERI,N1)
      CALL DISMOI('F','THER_F_INST',MATERI,'CHAM_MATER',IBID,REPK,IERD)
      MATCST = .FALSE.
      IF ( REPK .EQ. 'NON' ) MATCST = .TRUE.
      CALL RCMFMC ( MATERI , MATE )

C 2.3. ==> LES CARACTERISTIQUES ELEMENTAIRES
      CALL GETVID(' ',NOMCAR,0,1,1,CARA,N1)
      IF (N1.LE.0) CARA = '        '
      CARELE = CARA

C 2.4. ==> QUELS MATERIAUX ET CARACTERISTIQUES SONT TOUCHES PAR LE
C          CALCUL DE SENSIBILITE ?

      DO 24 , NRPASE = 1 , NBPASE
        IAUX = NRPASE
        JAUX = 1
        CALL PSNSLE ( INPSCO, IAUX, JAUX, NOPASE )
        CALL PSRENC ( MATERI, NOPASE, MATERS, IRET )
        IF ( IRET.EQ.0 ) THEN
          CALL PSTYPA ( NBPASE, INPSCO, MATERI, NOPASE, TYPEPS(-2) )
          CALL RCMFMC(MATERS,MATSEN)
        ENDIF
        CALL PSRENC ( CARA, NOPASE, K8BID, IRET )
        IF ( IRET.EQ.0 ) THEN
          CALL PSTYPA ( NBPASE, INPSCO, CARA, NOPASE, TYPEPS(-1) )
        ENDIF
   24 CONTINUE

C====
C 3. LES CHARGES
C====

      CALL GETFAC (NOMEXC,NCHAR)

      IF ( NCHAR.NE.0 ) THEN
C 3.1. ==> LISTE DES CHARGES

      CALL WKVECT(INFCHA//'.LCHA','V V K24',NCHAR,IALICH)
      CALL WKVECT(INFCHA//'.INFC','V V IS',2*NCHAR+1,JINF)
      ZI(JINF) = NCHAR
      FOMULT = '&&'//NOMPRO//'.LIFCTS'
      CALL WKVECT (FOMULT,'V V K24',NCHAR,IALIFC)
      CHARSE = '&&'//NOMPRO//'.CHARSE'
      IAUX = MAX(NBPASE,1)
      CALL WKVECT (CHARSE,'V V K8',IAUX,ADCHSE)
      NCHCI = 0

C 3.2. ==> DETAIL DE CHAQUE CHARGE

      DO 32 , ICH = 1 , NCHAR

        CALL GETVID (NOMEXC,'CHARGE',ICH,1,1,NOMCHA,N1)
        ZK24(IALICH+ICH-1) = NOMCHA

C 3.2.1. ==> LA CHARGE EST-ELLE CONCERNEE PAR UN CALCUL DE SENSIBILITE ?

        EXCHSE = .FALSE.
        DO 321 , NRPASE = 1 , NBPASE
          IAUX = NRPASE
          JAUX = 1
          CALL PSNSLE ( INPSCO, IAUX, JAUX, NOPASE )
          CALL PSRENC ( NOMCHA, NOPASE, K8BID, IRET )
          IF ( IRET.EQ.0 ) THEN
            ZK8(ADCHSE+NRPASE-1) = NOPASE
            EXCHSE = .TRUE.
          ELSE
            ZK8(ADCHSE+NRPASE-1) = '        '
          ENDIF
  321   CONTINUE

C 3.2.2. ==> TYPES DE CHARGES UTILISEES

        CALL DISMOI('F','TYPE_CHARGE',NOMCHA,'CHARGE',IBID,
     &               TYPCH,IERD)
        IF ((TYPCH(1:5).NE.'THER_').AND.(TYPCH(1:5).NE.'CITH_')) THEN
          CALL UTMESS('E',NOMPRO//'_02','LA CHARGE '
     &              //NOMCHA(1:8)//' N''EST PAS THERMIQUE')
        ENDIF
        LIGRCH =  NOMCHA(1:8)//'.CHTH.LIGRE'

C 3.2.3. ==> ON REGARDE LES CHARGES DU TYPE DIRICHLET PAR AFFE_CHAR_CINE

        IF (TYPCH(1:5).EQ.'CITH_') THEN
          CALL JEEXIN(NOMCHA(1:19)//'.DEFI',IRET)
          IF (IRET.EQ.0) THEN
            CALL UTMESS('F',NOMPRO//'_02','LA CHARGE CINE '
     &                  //NOMCHA(1:8)//' N"A PAS DE .DEFI')
          ENDIF
          IF (TYPCH(5:7).EQ.'_FT') THEN
            ZI(JINF+ICH) = -3
          ELSE IF (TYPCH(5:7).EQ.'_FO') THEN
            ZI(JINF+ICH) = -2
          ELSE
            ZI(JINF+ICH) = -1
          ENDIF
        ENDIF

C 3.2.4. ==> ON REGARDE LES CHARGES DU TYPE DIRICHLET

        LCHIN = LIGRCH(1:13)//'.CIMPO.DESC'
        CALL JEEXIN(LCHIN,IRET)
        IF ( IRET .NE. 0 ) THEN
          IF (TYPCH(5:7).EQ.'_FO') THEN
            ZI(JINF+ICH) = 2
            CALL DISMOI('F','PARA_INST',LCHIN(1:19),'CARTE',IBID,
     &                   PARCHA,IERD)
            IF ( PARCHA(1:3) .EQ. 'OUI' ) THEN
              ZI(JINF+ICH) = 3
            ENDIF
            IF ( EXCHSE ) THEN
            DO 3241 , NRPASE = 1 , NBPASE
              NOPASE = ZK8(ADCHSE+NRPASE-1)
              IF ( NOPASE.NE.'        ' ) THEN
                CALL TELLME ( 'F', 'NOM_FONCTION', LCHIN(1:19), NOPASE,
     &                        K24BID, IBID, REPK, IERD )
                IF ( REPK.EQ.'OUI' ) THEN
                  CALL PSTYPA ( NBPASE, INPSCO, NOMCHA, NOPASE,
     >                          TYPEPS(0) )
                ENDIF
              ENDIF
 3241       CONTINUE
            ENDIF
          ELSE
            ZI(JINF+ICH) = 1
          ENDIF
        ENDIF

C 3.2.5. ==> FONCTIONS MULTIPLICATIVES DES CHARGES

        FMULT = .FALSE.
        CALL GETVID(NOMEXC,'FONC_MULT',ICH,1,1,ZK24(IALIFC+ICH-1),N1)
        IF (N1.EQ.0) THEN
          NOMFCT = '&&'//NOMPRO
          CALL JEEXIN(NOMFCT(1:19)//'.PROL',IRET)
          IF ( IRET .EQ. 0 ) THEN
            CALL WKVECT(NOMFCT(1:19)//'.PROL','V V K16',5,JPRO)
            ZK16(JPRO)   = 'CONSTANT'
            ZK16(JPRO+1) = 'CONSTANT'
            ZK16(JPRO+2) = 'TOUTPARA'
            ZK16(JPRO+3) = 'TOUTRESU'
            ZK16(JPRO+4) = 'CC      '

            CALL WKVECT(NOMFCT(1:19)//'.VALE','V V R',2,JVAL)
            ZR(JVAL)  = 1.0D0
            ZR(JVAL+1)= 1.0D0
          ENDIF
          ZK24(IALIFC+ICH-1) = '&&'//NOMPRO
        ELSE
          FMULT  = .TRUE.
        ENDIF

C 3.2.6. ==> ON REGARDE LES AUTRES CHARGES

        DO 326 , K = 2 ,NBTYCH
          LCHIN = LIGRCH(1:13)//NOMLIG(K)//'.DESC'
          CALL EXISD('CHAMP_GD',LCHIN,IRET)
          IF ( IRET .NE. 0 ) THEN
            IF  ((K.GE.7).AND.FMULT) THEN
                CALL UTMESS('F',NOMPRO//'_03','LA CHARGE '
     &          //NOMCHA(1:8)//' N''EST PAS COMPATIBLE'
     &          //' AVEC FONC_MULT')
            ENDIF
            IF (TYPCH(5:7).EQ.'_FO') THEN
              ZI(JINF+NCHAR+ICH) = MAX(2,ZI(JINF+NCHAR+ICH))
              CALL DISMOI('F','PARA_INST',LCHIN(1:19),'CARTE',IBID,
     &                    PARCHA,IERD)
              IF ( PARCHA(1:3) .EQ. 'OUI' ) THEN
C
C               IL EST INUTILE DE REASSEMBLER LA MATRICE DE RIGIDITE
C               SI T_EXT VARIE SEULE (ET COEFH CONSTANT)
C               TRAVAIL A COMPLETER POUR SOURCE, FLUX,...
C
                IF ( NOMLIG(K).NE.'.T_EXT' ) THEN
                  COECST = .FALSE.
                ENDIF
                ZI(JINF+NCHAR+ICH) = MAX(3,ZI(JINF+NCHAR+ICH))
              ENDIF
              IF ( EXCHSE ) THEN
              DO 3261 , NRPASE = 1 , NBPASE
                NOPASE = ZK8(ADCHSE+NRPASE-1)
                IF ( NOPASE.NE.'        ' ) THEN
                  CALL TELLME ( 'F', 'NOM_FONCTION', LCHIN(1:19),
     &                           NOPASE, K24BID, IBID, REPK, IERD )
                  IF ( REPK.EQ.'OUI' ) THEN
                    CALL PSTYPA ( NBPASE, INPSCO, NOMCHA, NOPASE,
     >                            TYPEPS(K) )
                  ENDIF
                ENDIF
 3261         CONTINUE
              ENDIF
            ELSE
              ZI(JINF+NCHAR+ICH) = MAX(1,ZI(JINF+NCHAR+ICH))
            ENDIF
          ENDIF
  326   CONTINUE

   32 CONTINUE

      IF ( NCHCI .GT. 0 ) CALL JEECRA (LCHCI,'LONUTI',NCHCI,K8BID)
      CALL JEDETR ( CHARSE )

      ENDIF

C FIN ------------------------------------------------------------------
      CALL JEDEMA()

      END

      SUBROUTINE OP0039 ()
      IMPLICIT   NONE
C ----------------------------------------------------------------------
C MODIF PREPOST  DATE 09/07/2012   AUTEUR PELLET J.PELLET 
C RESPONSABLE SELLENET N.SELLENET
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C TOLE CRP_20
C
C     BUT:
C       IMPRIMER DES RESULTATS ET DES MAILLAGE
C       PROCEDURE IMPR_RESU
C
C
C     ARGUMENTS:
C     ----------
C
C      ENTREE :
C-------------
C
C      SORTIE :
C-------------
C
C ......................................................................
C
C
C
C
      INCLUDE 'jeveux.h'
      INTEGER NOCC, IOCC, IOC2, NBREST
      INTEGER VALI
      INTEGER IFC, IFI
      INTEGER VERSIO
      INTEGER INFMAI
      INTEGER NIVE
      INTEGER N, NC, NM, NRES, NMI, NMO, NN, NP, NPA, NR
      INTEGER N0, N1, N10, N11, N01, N2, N21, N22, N23, N3, N4, N24
      INTEGER NUMEMO, NBMODL, NMAIL, NCHAR
      INTEGER IUTIL, IDEBU
      INTEGER IMXGM,  IMXGN,  IMXNO, IMXMA
      INTEGER NBMA, NBNO, NBGRM, NBGRN
      INTEGER NBNOMX, NBGNMX, NBMAMX, NBGMMX
      INTEGER NBNOS, NBNOT, NBNOU, NBMAT, NBCMP, NBNOSY, NBORDR, NBCMDU
      INTEGER NBPARA, JPARA, JLAST, JNOSY, JPA, JORDR, ISY, JCMP, JNCMED
      INTEGER JMODL, JNUNOS, JNUNOT, JTOPO, JNUMA, JLMA, JMMA, JNUNOU
      INTEGER JLGRM, JNGRM, JLGRN, JNGRN, JLNO, JNNO
      INTEGER NBELE, NBNOE
      INTEGER NINF, NSUP, NMA
      INTEGER I, J, JINDNO, JNOFI, II, IER
      INTEGER IBID, IRET, ICMP, NVCMP, JVCMP
      INTEGER GD, NCMPMX,IAD, NBCMPT
      INTEGER LXLGUT,NVARI
C
      REAL*8 PREC, BORSUP, BORINF, VERSI2, EPS
C
      CHARACTER*1  CECR,K1BID,K1OCC
      CHARACTER*3  TOUPAR,TOUCHA,COOR,TMAX,TMIN,SAUX03,VARIEL
      CHARACTER*4  PARTIE
      CHARACTER*8  MODELE,NOMA,NOMA2,FORM,NOMMA,TABL,NOMARE,NOMSQ
      CHARACTER*8  K8B,RESU,CRIT,TEXTE,NOMAB,TYCHA,RESURE(9),RESUR(9)
      CHARACTER*8  LERESU,NOMGD,RESMED
      CHARACTER*16 NOMCMD,K16BID,FORMR,FICH,TYRES
      CHARACTER*19 NOCH19,KNUM,RESU19
      CHARACTER*24 NOMJV
      CHARACTER*24 VALK(6)
      CHARACTER*24 CORRN,CORRM,NOVCMP
      CHARACTER*64 K64BID
      CHARACTER*80 TITRE
C
      LOGICAL LRESU,LCOR,LMAX,LMIN,LINF,LSUP,LCASTS,LMOD,LGMSH,ULEXIS
      LOGICAL LMAIL,AFAIRE,LREST,LNCMED,LVARIE
      INTEGER      IARG
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFMAJ()

      NBCMDU = 0
      BORINF = 0.D0
      BORSUP = 0.D0
      NOVCMP = '&&OP0039.NOM_CH_MED'

C     --- RECUPERATION DU NOM DE LA COMMANDE ---
      CALL GETRES ( K8B, K8B, NOMCMD )

C     --- RECUPERATION DU NOMBRE DE MISES EN FACTEUR DU MOT-CLE RESU ---
      CALL GETFAC ( 'RESU', NOCC )

      DO 100 IOCC = 1,NOCC
       CALL GETVTX('RESU','NOEUD_CMP',IOCC,IARG,0,K8B,NMO)
       IF(NMO.NE.0) THEN
         NN = NMO / 2
         IF(2*NN.NE.NMO) THEN
           CALL U2MESS('F','PREPOST3_65')
         ENDIF
       ENDIF
  100 CONTINUE

C     -----------------
C     --- LE MODELE ---
C     -----------------
      LMOD   = .FALSE.
      MODELE = ' '
      CALL GETVID ( ' ', 'MODELE'  ,1,IARG,1, MODELE, N )
      IF ( N .NE. 0 ) LMOD= .TRUE.



C     ----------------------------------------------------------------
C     TRAITEMENT DU MOT CLE "RESTREINT"
C     ----------------------------------------------------------------
C     -- REMARQUE : LE MOT CLE RESTREINT CREE DES SD_RESULTAT
C        TEMPORAIRES + 1 MAILLAGE TEMPORAIRE ET CE SONT CES
C        SD QUI DOIVENT ETRE IMPRIMEES.
C        C'EST POURQUOI, DANS LE RESTE DE L'OPERATEUR (QUI IMPRIME),
C        ON DOIT REMPLACER :
C          - GETVID/RESULTAT PAR RESURE(IOCC)
C          - GETVID/MAILLAGE PAR NOMARE
      LREST=.FALSE.
      CALL GETFAC('RESTREINT',NBREST)
      CALL ASSERT(NBREST.EQ.0.OR.NBREST.EQ.1)
      IF (NBREST.EQ.1) THEN
        LREST=.TRUE.
C       -- SI RESTREINT, IL FAUT VERIFIER QUE RESU/RESULTAT EST
C          TOUJOURS FOURNI :
        CALL ASSERT(NOCC.LE.9)
        DO 74,IOCC=1,NOCC
          CALL GETVID('RESU','RESULTAT',IOCC,IARG,1,RESUR(IOCC),NM)
          IF (NM.EQ.0) CALL U2MESS('F','CALCULEL4_5')
  74    CONTINUE

C       -- ON VERIFIE QUE TOUS LES RESUR ONT LE MEME MAILLAGE
        CALL DISMOI('F','NOM_MAILLA',RESUR(1),'RESULTAT',IBID,NOMA,IER)
        DO 76,IOCC=2,NOCC
          CALL DISMOI('F','NOM_MAILLA',RESUR(IOCC),'RESULTAT',IBID,
     &                 NOMA2,IER)
          CALL ASSERT(NOMA2.NE.' ')
          IF (NOMA2.NE.NOMA) THEN
            VALK(1)=RESUR(1)
            VALK(2)=RESUR(IOCC)
            VALK(3)=NOMA
            VALK(4)=NOMA2
            CALL U2MESK('F','CALCULEL4_2',4,VALK)
          ENDIF
 76     CONTINUE

C       -- ON VA FABRIQUER :
C          NOMARE : 1 SD_MAILLAGE "RESTREINT"
C          RESURE : NOCC SD_RESULTAT "RESTREINT"
        NOMARE='&&OP0039'
        CORRN='&&OP0039.CORRN'
        CORRM='&&OP0039.CORRM'
        CALL RDTMAI(NOMA,NOMARE,'V',CORRN,CORRM,'V',0,0)

        DO 77, IOCC=1,NOCC
          CALL CODENT(IOCC,'D',K1OCC)
          RESURE(IOCC)='&RESUR'//K1OCC//'_'
          CALL RDTRES(RESUR(IOCC),RESURE(IOCC),NOMA,NOMARE,CORRN,
     &                CORRM,IOCC)
 77     CONTINUE
      ENDIF



C     ---------------------------------------------
C     --- FORMAT, FICHIER ET UNITE D'IMPRESSION ---
C     ---------------------------------------------
C
C     --- FORMAT ---
      CALL GETVTX ( ' ', 'FORMAT'  ,1,IARG,1, FORM , N )
      IF (LREST.AND.FORM.NE.'MED') CALL U2MESS('F','CALCULEL4_3')

C     --- VERIFICATION DE LA COHERENCE ENTRE LE MAILLAGE ---
C     --- PORTANT LE RESULTAT ET LE MAILLAGE DONNE PAR   ---
C     --- L'UTILISATEUR DANS IMPR_RESU(FORMAT='IDEAS')   ---

      IF (FORM(1:5) .EQ. 'IDEAS') THEN
         CALL GETVID('RESU','RESULTAT',1,IARG,1,RESU,NRES)
         CALL GETVID('RESU','MAILLAGE',1,IARG,1,NOMA,NMAIL)
         IF (NRES*NMAIL .GT.0) THEN
            CALL DISMOI('F','NOM_MAILLA',RESU,'RESULTAT',IBID,NOMSQ,IER)
            IF (NOMSQ .NE. NOMA) THEN
              VALK(1)=NOMA
              VALK(2)=NOMSQ
              VALK(3)=RESU
              CALL U2MESK('A','PREPOST3_74',3,VALK)
            ENDIF
         ENDIF
      ENDIF

C
C     --- VERSION D'ECRITURE  ----
      NIVE = 0
      VERSIO = 0
      LCASTS = .FALSE.
      LGMSH = .FALSE.
      IF ( FORM .EQ. 'CASTEM' ) THEN
         LCASTS = .TRUE.
         CALL GETVIS ( ' ', 'NIVE_GIBI', 1,IARG,1, NIVE, N )

      ELSEIF ( FORM(1:5) .EQ. 'IDEAS' ) THEN
         VERSIO = 5
         CALL GETVIS ( ' ', 'VERSION', 1,IARG,1, VERSIO, N )

      ELSEIF ( FORM(1:4) .EQ. 'GMSH' ) THEN
         VERSIO = 1
         VERSI2 = 1.0D0
         EPS    = 1.0D-6
         CALL GETVR8 ( ' ', 'VERSION', 1,IARG,1, VERSI2, N )
         IF (VERSI2.GT.1.0D0-EPS.AND.VERSI2.LT.1.0D0+EPS) THEN
            VERSIO = 1
         ELSEIF (VERSI2.GT.1.2D0-EPS.AND.VERSI2.LT.1.2D0+EPS) THEN
            VERSIO = 2
         ENDIF
      ENDIF
C
C     --- FICHIER ---
      IFI = 0
      FICH = 'F_'//FORM
      CALL GETVIS ( ' ', 'UNITE', 1,IARG,1, IFI , N11 )
      IFC = IFI
      IF ( .NOT. ULEXIS( IFI ) ) THEN
         CALL ULOPEN ( IFI, ' ', FICH, 'NEW', 'O' )
      ENDIF


C     -- FORMAT CASTEM : IMPRESSION DU MAILLAGE :
C     -------------------------------------------
      IF ( FORM .EQ. 'CASTEM' ) THEN
         NUMEMO = 0
         NOMJV  = '&&OP0039.NOM_MODELE'
         INFMAI = 0
         DO 200 IOCC = 1,NOCC
            IF ( NUMEMO .EQ. 0 ) THEN
               IF ( LMOD ) THEN
                  NBMODL = 1
                  CALL WKVECT ( NOMJV, 'V V K24', 10, JMODL )
                  CALL JEECRA ( NOMJV , 'LONUTI' , NBMODL , ' ')
                  CALL JEVEUO ( NOMJV, 'E', JMODL )
                  ZK24(JMODL) = MODELE//'.MODELE'
               ENDIF
               DO 202 IOC2 = 1 , NOCC
                 CALL GETVID('RESU','RESULTAT',IOC2,IARG,1,RESU,NR)
                 IF ( NR .NE. 0 ) CALL RSCRMO ( IOC2, RESU , NOMJV)
 202           CONTINUE
               NUMEMO = NUMEMO + 1
            ENDIF

C           ---  IMPRESSION DU MAILLAGE -----
            CALL GETVID('RESU','MAILLAGE',IOCC,IARG,1,NOMA,NM)
            IF ( NM .NE. 0 ) THEN
               IF ( LMOD  ) THEN
                  CALL DISMOI('C','NOM_MAILLA',MODELE,'MODELE',IBID,
     &                                                 NOMAB,IRET)
                  IF (NOMA.NE.NOMAB) CALL U2MESS('F','PREPOST3_66')
               ENDIF
               FORMR=' '
               CALL IRMAIL ( FORM,IFI,VERSIO,NOMA,LMOD,MODELE,NIVE,
     &                     INFMAI, FORMR )
               NUMEMO = NUMEMO + 1
            ENDIF
 200     CONTINUE

         IF ( NUMEMO .LE. 1 ) CALL U2MESS('F','PREPOST3_67')
C
         CALL JEEXIN('&&OP0039.LAST',IRET)
         IF(IRET.EQ.0)  CALL WKVECT('&&OP0039.LAST','V V I',8,JLAST)
      ENDIF



C     -- VERIFICATIONS POUR GMSH :
      IF (FORM(1:4).EQ.'GMSH') THEN
         LMAIL=.FALSE.
         LRESU=.FALSE.
         DO 220 IOCC = 1 , NOCC
            CALL GETVID('RESU','MAILLAGE',IOCC,IARG,1,NOMA,NM)
            CALL GETVID('RESU','RESULTAT',IOCC,IARG,1,RESU,NR)
            CALL GETVID('RESU','CHAM_GD',IOCC,IARG,1,RESU,NC)
            IF ( NR.NE.0 .OR. NC.NE.0 ) THEN
               LRESU=.TRUE.
               GOTO 220
            ENDIF
            IF ( NM.NE.0 )  LMAIL=.TRUE.
 220     CONTINUE
         IF ( LMAIL.AND.LRESU ) THEN
            CALL U2MESS('A','PREPOST3_68')
            GOTO 9999
         ENDIF
      ENDIF


C
C     --- BOUCLE SUR LE NOMBRE DE MISES EN FACTEUR ---
C     -----------------------------------------------------------------
      DO 10 IOCC = 1,NOCC
         LMAX = .FALSE.
         LMIN = .FALSE.
         LINF = .FALSE.
         LSUP = .FALSE.
         LCOR = .FALSE.
         CECR = 'L'
         AFAIRE = .FALSE.
C
C        --- FORMAT D'ECRITURE DES REELS ---
         CALL GETVTX('RESU','FORMAT_R',IOCC,IARG,1,FORMR,N)
C
C        --- MODE D'ECRITURE DES PARAMETRES------
C            (RMQUE: UNIQUEMENT INTERESSANT POUR FORMAT 'RESULTAT')
         CALL GETVTX ( 'RESU', 'FORM_TABL', IOCC,IARG,1, TABL, N )
         IF ( N .NE. 0 ) THEN
            IF ( TABL(1:3) .EQ. 'OUI' ) THEN
               CECR = 'T'
            ELSEIF ( TABL(1:5) .EQ. 'EXCEL' ) THEN
               CECR = 'E'
            ENDIF
         ENDIF
C
C        --- IMPRESSION DES COORDONNEES------
C            (ECRITURE VARIABLES DE TYPE RESULTAT AU FORMAT 'RESULTAT')
         COOR = ' '
         CALL GETVTX('RESU','IMPR_COOR',IOCC,IARG,1,COOR,N)
         IF (N.NE.0 .AND. COOR.EQ.'OUI') LCOR = .TRUE.
C
C        --- SEPARATION DES DIFFERENTES OCCURENCES (FORMAT 'RESULTAT')
         IF (FORM.EQ.'RESULTAT') WRITE(IFI,'(/,1X,80(''-''))')
C
C        --- RECHERCHE TYPE DE DONNEES A TRAITER POUR L'OCCURENCE IOCC
C        *** VARIABLE DE TYPE RESULTAT (NR!=0) OU CHAMP_GD (NC!=0)
         RESU = ' '
         PARTIE = ' '
         CALL GETVID('RESU','RESULTAT',IOCC,IARG,1,RESU,NR)
         IF (LREST) THEN
           NR=1
           RESU=RESURE(IOCC)
         ENDIF
         CALL GETVTX('RESU','PARTIE',IOCC,IARG,1,PARTIE,NP)
         IF(NR.NE.0)THEN
            CALL GETTCO(RESU,TYRES)
            IF(TYRES(1:10).EQ.'DYNA_HARMO' .OR.
     &         TYRES(1:10).EQ.'ACOU_HARMO')THEN
               IF(FORM(1:4).EQ.'GMSH'  .OR.
     &            FORM(1:6).EQ.'CASTEM'.OR.
     &            FORM(1:3).EQ.'MED'   )THEN
                    IF(NP.EQ.0)
     &                  CALL U2MESS('F','PREPOST3_69')
               ENDIF
            ENDIF
         ENDIF

         CALL GETVID('RESU','CHAM_GD' ,IOCC,IARG,1,RESU,NC)
         IF(NC.NE.0)THEN
            RESU19=RESU
            CALL DISMOI('C','NOM_GD',RESU19,'CHAMP',IBID,NOMGD,IER)
            IF(NOMGD(6:6).EQ.'C')THEN
              IF(FORM(1:4).EQ.'GMSH' .OR. FORM(1:6).EQ.'CASTEM')THEN
                   IF(NP.EQ.0)
     &                  CALL U2MESS('F','PREPOST3_69')
              ENDIF
            ENDIF
         ENDIF

         LRESU = NR.NE.0
C          --- TEST PRESENCE DU MOT CLE INFO_MAILLAGE (FORMAT 'MED')
         INFMAI = 1
         CALL GETVTX('RESU','INFO_MAILLAGE',IOCC,IARG,1,SAUX03,N01)
         IF (N01.NE.0) THEN
           IF (SAUX03.EQ.'OUI'.AND.FORM.EQ.'MED') THEN
             INFMAI = 2
           ELSEIF (SAUX03.EQ.'OUI'.AND.FORM.NE.'MED') THEN
             CALL U2MESS('A','MED_63')
           ENDIF
         ENDIF
C
C
C        *** MAILLAGE AVEC OU SANS MODELE
C             SI LE MOT-CLE 'MODELE' EST PRESENT DANS LA COMMANDE
C             ET QUE L'ON DEMANDE L'IMPRESSION DU MAILLAGE, IL NE FAUDRA
C             IMPRIMER QUE LA PARTIE DU MAILLAGE AFFECTEE DANS LE MODELE
         NOMA   = ' '
         NOMAB   = ' '
         CALL GETVID('RESU','MAILLAGE', IOCC,IARG,1, NOMA, NM )
         IF ( (FORM.EQ.'ASTER').AND.(NOMA.EQ.'        ')) THEN
               CALL U2MESS('A','PREPOST3_70')
         ENDIF
         IF (LREST) THEN
           IF (IOCC.EQ.1) THEN
             NM=1
             NOMA=NOMARE
           ELSE
             NM=0
           ENDIF
         ENDIF
C
C        --- TEST DE LA COHERENCE DU MAILLAGE ET DU MODELE ---
C
         IF ( LMOD .AND. NM.NE.0 ) THEN
          CALL DISMOI('C','NOM_MAILLA',MODELE,'MODELE',IBID,NOMAB,IRET)
          IF (NOMA.NE.NOMAB) THEN
               CALL U2MESS('F','PREPOST3_66')
          ENDIF
         ENDIF
C
C===================================
         NBCMP = 0
         NBMAT = 0
         NBNOS = 0
         NBNOT = 0
         NBNOU = 0
         LERESU = RESU

C        --- ECRITURE DU TITRE ---
C             SI NOMA = ' ' ON NE DEMANDE PAS L'IMPRESSION DU MAILLAGE
C             IRTITR SE RESUME ALORS A L'ECRITURE D'UN TITRE DANS UN K80
         CALL IRTITR(RESU,NOMA,FORM,IFI,TITRE)
C
C        ---  IMPRESSION DU MAILLAGE AU PREMIER PASSAGE -----
         IF( NM.NE.0 .AND. FORM.NE.'CASTEM') THEN
           IF (FORM(1:4).NE.'GMSH'.OR.(NR.EQ.0.AND.NC.EQ.0)) THEN
             CALL IRMAIL ( FORM, IFI, VERSIO, NOMA, LMOD, MODELE, NIVE,
     &                     INFMAI, FORMR )
           ENDIF
         ENDIF
C
C        --- ECRITURE D'UN CHAM_GD ---
         LNCMED = .FALSE.
         IF(NC .NE.0) THEN
           NBNOSY = 1
           CALL WKVECT('&&OP0039.NOM_SYMB','V V K16',NBNOSY,JNOSY)
           CALL WKVECT(NOVCMP,'V V K80',NBNOSY,JNCMED)
C          --- NOM DU CHAM_GD ---
           ZK16(JNOSY)  = LERESU
           NPA       = 0
           JPA       = 1
           NBORDR    = 1
           CALL WKVECT('&&OP0039.NUM_ORDR','V V I',NBORDR,JORDR)
           ZI(JORDR) = 1
           CALL GETVTX('RESU','NOM_CHAM_MED' ,IOCC,IARG,0,K64BID,N23)
           NBCMDU = - N23
           CALL GETVTX('RESU','NOM_CHAM_MED',IOCC,IARG,NBCMDU,
     &                                              ZK80(JNCMED),IRET)
           LNCMED = .TRUE.
C
C        --- ECRITURE D'UN RESULTAT_COMPOSE ---
         ELSEIF (NR.NE.0) THEN
C          --- ON REGARDE QUELS SONT LES NOM_CHAM A IMPRIMER:
           TOUCHA = 'OUI'
           CALL GETVTX('RESU','TOUT_CHAM',IOCC,IARG,1,TOUCHA,N21)
           CALL GETVTX('RESU','NOM_CHAM' ,IOCC,IARG,0,K16BID,N22)
           CALL GETVTX('RESU','NOM_CHAM_MED' ,IOCC,IARG,0,K64BID,N23)
           CALL GETVTX('RESU','NOM_RESU_MED' ,IOCC,IARG,0,K8B,N24)
C          *** N22 EST NEGATIF SI L'UTILISATEUR DONNE UNE LISTE DE NOMS
C              (PAR DEFAUT TOUS LES CHAMPS CAR MOT-CLE FACULTATIF)
           IF(ABS(N21)+ABS(N22).EQ.0) N21=1
           IF(N21.GT.0 .AND. TOUCHA.EQ.'OUI'.AND.N24.EQ.0) THEN
C            - ON RECUPERE LES NOMS (ON IMPRIME TOUS LES CHAMPS)
             CALL JELIRA(LERESU//'           .DESC','NOMUTI',
     &                   NBNOSY,K1BID)
             CALL WKVECT('&&OP0039.NOM_SYMB','V V K16',
     &                   NBNOSY,JNOSY)
             DO 12 ISY = 1,NBNOSY
               CALL JENUNO(JEXNUM(LERESU//'           .DESC',ISY),
     &                                         ZK16(JNOSY-1+ISY))
   12        CONTINUE
           ELSEIF (N21.GT.0 .AND. TOUCHA.EQ.'NON') THEN
             NBNOSY=0
             JNOSY =1
           ELSEIF(N22.LT.0) THEN
             NBNOSY = - N22
             NBCMDU = - N23
             CALL WKVECT('&&OP0039.NOM_SYMB','V V K16',
     &                   NBNOSY,JNOSY)
             CALL WKVECT(NOVCMP,'V V K80',NBNOSY,JNCMED)

C            - ON RECUPERE LA LISTE DES NOMS DONNEE PAR L'UTILISATEUR
             CALL GETVTX('RESU','NOM_CHAM',IOCC,IARG,NBNOSY,
     &                                                ZK16(JNOSY),N0)
             CALL GETVTX('RESU','NOM_CHAM_MED',IOCC,IARG,
     &                                         NBCMDU,ZK80(JNCMED),IRET)
             LNCMED = .TRUE.
             IF ((NBCMDU.NE.0).AND.(NBCMDU.NE.NBNOSY)) THEN
               CALL U2MESS('F','PREPOST2_1')
             ENDIF
           ELSEIF(N24.LT.0) THEN
             CALL GETVTX('RESU','NOM_CMP',IOCC,IARG,0,K8B,N)
             IF(N.LT.0) THEN
                VALK(1)='NOM_CMP'
                VALK(2)='NOM_RESU_MED'
                CALL U2MESK('F','MED2_6',2,VALK)
             ENDIF
             LNCMED = .TRUE.
             CALL JELIRA(LERESU//'           .DESC','NOMUTI',NBCMDU,K8B)
             CALL GETVTX('RESU','NOM_RESU_MED' ,IOCC,IARG,1,RESMED,N24)
             CALL WKVECT('&&OP0039.NOM_SYMB','V V K16',NBCMDU,JNOSY)
             CALL WKVECT(NOVCMP,'V V K80',NBCMDU,JNCMED)
             NBNOSY=NBCMDU
             DO 13 ISY = 1,NBCMDU
               CALL JENUNO(JEXNUM(LERESU//'           .DESC',ISY),
     &                                         ZK16(JNOSY-1+ISY))
               ZK80(JNCMED+ISY-1)='________'
               NCHAR=LXLGUT(RESMED)
               ZK80(JNCMED+ISY-1)(1:NCHAR)=RESMED(1:NCHAR)
               NCHAR=LXLGUT(ZK16(JNOSY-1+ISY))
               ZK80(JNCMED+ISY-1)(9:8+NCHAR)=ZK16(JNOSY-1+ISY)(1:NCHAR)
   13        CONTINUE
           ENDIF

C          --- ON REGARDE QUELS SONT LES NOM_CMP A IMPRIMER:
           CALL GETVTX('RESU','NOM_CMP',IOCC,IARG,0,K8B,N)
           IF(N.LT.0) THEN
             NVCMP=-N
             CALL WKVECT('&&OP0039.VERI_NOM_CMP','V V K8',NVCMP,JVCMP)
             CALL GETVTX('RESU','NOM_CMP',IOCC,IARG,NVCMP,
     &                   ZK8(JVCMP),IBID)
             AFAIRE = .TRUE.
           ENDIF

C          *** NOMS DES CHAMPS DANS ZK16 A PARTIR DE ZK16(JNOSY)
C
C          --- NUMEROS D'ORDRE POUR IOCC DU MOT-CLE FACTEUR RESU
           KNUM = '&&OP0039.NUME_ORDRE'
C          *** TEST DE PRESENCE DES MOTS CLES PRECISION ET CRITERE
           CALL GETVR8('RESU','PRECISION',IOCC,IARG,1,PREC,NP)
           CALL GETVTX('RESU','CRITERE'  ,IOCC,IARG,1,CRIT,NC)
C          *** RECUPERATION DES NUMEROS D'ORDRE DE LA STRUCTURE DE
C             DONNEES DE TYPE RESULTAT LERESU A PARTIR DES VARIABLES
C             D'ACCES UTILISATEUR 'NUME_ORDRE','FREQ','INST','NOEUD_CMP'
C              (VARIABLE D'ACCES 'TOUT_ORDRE' PAR DEFAUT)
           CALL RSUTNU ( LERESU,'RESU',IOCC,KNUM,NBORDR,PREC,CRIT,IRET)
C          *** SI PB ON PASSE AU FACTEUR SUIVANT DE IMPR_RESU
           IF (IRET.NE.0) GOTO 99
           CALL JEVEUO ( KNUM, 'L', JORDR )
C
           IF( N22 .LT. 0 ) THEN
           DO 16 I = 0 , NBNOSY-1
             DO 14 J = 0 , NBORDR-1
               CALL RSEXCH (LERESU,ZK16(JNOSY+I),ZI(JORDR+J),
     &                      NOCH19,IRET)
               IF ( IRET .NE. 0 ) THEN
               VALK (1) = ZK16(JNOSY+I)
               VALI = ZI(JORDR+J)
                  CALL U2MESG('A', 'POSTRELE_41',1,VALK,1,VALI,0,0.D0)
               ENDIF
 14          CONTINUE
 16        CONTINUE
           IF ( AFAIRE ) THEN
           DO 15 ICMP = 0 , NVCMP-1
             NBCMPT = 0
             DO 17 I = 0 , NBNOSY-1
               CALL RSEXCH (LERESU,ZK16(JNOSY+I),ZI(JORDR),NOCH19,IRET)
               IF ( IRET .EQ. 0 ) THEN
                 CALL DISMOI('F','NUM_GD',NOCH19,'CHAMP',GD,K8B,IBID)
                 CALL JENUNO(JEXNUM('&CATA.GD.NOMGD',GD),NOMGD)
                 IF ( NOMGD .EQ. 'VARI_R' ) THEN
C                   TRAITEMENT PARTICULIER POUR LA GRANDEUR VARI_R
                    NBCMPT = NBCMPT + 1
                    GOTO 17
                 ENDIF
                 CALL JELIRA(JEXNUM('&CATA.GD.NOMCMP',GD),'LONMAX',
     &                       NCMPMX,K1BID)
                 CALL JEVEUO(JEXNUM('&CATA.GD.NOMCMP',GD),'L',IAD)
                 CALL IRVCMP(NCMPMX,ZK8(IAD),ZK8(JVCMP+ICMP),NBCMPT)
               ENDIF
 17          CONTINUE
             IF ( NBCMPT .EQ. 0 ) THEN
               VALK (1) = ZK8(JVCMP+ICMP)
               VALK (2) = K1BID
               CALL U2MESG('A', 'PREPOST5_61',2,VALK,0,0,0,0.D0)
             ENDIF
 15        CONTINUE
           ENDIF
           ENDIF
C
C          --- ON RECHERCHE LES PARAMETRES A ECRIRE ---
C              (UNIQUEMENT SI FORMAT FICHIER = 'RESULTAT')
           TOUPAR = 'NON'
           CALL GETVTX('RESU','TOUT_PARA',IOCC,IARG,1,TOUPAR,N11)
           CALL GETVTX('RESU','NOM_PARA' ,IOCC,IARG,0,K8B   ,N10)
           IF(N10.EQ.0 ) N11 = 1
           IF(N11.NE.0.AND.TOUPAR.EQ.'NON') THEN
             NPA =  0
             JPA =  1
           ELSEIF(N11.NE.0.AND.TOUPAR.EQ.'OUI') THEN
             NPA = -1
             JPA =  1
           ELSEIF(N10.NE.0) THEN
             NPA = -N10
             CALL WKVECT('&&OP0039.NOMUTI_PARA','V V K16',NPA,JPA)
             CALL GETVTX('RESU','NOM_PARA',IOCC,IARG,NPA,ZK16(JPA),NPA)
           ENDIF
         ENDIF
C
C        **************************************************************
C        ON RENTRE DANS CE QUI SUIT SAUF SI ON IMPRIME LE MAILLAGE
C          NC!=0 SI CHAM_GD, NR!=0 SI RESULTAT COMPOSE DE TYPE RESULTAT
C        **************************************************************
         IF( NC.NE.0 .OR. NR.NE.0 ) THEN
C
C          --- TRAITEMENT DES NOEUDS,MAILLES,GPES DE NOEUDS ET MAILLES
C              (OPERANDE DE SELECTION SUR DES ENTITES TOPOLOGIQUES)
           NBNO=0
           NBGRN=0
           NBMA=0
           NBGRM=0
           CALL GETVTX('RESU','NOEUD'   ,IOCC,IARG,0,K8B,N1)
           CALL GETVTX('RESU','GROUP_NO',IOCC,IARG,0,K8B,N2)
           CALL GETVTX('RESU','MAILLE'  ,IOCC,IARG,0,K8B,N3)
           CALL GETVTX('RESU','GROUP_MA',IOCC,IARG,0,K8B,N4)
           IF((N1.NE.0.OR.N2.NE.0.OR.N3.NE.0.OR.N4.NE.0)
     &          .AND. (FORM(1:6).EQ.'CASTEM')) THEN
             CALL U2MESS('A','PREPOST3_73')
           ENDIF

C          *** ON S'ALLOUE UN TABLEAU DE 8 ENTIERS POUR LA TOPOLOGIE
           CALL WKVECT('&&OP0039.LIST_TOPO','V V I',8,JTOPO)
C
           IF(FORM(1:6).NE.'CASTEM') THEN
C            *** CAS D'UNE LISTE DE GROUPES DE NOEUDS
             IF(N2.LT.0) THEN
               NBGRN = -N2
C              - ON S'ALLOUE :
C                UN TABLEAU DE K8, LISTE DES NOMS DE GPES DE NOEUDS
C                UN TABLEAU DE K80 (POUR FORMAT 'RESULTAT')
               CALL WKVECT('&&OP0039.LIST_GRNO','V V K8',
     &                     NBGRN,JLGRN)
               CALL WKVECT('&&OP0039.NOM_GRNO','V V K80',
     &                     NBGRN,JNGRN)
               CALL GETVTX('RESU','GROUP_NO',IOCC,IARG,NBGRN,
     &                                               ZK8(JLGRN),IBID)
               ZI(JTOPO-1+3) = NBGRN
             ENDIF
C
C            *** CAS D'UNE LISTE DE NOEUDS
             IF(N1.LT.0) THEN
               NBNO = -N1
C              - ON S'ALLOUE :
C                UN TABLEAU DE K8, LISTE DES NOMS DE NOEUDS
C                UN TABLEAU DE K80 (POUR FORMAT 'RESULTAT')
               CALL WKVECT('&&OP0039.LIST_NOE','V V K8',NBNO,JLNO)
               CALL WKVECT('&&OP0039.NOM_NOE','V V K80',NBNO,JNNO)
               CALL GETVTX('RESU','NOEUD',IOCC,IARG,NBNO,ZK8(JLNO),IBID)
               ZI(JTOPO-1+1) = NBNO
             ENDIF
C
C            *** CAS D'UNE LISTE DE GROUPES DE MAILLES
             IF(N4.LT.0) THEN
               NBGRM = -N4
C              - ON S'ALLOUE :
C                UN TABLEAU DE K8, LISTE DES NOMS DE GPES DE MAILLES
C                UN TABLEAU DE K80 (POUR FORMAT 'RESULTAT')
               CALL WKVECT('&&OP0039.LIST_GRMA','V V K8',
     &                     NBGRM,JLGRM)
               CALL WKVECT('&&OP0039.NOM_GRMA','V V K80',
     &                     NBGRM,JNGRM)
               CALL GETVTX('RESU','GROUP_MA',IOCC,IARG,NBGRM,
     &                                               ZK8(JLGRM),IBID)
               ZI(JTOPO-1+7) = NBGRM
             ENDIF
C
C            ***  CAS D'UNE LISTE DE MAILLES
             IF(N3.LT.0) THEN
               NBMA = -N3
C              - ON S'ALLOUE :
C                UN TABLEAU DE K8, LISTE DES NOMS DE MAILLES
C                UN TABLEAU DE K80 (POUR FORMAT 'RESULTAT')
               CALL WKVECT('&&OP0039.LIST_MAI','V V K8',NBMA,JLMA)
               CALL WKVECT('&&OP0039.NOM_MAI','V V K80',NBMA,JMMA)
               CALL GETVTX('RESU','MAILLE',IOCC,IARG,NBMA,
     &                     ZK8(JLMA),IBID)
               ZI(JTOPO-1+5) = NBMA
             ENDIF
C
C            ***  IL Y A SELECTION EN OPERANDE SUR NOEUDS OU MAILLES
C                 OU DES GROUPES DE NOEUDS OU DES GROUPES DE MAILLES
             IF(NBNO.NE.0.OR.NBGRN.NE.0
     &          .OR.NBMA.NE.0.OR.NBGRM.NE.0) THEN
               IF(NR.NE.0) THEN
C                - C'EST UN RESULTAT COMPOSE: NOM DU MAILLAGE DANS NOMMA
                 CALL DISMOI('F','NOM_MAILLA',LERESU,'RESULTAT',IBID,
     &                       NOMMA,IBID)
               ELSEIF (NC.NE.0) THEN
C                - C'EST UN CHAM_GD: NOM DU MAILLAGE DANS NOMMA
                 CALL DISMOI('F','NOM_MAILLA',LERESU,'CHAMP',IBID,
     &                       NOMMA,IBID)
               ENDIF
C              - NOMBRE TOTAL DE NOEUDS DU MAILLAGE NOMMA = NBNOE
               CALL DISMOI('F','NB_NO_MAILLA',NOMMA,'MAILLAGE',
     &                                               NBNOE,K8B,IBID)
               CALL WKVECT('&&OP0039.IND_NOEU','V V I',NBNOE,JINDNO)
               DO 70 I=1,NBNOE
                  ZI(JINDNO+I-1)=0
 70            CONTINUE
             ENDIF
C
C            ***  SELECTION SUR DES NOEUDS OU GROUPES DE NOEUDS
             IF( NBNO.NE.0 .OR. NBGRN.NE.0 ) THEN
C              - ON S'ALLOUE UN TABLEAU D'ENTIERS A PARTIR DE ZI(JNUNOU)
C                POUR LA LISTE DES NUMEROS DES NOEUDS A IMPRIMER
               CALL WKVECT('&&OP0039.NUMNOE','V V I',NBNOE,JNUNOU)
C              - ON RECUPERE A PARTIR DE ZI(JNUNOU) LES NUMEROS DES
C                NOEUDS DE LA LISTE DE NOEUDS OU DE GROUPES DE NOEUDS
C                (NBNOU EST LE NBRE TOTAL DE NOEUDS TROUVES A IMPRIMER)
               CALL IRNONO(NOMMA,NBNOE,NBNO,ZK8(JLNO),NBGRN,ZK8(JLGRN),
     &                    '&&OP0039.NUMNOE',NBNOU,ZI(JINDNO))
C              - ON RECUPERE DE NOUVEAU L'ADRESSE DE .NUMNOE CAR IRNONO
C                A PU AGRANDIR CET OBJET :
               CALL JEVEUO('&&OP0039.NUMNOE','L',JNUNOU)
               NBNOT = NBNOT + NBNOU
             ENDIF
C
C            ***  SELECTION SUR DES MAILLES OU GROUPES DE MAILLES
             IF(NBMA.NE.0.OR.NBGRM.NE.0) THEN
C              - ON S'ALLOUE UN TABLEAU POUR LES NUMEROS DES MAILLES ET
C                UN TABLEAU POUR LES NUMEROS DES NOEUDS DE CES MAILLES
               CALL JELIRA(NOMMA//'.NOMMAI','NOMMAX',NBELE,K1BID)
               CALL WKVECT('&&OP0039.NUMMAI','V V I',NBELE,JNUMA)
               CALL WKVECT('&&OP0039.NUMNOS','V V I',NBNOE,JNUNOS)
C              - ON RECUPERE A PARTIR DE ZI(JNUMA) LES NUMEROS DES
C                MAILLES DE LA LISTE DE MAILLES OU DE GROUPES DE MAILLES
C                (NBMAT = NBRE TOTAL DE MAILLES TROUVEES A IMPRIMER)
               CALL IRMAMA(NOMMA,NBMA,ZK8(JLMA),NBGRM,ZK8(JLGRM),
     &                     '&&OP0039.NUMMAI',NBMAT)
C              - ON RECUPERE DE NOUVEAU L'ADRESSE DE .NUMMAI CAR IRMAMA
C                A PU AGRANDIR CET OBJET :
               CALL JEVEUO('&&OP0039.NUMMAI','L',JNUMA)

C              - ON RECUPERE A PARTIR DE ZI(JNUNOS) LA LISTE DES NUMEROS
C                DES NOEUDS SOMMETS DE CES MAILLES
C               (NBNOS = NOMBRE DE NOEUDS SOMMETS DE CETTE LISTE)
               CALL IRMANO(NOMMA,NBMAT,ZI(JNUMA),NBNOS,ZI(JNUNOS))
               CALL WKVECT('&&OP0039.FILTRE_NO','V V I',NBNOS,JNOFI)
               II=0
               DO 490 I=1,NBNOS
                 IF(ZI(JINDNO+ZI(JNUNOS+I-1)-1).EQ.0)THEN
                    II=II+1
                    ZI(JNOFI+II-1)=ZI(JNUNOS+I-1)
                 ENDIF
 490           CONTINUE
               NBNOS=II
               NBNOT = NBNOT + NBNOS
             ENDIF
C
             IF ( NBNO.NE.0 .OR. NBGRN.NE.0 .OR.
     &            NBMA.NE.0 .OR. NBGRM.NE.0 ) THEN
                IF ( NBNOU.EQ.0 .AND. NBMAT.EQ.0 ) GOTO 99
             ENDIF
C
C            ***  ON CREE UNE LISTE DE NUMEROS DE NOEUDS ISSUS
C                 DE GROUP_NO ET GROUP_MA
             IF(NBNOT.GT.0) THEN
C              - ON S'ALLOUE UN TABLEAU POUR LES NUMEROS DE CES NOEUDS
               CALL WKVECT('&&OP0039.NUMNOT','V V I',NBNOT,JNUNOT)
             ENDIF
             IF( NBNOU .GT. 0 ) THEN
C              - LISTE DES NUMEROS DE NOEUDS
               DO 500 I=1,NBNOU
                 ZI(JNUNOT-1+I)=ZI(JNUNOU-1+I)
  500          CONTINUE
             ENDIF
             IF( NBNOS .GT. 0 ) THEN
C              - SUIVIE DE LA LISTE DES NUMEROS DE NOEUDS SOMMETS
               DO 501 I=1,NBNOS
                 ZI(JNUNOT-1+NBNOU+I)= ZI(JNOFI-1+I)
  501          CONTINUE
             ENDIF
           ENDIF
         ENDIF
C
C        ***************************************************************
C        - CHAM_GD OU RESULTAT COMPOSE AU FORMAT 'RESULTAT':
C          IMPRESSION LISTES DES NOMS DES NOEUDS ET MAILLES SELECTIONNES
C        ***************************************************************
         IF((NC.NE.0.OR.NR.NE.0).AND.(FORM.EQ.'RESULTAT')) THEN
           NBNOMX = ZI(JTOPO-1+2)
           NBGNMX = ZI(JTOPO-1+4)
           NBMAMX = ZI(JTOPO-1+6)
           NBGMMX = ZI(JTOPO-1+8)
           IMXNO = 0
           IMXGN = 0
           IMXMA = 0
           IMXGM = 0
           IF(NBNOMX.NE.0) THEN
             IDEBU = 12
             IMXNO = IMXNO+1
             DO 800 I=1,NBNO
               TEXTE = ZK8(JLNO-1+I)
               IUTIL = LXLGUT(TEXTE)
               IF(IUTIL.NE.0) THEN
                 IF((IDEBU+IUTIL).GT.80) THEN
                    IMXNO = IMXNO + 1
                    IDEBU = 1
                 ENDIF
                 ZK80(JNNO-1+IMXNO)(IDEBU:IDEBU+IUTIL)=TEXTE(1:IUTIL)
                 IDEBU=IDEBU+IUTIL+1
               ENDIF
 800         CONTINUE
           ENDIF
           IF(NBGNMX.NE.0) THEN
             IDEBU = 12
             IMXGN = IMXGN + 1
             DO 810 I=1,NBGRN
               TEXTE = ZK8(JLGRN-1+I)
               IUTIL = LXLGUT(TEXTE)
               IF(IUTIL.NE.0) THEN
                 IF((IDEBU+IUTIL).GT.80) THEN
                    IMXGN = IMXGN + 1
                    IDEBU = 1
                 ENDIF
                 ZK80(JNGRN-1+IMXGN)(IDEBU:IDEBU+IUTIL)=TEXTE(1:IUTIL)
                 IDEBU=IDEBU+IUTIL+1
               ENDIF
 810         CONTINUE
           ENDIF
           IF(NBGMMX.NE.0) THEN
             IDEBU = 12
             IMXGM = IMXGM + 1
             DO 820 I=1,NBGRM
               TEXTE = ZK8(JLGRM-1+I)
               IUTIL = LXLGUT(TEXTE)
               IF(IUTIL.NE.0) THEN
                 IF((IDEBU+IUTIL).GT.80) THEN
                    IDEBU = 1
                 ENDIF
                 ZK80(JNGRM-1+IMXGM)(IDEBU:IDEBU+IUTIL)=TEXTE(1:IUTIL)
                 IDEBU=IDEBU+IUTIL+1
               ENDIF
 820         CONTINUE
           ENDIF
           IF(NBMAMX.NE.0) THEN
             IDEBU = 12
             IMXMA = IMXMA + 1
             DO 830 I=1,NBMA
               TEXTE = ZK8(JLMA-1+I)
               IUTIL = LXLGUT(TEXTE)
               IF(IUTIL.NE.0) THEN
                 IF((IDEBU+IUTIL).GT.80) THEN
                    IMXMA = IMXMA + 1
                    IDEBU = 1
                 ENDIF
                 ZK80(JMMA-1+IMXMA)(IDEBU:IDEBU+IUTIL)=TEXTE(1:IUTIL)
                 IDEBU=IDEBU+IUTIL+1
               ENDIF
 830         CONTINUE
           ENDIF
           CALL JEVEUO('&&OP0039.LIST_TOPO','L',JTOPO)
           IF(IMXNO.NE.0.OR.IMXGN.NE.0.OR.IMXMA.NE.0.OR.
     &       IMXGM.NE.0)  WRITE(IFI,'(/,20X,A)') 'ENTITES '
     &               //'TOPOLOGIQUES SELECTIONNEES '
           IF(IMXNO.NE.0) THEN
             ZK80(JNNO-1+1)(1:11) = 'NOEUD    : '
             WRITE(IFI,'(1X,A80)') (ZK80(JNNO-1+I),I=1,IMXNO)
           ENDIF
           IF(IMXGN.NE.0) THEN
             ZK80(JNGRN-1+1)(1:11) = 'GROUP_NO : '
             WRITE(IFI,'(1X,A80)') (ZK80(JNGRN-1+I),I=1,IMXGN)
           ENDIF
           IF(IMXMA.NE.0) THEN
             ZK80(JMMA-1+1)(1:11) = 'MAILLE   : '
             WRITE(IFI,'(1X,A80)') (ZK80(JMMA-1+I),I=1,IMXMA)
           ENDIF
           IF(IMXGM.NE.0) THEN
             ZK80(JNGRM-1+1)(1:11) = 'GROUP_MA : '
             WRITE(IFI,'(1X,A80)') (ZK80(JNGRM-1+I),I=1,IMXGM)
           ENDIF
           WRITE(IFI,'(A)')
         ENDIF
C        --- IMPRESSION DANS UN INTERVALLE   BORINF,BORSUP
C         BORINF = 0.D
C         BORSUP = 0.D
         IF((NC.NE.0.OR.NR.NE.0).AND.(FORM.EQ.'RESULTAT')) THEN
           CALL GETVR8('RESU','BORNE_INF',IOCC,IARG,1,BORINF,NINF)
           CALL GETVR8('RESU','BORNE_SUP',IOCC,IARG,1,BORSUP,NSUP)
           IF(NINF.NE.0) LINF=.TRUE.
           IF(NSUP.NE.0) LSUP=.TRUE.
         ENDIF
C
C        ---- IMPRESSION VALEUR MAX, VALEUR MIN----
         IF((NC.NE.0.OR.NR.NE.0).AND.(FORM.EQ.'RESULTAT')) THEN
           TMAX=' '
           TMIN=' '
           CALL GETVTX('RESU','VALE_MAX',IOCC,IARG,1,TMAX,NMA)
           CALL GETVTX('RESU','VALE_MIN',IOCC,IARG,1,TMIN,NMI)
           IF(NMA.NE.0.AND.TMAX.EQ.'OUI') LMAX=.TRUE.
           IF(NMI.NE.0.AND.TMIN.EQ.'OUI') LMIN=.TRUE.
         ENDIF
C
C        ********************************************
C        ---- CHOIX DES COMPOSANTES AUX FORMATS ----
C        ---- RESULTAT, CASTEM, MED ET GMSH  ----
C        ********************************************
         IF((NC.NE.0.OR.NR.NE.0).AND.
     &        ( FORM.EQ.'RESULTAT'     .OR.
     &          FORM(1:4).EQ.'GMSH'    .OR.
     &          FORM(1:3).EQ.'MED'     .OR.
     &          FORM(1:5).EQ.'IDEAS'   .OR.
     &          FORM(1:6).EQ.'CASTEM' ) ) THEN
           CALL GETVTX('RESU','NOM_CMP',IOCC,IARG,0,K8B,N)
           IF(N.LT.0) THEN
           NBCMP=-N
           CALL WKVECT('&&OP0039.NOM_CMP','V V K8',NBCMP,JCMP)
           CALL GETVTX('RESU','NOM_CMP',IOCC,IARG,NBCMP,ZK8(JCMP),IBID)
           ENDIF
         ENDIF
C
C        TYPE DE CHAMP A IMPRIMER POUR LE FORMAT GMSH (VERSION >= 1.2)
         TYCHA=' '
         IF( (NC.NE.0.OR.NR.NE.0) .AND.
     &        FORM(1:4).EQ.'GMSH' .AND. VERSIO.GE.2) THEN
           CALL GETVTX('RESU','TYPE_CHAM',IOCC,IARG,1,TYCHA,IBID)
         ENDIF
C
         VARIEL=' '
         CALL GETVTX('RESU','IMPR_NOM_VARI',IOCC,IARG,1,
     &               VARIEL,NVARI)
         IF ( VARIEL.EQ.'OUI' ) THEN
           LVARIE=.TRUE.
         ELSE
           LVARIE=.FALSE.
         ENDIF

C        ***************************************
C        --- APPEL A LA ROUTINE D'IMPRESSION ---
C        ***************************************
         IF(NC.NE.0.OR.NR.NE.0) THEN
C          - VERIFICATION DES PARAMETRES (FORMAT 'RESULTAT')
           CALL IRPARB(LERESU,NPA,ZK16(JPA),'&&OP0039.NOM_PAR',
     &                 NBPARA)
           CALL JEEXIN('&&OP0039.NOM_PAR',IRET)
           IF (IRET.GT.0) THEN
              CALL JEVEUO('&&OP0039.NOM_PAR','E',JPARA)
           ELSE
              JPARA=1
           ENDIF
C
C          - ECRITURE DU CONCEPT LERESU SUR FICHIER FICH AU FORMAT FORM
          IF ( FORM(1:4).EQ.'MED' ) THEN
            IF ( LNCMED ) THEN
            CALL IREMED(LERESU,IFI,NBNOSY,ZK16(JNOSY),NBCMDU,
     &                  ZK80(JNCMED),PARTIE,NBORDR,ZI(JORDR),LRESU,
     &                  NBNOT,ZI(JNUNOT),NBMAT,ZI(JNUMA),
     &                  NBCMP,ZK8(JCMP),LVARIE)



            ELSE
            CALL IREMED(LERESU,IFI,NBNOSY,ZK16(JNOSY),NBCMDU,
     &                  ' ',PARTIE,NBORDR,ZI(JORDR),LRESU,
     &                  NBNOT,ZI(JNUNOT),NBMAT,ZI(JNUMA),
     &                  NBCMP,ZK8(JCMP),LVARIE)
            ENDIF
          ELSE
            CALL IRECRI(LERESU,FORM,IFI,TITRE,LGMSH,
     &        NBNOSY,ZK16(JNOSY),PARTIE,NBPARA,ZK16(JPARA),
     &        NBORDR,ZI(JORDR),LRESU,'RESU',IOCC,CECR,
     &        TYCHA,LCOR,NBNOT,ZI(JNUNOT),NBMAT,ZI(JNUMA),NBCMP,
     &        ZK8(JCMP),LSUP,BORSUP,LINF,BORINF,LMAX,LMIN,FORMR,
     &        NIVE,VERSIO)
           ENDIF
         ENDIF
C        **********************
C        --- FIN IMPRESSION ---
C        **********************
 99      CONTINUE
C
C        --- DESTRUCTION TABLEAUX DE TRAVAIL
         CALL JEDETR ( '&&OP0039.NOM_SYMB'    )
         CALL JEDETR ( NOVCMP  )
         CALL JEDETR ( '&&OP0039.NUM_ORDR'    )
         CALL JEDETR ( '&&OP0039.NUME_ORDRE'  )
         CALL JEDETR ( '&&OP0039.NOMUTI_PARA' )
         CALL JEDETR ( '&&OP0039.LIST_GRNO'   )
         CALL JEDETR ( '&&OP0039.LIST_NOE'    )
         CALL JEDETR ( '&&OP0039.LIST_GRMA'   )
         CALL JEDETR ( '&&OP0039.LIST_MAI'    )
         CALL JEDETR ( '&&OP0039.NOM_GRNO'    )
         CALL JEDETR ( '&&OP0039.NOM_NOE'     )
         CALL JEDETR ( '&&OP0039.NOM_GRMA'    )
         CALL JEDETR ( '&&OP0039.NOM_MAI'     )
         CALL JEDETR ( '&&OP0039.NUMNOE'      )
         CALL JEDETR ( '&&OP0039.NUMMAI'      )
         CALL JEDETR ( '&&OP0039.NUMNOS'      )
         CALL JEDETR ( '&&OP0039.NUMNOT'      )
         CALL JEDETR ( '&&OP0039.VERI_NOM_CMP')
         CALL JEDETR ( '&&OP0039.NOM_CMP'     )
         CALL JEDETR ( '&&OP0039.LIST_TOPO'   )
         CALL JEDETR ( '&&OP0039.NOM_PAR'     )
         CALL JEDETR ( '&&OP0039.FILTRE_NO'   )
         CALL JEDETR ( '&&OP0039.IND_NOEU'    )
C

 10   CONTINUE
      IF(LCASTS) THEN
         IBID=5
         WRITE(IFC,'(A,I4)')  ' ENREGISTREMENT DE TYPE',IBID
         IF (NIVE.EQ.10) THEN
           IBID = 1
           WRITE(IFC,'(A,I4)')  'LABEL AUTOMATIQUE :',IBID
         ENDIF
      ENDIF

C     -- IMPRESSION DES CARTES DE DONNEES DE CHAM_MATER,  ... :
      CALL W039CA (IFI,FORM)


9999  CONTINUE
      CALL JEDEMA()
      END

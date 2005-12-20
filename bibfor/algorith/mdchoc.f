      SUBROUTINE MDCHOC (NBNLI,NBCHOC,NBFLAM,LOGCHO,DPLMOD,PARCHO,
     +                   NOECHO,INTITU,PS1DEL,PS2DEL,NUMDDL,NBMODE,
     +                   PULSAT,MASGEN,LAMOR,AMOGEN,BMODAL,NEQ,NEXCIT,
     +                   INFO,LFLU,MONMOT,IER)
      IMPLICIT  REAL*8  (A-H,O-Z)
      INTEGER            NBNLI
      INTEGER            NBCHOC,LOGCHO(NBNLI,*),NBMODE,NEQ,NEXCIT,INFO
      REAL*8             PARCHO(NBNLI,*),PULSAT(*),MASGEN(*),AMOGEN(*)
      REAL*8             DPLMOD(NBNLI,NBMODE,*),BMODAL(NEQ,*)
      REAL*8             PS1DEL(NEQ,NEXCIT),PS2DEL(NBNLI,NEXCIT,*)
      CHARACTER*8        NOECHO(NBNLI,*),MAILLA,INTITU(*),MONMOT
      CHARACTER*14       NUMDDL
      LOGICAL            LAMOR,LFLU
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/12/2005   AUTEUR CIBHHLV L.VIVAN 
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
C TOLE CRP_20 CRP_21
C
C     STOCKAGE DES INFORMATIONS DE CHOC DANS DES TABLEAUX
C     ------------------------------------------------------------------
C IN  : NBNLI  : DIMENSION DES TABLEAUX (NBCHOC+NBSISM+NBFLAM)
C IN  : NBCHOC : NOMBRE DE POINTS DE CHOC
C IN  : NBFLAM : NOMBRE DE CHOCS AVEC FLAMBEMENT
C OUT : LOGCHO : LOGIQUE CHOC: LOGCHO(I,1) = SI ADHERENCE OU NON
C                              LOGCHO(I,2) = SI FORCE FLUIDE OU NON
C                              LOGCHO(I,3) = SI CHOC SEC + LAME FLUIDE
C                              LOGCHO(I,4) = SI DISPO ANTI S OU NON
C                              LOGCHO(I,5) = SI FLAMBEMENT OU NON
C OUT : DPLMOD : DEPL MODAUX AUX NOEUDS DE CHOC APRES ORIENTATION
C                DPLMOD(I,J,1) = DEPL DX DU NOEUD_1 DE CHOC I - MODE J
C                DPLMOD(I,J,2) = DEPL DY
C                DPLMOD(I,J,3) = DEPL DZ
C                DPLMOD(I,J,4) = DEPL DX DU NOEUD_2 DE CHOC I - MODE J
C                DPLMOD(I,J,5) = DEPL DY
C                DPLMOD(I,J,6) = DEPL DZ
C OUT : PARCHO : PARAMETRE DE CHOC:
C                PARCHO(I, 1)= JEU AU NOEUD DE CHOC I
C                PARCHO(I, 2)= RIGI NORMALE
C                PARCHO(I, 3)= AMOR NORMAL
C                PARCHO(I, 4)= RIGI TANGENTIELLE
C                PARCHO(I, 5)= AMOR TANGENTIEL
C                PARCHO(I, 6)= COULOMB
C                PARCHO(I, 7)= COOR INIT NOEUD_1 X REP GLOBAL
C                PARCHO(I, 8)= COOR INIT NOEUD_1 Y REP GLOBAL
C                PARCHO(I, 9)= COOR INIT NOEUD_1 Z REP GLOBAL
C                PARCHO(I,10)= COOR INIT NOEUD_2 X REP GLOBAL
C                PARCHO(I,11)= COOR INIT NOEUD_2 Y REP GLOBAL
C                PARCHO(I,12)= COOR INIT NOEUD_2 Z REP GLOBAL
C                PARCHO(I,13)= COOR ORIGINE OBSTACLE X REP GLOBAL
C                PARCHO(I,14)= COOR ORIGINE OBSTACLE Y REP GLOBAL
C                PARCHO(I,15)= COOR ORIGINE OBSTACLE Z REP GLOBAL
C                PARCHO(I,16)= SIN A
C                PARCHO(I,17)= COS A
C                PARCHO(I,18)= SIN B
C                PARCHO(I,19)= COS B
C                PARCHO(I,20)= SIN G
C                PARCHO(I,21)= COS G
C                PARCHO(I,22)= X AVANT ADHERENCE
C                PARCHO(I,23)= Y AVANT ADHERENCE
C                PARCHO(I,24)= Z AVANT ADHERENCE
C                PARCHO(I,25)= FT1 AVANT ADHERENCE
C                PARCHO(I,26)= FT2 AVANT ADHERENCE
C                PARCHO(I,27)= VT1 PAS PRECEDENT
C                PARCHO(I,28)= VT2 PAS PRECEDENT
C                PARCHO(I,29)= DIST_1 DU NOEUD_1
C                PARCHO(I,30)= DIST_2 DU NOEUD_2
C                PARCHO(I,31)= COEF A FORCE FLUIDE
C                PARCHO(I,32)= COEF B FORCE FLUIDE
C                PARCHO(I,33)= COEF C FORCE FLUIDE
C                PARCHO(I,34)= COEF D FORCE FLUIDE
C                PARCHO(I,35)= COUCHE LIMITE
C                PARCHO(I,36)= SIGNE DE Y20LOC-Y10LOC
C                PARCHO(I,37)= SIGNE DE Z20LOC-Z10LOC
C                PARCHO(I,38)= COEF RIGI_K1 DISPO ANTI SISMIQUE
C                PARCHO(I,39)= COEF RIGI_K2 DISPO ANTI SISMIQUE
C                PARCHO(I,40)= COEF SEUIL_FX DISPO ANTI SISMIQUE
C                PARCHO(I,41)= COEF C DISPO ANTI SISMIQUE
C                PARCHO(I,42)= COEF PUIS_ALPHA DISPO ANTI SISMIQUE
C                PARCHO(I,43)= COEF DX_MAX DISPO ANTI SISMIQUE
C                PARCHO(I,44)= NORMALE X
C                PARCHO(I,45)= NORMALE Y
C                PARCHO(I,46)= NORMALE Z
C                PARCHO(I,47)= TAUX DE RESTITUTION (CALCULE DANS CRICHO)
C                PARCHO(I,48)= TAUX DE RESTITUTION (CALCULE DANS CRICHO)
C                PARCHO(I,49)= FORCE LIMITE DE FLAMBAGE
C                PARCHO(I,50)= PALIER FORCE DE REACTION APRES FLAMBAGE
C                PARCHO(I,51)= RIGIDITE APRES FLAMBAGE
C OUT : NOECHO : NOEUD DE CHOC: NOECHO(I,1) = NOEUD_1
C                               NOECHO(I,2) = SOUS_STRUC_1
C                               NOECHO(I,3) = NUME_1
C                               NOECHO(I,4) = MAILLA_1
C                               NOECHO(I,5) = NOEUD_2
C                               NOECHO(I,6) = SOUS_STRUC_2
C                               NOECHO(I,7) = NUME_2
C                               NOECHO(I,8) = MAILLA_2
C                               NOECHO(I,9) = TYPE D'OBSTACLE
C OUT : INTITU : INTITULE DE CHOC
C IN  : PS1DEL : PSI*DELTA (MULTI-APPUI) = NOMRES//'.IPSD'
C OUT : PS2DEL : PSI*DELTA: PS2DEL(I,J,1)=  DX NOEUD_1 CHOC I - EXCIT J
C                           PS2DEL(I,J,2)=  DY NOEUD_1 CHOC I - EXCIT J
C                           PS2DEL(I,J,3)=  DZ NOEUD_1 CHOC I - EXCIT J
C                           PS2DEL(I,J,4)=  DX NOEUD_2 CHOC I - EXCIT J
C                           PS2DEL(I,J,5)=  DY NOEUD_2 CHOC I - EXCIT J
C                           PS2DEL(I,J,6)=  DZ NOEUD_2 CHOC I - EXCIT J
C IN  : NUMDDL : NOM DE LA NUMEROTATION
C IN  : NBMODE : NOMBRE DE MODES DE LA BASE DE PROJECTION
C IN  : PULSAT : PULSATIONS DES MODES
C IN  : MASGEN : MASSES GENERALISEES DES MODES
C IN  : LAMOR  : LOGIQUE POUR AMORTISSEMENTS MODAUX
C IN  : AMOGEN : MATRICE DES AMORTISSEMENTS GENERALISES
C IN  : BMODAL : VECTEURS MODAUX
C IN  : NEQ    : NOMBRE D'EQUATIONS
C IN  : NEXCIT : NOMBRE D'EXCITATIONS
C IN  : INFO   : NIVEAU D'IMPRESSION
C OUT : LFLU   : LOGIQUE INDIQUANT LA PRESENCE DE LAME FLUIDE
C OUT : IER    : CODE RETOUR
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
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C
C
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      REAL*8        TEMPO(3),KTANG,K,COOR1(3),COOR2(3),COORD(3),NORME(3)
      REAL*8        DPILOC(6), DPIGLO(6), DDPILO(3),ORIGOB(3)
      REAL*8        TXLOC(3), TZLOC(3), TYLOC(3), ANG(3)
      REAL*8        NORMX(3), NORMY(3), DIRCHO(3)
      CHARACTER*8   NOMNO1,NOMNO2,NOMCHO,SST1,SST2,MAYA1,MAYA2
      CHARACTER*8   NOMGR1,NOMGR2,REPERE,NOEUD(3),KBID,MAMAI
      CHARACTER*10  MOTFAC
      CHARACTER*14  NUME1,NUME2
      CHARACTER*16  NOMCMD,TYPNUM,K16B
      CHARACTER*24  K24REP,MDGENE,MDSSNO,NUMERO,K24DIS
      CHARACTER*32  JEXNUM,JEXNOM
C
      UN   = 1.D0
      MOTFAC = 'CHOC'
C
C     ------------------------------------------------------------------
C
C     --- RECHERCHE DU MODE DE MASSE LA PLUS ELEVEE ---
C
      CALL JEMARQ()
      NBSISM = NBNLI - NBCHOC - NBFLAM
      LFLU = .FALSE.
      XMAS = MASGEN(1)
      IMODE = 1
      DO 2 IM = 1,NBMODE
        IF (MASGEN(IM).GT.XMAS) THEN
          XMAS = MASGEN(IM)
          IMODE = IM
        ENDIF
 2    CONTINUE
      IF ( LAMOR ) THEN
        IAMOR = IMODE
      ELSE
        IAMOR = IMODE + NBMODE * ( IMODE - 1 )
      ENDIF
C
C     --- REMPLISSAGE DE NOECHO(I,J) ---
C
      CALL GETTCO(NUMDDL,TYPNUM)
C
C --- CALCUL DIRECT
      IF (TYPNUM.EQ.'NUME_DDL_SDASTER') THEN
        CALL DISMOI('F','NOM_MAILLA',NUMDDL,'NUME_DDL',IB,MAILLA,IE)
        DO 100 I=1,NBNLI
          II = I
          IF (I.GT.NBCHOC+NBSISM) THEN
              MOTFAC = 'FLAMBAGE'
              II = I-(NBCHOC+NBSISM)
          ELSEIF (I.GT.NBCHOC) THEN
              MOTFAC = 'ANTI_SISM'
              II = I-NBCHOC
          ENDIF
C
C ------- RECUPERATION DES NOEUDS DE CHOC
          IF (MOTFAC(1:4).EQ.'CHOC') THEN
          CALL GETVEM ( MAILLA, 'MAILLE', MOTFAC, 'MAILLE',
     +                                              II,1,1,MAMAI,IBID)
          IF (IBID.NE.0) THEN
            CALL JENONU(JEXNOM(MAILLA//'.NOMMAI',MAMAI),IMAMA)
            CALL JEVEUO(JEXNUM(MAILLA//'.CONNEX',IMAMA),'L',JMAMA)
            CALL JELIRA(JEXNUM(MAILLA//'.CONNEX',IMAMA),'LONMAX',
     +                                                  NBNMA,KBID)
            IF (NBNMA.NE.2) CALL UTMESS('F','MDCHOC','QUE 2 NOEUDS')
          CALL JENUNO(JEXNUM(MAILLA//'.NOMNOE',ZI(JMAMA))  ,NOECHO(I,1))
          CALL JENUNO(JEXNUM(MAILLA//'.NOMNOE',ZI(JMAMA+1)),NOECHO(I,5))
            NN1 = 1
            NN2 = 0
            GOTO 102
          ENDIF
C
          CALL GETVEM ( MAILLA, 'GROUP_MA', MOTFAC, 'GROUP_MA',
     +                                              II,1,1,MAMAI,IBID)
          IF (IBID.NE.0) THEN
            CALL JELIRA(JEXNOM(MAILLA//'.GROUPEMA',MAMAI),'LONMAX',
     +                                                  NBNMA,KBID)
            CALL JEVEUO(JEXNOM(MAILLA//'.GROUPEMA',MAMAI),'L',KMA)
            IF (NBNMA.NE.1) THEN
              CALL JENUNO(JEXNUM(MAILLA//'.NOMMAI',ZI(KMA)),KBID)
              CALL UTDEBM('A','MDCHOC','TROP DE MAILLES')
              CALL UTIMPK('S',' DANS LE GROUP_MA ',1,MAMAI)
              CALL UTIMPK('L',' ON NE PREND QUE LA MAILLE ',1,KBID)
              CALL UTFINM()
            ENDIF
            CALL JEVEUO(JEXNUM(MAILLA//'.CONNEX',ZI(KMA)),'L',JMAMA)
            CALL JELIRA(JEXNUM(MAILLA//'.CONNEX',ZI(KMA)),'LONMAX',
     +                                                  NBNMA,KBID)
            IF (NBNMA.NE.2) CALL UTMESS('F','MDCHOC','QUE 2 NOEUDS')
          CALL JENUNO(JEXNUM(MAILLA//'.NOMNOE',ZI(JMAMA))  ,NOECHO(I,1))
          CALL JENUNO(JEXNUM(MAILLA//'.NOMNOE',ZI(JMAMA+1)),NOECHO(I,5))
            NN1 = 1
            NN2 = 0
            GOTO 102
          ENDIF
          ENDIF
C
          CALL GETVEM ( MAILLA, 'NOEUD', MOTFAC, 'NOEUD_1',
     +                                              II,1,1,NOMNO1,IBID)
          IF (IBID.NE.0) THEN
            NOECHO(I,1) = NOMNO1
            CALL GETVEM ( MAILLA, 'NOEUD', MOTFAC, 'NOEUD_2',
     +                                              II,1,1,NOMNO2,NN1)
            IF (NN1.NE.0) THEN
              NOECHO(I,5) = NOMNO2
              NN2 = 0
            ELSE
              CALL GETVID(MOTFAC,'GROUP_NO_2',II,1,1,NOMGR2,NN2)
              IF (NN2.NE.0) THEN
                CALL UTNONO(' ',MAILLA,'NOEUD',NOMGR2,NOMNO2,IRET)
                IF (IRET.EQ.10) THEN
                  CALL UTMESS('F','MDCHOC',
     +                    'LE GROUP_NO : '//NOMGR2//'N''EXISTE PAS.')
                ELSEIF (IRET.EQ.1) THEN
                  CALL UTDEBM('A','MDCHOC','TROP DE NOEUDS')
                  CALL UTIMPK('S',' DANS LE GROUP_NO ',1,NOMGR2)
                  CALL UTIMPK('L','  NOEUD UTILISE: ',1,NOMNO2)
                  CALL UTFINM( )
                ENDIF
                NOECHO(I,5) = NOMNO2
              ELSE
                NOECHO(I,5) = NOMNO1
              ENDIF
            ENDIF
            GOTO 102
          ENDIF
C
          CALL GETVEM ( MAILLA, 'GROUP_NO', MOTFAC, 'GROUP_NO_1',
     +                                              II,1,1,NOMGR1,IBI2)
          CALL UTNONO(' ',MAILLA,'NOEUD',NOMGR1,NOMNO1,IRET)
          IF (IRET.EQ.10) THEN
             CALL UTMESS('F','MDCHOC',
     +                     'LE GROUP_NO : '//NOMGR1//'N''EXISTE PAS.')
          ELSEIF (IRET.EQ.1) THEN
             CALL UTDEBM('A','MDCHOC','TROP DE NOEUDS')
             CALL UTIMPK('S',' DANS LE GROUP_NO ',1,NOMGR1)
             CALL UTIMPK('L',' NOEUD UTILISE: ',1,NOMNO1)
             CALL UTFINM( )
          ENDIF
          NOECHO(I,1) = NOMNO1
          CALL GETVEM ( MAILLA, 'NOEUD', MOTFAC, 'NOEUD_2',
     +                                              II,1,1,NOMNO2,NN1)
          IF (NN1.NE.0) THEN
            NOECHO(I,5) = NOMNO2
            NN2 = 0
          ELSE
            CALL GETVID(MOTFAC,'GROUP_NO_2',II,1,1,NOMGR2,NN2)
            IF (NN2.NE.0) THEN
              CALL UTNONO(' ',MAILLA,'NOEUD',NOMGR2,NOMNO2,IRET)
              IF (IRET.EQ.10) THEN
                CALL UTMESS('F','MDCHOC',
     +                     'LE GROUP_NO : '//NOMGR2//'N''EXISTE PAS.')
              ELSEIF (IRET.EQ.1) THEN
                CALL UTDEBM('A','MDCHOC','TROP DE NOEUDS')
                CALL UTIMPK('S',' DANS LE GROUP_NO ',1,NOMGR2)
                CALL UTIMPK('L',' NOEUD UTILISE: ',1,NOMNO2)
                CALL UTFINM( )
              ENDIF
              NOECHO(I,5) = NOMNO2
            ELSE
              NOECHO(I,5) = NOMNO1
            ENDIF
          ENDIF
 102      CONTINUE
C
          IF (MOTFAC(1:9).EQ.'ANTI_SISM' .AND. 
     &                         NN1.EQ.0 .AND. NN2.EQ.0) THEN
           CALL UTMESS('F','MDCHOC',' DISPOSITIF ANTI-SISMIQUE : '//
     &                 ' LA CONNAISSANCE DE DEUX NOEUDS OU GROUPES '//
     &                 ' EST OBLIGATOIRE')
          ENDIF
          NOECHO(I,3) = NUMDDL(1:8)
          NOECHO(I,4) = MAILLA
          NOECHO(I,7) = NUMDDL(1:8)
          NOECHO(I,8) = MAILLA
 100    CONTINUE
C
C --- CALCUL PAR SOUS-STRUCTURATION
      ELSEIF (TYPNUM(1:13).EQ.'NUME_DDL_GENE') THEN
        IF (NBSISM.GT.0 .OR. NBFLAM.GT.0) THEN
           CALL UTMESS('F','MDCHOC','CALCUL NON-LINEAIRE PAR '//
     &                 'SOUS-STRUCTURATION, PAS DE DISPOSITIF'//
     &                 ' ANTI-SISMIQUE OU DE FLAMBAGE POSSIBLE ')
        ENDIF
        CALL JEVEUO(NUMDDL//'.NUME.REFN','L',LLREFE)
        MDGENE = ZK24(LLREFE)
        MDSSNO = MDGENE(1:14)//'.MODG.SSNO'
        DO 112 I=1,NBNLI
          CALL GETVTX('CHOC','SOUS_STRUC_1',I,1,1,SST1,N1)
          IF (N1.EQ.0) THEN
            CALL UTMESS('F','MDCHOC','CALCUL NON-LINEAIRE PAR '//
     &                  'SOUS-STRUCTURATION, LE MOT-CLE SOUS_STRUC_1'//
     &                  ' EST OBLIGATOIRE')
          ENDIF
          CALL JENONU(JEXNOM(MDSSNO,SST1),IRET)
          IF (IRET.EQ.0) THEN
            CALL UTMESS('F','MDCHOC','ARGUMENT DU MOT-CLE'//
     &            ' "SOUS_STRUC_1" N''EST PAS UN NOM DE SOUS-STRUCTURE')
          ENDIF
          CALL MGUTDM(MDGENE,SST1,IBID,'NOM_NUME_DDL',IBID,NUME1)
          CALL MGUTDM(MDGENE,SST1,IBID,'NOM_MAILLAGE',IBID,MAYA1)
          CALL GETVID('CHOC','NOEUD_1',I,1,1,NOMNO1,IBID)
          IF (IBID.NE.0) THEN
             NOECHO(I,1) = NOMNO1
          ELSE
            CALL GETVID('CHOC','GROUP_NO_1',I,1,1,NOMGR1,IBI2)
            CALL UTNONO(' ',MAYA1,'NOEUD',NOMGR1,NOMNO1,IRET)
            IF (IRET.EQ.10) THEN
               CALL UTMESS('F','MDCHOC',
     +                     'LE GROUP_NO : '//NOMGR1//'N''EXISTE PAS.')
            ELSEIF (IRET.EQ.1) THEN
               CALL UTDEBM('A','MDCHOC',
     +                     'TROP DE NOEUDS DANS LE GROUP_NO')
               CALL UTIMPK('L','  NOEUD UTILISE: ',1,NOMNO1)
               CALL UTFINM( )
            ENDIF
            NOECHO(I,1) = NOMNO1
          ENDIF
          NOECHO(I,2) = SST1
          NOECHO(I,3) = NUME1(1:8)
          NOECHO(I,4) = MAYA1
          CALL GETVID('CHOC','NOEUD_2'   ,I,1,1,NOMNO2,NN1)
          CALL GETVID('CHOC','GROUP_NO_2',I,1,1,NOMGR2,NN2)
          IF (NN1.NE.0.OR.NN2.NE.0) THEN
            CALL GETVTX('CHOC','SOUS_STRUC_2',I,1,1,SST2,N2)
            IF (N2.EQ.0) THEN
              CALL UTMESS('F','MDCHOC','CALCUL NON-LINEAIRE PAR '//
     &                    'SOUS-STRUCTURATION ENTRE 2 STRUCTURES '//
     &                    'MOBILES, LE MOT-CLE SOUS_STRUC_2 '//
     &                    'EST OBLIGATOIRE')
            ENDIF
            CALL JENONU(JEXNOM(MDSSNO,SST2),IRET)
            IF (IRET.EQ.0) THEN
              CALL UTMESS('F','MDCHOC','ARGUMENT DU MOT-CLE'//
     &            ' "SOUS_STRUC_2" N''EST PAS UN NOM DE SOUS-STRUCTURE')
            ENDIF
            CALL MGUTDM(MDGENE,SST2,IBID,'NOM_NUME_DDL',IBID,NUME2)
            CALL MGUTDM(MDGENE,SST2,IBID,'NOM_MAILLAGE',IBID,MAYA2)
            IF (NN1.NE.0) THEN
               CALL GETVID('CHOC','NOEUD_2',I,1,1,NOMNO2,NN1)
               NOECHO(I,5) = NOMNO2
            ELSE 
              CALL UTNONO(' ',MAYA2,'NOEUD',NOMGR2,NOMNO2,IRET)
              IF (IRET.EQ.10) THEN
                CALL UTMESS('F','MDCHOC',
     +                     'LE GROUP_NO : '//NOMGR2//'N''EXISTE PAS.')
              ELSEIF (IRET.EQ.1) THEN
                CALL UTDEBM('A','MDCHOC',
     +                     'TROP DE NOEUDS DANS LE GROUP_NO')
                CALL UTIMPK('L','  NOEUD UTILISE: ',1,NOMNO2)
                CALL UTFINM( )
              ENDIF
              NOECHO(I,5) = NOMNO2
            ENDIF
            CALL VECHBN(MDGENE,NOMNO1,SST1,NOMNO2,SST2)
            NOECHO(I,6) = SST2
            NOECHO(I,7) = NUME2(1:8)
            NOECHO(I,8) = MAYA2
          ELSE
            NOECHO(I,5) = NOMNO1
            NOECHO(I,6) = SST1
            NOECHO(I,7) = NUME1(1:8)
            NOECHO(I,8) = MAYA1
          ENDIF
112     CONTINUE
C
      ENDIF
C
      IER = 0
      RAD = R8DGRD()
      ZERO = 0.D0
      NOMCMD = 'MDCHOC'
C
      CALL WKVECT('&&MDCHOC.DDLCHO','V V I',NBNLI*6,JDDL)
C
      MOTFAC = 'CHOC          '
      DO 10 I = 1,NBNLI
         II = I
         IF (I.GT.NBCHOC+NBSISM) THEN
            MOTFAC = 'FLAMBAGE'
            II =I -(NBCHOC+NBSISM)
         ELSEIF (I.GT.NBCHOC) THEN
            MOTFAC = 'ANTI_SISM'
            II =I -NBCHOC
         ENDIF
C
        CALL POSDDL('NUME_DDL',NOECHO(I,3),NOECHO(I,1),'DX',NUNOE,NUDDL)
        IF (NUNOE.EQ.0) THEN
          IER = IER + 1
          CALL UTMESS('E',NOMCMD,'LE NOEUD '//NOECHO(I,1)//
     +           ' N''EST PAS UN NOEUD DU MAILLAGE '//NOECHO(I,4))
        ENDIF
        IF (NUDDL.EQ.0) THEN
          IER = IER + 1
          CALL UTMESS('E',NOMCMD,'ON N''A PAS TROUVE LE DDL "DX" '//
     +                           'POUR LE NOEUD '//NOECHO(I,1))
        ENDIF
        ZI(JDDL-1+6*(I-1)+1) = NUDDL
        CALL POSDDL('NUME_DDL',NOECHO(I,3),NOECHO(I,1),'DY',NUNOE,NUDDL)
        IF (NUDDL.EQ.0) THEN
          IER = IER + 1
          CALL UTMESS('E',NOMCMD,'ON N''A PAS TROUVE LE DDL "DY" '//
     +                           'POUR LE NOEUD '//NOECHO(I,1))
        ENDIF
        ZI(JDDL-1+6*(I-1)+2) = NUDDL
        CALL POSDDL('NUME_DDL',NOECHO(I,3),NOECHO(I,1),'DZ',NUNOE,NUDDL)
        IF (NUDDL.EQ.0) THEN
          IER = IER + 1
          CALL UTMESS('E',NOMCMD,'ON N''A PAS TROUVE LE DDL "DZ" '//
     +                           'POUR LE NOEUD '//NOECHO(I,1))
        ENDIF
        ZI(JDDL-1+6*(I-1)+3) = NUDDL
        CALL JENONU(JEXNOM(NOECHO(I,4)//'.NOMNOE',NOECHO(I,1)),INO1)
C
        N1 = 0
        IF(I.LE.NBCHOC) 
     &     CALL GETVTX('CHOC','INTITULE',I,1,1,NOMCHO,N1)
        IF (N1.EQ.0) THEN
          INTITU(I) = NOECHO(I,1)
        ELSE
          INTITU(I) = NOMCHO
        ENDIF
C
        CALL GETVID(MOTFAC,'NOEUD_2'   ,II,1,1,NOMNO2,N1)
        CALL GETVID(MOTFAC,'GROUP_NO_2',II,1,1,NOMNO2,N2)
        IF (MOTFAC(1:4).EQ.'CHOC') THEN
          CALL GETVID(MOTFAC,'MAILLE'    ,II,1,1,NOMNO2,N3)
          CALL GETVID(MOTFAC,'GROUP_MA'  ,II,1,1,NOMNO2,N4)
        ELSE
          N3=0
          N4=0
        ENDIF
        IF (N1.NE.0.OR.N2.NE.0.OR.N3.NE.0.OR.N4.NE.0) THEN
          CALL POSDDL('NUME_DDL',NOECHO(I,7),NOECHO(I,5),'DX',
     &       NUNOE,NUDDL)
          IF (NUNOE.EQ.0) THEN
            IER = IER + 1
            CALL UTMESS('E',NOMCMD,'LE NOEUD '//NOECHO(I,5)//
     +          ' N''EST PAS UN NOEUD DU MAILLAGE '//NOECHO(I,8))
          ENDIF
          IF (NUDDL.EQ.0) THEN
            IER = IER + 1
            CALL UTMESS('E',NOMCMD,'ON N''A PAS TROUVE LE DDL '//
     +                         '"DX" POUR LE NOEUD '//NOECHO(I,5))
          ENDIF
          ZI(JDDL-1+6*(I-1)+4) = NUDDL
          CALL POSDDL('NUME_DDL',NOECHO(I,7),NOECHO(I,5),'DY',
     &                 NUNOE,NUDDL)
          IF (NUDDL.EQ.0) THEN
            IER = IER + 1
            CALL UTMESS('E',NOMCMD,'ON N''A PAS TROUVE LE DDL '//
     +                         '"DY" POUR LE NOEUD '//NOECHO(I,5))
          ENDIF
          ZI(JDDL-1+6*(I-1)+5) = NUDDL
          CALL POSDDL('NUME_DDL',NOECHO(I,7),NOECHO(I,5),'DZ',
     &                NUNOE,NUDDL)
          IF (NUDDL.EQ.0) THEN
            IER = IER + 1
            CALL UTMESS('E',NOMCMD,'ON N''A PAS TROUVE LE DDL '//
     +                         '"DZ" POUR LE NOEUD '//NOECHO(I,5))
          ENDIF
          ZI(JDDL-1+6*(I-1)+6) = NUDDL
          CALL JENONU(JEXNOM(NOECHO(I,8)//'.NOMNOE',NOECHO(I,5)),INO2)
        ELSE
          INO2 = INO1
          ZI(JDDL-1+6*(I-1)+4) = ZI(JDDL-1+6*(I-1)+1)
          ZI(JDDL-1+6*(I-1)+5) = ZI(JDDL-1+6*(I-1)+2)
          ZI(JDDL-1+6*(I-1)+6) = ZI(JDDL-1+6*(I-1)+3)
        ENDIF
C
        CALL JEVEUO(NOECHO(I,4)//'.COORDO    .VALE','L',JCOOR1)
        CALL JEVEUO(NOECHO(I,8)//'.COORDO    .VALE','L',JCOOR2)
        IF (TYPNUM.EQ.'NUME_DDL_SDASTER') THEN
          DO 12 J = 1,3
            PARCHO(I,6+J) = ZR(JCOOR1+3*(INO1-1)+J-1)
            PARCHO(I,9+J) = ZR(JCOOR2+3*(INO2-1)+J-1)
 12       CONTINUE
        ELSEIF (TYPNUM(1:13).EQ.'NUME_DDL_GENE') THEN
          CALL ORIENT(MDGENE,NOECHO(I,2),JCOOR1,INO1,COOR1,1)
          CALL ORIENT(MDGENE,NOECHO(I,6),JCOOR2,INO2,COOR2,1)
          DO 13 J = 1,3
            PARCHO(I,6+J) = COOR1(J)
            PARCHO(I,9+J) = COOR2(J)
 13       CONTINUE
C
        ENDIF
C
        IF (MOTFAC(1:4).EQ.'CHOC' .OR. MOTFAC(1:8).EQ.'FLAMBAGE') THEN
           CALL GETVR8(MOTFAC,'JEU'     ,II,1,1,PARCHO(I,1) ,N1)
           CALL GETVR8(MOTFAC,'DIST_1'  ,II,1,1,PARCHO(I,29),N1)
           CALL GETVR8(MOTFAC,'DIST_2  ',II,1,1,PARCHO(I,30),N1)
           CALL GETVR8(MOTFAC,'RIGI_NOR',II,1,1,PARCHO(I,2) ,N1)
           LOGCHO(I,2)=0
           LOGCHO(I,3)=0
           LOGCHO(I,5) = 0
           PARCHO(I,31)=0.D0
           PARCHO(I,32)=0.D0
           PARCHO(I,33)=0.D0
           PARCHO(I,34)=0.D0
           PARCHO(I,35)=0.D0
           PARCHO(I,49) = 0.D0
           PARCHO(I,50) = 0.D0
           PARCHO(I,51) = 0.D0
           IF (MOTFAC(1:4).EQ.'CHOC') THEN
              CALL GETVR8('CHOC','AMOR_NOR   ',I,1,1,PARCHO(I,3),N1)
              CALL GETVR8('CHOC','RIGI_TAN   ',I,1,1,KTANG      ,N1)
              CALL GETVR8('CHOC','COULOMB    ',I,1,1,PARCHO(I,6),N1)
              CALL GETVR8('CHOC','AMOR_TAN   ',I,1,1,CTANG      ,N1)
              CALL GETVTX('CHOC','LAME_FLUIDE',I,1,1,K24REP     ,N1)
              IF (K24REP.EQ.'OUI') THEN
                LFLU=.TRUE.
                LOGCHO(I,2)=1
                CALL GETVR8('CHOC','ALPHA   ',I,1,1,PARCHO(I,31),N1)
                CALL GETVR8('CHOC','BETA    ',I,1,1,PARCHO(I,32),N1)
                CALL GETVR8('CHOC','CHI     ',I,1,1,PARCHO(I,33),N1)
                CALL GETVR8('CHOC','DELTA   ',I,1,1,PARCHO(I,34),N1)
              ENDIF
           ELSE
C          --- TRAITEMENT FLAMBAGE DU AU CHOC ---
              LOGCHO(I,5) = 1
              KTANG = 0.D0
              CTANG = 0.D0
C             --- PARAMETRES DU FLAMBEMENT ---
              CALL GETVR8(MOTFAC,'FNOR_CRIT',II,1,1,PARCHO(I,49),N1)
              CALL GETVR8(MOTFAC,'FNOR_POST_FL',II,1,1,PARCHO(I,50),N1)
              CALL GETVR8(MOTFAC,'RIGI_NOR_POST_FL',II,1,1,PARCHO(I,51),
     &                     N1)
              IF (PARCHO(I,2).LE.0.D0 .OR. PARCHO(I,51).LE.0.D0) THEN
                 CALL UTMESS('F','MDCHOC','LES RIGIDITES DE CHOCS '//
     &                       'DOIVENT ETRE STRICTEMENT POSITIVES')
              ELSE
                 RAP=PARCHO(I,49)/PARCHO(I,2)-PARCHO(I,50)/PARCHO(I,51)
                 IF (RAP .LT. 0.D0)
     &             CALL UTMESS('F','MDCHOC','INCOHERENCE DANS LES '//
     &                 'DONNEES DE LA LOI DE FLAMBAGE : LES '//
     &                 'CARACTERISTIQUES INTRODUITES PEUVENT INDUIRE '//
     &                 'A UN ECRASEMENT RESIDUEL NEGATIF ')
              ENDIF
           ENDIF
C
           LOGCHO(I,4)=0
           PARCHO(I,38)=0.D0
           PARCHO(I,39)=0.D0
           PARCHO(I,40)=0.D0
           PARCHO(I,41)=0.D0
           PARCHO(I,42)=0.D0
           PARCHO(I,43)=0.D0
        ELSE
           PARCHO(I,1)=0.D0
           PARCHO(I,2)=0.D0
           PARCHO(I,3)=0.D0
           KTANG=0.D0
           CTANG=0.D0
           PARCHO(I,6)=0.D0
           PARCHO(I,31)=0.D0
           PARCHO(I,32)=0.D0
           PARCHO(I,33)=0.D0
           PARCHO(I,34)=0.D0
           PARCHO(I,35)=0.D0
           LOGCHO(I,4)=1
           II = I-NBCHOC
           CALL GETVR8('ANTI_SISM','RIGI_K1   ',II,1,1,PARCHO(I,38),N1)
           CALL GETVR8('ANTI_SISM','RIGI_K2   ',II,1,1,PARCHO(I,39),N1)
           CALL GETVR8('ANTI_SISM','SEUIL_FX  ',II,1,1,PARCHO(I,40),N1)
           CALL GETVR8('ANTI_SISM','C         ',II,1,1,PARCHO(I,41),N1)
           CALL GETVR8('ANTI_SISM','PUIS_ALPHA',II,1,1,PARCHO(I,42),N1)
           CALL GETVR8('ANTI_SISM','DX_MAX    ',II,1,1,PARCHO(I,43),N1)
        ENDIF
C       SI CTANG NON PRECISE ON CALCULE UN AMORTISSEMENT CRITIQUE
        IF ( CTANG.EQ.ZERO .AND. KTANG.NE.ZERO ) THEN
          K = SQRT( PULSAT(IMODE) ) * MASGEN(IMODE)
          CTANG = 2.D0*SQRT( MASGEN(IMODE)*(K+KTANG) )
     +            - 2.D0*AMOGEN(IAMOR)*SQRT( K*MASGEN(IMODE) )
        ENDIF
        PARCHO(I,4) = KTANG
        PARCHO(I,5) = CTANG
C
C ---   RECHERCHE DU TYPE D'OBSTACLE
        IF(MOTFAC.EQ.'CHOC' .OR. MOTFAC.EQ.'FLAMBAGE') THEN
           CALL GETVID(MOTFAC,'OBSTACLE',II,1,1,NOECHO(I,9),N1)
           CALL JEVEUO(NOECHO(I,9)//'           .REFO','L',JREFE)
           IF (ZK24(JREFE)(1:9).EQ.'BI_PLAN_Y') THEN
             NOECHO(I,9) = 'BI_PLANY'
           ELSEIF (ZK24(JREFE)(1:9).EQ.'BI_PLAN_Z') THEN
             NOECHO(I,9) = 'BI_PLANZ'
           ELSEIF (ZK24(JREFE)(1:11).EQ.'BI_CERC_INT') THEN
             NOECHO(I,9) = 'BI_CERCI'
           ELSEIF (ZK24(JREFE)(1:7).NE.'DISCRET') THEN
             NOECHO(I,9) = ZK24(JREFE)(1:8)
           ENDIF
           IF (INFO.EQ.2) THEN
             CALL UTIMPI('L','OBSTACLE NO : ',1,I)
             CALL UTIMPK(' ',' DE TYPE : ',1,NOECHO(I,9))
           ENDIF
           IF (NOECHO(I,9).EQ.'BI_CERCI' .AND. 
     &         PARCHO(I,30).LT.PARCHO(I,29)) THEN
              CALL UTMESS('F','MDCHOC',' OBSTACLE BI_CERC_INT : '//
     &                  'DIST_2 DOIT ETRE SUPERIEURE OU EGALE A DIST_1')
           ENDIF
        ELSE
C          CAS ANTI-SISMIQUE
           NOECHO(I,9) = 'BI_PLANY'
        ENDIF
        IF (NOECHO(I,9)(1:2).EQ.'BI') THEN
           XJEU = (PARCHO(I,10)-PARCHO(I,7))**2 + 
     &              (PARCHO(I,11)-PARCHO(I,8))**2 +
     &              (PARCHO(I,12)-PARCHO(I,9))**2
        ENDIF
C
        N1 = 0
        IF(MOTFAC.EQ.'CHOC' .OR. MOTFAC.EQ.'FLAMBAGE') THEN
           CALL GETVTX(MOTFAC,'REPERE',II,1,0,REPERE,N1)
           IF (N1.EQ.0) THEN
            REPERE = 'GLOBAL'
           ELSE
            CALL GETVTX(MOTFAC,'REPERE',II,1,1,REPERE,N1)
           ENDIF
           CALL GETVR8(MOTFAC,'ORIG_OBST',II,1,1,TEMPO,N1)
        ENDIF
        N1=-N1
        IF (N1.EQ.3) THEN
          CALL GETVR8(MOTFAC,'ORIG_OBST',II,1,3,TEMPO,N1)
          IF (TYPNUM.EQ.'NUME_DDL_SDASTER') THEN
            PARCHO(I,13) = TEMPO(1)
            PARCHO(I,14) = TEMPO(2)
            PARCHO(I,15) = TEMPO(3)
          ELSE
            CALL GETVTX(MOTFAC,'REPERE',II,1,1,REPERE,N1)
            IF (REPERE.EQ.'GLOBAL') THEN
              PARCHO(I,13) = TEMPO(1)
              PARCHO(I,14) = TEMPO(2)
              PARCHO(I,15) = TEMPO(3)
            ELSE
              CALL JENONU(JEXNOM(MDSSNO,REPERE),IRET)
              IF (IRET.EQ.0) THEN
                CALL UTMESS('F','MDCHOC','ARGUMENT DU MOT-CLE'//
     &                                   ' "REPERE" INCONNU')
              ENDIF
              CALL WKVECT('&&MDCHOC.COORDO','V V R',3,JCOORD)
              ZR(JCOORD)   = TEMPO(1)
              ZR(JCOORD+1) = TEMPO(2)
              ZR(JCOORD+2) = TEMPO(3)
              CALL ORIENT(MDGENE,REPERE,JCOORD,1,COORD,1)
              PARCHO(I,13) = COORD(1)
              PARCHO(I,14) = COORD(2)
              PARCHO(I,15) = COORD(3)
              CALL JEDETR('&&MDCHOC.COORDO')
            ENDIF
          ENDIF
        ELSE
          PARCHO(I,13) = (PARCHO(I,7)+PARCHO(I,10))/2.D0
          PARCHO(I,14) = (PARCHO(I,8)+PARCHO(I,11))/2.D0
          PARCHO(I,15) = (PARCHO(I,9)+PARCHO(I,12))/2.D0
        ENDIF
        ORIGOB(1) = PARCHO(I,13)
        ORIGOB(2) = PARCHO(I,14)
        ORIGOB(3) = PARCHO(I,15)
        CALL GETVID(MOTFAC,'NOEUD_2'   ,II,1,1,K16B,NN1)
        CALL GETVID(MOTFAC,'GROUP_NO_2',II,1,1,K16B,NN2)
        IF (MOTFAC(1:4).EQ.'CHOC') THEN
          CALL GETVID(MOTFAC,'MAILLE'    ,II,1,1,K16B,NN3)
          CALL GETVID(MOTFAC,'GROUP_MA'  ,II,1,1,K16B,NN4)
        ELSE
          NN3=0
          NN4=0
        ENDIF
        IF (NN1.NE.0.OR.NN2.NE.0.OR.NN3.NE.0.OR.NN4.NE.0) THEN
          DIRCHO(1)=PARCHO(I,7)-PARCHO(I,10)
          DIRCHO(2)=PARCHO(I,8)-PARCHO(I,11)
          DIRCHO(3)=PARCHO(I,9)-PARCHO(I,12)
        ELSE
          DIRCHO(1)=PARCHO(I,7)-PARCHO(I,13)
          DIRCHO(2)=PARCHO(I,8)-PARCHO(I,14)
          DIRCHO(3)=PARCHO(I,9)-PARCHO(I,15)
        ENDIF
        TXNO=SQRT(DIRCHO(1)**2+DIRCHO(2)**2+DIRCHO(3)**2)
        IF (TXNO.EQ.0.D0) TXNO=1.D0
C
C DEBUG : UN TRAVAIL DOIT ETRE FAIT SI TXNO = 0.
C 
           PARCHO(I,44)=DIRCHO(1)/TXNO
           PARCHO(I,45)=DIRCHO(2)/TXNO
           PARCHO(I,46)=DIRCHO(3)/TXNO
C
C ---   RECHERCHE DES ANGLES NAUTIQUES
        N1=0
        IF (MOTFAC.EQ.'CHOC' .OR. MOTFAC.EQ.'FLAMBAGE') THEN
           CALL GETVR8(MOTFAC,'NORM_OBST',II,1,3,TXLOC,N1)
           CALL GETVR8(MOTFAC,'ANGL_VRIL',II,1,1,ANGL,N1)
           IF (N1.NE.0) THEN
              IF (TYPNUM.EQ.'NUME_DDL_SDASTER'.OR.
     &            REPERE.EQ.'GLOBAL') THEN
                CALL ANGVX(TXLOC,ALPHA,BETA)
                PARCHO(I,16) = SIN(ALPHA)
                PARCHO(I,17) = COS(ALPHA)
                PARCHO(I,18) = SIN(BETA)
                PARCHO(I,19) = COS(BETA)
              ELSE
                CALL WKVECT('&&MDCHOC.NORM','V V R',3,JNORM)
                ZR(JNORM)   = TXLOC(1)
                ZR(JNORM+1) = TXLOC(2)
                ZR(JNORM+2) = TXLOC(3)
                CALL ORIENT(MDGENE,REPERE,JNORM,1,NORMX,0)
                CALL ANGVX(NORMX,ALPHA,BETA)
                PARCHO(I,16) = SIN(ALPHA)
                PARCHO(I,17) = COS(ALPHA)
                PARCHO(I,18) = SIN(BETA)
                PARCHO(I,19) = COS(BETA)
                CALL JEDETR('&&MDCHOC.NORM')
              ENDIF
              PARCHO(I,20) = SIN(ANGL*RAD)
              PARCHO(I,21) = COS(ANGL*RAD)
           ELSEIF (NOECHO(I,9).EQ.'BI_PLANY') THEN
              TYLOC(1) = (PARCHO(I,10) - PARCHO(I,7))
              TYLOC(2) = (PARCHO(I,11) - PARCHO(I,8))
              TYLOC(3) = (PARCHO(I,12) - PARCHO(I,9))
              IF (TYPNUM.EQ.'NUME_DDL_SDASTER'.OR.
     &           REPERE.EQ.'GLOBAL') THEN
                 CALL ANGVXY(TXLOC,TYLOC,ANG)
                 PARCHO(I,16) = SIN(ANG(1))
                 PARCHO(I,17) = COS(ANG(1))
                 PARCHO(I,18) = SIN(ANG(2))
                 PARCHO(I,19) = COS(ANG(2))
                 PARCHO(I,20) = SIN(ANG(3))
                 PARCHO(I,21) = COS(ANG(3))
              ELSE
                 CALL WKVECT('&&MDCHOC.NORM','V V R',3,JNORM)
                 ZR(JNORM)   = TXLOC(1)
                 ZR(JNORM+1) = TXLOC(2)
                 ZR(JNORM+2) = TXLOC(3)
                 CALL ORIENT(MDGENE,REPERE,JNORM,1,NORMX,0)
                 ZR(JNORM)   = TYLOC(1)
                 ZR(JNORM+1) = TYLOC(2)
                 ZR(JNORM+2) = TYLOC(3)
                 CALL ORIENT(MDGENE,REPERE,JNORM,1,NORMY,0)
                 CALL ANGVXY(NORMX,NORMY,ANG)
                 PARCHO(I,16) = SIN(ANG(1))
                 PARCHO(I,17) = COS(ANG(1))
                 PARCHO(I,18) = SIN(ANG(2))
                 PARCHO(I,19) = COS(ANG(2))
                 PARCHO(I,20) = SIN(ANG(3))
                 PARCHO(I,21) = COS(ANG(3))
                 CALL JEDETR('&&MDCHOC.NORM')
              ENDIF
            ELSEIF (NOECHO(I,9).EQ.'BI_PLANZ') THEN
              TZLOC(1) = (PARCHO(I,10) - PARCHO(I,7))
              TZLOC(2) = (PARCHO(I,11) - PARCHO(I,8))
              TZLOC(3) = (PARCHO(I,12) - PARCHO(I,9))
             CALL PROVEC(TZLOC,TXLOC,TYLOC)
              IF (TYPNUM.EQ.'NUME_DDL_SDASTER'.OR.
     &            REPERE.EQ.'GLOBAL') THEN
                  CALL ANGVXY(TXLOC,TYLOC,ANG)
                  PARCHO(I,16) = SIN(ANG(1))
                  PARCHO(I,17) = COS(ANG(1))
                  PARCHO(I,18) = SIN(ANG(2))
                  PARCHO(I,19) = COS(ANG(2))
                  PARCHO(I,20) = SIN(ANG(3))
                  PARCHO(I,21) = COS(ANG(3))
              ELSE
                  CALL WKVECT('&&MDCHOC.NORM','V V R',3,JNORM)
                  ZR(JNORM)   = TXLOC(1)
                  ZR(JNORM+1) = TXLOC(2)
                  ZR(JNORM+2) = TXLOC(3)
                  CALL ORIENT(MDGENE,REPERE,JNORM,1,NORMX,0)
                  ZR(JNORM)   = TZLOC(1)
                  ZR(JNORM+1) = TZLOC(2)
                  ZR(JNORM+2) = TZLOC(3)
                  CALL ORIENT(MDGENE,REPERE,JNORM,1,NORMY,0)
                  CALL ANGVXY(NORMX,NORMY,ANG)
                  PARCHO(I,16) = SIN(ANG(1))
                  PARCHO(I,17) = COS(ANG(1))
                  PARCHO(I,18) = SIN(ANG(2))
                  PARCHO(I,19) = COS(ANG(2))
                  PARCHO(I,20) = SIN(ANG(3))
                  PARCHO(I,21) = COS(ANG(3))
                  CALL JEDETR('&&MDCHOC.NORM')
              ENDIF
            ELSE
              CALL UTMESS('I','MDCHOC',' GAMMA = 0 :VALEUR PAR'//
     &                                   ' DEFAUT ')
              ANGL = 0.D0
              IF (TYPNUM.EQ.'NUME_DDL_SDASTER'.OR.
     &            REPERE.EQ.'GLOBAL') THEN
                CALL ANGVX(TXLOC,ALPHA,BETA)
                PARCHO(I,16) = SIN(ALPHA)
                PARCHO(I,17) = COS(ALPHA)
                PARCHO(I,18) = SIN(BETA)
                PARCHO(I,19) = COS(BETA)
              ELSE
                CALL WKVECT('&&MDCHOC.NORM','V V R',3,JNORM)
                ZR(JNORM)   = TXLOC(1)
                ZR(JNORM+1) = TXLOC(2)
                ZR(JNORM+2) = TXLOC(3)
                CALL ORIENT(MDGENE,REPERE,JNORM,1,NORMX,0)
                CALL ANGVX(NORMX,ALPHA,BETA)
                PARCHO(I,16) = SIN(ALPHA)
                PARCHO(I,17) = COS(ALPHA)
                PARCHO(I,18) = SIN(BETA)
                PARCHO(I,19) = COS(BETA)
                CALL JEDETR('&&MDCHOC.NORM')
              ENDIF
              PARCHO(I,20) = SIN(ANGL*RAD)
              PARCHO(I,21) = COS(ANGL*RAD)
            ENDIF
        ELSE
C ---      CAS DE DAS
           PARCHO(I,29)= SQRT(XJEU)/2.D0
           PARCHO(I,30)= SQRT(XJEU)/2.D0
C          VECTEUR NOEUD1 VERS NOEUD2
           TYLOC(1) = (PARCHO(I,10) - PARCHO(I,7))
           TYLOC(2) = (PARCHO(I,11) - PARCHO(I,8))
           TYLOC(3) = (PARCHO(I,12) - PARCHO(I,9))
           CALL NORMEV(TYLOC,RNORM)
           IF (RNORM .EQ. 0.0D0) THEN
              CALL UTMESS('F','MDCHOC',' DISPOSITIF ANTI-SISMIQUE : '//
     &                    ' LA DISTANCE DES NOEUDS 1 ET 2 EST NULLE')
           ENDIF
C          DETERMINATION DES AXES LOCAUX
           IF (ABS(TYLOC(3)).LE.ABS(TYLOC(1)) .AND.
     &         ABS(TYLOC(3)).LE.ABS(TYLOC(2))) THEN
               TZLOC(1) = -TYLOC(2)
               TZLOC(2) = TYLOC(1)
               TZLOC(3) = 0.D0
           ELSEIF (ABS(TYLOC(2)).LE.ABS(TYLOC(1)) .AND.
     &             ABS(TYLOC(2)).LE.ABS(TYLOC(3))) THEN
               TZLOC(1) = -TYLOC(3)
               TZLOC(2) = 0.D0
               TZLOC(3) = TYLOC(1)
           ELSE
               TZLOC(1) = 0.D0
               TZLOC(2) = -TYLOC(3)
               TZLOC(3) = TYLOC(2)
           ENDIF
           CALL PROVEC(TYLOC,TZLOC,TXLOC)
           IF (TYPNUM.EQ.'NUME_DDL_SDASTER'.OR.
     &         REPERE.EQ.'GLOBAL') THEN
              CALL ANGVXY(TXLOC,TYLOC,ANG)
              PARCHO(I,16) = SIN(ANG(1))
              PARCHO(I,17) = COS(ANG(1))
              PARCHO(I,18) = SIN(ANG(2))
              PARCHO(I,19) = COS(ANG(2))
              PARCHO(I,20) = SIN(ANG(3))
              PARCHO(I,21) = COS(ANG(3))
           ELSE
             CALL WKVECT('&&MDCHOC.NORM','V V R',3,JNORM)
             ZR(JNORM)   = TXLOC(1)
             ZR(JNORM+1) = TXLOC(2)
             ZR(JNORM+2) = TXLOC(3)
             CALL ORIENT(MDGENE,REPERE,JNORM,1,NORMX,0)
             ZR(JNORM)   = TYLOC(1)
             ZR(JNORM+1) = TYLOC(2)
             ZR(JNORM+2) = TYLOC(3)
             CALL ORIENT(MDGENE,REPERE,JNORM,1,NORMY,0)
             CALL ANGVXY(NORMX,NORMY,ANG)
             PARCHO(I,16) = SIN(ANG(1))
             PARCHO(I,17) = COS(ANG(1))
             PARCHO(I,18) = SIN(ANG(2))
             PARCHO(I,19) = COS(ANG(2))
             PARCHO(I,20) = SIN(ANG(3))
             PARCHO(I,21) = COS(ANG(3))
             CALL JEDETR('&&MDCHOC.NORM')
          ENDIF
        ENDIF
        SINA = PARCHO(I,16)
        COSA = PARCHO(I,17)
        SINB = PARCHO(I,18)
        COSB = PARCHO(I,19)
        SING = PARCHO(I,20)
        COSG = PARCHO(I,21)
        IF (INFO.EQ.2) THEN
          CALL UTDEBM('I',NOMCMD,' INFOS NOEUDS DE CHOC')
          CALL UTIMPI('L','LIEU DE CHOC  : ',1,I)
          CALL UTIMPK('L','NOEUD DE CHOC  : ',1,NOECHO(I,1))
          IF (TYPNUM(1:13).EQ.'NUME_DDL_GENE') THEN
            CALL UTIMPK('L','SOUS-STRUCTURE : ',1,NOECHO(I,2))
          ENDIF
          CALL UTIMPR('L','COORDONNEES    : X : ',1,PARCHO(I,7))
          CALL UTIMPR('L','                 Y : ',1,PARCHO(I,8))
          CALL UTIMPR('L','                 Z : ',1,PARCHO(I,9))
          IF ( NOECHO(I,9)(1:2).EQ.'BI') THEN
            CALL UTIMPK('L','NOEUD DE CHOC  : ',1,NOECHO(I,5))
            IF (TYPNUM(1:13).EQ.'NUME_DDL_GENE') THEN
              CALL UTIMPK('L','SOUS-STRUCTURE : ',1,NOECHO(I,6))
            ENDIF
            CALL UTIMPR('L','COORDONNEES    : X : ',1,PARCHO(I,10))
            CALL UTIMPR('L','                 Y : ',1,PARCHO(I,11))
            CALL UTIMPR('L','                 Z : ',1,PARCHO(I,12))
          ENDIF
          CALL UTIMPR('L','AMORTISSEMENT TANGENT UTILISE : ',1,CTANG)
          CALL UTIMPR('L','ORIGINE CHOC X : ',1,PARCHO(I,13))
          CALL UTIMPR('L','             Y : ',1,PARCHO(I,14))
          CALL UTIMPR('L','             Z : ',1,PARCHO(I,15))
          CALL UTIMPR('L','NORM_OBST SIN(ALPHA) : ',1,PARCHO(I,16))
          CALL UTIMPR('L','          COS(ALPHA) : ',1,PARCHO(I,17))
          CALL UTIMPR('L','          SIN(BETA)  : ',1,PARCHO(I,18))
          CALL UTIMPR('L','          COS(BETA)  : ',1,PARCHO(I,19))
          CALL UTIMPR('L','ANGL_VRILLE : SIN(GAMMA) : ',1,PARCHO(I,20))
          CALL UTIMPR('L','              COS(GAMMA) : ',1,PARCHO(I,21))
          IF ( NOECHO(I,9)(1:2).EQ.'BI') THEN
             IF (I.LE.NBCHOC) THEN
                XJEU = SQRT(XJEU) - (PARCHO(I,29)+PARCHO(I,30))
             ELSE
                XJEU = SQRT(XJEU)
             ENDIF
             CALL UTIMPR('L','JEU INITIAL : ',1,XJEU)
          ENDIF
          CALL UTFINM( )
        ENDIF
C
C       POSITION INITIALE DU NOEUD 1 DANS LE REPERE GLOBAL
        DPIGLO(1) = PARCHO(I,7)
        DPIGLO(2) = PARCHO(I,8)
        DPIGLO(3) = PARCHO(I,9)
C       --- PASSAGE DANS LE REPERE LOCAL --- POUR LE NOEUD 1
        CALL GLOLOC(DPIGLO,ORIGOB,SINA,COSA,SINB,COSB,SING,COSG,DPILOC)
C       POSITON INITIALE DIFFERENTIELLE = DPILOC SI 1 NOEUD
        DDPILO(1) = DPILOC(1)
        DDPILO(2) = DPILOC(2)
        DDPILO(3) = DPILOC(3)
C
        IF ( NOECHO(I,9)(1:2).EQ.'BI') THEN
C          POSITION INITIALE DU NOEUD 2 DANS LE REPERE GLOBAL
           DPIGLO(4) = PARCHO(I,10)
           DPIGLO(5) = PARCHO(I,11)
           DPIGLO(6) = PARCHO(I,12)
C          --- PASSAGE DANS LE REPERE LOCAL --- POUR LE NOEUD 2
           CALL GLOLOC(DPIGLO(4),ORIGOB,SINA,COSA,SINB,COSB,SING,COSG,
     +                 DPILOC(4))
C          POSITION INITIALE DU NOEUD1 PAR RAPPORT AU NOEUD2
           DDPILO(1) = DPILOC(1)-DPILOC(4)
           DDPILO(2) = DPILOC(2)-DPILOC(5)
           DDPILO(3) = DPILOC(3)-DPILOC(6)
        ENDIF
        PARCHO(I,36)= -SIGN(UN,DDPILO(2))
        PARCHO(I,37)= -SIGN(UN,DDPILO(3))

 10   CONTINUE
C
C     --- REMPLISSAGE DE DPLMOD(I,J,K) ---
C
C --- CALCUL DIRECT
      IF (TYPNUM.EQ.'NUME_DDL_SDASTER') THEN
        DO 113 I=1,NBNLI
          DO 114 J=1,NBMODE
            DPLMOD(I,J,1) = BMODAL(ZI(JDDL-1+6*(I-1)+1),J)
            DPLMOD(I,J,2) = BMODAL(ZI(JDDL-1+6*(I-1)+2),J)
            DPLMOD(I,J,3) = BMODAL(ZI(JDDL-1+6*(I-1)+3),J)
            IF (NOECHO(I,9)(1:2).EQ.'BI') THEN
              DPLMOD(I,J,4) = BMODAL(ZI(JDDL-1+6*(I-1)+4),J)
              DPLMOD(I,J,5) = BMODAL(ZI(JDDL-1+6*(I-1)+5),J)
              DPLMOD(I,J,6) = BMODAL(ZI(JDDL-1+6*(I-1)+6),J)
            ELSE
              DPLMOD(I,J,4) = 0.D0
              DPLMOD(I,J,5) = 0.D0
              DPLMOD(I,J,6) = 0.D0
            ENDIF
114       CONTINUE
113     CONTINUE
C
C --- CALCUL PAR SOUS-STRUCTURATION
      ELSEIF (TYPNUM(1:13).EQ.'NUME_DDL_GENE') THEN
        NUMERO(1:14) = NUMDDL
        DO 115 I=1,NBNLI
          CALL WKVECT('&&MDCHOC.DPLCHO','V V R8',NBMODE*6,JDPL)
          NOEUD(1) = NOECHO(I,1)
          NOEUD(2) = NOECHO(I,2)
          NOEUD(3) = NOECHO(I,3)
          CALL RESMOD(BMODAL,NBMODE,NEQ,NUMERO,MDGENE,NOEUD,ZR(JDPL))
          DO 125 J=1,NBMODE
            DPLMOD(I,J,1) = ZR(JDPL-1+J)
            DPLMOD(I,J,2) = ZR(JDPL-1+J+NBMODE)
            DPLMOD(I,J,3) = ZR(JDPL-1+J+2*NBMODE)
125       CONTINUE
          IF (NOECHO(I,9)(1:2).EQ.'BI') THEN
            NOEUD(1) = NOECHO(I,5)
            NOEUD(2) = NOECHO(I,6)
            NOEUD(3) = NOECHO(I,7)
            CALL RESMOD(BMODAL,NBMODE,NEQ,NUMERO,MDGENE,NOEUD,ZR(JDPL))
            DO 126 J=1,NBMODE
              DPLMOD(I,J,4) = ZR(JDPL-1+J)
              DPLMOD(I,J,5) = ZR(JDPL-1+J+NBMODE)
              DPLMOD(I,J,6) = ZR(JDPL-1+J+2*NBMODE)
126         CONTINUE
          ELSE
            DO 116 J=1,NBMODE
              DPLMOD(I,J,4) = 0.D0
              DPLMOD(I,J,5) = 0.D0
              DPLMOD(I,J,6) = 0.D0
116         CONTINUE
          ENDIF
          CALL JEDETR('&&MDCHOC.DPLCHO')
115     CONTINUE
C
      ENDIF
C
C     --- REMPLISSAGE DE PS2DEL(I,J,K) ---
C
      IF (MONMOT(1:3).EQ.'OUI') THEN
C
C ----- CALCUL DIRECT
        IF (TYPNUM.EQ.'NUME_DDL_SDASTER') THEN
          DO 123 I=1,NBNLI
            DO 124 J=1,NEXCIT
              PS2DEL(I,J,1) = PS1DEL(ZI(JDDL-1+6*(I-1)+1),J)
              PS2DEL(I,J,2) = PS1DEL(ZI(JDDL-1+6*(I-1)+2),J)
              PS2DEL(I,J,3) = PS1DEL(ZI(JDDL-1+6*(I-1)+3),J)
              IF (NOECHO(I,9)(1:2).EQ.'BI') THEN
                PS2DEL(I,J,4) = PS1DEL(ZI(JDDL-1+6*(I-1)+4),J)
                PS2DEL(I,J,5) = PS1DEL(ZI(JDDL-1+6*(I-1)+5),J)
                PS2DEL(I,J,6) = PS1DEL(ZI(JDDL-1+6*(I-1)+6),J)
              ELSE
                PS2DEL(I,J,4) = 0.D0
                PS2DEL(I,J,5) = 0.D0
                PS2DEL(I,J,6) = 0.D0
              ENDIF
124         CONTINUE
123       CONTINUE
C
C ----- CALCUL PAR SOUS-STRUCTURATION
        ELSEIF (TYPNUM(1:13).EQ.'NUME_DDL_GENE') THEN
          IER = IER + 1
          CALL UTMESS('E',NOMCMD,'LE MULTI-APPUI + SOUS-'//
     +   'STRUCTURATION N''EST PAS DEVELOPPE - BON COURAGE')
        ENDIF
C
      ENDIF
C
C ---- VERIFICATION DE COHERENCE ENTRE CHOC ET FLAMBAGE ---
      IF (NBCHOC.NE.0 .AND. NBFLAM.NE.0) THEN
         DO 140 I=1,NBCHOC
            J = NBCHOC+NBSISM
 130        CONTINUE
            J = J + 1
            IF (J.LE. NBNLI) THEN
               IF (NOECHO(I,1).NE.NOECHO(J,1)) GOTO 130
               IF (NOECHO(I,5).NE.NOECHO(J,5)) GOTO 130
               CALL UTMESS('A',NOMCMD,'CONFLIT ENTRE CHOC ET '//
     &                    'FLAMBAGE AU MEME LIEU DE CHOC : LE '//
     &                    'CALCUL SERA DE TYPE FLAMBAGE')
               PARCHO(I,2) = 0.D0
               PARCHO(I,4) = 0.D0
            ENDIF
 140     CONTINUE
      ENDIF
      CALL JEDETR('&&MDCHOC.DDLCHO')
C
      CALL JEDEMA()
      END

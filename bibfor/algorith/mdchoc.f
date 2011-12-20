      SUBROUTINE MDCHOC (NBNLI,NBCHOC,NBFLAM,NBSISM,NBRFIS,NBPAL,LOGCHO,
     &                   DPLMOD,PARCHO,NOECHO,INTITU,PS1DEL,PS2DEL,
     &                   NUMDDL,NBMODE,PULSAT,MASGEN,LAMOR,AMOGEN,
     &                   BMODAL,NEQ,NEXCIT,INFO,LFLU,MONMOT,IER)
      IMPLICIT  NONE
      INTEGER            NBNLI, NBCHOC,NBFLAM, NBSISM, NBMODE, NEQ
      INTEGER            NBRFIS, NBPAL
      INTEGER            LOGCHO(NBNLI,*), IER, NEXCIT, INFO
      REAL*8             PARCHO(NBNLI,*),PULSAT(*),MASGEN(*),AMOGEN(*)
      REAL*8             DPLMOD(NBNLI,NBMODE,*),BMODAL(NEQ,*)
      REAL*8             PS1DEL(NEQ,NEXCIT),PS2DEL(NBNLI,NEXCIT,*)
      CHARACTER*8        NOECHO(NBNLI,*), INTITU(*), MONMOT
      CHARACTER*14       NUMDDL
      LOGICAL            LAMOR, LFLU
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/12/2011   AUTEUR BOYERE E.BOYERE 
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
C
C     STOCKAGE DES INFORMATIONS DE CHOC DANS DES TABLEAUX
C     ------------------------------------------------------------------
C IN  : NBNLI  : DIMENSION DES TABLEAUX (NBCHOC+NBSISM+NBFLAM+NBRFIS)
C IN  : NBCHOC : NOMBRE DE POINTS DE CHOC
C IN  : NBFLAM : NOMBRE DE CHOCS AVEC FLAMBEMENT
C IN  : NBPAL : NOMBRE DE PALIERS (COUPLAGE EDYOS)
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
C                PARCHO(I, 6)= COULOMB_DYNA
C                PARCHO(I, 7)= COULOMB_STAT
C                PARCHO(I, 8)= COOR INIT NOEUD_1 X REP GLOBAL
C                PARCHO(I, 9)= COOR INIT NOEUD_1 Y REP GLOBAL
C                PARCHO(I,10)= COOR INIT NOEUD_1 Z REP GLOBAL
C                PARCHO(I,11)= COOR INIT NOEUD_2 X REP GLOBAL
C                PARCHO(I,12)= COOR INIT NOEUD_2 Y REP GLOBAL
C                PARCHO(I,13)= COOR INIT NOEUD_2 Z REP GLOBAL
C                PARCHO(I,14)= COOR ORIGINE OBSTACLE X REP GLOBAL
C                PARCHO(I,15)= COOR ORIGINE OBSTACLE Y REP GLOBAL
C                PARCHO(I,16)= COOR ORIGINE OBSTACLE Z REP GLOBAL
C                PARCHO(I,17)= SIN A
C                PARCHO(I,18)= COS A
C                PARCHO(I,19)= SIN B
C                PARCHO(I,20)= COS B
C                PARCHO(I,21)= SIN G
C                PARCHO(I,22)= COS G
C                PARCHO(I,23)= X AVANT ADHERENCE
C                PARCHO(I,24)= Y AVANT ADHERENCE
C                PARCHO(I,25)= Z AVANT ADHERENCE
C                PARCHO(I,26)= FT1 AVANT ADHERENCE
C                PARCHO(I,27)= FT2 AVANT ADHERENCE
C                PARCHO(I,28)= VT1 PAS PRECEDENT
C                PARCHO(I,29)= VT2 PAS PRECEDENT
C                PARCHO(I,30)= DIST_1 DU NOEUD_1
C                PARCHO(I,31)= DIST_2 DU NOEUD_2
C                PARCHO(I,32)= COEF A FORCE FLUIDE
C                PARCHO(I,33)= COEF B FORCE FLUIDE
C                PARCHO(I,34)= COEF C FORCE FLUIDE
C                PARCHO(I,35)= COEF D FORCE FLUIDE
C                PARCHO(I,36)= COUCHE LIMITE
C                PARCHO(I,37)= SIGNE DE Y20LOC-Y10LOC
C                PARCHO(I,38)= SIGNE DE Z20LOC-Z10LOC
C                PARCHO(I,39)= COEF RIGI_K1 DISPO ANTI SISMIQUE
C                PARCHO(I,40)= COEF RIGI_K2 DISPO ANTI SISMIQUE
C                PARCHO(I,41)= COEF SEUIL_FX DISPO ANTI SISMIQUE
C                PARCHO(I,42)= COEF C DISPO ANTI SISMIQUE
C                PARCHO(I,43)= COEF PUIS_ALPHA DISPO ANTI SISMIQUE
C                PARCHO(I,44)= COEF DX_MAX DISPO ANTI SISMIQUE
C                PARCHO(I,45)= NORMALE X
C                PARCHO(I,46)= NORMALE Y
C                PARCHO(I,47)= NORMALE Z
C                PARCHO(I,48)= TAUX DE RESTITUTION (CALCULE DANS CRICHO)
C                PARCHO(I,49)= TAUX DE RESTITUTION (CALCULE DANS CRICHO)
C                PARCHO(I,50)= FORCE LIMITE DE FLAMBAGE
C                PARCHO(I,51)= PALIER FORCE DE REACTION APRES FLAMBAGE
C                PARCHO(I,52)= RIGIDITE APRES FLAMBAGE
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER       IMODE, IAMOR, IM, I, J, JDPL, JDDL, LREFE
      INTEGER VALI
      REAL*8        DPILOC(6), DPIGLO(6), DDPILO(3), ORIGOB(3), UN
      REAL*8 VALR(10)
      REAL*8        SINA, COSA, SINB, COSB, SING, COSG, XJEU, XMAS,
     &              CTANG
      CHARACTER*8   NOEUD(3)
      CHARACTER*16  TYPNUM
      CHARACTER*24  MDGENE, NUMERO
      CHARACTER*24  VALK
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      IER  = 0
      UN   = 1.D0
      LFLU = .FALSE.
      NUMERO = ' '
      MDGENE = ' '
      CALL GETTCO ( NUMDDL, TYPNUM )
      IF (TYPNUM(1:13).EQ.'NUME_DDL_GENE') THEN
         IF (NBSISM.GT.0 .OR. NBFLAM.GT.0) THEN
            CALL U2MESS('F','ALGORITH5_36')
         ENDIF
      ENDIF
C
C --- RECHERCHE DU MODE DE MASSE LA PLUS ELEVEE ---
C
      XMAS  = MASGEN(1)
      IMODE = 1
      DO 10 IM = 2 , NBMODE
         IF (MASGEN(IM).GT.XMAS) THEN
            XMAS = MASGEN(IM)
            IMODE = IM
         ENDIF
 10   CONTINUE
      IF ( LAMOR ) THEN
         IAMOR = IMODE
      ELSE
         IAMOR = IMODE + NBMODE*( IMODE - 1 )
      ENDIF
C
      DO 20 I = 1 , NBNLI
         DO 22 J = 1, 5
            LOGCHO(I,J) = 0
 22      CONTINUE
         DO 24 J = 1, 9
            NOECHO(I,J) = ' '
 24      CONTINUE
         DO 26 J = 1, 52
            PARCHO(I,J) = 0.D0
 26      CONTINUE
 20   CONTINUE
C
      CALL WKVECT ( '&&MDCHOC.DDLCHO', 'V V I', NBNLI*6, JDDL )
C
C --- CALCUL DIRECT
C
      IF (TYPNUM.EQ.'NUME_DDL_SDASTER') THEN
C         ----------------------------
         CALL MDCHST( NUMDDL, TYPNUM, IMODE, IAMOR, PULSAT, MASGEN,
     &                AMOGEN,LFLU,NBNLI,NBPAL,NOECHO,NBRFIS,LOGCHO,
     &                PARCHO, INTITU, ZI(JDDL), IER )

C --- CALCUL PAR SOUS-STRUCTURATION
C
      ELSEIF (TYPNUM(1:13).EQ.'NUME_DDL_GENE') THEN
C             ------------------------------
         CALL MDCHGE ( NUMDDL, TYPNUM, IMODE, IAMOR, PULSAT, MASGEN,
     &                 AMOGEN, LFLU, NBNLI, NOECHO, LOGCHO, PARCHO,
     &                 INTITU, ZI(JDDL), IER )
C
      ENDIF
C
      NBNLI = NBNLI - NBPAL
      DO 100 I = 1,NBNLI
C
        CTANG = PARCHO(I,5)
C
        ORIGOB(1) = PARCHO(I,14)
        ORIGOB(2) = PARCHO(I,15)
        ORIGOB(3) = PARCHO(I,16)
C
        SINA = PARCHO(I,17)
        COSA = PARCHO(I,18)
        SINB = PARCHO(I,19)
        COSB = PARCHO(I,20)
        SING = PARCHO(I,21)
        COSG = PARCHO(I,22)
C
        IF (INFO.EQ.2) THEN
          VALI = I
          VALK = NOECHO(I,1)
          CALL U2MESG('I','ALGORITH16_2',1,VALK,1,VALI,0,0.D0)
          IF (TYPNUM(1:13).EQ.'NUME_DDL_GENE') THEN
            VALK = NOECHO(I,2)
            CALL U2MESG('I','ALGORITH16_3',1,VALK,0,0,0,0.D0)
          ENDIF
          VALR (1) = PARCHO(I,8)
          VALR (2) = PARCHO(I,9)
          VALR (3) = PARCHO(I,10)
          CALL U2MESG('I','ALGORITH16_4',0,' ',0,0,3,VALR)
          IF ( NOECHO(I,9)(1:2).EQ.'BI') THEN
            VALK = NOECHO(I,5)
            CALL U2MESG('I','ALGORITH16_5',1,VALK,0,0,0,0.D0)
            IF (TYPNUM(1:13).EQ.'NUME_DDL_GENE') THEN
              VALK = NOECHO(I,6)
              CALL U2MESG('I','ALGORITH16_3',1,VALK,0,0,0,0.D0)
            ENDIF
            VALR (1) = PARCHO(I,11)
            VALR (2) = PARCHO(I,12)
            VALR (3) = PARCHO(I,13)
            CALL U2MESG('I','ALGORITH16_4',0,' ',0,0,3,VALR)
          ENDIF
          VALR (1) = CTANG
          VALR (2) = PARCHO(I,14)
          VALR (3) = PARCHO(I,15)
          VALR (4) = PARCHO(I,16)
          VALR (5) = PARCHO(I,17)
          VALR (6) = PARCHO(I,18)
          VALR (7) = PARCHO(I,19)
          VALR (8) = PARCHO(I,20)
          VALR (9) = PARCHO(I,21)
          VALR (10)= PARCHO(I,22)
          CALL U2MESG('I','ALGORITH16_8',0,' ',0,0,10,VALR)
          IF ( NOECHO(I,9)(1:2).EQ.'BI') THEN
             XJEU = (PARCHO(I,11)-PARCHO(I,8))**2 +
     &              (PARCHO(I,12)-PARCHO(I,9))**2 +
     &              (PARCHO(I,13)-PARCHO(I,10))**2
             IF (I.LE.NBCHOC) THEN
                XJEU = SQRT(XJEU) - (PARCHO(I,30)+PARCHO(I,31))
             ELSE
                XJEU = SQRT(XJEU)
             ENDIF
             VALR (1) = XJEU
             CALL U2MESG('I','ALGORITH16_9',0,' ',0,0,1,VALR)
          ENDIF
          CALL U2MESG('I','VIDE_1',0,' ',0,0,0,0.D0)
        ENDIF
C
C       POSITION INITIALE DU NOEUD 1 DANS LE REPERE GLOBAL
        DPIGLO(1) = PARCHO(I,8)
        DPIGLO(2) = PARCHO(I,9)
        DPIGLO(3) = PARCHO(I,10)
C       --- PASSAGE DANS LE REPERE LOCAL --- POUR LE NOEUD 1
        CALL GLOLOC(DPIGLO,ORIGOB,SINA,COSA,SINB,COSB,SING,COSG,DPILOC)
C       POSITON INITIALE DIFFERENTIELLE = DPILOC SI 1 NOEUD
        DDPILO(1) = DPILOC(1)
        DDPILO(2) = DPILOC(2)
        DDPILO(3) = DPILOC(3)
C
        IF ( NOECHO(I,9)(1:2).EQ.'BI') THEN
C          POSITION INITIALE DU NOEUD 2 DANS LE REPERE GLOBAL
           DPIGLO(4) = PARCHO(I,11)
           DPIGLO(5) = PARCHO(I,12)
           DPIGLO(6) = PARCHO(I,13)
C          --- PASSAGE DANS LE REPERE LOCAL --- POUR LE NOEUD 2
           CALL GLOLOC(DPIGLO(4),ORIGOB,SINA,COSA,SINB,COSB,SING,COSG,
     &                 DPILOC(4))
C          POSITION INITIALE DU NOEUD1 PAR RAPPORT AU NOEUD2
           DDPILO(1) = DPILOC(1)-DPILOC(4)
           DDPILO(2) = DPILOC(2)-DPILOC(5)
           DDPILO(3) = DPILOC(3)-DPILOC(6)
        ENDIF
        PARCHO(I,37)= -SIGN(UN,DDPILO(2))
        PARCHO(I,38)= -SIGN(UN,DDPILO(3))

 100  CONTINUE
C
C --- REMPLISSAGE DE DPLMOD(I,J,K) ---
C
      IF (TYPNUM.EQ.'NUME_DDL_SDASTER') THEN
C         ----------------------------
         DO 200 I=1,NBNLI
            DO 210 J=1,NBMODE
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
 210        CONTINUE
 200     CONTINUE
C  COUPLAGE AVEC EDYOS
        IF ( NBPAL .GT. 0 ) THEN
            DO 500 I=NBNLI+1,NBNLI+NBPAL
               DO 510 J=1,NBMODE
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
 510           CONTINUE
 500        CONTINUE
        ENDIF
C  FIN COUPLAGE AVEC EDYOS
C
C  ROTOR FISSURE
        IF ( NBRFIS .GT. 0 ) THEN
            DO 600 I=NBNLI+1-NBRFIS,NBNLI
               DO 610 J=1,NBMODE
                  DPLMOD(I,J,1) = BMODAL(ZI(JDDL-1+6*(I-1)+1),J)
                  DPLMOD(I,J,2) = BMODAL(ZI(JDDL-1+6*(I-1)+2),J)
                  DPLMOD(I,J,3) = BMODAL(ZI(JDDL-1+6*(I-1)+3),J)
                  DPLMOD(I,J,4) = BMODAL(ZI(JDDL-1+6*(I-1)+4),J)
                  DPLMOD(I,J,5) = BMODAL(ZI(JDDL-1+6*(I-1)+5),J)
                  DPLMOD(I,J,6) = BMODAL(ZI(JDDL-1+6*(I-1)+6),J)
 610           CONTINUE
 600        CONTINUE
        ENDIF
C  FIN ROTOR FISSURE
C
      ELSEIF (TYPNUM(1:13).EQ.'NUME_DDL_GENE') THEN
C             -------------------------------
         NUMERO(1:14) = NUMDDL
         CALL JEVEUO(NUMDDL//'.NUME.REFN','L',LREFE)
         MDGENE = ZK24(LREFE)
         DO 220 I=1,NBNLI
            CALL WKVECT('&&MDCHOC.DPLCHO','V V R8',NBMODE*6,JDPL)
            NOEUD(1) = NOECHO(I,1)
            NOEUD(2) = NOECHO(I,2)
            NOEUD(3) = NOECHO(I,3)
            CALL RESMOD(BMODAL,NBMODE,NEQ,NUMERO,MDGENE,NOEUD,ZR(JDPL))
            DO 230 J=1,NBMODE
               DPLMOD(I,J,1) = ZR(JDPL-1+J)
               DPLMOD(I,J,2) = ZR(JDPL-1+J+NBMODE)
               DPLMOD(I,J,3) = ZR(JDPL-1+J+2*NBMODE)
 230        CONTINUE
            IF (NOECHO(I,9)(1:2).EQ.'BI') THEN
               NOEUD(1) = NOECHO(I,5)
               NOEUD(2) = NOECHO(I,6)
               NOEUD(3) = NOECHO(I,7)
             CALL RESMOD(BMODAL,NBMODE,NEQ,NUMERO,MDGENE,NOEUD,ZR(JDPL))
               DO 240 J=1,NBMODE
                  DPLMOD(I,J,4) = ZR(JDPL-1+J)
                  DPLMOD(I,J,5) = ZR(JDPL-1+J+NBMODE)
                  DPLMOD(I,J,6) = ZR(JDPL-1+J+2*NBMODE)
 240           CONTINUE
            ELSE
               DO 250 J=1,NBMODE
                  DPLMOD(I,J,4) = 0.D0
                  DPLMOD(I,J,5) = 0.D0
                  DPLMOD(I,J,6) = 0.D0
 250           CONTINUE
            ENDIF
            CALL JEDETR('&&MDCHOC.DPLCHO')
 220     CONTINUE
      ENDIF
C
C --- REMPLISSAGE DE PS2DEL(I,J,K) ---
C
      IF (MONMOT(1:3).EQ.'OUI') THEN
         IF (TYPNUM.EQ.'NUME_DDL_SDASTER') THEN
            DO 300 I=1,NBNLI
               DO 310 J=1,NEXCIT
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
 310           CONTINUE
 300        CONTINUE
         ELSEIF (TYPNUM(1:13).EQ.'NUME_DDL_GENE') THEN
            IER = IER + 1
            CALL U2MESS('E','ALGORITH5_37')
         ENDIF
      ENDIF
C
C --- VERIFICATION DE COHERENCE ENTRE CHOC ET FLAMBAGE ---
C
      IF (NBCHOC.NE.0 .AND. NBFLAM.NE.0) THEN
         DO 140 I=1,NBCHOC
            J = NBCHOC+NBSISM
 130        CONTINUE
            J = J + 1
            IF (J.LE. NBNLI) THEN
               IF (NOECHO(I,1).NE.NOECHO(J,1)) GOTO 130
               IF (NOECHO(I,5).NE.NOECHO(J,5)) GOTO 130
               CALL U2MESS('A','ALGORITH5_38')
               PARCHO(I,2) = 0.D0
               PARCHO(I,4) = 0.D0
            ENDIF
 140     CONTINUE
      ENDIF
      NBNLI = NBNLI + NBPAL
C
      CALL JEDETR('&&MDCHOC.DDLCHO')
C
      CALL JEDEMA()
      END

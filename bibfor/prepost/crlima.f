      SUBROUTINE CRLIMA(MOTFAZ, MATREZ, IOCC, LISMAZ, LONLIS)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     MOTFAZ, MATREZ, LISMAZ
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 25/03/2008   AUTEUR REZETTE C.REZETTE 
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
C TOLE CRP_20
C
C     CREATION DU VECTEUR DE K8 DE NOM LISMAZ ET DE LONGUEUR
C     LONLIS.
C     CE VECTEUR CONTIENT LA LISTE DES ELEMENTS SPECIFIES PAR
C     PAR L'UTILISATEUR :  . SOIT DIRECTEMENT
C                          . SOIT PAR UNE LISTE DE NOEUDS
C                            DONT UN AU-MOINS FIGURE DANS CES ELEMENTS
C
C IN       : MOTFAZ : MOT-CLE FACTEUR 'MATR_ELEM'
C IN       : MATREZ : NOM D'UN MATR_ELEM OU D'UN VECT_ELEM
C IN       : IOCC   : NUMERO D'OCCURENCE DU MOT-FACTEUR
C OUT      : LISMAZ : NOM DE LA LISTE DES MAILLES
C OUT      : LONLIS : LONGUEUR DE LA LISTE DES NOEUDS
C ----------------------------------------------------------------------
C     ----------- COMMUNS NORMALISES  JEVEUX  --------------------------
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
      CHARACTER*32      JEXNOM, JEXNUM
C---------------- FIN COMMUNS NORMALISES  JEVEUX  ----------------------
C
      CHARACTER*1   K1BID
      CHARACTER*19  MATEL
      CHARACTER*8   K8BID, NOMA, NOMNOE, NOMAIL
      CHARACTER*8   MONOEU, MOGRNO, MOMAIL, MOGRMA, MOTOUI
      CHARACTER*16  MOTFAC
      CHARACTER*24  NOEUMA, GRNOMA, MAILMA, GRMAMA, LISMAI
      CHARACTER*24  RESU, NOLI, LIEL, NEMA
      INTEGER       NBNO
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      MATEL  = MATREZ
      MOTFAC = MOTFAZ
      LISMAI = LISMAZ
C
      MONOEU = 'NOEUD'
      MOGRNO = 'GROUP_NO'
      MOMAIL = 'MAILLE'
      MOGRMA = 'GROUP_MA'
C
      CALL GETFAC(MOTFAC,NLIAI)
      IF (NLIAI.EQ.0) GOTO 9999
C
      CALL GETVTX (MOTFAC,'TOUT' ,IOCC,1,0,MOTOUI,NOUI)
      IF (NOUI.NE.0) GOTO 9999
C
C --- RECUPERATION DU MAILLAGE ASSOCIE AU MATR_ELEM :
C     ---------------------------------------------
      CALL DISMOI('F','NOM_MAILLA',MATEL,'MATR_ELEM',IBID,NOMA,IER)

C
      NOEUMA = NOMA//'.NOMNOE'
      GRNOMA = NOMA//'.GROUPENO'
      MAILMA = NOMA//'.NOMMAI'
      GRMAMA = NOMA//'.GROUPEMA'
C
      IDIM1  = 0
      IDIM2  = 0
      IDIM3  = 0
      IDIM4  = 0
      NBMAIL = 0
C
C --- CALCUL DE IDIM1 = NB_NOEUD/GROUP_NO*NB_GROUP_NO
C --- ET VERIFICATION DE L'APPARTENANCE DES GROUP_NO
C --- AUX GROUP_NO DU MAILLAGE :
C     ------------------------
      CALL GETVID (MOTFAC,MOGRNO,IOCC,1,0,K8BID,NG)
      IF (NG.NE.0) THEN
          NG = -NG
          CALL WKVECT ('&&CRLIMA.TRAV1','V V K8',NG,JJJ)
          CALL GETVEM (NOMA,'GROUP_NO',
     .                 MOTFAC,MOGRNO,IOCC,1,NG,ZK8(JJJ),NGR)
          DO 10 IGR = 1, NGR
               CALL JELIRA (JEXNOM(GRNOMA,ZK8(JJJ+IGR-1)),'LONMAX',
     +                      N1,K1BID)
               IDIM1 = IDIM1 + N1
 10       CONTINUE
      ENDIF
C
C --- CALCUL DE IDIM2 = NB_NOEUD DE LA LISTE DE NOEUDS
C --- ET VERIFICATION DE L'APPARTENANCE DES NOEUDS
C --- AUX NOEUDS DU MAILLAGE :
C        -------------------
      CALL GETVID (MOTFAC,MONOEU,IOCC,1,0,K8BID,NBNOE)
      IF (NBNOE.NE.0) THEN
          NBNOE = -NBNOE
          CALL WKVECT ('&&CRLIMA.TRAV2','V V K8',NBNOE,JJJ)
          CALL GETVEM (NOMA,'NOEUD',
     .                 MOTFAC,MONOEU,IOCC,1,NBNOE,ZK8(JJJ),NNO)
          IDIM2 = IDIM2 + NNO
      ENDIF
C
C --- NOMBRE TOTAL DE NOEUDS DONT ON VA CONSTITUER LA LISTE
C --- DES MAILLES S'APPUYANT SUR EUX :
C     ------------------------------
       NBNOLI = IDIM1 + IDIM2
C
C --- CONSTITUTION DE LA LISTE DE NOEUDS :
C     ---------------------------------
      IF (NBNOLI.NE.0) THEN
C
C ---   ALLOCATION DU TABLEAU DES NOMS DE NOEUDS :
C       ----------------------------------------
        CALL WKVECT ('&&CRLIMA.LISNOEU','V V K8',NBNOLI,JLIST)
C
C ---   AFFECTATION DU TABLEAU DES NOMS DE NOEUDS :
C       -----------------------------------------
        INDNOE = 0
        CALL GETVID (MOTFAC,MOGRNO,IOCC,1,0,K8BID,NG)
        IF (NG.NE.0) THEN
          NG = -NG
          CALL GETVID (MOTFAC,MOGRNO,IOCC,1,NG,ZK8(JJJ),NGR)
          DO 20 IGR = 1, NGR
               CALL JEVEUO (JEXNOM(GRNOMA,ZK8(JJJ+IGR-1)),'L',JGRO)
               CALL JELIRA (JEXNOM(GRNOMA,ZK8(JJJ+IGR-1)),'LONMAX',
     +                      N1,K1BID)
               DO 30 INO = 1, N1
                  IN = ZI(JGRO+INO-1)
                  INDNOE = INDNOE + 1
                  CALL JENUNO(JEXNUM(NOEUMA,IN),NOMNOE)
                  ZK8(JLIST+INDNOE-1) = NOMNOE
 30           CONTINUE
 20       CONTINUE
        ENDIF
C
        CALL GETVID (MOTFAC,MONOEU,IOCC,1,0,K8BID,NBNOE)
        IF (NBNOE.NE.0) THEN
          NBNOE = -NBNOE
          CALL GETVID (MOTFAC,MONOEU,IOCC,1,NBNOE,ZK8(JJJ),NNO)
          DO 40 INO = 1, NNO
                  INDNOE = INDNOE + 1
                  ZK8(JLIST+INDNOE-1) = ZK8(JJJ+INO-1)
 40       CONTINUE
        ENDIF
C
C ---   ELIMINATION DES REDONDANCES EVENTUELLES DES NOEUDS
C ---   DE LA LISTE :
C       -----------
        CALL WKVECT ('&&CRLIMA.INDICE','V V I',NBNOLI,JIND)
C
        DO 50 INO = 1, NBNOLI
          DO 60 IN1 = INO+1, NBNOLI
                IF (ZK8(JLIST+IN1-1).EQ.ZK8(JLIST+INO-1)) THEN
                      ZI(JIND+IN1-1) = 1
                ENDIF
 60       CONTINUE
 50     CONTINUE
C
        INDLIS = 0
        DO 70 INO = 1, NBNOLI
          IF (ZI(JIND+INO-1).EQ.0) THEN
               INDLIS = INDLIS + 1
               ZK8(JLIST+INDLIS-1) = ZK8(JLIST+INO-1)
          ENDIF
 70     CONTINUE
C
        LONLIS = INDLIS
C
C ---   DETERMINATION DU NOMBRE DE MAILLES AUXQUELLES APPARTIENNENT
C ---   LES NOEUDS DE LA LISTE :
C       ----------------------
C
C ---   RECUPERATION DE LA LISTE DES RESU_ELEM DU MATR_ELEM :
C       ---------------------------------------------------
        CALL JEVEUO(MATEL//'.RELR','L',IDLRES)
C
C ---   RECUPERATION DU NOMBRE DE RESU_ELEM DU MATR_ELEM :
C       ------------------------------------------------
        CALL JELIRA(MATEL//'.RELR','LONUTI',NBRESU,K1BID)
C
C ---   BOUCLE SUR LES RESU_ELEM DU MATR_ELEM :
C       -------------------------------------
        DO 80 IRESU = 1, NBRESU
C
C ---     NOM DU RESU_ELEM COURANT :
C         ------------------------
          RESU = ZK24(IDLRES+IRESU-1)
C
          CALL JEEXIN(RESU(1:19)//'.DESC',IER)
          IF (IER.EQ.0) GO TO 80
C
C ---     RECUPERATION DU DESCRIPTEUR DU RESU_ELEM :
C         ----------------------------------------
         CALL JEVEUO(RESU(1:19)//'.DESC','L',IDDESC)
C
C ---     RECUPERATION DU .NOLI DU RESU_ELEM :
C         ----------------------------------
          CALL JEVEUO(RESU(1:19)//'.NOLI','L',IDNOLI)
C
C ---     NOM DU LIGREL AUQUEL EST ASSOCIE LE RESU_ELEM :
C         ---------------------------------------------
          NOLI = ZK24(IDNOLI)
          LIEL = NOLI(1:19)//'.LIEL'
          NEMA = NOLI(1:19)//'.NEMA'
C
C ---     RECUPERATION DU NOMBRE DE GROUPES D'ELEMENTS DU LIGREL :
C         ------------------------------------------------------
          CALL DISMOI('F','NB_GREL',NOLI,'LIGREL',NBGREL,K8BID,IER)
C
C ---     BOUCLE SUR LES GROUPES D'ELEMENTS DU LIGREL :
C         ------------------------------------------
          DO 90 IGR = 1, NBGREL
C
C ---      RECUPERATION DU MODE ASSOCIE AU GROUPE D'ELEMENTS COURANT :
C          ---------------------------------------------------------
            MODE = ZI(IDDESC+2+IGR-1)
C
            IF (MODE.GT.0) THEN
C
C ---       RECUPERATION DU NOMBRE DE CONNECTIVITES DES
C ---       ELEMENTS DU GREL:
C           ----------------
             NNOE = NBNO(MODE)
C
C ---       RECUPERATION DU NOMBRE D'ELEMENTS DU GREL :
C           -----------------------------------------
             CALL JELIRA(JEXNUM(LIEL,IGR),'LONMAX',NBEL1,K1BID)
             NBEL = NBEL1 - 1
C
C ---       RECUPERATION DU GREL :
C           --------------------
             CALL JEVEUO(JEXNUM(LIEL,IGR),'L',IDGREL)
C
C ---       BOUCLE SUR LES ELEMENTS DU GREL :
C           -------------------------------
             DO 100 IEL = 1, NBEL
C
C ---          RECUPERATION DU NUMERO DE LA MAILLE :
C              -----------------------------------
               NUMEL = ZI(IDGREL+IEL-1)
C
C ---          CAS D'UNE MAILLE PHYSIQUE (NUMEL > 0) :
C              -------------------------------------
               IF (NUMEL.GT.0) THEN
C
C ---          NOM DE LA MAILLE :
C              ----------------
                CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',NUMEL),NOMAIL)
C
C ---          RECUPERATION DES CONNECTIVITES DE LA MAILLE :
C              -------------------------------------------
                CALL JEVEUO(JEXNUM(NOMA//'.CONNEX',NUMEL),'L',IDNOEU)
C
C ---          BOUCLE SUR LES CONNECTIVITES DE LA MAILLE :
C              -----------------------------------------
                DO 110 INO = 1, NNOE
C
C ---            NOM DE LA CONNECTIVITE :
C                ---------------------
                   CALL JENUNO(JEXNUM(NOEUMA,ZI(IDNOEU+INO-1)),NOMNOE)
C
C ---            BOUCLE SUR LES NOEUDS DE LA LISTE UTILISATEUR :
C                ---------------------------------------------
                   DO 120 JNO = 1, LONLIS
                     IF (ZK8(JLIST+JNO-1).EQ.NOMNOE) THEN
                         NBMAIL = NBMAIL + 1
                         GOTO 100
                     ENDIF
 120               CONTINUE
 110            CONTINUE
C
C ---           CAS D'UNE MAILLE TARDIVE (NUMEL < 0) :
C               ------------------------------------
               ELSE
C
C ---          RECUPERATION DU DESCRIPTEUR DE LA MAILLE DANS LE .NEMA :
C              ------------------------------------------------------
                IMA = -NUMEL
                CALL JEVEUO(JEXNUM(NEMA,IMA),'L',IDNEMA)
C
C ---         BOUCLE SUR LES CONNECTIVITES  DE LA MAILLE :
C             ------------------------------------------
               DO 130 INO = 1, NNOE
C
C ---          CAS D'UN NOEUD PHYSIQUE :
C              -----------------------
                 IF (ZI(IDNEMA+INO-1).GT.0) THEN
                    NUMNOE = ZI(IDNEMA+INO-1)
C
C ---            NOM DE LA CONNECTIVITE :
C                ---------------------
                    CALL JENUNO(JEXNUM(NOEUMA,NUMNOE),NOMNOE)
C
C ---            BOUCLE SUR LES NOEUDS DE LA LISTE UTILISATEUR :
C                ---------------------------------------------
                    DO 140 JNO = 1, LONLIS
                     IF (ZK8(JLIST+JNO-1).EQ.NOMNOE) THEN
                         NBMAIL = NBMAIL + 1
                         GOTO 100
                     ENDIF
 140               CONTINUE
                 ENDIF
 130            CONTINUE
               ENDIF
 100         CONTINUE
            ENDIF
 90       CONTINUE
 80     CONTINUE
      ENDIF
C
C --- RECUPERATION DES GROUP_MA ET VERIFICATION DE L'APPARTENANCE
C --- DES GROUP_MA AUX GROUP_MA DU MAILLAGE :
C     -------------------------------------
      CALL GETVID (MOTFAC,MOGRMA,IOCC,1,0,K8BID,NG)
      IF (NG.NE.0) THEN
          NG = -NG
          CALL WKVECT ('&&CRLIMA.TRAV3','V V K8',NG,JJJ)
          CALL GETVEM (NOMA,'GROUP_MA',
     .                 MOTFAC,MOGRMA,IOCC,1,NG,ZK8(JJJ),NGR)
          DO 150 IGR = 1, NGR
                CALL JEVEUO (JEXNOM(GRMAMA,ZK8(JJJ+IGR-1)),'L',JGRO)
                CALL JELIRA (JEXNOM(GRMAMA,ZK8(JJJ+IGR-1)),'LONMAX',
     +                      NMAIL,K1BID)
                  IDIM3 = IDIM3 + NMAIL
 150      CONTINUE
      ENDIF
C
C --- RECUPERATION DES MAILLES ET VERIFICATION DE L'APPARTENANCE
C --- DES MAILLES AUX MAILLES DU MAILLAGE :
C     -----------------------------------
      CALL GETVID (MOTFAC,MOMAIL,IOCC,1,0,K8BID,NBMA)
      IF (NBMA.NE.0) THEN
          NBMA = -NBMA
          CALL WKVECT ('&&CRLIMA.TRAV4','V V K8',NBMA,JJJ)
          CALL GETVEM (NOMA,'MAILLE',
     .                 MOTFAC,MOMAIL,IOCC,1,NBMA,ZK8(JJJ),NMAI)
          IDIM4 = NBMA
      ENDIF
C
C --- NOMBRE TOTAL D'ELEMENTS A PRENDRE EN CONSIDERATION :
C     --------------------------------------------------
      NBELEM = IDIM3 + IDIM4 + NBMAIL
C
      IF (NBELEM.EQ.0) GOTO 9999
C
C --- CREATION DU TABLEAU DU NOM DES ELEMENTS :
C     ---------------------------------------
      CALL WKVECT (LISMAI,'V V K8',NBELEM,IDNOEL)
C
      NUMLAG = 0
      NBMAIL = 0
C
C --- CONSTITUTION DE LA LISTE DES ELEMENTS :
C     -------------------------------------
      IF (NBNOLI.NE.0) THEN
C
C ---   RECUPERATION DE LA LISTE DES RESU_ELEM DU MATR_ELEM :
C       ---------------------------------------------------
        CALL JEVEUO(MATEL//'.RELR','L',IDLRES)
C
C ---   RECUPERATION DU NOMBRE DE RESU_ELEM DU MATR_ELEM :
C       ------------------------------------------------
        CALL JELIRA(MATEL//'.RELR','LONUTI',NBRESU,K1BID)
C
C ---   BOUCLE SUR LES RESU_ELEM DU MATR_ELEM :
C       -------------------------------------
        DO 160 IRESU = 1, NBRESU
C
C ---     NOM DU RESU_ELEM COURANT :
C         ------------------------
          RESU = ZK24(IDLRES+IRESU-1)
C
          CALL JEEXIN(RESU(1:19)//'.DESC',IER)
          IF (IER.EQ.0) GO TO 160
C
C ---     RECUPERATION DU DESCRIPTEUR DU RESU_ELEM :
C         ----------------------------------------
         CALL JEVEUO(RESU(1:19)//'.DESC','L',IDDESC)
C
C ---     RECUPERATION DU .NOLI DU RESU_ELEM :
C         ----------------------------------
          CALL JEVEUO(RESU(1:19)//'.NOLI','L',IDNOLI)
C
C ---     NOM DU LIGREL AUQUEL EST ASSOCIE LE RESU_ELEM :
C         ---------------------------------------------
          CALL JEVEUO(RESU(1:19)//'.NOLI','L',IDNOLI)
          NOLI = ZK24(IDNOLI)
          LIEL = NOLI(1:19)//'.LIEL'
          NEMA = NOLI(1:19)//'.NEMA'
C
C ---     RECUPERATION DU NOMBRE DE GROUPES D'ELEMENTS DU LIGREL :
C         ------------------------------------------------------
          CALL DISMOI('F','NB_GREL',NOLI,'LIGREL',NBGREL,K8BID,IER)
C
C ---     BOUCLE SUR LES GROUPES D'ELEMENTS DU LIGREL :
C         ------------------------------------------
          DO 170 IGR = 1, NBGREL
C
C ---      RECUPERATION DU MODE ASSOCIE AU GROUPE D'ELEMENTS COURANT :
C          ---------------------------------------------------------
            MODE = ZI(IDDESC+2+IGR-1)
C
            IF (MODE.GT.0) THEN
C
C ---       RECUPERATION DU NOMBRE DE CONNECTIVITES DES
C ---       ELEMENTS DU GREL:
C           ----------------
             NNOE = NBNO(MODE)
C
C ---       RECUPERATION DU NOMBRE D'ELEMENTS DU GREL :
C           -----------------------------------------
             CALL JELIRA(JEXNUM(LIEL,IGR),'LONMAX',NBEL1,K1BID)
             NBEL = NBEL1 - 1
C
C ---       RECUPERATION DU GREL :
C           --------------------
             CALL JEVEUO(JEXNUM(LIEL,IGR),'L',IDGREL)
C
C ---       BOUCLE SUR LES ELEMENTS DU GREL :
C           -------------------------------
             DO 180 IEL = 1, NBEL
C
C ---          RECUPERATION DU NUMERO DE LA MAILLE :
C              -----------------------------------
               NUMEL = ZI(IDGREL+IEL-1)
C
C ---          CAS D'UNE MAILLE PHYSIQUE (NUMEL > 0) :
C              -------------------------------------
               IF (NUMEL.GT.0) THEN
C
C ---          NOM DE LA MAILLE :
C              ----------------
                CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',NUMEL),NOMAIL)
C
C ---          RECUPERATION DES CONNECTIVITES DE LA MAILLE :
C              -------------------------------------------
                CALL JEVEUO(JEXNUM(NOMA//'.CONNEX',NUMEL),'L',IDNOEU)
C
C ---          BOUCLE SUR LES CONNECTIVITES DE LA MAILLE :
C              -----------------------------------------
                DO 190 INO = 1, NNOE
C
C ---            NOM DE LA CONNECTIVITE :
C                ---------------------
                   CALL JENUNO(JEXNUM(NOEUMA,ZI(IDNOEU+INO-1)),NOMNOE)
C
C ---            BOUCLE SUR LES NOEUDS DE LA LISTE UTILISATEUR :
C                ---------------------------------------------
                   DO 200 JNO = 1, LONLIS
                     IF (ZK8(JLIST+JNO-1).EQ.NOMNOE) THEN
                         NBMAIL = NBMAIL + 1
                         ZK8(IDNOEL+NBMAIL-1) = NOMAIL
                         GOTO 180
                     ENDIF
 200               CONTINUE
 190            CONTINUE
C
C ---           CAS D'UNE MAILLE TARDIVE (NUMEL < 0) :
C               ------------------------------------
               ELSE
C
                NUMLAG = NUMLAG - 1
C
C ---          CODAGE DU NUMERO DE LA MAILLE SOUS FORME D'UNE CHAINE
C ---          DE CARACTERES :
C              -------------
               CALL CODENT(NUMLAG,'G',NOMAIL)
C
C ---          RECUPERATION DU DESCRIPTEUR DE LA MAILLE DANS LE .NEMA :
C              ------------------------------------------------------
                IMA = -NUMEL
                CALL JEVEUO(JEXNUM(NEMA,IMA),'L',IDNEMA)
C
C ---         BOUCLE SUR LES CONNECTIVITES  DE LA MAILLE :
C             ------------------------------------------
               DO 210 INO = 1, NNOE
C
C ---          CAS D'UN NOEUD PHYSIQUE :
C              -----------------------
                 IF (ZI(IDNEMA+INO-1).GT.0) THEN
                    NUMNOE = ZI(IDNEMA+INO-1)
C
C ---            NOM DE LA CONNECTIVITE :
C                ---------------------
                    CALL JENUNO(JEXNUM(NOEUMA,NUMNOE),NOMNOE)
C
C ---            BOUCLE SUR LES NOEUDS DE LA LISTE UTILISATEUR :
C                ---------------------------------------------
                    DO 220 JNO = 1, LONLIS
                     IF (ZK8(JLIST+JNO-1).EQ.NOMNOE) THEN
                         NBMAIL = NBMAIL + 1
                         ZK8(IDNOEL+NBMAIL-1) = NOMAIL
                         GOTO 180
                     ENDIF
 220               CONTINUE
                 ENDIF
 210            CONTINUE
               ENDIF
 180         CONTINUE
            ENDIF
 170       CONTINUE
 160     CONTINUE
      ENDIF
C
C --- RECUPERATION DES NOMS DES MAILLES DES GROUP_MA :
C     ----------------------------------------------
      CALL GETVID (MOTFAC,MOGRMA,IOCC,1,0,K8BID,NG)
      IF (NG.NE.0) THEN
          NG = -NG
          CALL GETVID (MOTFAC,MOGRMA,IOCC,1,NG,ZK8(JJJ),NGR)
          DO 230 IGR = 1, NGR
               CALL JEVEUO (JEXNOM(GRMAMA,ZK8(JJJ+IGR-1)),'L',JGRO)
               CALL JELIRA (JEXNOM(GRMAMA,ZK8(JJJ+IGR-1)),'LONMAX',
     +                      NBMAIL,K1BID)
               DO 240 M = 1, NBMAIL
                  NUMAIL = ZI(JGRO-1+M)
                  CALL JENUNO(JEXNUM(MAILMA,NUMAIL),NOMAIL)
                  NBMAIL = NBMAIL + 1
                  ZK8(IDNOEL+NBMAIL-1) = NOMAIL
 240           CONTINUE
 230       CONTINUE
      ENDIF
C
C --- RECUPERATION DES NOMS DES MAILLES DE LA LISTE DE MAILLES :
C     -------------------------------------------------------
      CALL GETVID (MOTFAC,MOMAIL,IOCC,1,0,K8BID,NBMA)
      IF (NBMA.NE.0) THEN
          NBMA = -NBMA
          CALL GETVID (MOTFAC,MOMAIL,IOCC,1,NBMA,ZK8(JJJ),NMAI)
          DO 250 IMA = 1, NMAI
            NBMAIL = NBMAIL + 1
            ZK8(IDNOEL+NBMAIL-1) = ZK8(JJJ+IMA-1)
 250      CONTINUE
      ENDIF
C
C --- ELIMINATION DES REDONDANCES EVENTUELLES DES ELEMENTS DE LA LISTE :
C     ----------------------------------------------------------------
      CALL WKVECT ('&&CRLIMA.INDELEM','V V I',NBELEM,JIND)
C
      DO 260 IEL = 1, NBELEM
          DO 270 IEL1 = IEL+1,NBELEM
                IF (ZK8(IDNOEL+IEL1-1).EQ.ZK8(IDNOEL+IEL-1)) THEN
                      ZI(JIND+IEL1-1) = 1
                ENDIF
 270      CONTINUE
 260   CONTINUE
C
      INDLIS = 0
      DO 280 IEL = 1, NBELEM
         IF (ZI(JIND+IEL-1).EQ.0) THEN
              INDLIS = INDLIS + 1
              ZK8(IDNOEL+INDLIS-1) = ZK8(IDNOEL+IEL-1)
         ENDIF
 280  CONTINUE
C
      LONLIS = INDLIS
C
      CALL JEDETR ('&&CRLIMA.TRAV1')
      CALL JEDETR ('&&CRLIMA.TRAV2')
      CALL JEDETR ('&&CRLIMA.TRAV3')
      CALL JEDETR ('&&CRLIMA.TRAV4')
      CALL JEDETR ('&&CRLIMA.INDELEM')
      CALL JEDETR ('&&CRLIMA.LISNOEU')
      CALL JEDETR ('&&CRLIMA.INDICE')
C
 9999 CONTINUE
      CALL JEDEMA()
      END

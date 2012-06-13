      SUBROUTINE CREACO(NBMATO,MA,BORD,NBBORD,NBLIEN,NBMABO)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C TOLE CRP_4
C TOLE CRP_20
C-----------------------------------------------------------------------
C    - FONCTION REALISEE:
C       - CREATION DE LA CONNECTIVITE DES MAILLES
C
C    - IN :     NBMATO : NOMBRE DE MAILLES
C               RENUM  : RENUMEROTATION
C               MA     : NOM DU MAILLAGE
C               NUMSDM : SOUS DOMAINES DE CHAQUES MAILLE
C
C    - OUT :    RENUM2 : RENUMEROTATION
C               RENUM2 : RENUMEROTATION INVERSE DE RENUM2
C               CO     : CONNECTIVITE DES MAILLES
C               IDCO   : INDEX DE CO
C               BORD   : TRAITES T ON LES BORDS ?
C               NBBORD : NOMBRE DE BORDS
C               MABORD : SI MABORD(I) != 0 CEST UN BORD SINON NON
C               NBLIEN : NOMBRE DE LIEN
C               NBMAMA : NOMBRE DE MAILLES RELIEES A CHAQUE MAILLE
C               NBMABO : NOMBRE DE MAILLES A PARTITONNER
C
C----------------------------------------------------------------------
C RESPONSABLE ASSIRE A.ASSIRE

C CORPS DU PROGRAMME
      IMPLICIT NONE


C DECLARATION VARIABLES D'APPEL
      INCLUDE 'jeveux.h'
      INTEGER       NBMATO,RENUM,RENUM2,RENUM3,CO,IDCO,NBMAMA,
     &              NBBORD,MABORD,NBLIEN,NBMABO
      CHARACTER*8   MA,BORD

C DECLARATION VARIABLES LOCALES
      INTEGER       NBMANO,IDCOI,ID1,TYPMA,IDNO,NBNO,IDNOEU,NBNOEU,IMA,
     &              I,J,MAIL,INO,ID,NBRE,ID2,ERR,TEMP,TEMP1,MAXI,
     &              IFM,NIV,COI,NUMNO,NUTYMA,NBNOTO
      REAL*8        TMPS(6)
      CHARACTER*8   NOM,K8BID,TYPMA1,TYPMA2
      INTEGER      IARG

C CORPS DU PROGRAMME
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)

      IF (NIV.GE.2) THEN
        CALL UTTCPU('CPU.CREACO','INIT',' ')
        CALL UTTCPU('CPU.CREACO','DEBUT',' ')
      ENDIF

      CALL JEVEUO ('&&FETSKP.RENUM','L',RENUM)

      CALL DISMOI('F','NB_NO_MAILLA',MA,'MAILLAGE',NBNOTO,K8BID,ERR)
      IF (ERR .NE. 0 ) THEN
        CALL U2MESS('F','UTILITAI_44')
      ENDIF

      WRITE(IFM,*)' -- NOMBRE DE MAILLES : ',NBMATO
      WRITE(IFM,*)' -- NOMBRE DE NOEUDS  : ',NBNOTO
      WRITE(IFM,*)' '

      CALL WKVECT('&&FETSKP.NBMANO','V V I',NBNOTO,NBMANO)
      CALL WKVECT('&&FETSKP.IDCOI','V V I',NBNOTO+1,IDCOI)
      CALL WKVECT('&&FETSKP.ID1','V V I',NBNOTO,ID1)
      CALL WKVECT('&&FETSKP.TYPMA','V V K8',NBMATO,TYPMA)
      CALL WKVECT('&&FETSKP.IDNO','V V I',NBMATO,IDNO)
      CALL WKVECT('&&FETSKP.NBNO','V V I',NBMATO,NBNO)

C ------- ON RECUPERE LE NOMBRE DE MAILLES RELIEES A CHAQUE NOEUD ----
C ------- CREATION DU TABLEAU DES TYPES DE MAILLES -------------------
C ------- REMPLISSAGE DES TABLEAUX NBNO ET IDNOEU --------------------
C ------- LINEARISATION DES ELEMENTS ---------------------------------

      CALL JEVEUO (MA//'.TYPMAIL','L',NUTYMA)

      DO 1 IMA=1,NBMATO
        MAIL=ZI(RENUM-1+IMA)
        NBRE=ZI(NUTYMA-1+MAIL)
        CALL JENUNO (JEXNUM('&CATA.TM.NOMTM',NBRE),NOM)
        CALL JEVEUO (JEXNUM(MA//'.CONNEX',MAIL),'L',IDNOEU)
        CALL JELIRA (JEXNUM(MA//'.CONNEX',MAIL),'LONMAX',NBNOEU,K8BID)
        ZI(IDNO-1+IMA)=IDNOEU

C      ------- ON LINEARISE LES ELEMENTS -------
        IF ( NOM .EQ. 'SEG3    ') THEN
          ZK8(TYPMA-1+IMA)= 'SEG2    '
          ZI(NBNO-1+IMA)=2
        ELSEIF ( NOM .EQ. 'TRIA6   ') THEN
          ZK8(TYPMA-1+IMA)= 'TRIA3   '
          ZI(NBNO-1+IMA)=3
        ELSEIF ( NOM .EQ. 'QUAD8   ') THEN
          ZK8(TYPMA-1+IMA)= 'QUAD4   '
          ZI(NBNO-1+IMA)=4
        ELSEIF ( NOM .EQ. 'QUAD9   ') THEN
          ZK8(TYPMA-1+IMA)= 'QUAD4   '
          ZI(NBNO-1+IMA)=4
        ELSEIF ( NOM .EQ. 'TETRA10 ') THEN
          ZK8(TYPMA-1+IMA)= 'TETRA4  '
          ZI(NBNO-1+IMA)=4
        ELSEIF ( NOM .EQ. 'PENTA15 ') THEN
          ZK8(TYPMA-1+IMA)= 'PENTA6  '
          ZI(NBNO-1+IMA)=6
        ELSEIF ( NOM .EQ. 'PENTA18 ') THEN
          ZK8(TYPMA-1+IMA)= 'PENTA6  '
          ZI(NBNO-1+IMA)=6
        ELSEIF ( NOM .EQ. 'HEXA20  ') THEN
          ZK8(TYPMA-1+IMA)= 'HEXA8   '
          ZI(NBNO-1+IMA)=8
        ELSEIF ( NOM .EQ. 'HEXA27  ') THEN
          ZK8(TYPMA-1+IMA)= 'HEXA8   '
          ZI(NBNO-1+IMA)=8
        ELSEIF ( NOM .EQ. 'PYRAM13 ') THEN
          ZK8(TYPMA-1+IMA)= 'PYRAM5  '
          ZI(NBNO-1+IMA)=5
        ELSE
          ZK8(TYPMA-1+IMA)= NOM
          ZI(NBNO-1+IMA)=NBNOEU
        ENDIF
C      ------- FIN DE LA LINEARISATION -------

        DO 2 INO=1,ZI(NBNO-1+IMA)
          NUMNO = ZI(IDNOEU-1+INO)
          ZI(NBMANO-1+NUMNO)=ZI(NBMANO-1+NUMNO)+1
  2     CONTINUE
  1   CONTINUE

C ------- ON CREE LE TABLEAU D'INDEX POUR COI ------------------------

      ZI(IDCOI)=1
      DO 4 INO=2,NBNOTO+1
        ZI(IDCOI-1+INO)=ZI(IDCOI-1+INO-1)+ZI(NBMANO-1+INO-1)
  4   CONTINUE

C ------- ON CREE LE TABLEAU DE CONNECTIVITE INVERSE ( COI ) ---------

      CALL WKVECT('&&FETSKP.COI','V V I',ZI(IDCOI-1+NBNOTO+1)-1,COI)

      DO 5 IMA=1,NBMATO
        IDNOEU=ZI(IDNO-1+IMA)
        DO 6 INO=1,ZI(NBNO-1+IMA)
          NUMNO=ZI(IDNOEU-1+INO)
          ID=ZI(IDCOI-1+NUMNO)+ZI(ID1-1+NUMNO)
          ZI(COI-1+ID)=IMA
          ZI(ID1-1+NUMNO)=ZI(ID1-1+NUMNO)+1
  6     CONTINUE
  5   CONTINUE

      CALL JEDETR('&&FETSKP.ID1')

C ------- ON REMPLIT LE TABLEAU NOMBRE DE MAILLES PAR MAILLE (NBMAMA)

      CALL WKVECT('&&FETSKP.NBMAMA','V V I',NBMATO,NBMAMA)

      DO 8 INO=1,NBNOTO
        IF (ZI(NBMANO-1+INO).GT.1) THEN
          DO 9 I=ZI(IDCOI-1+INO),ZI(IDCOI-1+INO+1)-1
            NBRE=ZI(IDCOI-1+INO+1)-1-ZI(IDCOI-1+INO)
            MAIL=ZI(COI-1+I)
            ZI(NBMAMA-1+MAIL)=ZI(NBMAMA-1+MAIL)+NBRE
  9       CONTINUE
        ENDIF
  8   CONTINUE

      CALL JEDETR('&&FETSKP.NBMANO')

C ------- ON REMPLIT L'INDEX DU TABLEAU DES CONNECTIVITES (CO) -------

      CALL WKVECT('&&FETSKP.IDCO','V V S',NBMATO+1,IDCO)

      MAXI=0
      ZI4(IDCO)=1
      DO 10 IMA=2,NBMATO+1
        IF (ZI(NBMAMA-1+IMA-1) .GT. MAXI) MAXI=ZI(NBMAMA-1+IMA-1)
        ZI4(IDCO-1+IMA)=ZI4(IDCO-1+IMA-1)+ZI(NBMAMA-1+IMA-1)
 10   CONTINUE

C ------------------------ JEVEUX ------------------------------------

      CALL JEDETR('&&FETSKP.NBMAMA')
      CALL WKVECT('&&FETSKP.TEMP','V V I',MAXI,TEMP)
      CALL WKVECT('&&FETSKP.TEMP1','V V I',MAXI,TEMP1)
      CALL WKVECT('&&FETSKP.MABORD','V V I',NBMATO,MABORD)

C ------- ENLEVE T ON LES MAILLES DE BORDS ? -------------------------

      CALL GETVTX(' ','TRAITER_BORDS',0,IARG,1,BORD,ERR)

C ------- ON CHERCHE LES MAILLES DE BORDS ----------------------------

      NBBORD=0
      DO 11 IMA=1,NBMATO
        ID=0
        MAXI=0
        IDNOEU=ZI(IDNO-1+IMA)
        DO 12 INO=1,ZI(NBNO-1+IMA)
          NUMNO=ZI(IDNOEU-1+INO)
          DO 13 I=ZI(IDCOI-1+NUMNO),ZI(IDCOI-1+NUMNO+1)-1
            MAIL=ZI(COI-1+I)
            IF (MAIL .EQ. IMA) GOTO 20
            DO 14 J=1,ID
              IF (ZI(TEMP-1+J) .EQ. MAIL ) THEN
                ZI(TEMP1-1+J)=ZI(TEMP1-1+J)+1
                GOTO 20
              ENDIF
 14         CONTINUE
            ZI(TEMP+ID)=MAIL
            ZI(TEMP1+ID)=1
            ID=ID+1
 20         CONTINUE
 13       CONTINUE
 12     CONTINUE
        DO 15 J=1,ID
          IF (ZI(NBNO-1+IMA) .EQ. ZI(TEMP1-1+J)) THEN
            IF (ZI(MABORD-1+IMA) .EQ. 0) THEN
              NBBORD=NBBORD+1
              MAXI=ZI(TEMP1-1+J)
            ENDIF
            IF (ZI(TEMP1-1+J) .GE. MAXI ) THEN
              ZI(MABORD-1+IMA) = ZI(TEMP-1+J)
            ENDIF
          ENDIF
 15     CONTINUE
 11   CONTINUE

C ------ ON ENLEVE LES MAILLES DE BORDS ------------------------------

      CALL WKVECT('&&FETSKP.RENUM2','V V I',NBMATO,RENUM2)
      CALL WKVECT('&&FETSKP.RENUM3','V V I',NBMATO,RENUM3)
      NBMABO=NBMATO
      IF (BORD .EQ. 'OUI     ') THEN
        NBMATO=NBMABO-NBBORD
        ID=1
        ID2=NBMATO+1
        DO 49 IMA=1,NBMABO
          IF ( ZI(MABORD-1+IMA) .NE. 0 ) THEN
            ZI(RENUM2-1+ID2)=IMA
            ZI(RENUM3-1+IMA)=ID2
            ID2=ID2+1
          ELSE
            ZI(RENUM2-1+ID)=IMA
            ZI(RENUM3-1+IMA)=ID
            ID=ID+1
          ENDIF
 49     CONTINUE
      ELSE
        DO 52 IMA=1,NBMATO
          ZI(RENUM2-1+IMA)=IMA
          ZI(RENUM3-1+IMA)=IMA
 52     CONTINUE
      ENDIF

C ------------------------ JEVEUX ------------------------------------

      CALL WKVECT('&&FETSKP.NBMAMA','V V I',NBMATO,NBMAMA)
      CALL WKVECT('&&FETSKP.ID1','V V I',NBMATO,ID1)

C ------ ON COMPTE LES LIENS -----------------------------------------

      NBLIEN=0
      DO 17 IMA=1,NBMATO
        ID=0
        IDNOEU=ZI(IDNO-1+ZI(RENUM2-1+IMA))
        TYPMA1=ZK8(TYPMA-1+ZI(RENUM2-1+IMA))
        DO 19 INO=1,ZI(NBNO-1+ZI(RENUM2-1+IMA))
          NUMNO=ZI(IDNOEU-1+INO)
          DO 21 I=ZI(IDCOI-1+NUMNO),ZI(IDCOI-1+NUMNO+1)-1
            MAIL=ZI(COI-1+I)
            IF ( BORD .EQ. 'OUI     ' ) THEN
              IF ( ZI(MABORD-1+MAIL) .NE. 0) GOTO 23
            ENDIF
            MAIL=ZI(RENUM3-1+MAIL)
            IF (MAIL .LE. IMA) GOTO 23
            DO 22 J=1,ID
              IF (ZI(TEMP-1+J) .EQ. MAIL ) THEN
                ZI(TEMP1-1+J)=ZI(TEMP1-1+J)+1
                GOTO 23
              ENDIF
 22         CONTINUE
            ZI(TEMP+ID)=MAIL
            ZI(TEMP1+ID)=1
            ID=ID+1
 23         CONTINUE
 21       CONTINUE
 19     CONTINUE

        DO 24 J=1,ID
          TYPMA2=ZK8(TYPMA-1+ZI(RENUM2-1+ZI(TEMP-1+J)))
          IF ( ZI(TEMP1-1+J) .EQ. 1 ) THEN
            IF ( TYPMA1 .EQ. 'POI1    ') GOTO 61
            IF ( TYPMA2 .EQ. 'POI1    ') GOTO 61
            IF ( ZI(MABORD+ZI(RENUM2-1+IMA)-1) .EQ. 0) THEN
              IF ( TYPMA1 .EQ. 'SEG2    ') GOTO 61
            ENDIF
            IF ( ZI(MABORD+ZI(RENUM2-1+ZI(TEMP-1+J))-1) .EQ. 0) THEN
              IF ( TYPMA2 .EQ. 'SEG2    ') GOTO 61
            ENDIF
          ELSEIF ( ZI(TEMP1-1+J) .EQ. 2 ) THEN
            IF ( TYPMA1 .EQ. 'SEG2    ') GOTO 61
            IF ( TYPMA2 .EQ. 'SEG2    ') GOTO 61
            IF ( ZI(MABORD+ZI(RENUM2-1+IMA)-1) .EQ. 0) THEN
              IF ( TYPMA1 .EQ. 'TRIA3   ') GOTO 61
              IF ( TYPMA1 .EQ. 'QUAD4   ') GOTO 61
            ENDIF
            IF ( ZI(MABORD+ZI(RENUM2-1+ZI(TEMP-1+J))-1) .EQ. 0) THEN
              IF ( TYPMA2 .EQ. 'TRIA3   ') GOTO 61
              IF ( TYPMA2 .EQ. 'QUAD4   ') GOTO 61
            ENDIF
          ELSEIF ( ZI(TEMP1-1+J) .EQ. 3 ) THEN
            IF ( TYPMA1 .EQ. 'TRIA3   ') THEN
              IF ( TYPMA2 .EQ. 'TETRA4  ') GOTO 61
              IF ( TYPMA2 .EQ. 'PENTA6  ') GOTO 61
              IF ( TYPMA2 .EQ. 'PYRAM5  ') GOTO 61
            ELSEIF ( TYPMA1 .EQ. 'TETRA4  ') THEN
              IF ( TYPMA2 .EQ. 'TRIA3   ') GOTO 61
              IF ( TYPMA2 .EQ. 'TETRA4  ') GOTO 61
              IF ( TYPMA2 .EQ. 'PENTA6  ') GOTO 61
              IF ( TYPMA2 .EQ. 'PYRAM5  ') GOTO 61
            ELSEIF ( TYPMA1 .EQ. 'PENTA6  ') THEN
              IF ( TYPMA2 .EQ. 'PENTA6  ') GOTO 61
              IF ( TYPMA2 .EQ. 'TRIA3   ') GOTO 61
              IF ( TYPMA2 .EQ. 'TETRA4  ') GOTO 61
              IF ( TYPMA2 .EQ. 'PYRAM5  ') GOTO 61
            ELSEIF ( TYPMA1 .EQ. 'PYRAM5  ') THEN
              IF ( TYPMA2 .EQ. 'TRIA3   ') GOTO 61
              IF ( TYPMA2 .EQ. 'TETRA4  ') GOTO 61
              IF ( TYPMA2 .EQ. 'PENTA6  ') GOTO 61
              IF ( TYPMA2 .EQ. 'PYRAM5  ') GOTO 61
           ENDIF
          ELSEIF ( ZI(TEMP1-1+J) .EQ. 4 ) THEN
            IF ( TYPMA1 .EQ. 'QUAD4   ') THEN
              IF ( TYPMA2 .EQ. 'PENTA6  ') GOTO 61
              IF ( TYPMA2 .EQ. 'HEXA8   ') GOTO 61
              IF ( TYPMA2 .EQ. 'PYRAM5  ') GOTO 61
            ELSEIF ( TYPMA1 .EQ. 'PENTA6  ') THEN
              IF ( TYPMA2 .EQ. 'PENTA6  ') GOTO 61
              IF ( TYPMA2 .EQ. 'PYRAM5  ') GOTO 61
              IF ( TYPMA2 .EQ. 'HEXA8   ') GOTO 61
            ELSEIF ( TYPMA1 .EQ. 'HEXA8   ') THEN
              IF ( TYPMA2 .EQ. 'HEXA8   ') GOTO 61
              IF ( TYPMA2 .EQ. 'QUAD4   ') GOTO 61
              IF ( TYPMA2 .EQ. 'PYRAM5  ') GOTO 61
              IF ( TYPMA2 .EQ. 'PENTA6  ') GOTO 61
            ELSEIF ( TYPMA1 .EQ. 'PYRAM5  ') THEN
              IF ( TYPMA2 .EQ. 'QUAD4   ') GOTO 61
              IF ( TYPMA2 .EQ. 'PENTA6  ') GOTO 61
              IF ( TYPMA2 .EQ. 'HEXA8   ') GOTO 61
              IF ( TYPMA2 .EQ. 'PYRAM5  ') GOTO 61
            ENDIF
          ENDIF

          GOTO 24

 61       CONTINUE
          ZI(NBMAMA-1+IMA)=ZI(NBMAMA-1+IMA)+1
          ZI(NBMAMA-1+ZI(TEMP-1+J))=ZI(NBMAMA-1+ZI(TEMP-1+J))+1
          NBLIEN=NBLIEN+2

 24     CONTINUE
 17   CONTINUE

C ------- ON RE-REMPLIT IDCO -----------------------------------------

      ZI4(IDCO)=1
      DO 99 IMA=2,NBMATO+1
        ZI4(IDCO-1+IMA)=ZI4(IDCO-1+IMA-1)+ZI(NBMAMA-1+IMA-1)
 99   CONTINUE

C ------ CREATION DES CONNECTIVITES DES MAILLES ( CO ) ---------------

      CALL WKVECT('&&FETSKP.CO','V V S',NBLIEN,CO)

      DO 25 IMA=1,NBMATO
        NBRE=0
        IDNOEU=ZI(IDNO-1+ZI(RENUM2-1+IMA))
        TYPMA1=ZK8(TYPMA-1+ZI(RENUM2-1+IMA))
        DO 26 INO=1,ZI(NBNO-1+ZI(RENUM2-1+IMA))
          NUMNO=ZI(IDNOEU-1+INO)
          DO 27 I=ZI(IDCOI-1+NUMNO),ZI(IDCOI-1+NUMNO+1)-1
            MAIL=ZI(COI-1+I)
            IF ( BORD .EQ. 'OUI     ' ) THEN
              IF ( ZI(MABORD-1+MAIL) .NE. 0 ) GOTO 29
            ENDIF
            MAIL=ZI(RENUM3-1+MAIL)
            IF (MAIL .LE. IMA) GOTO 29
            DO 28 J=1,NBRE
              IF (ZI(TEMP-1+J) .EQ. MAIL ) THEN
                ZI(TEMP1-1+J)=ZI(TEMP1-1+J)+1
                GOTO 29
              ENDIF
 28         CONTINUE
            ZI(TEMP+NBRE)=MAIL
            ZI(TEMP1+NBRE)=1
            NBRE=NBRE+1
 29         CONTINUE
 27       CONTINUE
 26     CONTINUE

        DO 30 J=1,NBRE
          TYPMA2=ZK8(TYPMA-1+ZI(RENUM2-1+ZI(TEMP-1+J)))
          IF ( ZI(TEMP1-1+J) .EQ. 1 ) THEN
            IF ( TYPMA1 .EQ. 'POI1    ') GOTO 31
            IF ( TYPMA2 .EQ. 'POI1    ') GOTO 31
            IF ( ZI(MABORD+ZI(RENUM2-1+IMA)-1) .EQ. 0) THEN
              IF ( TYPMA1 .EQ. 'SEG2    ') GOTO 31
            ENDIF
            IF ( ZI(MABORD+ZI(RENUM2-1+ZI(TEMP-1+J))-1) .EQ. 0) THEN
              IF ( TYPMA2 .EQ. 'SEG2    ') GOTO 31
            ENDIF
          ELSEIF ( ZI(TEMP1-1+J) .EQ. 2 ) THEN
            IF ( TYPMA1 .EQ. 'SEG2    ') GOTO 31
            IF ( TYPMA2 .EQ. 'SEG2    ') GOTO 31
            IF ( ZI(MABORD+ZI(RENUM2-1+IMA)-1) .EQ. 0) THEN
              IF ( TYPMA1 .EQ. 'TRIA3   ') GOTO 31
              IF ( TYPMA1 .EQ. 'QUAD4   ') GOTO 31
            ENDIF
            IF ( ZI(MABORD+ZI(RENUM2-1+ZI(TEMP-1+J))-1) .EQ. 0) THEN
              IF ( TYPMA2 .EQ. 'TRIA3   ') GOTO 31
              IF ( TYPMA2 .EQ. 'QUAD4   ') GOTO 31
            ENDIF
          ELSEIF ( ZI(TEMP1-1+J) .EQ. 3 ) THEN
            IF ( TYPMA1 .EQ. 'TRIA3   ') THEN
              IF ( TYPMA2 .EQ. 'TETRA4  ') GOTO 31
              IF ( TYPMA2 .EQ. 'PENTA6  ') GOTO 31
              IF ( TYPMA2 .EQ. 'PYRAM5  ') GOTO 31
            ELSEIF ( TYPMA1 .EQ. 'TETRA4  ') THEN
              IF ( TYPMA2 .EQ. 'TETRA4  ') GOTO 31
              IF ( TYPMA2 .EQ. 'PENTA6  ') GOTO 31
              IF ( TYPMA2 .EQ. 'TRIA3   ') GOTO 31
              IF ( TYPMA2 .EQ. 'PYRAM5  ') GOTO 31
            ELSEIF ( TYPMA1 .EQ. 'PENTA6  ') THEN
              IF ( TYPMA2 .EQ. 'PENTA6  ') GOTO 31
              IF ( TYPMA2 .EQ. 'TRIA3   ') GOTO 31
              IF ( TYPMA2 .EQ. 'TETRA4  ') GOTO 31
              IF ( TYPMA2 .EQ. 'PYRAM5  ') GOTO 31
            ELSEIF ( TYPMA1 .EQ. 'PYRAM5  ') THEN
              IF ( TYPMA2 .EQ. 'TRIA3   ') GOTO 31
              IF ( TYPMA2 .EQ. 'TETRA4  ') GOTO 31
              IF ( TYPMA2 .EQ. 'PENTA6  ') GOTO 31
              IF ( TYPMA2 .EQ. 'PYRAM5  ') GOTO 31
            ENDIF
          ELSEIF ( ZI(TEMP1-1+J) .EQ. 4 ) THEN
            IF ( TYPMA1 .EQ. 'QUAD4   ') THEN
              IF ( TYPMA2 .EQ. 'PENTA6  ') GOTO 31
              IF ( TYPMA2 .EQ. 'HEXA8   ') GOTO 31
              IF ( TYPMA2 .EQ. 'PYRAM5  ') GOTO 31
            ELSEIF ( TYPMA1 .EQ. 'PENTA6  ') THEN
              IF ( TYPMA2 .EQ. 'PENTA6  ') GOTO 31
              IF ( TYPMA2 .EQ. 'HEXA8   ') GOTO 31
              IF ( TYPMA2 .EQ. 'PYRAM5  ') GOTO 31
            ELSEIF ( TYPMA1 .EQ. 'HEXA8   ') THEN
              IF ( TYPMA2 .EQ. 'HEXA8   ') GOTO 31
              IF ( TYPMA2 .EQ. 'PENTA6  ') GOTO 31
              IF ( TYPMA2 .EQ. 'QUAD4   ') GOTO 31
              IF ( TYPMA2 .EQ. 'PYRAM5  ') GOTO 31
            ELSEIF ( TYPMA1 .EQ. 'PYRAM5  ') THEN
              IF ( TYPMA2 .EQ. 'QUAD4   ') GOTO 31
              IF ( TYPMA2 .EQ. 'PENTA6  ') GOTO 31
              IF ( TYPMA2 .EQ. 'HEXA8   ') GOTO 31
              IF ( TYPMA2 .EQ. 'PYRAM5  ') GOTO 31
            ENDIF
          ENDIF

          GOTO 30

 31       CONTINUE
          ID=ZI4(IDCO-1+IMA)+ZI(ID1-1+IMA)
          ID2=ZI4(IDCO-1+ZI(TEMP-1+J))+ZI(ID1-1+ZI(TEMP-1+J))
          ZI4(CO-1+ID2)=IMA
          ZI4(CO-1+ID)=ZI(TEMP-1+J)
          ZI(ID1-1+IMA)=ZI(ID1-1+IMA)+1
          ZI(ID1-1+ZI(TEMP-1+J))=ZI(ID1-1+ZI(TEMP-1+J))+1

 30     CONTINUE
 25   CONTINUE

C ------------------------ JEVEUX ------------------------------------
      CALL JEDETR('&&FETSKP.NBNO')
      CALL JEDETR('&&FETSKP.IDNO')
      CALL JEDETR('&&FETSKP.TYPMA')
      CALL JEDETR('&&FETSKP.IDCOI')
      CALL JEDETR('&&FETSKP.ID1')
      CALL JEDETR('&&FETSKP.COI')
      CALL JEDETR('&&FETSKP.TEMP')
      CALL JEDETR('&&FETSKP.TEMP1')

      IF ( NIV .GE. 2 ) THEN
        CALL UTTCPU('CPU.CREACO','FIN',' ')
        CALL UTTCPR('CPU.CREACO',6,TMPS)
        WRITE(IFM,*)'--- CONNECTIVITE DES MAILLES:',TMPS(3)
        WRITE(IFM,*)'  '
      ENDIF

      CALL JEDEMA()
      END

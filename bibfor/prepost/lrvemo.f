      SUBROUTINE LRVEMO(MODELE)
      IMPLICIT NONE
      CHARACTER*8 MODELE
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 29/03/2010   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------
C   BUT:ROUTINE DE LIRE RESU / LIRE_CHAMP QUI VERIFIE LA COHERENCE ENTRE
C       LE MODELE FOURNI ET LES DONNEES DU FICHIER MED
C
C   ENTREE: MODELE(K8)  = NOM DU MODELE
C-----------------------------------------------------------------------
C RESPONSABLE SELLENET N.SELLENET
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      CHARACTER*32 JEXATR
C     -----  FIN  COMMUNS NORMALISES  JEVEUX --------------------------

      INTEGER IBID,N1,MFICH,NDIM,IRET,NMATYP,IALIEL,ILLIEL,JTYPMA
      INTEGER ITE,NBGR,IGR,NBMA,JNBTYP,JMATYP,NBEL,IEL,IAGREL,NBTYM
      INTEGER I,J,JJ,IDFIMD,IAUX,JNBTY2,NBGREL,TYPELE,INDIK8,IMA
      INTEGER KAUX, TYAUX, INDIC,VALI(2), LNOMAM, LXLGUT
      PARAMETER (INDIC=1)
      INTEGER EDLECT
      PARAMETER (EDLECT=0)
      INTEGER NTYMAX
      PARAMETER (NTYMAX = 48)
      INTEGER EDCONN
      PARAMETER (EDCONN=1)
      INTEGER EDMAIL
      PARAMETER (EDMAIL=0)
      INTEGER EDNODA
      PARAMETER (EDNODA=0)

      INTEGER NUMMED(NTYMAX),NBETYP(NTYMAX)
      REAL*8 R8B
      CHARACTER*1 K1B
      CHARACTER*8 SAUX08,NOMAST(NTYMAX),NOMTM,NOMA,K8B
      CHARACTER*8 CHANOM,TYPECH
      CHARACTER*16 TYPRES,PHENO,VALK(2),NOMCMD,TYCH
      CHARACTER*24 LIGRMO
      CHARACTER*32 NOMAMD
      CHARACTER*200 NOFIMD,DAUX
      CHARACTER*255 KFIC
      LOGICAL EXISTM,LFIRST

      DATA NOMAST  /'POI1    ','SEG2    ','SEG22   ','SEG3    ',
     &              'SEG33   ','SEG4    ',
     &                         'TRIA3   ','TRIA33  ','TRIA6   ',
     &              'TRIA66  ','TRIA7   ','QUAD4   ','QUAD44  ',
     &              'QUAD8   ','QUAD88  ','QUAD9   ','QUAD99  ',
     &              'TETRA4  ','TETRA10 ','PENTA6  ','PENTA15 ',
     &              'PYRAM5  ','PYRAM13 ','HEXA8   ','HEXA20  ',
     &              'HEXA27  ','TR3QU4  ','QU4TR3  ','TR6TR3  ',
     &              'TR3TR6  ','TR6QU4  ','QU4TR6  ','TR6QU8  ',
     &              'QU8TR6  ','TR6QU9  ','QU9TR6  ','QU8TR3  ',
     &              'TR3QU8  ','QU8QU4  ','QU4QU8  ','QU8QU9  ',
     &              'QU9QU8  ','QU9QU4  ','QU4QU9  ','QU9TR3  ',
     &              'TR3QU9  ','SEG32   ','SEG23   '/
      DATA NUMMED  /1,         102,       0,         103,
     &              0,         0,
     &                         203,       0,         206,
     &              0,         0,         204,       0,
     &              208,       0,         0,         0,
     &              304,       310,       306,       315,
     &              305,       313,       308,       320,
     &              0,         0,         0,         0,
     &              0,         0,         0,         0,
     &              0,         0,         0,         0,
     &              0,         0,         0,         0,
     &              0,         0,         0,         0,
     &              0,         0,         0/
C-----------------------------------------------------------------------

      CALL JEMARQ()

      CALL GETRES(CHANOM, TYPECH, NOMCMD)

C     ON VERIFIE QUE LE PHENOMENE DU MODELE FOURNI EST COHERENT AVEC
C     LA SD RESULTAT A PRODUIRE
C     =========================
      IF(NOMCMD(1:9).EQ.'LIRE_RESU')THEN
        CALL GETVTX(' ','TYPE_RESU',0,1,1,TYPRES,N1)
        CALL DISMOI('F','PHENOMENE',MODELE,'MODELE',IBID,PHENO,N1)
        VALK(1)=PHENO
        VALK(2)=TYPRES
        IF(TYPRES(1:9).EQ.'EVOL_THER')THEN
          IF(PHENO(1:9).EQ.'MECANIQUE')THEN
            CALL U2MESS('F+','MED_54')
            CALL U2MESK('F','MED_56',2,VALK)
          ENDIF
        ELSE
          IF(PHENO(1:9).EQ.'THERMIQUE')THEN
            CALL U2MESS('F+','MED_54')
            CALL U2MESK('F','MED_56',2,VALK)
          ENDIF
        ENDIF
      ELSEIF(NOMCMD(1:10).EQ.'LIRE_CHAMP')THEN
        CALL DISMOI('F','PHENOMENE',MODELE,'MODELE',IBID,PHENO,N1)
        CALL GETVTX ( ' ', 'TYPE_CHAM', 0, 1, 1, TYCH, N1)
        IF(PHENO(1:9).EQ.'THERMIQUE')THEN
           VALK(1)=PHENO
           VALK(2)=TYCH
           CALL U2MESK('F','MED_64',2,VALK)
        ENDIF
      ENDIF


C     ON VERIFIE QUE LE MAILLAGE DU MODELE FOURNI ET CELUI
C     CONTENU DANS LE FICHIER MED ONT
C     - MEME TYPE DE MAILLE
C     - MEME NOMBRE DE MAILLE PAR TYPE
C     =================================
C
      CALL GETVIS(' ','UNITE',0,1,1,MFICH,IRET)
      CALL ULISOG(MFICH, KFIC, K1B)
      IF ( KFIC(1:1).EQ.' ' ) THEN
        CALL CODENT ( MFICH, 'G', SAUX08 )
        NOFIMD = 'fort.'//SAUX08
      ELSE
        NOFIMD = KFIC(1:200)
      ENDIF

      NOMAMD=' '
      CALL EFOUVR( IDFIMD, NOFIMD, EDLECT, IAUX )
      IF ( IAUX.NE.0 ) THEN
        LNOMAM = LXLGUT(SAUX08)
        CALL U2MESK('F','MED_78',1,SAUX08(1:LNOMAM))
      ENDIF
      CALL EFMAAI( IDFIMD,INDIC, NOMAMD, KAUX, TYAUX, DAUX, IRET )
      CALL WKVECT('&&LRVERIMO_NBETYP1','V V I',NTYMAX,JNBTYP)
      CALL WKVECT('&&LRVERIMO_NBETYP2','V V I',NTYMAX,JNBTY2)
      DO 10 I=1,NTYMAX
        ZI(JNBTYP+I-1)=0
        IF(NUMMED(I).NE.0)THEN
           CALL EFNEMA ( IDFIMD, NOMAMD, EDCONN, EDMAIL, NUMMED(I),
     &                   EDNODA, NMATYP, IRET )
            ZI(JNBTYP+I-1)=NMATYP
        ENDIF
 10   CONTINUE

      CALL EFFERM ( IDFIMD, IRET )
      CALL DISMOI('F','NOM_LIGREL',MODELE,'MODELE',IBID,LIGRMO,IRET)
      CALL DISMOI('C','NOM_MAILLA',MODELE,'MODELE',IBID,NOMA,IRET)
      CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMA,K8B,IRET)
      CALL WKVECT('&&LRVERIMO_NBMA_TYP','V V I',NBMA,JMATYP)
      DO 20 I=1,NBMA
         ZI(JMATYP+I-1)=0
 20   CONTINUE

      CALL JEVEUO(LIGRMO(1:19)//'.LIEL','L',IALIEL)
      CALL JEVEUO(JEXATR(LIGRMO(1:19)//'.LIEL','LONCUM'),'L',ILLIEL)
      NBGR = NBGREL(LIGRMO)
      CALL JEVEUO('&CATA.TE.TYPEMA','L',JTYPMA)
      DO 30 IGR = 1,NBGR
         ITE = TYPELE(LIGRMO,IGR)
         NOMTM=ZK8(JTYPMA+ITE-1)
         J = INDIK8 ( NOMAST, NOMTM, 1, NTYMAX )
         JJ = NUMMED(J)
         NBEL= ZI(ILLIEL-1+IGR+1)-ZI(ILLIEL-1+IGR) -1
         IAGREL= IALIEL + ZI(ILLIEL-1+IGR) -1
         DO 40 IEL = 1,NBEL
            IMA= ZI(IAGREL-1+IEL)
            ZI(JMATYP+IMA-1)=JJ
 40      CONTINUE
 30   CONTINUE

      DO 50 I=1,NTYMAX
        NBTYM=0
        ZI(JNBTY2+I-1)=NBTYM
        IF(NUMMED(I).NE.0)THEN
           DO 60 J=1,NBMA
              IF(ZI(JMATYP+J-1).EQ.NUMMED(I))THEN
                 NBTYM=NBTYM+1
              ENDIF
 60        CONTINUE
        ENDIF
        ZI(JNBTY2+I-1)=NBTYM
 50   CONTINUE

      LFIRST=.TRUE.
      DO 70 I=1,NTYMAX
        IF(NUMMED(I).NE.0)THEN
            IF(ZI(JNBTYP+I-1).NE.ZI(JNBTY2+I-1) .AND. LFIRST) THEN
                LFIRST=.FALSE.
                CALL U2MESS('A+','MED_54')
                IF(ZI(JNBTYP+I-1).LT.ZI(JNBTY2+I-1))THEN
                   VALI(1)=ZI(JNBTYP+I-1)
                   VALI(2)=ZI(JNBTY2+I-1)
                   CALL U2MESG('A','MED_59',1,NOMAST(I),2,VALI,0,R8B)
                ELSE
                   VALI(1)=ZI(JNBTYP+I-1)
                   VALI(2)=ZI(JNBTY2+I-1)
                   CALL U2MESG('A','MED_61',1,NOMAST(I),2,VALI,0,R8B)
                ENDIF

             ENDIF
         ENDIF
 70   CONTINUE

      CALL JEDETR('&&LRVERIMO_NBETYP1')
      CALL JEDETR('&&LRVERIMO_NBETYP2')
      CALL JEDETR('&&LRVERIMO_NBMA_TYP')

      CALL JEDEMA()

      END

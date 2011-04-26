      SUBROUTINE ASENAP ( MASSE )
      IMPLICIT  NONE
      CHARACTER*8       MASSE

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     ------------------------------------------------------------------
C     COMMANDE : COMB_SISM_MODAL POUR MULTI-SUPPORT UNIQUEMENT
C        VERIFIE QUE LES MODES STATIQUES SONT DEFINIS AUX SUPPORTS,
C                    OPTION REAC_NODA CALCULEE DANS LES MODES MECANIQUES
C        RECUPERATION DES TYPES DE COMBINAISON DES SUPPORTS,
C                     DES DEPLACEMENTS DES SUPPORTS
C   DANS CETTE ROUTINE ON CREE UN SERIE DE COLLECTIONS
C   - LISTE_CAS :
C
C     ------------------------------------------------------------------
C IN  : MASSE  : MATRICE DE MASSE DE LA STRUCTURE
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16               ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                 ZK32
      CHARACTER*80                                          ZK80
      COMMON  / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32       JEXNOM, JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER      IBID, ICAS, IER, INO, IOCC, IRE1, IRE2, IREF, IRET,
     +             JCAS, JDGN, JDIR, JDREF,  JNO, JNOEU, JNREF, JREF,
     +             JSTA, JTYP, NBMC, NBNO, NBOCC, NC, NCAS, NOCAS, NS,
     +             NT, NUCAS, NX,   NY,   NZ
      REAL*8       DX, DY, DZ, EPSIMA, R8VIDE
      CHARACTER*4  CTYP
      CHARACTER*8  K8B, RESU, NOMA, NOREF
      CHARACTER*8  KNUM, KDIR, STAT, MOTCLE(2), TYMOCL(2)
      CHARACTER*15 MOTFAC
      CHARACTER*16  CONCEP, NOMCMD,MESNOE
      CHARACTER*24 OBJ1, OBJ2, VALK(2)
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      EPSIMA =R8VIDE()
      NOREF=' '
C
      CALL GETRES(RESU,CONCEP,NOMCMD)
C
      CALL DISMOI('F','NOM_MAILLA',MASSE,'MATR_ASSE',IBID,NOMA,IRET)
      OBJ1 = NOMA//'.GROUPENO'
      OBJ2 = NOMA//'.NOMNOE'
      IER = 0
C
C     --- RECUPERATION DES DEPLACEMENTS DES SUPPORTS ---
C
      MOTFAC = 'COMB_DEPL_APPUI'
      CALL GETFAC ( MOTFAC, NBOCC )
C
C -- CREATION DE LA COLLECTION LIST_CAS DE TOUTES LES OCCURRENCES
C -- DE COMB_DEPL_APPUI
C
      NCAS = 0
      CALL JECREC('&&ASENAP.LISTCAS', 'V V I', 'NU',
     &                               'DISPERSE', 'VARIABLE', NBOCC )
C
      DO 10 IOCC = 1,NBOCC
        CALL GETVTX(MOTFAC,'TOUT',IOCC,1,0,K8B,NT)
        IF (NT.NE.0) THEN
          CALL GETFAC('DEPL_MULT_APPUI',NCAS)
          IF (NCAS.LT.2) THEN
           CALL U2MESS('F','SEISME_21')
          ENDIF
          CALL JECROC(JEXNUM('&&ASENAP.LISTCAS',IOCC))
          CALL JEECRA(JEXNUM('&&ASENAP.LISTCAS',IOCC),'LONMAX',
     &                                                   NCAS,' ')
          CALL JEVEUO(JEXNUM('&&ASENAP.LISTCAS',IOCC),'E',JCAS)
          DO 12 ICAS = 1,NCAS
           CALL GETVIS('DEPL_MULT_APPUI','NUME_CAS',ICAS,1,1,NUCAS,IBID)
           ZI(JCAS+ICAS-1) = NUCAS
 12       CONTINUE
        ELSE
          CALL GETVIS(MOTFAC,'LIST_CAS',IOCC,1,0,IBID,NC)
          NC=-NC
          IF (NC.LT.2) THEN
            CALL U2MESS('F','SEISME_22')
          ENDIF
          CALL JECROC(JEXNUM('&&ASENAP.LISTCAS',IOCC))
          CALL JEECRA(JEXNUM('&&ASENAP.LISTCAS',IOCC),'LONMAX',
     &                                                   NC,' ')
          CALL JEVEUO(JEXNUM('&&ASENAP.LISTCAS',IOCC),'E',JCAS)
          CALL GETVIS(MOTFAC,'LIST_CAS',IOCC,1,NC,ZI(JCAS),NC)
          NCAS =NCAS+NC
        ENDIF
 10   CONTINUE

C
C -- CREATION DU VECTEUR TYPE_COMBI DE TOUTES LES OCCURRENCES
C -- DE COMB_DEPL_APPUI
C
      CALL WKVECT('&&ASENAP.TYPE','V V I',NBOCC,JTYP)

      DO 20 IOCC =1, NBOCC
        CALL GETVTX(MOTFAC,'TYPE_COMBI',IOCC,1,1,CTYP,NC)
        IF (CTYP.EQ.'QUAD') ZI(JTYP+IOCC-1) = 1
        IF (CTYP.EQ.'LINE') ZI(JTYP+IOCC-1) = 2
        IF (CTYP.EQ.'ABS')  ZI(JTYP+IOCC-1) = 3

 20   CONTINUE

C
C -- CREATION DE LA COLLECTION QUI CONTIENT LES NOEUDS
C -- DES DIFFERENTES OCCURRENCES DE DEPL_MULT_APPUI TRAITEES
C
      MOTFAC = 'DEPL_MULT_APPUI'
      CALL GETFAC(MOTFAC,NOCAS)
      CALL JECREC('&&ASENAP.LINOEU ', 'V V K8', 'NO',
     &                               'DISPERSE', 'VARIABLE',NOCAS )

      CALL JECREC('&&ASENAP.LIDIR ', 'V V R', 'NO',
     &                               'DISPERSE', 'VARIABLE',NOCAS )
C VECTEUR MODE_STATIQUE
      CALL WKVECT('&&ASENAP.STAT','V V K8',NOCAS,JSTA)
C
C VECTERUS RELATIFS AU NOEUD_REFE
C
      CALL WKVECT('&&ASENAP.NOREF','V V K8',NOCAS,JNREF)
      CALL WKVECT('&&ASENAP.NREF','V V I',NOCAS,JREF)
      CALL WKVECT('&&ASENAP.DREF','V V R',3*NOCAS,JDREF)

      MESNOE = '&&ASENAP.NOEUDS'
      DO 30 ICAS =1, NOCAS
        CALL GETVIS(MOTFAC,'NUME_CAS',ICAS,1,1,NUCAS,NC)
C
C INITIALISATION DU DEPLACEMENT DE  NOEUD_REFE
C
C
C -- STOCKAGE MODE STATIQUE DU NUME_CAS TRAITE
         CALL GETVID(MOTFAC,'MODE_STAT',ICAS,1,1,STAT,NS)
         ZK8(JSTA+ICAS-1)=STAT
C -- STOCKAGE DES NOEUD
         KNUM = 'N       '
         CALL CODENT(NUCAS, 'D0' , KNUM(2:8) )
         NBMC=2
         MOTCLE(1) = 'NOEUD'
         TYMOCL(1) = 'NOEUD'
         MOTCLE(2) = 'GROUP_NO'
         TYMOCL(2) = 'GROUP_NO'
         CALL RELIEM(' ',NOMA,'NO_NOEUD',MOTFAC,ICAS,NBMC,MOTCLE,
     &                TYMOCL,MESNOE,NBNO)
         CALL JEVEUO(MESNOE,'L',JNOEU)
C
         CALL JECROC(JEXNOM('&&ASENAP.LINOEU',KNUM))
         CALL JEECRA(JEXNOM('&&ASENAP.LINOEU',KNUM),'LONMAX',NBNO,' ')
         CALL JEECRA(JEXNOM('&&ASENAP.LINOEU',KNUM),'LONUTI',NBNO,' ')
         CALL JEVEUO(JEXNOM('&&ASENAP.LINOEU',KNUM), 'E', JNO )
         DO 34 INO =1,NBNO
           ZK8(JNO+INO-1) = ZK8(JNOEU+INO-1)
 34      CONTINUE
C -- STOCKAGE DES NOEUD REFE
         ZI(JREF+ICAS-1)= 0
C
         ZR(JDREF+ICAS-1) = 0.0D0
         ZR(JDREF+ICAS+1-1) = 0.0D0
         ZR(JDREF+ICAS+2-1) = 0.0D0
         CALL GETVTX(MOTFAC,'NOEUD_REFE',ICAS,1,1,NOREF,IREF)
         IF (IREF.NE.0) THEN
           CALL JENONU(JEXNOM(OBJ2,NOREF),IRE1)
           CALL JEEXIN(JEXNOM(OBJ1,NOREF),IRE2)
           IF ((IRE1+IRE2).EQ.0) THEN
             IER = IER + 1
              VALK(1) = NOREF
              VALK(2) = NOMA
              CALL U2MESK('E','SEISME_1', 2 ,VALK)
             GOTO 9999
           ENDIF
           IF (IRE2.NE.0) THEN
             CALL JEVEUO(JEXNOM(OBJ1,NOREF),'L',JDGN)
             CALL JENUNO(JEXNUM(OBJ2,ZI(JDGN)),NOREF)
           ENDIF

          ZK8(JNREF+ICAS-1) = NOREF
          ZI(JREF+ICAS-1)= 1
        ENDIF
C -- STOCKAGE DES DIRECTIONS D''ANCRAGE

          KDIR = 'D       '
          CALL CODENT(NUCAS, 'D0' , KDIR(2:8) )
          CALL JECROC(JEXNOM('&&ASENAP.LIDIR',KDIR))
          CALL JEECRA(JEXNOM('&&ASENAP.LIDIR',KDIR),'LONMAX',3*NBNO,' ')
          CALL JEECRA(JEXNOM('&&ASENAP.LIDIR',KDIR),'LONUTI',3*NBNO,' ')
          CALL JEVEUO(JEXNOM('&&ASENAP.LIDIR',KDIR), 'E', JDIR )
          DO 36 INO =1,3*NBNO
            ZR(JDIR+INO-1)= EPSIMA
 36       CONTINUE
           CALL GETVR8(MOTFAC,'DX',ICAS,1,1,DX,NX)
           CALL GETVR8(MOTFAC,'DY',ICAS,1,1,DY,NY)
           CALL GETVR8(MOTFAC,'DZ',ICAS,1,1,DZ,NZ)
C
          DO 38 INO =1,NBNO
            IF (NX.NE.0)  ZR(JDIR+3*(INO-1))= DX
            IF (NY.NE.0)  ZR(JDIR+3*(INO-1)+1) = DY
            IF (NZ.NE.0)  ZR(JDIR+3*(INO-1)+2)= DZ
C
          IF (ZK8(JNO+INO-1).EQ.NOREF) THEN
            ZR(JDREF+ICAS-1) = DX
            ZR(JDREF+ICAS+1-1) = DY
            ZR(JDREF+ICAS+2-1) = DZ
          ENDIF
          IF (ZI(JREF+ICAS-1).EQ.1) THEN
           ZR(JDIR+3*(INO-1))=ZR(JDIR+3*(INO-1))-ZR(JDREF+ICAS-1)
           ZR(JDIR+3*(INO-1)+1)=ZR(JDIR+3*(INO-1)+1)-ZR(JDREF+ICAS+1-1)
           ZR(JDIR+3*(INO-1)+2)=ZR(JDIR+3*(INO-1)+2)-ZR(JDREF+ICAS+2-1)
          ENDIF

 38       CONTINUE
 30   CONTINUE
C

      CALL JEDETR ( MESNOE )

 9999 CONTINUE
      IF (IER.NE.0) CALL U2MESS('F','SEISME_6')
C
      CALL JEDEMA()
      END

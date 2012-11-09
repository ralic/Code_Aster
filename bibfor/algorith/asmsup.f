      SUBROUTINE ASMSUP ( MASSE, MECA, NBMODE, NEQ, NBSUP, NSUPP,
     &                    NOMSUP, NDIR, REASUP, TCOSUP, NUME, LORDR )
      IMPLICIT  NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM,JEXNOM
      INTEGER           NBMODE, NEQ, NBSUP, NDIR(*), NSUPP(*),
     &                  TCOSUP(NBSUP,*), LORDR(*)
      REAL*8            REASUP(NBSUP,NBMODE,*)
      CHARACTER*8       MASSE, MECA, NOMSUP(NBSUP,*)
      CHARACTER*14      NUME
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C     ------------------------------------------------------------------
C     COMMANDE : COMB_SISM_MODAL POUR MULTI-SUPPORT UNIQUEMENT
C        VERIFIE QUE LES MODES STATIQUES SONT DEFINIS AUX SUPPORTS,
C                    OPTION REAC_NODA CALCULEE DANS LES MODES MECANIQUES
C        RECUPERATION DES TYPES DE COMBINAISON DES SUPPORTS,
C                     DES DEPLACEMENTS DES SUPPORTS
C     ------------------------------------------------------------------
C IN  : MASSE  : MATRICE DE MASSE DE LA STRUCTURE
C IN  : MECA   : MODES MECANIQUES DE LA STRUCTURE
C IN  : NBMODE : NOMBRE DE MODES MECANIQUES
C IN  : NEQ    : NOMBRE D'EQUATIONS
C IN  : NBSUP  : NOMBRE DE SUPPORTS DE LA STRUCTURE
C IN  : NSUPP  : MAX DU NOMBRE DE SUPPORT PAR DIRECTION
C IN  : NOMSUP : VECTEUR DES NOMS DES SUPPORTS
C IN  : NDIR   : DIRECTION DES EXCITATIONS
C OUT : REASUP : VECTEUR DES REACTIONS MODALES AUX SUPPORTS
C OUT : TCOSUP : VECTEUR DES TYPES DE RECOMBINAISON DES SUPPORTS
C                TCOSUP(I) = 1 : COMBINAISON QUADRATIQUE
C                TCOSUP(I) = 2 : COMBINAISON LINEAIRE
C                TCOSUP(I) = 3 : COMBINAISON ABSOLUE
C     ------------------------------------------------------------------
      INTEGER      IBID, ID, IDDL, IER, IGR, IM, IN, INO, IOC, IRET, IS,
     &             JDDL1, JDDL2, JDGN, JGRN, JNOE, LVALE, NBA, NBB, N1,
     &             NBBD, NBL, NBLIAI, NBOCC, NBTROU, NGR, NNO, NT,
     &             VALI(2)
      CHARACTER*1  K1B
      CHARACTER*4  CTYP, DIR(3)
      CHARACTER*8  K8B, NOMA, GRNOEU, NOEU, NOMCMP(3)
      CHARACTER*15 MOTFAC
      CHARACTER*16 NOMSY
      CHARACTER*19 CHAM19
      CHARACTER*24 OBJ1, OBJ2, VALK(2)
      INTEGER      IARG
C     ------------------------------------------------------------------
      DATA  DIR / 'X' , 'Y' , 'Z' /
      DATA  NOMCMP / 'DX' , 'DY' , 'DZ' /
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL DISMOI('F','NOM_MAILLA',MASSE,'MATR_ASSE',IBID,NOMA,IER)
      OBJ1 = NOMA//'.GROUPENO'
      OBJ2 = NOMA//'.NOMNOE'
      IER = 0
C
C     --- VERIFICATION DES SUPPORTS ---
      CALL WKVECT('&&ASMSUP.POSITION.DDL1','V V I',NEQ,JDDL1)
      CALL WKVECT('&&ASMSUP.POSITION.DDL2','V V I',NEQ,JDDL2)
      CALL TYPDDL('BLOQ',NUME,NEQ,ZI(JDDL1),NBA,NBB,NBL,NBLIAI)
      DO 10 ID = 1,3
         IF (NDIR(ID).EQ.1) THEN
            CALL PTEDDL('NUME_DDL',NUME,1,NOMCMP(ID),NEQ,ZI(JDDL2))
            NBBD = 0
            DO 12 IN = 1,NEQ
               NBBD = NBBD + ( ZI(JDDL1+IN-1) * ZI(JDDL2+IN-1) )
 12         CONTINUE
            IF (NSUPP(ID).NE.NBBD) THEN
               IER = IER + 1
               VALK(1) = DIR(ID)
               VALI(1) = NBBD
               VALI(2) = NSUPP(ID)
               CALL U2MESG('E', 'SEISME_23',1,VALK,2,VALI,0,0.D0)
            ENDIF
         ENDIF
 10   CONTINUE
      CALL JEDETR('&&ASMSUP.POSITION.DDL1')
      CALL JEDETR('&&ASMSUP.POSITION.DDL2')
C
C     --- VERIFICATION DE L'OPTION "REAC_NODA" ---
      NOMSY = 'REAC_NODA'
      CALL RSUTNC(MECA,NOMSY,0,K8B,IBID,NBTROU)
      IF (NBTROU.EQ.0) THEN
         IER = IER + 1
         VALK(1) = MECA
         VALK(2) = NOMSY
         CALL U2MESK('E', 'SEISME_24',2,VALK)
         GOTO 9999
      ENDIF
C
C     --- RECUPERATION DES REACTIONS NODALES ---
      DO 60 IM = 1,NBMODE
         CALL RSEXCH('F',MECA,NOMSY,LORDR(IM),CHAM19,IRET)
         CALL JEVEUO(CHAM19//'.VALE','L',LVALE)
         DO 62 ID = 1,3
            IF (NDIR(ID).EQ.1) THEN
               DO 64 IS = 1,NSUPP(ID)
                  NOEU = NOMSUP(IS,ID)
                  CALL POSDDL('NUME_DDL',NUME,NOEU,NOMCMP(ID),INO,IDDL)
                  REASUP(IS,IM,ID) = ZR(LVALE+IDDL-1)
 64            CONTINUE
            ENDIF
 62      CONTINUE
 60   CONTINUE
C
C     --- RECUPERATION DES COMBINAISONS DES SUPPORTS ---
      MOTFAC = 'GROUP_APPUI'
      CALL GETFAC ( MOTFAC, NBOCC )
      IF (NBOCC.EQ.0) THEN
        MOTFAC = 'COMB_MULT_APPUI'
        CALL GETFAC ( MOTFAC, NBOCC )
      ENDIF
      DO 39 ID = 1,3
         DO 40 IS = 1,NBSUP
            TCOSUP(IS,ID) = 1
 40      CONTINUE
 39   CONTINUE
      DO 42 IOC = 1,NBOCC
        CTYP = ' '
        IF (MOTFAC.EQ.'GROUP_APPUI') THEN
          CTYP = 'QUAD'
          NT = 0
        ELSE
          CALL GETVTX(MOTFAC,'TYPE_COMBI',IOC,IARG,1,CTYP,N1)
          CALL GETVTX(MOTFAC,'TOUT',IOC,IARG,1,K8B,NT)
        ENDIF
        IF (CTYP.NE.'QUAD') THEN
          IF (NT.NE.0) THEN
            DO 44 ID = 1,3
              DO 45 IS = 1,NBSUP
                IF (CTYP.EQ.'LINE')  TCOSUP(IS,ID) = 2
 45           CONTINUE
 44         CONTINUE
          ELSE
            CALL GETVTX(MOTFAC,'NOEUD',IOC,IARG,0,NOEU,N1)
            IF (N1.NE.0) THEN
              NNO = -N1
              CALL WKVECT('&&ASMSUP.NOEUD','V V K8',NNO,JNOE)
              CALL GETVTX(MOTFAC,'NOEUD',IOC,IARG,NNO,ZK8(JNOE),N1)
              DO 46 INO = 1, NNO
                NOEU = ZK8(JNOE+INO-1)
                CALL JENONU(JEXNOM(OBJ2,NOEU),IRET)
                IF (IRET.EQ.0) THEN
                  IER = IER + 1
                  VALK(1) = NOEU
                  VALK(2) = NOMA
                  CALL U2MESK('E','SEISME_1', 2 ,VALK)
                  GOTO 46
                ENDIF
                DO 48 IS = 1,NBSUP
                  DO 49 ID = 1,3
                    IF (NOMSUP(IS,ID).EQ.NOEU) THEN
                      IF (CTYP.EQ.'LINE')  TCOSUP(IS,ID) = 2
                    ENDIF
 49               CONTINUE
 48             CONTINUE
 46           CONTINUE
              CALL JEDETR('&&ASMSUP.NOEUD')
            ELSE
              CALL GETVTX(MOTFAC,'GROUP_NO',IOC,IARG,0,K8B,N1)
              IF (N1.NE.0) THEN
                NGR = -N1
                CALL WKVECT('&&ASMSUP.GROUP_NO','V V K8',NGR,JGRN)
                CALL GETVTX(MOTFAC,'GROUP_NO',IOC,IARG,NGR,ZK8(JGRN),N1)
                DO 50 IGR = 1, NGR
                  GRNOEU = ZK8(JGRN+IGR-1)
                  CALL JEEXIN(JEXNOM(OBJ1,GRNOEU),IRET)
                  IF (IRET .EQ. 0) THEN
                    IER = IER + 1
                    VALK(1) = GRNOEU
                    VALK(2) = NOMA
                    CALL U2MESK('E','SEISME_2', 2 ,VALK)
                    GOTO 50
                  ELSE
                    CALL JELIRA(JEXNOM(OBJ1,GRNOEU),'LONUTI',NNO,K1B)
                    CALL JEVEUO(JEXNOM(OBJ1,GRNOEU),'L',JDGN)
                    DO 52 INO = 1, NNO
                      CALL JENUNO(JEXNUM(OBJ2,ZI(JDGN+INO-1)),NOEU)
                      DO 54 IS = 1,NBSUP
                        DO 55 ID = 1,3
                          IF (NOMSUP(IS,ID).EQ.NOEU) THEN
                            IF (CTYP.EQ.'LINE')   TCOSUP(IS,ID) = 2
                          ENDIF
 55                     CONTINUE
 54                   CONTINUE
 52                 CONTINUE
                  ENDIF
 50             CONTINUE
                CALL JEDETR('&&ASMSUP.GROUP_NO')
              ENDIF
            ENDIF
          ENDIF
        ENDIF
 42   CONTINUE
C
 9999 CONTINUE
      IF (IER.NE.0) CALL U2MESS('F','SEISME_6')
C
      CALL JEDEMA()
      END

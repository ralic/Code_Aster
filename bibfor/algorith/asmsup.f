      SUBROUTINE ASMSUP(MASSE,MECA,STAT,NBMODE,NEQ,NBSUP,NSUPP,
     +                  NOMSUP,NDIR,PSMO,DEPSUP,REASUP,TCOSUP,
     +                  NUME,LORDR)
      IMPLICIT  REAL*8 (A-H,O-Z)
      INTEGER           NDIR(*),NSUPP(*),TCOSUP(NBSUP,*),LORDR(*)
      REAL*8            DEPSUP(NBSUP,*),REASUP(NBSUP,NBMODE,*)
      CHARACTER*8       MASSE,MECA,STAT,PSMO,NOMSUP(NBSUP,*)
      CHARACTER*14      NUME
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/06/2002   AUTEUR CIBHHPD D.NUNEZ 
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
C     ------------------------------------------------------------------
C     COMMANDE : COMB_SISM_MODAL POUR MULTI-SUPPORT UNIQUEMENT
C        VERIFIE QUE LES MODES STATIQUES SONT DEFINIS AUX SUPPORTS,
C                    OPTION REAC_NODA CALCULEE DANS LES MODES MECANIQUES
C        RECUPERATION DES TYPES DE COMBINAISON DES SUPPORTS,
C                     DES DEPLACEMENTS DES SUPPORTS
C     ------------------------------------------------------------------
C IN  : MASSE  : MATRICE DE MASSE DE LA STRUCTURE
C IN  : MECA   : MODES MECANIQUES DE LA STRUCTURE
C OUT : STAT   : MODES STATIQUES DE LA STRUCTURE
C IN  : NBMODE : NOMBRE DE MODES MECANIQUES
C IN  : NEQ    : NOMBRE D'EQUATIONS
C IN  : NBSUP  : NOMBRE DE SUPPORTS DE LA STRUCTURE
C IN  : NSUPP  : MAX DU NOMBRE DE SUPPORT PAR DIRECTION
C IN  : NOMSUP : VECTEUR DES NOMS DES SUPPORTS
C IN  : NDIR   : DIRECTION DES EXCITATIONS
C IN  : PSMO   : PSEUDO-MODES STATIQUES DE LA STRUCTURE
C OUT : DEPSUP : VECTEUR DES DEPLACEMENTS DES SUPPORTS
C OUT : REASUP : VECTEUR DES REACTIONS MODALES AUX SUPPORTS
C OUT : TCOSUP : VECTEUR DES TYPES DE RECOMBINAISON DES SUPPORTS
C                TCOSUP(I) = 1 : COMBINAISON QUADRATIQUE
C                TCOSUP(I) = 2 : COMBINAISON LINEAIRE
C                TCOSUP(I) = 3 : COMBINAISON ABSOLUE
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
      CHARACTER*4  CTYP, DIR(3)
      CHARACTER*8  K8B, RESU, NOMA, GRNOEU, NOEU, CMP, NOMCMP(3), NOEREF
      CHARACTER*15 MOTFAC,MOTFA2
      CHARACTER*16 NOMSY, CONCEP, NOMCMD, MONACC, MONPAR
      CHARACTER*19 CHAM19
      CHARACTER*24 OBJ1, OBJ2
      CHARACTER*8  K8BID
      CHARACTER*1  K1BID
C     ------------------------------------------------------------------
      DATA  DIR / 'X' , 'Y' , 'Z' /
      DATA  NOMCMP / 'DX' , 'DY' , 'DZ' /
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL GETRES(RESU,CONCEP,NOMCMD)
C
      CALL DISMOI('F','NOM_MAILLA',MASSE,'MATR_ASSE',IBID,NOMA,IRET)
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
            CALL PTEDDL('NUME_DDL',NUME,1,NOMCMP(ID),NEQ,ZI(JDDL2) )
            NBBD = 0
            DO 12 IN = 1,NEQ
               NBBD = NBBD + ( ZI(JDDL1+IN-1) * ZI(JDDL2+IN-1) )
 12         CONTINUE
            IF (NSUPP(ID).NE.NBBD) THEN
               IER = IER + 1
               CALL UTDEBM('E',NOMCMD,'DONNEES INCOMPATIBLES :')
               CALL UTIMPK('L','   POUR LA DIRECTION ',1,DIR(ID))
               CALL UTIMPI('L','   NOMBRE DE BLOCAGE : ',1,NBBD)
              CALL UTIMPI('L','   NOMBRE D''EXCITATIONS : ',1,NSUPP(ID))
               CALL UTFINM( )
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
         CALL UTDEBM('E',NOMCMD,'DONNEES INCOMPATIBLES :')
         CALL UTIMPK('L','   POUR LES MODES MECANIQUES : ',1,MECA)
         CALL UTIMPK('L','   IL MANQUE L''OPTION : ',1,NOMSY)
         CALL UTFINM( )
         GOTO 9999
      ENDIF
C
C     --- RECUPERATION DES REACTIONS NODALES ---
      DO 60 IM = 1,NBMODE
         CALL RSEXCH(MECA,NOMSY,LORDR(IM),CHAM19,IE)
         CALL JEVEUO(CHAM19//'.VALE','L',LVALE)
         DO 62 ID = 1,3
            IF (NDIR(ID).EQ.1) THEN
               DO 64 IS = 1,NSUPP(ID)
                  NOEU = NOMSUP(IS,ID)
                CALL POSDDL('NUME_DDL',NUME,NOEU,NOMCMP(ID),INOEUD,IDDL)
                  REASUP(IS,IM,ID) = ZR(LVALE+IDDL-1)
 64            CONTINUE
            ENDIF
 62      CONTINUE
 60   CONTINUE
C
C     --- RECUPERATION DES COMBINAISONS DES SUPPORTS ---
      DO 39 ID = 1,3
         DO 40 IS = 1,NBSUP
            TCOSUP(IS,ID) = 1
 40      CONTINUE
 39   CONTINUE
      MOTFAC = 'COMB_MULT_APPUI'
      CALL GETFAC(MOTFAC,NBOCC)
      DO 42 IOC = 1,NBOCC
         CTYP = ' '
         CALL GETVTX(MOTFAC,'TYPE'      ,IOC,1,1,CTYP,NC)
         IF (NC.NE.0) THEN
           CALL UTDEBM('A',NOMCMD,'LE MOT ')
           CALL UTIMPK('S','CLE ',1,'TYPE')
           CALL UTIMPK('S',' EST APPELE A DISPARAITRE EN 6.4 ET SERA'//
     +                     ' REMPLACE PAR ',1,'TYPE_COMBI')
           CALL UTFINM()
         ENDIF
         CALL GETVTX(MOTFAC,'TYPE_COMBI',IOC,1,1,CTYP,NC)
         CALL GETVTX(MOTFAC,'TOUT',IOC,1,1,K8B ,NT)
         IF (CTYP.NE.'QUAD') THEN
          IF (NT.NE.0) THEN
           DO 44 ID = 1,3
            DO 45 IS = 1,NBSUP
             IF (CTYP.EQ.'LINE') THEN
              TCOSUP(IS,ID) = 2
             ELSE
              TCOSUP(IS,ID) = 3
             ENDIF
 45         CONTINUE
 44        CONTINUE
          ELSE
           CALL GETVID(MOTFAC,'NOEUD',IOC,1,0,NOEU,NN)
           IF (NN.NE.0) THEN
            NNO = -NN
            CALL WKVECT('ASMSUP.NOEUD','V V K8',NNO,JNOE)
            CALL GETVID(MOTFAC,'NOEUD',IOC,1,NNO,ZK8(JNOE),NN)
            DO 46 INO = 1, NNO
             NOEU = ZK8(JNOE+INO-1)
             CALL JENONU(JEXNOM(OBJ2,NOEU),IRET)
             IF (IRET.EQ.0) THEN
              IER = IER + 1
              CALL UTMESS('E',MOTFAC,'LE NOEUD '//NOEU//
     +                    ' NE FAIT PAS PARTI DU MAILLAGE : '//NOMA)
              GOTO 46
             ENDIF
             DO 48 IS = 1,NBSUP
              DO 49 ID = 1,3
               IF (NOMSUP(IS,ID).EQ.NOEU) THEN
                IF (CTYP.EQ.'LINE') THEN
                  TCOSUP(IS,ID) = 2
                ELSE
                  TCOSUP(IS,ID) = 3
                ENDIF
               ENDIF
 49           CONTINUE
 48          CONTINUE
 46         CONTINUE
            CALL JEDETR('ASMSUP.NOEUD')
           ELSE
            CALL GETVID(MOTFAC,'GROUP_NO',IOC,1,0,K8BID,NG)
            IF (NG.NE.0) THEN
             NGR = -NG
             CALL WKVECT('ASMSUP.GROUP_NO','V V K8',NGR,JGRN)
             CALL GETVID(MOTFAC,'GROUP_NO',IOC,1,NGR,ZK8(JGRN),NG)
             DO 50 IGR = 1, NGR
              GRNOEU = ZK8(JGRN+IGR-1)
              CALL JEEXIN(JEXNOM(OBJ1,GRNOEU),IRET)
              IF (IRET .EQ. 0) THEN
               IER = IER + 1
               CALL UTMESS('E',MOTFAC,'LE GROUPE '//GRNOEU//
     +                        ' N''APPARTIENT PAS AU MAILLAGE : '//NOMA)
               GOTO 50
              ELSE
               CALL JELIRA(JEXNOM(OBJ1,GRNOEU),'LONMAX',NN,
     +                              K1BID)
               CALL JEVEUO(JEXNOM(OBJ1,GRNOEU),'L',JDGN)
               DO 52 INO = 1, NN
                CALL JENUNO(JEXNUM(OBJ2,ZI(JDGN+INO-1)),NOEU)
                DO 54 IS = 1,NBSUP
                 DO 55 ID = 1,3
                  IF (NOMSUP(IS,ID).EQ.NOEU) THEN
                   IF (CTYP.EQ.'LINE') THEN
                    TCOSUP(IS,ID) = 2
                   ELSE
                    TCOSUP(IS,ID) = 3
                   ENDIF
                  ENDIF
 55              CONTINUE
 54             CONTINUE
 52            CONTINUE
              ENDIF
 50          CONTINUE
             CALL JEDETR('ASMSUP.GROUP_NO')
            ENDIF
           ENDIF
          ENDIF      
         ENDIF           
 42   CONTINUE
C
 9999 CONTINUE
      IF (IER.NE.0) CALL UTMESS('F',NOMCMD,'DONNEES INCOMPATIBLES.')
C
99999 CONTINUE
      CALL JEDEMA()
      END

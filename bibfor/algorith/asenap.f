      SUBROUTINE ASENAP(MASSE,NBSUP,NSUPP,
     +                  NOMSUP,NDIR,DEPSUP,TCOSUP)
      IMPLICIT  REAL*8 (A-H,O-Z)
      INTEGER           NDIR(*),NSUPP(*),TCOSUP(NBSUP,*)
      REAL*8            DEPSUP(NBSUP,*)
      CHARACTER*8       MASSE,NOMSUP(NBSUP,*)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/06/2002   AUTEUR CIBHHPD D.NUNEZ 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C IN  : MASSE  : MATRICE DE MASSE DE LA STRUCTURE
C IN  : NBSUP  : NOMBRE DE SUPPORTS DE LA STRUCTURE
C IN  : NSUPP  : MAX DU NOMBRE DE SUPPORT PAR DIRECTION
C IN  : NOMSUP : VECTEUR DES NOMS DES SUPPORTS
C IN  : NDIR   : DIRECTION DES EXCITATIONS
C OUT : DEPSUP : VECTEUR DES DEPLACEMENTS DES SUPPORTS
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
      CHARACTER*4  CTYP
      CHARACTER*8  K8B, RESU, NOMA, GRNOEU, NOEU, CMP, NOEREF
      CHARACTER*15 MOTFAC,MOTFA2
      CHARACTER*16 NOMSY, CONCEP, NOMCMD, MONACC, MONPAR
      CHARACTER*19 CHAM19
      CHARACTER*24 OBJ1, OBJ2
      CHARACTER*8  K8BID
      CHARACTER*1  K1BID
C     ------------------------------------------------------------------

      CALL JEMARQ()
      CALL GETRES(RESU,CONCEP,NOMCMD)
C
      CALL DISMOI('F','NOM_MAILLA',MASSE,'MATR_ASSE',IBID,NOMA,IRET)
      OBJ1 = NOMA//'.GROUPENO'
      OBJ2 = NOMA//'.NOMNOE'
      IER = 0
C
C     --- RECUPERATION DES DEPLACEMENTS DES SUPPORTS ---
      MOTFAC = 'DEPL_MULT_APPUI'
      CALL GETFAC(MOTFAC,NBOCC)
      IF (NBOCC.EQ.0) THEN
         IER = IER + 1
        CALL UTMESS('E',NOMCMD,'IL MANQUE LES DEPLACEMENTS DES APPUIS.')
         GOTO 9999
      ENDIF
      INORF = 0
C
C MISE A ZERO DE DEPSUP POUR LE CAS OU IL N'Y A PAS DE NOEUD_REFE
C 
      DO 5 II = 1,3
       DO 6 IS  = 1,NSUPP(II)
         DEPSUP(IS,II) = 0.0D0
 6     CONTINUE
 5    CONTINUE
C
      DO 20 IOC = 1,NBOCC
         CALL GETVID(MOTFAC,'NOEUD_REFE',IOC,1,1,NOEREF,NNR)
         IF (NNR.NE.0) INORF = 1
         CALL GETVID(MOTFAC,'NOEUD',IOC,1,0,NOEU,NN)
         IF (NN.NE.0) THEN
            NNO = -NN
            CALL WKVECT('ASENAP.NOEUD','V V K8',NNO,JNOE)
            CALL GETVID(MOTFAC,'NOEUD',IOC,1,NNO,ZK8(JNOE),NN)
            CALL GETVR8(MOTFAC,'DX',IOC,1,1,DX,NX)
            CALL GETVR8(MOTFAC,'DY',IOC,1,1,DY,NY)
            CALL GETVR8(MOTFAC,'DZ',IOC,1,1,DZ,NZ)
            DO 22 INO = 1, NNO
               NOEU = ZK8(JNOE+INO-1)
               CALL JENONU(JEXNOM(OBJ2,NOEU),IRET)
               IF (IRET.EQ.0) THEN
                  IER = IER + 1
                  CALL UTMESS('E',MOTFAC,'LE NOEUD '//NOEU//
     +                        ' N''APPARTIENT PAS AU MAILLAGE : '//NOMA)
                  GOTO 22
               ENDIF
               IF (NX.NE.0) THEN
                  DO 72 IS = 1,NSUPP(1)
                     IF (NOMSUP(IS,1).EQ.NOEU) DEPSUP(IS,1) = DX
 72               CONTINUE
               ENDIF
               IF (NY.NE.0) THEN
                  DO 74 IS = 1,NSUPP(2)
                     IF (NOMSUP(IS,2).EQ.NOEU) DEPSUP(IS,2) = DY
 74               CONTINUE
               ENDIF
               IF (NZ.NE.0) THEN
                  DO 76 IS = 1,NSUPP(3)
                     IF (NOMSUP(IS,3).EQ.NOEU) DEPSUP(IS,3) = DZ
 76               CONTINUE
               ENDIF
 22         CONTINUE
            CALL JEDETR('ASENAP.NOEUD')
         ELSE
           CALL GETVID(MOTFAC,'GROUP_NO',IOC,1,0,K8BID,NG)
           IF (NG.NE.0) THEN
            NGR = -NG
            CALL WKVECT('ASENAP.GROUP_NO','V V K8',NGR,JGRN)
            CALL GETVID(MOTFAC,'GROUP_NO',IOC,1,NGR,ZK8(JGRN),NG)
            CALL GETVR8(MOTFAC,'DX',IOC,1,1,DX,NX)
            CALL GETVR8(MOTFAC,'DY',IOC,1,1,DY,NY)
            CALL GETVR8(MOTFAC,'DZ',IOC,1,1,DZ,NZ)
            DO 26 IGR = 1, NGR
               GRNOEU = ZK8(JGRN+IGR-1)
               CALL JEEXIN(JEXNOM(OBJ1,GRNOEU),IRET)
               IF (IRET .EQ. 0) THEN
                  IER = IER + 1
                  CALL UTMESS('E',MOTFAC,'LE GROUPE '//GRNOEU//
     +                        ' N''APPARTIENT PAS AU MAILLAGE : '//NOMA)
                  GOTO 26
               ELSE
                  CALL JELIRA(JEXNOM(OBJ1,GRNOEU),'LONMAX',NN,K1BID)
                  CALL JEVEUO(JEXNOM(OBJ1,GRNOEU),'L',JDGN)
                  DO 28 INO = 1, NN
                     CALL JENUNO(JEXNUM(OBJ2,ZI(JDGN+INO-1)),NOEU)
                     IF (NX.NE.0) THEN
                        DO 82 IS = 1,NSUPP(1)
                           IF (NOMSUP(IS,1).EQ.NOEU) DEPSUP(IS,1) = DX
 82                     CONTINUE
                     ENDIF
                     IF (NY.NE.0) THEN
                        DO 84 IS = 1,NSUPP(2)
                           IF (NOMSUP(IS,2).EQ.NOEU) DEPSUP(IS,2) = DY
 84                     CONTINUE
                     ENDIF
                     IF (NZ.NE.0) THEN
                        DO 86 IS = 1,NSUPP(3)
                           IF (NOMSUP(IS,3).EQ.NOEU) DEPSUP(IS,3) = DZ
 86                     CONTINUE
                     ENDIF
 28               CONTINUE
               ENDIF
 26         CONTINUE
            CALL JEDETR('ASENAP.GROUP_NO')
          ENDIF
         ENDIF
 20   CONTINUE
      IF (INORF.EQ.0) THEN
         CALL UTMESS('A',NOMCMD,' PAS DE NOEUD DE REFERENCE.')
      ELSE
         CALL JENONU(JEXNOM(OBJ2,NOEREF),IRE1)
         CALL JEEXIN(JEXNOM(OBJ1,NOEREF),IRE2)
         IF ((IRE1+IRE2).EQ.0) THEN
            IER = IER + 1
            CALL UTMESS('E',MOTFAC,'LE NOEUD '//NOEREF//
     +                        ' N''APPARTIENT PAS AU MAILLAGE : '//NOMA)
            GOTO 9999
         ENDIF
         IF (IRE2.NE.0) THEN
            CALL JEVEUO(JEXNOM(OBJ1,NOEREF),'L',JDGN)
            CALL JENUNO(JEXNUM(OBJ2,ZI(JDGN)),NOEREF)
         ENDIF
         DO 90 ID = 1,3
            IF (NDIR(ID).EQ.1) THEN
               DO 92 IS = 1,NSUPP(ID)
                  IF (NOMSUP(IS,ID).EQ.NOEREF) THEN
                      DO 94 IN = 1,NSUPP(ID)
                         DEPSUP(IN,ID) = DEPSUP(IN,ID) - DEPSUP(IS,ID)
 94                   CONTINUE
                      GOTO 90
                  ENDIF
 92            CONTINUE
               IER = IER + 1
               CALL UTMESS('E',MOTFAC,'LE NOEUD '//NOEREF//
     +                             ' N''EST PAS UN NOEUD SUPPORT.')
               GOTO 9999
            ENDIF
 90      CONTINUE
      ENDIF
C
C     --- RECUPERATION DES COMBINAISONS DES SUPPORTS ---
      DO 39 ID = 1,3
         DO 40 IS = 1,NBSUP
            TCOSUP(IS,ID) = 1
 40      CONTINUE
 39   CONTINUE
      MOTFAC = 'COMB_DEPL_APPUI'
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
C RECUPERATION DES CAS A TRAITER TOUT_CAS OU LIST_CAS
            IF (NT.NE.0)  THEN 
              CALL GETFAC('DEPL_MULT_APPUI',NCAS)
              DO 2 II = 1,NCAS
               CALL GETVIS('DEPL_MULT_APPUI','NUME_CAS',II,1,1,NUCAS,NC)
               ZI(JCAS+II-1) = NUCAS
 2           CONTINUE
            ENDIF
            CALL GETVIS(MOTFAC,'LIST_CAS',IOC,1,0,IBID,NC)
            IF (NC.NE.0) NCAS = -NC
            CALL WKVECT('ASENAP.LISTE','V V I',NCAS,JCAS)
            CALL GETVIS(MOTFAC,'LIST_CAS',IOC,1,NCAS,ZI(JCAS),NC)
C BOUCLE SUR TOUS LES CAS TRAITES
            DO 67 ICAS = 1, NCAS    
            LCAS = ZI(JCAS+ICAS-1) 
            MOTFA2 = 'DEPL_MULT_APPUI'
            CALL GETFAC(MOTFA2,NBOC2)
            DO 68 II =1, NBOC2   
             CALL GETVIS(MOTFA2,'NUME_CAS',II,1,1,NUCAS,NC)
C ON PARCOURT LES NUME_CAS DE DEPL_MULT_APPUI POUR TROUVER CEUX 
C CONCERNES PAS COMB_MULT_APPUI
             IF (NUCAS.EQ.LCAS) THEN
              CALL GETVID(MOTFA2,'NOEUD',II,1,0,NOEU,NN)
              IF (NN.NE.0) THEN
               NNO = -NN
               CALL WKVECT('ASENAP.NOEUD','V V K8',NNO,JNOE)
               CALL GETVID(MOTFA2,'NOEUD',II,1,NNO,ZK8(JNOE),NN)
               DO 56 INO = 1, NNO
                 NOEU = ZK8(JNOE+INO-1)
                 CALL JENONU(JEXNOM(OBJ2,NOEU),IRET)
                 IF (IRET.EQ.0) THEN
                  IER = IER + 1
                  CALL UTMESS('E',MOTFA2,'LE NOEUD '//NOEU//
     +            ' NE FAIT PAS PARTI DU MAILLAGE : '//NOMA)
                  GOTO 56
                 ENDIF
                 DO 58 IS = 1,NBSUP
                  DO 59 ID = 1,3
                   IF (NOMSUP(IS,ID).EQ.NOEU) THEN
                    IF (CTYP.EQ.'LINE') THEN
                      TCOSUP(IS,ID) = 2
                    ELSE
                      TCOSUP(IS,ID) = 3
                    ENDIF
                   ENDIF
 59               CONTINUE
 58              CONTINUE
 56             CONTINUE
                CALL JEDETR('ASENAP.NOEUD')
               ELSE
                CALL GETVID(MOTFA2,'GROUP_NO',II,1,0,K8BID,NG)
                IF (NG.NE.0) THEN
                 NGR = -NG
                 CALL WKVECT('ASENAP.GROUP_NO','V V K8',NGR,JGRN)
                 CALL GETVID(MOTFA2,'GROUP_NO',II,1,NGR,
     +              ZK8(JGRN),NG)
                 DO 70 IGR = 1, NGR
                  GRNOEU = ZK8(JGRN+IGR-1)
                  CALL JEEXIN(JEXNOM(OBJ1,GRNOEU),IRET)
                  IF (IRET .EQ. 0) THEN
                   IER = IER + 1
                   CALL UTMESS('E',MOTFA2,'LE GROUPE '//GRNOEU//
     +             ' N''APPARTIENT PAS AU MAILLAGE : '//NOMA)
                    GOTO 70
                   ELSE
                    CALL JELIRA(JEXNOM(OBJ1,GRNOEU),'LONMAX',NN,
     +              K1BID)
                    CALL JEVEUO(JEXNOM(OBJ1,GRNOEU),'L',JDGN)
                    DO 102 INO = 1, NN
                     CALL JENUNO(JEXNUM(OBJ2,ZI(JDGN+INO-1)),NOEU)
                     DO 104 IS = 1,NBSUP
                      DO 65 ID = 1,3
                       IF (NOMSUP(IS,ID).EQ.NOEU) THEN
                        IF (CTYP.EQ.'LINE') THEN
                          TCOSUP(IS,ID) = 2
                        ELSE
                          TCOSUP(IS,ID) = 3
                        ENDIF  
                       ENDIF
 65                   CONTINUE
 104                 CONTINUE
 102                CONTINUE
                   ENDIF
 70               CONTINUE
                  CALL JEDETR('ASENAP.GROUP_NO')
                 ENDIF
                ENDIF
               ENDIF
 68           CONTINUE
 67          CONTINUE  
             CALL JEDETR('ASENAP.LISTE')
        ENDIF
 42   CONTINUE
C
 9999 CONTINUE
      IF (IER.NE.0) CALL UTMESS('F',NOMCMD,'DONNEES INCOMPATIBLES.')
C
99999 CONTINUE
      CALL JEDEMA()
      END

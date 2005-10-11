      SUBROUTINE DYOBS1(MAILLA,NBOCC,NTOBS)

C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 10/10/2005   AUTEUR REZETTE C.REZETTE 
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
      IMPLICIT     NONE
      CHARACTER*8  MAILLA
      INTEGER      NBOCC
      INTEGER      NTOBS
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : DYOBSE
C ----------------------------------------------------------------------
C
C SAISIE DU MOT CLE FACTEUR "OBSERVATION"
C VERIFICATION DES DONNEES ET COMPTAGE DES FONCTIONS
C
C IN  MAILLA : NOM DU MAILLAGE
C IN  NBOCC  : NOMBRE D'OCCURENCES DU MOT-CLEF FACTEUR OBSERVATION
C IN  NTOBS  : NOMBRE DE FONCTIONS (= NOMBRE D'OBSERVATIONS PAR INSTANT
C               D'OBSERVATION)
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
      CHARACTER*32       JEXNOM,JEXNUM
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER      N,N1,N2,N3,N4,INO,IMA,IGNO,IOCC,NBN,JGREL,JPRNM
      INTEGER      IBID,IRET,NCHP,NCMP,I,NBNO,NBMA,NBGN,NBMAGL,NUTYEL
      INTEGER      JNOE,JGRN,JMAI,NBPT,J,JREP,JNO,NBMANO,ADRMA,IGREL
      INTEGER      NBEC,JCMP,IEC,IER,ICMP,IAD,NCMPMX,INDIK8
      LOGICAL      CHAMNO,CHAMES,CHAMEV,EXISDG
      CHARACTER*1  KBID
      CHARACTER*8  K8B,MO,NOMGD,NOMNO
      CHARACTER*16 NOCHP(50),NOTYPE,PHENO
      CHARACTER*19 CNXINV,LIGRMO
      CHARACTER*24 GRPNO,NOMNOE,NOMMAI
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      GRPNO  = MAILLA//'.GROUPENO'
      NOMNOE = MAILLA//'.NOMNOE'
      NOMMAI = MAILLA//'.NOMMAI'
C
      NTOBS = 0
C
      DO 10 IOCC = 1 , NBOCC
C
C ------ VERIFICATION DES CHAMPS ---------------------------------------
C
         CALL GETVTX ('OBSERVATION','NOM_CHAM',IOCC,1,0,K8B,N1)
         NCHP = -N1
         CALL GETVTX ('OBSERVATION','NOM_CHAM',IOCC,1,NCHP,NOCHP,N1)
         CHAMNO = .FALSE.
         CHAMES = .FALSE.
         CHAMEV = .FALSE.
         DO 12 I = 1 , NCHP
            IF ( NOCHP(I)(1:4) .EQ. 'DEPL' ) THEN
               CHAMNO = .TRUE.
            ELSEIF ( NOCHP(I)(1:4) .EQ. 'VITE' ) THEN
               CHAMNO = .TRUE.
            ELSEIF ( NOCHP(I)(1:4) .EQ. 'ACCE' ) THEN
               CHAMNO = .TRUE.
            ELSEIF ( NOCHP(I)(1:9) .EQ. 'VALE_CONT' ) THEN
               CHAMNO = .TRUE.
            ELSEIF ( NOCHP(I)(1:9) .EQ. 'SIEF_ELGA' ) THEN
               CHAMES = .TRUE.
            ELSEIF ( NOCHP(I)(1:9) .EQ. 'VARI_ELGA' ) THEN
               CHAMEV = .TRUE.
            ENDIF
 12      CONTINUE
         IF ( CHAMES .AND. CHAMEV ) THEN
            CALL UTMESS('F','DYOBS1','MELANGE DE CHAMPS DE NATURE '//
     &      'DIFFERENTE DANS LE MEME MOT-CLEF FACTEUR OBSERVATION')
         ENDIF
         IF ( CHAMNO .AND. ( CHAMES .OR. CHAMEV ) ) THEN
            CALL UTMESS('F','DYOBS1','MELANGE DE CHAMPS DE NATURE '//
     &      'DIFFERENTE DANS LE MEME MOT-CLEF FACTEUR OBSERVATION')
         ENDIF
C
C ------ VERIFICATION DES COMPOSANTES ----------------------------------
C
         CALL GETVTX ('OBSERVATION','NOM_CMP' , IOCC,1,0, K8B , N2 )
         NCMP = -N2
         CALL WKVECT ('&&DYOBS1.LIST_CMP','V V K8',NCMP,JCMP)
         CALL GETVTX ('OBSERVATION','NOM_CMP',IOCC,1,NCMP,
     &        ZK8(JCMP),IRET)
C
C ------ VERIFICATION DES NOEUDS ET MAILLES ----------------------------
C
         CALL GETVID ('OBSERVATION','NOEUD'   ,IOCC,1,0,K8B ,N1 )
         CALL GETVID ('OBSERVATION','GROUP_NO',IOCC,1,0,K8B ,N2 )
         CALL GETVID ('OBSERVATION','MAILLE'  ,IOCC,1,0,K8B ,N3 )
         CALL GETVIS ('OBSERVATION','POINT'   ,IOCC,1,0,IBID,N4 )
         IF ( N1 .NE. 0 ) THEN
            NBNO = -N1
            CALL WKVECT ('&&DYOBS1.LIST_NOEU','V V K8',NBNO,JNOE)
            CALL GETVID ('OBSERVATION','NOEUD',IOCC,1,NBNO,
     &                   ZK8(JNOE),N1)
            DO 20 INO = 0 , NBNO-1
               CALL JEEXIN ( JEXNOM(NOMNOE,ZK8(JNOE+INO)) , IRET )
               IF ( IRET .EQ. 0 ) THEN
                  CALL UTDEBM('F','DYOBS1','ERREUR DANS LES DONNEES '//
     &                         'D''OBSERVATION')
                  CALL UTIMPK('L','LE NOEUD ',1,ZK8(JNOE+INO))
                  CALL UTIMPK('S',' N''EXISTE PAS DANS ',1,MAILLA)
                  CALL UTFINM()
               ENDIF
 20         CONTINUE
         ENDIF
         IF ( N2 .NE. 0 ) THEN
            NBGN = -N2
            CALL WKVECT ('&&DYOBS1.LIST_GRNO','V V K8',NBGN,JGRN)
            CALL GETVID ('OBSERVATION','GROUP_NO',IOCC,1,NBGN,
     &                   ZK8(JGRN),N2)
            DO 22 IGNO = 0 , NBGN-1
               CALL JEEXIN ( JEXNOM(GRPNO,ZK8(JGRN+IGNO)) , IRET )
               IF ( IRET .EQ. 0 ) THEN
                  CALL UTDEBM('F','DYOBS1','ERREUR DANS LES DONNEES'//
     &                         'D''OBSERVATION')      
                  CALL UTIMPK('L','LE GROUP_NO ',1,ZK8(JGRN+IGNO))
                  CALL UTIMPK('S',' N''EXISTE PAS DANS ',1,MAILLA)
                  CALL UTFINM()
               ENDIF
 22         CONTINUE
         ENDIF
         IF ( N3 .NE. 0 ) THEN
            NBMA = -N3
            CALL WKVECT ('&&DYOBS1.LIST_MAIL','V V K8',NBMA,JMAI)
            CALL GETVID ('OBSERVATION','MAILLE',IOCC,1,NBMA,
     +                   ZK8(JMAI),N3)
            DO 24 IMA = 0 , NBMA-1
               CALL JEEXIN ( JEXNOM(NOMMAI,ZK8(JMAI+IMA)) , IRET )
               IF ( IRET .EQ. 0 ) THEN
                  CALL UTDEBM('F','DYOBS1','ERREUR DANS LES DONNEES '//
     &                         'D''OBSERVATION')
                  CALL UTIMPK('L','LA MAILLE ',1,ZK8(JMAI+IMA))
                  CALL UTIMPK('S',' N''EXISTE PAS DANS ',1,MAILLA)
                  CALL UTFINM()
               ENDIF
 24         CONTINUE
         ENDIF
C
         NBPT = 0
         DO 16 I = 1 , NCHP
            IF ( NOCHP(I)(1:4) .EQ. 'DEPL' .OR.
     &           NOCHP(I)(1:4) .EQ. 'VITE' .OR.
     &           NOCHP(I)(1:4) .EQ. 'ACCE' .OR.
     &           NOCHP(I)(1:9) .EQ. 'VALE_CONT'   ) THEN
               IF ( N1 .NE. 0 ) THEN
                  NBPT = NBPT + NBNO
               ELSEIF ( N2 .NE. 0 ) THEN
                  DO 18 IGNO = 0 , NBGN-1
                     CALL JELIRA (JEXNOM(GRPNO,ZK8(JGRN+IGNO)),
     &                            'LONMAX',NBN,K8B )
                     NBPT = NBPT + NBN
 18               CONTINUE
               ELSE
                  CALL UTDEBM('F','DYOBS1','ERREUR DANS LES DONNEES '//
     &                         'D''OBSERVATION')
                  CALL UTIMPK('L','POUR "NOM_CHAM" ',1,NOCHP(I)(1:4))
                  CALL UTIMPK('S',' IL FAUT RENSEIGNER ',1,'NOEUD')
                  CALL UTIMPK('S',' OU ',1,'GROUP_NO')
                  CALL UTFINM()
               ENDIF
C
            ELSEIF ( NOCHP(I)(1:9) .EQ. 'SIEF_ELGA' .OR.
     +               NOCHP(I)(1:9) .EQ. 'VARI_ELGA' ) THEN
               IF ( N3*N4 .EQ. 0 ) THEN
                  CALL UTDEBM('F','DYOBS1','ERREUR DANS LES DONNEES '//
     &                         'D''OBSERVATION')
                  CALL UTIMPK('L','POUR "NOM_CHAM" ',1,NOCHP(I))
                  CALL UTIMPK('S',' IL FAUT RENSEIGNER ',1,'MAILLE')
                  CALL UTIMPK('S',' ET ',1,'POINT')
                  CALL UTFINM()
               ELSE
                  NBPT = NBPT + ABS( N3*N4 )
               ENDIF
            ENDIF
C
 16      CONTINUE


C ---    VERIFICATION QUE LES NOEUDS SUPPORTENT
C        LES COMPOSANTES FOURNIES
         IF(CHAMNO)THEN
         CALL GETVID(' ','MODELE',0,1,1,MO,IRET)
         CALL DISMOI('F','PHENOMENE',MO,'MODELE',IBID,PHENO,IER)
         CALL DISMOI('F','NOM_GD',PHENO,'PHENOMENE',IBID,NOMGD,IER)
         CALL DISMOI('F','NB_EC',NOMGD,'GRANDEUR',NBEC,KBID,IER)
         CALL JENONU(JEXNOM('&CATA.GD.NOMCMP',NOMGD),IAD)
         CALL JELIRA(JEXNOM('&CATA.GD.NOMCMP',NOMGD),'LONMAX',
     &        NCMPMX,K8B)
         CALL JEVEUO(MO//'.MODELE    .PRNM','L',JPRNM)
         CALL JEVEUO(JEXNOM('&CATA.GD.NOMCMP',NOMGD),'L',IAD)
C
C        GROUP_NO
         IF(N2.NE.0)THEN
           DO 180 IGNO = 1 , NBGN
             CALL JELIRA (JEXNOM(GRPNO,ZK8(JGRN+IGNO-1)),'LONMAX',
     &             NBN,K8B )
             CALL JEVEUO(JEXNOM(GRPNO,ZK8(JGRN+IGNO-1)),'L',JNO)
             DO 181 I=1,NBN
                INO=ZI(JNO+I-1)
                DO 182 J=1,NCMP
                   ICMP=INDIK8(ZK8(IAD),ZK8(JCMP+J-1),1,NCMPMX)
                   IF(.NOT.EXISDG(ZI(JPRNM-1+(INO-1)*NBEC+1),ICMP))THEN
                      CALL JENUNO(JEXNUM(MAILLA//'.NOMNOE',INO),NOMNO)
                      CALL UTDEBM('F','DYOBS1',' ')
                      CALL UTIMPK('L','LE NOEUD',1,NOMNO)
                      CALL UTIMPK('S','NE SUPPORTE PAS LA '//
     &                     'COMPOSANTE',1,ZK8(JCMP+J-1))
                      CALL UTFINM()
                   ENDIF
 182            CONTINUE
 181         CONTINUE
 180      CONTINUE
       ENDIF
C
C      NOEUD
       IF(N1.NE.0)THEN
          DO 183 I=1,NBNO
             CALL JENONU(JEXNOM(MAILLA//'.NOMNOE',
     &            ZK8(JNOE+I-1)),INO)
             DO 184 J=1,NCMP
                ICMP=INDIK8(ZK8(IAD),ZK8(JCMP+J-1),1,NCMPMX)
                IF(.NOT.EXISDG(ZI(JPRNM-1+(INO-1)*NBEC+1),ICMP))THEN
                   CALL UTDEBM('F','DYOBS1',' ')
                   CALL UTIMPK('L','LE NOEUD',1,ZK8(JNOE+I-1))
                   CALL UTIMPK('S','NE SUPPORTE PAS LA '//
     &                  'COMPOSANTE',1,ZK8(JCMP+J-1))
                   CALL UTFINM()
                ENDIF
 184         CONTINUE
 183      CONTINUE
       ENDIF
      ENDIF

      CALL JEDETR ('&&DYOBS1.LIST_CMP')
      IF ( N1 .NE. 0 )  CALL JEDETR ( '&&DYOBS1.LIST_NOEU' )
      IF ( N2 .NE. 0 )  CALL JEDETR ( '&&DYOBS1.LIST_GRNO' )
      IF ( N3 .NE. 0 )  CALL JEDETR ( '&&DYOBS1.LIST_MAIL' )
C
C --- NOMBRE DE FONCTIONS CREEES 
C
      NTOBS  = NTOBS + ( NCHP * NCMP * NBPT )
C
 10   CONTINUE
C
      CALL JEDEMA()
C
      END

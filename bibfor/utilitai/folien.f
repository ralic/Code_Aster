      SUBROUTINE FOLIEN ( NOMOPE, SORTIE, BASE, IER )
      IMPLICIT  NONE
      INTEGER                                   IER
      CHARACTER*1                         BASE
      CHARACTER*16        NOMOPE
      CHARACTER*19                SORTIE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 18/09/2002   AUTEUR CIBHHLV L.VIVAN 
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
C
C  IN : NOMOPE : MOT CLE FACTEUR
C OUT : SORTIE : NOM DE LA FONCTION EN SORTIE
C  IN : BASE   : BASE DE LA CREATION DE LA FONCTION EN SORTIE
C OUT : IER    : CODE RETOUR
C     ------------------------------------------------------------------
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER       N1, I, LPRO, NBCREU, JFCREU, LAMOR, LVAR, LFON, 
     +              NBVAL, JVAL, NBPAR, LPAR, IPARA, NBFR, IRET, IF, 
     +              JPROS, JPARS, JVALS, JABSC, JORDO, NBFLIS, LONT,
     +              NBFONC, LNOMF, LONUTI
      LOGICAL       NAPP, FONC, ECHLIF, ECHAMF
      REAL*8        FMIN, FMAX, F1, F2, LISS, LARG, VALTG, VALTD,
     +              AMOR, R8B, AMOECH, TOLE, VALINT(2)
      CHARACTER*8   K8B, TYPFON, ELARG, ECHFR, NOPAR1, NOPAR2, NOPAR3,
     +              NPAINT(2)
      CHARACTER*19  NOMFON, LFECH
      CHARACTER*24  PROL, VALE, PARA, NOMF
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      IER = 0
      TOLE = 1.0D-10
C
C     ------------------------------------------------------------------
C
C                  --- RECUPERATION DES DONNEES ---
C
C     ------------------------------------------------------------------
C
C --- RECUPERATION DE L'ACCELEROGRAMME ---
C
      CALL GETVID ( NOMOPE, 'FONCTION', 1,1,1, NOMFON, N1 )
      PROL = NOMFON//'.PROL'
      VALE = NOMFON//'.VALE'
      CALL JEVEUO ( PROL, 'L', LPRO )
      TYPFON = ZK8(LPRO)
      FONC = .FALSE.
      NAPP = .FALSE.
C
      IF (TYPFON.EQ.'FONCTION') THEN
         FONC = .TRUE.
         NBFONC = 1
      ELSEIF (TYPFON.EQ.'NAPPE   ') THEN
         NAPP = .TRUE.
         NOPAR1 = ZK8(LPRO+2)
         NOPAR2 = ZK8(LPRO+3)
         NOPAR3 = ZK8(LPRO+5)
         PARA = NOMFON//'.PARA'
         CALL JELIRA ( PARA, 'LONUTI', NBPAR, K8B )
         CALL JEVEUO ( PARA, 'L', LPAR )
         CALL GETVR8 ( NOMOPE, 'AMOR', 1,1,1, AMOR, LAMOR )
         IF ( LAMOR .EQ. 0 ) THEN
            NBFONC = NBPAR
         ELSE
            NBFONC = 1
            DO 10 IPARA = 1,NBPAR
               IF ( ABS(ZR(LPAR+IPARA-1)-AMOR) .LE. TOLE ) GOTO 12
 10         CONTINUE 
            CALL UTDEBM('F','FOLIEN','ERREUR DONNEE:')
            CALL UTIMPK('L','ON N''A PAS PU EXTRAIRE DE LA NAPPE ',
     +                                                1, NOMFON(1:8) )
            CALL UTIMPR('S',' LA FONCTION POUR L''AMORTISSEMENT ',
     +                                                1, AMOR )
            CALL UTFINM()
 12         CONTINUE 
         ENDIF
      ELSE
         CALL UTMESS('F','FOLIEN','TYPE FONTION NON TRAITEE '//TYPFON)
      ENDIF
C
C --- PLAGE DE FREQUENCE DU SPECTRE LISSE ---
C
      CALL GETVR8 ( NOMOPE, 'FREQ_MIN', 1,1,1, FMIN, N1 )
      CALL GETVR8 ( NOMOPE, 'FREQ_MAX', 1,1,1, FMAX, N1 )
C
C --- LISTE DES FREQUENCES DE CREUX A GARDER ---
C
      CALL GETVR8 ( NOMOPE, 'FREQ_CREUX', 1,1,0, R8B, NBCREU)
      IF ( NBCREU .NE. 0 ) THEN
         NBCREU = - NBCREU
         CALL WKVECT ('&&FOLIEN.FREQ_CREU', 'V V R', NBCREU, JFCREU )
         CALL GETVR8 ( NOMOPE,'FREQ_CREUX', 1,1,NBCREU, ZR(JFCREU), N1)
      ENDIF
C
C --- ELARGISSEMENT EN FREQUENCE DU SPECTRE
C 
      CALL GETVTX ( NOMOPE, 'ELARG', 1,1,1, ELARG, N1 )
C
      F1 = FMIN
      F2 = FMAX
      IF ( ELARG .EQ. 'LOCAL' ) THEN
C
C ------ RECUPERATION DES FREQUENCES 1 ET 2 ---
C
         CALL GETVR8 ( NOMOPE, 'FREQ_1', 1,1,1, F1, N1 )
         CALL GETVR8 ( NOMOPE, 'FREQ_2', 1,1,1, F2, N1 )
      ENDIF
C
      CALL GETVR8 ( NOMOPE, 'GAUCHE',1,1,1, VALTG, N1 )
      CALL GETVR8 ( NOMOPE, 'DROITE',1,1,1, VALTD, N1 )
C
C --- CRITERE POUR L'ELIMINATION DES POINTS DE LISSAGE ---
C
      CALL GETVR8 ( NOMOPE, 'TOLE_LISS', 1,1,1, LISS, N1 )
C
C --- SEUIL EN LARGEUR POUR SELECTIONNER LES PLATEAUX ---
C
      CALL GETVR8 ( NOMOPE, 'LARG_PLAT', 1,1,1, LARG, N1 )
C
C --- ECHANTILLONNAGE EN FREQUENCE DU SPECTRE LISSE---
C
      ECHLIF = .FALSE.
      ECHAMF = .FALSE.
      CALL GETVTX ( NOMOPE, 'ECH_FREQ_REF', 1,1,1, ECHFR, N1 )
      IF ( ECHFR .EQ. 'OUI' ) THEN
         CALL GETVID ( NOMOPE, 'LIST_FREQ', 1,1,1, LFECH, N1 )
         IF ( N1 .NE. 0 ) THEN
            ECHLIF = .TRUE.
         ELSE
            CALL GETVR8 ( NOMOPE, 'AMOR_ECH', 1,1,1, AMOECH, N1 )
            ECHAMF = .TRUE.
         ENDIF
      ENDIF
      IF ( ECHAMF .AND. (.NOT. NAPP) ) THEN
         CALL UTDEBM('F','FOLIEN','ERREUR DONNEE:')
         CALL UTIMPK('L','IL FAUT FOURNIR UNE NAPPE POUR POUVOIR '//
     +                        ' ECHANTILLONNER SELON ', 1, 'AMOR_ECH' )
         CALL UTFINM()
      ENDIF
C
C     ------------------------------------------------------------------
C
C                    --- LISSSAGE ENVELOPPE ---
C
C     ------------------------------------------------------------------
C
      CALL WKVECT ( '&&FOLIEN.NOMFON', 'V V K24', NBFONC, LNOMF )
      DO 100 IF = 1, NBFONC
         ZK24(LNOMF+IF-1)(1:10) = '&&FOLIEN.F'
         CALL CODENT ( IF, 'D0', ZK24(LNOMF+IF-1)(11:19) )
         ZK24(LNOMF+IF-1)(20:24) = '     '
 100  CONTINUE
C
      LONT = 0
C
      DO 110 IF = 1 , NBFONC
C
         NOMF = ZK24(LNOMF+IF-1)
C
         IF ( NAPP ) THEN 
            IF ( LAMOR .EQ. 0 ) THEN
               CALL JELIRA ( JEXNUM(VALE,IF), 'LONUTI', NBVAL, K8B )
               CALL JEVEUO ( JEXNUM(VALE,IF), 'L', JVAL )
            ELSE
               CALL JELIRA ( JEXNUM(VALE,IPARA), 'LONUTI', NBVAL, K8B )
               CALL JEVEUO ( JEXNUM(VALE,IPARA), 'L', JVAL )
            ENDIF
         ELSEIF (FONC) THEN
            CALL JELIRA ( VALE, 'LONMAX', NBVAL, K8B )
            CALL JEVEUO ( VALE, 'L', JVAL )
         ENDIF
         NBFR = NBVAL / 2
C
         CALL WKVECT ( '&&FOLIEN.ABSC', 'V V R8', NBFR, JABSC )
         CALL WKVECT ( '&&FOLIEN.ORDO', 'V V R8', NBFR, JORDO )
C
         CALL FOLIE1 ( NBFR, ZR(JVAL), ZR(JVAL+NBFR), NBCREU, 
     +                 ZR(JFCREU), ELARG, VALTG, VALTD, F1, F2, LISS,
     +                 LARG, NBFLIS, ZR(JABSC), ZR(JORDO) )
C
         LONT = LONT + NBFLIS
C
         CALL WKVECT ( NOMF, 'V V R', 2*NBFLIS, JVALS )
         DO 112 I = 1 , NBFLIS
            ZR(JVALS       +I-1) = ZR(JABSC+I-1)
            ZR(JVALS+NBFLIS+I-1) = ZR(JORDO+I-1)
 112     CONTINUE      
C
         CALL JEDETR ( '&&FOLIEN.ABSC' )
         CALL JEDETR ( '&&FOLIEN.ORDO' )
C
 110  CONTINUE
C
C     ------------------------------------------------------------------
C
C                   --- CREATION DE LA SORTIE ---
C
C     ------------------------------------------------------------------
C
C --- AFFECTATION DU .PROL ---
C
      PROL = SORTIE//'.PROL'
C
      IF ( FONC ) THEN
         CALL WKVECT ( PROL, BASE//' V K8', 5, JPROS )
         ZK8(JPROS  ) = 'FONCTION'
         ZK8(JPROS+1) = 'LOG LOG '
         ZK8(JPROS+2) = 'FREQ    '
         ZK8(JPROS+3) = 'ACCE    '
         ZK8(JPROS+4) = 'EE      '
         CALL JELIBE ( PROL )
C
      ELSEIF ( NAPP ) THEN
         CALL WKVECT ( PROL, BASE//' V K8', 6+2*NBFONC, JPROS )
         ZK8(JPROS  ) = 'NAPPE   '
         ZK8(JPROS+1) = 'LOG LOG '
         ZK8(JPROS+2) = NOPAR1
         ZK8(JPROS+3) = NOPAR2
         ZK8(JPROS+4) = 'EE      '
         ZK8(JPROS+5) = NOPAR3
         DO 20 IF = 1 , NBFONC
            ZK8(JPROS+5+2*IF-1) = 'LOG LOG '
            ZK8(JPROS+5+2*IF  ) = 'EE      '
 20      CONTINUE 
         CALL JELIBE ( PROL )
C
         PARA = SORTIE//'.PARA'
         CALL WKVECT ( PARA, BASE//' V R', NBFONC, JPARS )
         IF ( LAMOR .NE. 0 ) THEN
            ZR(JPARS) = AMOR
         ELSE
            DO 22 IF = 1 , NBFONC
               ZR(JPARS+IF-1) = ZR(LPAR+IF-1)
 22        CONTINUE
         ENDIF
         CALL JELIBE ( PARA )
      ENDIF
C
C --- AFFECTATION DU .VALE ---
C
      VALE = SORTIE//'.VALE'
C
      IF ( FONC ) THEN
         CALL WKVECT ( VALE, BASE//' V R', 2*LONT, JVALS )
         DO 30 I = 1 , LONT
            ZR(JVALS     +I-1) = ZR(JABSC+I-1)
            ZR(JVALS+LONT+I-1) = ZR(JORDO+I-1)
 30      CONTINUE      
         CALL JELIBE ( VALE )
      ELSEIF ( NAPP ) THEN
         CALL JECREC(VALE,BASE//' V R','NU','CONTIG','VARIABLE',NBFONC)
         CALL JEECRA ( VALE, 'LONT', 2*LONT, ' ' )
         DO 32 IF = 1 , NBFONC
            CALL JECROC ( JEXNUM(VALE,IF) )
            CALL JELIRA ( ZK24(LNOMF+IF-1), 'LONUTI', LONUTI, K8B )
            CALL JEECRA ( JEXNUM(VALE,IF) , 'LONMAX', LONUTI, ' ' )
            CALL JEECRA ( JEXNUM(VALE,IF) , 'LONUTI', LONUTI, ' ' )
            CALL JEVEUO ( JEXNUM(VALE,IF), 'E', JVALS )
            CALL JEVEUO ( ZK24(LNOMF+IF-1), 'L', JVAL )
            DO 34 I = 1 , LONUTI
               ZR(JVALS+I-1) = ZR(JVAL+I-1)
 34        CONTINUE
 32     CONTINUE
      ENDIF
C
C     ------------------------------------------------------------------
C
C                  --- ECHANTILLONAGE FREQUENCE ---
C
C     ------------------------------------------------------------------
C
      IF ( ECHAMF ) THEN
         PARA = SORTIE//'.PARA'
         CALL JELIRA ( PARA, 'LONUTI', NBPAR, K8B )
         CALL JEVEUO ( PARA, 'L', LPAR )
         DO 70 IPARA = 1,NBPAR
            IF ( ABS(ZR(LPAR+IPARA-1)-AMOECH) .LE. TOLE ) GOTO 72
 70      CONTINUE 
         CALL UTDEBM('A','FOLIEN','ERREUR DONNEE:')
         CALL UTIMPK('L','ON N''A PAS PU EXTRAIRE DE LA NAPPE ',
     +                                                1, SORTIE(1:8) )
         CALL UTIMPR('S',' LA FONCTION POUR L''AMORTISSEMENT ',
     +                                                1, AMOECH )
         CALL UTIMPR('L','ON N''A PAS ECHANTILLONNER.', 0, AMOECH )
         CALL UTFINM()
         GOTO 9999
 72      CONTINUE 
C
         VALE = SORTIE//'.VALE'
         CALL JELIRA ( JEXNUM(VALE,IPARA), 'LONUTI', NBVAL, K8B )
         CALL JEVEUO ( JEXNUM(VALE,IPARA), 'L', JVAL )
         NBVAL = NBVAL / 2
         LFECH = '&&FOLIEN.ECH_FREQ  '
         CALL WKVECT ( LFECH//'.VALE', 'V V R', NBVAL, LVAR )
         DO 74 I = 1, NBVAL
            ZR(LVAR+I-1) = ZR(JVAL+I-1)
 74      CONTINUE
         ECHLIF = .TRUE.
      ENDIF
C
      IF ( ECHLIF ) THEN
         CALL JELIRA ( LFECH//'.VALE', 'LONMAX', NBVAL, K8B )
         CALL JEVEUO ( LFECH//'.VALE', 'L', JVAL )
         IF ( FONC ) THEN
            CALL WKVECT ( '&&FOLIEN.VALE_FR', 'V V R', 2*NBVAL, LVAR )
            LFON = LVAR + NBVAL
            DO 80 I = 1, NBVAL
               ZR(LVAR+I-1) = ZR(JVAL+I-1)
               CALL FOINTE ( 'F ', SORTIE, 1, 'FREQ', ZR(LVAR+I-1),
     +                                             ZR(LFON+I-1),IRET)
 80         CONTINUE
            VALE = SORTIE//'.VALE'
            CALL JEDETR ( VALE )
            CALL JEDUPO ( '&&FOLIEN.VALE_FR', 'G', VALE, .FALSE. )
            CALL JEDETR ( '&&FOLIEN.VALE_FR' )
C
         ELSEIF ( NAPP ) THEN
            NPAINT(1) = 'AMOR'
            NPAINT(2) = 'FREQ'
            PARA = SORTIE//'.PARA'
            CALL JELIRA ( PARA, 'LONUTI', NBPAR, K8B )
            CALL JEVEUO ( PARA, 'L', LPAR )
            LONT = NBPAR * NBVAL
            CALL WKVECT ( '&&FOLIEN.VALE_INT', 'V V R', LONT, LVAR )
            DO 82 IF = 1 , NBPAR
               VALINT(1) = ZR(LPAR+IF-1)
               DO 84 I = 1 , NBVAL
                  VALINT(2) = ZR(JVAL+I-1)
                  CALL FOINTE ( 'F ', SORTIE, 2, NPAINT, VALINT,
     +                                ZR(LVAR-1+(IF-1)*NBVAL+I), IRET )
 84            CONTINUE
 82         CONTINUE
C
            VALE = SORTIE//'.VALE'
            CALL JEDETR ( VALE )
            LONT = 2 * NBPAR * NBVAL
          CALL JECREC(VALE,BASE//' V R','NU','CONTIG','VARIABLE',NBPAR)
            CALL JEECRA ( VALE, 'LONT', LONT, ' ' )
            DO 86 IF = 1 , NBPAR
               CALL JECROC ( JEXNUM(VALE,IF) )
               CALL JEECRA ( JEXNUM(VALE,IF) , 'LONMAX', 2*NBVAL, ' ' )
               CALL JEECRA ( JEXNUM(VALE,IF) , 'LONUTI', 2*NBVAL, ' ' )
               CALL JEVEUO ( JEXNUM(VALE,IF), 'E', JVALS )
               LFON = JVALS + NBVAL
               DO 88 I = 1 , NBVAL
                  ZR(JVALS+I-1) = ZR(JVAL+I-1)
                  ZR(LFON +I-1) = ZR(LVAR-1+(IF-1)*NBVAL+I)
 88            CONTINUE
 86         CONTINUE
            CALL JEDETR ( '&&FOLIEN.VALE_INT' )
         ENDIF
      ENDIF
C
 9999 CONTINUE
C
      CALL JEDETR ( '&&FOLIEN.ELIME'     )
      CALL JEDETR ( '&&FOLIEN.LISS'      )
      CALL JEDETR ( '&&FOLIEN.MAX'       )
      CALL JEDETR ( '&&FOLIEN.ENVELOP'   )
      CALL JEDETR ( '&&FOLIEN.FREQ_PLAT' )
      CALL JEDETR ( '&&FOLIEN.ELARM'     )
      CALL JEDETR ( '&&FOLIEN.ELARP'     )
      IF ( ECHAMF )  CALL JEDETR ( LFECH )
      DO 90 I = 1, NBFONC
         NOMF = ZK24(LNOMF+I-1)
         CALL JEDETR ( NOMF )
 90   CONTINUE
      CALL JEDETR ( '&&FOLIEN.NOMFON' )
C
      CALL JEDEMA()
      END

      SUBROUTINE OP0083 (IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER            IER
C     ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 16/06/2004   AUTEUR DURAND C.DURAND 
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
C     LECTURE D'UNE FONCTION SUR FICHIER EXTERNE
C     ----------------------------------------------------------------
C
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
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CHARACTER*32    JEXNUM
      CHARACTER*8     LISTR
      CHARACTER*16    CONCEP, NOMCMD
      CHARACTER*24    PROL, VALE, PARA
C     ----------------------------------------------------------------
      INTEGER         ICLASS,IVAL
      REAL*8          RVAL
      CHARACTER*80    CVAL
C
      CHARACTER*16    ORIGIN, NAT, ORG, FMT
      CHARACTER*16    FVA, NOMRES, FFO
      CHARACTER*19    NOMFON
      CHARACTER*24    NOMPAR,NOMVAR
      REAL*8          PAS , NORME
      PARAMETER       (NBMOT=23, IERMAX=25)
      CHARACTER*8     MOTCLE(NBMOT)
      CHARACTER*2     PROLGD, PROFGD
      CHARACTER*4     INTERP(2), INTERF(2)
C     ----------------------------------------------------------------
      DATA  LRECL/80/
      DATA  MOTCLE
     +   /'DATE    ', 'AUTEUR  ', 'ORIGINE ', 'NATURE  ', 'NPS     ',
     +    'ORG     ', 'IDENT   ', 'UVA     ', 'FVA     ', 'NOM_RESU',
     +    'UFO     ', 'FFO     ', 'NORME   ', 'VALEUR  ', 'FINSF   ',
     +    'PAS     ', 'NOM_PARA', 'NOM     ', 'NB_PARA ', 'NOM_VAR ',
     +    'NOMFON  ', 'NOMFON_X', 'NOMFON_Y'/
C
C
      CALL JEMARQ()
C
C --- RECUPERATION DU NIVEAU D'IMPRESSION
      IER = 0
      CALL INFMAJ
      CALL INFNIV(IFM,NIV)
C
C     --- INITIALISATION DIVERS ---
      PAS    = 0.D0
      ORG    = ' '
      FVA    = ' '
      FFO    = ' '
      NORME  = 1.D0
      NOMPAR = ' '
      NOMVAR = ' '
      NOMRES = ' '
      NBPARA = 0
C
      CALL GETRES(NOMFON,CONCEP,NOMCMD)
      PROL = NOMFON // '.PROL'
      VALE = NOMFON // '.VALE'
      PARA = NOMFON // '.PARA'
C
      CALL GETVTX(' ','INTERPOL'   ,0,1,2,INTERP,N3)
      IF ( N3 .EQ. 1 ) INTERP(2) = INTERP(1)
      CALL GETVTX(' ','PROL_GAUCHE',0,1,1,PROLGD(1:1),N4)
      CALL GETVTX(' ','PROL_DROITE',0,1,1,PROLGD(2:2),N5)
C
      CALL GETVTX(' ','INTERPOL_FONC'   ,0,1,2,INTERF,N13)
      IF ( N13 .EQ. 1 ) INTERF(2) = INTERF(1)
      CALL GETVTX(' ','PROL_GAUCHE_FONC',0,1,1,PROFGD(1:1),N14)
      CALL GETVTX(' ','PROL_DROITE_FONC',0,1,1,PROFGD(2:2),N15)
C
      CALL GETVIS(' ','UNITE',1,1,1,IFSIG,L)
C
C     --- INITIALISATION DE LA LECTURE ---
      CALL LXUNIT(IFSIG,LRECL,IFM,'SIGMED1')
      CALL LXPOSI(1,1,0)
C
C     --- BON SOUS CHAPITRE ---
      CALL LXLIRE(ICLASS,IVAL,RVAL,CVAL)
      IF ( ICLASS .NE.  3 ) THEN
         IER = IER + 1
      ELSEIF ( CVAL(1:IVAL) .NE.'FONCTION' ) THEN
         IER = IER + 1
      ENDIF
      IF (IER .NE.0) THEN
         CALL UTMESS('E',NOMCMD//'(ERREUR 00)',
     +                           'ENTETE DE FICHIER ERRONEE '//
     +                           'ON ATTENDAIT LE MOT CLE "FONCTION".')
         GOTO 2
      ENDIF
C
C     --- LECTURE ---
    1 CONTINUE
         CALL LXLIRE(ICLASS,IVAL,RVAL,CVAL)
         IF ( ICLASS .EQ. -1 ) THEN
            IER = IER + 1
            CALL UTMESS('E',NOMCMD//'(ERREUR 01)',
     +                              'FIN PREMATUREE DU FICHIER.')
            GOTO 2
         ELSEIF ( ICLASS .NE. 3  ) THEN
            IER = IER + 1
            IF (IER .GT. IERMAX ) GOTO 999
            CALL UTMESS('E',NOMCMD//'(ERREUR 02)',
     +                      'ON ATTENDAIT UN MOT CLE.')
            GOTO 1
         ELSE
            CALL UTREMT(CVAL(1:IVAL),MOTCLE,NBMOT,IPLACE)
            IF(IPLACE.EQ.21 .OR.IPLACE.EQ.22 .OR.IPLACE.EQ.23)THEN
                  CALL LXLIRE(ICLASS,IVAL,RVAL,CVAL)  
                  CALL LXLIRE(ICLASS,IVAL,RVAL,CVAL)  
                  GOTO 1
            ENDIF
            IF ( IPLACE .EQ. 0 ) THEN
               IER = IER + 1
               IF (IER .GT. IERMAX ) GOTO 999
               CALL UTMESS('E',NOMCMD//'(ERREUR 03)', '"'//CVAL(1:IVAL)
     +                            //'" N''EST PAS UN MOT CLE RECONNU.')
               GOTO 1
            ENDIF
         ENDIF
C
C        --- CAS DE FINSF ---
         IF ( IPLACE .EQ. 15 ) GOTO 150
C
         CALL LXLIRE(ICLASS,IVAL,RVAL,CVAL)
         IF ( CVAL(1:1) .NE. '=' ) THEN
            IER = IER + 1
            IF (IER .GT. IERMAX ) GOTO 999
            CALL UTMESS('E',NOMCMD//'(ERREUR 04)',
     +                     'APRES UN MOT CLE ON ATTENDAIT "=".')
C
         ELSE
C           --- LIRE L'ITEM SAUF SI MOT CLE = VALEUR ---
            IF (IPLACE .NE. 14) CALL LXLIRE(ICLASS,IVAL,RVAL,CVAL)
C
         ENDIF
C
         GOTO ( 10,  20,  30,  40,  50,  60,  70,  80,  90, 100,
     +         110, 120, 130, 140, 150, 160, 170, 180, 190, 200 ) IPLACE
C
C
C
 10      CONTINUE
C        --- DATE ---
         IF (ICLASS.EQ.4) THEN
         ELSE
            IER = IER + 1
            IF (IER .GT. IERMAX ) GOTO 999
            CALL UTMESS('E',NOMCMD//'(ERREUR 05)','L''ARGUMENT DU '//
     +                  'MOT CLE "DATE" DOIT ETRE UN "TEXTE".')
         ENDIF
C
      GOTO 1
C
C
   20    CONTINUE
C        --- AUTEUR ---
         IF (ICLASS.EQ.3 .OR.ICLASS.EQ.4) THEN
         ELSE
            IER = IER + 1
            IF (IER .GT. IERMAX ) GOTO 999
            CALL UTMESS('E',NOMCMD//'(ERREUR 06)','L''ARGUMENT DU '//
     +                  'MOT CLE "AUTEUR" DOIT ETRE UN "TEXTE" OU '//
     +                  '"IDENTIFICATEUR".')
         ENDIF
C
      GOTO 1
C
C
   30    CONTINUE
C        --- ORIGINE ---
         IF (ICLASS.EQ.3 .OR.ICLASS.EQ.4) THEN
         ELSE
            IER = IER + 1
            IF (IER .GT. IERMAX ) GOTO 999
            CALL UTMESS('E',NOMCMD//'(ERREUR 07)','L''ARGUMENT DU '//
     +                  'MOT CLE "ORIGINE" DOIT ETRE UN "TEXTE" OU '//
     +                  'UN "IDENTIFICATEUR".')
         ENDIF
C
      GOTO 1
C
C
   40    CONTINUE
C        --- NATURE: FONCTION / NAPPE ---
         IF (ICLASS.EQ.3 .OR.ICLASS.EQ.4) THEN
            NAT    = CVAL(1:IVAL)
            IF ( NAT .NE. 'FONCTION' .AND. NAT.NE.'NAPPE') THEN
               CALL UTMESS('E',NOMCMD//' (ERREUR 33)','"'//CVAL(1:IVAL)
     +               //'" : ARGUMENT INCONNU POUR LE MOT CLE "NATURE". '
     +               //'LES ARGUMENTS RECONNUS SONT "FONCTION" '
     +                                                 //'ET "NAPPE".')
               NAT = 'FONCTION'
               IER = IER + 1
            ENDIF
         ELSE
            IER = IER + 1
            IF (IER .GT. IERMAX ) GOTO 999
            CALL UTMESS('E',NOMCMD//' (ERREUR 08)','L''ARGUMENT DU '//
     +                  'MOT CLE "NATURE" DOIT ETRE UN "TEXTE" OU '//
     +                  'UN "IDENTIFICATEUR".')
         ENDIF
C
      GOTO 1
C
   50    CONTINUE
C        --- NPS  ---
         IF (ICLASS.EQ.1) THEN
            NPS = IVAL
            IF ( NPS .LT. 1 ) THEN
               IER = IER + 1
               IF (IER .GT. IERMAX ) GOTO 999
               CALL UTMESS('E',NOMCMD//'(ERREUR 09)',
     +                    'L''ARGUMENT DU MOT CLE '
     +                  //'"NPS" DOIT ETRE UN "ENTIER" STRICTEMENT '
     +                  //'POSITIF.')
             ENDIF
         ELSE
            IER = IER + 1
            IF (IER .GT. IERMAX ) GOTO 999
            CALL UTMESS('E',NOMCMD//' (ERREUR 10)','L''ARGUMENT DU MOT'
     +                  //' CLE "NPS" DOIT ETRE UN "ENTIER" STRICTEMENT'
     +                  //' POSITIF.')
         ENDIF
      GOTO 1
C
C
   60    CONTINUE
C        --- ORG  ---
         IF (ICLASS.EQ.3 .OR.ICLASS.EQ.4) THEN
            ORG = CVAL(1:IVAL)
         ELSE
            IER = IER + 1
            IF (IER .GT. IERMAX ) GOTO 999
            CALL UTMESS('E',NOMCMD//'(ERREUR 11)','L''ARGUMENT DU '//
     +                  'MOT CLE "ORG" DOIT ETRE UN "TEXTE" OU '//
     +                  '"IDENTIFICATEUR".')
         ENDIF
      GOTO 1
C
C
   70    CONTINUE
C
C
   80    CONTINUE
C        --- UVA  ---
         IF (ICLASS.EQ.3 .OR.ICLASS.EQ.4) THEN
         ELSE
            IER = IER + 1
            IF (IER .GT. IERMAX ) GOTO 999
            CALL UTMESS('E',NOMCMD//'(ERREUR 13)','L''ARGUMENT DU '//
     +                  'MOT CLE "UVA" DOIT ETRE UN "TEXTE" OU '//
     +                  '"IDENTIFICATEUR".')
         ENDIF
      GOTO 1
C
C
   90    CONTINUE
C        --- FVA  ---
         IF (ICLASS.EQ.3 .OR.ICLASS.EQ.4) THEN
            FVA   = CVAL(1:IVAL)
         ELSE
            IER = IER + 1
            IF (IER .GT. IERMAX ) GOTO 999
            CALL UTMESS('E',NOMCMD//'(ERREUR 14)','L''ARGUMENT DU '//
     +                  'MOT CLE "FVA" DOIT ETRE UN "TEXTE" OU '//
     +                  '"IDENTIFICATEUR".')
         ENDIF
      GOTO 1
C
C
  100    CONTINUE
C        --- NOM_RESU ---
         IF (ICLASS.EQ.3 .OR.ICLASS.EQ.4) THEN
            NOMRES = CVAL(1:IVAL)
         ELSE
            IER = IER + 1
            IF (IER .GT. IERMAX ) GOTO 999
            CALL UTMESS('E',NOMCMD//'(ERREUR 15)','L''ARGUMENT DU '//
     +                  'MOT CLE "NOM_RESU" DOIT ETRE UN "TEXTE" OU '//
     +                  '"IDENTIFICATEUR".')
         ENDIF
      GOTO 1
C
C
  110    CONTINUE
C        --- UFO  ---
         IF (ICLASS.EQ.3 .OR.ICLASS.EQ.4) THEN
         ELSE
            IER = IER + 1
            IF (IER .GT. IERMAX ) GOTO 999
            CALL UTMESS('E',NOMCMD//'(ERREUR 16)','L''ARGUMENT DU '//
     +                  'MOT CLE "UFO" DOIT ETRE UN "TEXTE" OU '//
     +                  '"IDENTIFICATEUR".')
            IF (IER .GT. IERMAX ) GOTO 999
         ENDIF
      GOTO 1
C
C
  120    CONTINUE
C        --- FFO  ---
         IF (ICLASS.EQ.3 .OR.ICLASS.EQ.4) THEN
            FFO   = CVAL(1:IVAL)
            IF ( FFO(1:1) .NE. 'R' ) THEN
              IER = IER + 1
              IF (IER .GT. IERMAX ) GOTO 999
              CALL UTMESS('E',NOMCMD//'(ERREUR 41)','FORMAT FFO :'//
     +                  ' SEUL RII EST PROGRAMME ACTUELLEMENT.')
            ENDIF
         ELSE
            IER = IER + 1
            CALL UTMESS('E',NOMCMD//'(ERREUR 17)','L''ARGUMENT DU '//
     +                  'MOT CLE "FFO" DOIT ETRE UN "TEXTE" OU '//
     +                  '"IDENTIFICATEUR".')
            IF (IER .GT. IERMAX ) GOTO 999
         ENDIF
      GOTO 1
C
C
  130    CONTINUE
C        --- NORME ---
         IF (ICLASS.EQ.2) THEN
            NORME = RVAL
            IF ( NORME .EQ. 0.D0 ) THEN
               IER = IER + 1
               CALL UTDEBM('E',NOMCMD//'(ERREUR 28)','L''ARGUMENT '//
     +                  'DU MOT CLE "NORME" EST INVALIDE.')
               CALL UTIMPR('L','LA VALEUR INVALIDE EST ',1,NORME)
               CALL UTFINM()
               IF (IER .GT. IERMAX ) GOTO 999
            ENDIF
         ELSE
            IER = IER + 1
            CALL UTMESS('E',NOMCMD//'(ERREUR 18)','L''ARGUMENT DU '//
     +                  'MOT CLE "NORME" DOIT ETRE UN "REEL".')
            IF (IER .GT. IERMAX ) GOTO 999
         ENDIF
      GOTO 1
C
C
  140    CONTINUE
C        --- VALEUR ---
         IF ( NAT.EQ.'NAPPE' ) THEN
C            --- SIGNAL SPECTRE D'OSCILLATEUR SISPOUX ---
            IF(NPS*NBPARA .EQ. 0) CALL UTMESS('E',NOMCMD//'(ERREUR 29)',
     +                  'LE NOMBRE DE PARAMETRES "NB_PARA" OU '//
     +                  'LE NOMBRE DE POINTS DES FONCTIONS "NPS"  '//
     +                  'N''EST PAS DEFINI.')
            CALL WKVECT('&&OP0083.NOMBRE.DE.PT','V V R',NPS, LVAR )
            CALL WKVECT('&&OP0083.SPECTRE   ','V V R',NBPARA*NPS,LSPEC)
            CALL WKVECT(PARA,'G V R',NBPARA, LPARA)
            CALL FOLENA(NOMCMD,NPS,ZR(LVAR),NBPARA,ZR(LPARA),
     +                                                   ZR(LSPEC),IER)
C
C           --- CREATION DE LA NAPPE RESULTAT ---
C           1/ LE .PARA ... C'EST FAIT
C
C           2/ LE .PROL
            CALL WKVECT(PROL,'G V K16',6+2*NBPARA,LPROL)
            ZK16(LPROL)   = 'NAPPE   '
            IF ( N13 .EQ. 0 ) THEN
               ZK16(LPROL+1) = 'LIN LIN '
            ELSE
               ZK16(LPROL+1) = INTERF(1)//INTERF(2)
            ENDIF
            ZK16(LPROL+2) = NOMPAR
            ZK16(LPROL+3) = NOMRES
            IF ( N14 .EQ. 0 .AND. N15 .EQ. 0 ) THEN
               ZK16(LPROL+4) = 'EE'
            ELSE
               ZK16(LPROL+4) = PROFGD
            ENDIF
            ZK16(LPROL+5) = NOMVAR
C
C           3/ LE .VALE
            CALL JECREC(VALE,'G V R','NU','CONTIG','VARIABLE',NBPARA)
            CALL JEECRA(VALE,'LONT',2*NBPARA*NPS,' ')
C
C           4/ REMPLISSAGE DU .PROL DE LA NAPPE
            DO 299 IPARA =1,NBPARA
              IF ( N3 .EQ. 0 ) THEN
                ZK16(LPROL+5+2*IPARA-1) = 'LIN LIN '
              ELSE
                ZK16(LPROL+5+2*IPARA-1) = INTERP(1)//INTERP(2)
              ENDIF
              IF ( N4 .EQ. 0 .AND. N5 .EQ. 0 ) THEN
                ZK16(LPROL+5+2*IPARA  ) = 'EE'
              ELSE
                ZK16(LPROL+5+2*IPARA  ) = PROLGD
              ENDIF
  299       CONTINUE
C
C           5/ REMPLISSGE DU VALE
            DO 300 IPARA =1,NBPARA
C
               CALL JECROC(JEXNUM(VALE,IPARA))
               CALL JEECRA(JEXNUM(VALE,IPARA) ,'LONMAX',2*NPS,' ')
               CALL JEECRA(JEXNUM(VALE,IPARA) ,'LONUTI',2*NPS,' ')
               CALL JEVEUO(JEXNUM(VALE,IPARA),'E',LVARF)
               LFONF = LVARF + NPS
               DO 310 IPTS=1,NPS
                  ZR(LVARF+IPTS-1) = ZR(LVAR +IPTS-1)
                  ZR(LFONF+IPTS-1) = ZR(LSPEC+IPTS-1)
 310          CONTINUE
              LSPEC = LSPEC + NPS
 300        CONTINUE
            CALL JEDETR('&&OP0083.NOMBRE.DE.PT')
            CALL JEDETR('&&OP0083.SPECTRE     ')
            IF (IFM .GT. 0 ) THEN
              CALL FOEC2N(IFM,ZK16(LPROL),ZR(LPARA),VALE,NBPARA,2)
            ENDIF
            IF ( NORME .NE. 0 ) THEN
               DO 350 IPARA = 1, NBPARA
                  CALL JEVEUO(JEXNUM(VALE,IPARA),'E',LVAR)
                  LFON = LVAR + NPS
                  DO 349 IPS = 0, NPS-1
                     ZR(LFON+IPS) = ZR(LFON+IPS) * NORME
  349             CONTINUE
  350          CONTINUE
            ENDIF
C
         ELSEIF( NAT .EQ. 'FONCTION' ) THEN
            CALL WKVECT(VALE,'G V R',2*NPS, LVAR )
            CALL JEECRA(VALE,'LONUTI',2*NPS,' ')
            LFON = LVAR + NPS
            CALL FOLEFO(IFSIG,ORG,PAS,NPS,FVA,FFO,NOMCMD,
     +                  ZR(LVAR),ZR(LFON),IER)
            IF (IER .GT. IERMAX ) GOTO 999
            CALL WKVECT(PROL,'G V K16',5, LPROL )
            CALL JEECRA(PROL,'LONUTI',5,' ')
            ZK16(LPROL  ) = NAT
            ZK16(LPROL+1) = 'LIN LIN '
            ZK16(LPROL+2) = NOMPAR
            ZK16(LPROL+3) = NOMRES
            ZK16(LPROL+4) = 'EE      '
            IF (IFM .GT. 0 ) THEN
               IVAL1 = 1
               IVAL2 = 10
               CALL FOEC2F(IFM,ZR(LVAR),NPS,IVAL1,IVAL2,NOMPAR,NOMRES)
            ENDIF
            IF ( NORME .NE. 0 ) THEN
               DO 510 IPS = 0, NPS-1
                  ZR(LFON+IPS) = ZR(LFON+IPS) * NORME
  510          CONTINUE
            ENDIF
         ELSE
            CALL UTMESS('E',NOMCMD//'(ERREUR 31)','IMPOSSIBLE DE '//
     +                  'LIRE LES VALEURS AVANT DE CONNAITRE  '//
     +                  'L''ARGUMENT DE "NATURE".')
         ENDIF
      GOTO 1
C
C
  150    CONTINUE
C        --- FINSF ---
      GOTO 2
C
C
  160    CONTINUE
C        --- PAS   ---
         IF (ICLASS.EQ.2 ) THEN
            PAS = RVAL
         ELSE
            IER = IER + 1
            IF (IER .GT. IERMAX ) GOTO 999
            CALL UTMESS('E',NOMCMD//'(ERREUR 19)','L''ARGUMENT DU '//
     +                  'MOT CLE "PAS" DOIT ETRE UN "REEL".')
         ENDIF
      GOTO 1
C
C
  170    CONTINUE
C        --- NOM_PARA ---
         IF (ICLASS.EQ.3 .OR.ICLASS.EQ.4) THEN
            NOMPAR = CVAL(1:IVAL)
         ELSE
            IER = IER + 1
            IF (IER .GT. IERMAX ) GOTO 999
            CALL UTMESS('E',NOMCMD//'(ERREUR 20)','L''ARGUMENT DU '//
     +                  'MOT CLE "NOM_PARA" DOIT ETRE UN "TEXTE" OU '//
     +                  '"IDENTIFICATEUR".')
         ENDIF
      GOTO 1
C
C
  180    CONTINUE
C        --- NOM ----
         IF (ICLASS.EQ.3 .OR.ICLASS.EQ.4) THEN
         ELSE
            IER = IER + 1
            IF (IER .GT. IERMAX ) GOTO 999
            CALL UTMESS('E',NOMCMD//'(ERREUR 21)','L''ARGUMENT DU '//
     +                  'MOT CLE "NOM" DOIT ETRE UN "TEXTE" OU '//
     +                  '"IDENTIFICATEUR".')
         ENDIF
      GOTO 1
C
C
  190    CONTINUE
C        --- NB_PARA ---
         IF (ICLASS.EQ.1 ) THEN
            NBPARA = IVAL
         ELSE
            IER = IER + 1
            IF (IER .GT. IERMAX ) GOTO 999
            CALL UTMESS('E',NOMCMD//'(ERREUR 21)','L''ARGUMENT DU '//
     +                  'MOT CLE "NB_PARA" DOIT ETRE UN '//
     +                  '"ENTIER".')
         ENDIF
      GOTO 1
C
C
  200    CONTINUE
C        --- NOM_VAR ----
         IF (ICLASS.EQ.3 .OR.ICLASS.EQ.4) THEN
            NOMVAR = CVAL(1:IVAL)
         ELSE
            IER = IER + 1
            IF (IER .GT. IERMAX ) GOTO 999
            CALL UTMESS('E',NOMCMD//'(ERREUR 21)','L''ARGUMENT DU '//
     +                  'MOT CLE "NOM_VAR" DOIT ETRE UN "TEXTE" OU '//
     +                  '"IDENTIFICATEUR".')
         ENDIF
      GOTO 1
C
C
C     ------------------------------------------------------------------
  999 CONTINUE
      CALL UTMESS('E',NOMCMD//'(ERREUR 99)','TROP D''ERREURS '//
     +                        'DETECTEES. ON ARRETE LA LECTURE.')
C
    2 CONTINUE
      CALL LXUNIT(-IFSIG,LRECL,IFM,'SIGMED1')
C
C     --- VERIFICATION QU'ON A BIEN CREER UNE FONCTION ---
C         ET REMISE DES ABSCISSES EN ORDRE CROISSANT
      CALL ORDONN(NOMFON,NOMCMD,0)
C
      CALL TITRE
      CALL FOATTR(' ',1,NOMFON)
      IF (NIV.GT.1) CALL FOIMPR(NOMFON,NIV,IFM,0,LISTR)
C
      CALL JEDEMA()
      END

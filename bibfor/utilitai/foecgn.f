      SUBROUTINE FOECGN (MOTFAC,NBOCC,IOCC,NOMFON,IUL,IPS,IND,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER                   NBOCC,IOCC,       IUL,IPS,IND,IER
      CHARACTER*19                         NOMFON
      CHARACTER*(*)      MOTFAC
C     ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 11/02/98   AUTEUR CIBHHLV L.VIVAN 
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
C TOLE CRP_6
C     Ecriture des directives "GNUPLOT"
C     ATTENTION: les commandes "GNUPLOT" sont en minuscules.
C     ==========
C     ------------------------------------------------------------------
C IN  : MOTFAC : mot cle facteur de la commande
C IN  : NBOCC  : nombre d'occurence du mot cle facteur
C IN  : IOCC   : occurence traitee
C IN  : NOMFON : nom de la fonction ou de la nappe a imprimer
C IN  : IUL    : numero d'unite logique
C OUT : IER    : code retour
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
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32     JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      REAL*8         BORNE(2)
      CHARACTER*1    BACS
      CHARACTER*8    K8B, TYPFON, NOMPAR, NOMRES, MODE
      CHARACTER*12   STYLE, SORTIE
      CHARACTER*16   NOMCMD, LABEL, LEGEND, NOMAMO
      CHARACTER*19   NOMF1
      CHARACTER*80   MONTI
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
C POUR REMPLACER LE BACK SLASH DANS LES FORMAT PAR SON CODE ASCII
C
      BACS = CHAR(92)
      IER = 0
      CALL GETRES(K8B,K8B,NOMCMD)
      IF (IPS.EQ.1) THEN
         NOMF1 = '%%%'
      ELSE
         NOMF1 = NOMFON
      ENDIF
      LF = LXLGUT(NOMF1)
C
      CALL JEVEUO(NOMFON//'.PROL','L',LPRO)
      IF ( ZK8(LPRO) .EQ. 'INTERPRE' )  THEN
         CALL JEEXIN(NOMFON//'.NOVA',IRET)
         IF ( IRET.EQ.0 ) THEN
            IER = IER + 1
            CALL UTMESS('A',NOMCMD,'FONCTION INTERPRETEE '//
     +                              NOMFON(1:LF)//' INCONNUE.')
            GOTO 9999
         ELSEIF ( IRET.NE.0 .AND. IND.EQ.0 ) THEN
            IER = IER + 1
            CALL UTMESS('A',NOMCMD,'PAS DE PARAMETRES DEFINIS POUR '//
     +                             'LA FONCTION '//NOMFON(1:LF)//'.')
            GOTO 9999
         ELSE
            CALL JEVEUO(NOMFON//'.NOVA','L',LNOVA)
            CALL JELIRA(NOMFON//'.NOVA','LONUTI',NBNOVA,K8B)
            IF ( NBNOVA .NE. 1 ) THEN
               IER = IER + 1
               CALL UTMESS('A',NOMCMD,'FONCTION '//NOMFON(1:LF)//
     +                                ' A UNE SEULE VARIABLE ADMIS.')
               GOTO 9999
            ENDIF
            TYPFON = 'FONCTION'
            NOMPAR = ZK8(LNOVA)
            NOMRES = 'TOUTRESU'
         ENDIF
C
      ELSEIF ( ZK8(LPRO) .EQ. 'FONCTION' )  THEN
         TYPFON = ZK8(LPRO  )
         NOMPAR = ZK8(LPRO+2)
         NOMRES = ZK8(LPRO+3)
C
      ELSEIF ( ZK8(LPRO) .EQ. 'NAPPE' )  THEN
         TYPFON = ZK8(LPRO  )
         NOMPAR = ZK8(LPRO+2)
         NOMRES = ZK8(LPRO+3)
C
      ELSEIF ( ZK8(LPRO) .EQ. 'CONSTANTE' ) THEN
         IER = IER + 1
         CALL UTMESS('A',NOMCMD,'NE TRACE LA FONCTION "CONSTANTE"')
         GOTO 9999
C
      ELSE
         IER = IER + 1
         CALL UTMESS('E',NOMCMD,' TYPE DE FONCTION INCONNU.')
         GOTO 9999
      ENDIF
C
      IF (IOCC.EQ.1) THEN
         WRITE(IUL,1000)
C
         CALL GETVTX(' ','TITRE',1,1,1,MONTI,NTI)
         IF (NTI.EQ.0) THEN
            CALL JEEXIN(NOMFON//'.TITR',IRET)
            IF (IRET.EQ.0) THEN
               MONTI = NOMFON
            ELSE
               CALL JEVEUO(NOMFON//'.TITR','L',JTIT)
               MONTI = ZK80(JTIT)
            ENDIF
         ENDIF
         LG = MIN(80,LXLGUT(MONTI))
         WRITE(IUL,1010) MONTI(1:LG)
C
         MODE = 'PAYSAGE'
         CALL GETVTX(' ','SORTIE',1,1,1,SORTIE,NSO)
         IF (NSO.NE.0 .AND. SORTIE(1:10).EQ.'MONOCHROME') THEN
            CALL GETVTX(' ','PRESENTATION',1,1,1,MODE,NMO)
            IF (NMO.NE.0 .AND. MODE.EQ.'PORTRAIT') THEN
               WRITE(IUL,1512)
            ELSE
               WRITE(IUL,1502)
            ENDIF
         ELSE
            CALL GETVTX(' ','PRESENTATION',1,1,1,MODE,NMO)
            IF (NMO.NE.0 .AND. MODE.EQ.'PORTRAIT') THEN
               WRITE(IUL,1510)
            ELSE
               WRITE(IUL,1500)
            ENDIF
         ENDIF
C
         CALL GETVTX(' ','FENETRE',1,1,1,K8B,NFE)
         IF (NFE.NE.0 .AND. K8B(1:8).EQ.'CARREE') THEN
               WRITE(IUL,1520) 0.75D0 , 1.D0
         ELSE
            IF (MODE.EQ.'PORTRAIT') THEN
               WRITE(IUL,1520) 0.75D0 , 1.45D0
            ELSE
               WRITE(IUL,1520) 1.D0 , 1.D0
            ENDIF
         ENDIF
C
         CALL GETVTX(' ','DATE',1,1,1,K8B,NDA)
         IF (NDA.NE.0 .AND. K8B(1:3).EQ.'OUI') THEN
            WRITE(IUL,1012)
         ELSE
            WRITE(IUL,1014)
         ENDIF
C
         CALL GETVTX(' ','GRILLE',1,1,1,K8B,NGR)
         IF (NGR.NE.0 .AND. K8B(1:3).EQ.'OUI') THEN
            WRITE(IUL,1016)
         ELSE
            WRITE(IUL,1018)
         ENDIF
C
         CALL GETVTX(' ','ECHELLE_X',1,1,1,K8B,NEX)
         IF (NEX.NE.0 .AND. K8B(1:3).EQ.'LOG') THEN
            WRITE(IUL,1100)
         ELSE
            WRITE(IUL,1102)
         ENDIF
         CALL GETVTX(' ','ECHELLE_Y',1,1,1,K8B,NEY)
         IF (NEY.NE.0 .AND. K8B(1:3).EQ.'LOG') THEN
            WRITE(IUL,1110)
         ELSE
            WRITE(IUL,1112)
         ENDIF
C
         CALL GETVTX(' ','AXE_ZERO_X',1,1,1,K8B,NZX)
         IF (NZX.NE.0 .AND. K8B(1:3).EQ.'OUI') THEN
            WRITE(IUL,1200)
         ELSE
           WRITE(IUL,1210)
         ENDIF
         CALL GETVTX(' ','AXE_ZERO_Y',1,1,1,K8B,NZY)
         IF (NZY.NE.0 .AND. K8B(1:3).EQ.'OUI') THEN
            WRITE(IUL,1212)
         ELSE
            WRITE(IUL,1214)
         ENDIF
C
         CALL GETVR8(' ','BORNE_X',1,1,0,RBID,NBX)
         IF (NBX.NE.0) THEN
            CALL GETVR8(' ','BORNE_X',1,1,2,BORNE,NBX)
            WRITE(IUL,1400) BORNE(1), BORNE(2)
         ELSE
            WRITE(IUL,1410)
         ENDIF
         CALL GETVR8(' ','BORNE_Y',1,1,0,RBID,NBY)
         IF (NBY.NE.0) THEN
            CALL GETVR8(' ','BORNE_Y',1,1,2,BORNE,NBY)
            WRITE(IUL,1420) BORNE(1), BORNE(2)
         ELSE
            WRITE(IUL,1430)
         ENDIF
C
         CALL GETVTX(' ','LEGENDE_X',1,1,1,LABEL,NLX1)
         CALL GETVTX(' ','LABEL_X'  ,1,1,1,LABEL,NLX2)
         IF (NLX1.NE.0 .OR. NLX2.NE.0) THEN
            LG = MAX(1,LXLGUT(LABEL))
            WRITE(IUL,1020) LABEL(1:LG)
         ELSE
            LG = MAX(1,LXLGUT(NOMPAR))
            WRITE(IUL,1020) NOMPAR(1:LG)
         ENDIF
         CALL GETVTX(' ','LEGENDE_Y',1,1,1,LABEL,NLY1)
         CALL GETVTX(' ','LABEL_Y'  ,1,1,1,LABEL,NLY2)
         IF (NLY1.NE.0 .OR. NLY2.NE.0) THEN
            LG = MAX(1,LXLGUT(LABEL))
            WRITE(IUL,1030) LABEL(1:LG)
         ELSE
            LG = MAX(1,LXLGUT(NOMRES))
            WRITE(IUL,1030) NOMRES(1:LG)
         ENDIF
C
      ENDIF
C
      CALL GETVTX(MOTFAC,'STYLE',IOCC,1,1,K8B,NST)
      IF (NST.NE.0) THEN
         IF (K8B(1:5).EQ.'LIGNE') THEN
            STYLE = 'lines'
         ELSEIF (K8B.EQ.'POINT_RE') THEN
            STYLE = 'linespoints'
         ELSEIF (K8B(1:5).EQ.'POINT') THEN
            STYLE = 'points'
         ENDIF
      ELSE
         STYLE = 'lines'
      ENDIF
C
      ICOL = 0
      CALL GETVTX(MOTFAC,'COULEUR',IOCC,1,1,K8B,NCO)
      IF (NCO.NE.0) THEN
         IF (K8B(1:5).EQ.'ROUGE') THEN
            ICOL = 1
         ELSEIF (K8B(1:4).EQ.'VERT') THEN
            ICOL = 2
         ELSEIF (K8B(1:4).EQ.'BLEU') THEN
            ICOL = 3
         ELSEIF (K8B(1:6).EQ.'VIOLET') THEN
            ICOL = 4
         ELSEIF (K8B(1:8).EQ.'TURQUOIS') THEN
            ICOL = 5
         ELSEIF (K8B(1:4).EQ.'BRUN') THEN
            ICOL = 6
         ELSEIF (K8B(1:6).EQ.'ORANGE') THEN
            ICOL = 7
         ELSEIF (K8B(1:6).EQ.'CORAIL') THEN
            ICOL = 8
         ENDIF
      ENDIF
      IF (SORTIE(1:10).EQ.'MONOCHROME') ICOL = 0
C
      IMAR = 0
      CALL GETVTX(MOTFAC,'MARQUEUR',IOCC,1,1,K8B,NMA)
      IF (NMA.NE.0) THEN
         IF (K8B(1:5).EQ.'POINT') THEN
            IMAR = 0
         ELSEIF (K8B(1:7).EQ.'LOSANGE') THEN
            IMAR = 1
         ELSEIF (K8B(1:4).EQ.'PLUS') THEN
            IMAR = 2
         ELSEIF (K8B(1:4).EQ.'CARRE') THEN
            IMAR = 3
         ELSEIF (K8B(1:1).EQ.'X') THEN
            IMAR = 4
         ELSEIF (K8B(1:8).EQ.'TRIANGLE') THEN
            IMAR = 5
         ELSEIF (K8B(1:6).EQ.'ETOILE') THEN
            IMAR = 6
         ENDIF
      ENDIF
C
      CALL GETVTX(MOTFAC,'LEGENDE',IOCC,1,1,LEGEND,NLE)
      IF (TYPFON.EQ.'FONCTION') THEN
         IF (NLE.EQ.0) LEGEND = NOMFON
         LE = MAX(1,LXLGUT(LEGEND))
         IF (ICOL.NE.0) THEN
            IF (IOCC.EQ.1 .AND. NBOCC.EQ.1) THEN
               WRITE(IUL,1040) NOMF1(1:LF),LEGEND(1:LE),STYLE,ICOL,IMAR
               WRITE(IUL,1002)
            ELSEIF (IOCC.EQ.1 .AND. NBOCC.NE.1) THEN
               WRITE(IUL,1042) NOMF1(1:LF),LEGEND(1:LE),STYLE,ICOL,IMAR,
     &                         BACS
            ELSEIF (IOCC.EQ.NBOCC) THEN
               WRITE(IUL,1052) NOMF1(1:LF),LEGEND(1:LE),STYLE,ICOL,IMAR
               WRITE(IUL,1002)
            ELSE
               WRITE(IUL,1050) NOMF1(1:LF),LEGEND(1:LE),STYLE,ICOL,IMAR,
     &                         BACS
            ENDIF
         ELSE
            IF (IOCC.EQ.1 .AND. NBOCC.EQ.1) THEN
               WRITE(IUL,1060) NOMF1(1:LF),LEGEND(1:LE),STYLE
               WRITE(IUL,1002)
            ELSEIF (IOCC.EQ.1 .AND. NBOCC.NE.1) THEN
               WRITE(IUL,1062) NOMF1(1:LF),LEGEND(1:LE),STYLE,BACS
            ELSEIF (IOCC.EQ.NBOCC) THEN
               WRITE(IUL,1066) NOMF1(1:LF),LEGEND(1:LE),STYLE
               WRITE(IUL,1002)
            ELSE
               WRITE(IUL,1064) NOMF1(1:LF),LEGEND(1:LE),STYLE,BACS
            ENDIF
         ENDIF
C
      ELSE
         CALL JEVEUO(NOMFON//'.PARA','L',LPAR)
         CALL JELIRA(NOMFON//'.PARA','LONMAX',NBAMOR,K8B)
         NOMAMO = ZK8(LPRO+2)
         IF (NLE.NE.0) NOMAMO = LEGEND
         LG = MAX(1,LXLGUT(NOMAMO))
         IF (NBAMOR.EQ.1) THEN
            IF (IOCC.EQ.1 .AND. NBOCC.EQ.1) THEN
               WRITE(IUL,2010) NOMF1(1:LF),NOMAMO(1:LG),ZR(LPAR),STYLE
               WRITE(IUL,1002)
            ELSEIF (IOCC.EQ.1 .AND. NBOCC.NE.1) THEN
               WRITE(IUL,2020) NOMF1(1:LF),NOMAMO(1:LG),ZR(LPAR),STYLE,
     &                         BACS
            ELSEIF (IOCC.EQ.NBOCC) THEN
               WRITE(IUL,2120) NOMF1(1:LF),NOMAMO(1:LG),ZR(LPAR),STYLE
               WRITE(IUL,1002)
            ELSE
               WRITE(IUL,2110) NOMF1(1:LF),NOMAMO(1:LG),ZR(LPAR),
     &                                     STYLE, BACS
            ENDIF
         ELSE
            IF (IOCC.EQ.1) THEN
               WRITE(IUL,2020) NOMF1(1:LF),NOMAMO(1:LG),ZR(LPAR),
     &                                     STYLE, BACS
            ELSE
               WRITE(IUL,2110) NOMF1(1:LF),NOMAMO(1:LG),ZR(LPAR),
     &                                     STYLE, BACS
            ENDIF
         ENDIF
         LPAR = LPAR - 1
         DO 30 IA = 2,NBAMOR
            IF (IA.EQ.NBAMOR) THEN
               IF (IOCC.EQ.1 .AND. NBOCC.EQ.1) THEN
                  WRITE(IUL,2120) NOMF1(1:LF),NOMAMO(1:LG),ZR(LPAR+IA),
     &                                        STYLE
                  WRITE(IUL,1002)
               ELSEIF (IOCC.EQ.NBOCC) THEN
                  WRITE(IUL,2120) NOMF1(1:LF),NOMAMO(1:LG),ZR(LPAR+IA),
     &                                        STYLE
                  WRITE(IUL,1002)
               ELSE
                  WRITE(IUL,2110) NOMF1(1:LF),NOMAMO(1:LG),ZR(LPAR+IA),
     &                                        STYLE, BACS
               ENDIF
            ELSE
              WRITE(IUL,2110) NOMF1(1:LF),NOMAMO(1:LG),ZR(LPAR+IA),
     &                                    STYLE, BACS
            ENDIF
 30      CONTINUE
      ENDIF
C
 1000 FORMAT('#DEBUT_DIRECTIVE_GNUPLOT')
 1010 FORMAT('  set title "',A,'"')
 1012 FORMAT('  set time')
 1014 FORMAT('  set notime')
 1016 FORMAT('  set grid')
 1018 FORMAT('  set nogrid')
 1100 FORMAT('  set logscale x')
 1102 FORMAT('  set nologscale x')
 1110 FORMAT('  set logscale y')
 1112 FORMAT('  set nologscale y')
 1200 FORMAT('  set xzeroaxis')
 1210 FORMAT('  set noxzeroaxis')
 1212 FORMAT('  set yzeroaxis')
 1214 FORMAT('  set noyzeroaxis')
 1400 FORMAT('  set xrange [ ',D12.5,' : ',D12.5,' ]')
 1410 FORMAT('  set autoscale x')
 1420 FORMAT('  set yrange [ ',D12.5,' : ',D12.5,' ]')
 1430 FORMAT('  set autoscale y')
 1500 FORMAT('  set terminal postscript landscape color')
 1502 FORMAT('  set terminal postscript landscape monochrome')
 1510 FORMAT('  set terminal postscript portrait color')
 1512 FORMAT('  set terminal postscript portrait monochrome')
 1520 FORMAT('  set size ',D12.5,' , ',D12.5)
 1020 FORMAT('  set xlabel "',A,'"')
 1030 FORMAT('  set ylabel "',A,'"')
 1040 FORMAT('  plot "',A,'" using 1:2 t "',A,'" with ',A,1X,I1,1X,I1)
 1042 FORMAT
     +('  plot "',A,'" using 1:2 t "',A,'" with ',A,1X,I1,1X,I1,A)
 1050 FORMAT
     +('     , "',A,'" using 1:2 t "',A,'" with ',A,1X,I1,1X,I1,A)
 1052 FORMAT
     +('     , "',A,'" using 1:2 t "',A,'" with ',A,1X,I1,1X,I1)
 1060 FORMAT('  plot "',A,'" using 1:2  t "',A,'" with ',A)
 1062 FORMAT('  plot "',A,'" using 1:2  t "',A,'" with ',A,A)
 1064 FORMAT('     , "',A,'" using 1:2  t "',A,'" with ',A,A)
 1066 FORMAT('     , "',A,'" using 1:2  t "',A,'" with ',A)
 2010 FORMAT('  plot "',A,'" using 1:2  t "',A,':',F8.4,'" with ',A)
 2020 FORMAT('  plot "',A,'" using 1:2  t "',A,':',F8.4,'" with ',
     +       A,A)
 2110 FORMAT('     , "',A,'" using 1:2  t "',A,':',F8.4,'" with ',
     +       A,A)
 2120 FORMAT('     , "',A,'" using 1:2  t "',A,':',F8.4,'" with ',A)
 1002 FORMAT('#FIN_DIRECTIVE_GNUPLOT')
C
 9999 CONTINUE
      CALL JEDEMA()
      END

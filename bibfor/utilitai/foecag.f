      SUBROUTINE FOECAG (MOTFAC,NBOCC,IOCC,NOMFON,IUL,IPS,IND,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER                   NBOCC,IOCC,       IUL,IPS,IND,IER
      CHARACTER*19                         NOMFON
      CHARACTER*(*)      MOTFAC
C     ------------------------------------------------------------------
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
C     Ecriture des directives "AGRAF"
C     ATTENTION: les commandes "AGRAF" sont en minuscules.
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER        ICOURB
      REAL*8          BORNE(2),RBID
      CHARACTER*2     TRI
      CHARACTER*8    K8B, TYPFON
      INTEGER LF,LXLGUT,LPRO,IRET,LNOVA,NBNOVA,NTI,LG,IFGX,NGR
      INTEGER IFGY,NEX,NEY,NBX,NBY,NLX,NLY,ISTYLE,NST,ICOL,NCO,IMAR,NMA
      INTEGER IFMA,NLE,NTR,LPAR,NBAMOR,IA
      CHARACTER*12   K12B
      CHARACTER*16   NOMCMD, LABEL, LEGEND
      CHARACTER*19   NOMF1
      CHARACTER*80   MONTI, MONCOM
C
      SAVE           ICOURB
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      IF ( IOCC .EQ. 1 ) ICOURB = 0
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
         ENDIF
C
      ELSEIF ( ZK8(LPRO) .EQ. 'FONCTION' )  THEN
         TYPFON = ZK8(LPRO  )
C
      ELSEIF ( ZK8(LPRO) .EQ. 'NAPPE' )  THEN
         TYPFON = ZK8(LPRO  )
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
C
         WRITE(IUL,6000)
         WRITE(IUL,6010)
         WRITE(IUL,6020)
         WRITE(IUL,6030)
         WRITE(IUL,6040)
         WRITE(IUL,6050)
         WRITE(IUL,6060)
         WRITE(IUL,6070)
         WRITE(IUL,6080)
         WRITE(IUL,6100)
         WRITE(IUL,6101)
         WRITE(IUL,6102)
         WRITE(IUL,6103)
         WRITE(IUL,6104)
         WRITE(IUL,6105)
         WRITE(IUL,6106)
         WRITE(IUL,6107)
         WRITE(IUL,6108)
         WRITE(IUL,6109)
         WRITE(IUL,6110)
         WRITE(IUL,6111)
C
         WRITE(IUL,7000)
C
         CALL GETVTX(' ','TITRE_GRAPHIQUE',1,1,1,MONTI,NTI)
         IF ( NTI .NE. 0 ) THEN
            LG = MIN(80,LXLGUT(MONTI))
            WRITE(IUL,7010) MONTI(1:LG)
         ELSE
            WRITE(IUL,7010) 'GRAPHIQUE'
         ENDIF
C
         CALL GETVTX(' ','COMMENTAIRE',1,1,1,MONCOM,NTI)
         IF (NTI.NE.0) THEN
            LG = MIN(80,LXLGUT(MONCOM))
            WRITE(IUL,7012) MONCOM(1:LG)
         ENDIF
C
         CALL GETVIS(' ','FREQ_GRILLE_X',1,1,1,IFGX,NGR)
         WRITE(IUL,7022) IFGX
         CALL GETVIS(' ','FREQ_GRILLE_Y',1,1,1,IFGY,NGR)
         WRITE(IUL,7032) IFGY
C
         CALL GETVTX(' ','ECHELLE_X',1,1,1,K8B,NEX)
         IF (NEX.NE.0 .AND. K8B(1:3).EQ.'LOG') THEN
            WRITE(IUL,7018) 1
         ELSE
            WRITE(IUL,7018) 0
         ENDIF
         CALL GETVTX(' ','ECHELLE_Y',1,1,1,K8B,NEY)
         IF (NEY.NE.0 .AND. K8B(1:3).EQ.'LOG') THEN
            WRITE(IUL,7028) 1
         ELSE
            WRITE(IUL,7028) 0
         ENDIF
C
         CALL GETVR8(' ','BORNE_X',1,1,0,RBID,NBX)
         IF (NBX.NE.0) THEN
            CALL GETVR8(' ','BORNE_X',1,1,2,BORNE,NBX)
            WRITE(IUL,7014) BORNE(1)
            WRITE(IUL,7016) BORNE(2)
         ENDIF
         CALL GETVR8(' ','BORNE_Y',1,1,0,RBID,NBY)
         IF (NBY.NE.0) THEN
            CALL GETVR8(' ','BORNE_Y',1,1,2,BORNE,NBY)
            WRITE(IUL,7024) BORNE(1)
            WRITE(IUL,7026) BORNE(2)
         ENDIF
C
         CALL GETVTX(' ','LEGENDE_X',1,1,1,LABEL,NLX)
         IF (NLX.NE.0) THEN
            LG = MAX(1,LXLGUT(LABEL))
            WRITE(IUL,7020) LABEL(1:LG)
         ENDIF
         CALL GETVTX(' ','LEGENDE_Y',1,1,1,LABEL,NLY)
         IF (NLY.NE.0) THEN
            LG = MAX(1,LXLGUT(LABEL))
            WRITE(IUL,7030) LABEL(1:LG)
         ENDIF
C
      ENDIF
C
      WRITE(IUL,7100)
C
      ISTYLE = 0
      CALL GETVTX(MOTFAC,'STYLE',IOCC,1,1,K8B,NST)
      IF (NST.NE.0) THEN
         IF     (K8B(1:5).EQ.'LIGNE'   ) THEN
            ISTYLE = 0
         ELSEIF (K8B     .EQ.'POINTILL') THEN
            ISTYLE = 1
         ELSEIF (K8B(1:5).EQ.'POINT'   ) THEN
            ISTYLE = 2
         ENDIF
      ENDIF
      WRITE(IUL,7106) ISTYLE
C
      ICOL = 0
      CALL GETVTX(MOTFAC,'COULEUR',IOCC,1,1,K8B,NCO)
      IF (NCO.NE.0) THEN
         IF     (K8B(1:4).EQ.'NOIR'     ) THEN
            ICOL = 0
         ELSEIF (K8B(1:5).EQ.'ROUGE'    ) THEN
            ICOL = 1
         ELSEIF (K8B(1:8).EQ.'VERT_FON' ) THEN
            ICOL = 2
         ELSEIF (K8B(1:4).EQ.'BLEU'     ) THEN
            ICOL = 3
         ELSEIF (K8B(1:7).EQ.'MAGENTA'  ) THEN
            ICOL = 4
         ELSEIF (K8B(1:4).EQ.'CYAN'     ) THEN
            ICOL = 5
         ELSEIF (K8B(1:4).EQ.'VERT'     ) THEN
            ICOL = 6
         ELSEIF (K8B(1:6).EQ.'MARRON'   ) THEN
            ICOL = 7
         ELSEIF (K8B(1:6).EQ.'ORANGE'   ) THEN
            ICOL = 8
         ELSEIF (K8B(1:5).EQ.'MAUVE'    ) THEN
            ICOL = 9
         ELSEIF (K8B(1:5).EQ.'JAUNE'    ) THEN
            ICOL = 10
         ELSEIF (K8B(1:8).EQ.'MARRON_C' ) THEN
            ICOL = 11
         ENDIF
      ENDIF
      WRITE(IUL,7108) ICOL
C
      IMAR = 0
      CALL GETVTX(MOTFAC,'MARQUEUR',IOCC,1,1,K12B,NMA)
      IF (NMA.NE.0) THEN
         IF     (K12B(1:7) .EQ.'CARRE_P'     ) THEN
            IMAR = 5
         ELSEIF (K12B(1:7) .EQ.'CARRE_X'     ) THEN
            IMAR = 10
         ELSEIF (K12B(1:5) .EQ.'CARRE'       ) THEN
            IMAR = 1
         ELSEIF (K12B(1:10).EQ.'CERCLE_P_X'  ) THEN
            IMAR = 7
         ELSEIF (K12B(1:8) .EQ.'CERCLE_P'    ) THEN
            IMAR = 4
         ELSEIF (K12B(1:8) .EQ.'CERCLE_X'    ) THEN
            IMAR = 9
         ELSEIF (K12B(1:6) .EQ.'CERCLE'      ) THEN
            IMAR = 0
         ELSEIF (K12B(1:11).EQ.'LOSANGE_P_X' ) THEN
            IMAR = 8
         ELSEIF (K12B(1:9) .EQ.'LOSANGE_P'   ) THEN
            IMAR = 6
         ELSEIF (K12B(1:9) .EQ.'LOSANGE_X'   ) THEN
            IMAR = 11
         ELSEIF (K12B(1:7) .EQ.'LOSANGE'     ) THEN
            IMAR = 3
         ELSEIF (K12B(1:4) .EQ.'PLUS'        ) THEN
            IMAR = 2
         ENDIF
      ENDIF
      WRITE(IUL,7110) IMAR
C
      CALL GETVIS(MOTFAC,'FREQ_MARQUEUR',IOCC,1,1,IFMA,NMA)
      WRITE(IUL,7112) IFMA
C
      CALL GETVTX(MOTFAC,'LEGENDE',IOCC,1,1,LEGEND,NLE)
      IF ( NLE .NE. 0 ) THEN
         LG = MAX(1,LXLGUT(LEGEND))
         WRITE(IUL,7114) LEGEND(1:LG)
      ENDIF
C
      CALL GETVTX(MOTFAC,'TRI',IOCC,1,1,TRI,NTR)
      IF ( NTR .NE. 0 ) THEN
         LG = MAX(1,LXLGUT(TRI))
         WRITE(IUL,7116) TRI(1:LG)
      ENDIF
C
      IF (TYPFON.EQ.'FONCTION') THEN
         ICOURB = ICOURB + 1
         WRITE(IUL,7102) ICOURB , 1
         WRITE(IUL,7104) ICOURB , 2
C
      ELSE
         CALL JEVEUO(NOMFON//'.PARA','L',LPAR)
         CALL JELIRA(NOMFON//'.PARA','LONMAX',NBAMOR,K8B)
         DO 30 IA = 1,NBAMOR
            ICOURB = ICOURB + 1
            IF ( IA .EQ. 1 ) THEN
               WRITE(IUL,7102) ICOURB , 1
               WRITE(IUL,7104) ICOURB , 2
            ELSE
               WRITE(IUL,7100)
               WRITE(IUL,7102) ICOURB , 1
               WRITE(IUL,7104) ICOURB , 2
            ENDIF
 30      CONTINUE
      ENDIF
C
 6000 FORMAT(/,'ASPECT_GRAPHIQUE:')
 6010 FORMAT(  '  En-tete :Departement Mecanique et Modeles Numeriques')
 6020 FORMAT(  '  Aspect :0')
 6030 FORMAT(  '  Nombre de vues :1')
 6040 FORMAT(  '  Cesure commentaire :40')
 6050 FORMAT(  '  MinMax :0')
 6060 FORMAT(  '  Fonte Titre :%helvetica-14')
 6070 FORMAT(  '  Fonte Axes :%courier-12')
 6080 FORMAT(  '  Fonte Autre :%times-12')
 6100 FORMAT(/,'  DEFAUT_COURBE:',/,
     +         '    Couleur (rvb) :     0     0     0')
 6101 FORMAT(/,'  DEFAUT_COURBE:',/,
     +         '    Couleur (rvb) : 65535     0     0')
 6102 FORMAT(/,'  DEFAUT_COURBE:',/,
     +         '    Couleur (rvb) : 11822 35723 22359')
 6103 FORMAT(/,'  DEFAUT_COURBE:',/,
     +         '    Couleur (rvb) :     0     0 65535')
 6104 FORMAT(/,'  DEFAUT_COURBE:',/,
     +         '    Couleur (rvb) : 65535     0 65535')
 6105 FORMAT(/,'  DEFAUT_COURBE:',/,
     +         '    Couleur (rvb) :     0 65535 65535')
 6106 FORMAT(/,'  DEFAUT_COURBE:',/,
     +         '    Couleur (rvb) :     0 65535     0')
 6107 FORMAT(/,'  DEFAUT_COURBE:',/,
     +         '    Couleur (rvb) : 41120 21074 11565')
 6108 FORMAT(/,'  DEFAUT_COURBE:',/,
     +         '    Couleur (rvb) : 65535 42405     0')
 6109 FORMAT(/,'  DEFAUT_COURBE:',/,
     +         '    Couleur (rvb) : 41120  8224 61680')
 6110 FORMAT(/,'  DEFAUT_COURBE:',/,
     +         '    Couleur (rvb) : 65535 65535     0')
 6111 FORMAT(/,'  DEFAUT_COURBE:',/,
     +         '    Couleur (rvb) : 53970 46260 35980')
C
 7000 FORMAT(/,'GRAPHIQUE:')
 7010 FORMAT(  '  Titre :'               ,A     )
 7012 FORMAT(  '  Commentaire :'         ,A     )
 7014 FORMAT(  '  Min X :'               ,D12.5 )
 7016 FORMAT(  '  Max X :'               ,D12.5 )
 7018 FORMAT(  '  Echelle X :'           ,I1    )
 7020 FORMAT(  '  Legende X :'           ,A     )
 7022 FORMAT(  '  Frequence Grille X :'  ,I1    )
 7024 FORMAT(  '  Min Y :'               ,D12.5 )
 7026 FORMAT(  '  Max Y :'               ,D12.5 )
 7028 FORMAT(  '  Echelle Y :'           ,I1    )
 7030 FORMAT(  '  Legende Y :'           ,A     )
 7032 FORMAT(  '  Frequence Grille Y :'  ,I1    )
C
 7100 FORMAT(/,'  COURBE:')
 7102 FORMAT(  '     Abscisses : [',I2,',',I2,']')
 7104 FORMAT(  '     Ordonnees : [',I2,',',I2,']')
 7106 FORMAT(  '     Trait :'             ,I1 )
 7108 FORMAT(  '     Couleur :'           ,I1 )
 7110 FORMAT(  '     Marqueur :'          ,I1 )
 7112 FORMAT(  '     Frequence Marqueur :',I1 )
 7114 FORMAT(  '     Legende :'           ,A  )
 7116 FORMAT(  '     Tri :'               ,A  )
C
 9999 CONTINUE
      CALL JEDEMA()
      END

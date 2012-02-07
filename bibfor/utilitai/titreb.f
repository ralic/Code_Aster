      SUBROUTINE TITREB( DONNEE,ILIGD,ICOLD,NBTITR,SORTIE,ILIGS,ICOLS)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)      DONNEE(*),                SORTIE(*)
      INTEGER                   ILIGD,ICOLD,NBTITR,       ILIGS,ICOLS
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 07/02/2012   AUTEUR COURTOIS M.COURTOIS 
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
C TOLE CRP_20
C     TRAITEMENT DE DEMONS
C     ------------------------------------------------------------------
C IN DONNEE : K : TABLEAU DES DONNEES
C IN ILIGD  : I :
C IN ICOLD  : I : INDICE DE COLONNE OU SE SITUE LE &
C IN NBTITR : I : NOMBRE MAXIMUM DE LIGNES DE TITRE EN ENTREE
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER       IVAL, IGEN,IPOSA,IPOSB,IPOSC
      REAL*8        RVAL,RBID
      CHARACTER*4   CTYPE
      CHARACTER*4   CT(3)
      CHARACTER*80  CVAL
      CHARACTER*255 CGEN
C
      LOGICAL LFREQ
C
C     REMARQUE :  MXPARA DONNE LE NOMBRE DE PARAMETRES DU DEMON
      PARAMETER          (MXDEMO=20)
      CHARACTER*16 DEMONS(MXDEMO) , CBID,K16BID, TYSD
      CHARACTER*24 PARA(2)
      INTEGER      MXPARA(MXDEMO)
C     ------------------------------------------------------------------
C     --- LISTE DES DEMONS RECONNUS ---
      DATA DEMONS/
     &  'DATE'     , 'DATE_HEURE'    , 'HEURE'    ,
     &  'RESULTAT' , 'TYPE'          , 'COMMANDE' ,
     &  'CODE'     , 'TITRE_MAILLAGE', 'VERSION'  , 'RL'      ,
     &  'NB_ELEM'  , 'NB_NOEUD'      , 'PHENOMENE', 'DIM_GEOM',
     &  'NB_EQUA'  , 'LOC'           , 'NOM_SYMB' , 'NUME_ORDRE',
     &  'ACCES'    , 'VALEUR'        /
      DATA MXPARA/
     &    0        ,       0         ,     0      ,
     &    0        ,       1         ,     0      ,
     &    0        ,       1         ,     0      ,    0      ,
     &    1        ,       1         ,     1      ,    1      ,
     &    1        ,       1         ,     2      ,    2      ,
     &    2        ,       1         /
C     ------------------------------------------------------------------
C
C     --- LIRE LE NOM DU DEMON DE MINUIT ---
      CALL JEMARQ()
      NBPARA = 0
      LFREQ = .FALSE.
      ICOLD  = ICOLD + 1
    1 CONTINUE
      CALL LXSCAN(DONNEE(ILIGD),ICOLD,ICLASS,IVAL,RVAL,CVAL)
      IF ( ICLASS .EQ. -1 ) THEN
         ICOLD = 1
         ILIGD = ILIGD + 1
         IF ( ILIGD .LE. NBTITR )  GOTO 1
      ELSEIF( ICLASS .NE. 3 ) THEN
CCC      DEMON INCORRECT
         CALL SNDBG(IUNIFI('MESSAGE'),ICLASS,IVAL,RVAL,CVAL)
         CGEN = ' '
         IGEN = 0
      ELSE
C
          CGEN = ' '
          IGEN = 0
          CALL LXCAPS(CVAL(1:IVAL))
          CALL UTREMT(CVAL(1:IVAL),DEMONS,MXDEMO,IPLACE)
          GOTO( 10,  20,  30,  40,  50,  60,  70,  80,  90, 100,
     &         110, 120, 130, 140, 150, 160, 170, 180, 190, 200 ) IPLACE
C
CCC      DEMON INCORRECT
          GOTO 9000
C
C        --- DATE ---
   10    CONTINUE
            CALL JJMMAA( CT ,CBID(1:12) )
            CGEN(1:2)  = CT(1)(1:2)
            CGEN(4:5)  = CT(2)(1:2)
            CGEN(7:10) = CT(3)
            CGEN(6:6) = '/'
            CGEN(3:3) = '/'
            IGEN      = 10
         GOTO 9000
C
C        --- 'DATE_HEURE' ---
   20    CONTINUE
            CALL ENLIRD(CGEN)
            IGEN = 24
         GOTO 9000
C
C        --- 'HEURE' ---
   30    CONTINUE
            CALL ENLIRD(CGEN)
            CGEN(1:8) = CGEN(17:24)
            IGEN = 8
         GOTO 9000
C
C        --- 'RESULTAT' ---
   40    CONTINUE
            CALL GETRES(CGEN,CBID,CBID)
            IGEN = LXLGUT(CGEN(1:8))
         GOTO 9000
C
C        --- 'TYPE' ---
   50    CONTINUE
            CALL TITREC(DEMONS(IPLACE),DONNEE,ILIGD,ICOLD,
     &                               NBTITR,MXPARA(IPLACE),PARA,NBPARA)
            CALL GETTCO(PARA(1),CGEN)
            IF ( CGEN .EQ. '  ' ) GOTO 9001
            IGEN = LXLGUT(CGEN(1:16))
         GOTO 9000
C
C        --- COMMANDE ---
   60    CONTINUE
            CALL GETRES(CBID,CBID,CGEN)
            IGEN = LXLGUT(CGEN(1:16))
         GOTO 9000
C
C        --- CODE --- (SUPPRIME)
   70    CONTINUE
         CGEN = ' '
         IGEN = 0
         GOTO 9000
C
C        --- TITRE_MAILLAGE ---
   80    CONTINUE
            CALL TITREC(DEMONS(IPLACE),DONNEE,ILIGD,ICOLD,
     &                               NBTITR,MXPARA(IPLACE),PARA,NBPARA)
            CALL GETTCO(PARA(1),CGEN)
            IF ( CGEN .EQ. '  ' ) CGEN = 'CHAMP'
            IF ( CGEN(1:16) .EQ. 'MAILLAGE' ) THEN
               CBID = PARA(1)(1:16)
            ELSE
               CALL DISMOI('C','NOM_MAILLA',PARA(1),CGEN,IBID,CBID,IERD)
               IF ( IERD .NE. 0 ) GOTO 9000
            ENDIF
            CALL JEVEUO(CBID(1:8)//'           .TITR','L',LTIT)
            CALL JELIRA(CBID(1:8)//'           .TITR','LONMAX',NL,
     &                  CBID(9:))
C                 ---> LA RECOPIE SE FAIT ICI
            IF ( ICOLS+IGEN-1 .GT. LEN(SORTIE(1)) )  THEN
               ILIGS = ILIGS + 1
               ICOLS = 1
            ENDIF
            DO  81 ITIT = 1, NL
               SORTIE(ILIGS)(ICOLS:) = ZK80(LTIT+ITIT-1)
               ILIGS = ILIGS + 1
               ICOLS = 1
   81       CONTINUE
            IGEN = 0
         GOTO 9000
C
C        --- VERSION  ---
   90    CONTINUE
            CALL GTOPTK('versionD0', CGEN(1:8), IRET)
            IGEN = 8
         GOTO 9000
C
C        --- RETOUR A LA LIGNE ---
  100    CONTINUE
            ILIGS = ILIGS + 1
            ICOLS = 0
         GOTO 9000
C
C        --- NB_ELEM  ---
  110    CONTINUE
            CALL TITREC(DEMONS(IPLACE),DONNEE,ILIGD,ICOLD,
     &                               NBTITR,MXPARA(IPLACE),PARA,NBPARA)
            CALL GETTCO(PARA(1),CGEN)
            IF ( CGEN .EQ. '  ' ) CGEN = 'CHAMP'
            IF ( CGEN(1:16) .EQ. 'MAILLAGE' ) THEN
               CBID = PARA(1)(1:16)
            ELSE
               CALL DISMOI('C','NOM_MAILLA',PARA(1),CGEN,IBID,CBID,IERD)
               IF ( IERD .NE. 0 ) GOTO 9000
            ENDIF
            CALL DISMOI('C','NB_MA_MAILLA',CBID,'MAILLAGE',
     &                                                   IBID,CBID,IERD)
            IF ( IERD .NE. 0 ) GOTO 9000
            CGEN = '  '
            CALL CODENT(IBID,'G',CGEN(1:16))
            IGEN = LXLGUT(CGEN(1:16))
         GOTO 9000
C
C        --- NB_NOEUD ---
  120    CONTINUE
            CALL TITREC(DEMONS(IPLACE),DONNEE,ILIGD,ICOLD,
     &                               NBTITR,MXPARA(IPLACE),PARA,NBPARA)
            CALL GETTCO(PARA(1),CGEN)
            IF ( CGEN .EQ. '  ' ) CGEN = 'CHAMP'
            IF ( CGEN(1:16) .EQ. 'MAILLAGE' ) THEN
               CBID = PARA(1)(1:16)
            ELSE
               CALL DISMOI('C','NOM_MAILLA',PARA(1),CGEN,IBID,CBID,IERD)
               IF ( IERD .NE. 0 ) GOTO 9000
            ENDIF
            CALL DISMOI('C','NB_NO_MAILLA',CBID,'MAILLAGE',
     &                                                   IBID,CBID,IERD)
            IF ( IERD .NE. 0 ) GOTO 9000
            CGEN = '  '
            CALL CODENT(IBID,'G',CGEN(1:16))
            IGEN = LXLGUT(CGEN(1:16))
         GOTO 9000
C
C        --- PHENOMENE ---
  130    CONTINUE
            CALL TITREC(DEMONS(IPLACE),DONNEE,ILIGD,ICOLD,
     &                               NBTITR,MXPARA(IPLACE),PARA,NBPARA)
            CALL GETTCO(PARA(1),CGEN)
            IF ( CGEN .EQ. '  ' ) CGEN = 'CHAMP'
            IF ( CGEN(1:16) .EQ. 'MODELE' ) THEN
               CBID = PARA(1)(1:16)
            ELSE
               CALL DISMOI('C','NOM_MODELE',PARA(1),CGEN,IBID,CBID,IERD)
               IF ( IERD .NE. 0 ) GOTO 9000
            ENDIF
            CGEN = '  '
            CALL DISMOI('C','PHENOMENE',CBID,'MODELE',
     &                                             IBID,CGEN(1:16),IERD)
            IF ( IERD .NE. 0 ) GOTO 9000
            IGEN = LXLGUT(CGEN(1:16))
         GOTO 9000
C
C        --- DIMENSION GEOMETRIE ---
  140    CONTINUE
            CALL TITREC(DEMONS(IPLACE),DONNEE,ILIGD,ICOLD,
     &                               NBTITR,MXPARA(IPLACE),PARA,NBPARA)
            CALL GETTCO(PARA(1),CGEN)
            IF ( CGEN .EQ. '  ' ) CGEN = 'CHAMP'
            IF ( CGEN(1:16) .EQ. 'MAILLAGE' ) THEN
               CBID = PARA(1)(1:16)
            ELSE
               CALL DISMOI('C','NOM_MAILLA',PARA(1),CGEN,IBID,CBID,IERD)
               IF ( IERD .NE. 0 ) GOTO 9000
            ENDIF
            CALL DISMOI('C','DIM_GEOM_B',CBID,'MAILLAGE',IBID,CBID,IERD)
            IF ( IERD .NE. 0 ) GOTO 9000
            CGEN = '.D'
            CALL CODENT(IBID,'G',CGEN(1:1))
            IGEN = 2
         GOTO 9000
C
C        --- NOMBRE D'EQUATIONS ---
  150    CONTINUE
            CALL TITREC(DEMONS(IPLACE),DONNEE,ILIGD,ICOLD,
     &                               NBTITR,MXPARA(IPLACE),PARA,NBPARA)
            CALL GETTCO(PARA(1),CGEN)
            IF ( CGEN .EQ. '  ' ) CGEN = 'CHAMP'
            CALL DISMOI('C','NB_EQUA',PARA(1),CGEN,IBID,CBID,IERD)
            IF ( IERD .NE. 0 ) GOTO 9000
            CGEN = '  '
            CALL CODENT(IBID,'G',CGEN(1:16))
            IGEN = LXLGUT(CGEN(1:16))
         GOTO 9000
C
C        --- LOCALISATION POUR UN CHAM_ELEM ---
  160    CONTINUE
            CALL TITREC(DEMONS(IPLACE),DONNEE,ILIGD,ICOLD,
     &                               NBTITR,MXPARA(IPLACE),PARA,NBPARA)
            CALL DISMOI('C','TYPE_CHAMP',PARA(1),'CHAMP',IBID,
     &                  CBID,IERD)
            IF (CBID(1:4) .EQ. 'ELNO') THEN
               CGEN = 'AUX NOEUDS'
               IGEN = 10
            ELSEIF (CBID(1:4) .EQ. 'ELGA') THEN
               CGEN = 'AUX POINTS DE GAUSS'
               IGEN = 19
            ELSEIF (CBID(1:4) .EQ. 'ELEM') THEN
               CGEN = 'CONSTANT SUR L''ELEMENT'
               IGEN = 22
            ELSE
               CGEN = 'EN '//CBID(1:4)
               IGEN = 7
            ENDIF
         GOTO 9000
C
C        --- NOM SYMBOLIQUE POUR UN CHAMP D'UN RESULTAT ---
  170    CONTINUE
            CALL TITREC(DEMONS(IPLACE),DONNEE,ILIGD,ICOLD,
     &                               NBTITR,MXPARA(IPLACE),PARA,NBPARA)
            CALL RSUTOR(PARA(1)(1:8),PARA(2)(1:19),K16BID,IBID,IRET)
            CALL ASSERT(IRET.EQ.1)
            IF (IRET.EQ.1) THEN
               CGEN = K16BID
               IGEN = LXLGUT(K16BID)
            ELSE
              CALL U2MESK('A','UTILITAI5_91',1,PARA(1))
            ENDIF
         GOTO 9000
C
C        --- NUMERO D'ORDRE POUR UN CHAMP D'UN RESULTAT ---
  180    CONTINUE
            CALL TITREC(DEMONS(IPLACE),DONNEE,ILIGD,ICOLD,
     &                               NBTITR,MXPARA(IPLACE),PARA,NBPARA)
            CALL RSUTOR(PARA(1)(1:8),PARA(2)(1:19),K16BID,IBID,IRET)
            CALL ASSERT(IRET.EQ.1)
            IF (IRET.EQ.1 ) THEN
               CALL CODENT(IBID,'G',CGEN(1:16))
               IGEN = LXLGUT(CGEN(1:16))
            ELSE
               CALL U2MESK('A','UTILITAI5_91',1,PARA(1))
            ENDIF
         GOTO 9000
C
C        --- ACCES ---
  190    CONTINUE
            CALL TITREC(DEMONS(IPLACE),DONNEE,ILIGD,ICOLD,
     &                               NBTITR,MXPARA(IPLACE),PARA,NBPARA)
            CALL RSNOPA(PARA(1)(1:8),0,'&&TITREB.NOM_ACCE',NBACCE,NBPA)
            CALL JEEXIN('&&TITREB.NOM_ACCE',IRET)
            IF (IRET.GT.0)  CALL JEVEUO('&&TITREB.NOM_ACCE','E',JPARA)
            CALL RSUTOR(PARA(1)(1:8),PARA(2)(1:19),K16BID,IBID,IRET)
            CALL ASSERT(IRET.EQ.1)
            IF (IRET.NE.1 ) THEN
              CALL U2MESK('A','UTILITAI5_91',1,PARA(1))
              GOTO 9001
            ENDIF
            DO 191  IACC=1,NBACCE
               CALL GETTCO(PARA(1)(1:8),TYSD)
               ILG = LXLGUT(ZK16(JPARA-1+IACC))
               CGEN(IGEN+1:IGEN+ILG) = ZK16(JPARA-1+IACC)
               CGEN(IGEN+ILG+1:IGEN+ILG+1) = ':'

               CALL RSADPA(PARA(1)(1:8),'L',1,ZK16(JPARA-1+IACC),
     &                                               IBID,1,IAD,CTYPE)
C              TEST SUR LE TYPE DU CONCEPT
C              MODE_MECA A 3 VAR D'ACCES (FREQ,NUME_MODE,et NOEUD_CMP)
               IF(TYSD.EQ.'MODE_MECA') THEN
                  IF (ZK16(JPARA-1+IACC).EQ.'FREQ') THEN
                    IF(ZR(IAD).EQ.R8VIDE())THEN
                      LFREQ= .TRUE.
                    ELSE
                      LFREQ = .FALSE.
                    ENDIF
                  ENDIF

                  IPOSA = IGEN+1
                  IPOSB = IGEN+ILG
                  IPOSC = IGEN+ILG+1

                  IF (.NOT.LFREQ) THEN
C                   PAS D'AFFICHAGE DU TEXTE NOEUD_CMP
                    IF (ZK16(JPARA-1+IACC).EQ.'NOEUD_CMP')THEN
                      CGEN(IPOSA:IPOSB) = ' '
                      CGEN(IPOSC:IPOSC) = ' '
                    ENDIF
                  ELSE
C                   PAS D'AFFICHAGE DU TEXTE FREQ,ET NUME_MODE
                    IF (ZK16(JPARA-1+IACC).EQ.'FREQ')THEN
                      CGEN(IPOSA:IPOSB) = ' '
                      CGEN(IPOSC:IPOSC) = ' '
                    ELSE IF (ZK16(JPARA-1+IACC).EQ.'NUME_MODE')THEN
                      CGEN(IPOSA:IPOSB) = ' '
                      CGEN(IPOSC:IPOSC) = ' '
                    ENDIF
                  ENDIF

                  IGEN = IGEN+ILG+2
                  IF ((CTYPE(1:1).EQ.'I').AND.(.NOT.LFREQ)) THEN
C                   ENTIER
                    CALL CODENT(ZI(IAD),'G',CBID)
                    ILG = LXLGUT(CBID)
                    CGEN(IGEN+1:IGEN+ILG) = CBID
                    IGEN = IGEN+ILG+1
                  ELSE IF ((CTYPE(1:1).EQ.'R').AND.(.NOT.LFREQ)) THEN
C                   REEL
                    ILG = 12
                    WRITE(CGEN(IGEN+1:IGEN+ILG),'(1PE12.5)') ZR(IAD)
                    IGEN = IGEN+ILG+1
                  ELSE IF ((CTYPE(1:3).EQ.'K16').AND.(LFREQ)) THEN
C                   K16
                    ILG = 16
                    WRITE(CGEN(IGEN+1:IGEN+ILG),'(A)') ZK16(IAD)
                    IGEN = IGEN+ILG+1
                  ELSE
                    IF((CTYPE(1:3).EQ.'K16').OR.(CTYPE(1:1).EQ.'I').OR.
     &                 (CTYPE(1:1).EQ.'R'))THEN
                      GOTO 191
                    ENDIF
                    CALL ASSERT(.FALSE.)
                  ENDIF

               ELSE

                  IGEN = IGEN+ILG+2
                  IF (CTYPE(1:1).EQ.'I') THEN
C                   ENTIER
                    CALL CODENT(ZI(IAD),'G',CBID)
                    ILG = LXLGUT(CBID)
                    CGEN(IGEN+1:IGEN+ILG) = CBID
                    IGEN = IGEN+ILG+1
                  ELSE IF (CTYPE(1:1).EQ.'R') THEN
C                   REEL
                    ILG = 12
                    WRITE(CGEN(IGEN+1:IGEN+ILG),'(1PE12.5)') ZR(IAD)
                    IGEN = IGEN+ILG+1
                  ELSE IF (CTYPE(1:2).EQ.'K8') THEN
C                   K8
                    ILG = 8
                    WRITE(CGEN(IGEN+1:IGEN+ILG),'(A)') ZK8(IAD)
                    IGEN = IGEN+ILG+1
                  ELSE IF (CTYPE(1:3).EQ.'K16') THEN
C                   K16
                    ILG = 16
                    WRITE(CGEN(IGEN+1:IGEN+ILG),'(A)') ZK16(IAD)
                    IGEN = IGEN+ILG+1
                  ELSE IF (CTYPE(1:3).EQ.'K24') THEN
C                   K24
                    ILG = 24
                    WRITE(CGEN(IGEN+1:IGEN+ILG),'(A)') ZK24(IAD)
                    IGEN = IGEN+ILG+1
                  ELSE IF (CTYPE(1:3).EQ.'K32') THEN
C                   K32
                    ILG = 32
                    WRITE(CGEN(IGEN+1:IGEN+ILG),'(A)') ZK32(IAD)
                    IGEN = IGEN+ILG+1
                  ELSE IF (CTYPE(1:3).EQ.'K80') THEN
C                   K80
                    ILG = 80
                    WRITE(CGEN(IGEN+1:IGEN+ILG),'(A)') ZK80(IAD)
                    IGEN = IGEN+ILG+1
                  ELSE IF (CTYPE(1:1).EQ.'C') THEN
                    CALL ASSERT(.FALSE.)
                  ELSE
                    CALL ASSERT(.FALSE.)
                  ENDIF
              ENDIF
  191       CONTINUE
            CALL JEDETR('&&TITREB.NOM_ACCE')
         GOTO 9000
C
C        --- VALEUR PARAMETRE ---
  200    CONTINUE
            CALL TITREC(DEMONS(IPLACE),DONNEE,ILIGD,ICOLD,
     &                               NBTITR,MXPARA(IPLACE),PARA,NBPARA)
            IDEB = 1
            DO 210 IUTI = 1,2
               CALL JEEXIN(PARA(IUTI),IRET)
               IF (IRET.EQ.0) THEN
CCC   CONCEPT INEXISTANT
                 GOTO 210
               ENDIF
               CALL JELIRA(PARA(IUTI),'TYPE',IVAL,CVAL)
               CALL JEVEUO(PARA(IUTI),'L',JAD)
               IF (CVAL(1:1).EQ.'R') THEN
                  RBID = ZR(JAD)
                  WRITE(CGEN(IDEB:),'(1PE12.5)') RBID
                  IGEN = LXLGUT(CGEN)
                  IDEB = IGEN+1
               ELSE IF (CVAL(1:1).EQ.'I') THEN
                  IBID = ZI(JAD)
                  CALL CODENT(IBID,'G',CGEN(IDEB:))
                  IGEN = LXLGUT(CGEN)
                  IDEB = IGEN+1
               ELSE IF (CVAL(1:1).EQ.'K') THEN
                  CALL JELIRA(PARA(IUTI),'LTYP',IVAL,CVAL)
                  IF (IVAL.EQ.80) THEN
                     CGEN(IDEB:) = ZK80(JAD-1+1)
                     IGEN = LXLGUT(CGEN)
                     IDEB = IGEN+1
                  ELSEIF (IVAL.EQ.32) THEN
                     CGEN(IDEB:) = ZK32(JAD-1+1)
                     IGEN = LXLGUT(CGEN)
                     IDEB = IGEN+1
                  ELSEIF (IVAL.EQ.24) THEN
                     CGEN(IDEB:) = ZK24(JAD-1+1)
                     IGEN = LXLGUT(CGEN)
                     IDEB = IGEN+1
                  ELSEIF (IVAL.EQ.16) THEN
                     CGEN(IDEB:) = ZK16(JAD-1+1)
                     IGEN = LXLGUT(CGEN)
                     IDEB = IGEN+1
                  ELSEIF (IVAL.EQ.8 ) THEN
                     CGEN(IDEB:) = ZK8(JAD-1+1)
                     IGEN = LXLGUT(CGEN)
                     IDEB = IGEN+1
                  ENDIF
               ENDIF
 210        CONTINUE
            IGEN = LXLGUT(CGEN)
         GOTO 9000
      ENDIF
C     ------------------------------------------------------------------
 9000 CONTINUE
      IF ( IGEN .GT. 0 ) THEN
C        --- Y A T IL ASSEZ DE PLACE ---
         ICOLS = ICOLS + 1
         IF ( ICOLS+IGEN-1 .GT. LEN(SORTIE(1)) )  THEN
            ILIGS = ILIGS + 1
            ICOLS = 1
         ENDIF
         SORTIE(ILIGS)(ICOLS:) = CGEN(1:IGEN)
         ICOLS = ICOLS + IGEN - 1
      ENDIF
      GOTO 9999
C     ------------------------------------------------------------------
 9001 CONTINUE
      ILG = LXLGUT(PARA(1))
      CALL U2MESK('A','UTILITAI_99',1,PARA(1)(1:ILG))
 9999 CONTINUE
      CALL JEDEMA()
      END

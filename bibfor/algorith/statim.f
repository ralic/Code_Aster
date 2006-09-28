      SUBROUTINE STATIM ( NBOBST, NBPT, TEMPS, FCHO, VGLI,
     &                    DEFPLA,  WK1, WK2, WK3, TDEBUT, TFIN,
     &                    NBLOC, OFFSET,TREPOS, NBCLAS, NOECHO,
     &                    INTITU, NOMRES )
      IMPLICIT     REAL*8 (A-H,O-Z)
      INTEGER       NBOBST, NBPT, NBLOC
      REAL*8        TEMPS(*), FCHO(*), VGLI(*), TDEBUT, TFIN
      REAL*8        WK1(*), WK2(*), WK3(*), FNMAXA, FNMETY, FNMMOY
      REAL*8        OFFSET, TREPOS, DEFPLA(*)
      CHARACTER*8   NOECHO(*), INTITU(*)
      CHARACTER*(*) NOMRES
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C-----------------------------------------------------------------------
C     CALCUL ET IMPRESSION DES STATISTIQUES DE CHOC
C
C     NBOBST       : NB DE NOEUDS DE CHOC
C     NBPT         : NB DE PAS DE TEMPS TEMPORELS ARCHIVES
C     NBLOC        : NB DE BLOCS POUR LE MOYENNAGE
C     TEMPS        : INSTANTS DE CALCUL
C     FCHO         : VECTEUR DES FORCES DE CHOC
C     VGLI         : VECTEUR DES VITESSES DE GLISSEMENT
C     DEFPLA       : ECRASEMENT RESIDUEL CUMULE
C     NBCLAS       : NOMBRE DE CLASSES
C
C-----------------------------------------------------------------------
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
      INTEGER      IBID, NBPARA, NPARG, NPARP, NPARF
      PARAMETER    ( NPARG = 6 , NPARP = 7 , NPARI = 10 , NBPARA = 20 )
      PARAMETER    ( NPARF = 7 )
      REAL*8       PARA(3)
      COMPLEX*16   C16B
      CHARACTER*4  TPARA(NBPARA)
      CHARACTER*8  K8B, NOEUD
      CHARACTER*16 TVAR(4), LPARI(NPARI), LPARG(NPARG), LPARP(NPARP)
      CHARACTER*16 VALEK(4), NPARA(NBPARA), LPARF(NPARF)
C
      DATA TVAR  / 'IMPACT' , 'GLOBAL' , 'PROBA'  , 'FLAMBAGE' /
C
      DATA NPARA / 'INTITULE','NOEUD', 'CALCUL'       , 'CHOC'         ,
     &             'INSTANT'       , 'F_MAX'         , 'IMPULSION'    ,
     &             'T_CHOC'        , 'V_IMPACT'      , 'NB_IMPACT'    ,
     &             'F_MAX_ABS'     , 'F_MAX_MOY'     , 'F_MAX_ETYPE'  ,
     &             'CLASSE'        , 'DEBUT'         , 'FIN'          ,
     &             'PROBA'         , 'FLAMBAGE'      , 'ECRAS_RESI'   ,
     &             'INST_FLAMB'    /
      DATA TPARA / 'K8', 'K8'      , 'K16'           , 'I'            ,
     &             'R'             , 'R'             , 'R'            ,
     &             'R'             , 'R'             , 'I'            ,
     &             'R'             , 'R'             , 'R'            ,
     &             'I'             , 'R'             , 'R'            ,
     &             'R'             , 'K8'            , 'R'            ,
     &             'R'             /
C
      DATA LPARI / 'INTITULE','NOEUD', 'CALCUL'      , 'CHOC'         ,
     &             'INSTANT'       , 'F_MAX'         , 'IMPULSION'    ,
     &             'T_CHOC'        , 'V_IMPACT'      , 'NB_IMPACT'    /
C
      DATA LPARG / 'INTITULE'      ,'NOEUD'          ,'CALCUL'      ,
     &             'F_MAX_ABS'     , 'F_MAX_MOY'     , 'F_MAX_ETYPE'  /
C
      DATA LPARP / 'INTITULE','NOEUD','CALCUL'       , 'CLASSE'        ,
     &             'DEBUT'         , 'FIN'           , 'PROBA'         /
      DATA LPARF / 'INTITULE','NOEUD','CALCUL'       , 'CHOC'         ,
     &             'FLAMBAGE'      , 'ECRAS_RESI'    , 'INST_FLAMB'    /
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      DT = (TEMPS(NBPT)-TEMPS(1))/(NBPT-1)
      IDEBUT = INT((TDEBUT-TEMPS(1))/DT)+1
      IFIN   = MIN(INT((TFIN-TEMPS(1))/DT)+1,NBPT)
      NBPAS  = IFIN - IDEBUT + 1
C
      IF ( NBLOC .EQ. 0 ) NBLOC = 1
      IF ( NBLOC .GT. 1 ) THEN
        CALL U2MESS('I','ALGORITH10_76')

        NBLOC = 1
      ENDIF
      IF ( NBCLAS .EQ. 0 ) NBCLAS = 10
C
      CALL TBCRSD ( NOMRES, 'G' )
      CALL TBAJPA ( NOMRES, NBPARA, NPARA, TPARA )
C
C     BOUCLE SUR LES NOEUDS DE CHOC
C
      DO 10 I = 1,NBOBST
        NOEUD = NOECHO(I)
        VALEK(1) = INTITU(I)
        VALEK(2) = NOEUD
        VALEK(3) = TVAR(1)
C
        NBCHOC = 0
        CALL DCOPY ( NBPAS, FCHO(3*(I-1)+1), 3*NBOBST, WK1, 1 )
        CALL DCOPY ( NBPAS, VGLI(3*(I-1)+1), 3*NBOBST, WK2, 1 )
        CALL IMPACT ( NOMRES, NBPAS, WK1(IDEBUT), WK2(IDEBUT), WK3,
     &                OFFSET, TEMPS(IDEBUT), TREPOS, NBCHOC, FNMAXA,
     &                FNMMOY, FNMETY, NPARI, LPARI, VALEK )
C
        VALEK(3) = TVAR(2)
        PARA(1) = FNMAXA
        PARA(2) = FNMMOY
        PARA(3) = FNMETY
        CALL TBAJLI ( NOMRES, NPARG, LPARG,
     &                           IBID, PARA, C16B, VALEK, 0 )
C
        NDEC = NBCLAS
        FMIN = 1.D50
        FMAX = -FMIN
        CALL HISTOG ( NBCHOC, WK3, FMIN, FMAX, WK2, WK1, NDEC )
C
        VALEK(3) = TVAR(3)
        DO 30 IDEC = 1 , NDEC
           IF (IDEC.EQ.1) THEN
              PARA(1) = FMIN
           ELSE
              PARA(1) = WK2(IDEC-1)
           ENDIF
           PARA(2) = WK2(IDEC)
           PARA(3) = WK1(IDEC)
           CALL TBAJLI ( NOMRES, NPARP, LPARP,
     &                           IDEC, PARA, C16B, VALEK, 0 )
 30     CONTINUE
C
C       --- AJOUT FLAMBAGE SI CELUI-CI A EU LIEU ---
        IF (DEFPLA(NBOBST*(NBPAS-1)+I) .GT. 0.D0) THEN
           VALEK(3) = TVAR(4)
           VALEK(4) = 'OUI'
           PARA(1) = DEFPLA(NBOBST*(NBPAS-1)+I)
           IPAS = 1
 40        CONTINUE
           IF (DEFPLA(NBOBST*(IPAS-1)+I) .GT. 0) THEN
              PARA(2) = TEMPS(IPAS)
           ELSE
              IPAS = IPAS + 1
              IF (IPAS.LE.NBPAS) GOTO 40
           ENDIF
           IF (NOEUD .NE. NOECHO(NBOBST+I)) THEN
C          --- CAS CHOC ENTRE 2 NOEUDS : ON REPARTIT DEFPLA ---
              PARA(1) = PARA(1)/2.D0
C             --- 1ER NOEUD ---
              CALL TBAJLI(NOMRES, NPARF, LPARF, I, PARA, C16B, VALEK, 0)
C             --- 2EME NOEUD ---
              VALEK(1) = INTITU(NBOBST+I)
              VALEK(2) = NOECHO(NBOBST+I)
              CALL TBAJLI(NOMRES, NPARF, LPARF, I, PARA, C16B, VALEK, 0)
           ELSE
              CALL TBAJLI(NOMRES, NPARF, LPARF, I, PARA, C16B, VALEK, 0)
           ENDIF
        ENDIF
C
 10   CONTINUE
C
      CALL JEDEMA()
      END

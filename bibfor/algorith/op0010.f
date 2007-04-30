      SUBROUTINE OP0010(IER)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 30/04/2007   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE MASSIN P.MASSIN
C
      IMPLICIT NONE
      INTEGER           IER
C      
C ----------------------------------------------------------------------
C
C OPERATEUR PROPA_XFEM
C
C CALCUL DE LA FISSURE APRES PROPAGATION AU PAS DE TEMPS SUIVANT
C
C ----------------------------------------------------------------------
C
C
C OUT IER   : CODE RETOUR ERREUR COMMANDE
C               IER = 0 => TOUT S'EST BIEN PASSE
C               IER > 0 => NOMBRE D'ERREURS RENCONTREES
C
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
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
      CHARACTER*32    JEXATR
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER        IFM,NIV,IBID,NDIM,
     &               JCARAF,CLSM,JCONX1,
     &               JCONX2,NBMA
      INTEGER        IADRMA,JDIME
      REAL*8         LCMIN,CFLPRO,DELTAT
      CHARACTER*8    K8BID,NOMA,NOMO,FISS,FISPRE
      INTEGER        NFISS,JFISS,JNFIS
      CHARACTER*16   K16BID
      CHARACTER*19   CNSVT,CNSVN,GRLT,GRLN,CNSLT,CNSLN,CNSEN,CNSBAS,
     &               CNSENR,NOESOM,ISOZRO,NORESI,CNXINV
      CHARACTER*24   LISMAE,LISNOE,SDCONT
      INTEGER        JXC,JXSDC
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFMAJ()      
      CALL INFDBG('XFEM',IFM,NIV)  
C
C --- NOM DU CONCEPT FISSURE
C
      CALL GETRES(FISS,K16BID,K16BID)
C
C --- NOM DU MODELE
C      
      CALL GETVID(' ','MODELE',1,1,1,NOMO,IBID)
C
C --- NOM DU MAILLAGE ATTACHE AU MODELE
C
      CALL JEVEUO(NOMO(1:8)//'.MODELE    .NOMA','L',IADRMA)
      NOMA  = ZK8(IADRMA)
C
C --- DIMENSION DU PROBLEME
C
      CALL JEVEUO(NOMA//'.DIME','L',JDIME)
      NDIM  = ZI(JDIME-1+6)
      IF ((NDIM.LT.2).OR.(NDIM.GT.3)) THEN
        CALL U2MESS('F','XFEM_18')
      ENDIF
      CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMA,K8BID,IBID)
      CALL JEVEUO(NOMA(1:8)//'.CONNEX','L',JCONX1)
      CALL JEVEUO(JEXATR(NOMA(1:8)//'.CONNEX','LONCUM'),'L',JCONX2)
C
C --- CONNECTIVITE INVERSEE
C
      CNXINV = '&&XPRREO.CNCINV'
      CALL CNCINV(NOMA,IBID,0,'V',CNXINV)
C      
C --- RECUPERATION DE LA FISSURE PRECEDENTE
C
      CALL JEVEUO(NOMO//'.FISS','L',JFISS)
      FISPRE = ZK8(JFISS)
C      
C --- ACCES AUX OBJETS POUR MULTIFISSURATION DANS 
C
      CALL JEVEUO(NOMO//'.NFIS','L',JNFIS)
      NFISS  = ZI(JNFIS)  
      IF (NFISS.GT.1) THEN
        CALL U2MESS('F','XFEM2_6')
      ENDIF
C
C --- PRISE EN COMPTE DU CONTACT
C      
      CALL JEVEUO(NOMO(1:8)//'.CONT'  ,'L',JXC)
      CALL JEVEUO(NOMO(1:8)//'.SDCONT','L',JXSDC)
      SDCONT = ZK24(JXSDC)
      IF (SDCONT(1:1).NE.' ') THEN            
        CALL XCONTA(NOMA  ,NOMO  ,NDIM  ,NFISS,FISS  ,
     &              SDCONT)      
      ENDIF      
C
C --- RECUPERATION DES LEVEL SETS ET GRADIENTS
C
      CNSLT = '&&OP0010.CNSLT'
      CNSLN = '&&OP0010.CNSLN'
      GRLT  = '&&OP0010.GRLT'
      GRLN  = '&&OP0010.GRLN'
      CALL CNOCNS(FISPRE//'.LTNO','V',CNSLT)
      CALL CNOCNS(FISPRE//'.LNNO','V',CNSLN)
      CALL CNOCNS(FISPRE//'.GRLTNO','V',GRLT)
      CALL CNOCNS(FISPRE//'.GRLNNO','V',GRLN)
C
C --- DUPLICATION DES GROUP_MA_ENRI ET GROUP_NO_ENRI
C
      LISMAE = FISS//'.GROUP_MA_ENRI'
      LISNOE = FISS//'.GROUP_NO_ENRI'
      CALL JEDUPO(FISPRE//'.GROUP_MA_ENRI','G',LISMAE,.FALSE.)
      CALL JEDUPO(FISPRE//'.GROUP_NO_ENRI','G',LISNOE,.FALSE.)
C
C --- RECUPERATION DES CARACTERISTIQUES DU FOND DE FISSURE
C
      CALL JEDUPO(FISPRE//'.CARAFOND','G',FISS//'.CARAFOND',.FALSE.)
      CALL JEVEUO(FISS//'.CARAFOND','L',JCARAF)

C-----------------------------------------------------------------------
C     CALCUL DES CHAM_NO_S DES VITESSES DE PROPAGATION
C-----------------------------------------------------------------------
C      IF (NIV.GT.1)
      WRITE(IFM,*)
      WRITE(IFM,*)'OP0010-1) CALCUL DU CHAMP DE VITESSE AUX NOEUDS'
C      IF (NIV.GT.1)
      WRITE(IFM,901)

      CNSVT='&&OP0010.CNSVT'
      CNSVN='&&OP0010.CNSVN'

      CALL XPRVIT(NOMA,FISPRE,CNSVT,CNSVN)

C-----------------------------------------------------------------------
C     CALCUL DES LONGUEURS CARACTERISTIQUES ET DES CONDITIONS CFL
C-----------------------------------------------------------------------
C      IF (NIV.GT.1)
      WRITE(IFM,*)
      WRITE(IFM,*) 'OP0010-2) '//
     &      'CALCUL DES CONDITIONS CFL ET LONGUEURS CARACTERISTIQUES'
C      IF (NIV.GT.1)
      WRITE(IFM,902)

      CALL XPRCFL(NOMO,CNSVT,CFLPRO,LCMIN)

C-----------------------------------------------------------------------
C     AJUSTEMENT DE VT
C-----------------------------------------------------------------------
C      IF (NIV.GT.1)
      WRITE(IFM,*)
      WRITE(IFM,*)'OP0010-3) AJUSTEMENT DU CHAMP DES VITESSES VN'
C      IF (NIV.GT.1)
      WRITE(IFM,903)

      DELTAT=CFLPRO
      CALL XPRAJU(NOMA,CNSLT,CNSVT,CNSVN,DELTAT)

C-----------------------------------------------------------------------
C     PROPAGATION DES LEVEL SETS
C-----------------------------------------------------------------------
C      IF (NIV.GT.1)
      WRITE(IFM,*)
      WRITE(IFM,*)'OP0010-4) PROPAGATION DES LEVEL SETS'
C      IF (NIV.GT.1)
      WRITE(IFM,904)

      CALL XPRLS(NOMO,NOMA,CNSLN,CNSLT,GRLN,GRLT,CNSVT,CNSVN,DELTAT)

      CALL JEDETR(CNSVT)
      CALL JEDETR(CNSVN)

C-----------------------------------------------------------------------
C     INITIALISATION DES PARAMETRES DE XPRREI ET XPRREO
C-----------------------------------------------------------------------
C      IF (NIV.GT.1)
      WRITE(IFM,*)
      WRITE(IFM,*)'OP0010-5) REINITIALISATION DE LSN'
C      IF (NIV.GT.1)
      WRITE(IFM,905)
      NOESOM = '&&OP0010.NOESOM'
      NORESI = '&&OP0010.NORESI'

      CALL XPRINI(NOMO,NOMA,FISPRE,FISS,CNSLN,CNSLT,GRLT,NOESOM,NORESI)

C-----------------------------------------------------------------------
C     REINITIALISATION DE LSN
C-----------------------------------------------------------------------

      DELTAT = LCMIN*0.9D0
      ISOZRO = '&&OP0010.ISOZRO'

      CALL XPRREI(NOMO,NOMA,FISS,NOESOM,NORESI,CNSLN,CNSLT,GRLN,DELTAT,
     &            LCMIN,'LN',ISOZRO,CNXINV)

C-----------------------------------------------------------------------
C     REORTHOGONALISATION DE LST
C-----------------------------------------------------------------------
C      IF (NIV.GT.1)
      WRITE(IFM,*)
      WRITE(IFM,*)'OP0010-6) REORTHOGONALISATION DE LST'
C      IF (NIV.GT.1)
      WRITE(IFM,906)

      CALL XPRREO(NOMO,NOMA,FISS,NOESOM,NORESI,CNSLN,CNSLT,GRLN,GRLT,
     &            DELTAT,LCMIN,ISOZRO,CNXINV)
      CALL JEDETR(ISOZRO)
C-----------------------------------------------------------------------
C     REINITIALISATION DE  LST
C-----------------------------------------------------------------------
C      IF (NIV.GT.1)
      WRITE(IFM,*)
      WRITE(IFM,*)'OP0010-7) REINITIALISATION DE LST'
C      IF (NIV.GT.1)
      WRITE(IFM,907)

      CALL XPRREI(NOMO,NOMA,FISS,NOESOM,NORESI,CNSLT,CNSLT,GRLT,DELTAT,
     &            LCMIN,'LT',ISOZRO,CNXINV)
      CALL JEDETR(ISOZRO)
      CALL JEDETR(NOESOM)

C-----------------------------------------------------------------------
C     REAJUSTEMENT DES LEVEL SETS TROP PROCHES DE 0
C-----------------------------------------------------------------------
C      IF (NIV.GT.1)
      WRITE(IFM,*)
      WRITE(IFM,*)'OP0010-8) ENRICHISSEMENT DE LA SD FISS_XFEM'
C      IF (NIV.GT.1)
      WRITE(IFM,908)

      CALL XAJULS(IFM,NOMA,NBMA,CNSLT,CNSLN,JCONX1,JCONX2,CLSM)

      WRITE(IFM,*)'NOMBRE DE LEVEL SET REAJUSTEES APRES CONTROLE:',
     &            CLSM

C-----------------------------------------------------------------------
C     EXTENSION DES LEVEL SETS AUX NOEUDS MILIEUX
C-----------------------------------------------------------------------

      CALL XPRMIL(NOMA,CNSLT,CNSLN)

      CALL CNSCNO(CNSLT,' ','NON','G',FISS//'.LTNO')
      CALL CNSCNO(CNSLN,' ','NON','G',FISS//'.LNNO')
      CALL CNSCNO(GRLT,' ','NON','G',FISS//'.GRLTNO' )
      CALL CNSCNO(GRLN,' ','NON','G',FISS//'.GRLNNO' )

C----------------------------------------------------------------------+
C                 FIN DE LA PARTIE PROPAGATION :                       |
C                 ----------------------------                         |
C    LA SD FISS_XFEM EST ENRICHIE COMME DANS OP0041 : DEFI_FISS_XFEM   |
C   ( TOUTE MODIF. AFFECTANT OP0041 DOIT ETRE REPERCUTEE PLUS BAS,     |
C     EXCEPTE L'APPEL A SDCONX )                                       |
C----------------------------------------------------------------------+

C-----------------------------------------------------------------------
C     CALCUL DE L'ENRICHISSEMENT ET DES POINTS DU FOND DE FISSURE
C-----------------------------------------------------------------------

      CNSEN='&&OP0010.CNSEN'
      CNSENR='&&OP0010.CNSENR'
      IF (NDIM .EQ. 3) THEN
        CALL XNRCH3(NOMA,CNSLT,CNSLN,CNSEN,CNSENR,
     &              FISS,LISMAE,LISNOE)
      ELSEIF (NDIM .EQ. 2) THEN
        CALL XNRCH2(NOMA,CNSLT,CNSLN,CNSEN,CNSENR,
     &              FISS,LISMAE,LISNOE)
      ENDIF

      CALL CNSCNO(CNSENR,' ','NON','G',FISS//'.STNOR')
      CALL CNSCNO(CNSEN,' ','NON','G',FISS//'.STNO')

C-----------------------------------------------------------------------
C     CALCUL DE LA BASE LOCALE AU FOND DE FISSURE
C-----------------------------------------------------------------------

      CNSBAS='&&OP0010.CNSBAS'
      IF (NDIM .EQ. 3) THEN
        CALL XBASLO(NOMO,NOMA,FISS ,GRLT,GRLN,CNSBAS)
      ELSEIF (NDIM .EQ. 2) THEN
        CALL XBASL2(NOMO,NOMA,FISS ,GRLT,GRLN,CNSBAS)
      ENDIF

      CALL CNSCNO(CNSBAS,' ','NON','G',FISS//'.BASLOC')
      CALL DETRSD('CHAM_NO_S',CNSBAS)


C-----------------------------------------------------------------------
C     FIN
C-----------------------------------------------------------------------
      CALL JEDETR(CNXINV)

 901  FORMAT (10X,37('-'))
 902  FORMAT (10X,55('-'))
 903  FORMAT (10X,35('-'))
 904  FORMAT (10X,26('-'))
 905  FORMAT (10X,23('-'))
 906  FORMAT (10X,26('-'))
 907  FORMAT (10X,23('-'))
 908  FORMAT (10X,33('-'))

      CALL JEDEMA()
      END

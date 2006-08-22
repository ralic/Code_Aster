      SUBROUTINE OP0010(IER)
      IMPLICIT NONE
      INTEGER           IER
   
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/08/2006   AUTEUR MASSIN P.MASSIN 
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
C     ------------------------------------------------------------------
C                       OPERATEUR PROPA_XFEM :
C     CALCUL DE LA FISSURE APRES PROPAGATION AU PAS DE TEMPS SUIVANT
C     ------------------------------------------------------------------
C     OUT : IER = NOMBRE D'ERREURS RENCONTREES
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
      CHARACTER*32    JEXNUM,JEXATR
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      
      INTEGER        I,IFM,NIV,IBID,IRET,IRET2,NDIM,ADDIM,JFISS,JMAIL,
     &               JCARAF,JCMCF,JFON,NMAEN1,NMAEN2,NMAEN3,CLSM,JCONX1,
     &               JCONX2,NBMA,NBMAE
      REAL*8         LCMIN,CFLPRO,DELTAT,RAYON,PFI(3),VOR(3),
     &               ORI(3),NORME
      CHARACTER*8    K8B,MODEL,NOMA,FISS,FISPRE,ALGOLA
      CHARACTER*16   K16B
      CHARACTER*19   CNSVT,CNSVN,GRLT,GRLN,CNSLT,CNSLN,CNSEN,CNSBAS,
     &               CNSENR,NOESOM,ISOZRO,NORESI,CNXINV
      CHARACTER*24   OBJMA,CHFOND,LISMAE,LISNOE
      COMPLEX*16     CBID

C-----------------------------------------------------------------------
C     DEBUT
C-----------------------------------------------------------------------

      CALL JEMARQ()
      CALL INFMAJ()
      CALL INFNIV(IFM,NIV)

      CALL GETRES(FISS,K16B,K16B)

C  RECUPERATION DU MODELE, DU MAILLAGE ET DE SES CARACTERISTIQUES
      CALL GETVID(' ','MODELE',1,1,1,MODEL,IBID)
      OBJMA = MODEL//'.MODELE    .NOMA'
      CALL JEVEUO(OBJMA,'L',JMAIL)
      NOMA = ZK8(JMAIL)
      CALL JEVEUO(NOMA//'.DIME','L',ADDIM)
      NDIM=ZI(ADDIM-1+6)
      CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMA,K8B,IRET)
      CALL JEVEUO(NOMA//'.CONNEX','L',JCONX1)
      CALL JEVEUO(JEXATR(NOMA//'.CONNEX','LONCUM'),'L',JCONX2)

C  CONNECTIVITE INVERSEE
      CNXINV = '&&XPRREO.CNCINV'
      CALL CNCINV (NOMA,IBID,0,'V',CNXINV)
      
C  RECUPERATION DE LA FISSURE PRECEDENTE
      CALL JEVEUO(MODEL//'.FISS','L',JFISS)
      FISPRE = ZK8(JFISS)
      
C  RECUPERATION DES LEVEL SETS ET GRADIENTS
      CNSLT = '&&OP0010.CNSLT'
      CNSLN = '&&OP0010.CNSLN'
      GRLT = '&&OP0010.GRLT'
      GRLN = '&&OP0010.GRLN'
      CALL CNOCNS(FISPRE//'.LTNO','V',CNSLT)
      CALL CNOCNS(FISPRE//'.LNNO','V',CNSLN)
      CALL CNOCNS(FISPRE//'.GRLTNO','V',GRLT)
      CALL CNOCNS(FISPRE//'.GRLNNO','V',GRLN)
      
C  DUPLICATION DES GROUP_MA_ENRI ET GROUP_NO_ENRI
      LISMAE = FISS//'.GROUP_MA_ENRI'
      LISNOE = FISS//'.GROUP_NO_ENRI'
      CALL JEDUPO(FISPRE//'.GROUP_MA_ENRI','G',LISMAE,.FALSE.)
      CALL JEDUPO(FISPRE//'.GROUP_NO_ENRI','G',LISNOE,.FALSE.)

C  RECUPERATION DES CARACTERISTIQUES DU FOND DE FISSURE
      CALL JEDUPO(FISPRE//'.CARAFOND','G',FISS//'.CARAFOND',.FALSE.)
      CALL JEVEUO(FISS//'.CARAFOND','L',JCARAF)
      RAYON = ZR(JCARAF)
      IF (NDIM.EQ.3) THEN
         DO 101 I=1,3
            VOR(I) = ZR(JCARAF+I)
            ORI(I) = ZR(JCARAF+3+I)
 101     CONTINUE
      CALL JEVEUO(FISPRE//'.FONDFISS','L',JFON)
      DO 102 I=1,3
            PFI(I) = ZR(JFON-1+I)
 102  CONTINUE
      ENDIF

C  DUPLICATION DES DONNEES DE CONTACT
      CALL JEDUPO(FISPRE//'.CONTACT.ECPDON','G',FISS//'.CONTACT.ECPDON',
     &            .FALSE.)
      CALL JEDUPO(FISPRE//'.CONTACT.METHCO','G',FISS//'.CONTACT.METHCO',
     &            .FALSE.)
      CALL JEDUPO(FISPRE//'.CONTACT.XFEM','G',FISS//'.CONTACT.XFEM',
     &            .FALSE.)
      
      CALL JEEXIN(FISPRE//'.CONTACT.CARACF',IRET)
      IF (IRET.EQ.0) THEN
         ALGOLA = 'NON'
      ELSE
         CALL JEDUPO(FISPRE//'.CONTACT.CARACF','G',
     &               FISS//'.CONTACT.CARACF',.FALSE.)
         CALL JEVEUO(FISS//'.CONTACT.CARACF','L',JCMCF)
         IF ((ZR(JCMCF-1+9)).EQ.(0.D0)) THEN
            ALGOLA = 'NON'
         ELSEIF ((ZR(JCMCF-1+9)).EQ.(1.D0)) THEN
            ALGOLA = 'VERSION1'
         ELSEIF ((ZR(JCMCF-1+9)).EQ.(2.D0)) THEN
            ALGOLA = 'VERSION2'
         ENDIF
      ENDIF

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

      CALL XPRCFL(MODEL,CNSVT,CFLPRO,LCMIN)
      
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

      CALL XPRLS(MODEL,NOMA,CNSLN,CNSLT,GRLN,GRLT,CNSVT,CNSVN,DELTAT)

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
      
      CALL XPRINI(MODEL,NOMA,FISPRE,FISS,CNSLN,CNSLT,GRLT,NOESOM,NORESI)
      
C-----------------------------------------------------------------------
C     REINITIALISATION DE LSN
C-----------------------------------------------------------------------

      DELTAT = LCMIN*0.9D0
      ISOZRO = '&&OP0010.ISOZRO'

      CALL XPRREI(MODEL,NOMA,FISS,NOESOM,NORESI,CNSLN,CNSLT,GRLN,DELTAT,
     &            LCMIN,'LN',ISOZRO,CNXINV)
      
C-----------------------------------------------------------------------
C     REORTHOGONALISATION DE LST
C-----------------------------------------------------------------------
C      IF (NIV.GT.1) 
      WRITE(IFM,*)
      WRITE(IFM,*)'OP0010-6) REORTHOGONALISATION DE LST'
C      IF (NIV.GT.1) 
      WRITE(IFM,906)

      CALL XPRREO(MODEL,NOMA,FISS,NOESOM,NORESI,CNSLN,CNSLT,GRLN,GRLT,
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

      CALL XPRREI(MODEL,NOMA,FISS,NOESOM,NORESI,CNSLT,CNSLT,GRLT,DELTAT,
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
        CALL NORMEV(VOR,NORME)
        IF (NORME.LT.1.D-10) CALL UTMESS('F','OP0041','LA NORME '//
     &                              'DU VECTEUR VECT_ORIE EST NULLE')
        CALL XNRCH3(IFM,NIV,NOMA,CNSLT,CNSLN,CNSEN,CNSENR,PFI,VOR,ORI,
     &              RAYON,FISS,NMAEN1,NMAEN2,NMAEN3,LISMAE,LISNOE)
      ELSEIF (NDIM .EQ. 2) THEN
        CALL XNRCH2(IFM,NIV,NOMA,CNSLT,CNSLN,CNSEN,CNSENR,
     &              RAYON,FISS,NMAEN1,NMAEN2,NMAEN3,LISMAE,LISNOE)
      ENDIF
      
      CALL CNSCNO(CNSENR,' ','NON','G',FISS//'.STNOR')
      CALL CNSCNO(CNSEN,' ','NON','G',FISS//'.STNO')

C-----------------------------------------------------------------------
C     CALCUL DE LA BASE LOCALE AU FOND DE FISSURE
C-----------------------------------------------------------------------

      CHFOND = FISS//'.FONDFISS'
      CNSBAS='&&OP0010.CNSBAS'
      IF (NDIM .EQ. 3) THEN
        CALL XBASLO(MODEL,NOMA,CHFOND,GRLT,GRLN,CNSBAS)
      ELSEIF (NDIM .EQ. 2) THEN
        CALL XBASL2(MODEL,NOMA,CHFOND,GRLT,GRLN,CNSBAS)
      ENDIF

      CALL CNSCNO(CNSBAS,' ','NON','G',FISS//'.BASLOC')
      CALL DETRSD('CHAM_NO_S',CNSBAS)

C-----------------------------------------------------------------------
C     STRUCTURE DE DONNEES SUR LE CONTACT
C-----------------------------------------------------------------------

C     SEULEMENT S'IL Y A DES MAILLES DE CONTACT
      IF (NMAEN1+NMAEN2+NMAEN3.GT.0) THEN
C       APPEL À L'ALGORITHME DE RESTRICTION DE L'ESPACE DES
C       MULTIPLICATUERS DE LAGRANGE DE CONTACT
        IF (ALGOLA.NE.'NON') THEN 
          WRITE(IFM,*)'ACTIVATION DE L''ALGORITHME DE RESTRICTION DE '//
     &                'L''ESPACE DES MULTIPLICATEURS DE '//
     &                'PRESSION DE CONTACT'
          CALL XLAGSP(ALGOLA,NDIM,NOMA,CNSLT,CNSLN,GRLN,GRLT,FISS)
        ENDIF
      ENDIF

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

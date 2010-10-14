      SUBROUTINE UTDIDT(GETSET,SD,TYPQUE,IOCC,QUEST,VALR,VALI,VALK)
      IMPLICIT      NONE
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 12/10/2010   AUTEUR GENIAUT S.GENIAUT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE GENIAUT S.GENIAUT
C
      INTEGER       IOCC,VALI
      REAL*8        VALR
      CHARACTER*1   GETSET
      CHARACTER*4   TYPQUE
      CHARACTER*(*) QUEST,VALK
      CHARACTER*19  SD
C
C ----------------------------------------------------------------------
C
C
C ROUTINE UTILITAIRE SUR LA DISCRETISATION TEMPORELLE 
C   ACCES AUX SD LOCALES !! ET NON A LA SD DE L'OPERATEUR DEFI_LIST_INST
C   MAIS L'ARCHITECTURE ETANT LA MEME, LES OBJETS DOIVENT ETRE EN 
C   CONFORMITE AVEC LA ROUTINE OP0028 (DEFI_LIST_INST)
C
C ----------------------------------------------------------------------
C
C IN  GETSET : 'L' OU 'E'
C IN  SD     : SDDISC LOCALE A OP00700
C IN  TYPQUE : 
C IN  IOCC   : 
C IN  QUEST  : 
C OUT VALI   : 
C OUT VALR   : 
C IN/OUT VALK   : 
C
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      INTEGER      ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8       ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16   ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL      ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8  ZK8
      CHARACTER*16    ZK16
      CHARACTER*24        ZK24
      CHARACTER*32            ZK32
      CHARACTER*80                ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER      LEEVR,LEEVK,LESUR,LAEVR,LATPR,LATPK
      PARAMETER   (LEEVR=4,LEEVK=3,LESUR=8)
      PARAMETER   (LAEVR=5,LATPR=6,LATPK=4)

      INTEGER      IRET,N,JLIR,I,NOCC
      INTEGER      JEEVR,JEEVK,JESUR,JAEVR,JATPR,JATPK
      CHARACTER*8  K8BID
C
C ----------------------------------------------------------------------
C
      CALL ASSERT(TYPQUE.EQ.'LIST'.OR.
     &            TYPQUE.EQ.'ECHE'.OR.
     &            TYPQUE.EQ.'ADAP')

      CALL ASSERT(GETSET.EQ.'L'.OR.GETSET.EQ.'E')

C     ------------------------------------------------------------------
C                     QUESTION SUR LA LISTE
C     ------------------------------------------------------------------

      IF (TYPQUE.EQ.'LIST') THEN

        CALL ASSERT(QUEST.EQ.'DTMIN' .OR.
     &              QUEST.EQ.'METHODE'.OR.
     &              QUEST.EQ.'PAS_MINI'.OR.
     &              QUEST.EQ.'PAS_MAXI'.OR.
     &              QUEST.EQ.'NB_PAS_MAXI'.OR.
     &              QUEST.EQ.'NBINST'.OR.
     &              QUEST.EQ.'DT-'.OR.
     &              QUEST.EQ.'JINST')

        IF (QUEST.EQ.'METHODE') THEN
          CALL JEVEUO(SD//'.LINF','L',JLIR)
          VALI = NINT(ZR(JLIR-1+1))
          IF (VALI.EQ.1) VALK = 'MANUEL'
          IF (VALI.EQ.2) VALK = 'AUTO'
          IF (VALI.EQ.3) VALK = 'CFL'
          IF (VALI.EQ.4) VALK = 'MODAL'

        ELSEIF (QUEST.EQ.'DTMIN') THEN
          CALL JEVEUO(SD//'.LINF','L',JLIR)
          VALR = ZR(JLIR-1+5)

        ELSEIF (QUEST.EQ.'PAS_MINI') THEN
          CALL JEVEUO(SD//'.LINF','L',JLIR)
          VALR = ZR(JLIR-1+2)

        ELSEIF (QUEST.EQ.'PAS_MAXI') THEN
          CALL JEVEUO(SD//'.LINF','L',JLIR)
          VALR = ZR(JLIR-1+3)

        ELSEIF (QUEST.EQ.'NB_PAS_MAXI') THEN
          CALL JEVEUO(SD//'.LINF','L',JLIR)
          VALI = NINT(ZR(JLIR-1+4))

        ELSEIF (QUEST.EQ.'DT-') THEN
          CALL JEVEUO(SD//'.LINF',GETSET,JLIR)
          IF (GETSET.EQ.'L') VALR = ZR(JLIR-1+6)
          IF (GETSET.EQ.'E') ZR(JLIR-1+6) = VALR

        ELSEIF (QUEST.EQ.'NBINST') THEN
          CALL JELIRA(SD//'.DITR','LONMAX',VALI,K8BID)

        ELSEIF (QUEST.EQ.'JINST') THEN
          CALL JEVEUO(SD//'.DITR','L',VALI)

        ENDIF

C     ------------------------------------------------------------------
C                     QUESTION SUR L'ECHEC
C     ------------------------------------------------------------------

      ELSEIF (TYPQUE.EQ.'ECHE') THEN
      
        CALL ASSERT(QUEST.EQ.'NOM_EVEN'.OR.
     &              QUEST.EQ.'NOM_CHAM'.OR.
     &              QUEST.EQ.'NOM_CMP'.OR.
     &              QUEST.EQ.'VALE_REF'.OR.
     &              QUEST.EQ.'CRIT_COMP'.OR.
     &              QUEST.EQ.'SUBD_METH'.OR.
     &              QUEST.EQ.'SUBD_PAS'.OR.
     &              QUEST.EQ.'SUBD_PAS_MINI'.OR.
     &              QUEST.EQ.'SUBD_COEF_PAS_1'.OR.
     &              QUEST.EQ.'SUBD_NIVEAU'.OR.
     &              QUEST.EQ.'SUBD_ITER_IGNO'.OR.
     &              QUEST.EQ.'SUBD_ITER_FIN'.OR.
     &              QUEST.EQ.'SUBD_ITER_PLUS'.OR.
     &              QUEST.EQ.'IOCC_ERREUR'.OR.
     &              QUEST.EQ.'VERIF_EVEN'.OR.
     &              QUEST.EQ.'NB_OCC')
        
        IF (QUEST.EQ.'NB_OCC') THEN
          CALL JELIRA(SD//'.EEVR','LONMAX',IRET,K8BID)
          VALI = IRET /LEEVR

        ELSEIF (QUEST.EQ.'IOCC_ERREUR') THEN
          CALL ASSERT(GETSET.EQ.'L')
          CALL JELIRA(SD//'.EEVR','LONMAX',IRET,K8BID)
          NOCC = IRET /LEEVR
          CALL JEVEUO(SD//'.EEVR','L',JEEVR)
          VALI = 0
          DO 100 I = 1,NOCC
            N = NINT(ZR(JEEVR-1+LEEVR*(I-1)+1))
            IF (N.EQ.0) VALI=I
 100      CONTINUE
          CALL ASSERT(VALI.GT.0)
          CALL ASSERT(VALI.LE.NOCC)

        ELSEIF (QUEST.EQ.'NOM_EVEN') THEN
          CALL JEVEUO(SD//'.EEVR','L',JEEVR)
          N = NINT(ZR(JEEVR-1+LEEVR*(IOCC-1)+1))
          IF (N.EQ.0) VALK = 'ERREUR'
          IF (N.EQ.1) VALK = 'DELTA_GRANDEUR'
          IF (N.EQ.2) VALK = 'COLLISION'

        ELSEIF (QUEST.EQ.'VERIF_EVEN') THEN
          CALL JEVEUO(SD//'.EEVR',GETSET,JEEVR)
          IF (GETSET.EQ.'L') THEN
            IF (ZR(JEEVR-1+LEEVR*(IOCC-1)+4).EQ.1) THEN
              VALK='OUI'
            ELSEIF (ZR(JEEVR-1+LEEVR*(IOCC-1)+4).EQ.0) THEN
              VALK='NON'
            ENDIF
          ELSEIF (GETSET.EQ.'E') THEN
            IF (VALK.EQ.'OUI') THEN
              ZR(JEEVR-1+LEEVR*(IOCC-1)+4)=1
            ELSEIF (VALK.EQ.'NON') THEN
              ZR(JEEVR-1+LEEVR*(IOCC-1)+4)=0
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF
          ENDIF

        ELSEIF (QUEST.EQ.'NOM_CHAM') THEN
          CALL JEVEUO(SD//'.EEVK','L',JEEVK)
          VALK = ZK16(JEEVK-1+LEEVK*(IOCC-1)+2)

        ELSEIF (QUEST.EQ.'NOM_CMP') THEN
          CALL JEVEUO(SD//'.EEVK','L',JEEVK)
          VALK = ZK16(JEEVK-1+LEEVK*(IOCC-1)+3)

        ELSEIF (QUEST.EQ.'VALE_REF') THEN
          CALL JEVEUO(SD//'.EEVR','L',JEEVR)
          VALR = ZR(JEEVR-1+LEEVR*(IOCC-1)+3)

        ELSEIF (QUEST.EQ.'CRIT_COMP') THEN
          CALL JEVEUO(SD//'.EEVR','L',JEEVR)
          VALI = NINT(ZR(JEEVR-1+LEEVR*(IOCC-1)+2))
          IF (VALI.EQ.1) VALK = 'LT'
          IF (VALI.EQ.2) VALK = 'GT'
          IF (VALI.EQ.3) VALK = 'LE'
          IF (VALI.EQ.4) VALK = 'GE'

        ELSEIF (QUEST.EQ.'SUBD_METH') THEN
          CALL JEVEUO(SD//'.ESUR',GETSET,JESUR)
          IF (GETSET.EQ.'L') THEN
            N = NINT(ZR(JESUR-1+LESUR*(IOCC-1)+1))
            IF (N.EQ.0) VALK = 'AUCUNE'
            IF (N.EQ.1) VALK = 'UNIFORME'
            IF (N.EQ.2) VALK = 'EXTRAP_IGNO'
            IF (N.EQ.3) VALK = 'EXTRAP_FIN'
          ELSEIF (GETSET.EQ.'E') THEN
            IF (VALK .EQ. 'AUCUNE')      N=0
            IF (VALK .EQ. 'UNIFORME')    N=1
            IF (VALK .EQ. 'EXTRAP_IGNO') N=2
            IF (VALK .EQ. 'EXTRAP_FIN')  N=3
            ZR(JESUR-1+LESUR*(IOCC-1)+1) = N
          ENDIF

        ELSEIF (QUEST.EQ.'SUBD_PAS') THEN
          CALL JEVEUO(SD//'.ESUR',GETSET,JESUR)
          VALI = NINT(ZR(JESUR-1+LESUR*(IOCC-1)+2))

        ELSEIF (QUEST.EQ.'SUBD_PAS_MINI') THEN
          CALL JEVEUO(SD//'.ESUR',GETSET,JESUR)
          VALR = ZR(JESUR-1+LESUR*(IOCC-1)+3)

        ELSEIF (QUEST.EQ.'SUBD_COEF_PAS_1') THEN
          CALL JEVEUO(SD//'.ESUR',GETSET,JESUR)
          VALR = ZR(JESUR-1+LESUR*(IOCC-1)+4)

        ELSEIF (QUEST.EQ.'SUBD_NIVEAU') THEN
          CALL JEVEUO(SD//'.ESUR',GETSET,JESUR)
          VALI = NINT(ZR(JESUR-1+LESUR*(IOCC-1)+5))

        ELSEIF (QUEST.EQ.'SUBD_ITER_IGNO') THEN
          CALL JEVEUO(SD//'.ESUR',GETSET,JESUR)
          VALI = NINT(ZR(JESUR-1+LESUR*(IOCC-1)+6))

        ELSEIF (QUEST.EQ.'SUBD_ITER_FIN') THEN
          CALL JEVEUO(SD//'.ESUR',GETSET,JESUR)
          VALI = NINT(ZR(JESUR-1+LESUR*(IOCC-1)+7))

        ELSEIF (QUEST.EQ.'SUBD_ITER_PLUS') THEN
          CALL JEVEUO(SD//'.ESUR',GETSET,JESUR)
          VALR = ZR(JESUR-1+LESUR*(IOCC-1)+8)
          VALI = NINT(VALR)
        
        ENDIF

C     ------------------------------------------------------------------
C                     QUESTION SUR L'ADAPTATION
C     ------------------------------------------------------------------

      ELSEIF (TYPQUE.EQ.'ADAP') THEN

        CALL ASSERT(QUEST.EQ.'NOM_EVEN'.OR.
     &              QUEST.EQ.'NB_INCR_SEUIL'.OR.
     &              QUEST.EQ.'NOM_PARA'.OR.
     &              QUEST.EQ.'CRIT_COMP'.OR.
     &              QUEST.EQ.'VALE'.OR.
     &              QUEST.EQ.'METHODE'.OR.
     &              QUEST.EQ.'PCENT_AUGM'.OR.
     &              QUEST.EQ.'VALE_REF'.OR.
     &              QUEST.EQ.'NOM_CHAM'.OR.
     &              QUEST.EQ.'NOM_CMP'.OR.
     &              QUEST.EQ.'NU_CMP'.OR.
     &              QUEST.EQ.'NB_ITER_NEWTON_REF'.OR.
     &              QUEST.EQ.'NB_OCC')

        IF (QUEST.EQ.'NB_OCC') THEN
          CALL JELIRA(SD//'.AEVR','LONMAX',IRET,K8BID)
          VALI = IRET /LAEVR

        ELSEIF (QUEST.EQ.'NOM_EVEN') THEN
          CALL JEVEUO(SD//'.AEVR','L',JAEVR)
          N = NINT(ZR(JAEVR-1+LAEVR*(IOCC-1)+1))
          IF (N.EQ.0) VALK = 'AUCUN'
          IF (N.EQ.1) VALK = 'TOUT_INST'
          IF (N.EQ.2) VALK = 'SEUIL_SANS_FORMULE'
          IF (N.EQ.3) VALK = 'SEUIL_AVEC_FORMULE'

        ELSEIF (QUEST.EQ.'NB_INCR_SEUIL') THEN
          CALL JEVEUO(SD//'.AEVR','L',JAEVR)
          VALI = NINT(ZR(JAEVR-1+LAEVR*(IOCC-1)+2))

        ELSEIF (QUEST.EQ.'NOM_PARA') THEN
          CALL JEVEUO(SD//'.AEVR','L',JAEVR)
          VALI = NINT(ZR(JAEVR-1+LAEVR*(IOCC-1)+3))
          IF (VALI.EQ.1) VALK = 'NB_ITER_NEWT'
          IF (VALI.EQ.2) VALK = 'DP'

        ELSEIF (QUEST.EQ.'CRIT_COMP') THEN
          CALL JEVEUO(SD//'.AEVR','L',JAEVR)
          VALI = NINT(ZR(JAEVR-1+LAEVR*(IOCC-1)+4))
          IF (VALI.EQ.1) VALK = 'LT'
          IF (VALI.EQ.2) VALK = 'GT'
          IF (VALI.EQ.3) VALK = 'LE'
          IF (VALI.EQ.4) VALK = 'GE'

        ELSEIF (QUEST.EQ.'VALE') THEN
          CALL JEVEUO(SD//'.AEVR',GETSET,JAEVR)
          IF (GETSET.EQ.'L') THEN
            VALR = ZR(JAEVR-1+LAEVR*(IOCC-1)+5)
            VALI = NINT(ZR(JAEVR-1+LAEVR*(IOCC-1)+5))
          ELSEIF (GETSET.EQ.'E') THEN
            ZR(JAEVR-1+LAEVR*(IOCC-1)+5) = VALR
          ENDIF

        ELSEIF (QUEST.EQ.'METHODE') THEN
          CALL JEVEUO(SD//'.ATPR','L',JATPR)
          VALI = NINT(ZR(JATPR-1+LATPR*(IOCC-1)+1))
          IF (VALI.EQ.1) VALK = 'FIXE'
          IF (VALI.EQ.2) VALK = 'DELTA_GRANDEUR'
          IF (VALI.EQ.3) VALK = 'ITER_NEWTON'
          IF (VALI.EQ.4) VALK = 'FORMULE'

        ELSEIF (QUEST.EQ.'PCENT_AUGM') THEN
          CALL JEVEUO(SD//'.ATPR','L',JATPR)
          VALR = ZR(JATPR-1+LATPR*(IOCC-1)+2)

        ELSEIF (QUEST.EQ.'VALE_REF') THEN
          CALL JEVEUO(SD//'.ATPR','L',JATPR)
          VALR = ZR(JATPR-1+LATPR*(IOCC-1)+3)

        ELSEIF (QUEST.EQ.'NU_CMP') THEN
          CALL JEVEUO(SD//'.ATPR','L',JATPR)
          VALR = ZR(JATPR-1+LATPR*(IOCC-1)+4)
          VALI = NINT(VALR)

        ELSEIF (QUEST.EQ.'NB_ITER_NEWTON_REF') THEN
          CALL JEVEUO(SD//'.ATPR','L',JATPR)
          VALR = ZR(JATPR-1+LATPR*(IOCC-1)+5)
          VALI = NINT(VALR)
        
        ELSEIF (QUEST.EQ.'NOM_CHAM') THEN
          CALL JEVEUO(SD//'.ATPK','L',JATPK)
          VALK = ZK16(JATPK-1+LATPK*(IOCC-1)+2)

        ELSEIF (QUEST.EQ.'NOM_CMP') THEN
          CALL JEVEUO(SD//'.ATPK','L',JATPK)
          VALK = ZK16(JATPK-1+LATPK*(IOCC-1)+3)

        ENDIF

      ENDIF
     
      END

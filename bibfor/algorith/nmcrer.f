      SUBROUTINE NMCRER(CARCRI,SDERRO)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/04/2012   AUTEUR ABBAS M.ABBAS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE
      CHARACTER*24 SDERRO,CARCRI
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (SD ERREUR)
C
C CREATION DE LA SD
C
C ----------------------------------------------------------------------
C
C
C IN  CARCRI : PARAMETRES DES METHODES D'INTEGRATION LOCALES
C IN  SDERRO : SD ERREUR
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER      ZEVEN
      PARAMETER   (ZEVEN = 30)
      CHARACTER*16 NEVEN(ZEVEN)
      CHARACTER*8  NCRET(ZEVEN)
      INTEGER      VCRET(ZEVEN)
      CHARACTER*16 TEVEN(ZEVEN)
      CHARACTER*24 FEVEN(ZEVEN)
C
      INTEGER      IFM,NIV
      INTEGER      IARG,IBID,JVALE,IEVEN
      CHARACTER*24 ERRECN,ERRECV,ERRENI,ERRENO,ERRAAC,ERRFCT
      INTEGER      JEECON,JEECOV,JEENIV,JEENOM,JEEACT,JEEFCT
      CHARACTER*24 ERRTPS,ERRINF,ERRCVG,ERREVT
      INTEGER      JERRT,JEINFO,JECONV,JEEEVT
      CHARACTER*16 MOTFAC,CHAINE
      REAL*8       THETA
C
C --- NOM DES EVENEMENTS
C
      DATA NEVEN   /'ERRE_INTE','INTE_NPHY','DIVE_DEBO',
     &              'ERRE_PILO','CONV_PILO','ERRE_FACS',
     &              'ERRE_FACT','ERRE_CTD1','ERRE_CTD2',
     &              'ERRE_TIMN','ERRE_TIMP','ERRE_EXCP',
     &              'ITER_MAXI',
     &              'DIVE_RESI','RESI_MAXR','RESI_MAXL',
     &              'DIVE_PFIX','CRIT_STAB','DIVE_FIXG',
     &              'DIVE_FIXF','DIVE_FIXC','ERRE_CTCG',
     &              'ERRE_CTCF','ERRE_CTCC','DIVE_FROT',
     &              'DIVE_GEOM','DIVE_RELA','DIVE_MAXI',
     &              'DIVE_REFE','DIVE_COMP'/
C
C --- NOM DU CODE RETOUR ATTACHE A L'EVENEMENT
C
      DATA NCRET   /'LDC','LDC','LDC',
     &              'PIL','PIL','FAC',
     &              'FAC','CTC','CTC',
     &              'XXX','XXX','XXX',
     &              'XXX',
     &              'XXX','XXX','XXX',
     &              'XXX','XXX','XXX',
     &              'XXX','XXX','XXX',
     &              'XXX','XXX','XXX',
     &              'XXX','XXX','XXX',
     &              'XXX','XXX'/
C
C --- VALEUR DU CODE RETOUR CORRESPONDANT A CHAQUE EVENEMENT
C
      DATA VCRET   / 1 , 2, 3,
     &               1 , 2, 1,
     &               2 , 1, 2,
     &               99,99,99,
     &               99,
     &               99,99,99,
     &               99,99,99,
     &               99,99,99,
     &               99,99,99,
     &               99,99,99,
     &               99,99/
C
C --- TYPE ET NIVEAU DE DECLENCHEMENT POSSIBLES DE L'EVENEMENT
C TROIS TYPES
C EVEN  : EVENEMENT A CARACTERE PUREMENT INFORMATIF
C          -> PEUT ETRE TRAITE SI UTILISATEUR LE DEMANDE DANS
C             DEFI_LIST_INST
C ERRI_ : EVENEMENT A TRAITER IMMEDIATEMENT SI ON VEUT CONTINUER
C ERRC_ : EVENEMENT A TRAITER A CONVERGENCE
C CONV_ : EVENEMENT A TRAITER POUR DETERMINER LA CONVERGENCE
C
      DATA TEVEN   /'ERRI_NEWT','ERRC_NEWT','CONV_NEWT',
     &              'ERRI_NEWT','CONV_CALC','ERRI_NEWT',
     &              'ERRI_NEWT','ERRI_NEWT','ERRI_NEWT',
     &              'ERRI_CALC','ERRI_CALC','ERRI_CALC',
     &              'ERRI_NEWT',
     &              'EVEN'     ,'EVEN'     ,'EVEN'     ,
     &              'CONV_NEWT','EVEN'     ,'CONV_FIXE',
     &              'CONV_FIXE','CONV_FIXE','ERRI_FIXE',
     &              'ERRI_FIXE','ERRI_FIXE','CONV_RESI',
     &              'CONV_NEWT','CONV_RESI','CONV_RESI',
     &              'CONV_RESI','CONV_RESI'/
C
C --- FONCTIONNALITE ACTIVE SI NECESSAIRE POUR CONVERGENCE
C
      DATA FEVEN   /' ',' '       ,' ',
     &              ' ','PILOTAGE',' ',
     &              ' ',' '       ,' ',
     &              ' ',' '       ,' ',
     &              ' ',
     &              ' ',' '       ,' ',
     &              ' ',' '       ,' ',
     &              ' ',' '       ,' ',
     &              ' ',' '       ,' ',
     &              ' ',' '       ,' ',
     &              ' ',' '       /
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('MECA_NON_LINE',IFM,NIV)
C
C --- AFFICHAGE
C
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<MECANONLINE> ... LECTURE CALCUL ERREUR'
      ENDIF
C
C --- INITIALISATIONS
C
      MOTFAC = 'INCREMENT'
C
C --- VALEUR DE THETA
C
      CALL JEVEUO(CARCRI(1:19)//'.VALV','L',JVALE)
      THETA  =  ZR(JVALE+3)
C
C --- GENERAL
C
      ERRINF = SDERRO(1:19)//'.INFO'
      CALL WKVECT(ERRINF,'V V I'  ,2,JEINFO)
      ZI(JEINFO-1+1) = ZEVEN
C
C --- OBJETS
C
      ERRENO = SDERRO(1:19)//'.ENOM'
      ERRECV = SDERRO(1:19)//'.ECOV'
      ERRECN = SDERRO(1:19)//'.ECON'
      ERRENI = SDERRO(1:19)//'.ENIV'
      ERRFCT = SDERRO(1:19)//'.EFCT'
      ERRAAC = SDERRO(1:19)//'.EACT'
      ERRCVG = SDERRO(1:19)//'.CONV'
      ERREVT = SDERRO(1:19)//'.EEVT'
      CALL WKVECT(ERRENO,'V V K16',ZEVEN,JEENOM)
      CALL WKVECT(ERRECV,'V V I'  ,ZEVEN,JEECOV)
      CALL WKVECT(ERRECN,'V V K8' ,ZEVEN,JEECON)
      CALL WKVECT(ERRENI,'V V K16',ZEVEN,JEENIV)
      CALL WKVECT(ERRFCT,'V V K24',ZEVEN,JEEFCT)
      CALL WKVECT(ERRAAC,'V V I'  ,ZEVEN,JEEACT)
      CALL WKVECT(ERRCVG,'V V I'  ,5    ,JECONV)
      CALL WKVECT(ERREVT,'V V K16',2    ,JEEEVT)
C
      DO 10 IEVEN = 1,ZEVEN
        ZK16(JEENOM-1+IEVEN) = NEVEN(IEVEN)
        ZK8 (JEECON-1+IEVEN) = NCRET(IEVEN)
        ZI  (JEECOV-1+IEVEN) = VCRET(IEVEN)
        ZK16(JEENIV-1+IEVEN) = TEVEN(IEVEN)
        ZK24(JEEFCT-1+IEVEN) = FEVEN(IEVEN)
 10   CONTINUE
C
C --- ERREUR EN TEMPS (THM)
C
      CALL GETVTX(MOTFAC,'ERRE_TEMPS',1,IARG,1,CHAINE,IBID)
      IF (CHAINE.EQ.'OUI') THEN
        ERRTPS = SDERRO(1:19)//'.ERRT'
        CALL WKVECT(ERRTPS,'V V R',3,JERRT)
        ZR(JERRT-1+3) = THETA
      ENDIF
C
      CALL JEDEMA()
      END

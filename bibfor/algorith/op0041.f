      SUBROUTINE OP0041(IER)
      IMPLICIT NONE 
      INTEGER           IER

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 30/03/2004   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE CIBHHLV L.VIVAN
C                       OPERATEUR DEFI_FISS_XFEM :
C                INITIALISATION DES CHAMPS NÉCESSAIRES À XFEM
C                 - LEVEL-SETS
C                 - GRADIENTS DES LEVEL-SETS
C                 - MAILLES ENRICHIES DE LA ZONE FISSURE 
C                 - POINTS DU FOND DE FISSURE
C     ------------------------------------------------------------------
C     OUT : IER = 0 => TOUT S'EST BIEN PASSE
C     : IER > 0 => NOMBRE D'ERREURS RENCONTREES
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
      CHARACTER*32    JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C     
      INTEGER       IFM,NIV,IBID,ME1,ME2,NVAL,IADRMA
      REAL*8        PFI(3),VOR(3),ORI(3),NORME
      CHARACTER*8   FISS,MODE,NFONF,NFONG,MAFIS,FONFIS,MAENR,NOMA,METH
      CHARACTER*16  K16BID
      CHARACTER*19  CNSLT,CNSLN,GRLT,GRLN
      CHARACTER*24  OBJMA
C
C-----------------------------------------------------------------------
C     DÉBUT
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFMAJ()
      CALL INFNIV(IFM,NIV)
C
      CALL GETRES(FISS,K16BID,K16BID)
      CALL GETVID(' ','MODELE',1,1,1,MODE,IBID)    
      CALL GETVID('DEFI_FISS','FONC_LT',1,1,1,NFONF,ME1)  
      CALL GETVID('DEFI_FISS','FONC_LN',1,1,1,NFONG,ME1)   
      CALL GETVID('DEFI_FISS','GROUP_MA_FISS',1,1,1,MAFIS,ME2) 
      CALL GETVID('DEFI_FISS','GROUP_MA_FOND',1,1,1,FONFIS,ME2) 
      CALL GETVID(' ','GROUP_MA_ENRI',1,1,1,MAENR,IBID) 
      CALL GETVR8('ORIE_FOND','PFON_INI',1,1,3,PFI,IBID)
      CALL GETVR8('ORIE_FOND','VECT_ORIE',1,1,3,VOR,IBID)
      CALL GETVR8('ORIE_FOND','PT_ORIGIN',1,1,3,ORI,IBID)
C
      OBJMA = MODE//'.MODELE    .NOMA'
      CALL JEVEUO(OBJMA,'L',IADRMA)
      NOMA = ZK8(IADRMA)
C
C-----------------------------------------------------------------------
C     CALCUL DES LEVEL-SETS
C-----------------------------------------------------------------------
C
      CNSLT='&&OP0041.CNSLT'
      CNSLN='&&OP0041.CNSLN'
      CALL CNSCRE(NOMA,'NEUT_R',1,'X1','V',CNSLT)
      CALL CNSCRE(NOMA,'NEUT_R',1,'X1','V',CNSLN)
      IF (ME1.EQ.1) THEN 
        METH='FONCTION' 
      ELSEIF (ME2.EQ.1) THEN
        METH='GROUP_MA'
      ELSE
        CALL UTMESS('F','OP0041','ERREUR DANS LE CHOIX DE LA METHODE '//
     &                     'DE CALCUL DES LEVEL-SETS.')
      ENDIF

      CALL XINILS(IFM,NOMA,METH,NFONF,NFONG,CNSLT,CNSLN)

      CALL CNSCNO(CNSLT,' ','G',FISS//'.LTNO')
      CALL CNSCNO(CNSLN,' ','G',FISS//'.LNNO')

      IF (NIV.GT.1) THEN
        CALL IMPRSD('CHAMP',FISS//'.LTNO',IFM,'FISSURE.LTNO=')
        CALL IMPRSD('CHAMP',FISS//'.LNNO',IFM,'FISSURE.LNNO=')
      END IF
C
C-----------------------------------------------------------------------
C     CALCUL DES GRADIENTS DES LEVEL-SETS
C-----------------------------------------------------------------------
C
      GRLT = '&&OP0041.GRLT'
      GRLN = '&&OP0041.GRLN'

      CALL XGRALS(IFM,MODE,NOMA,FISS,GRLT,GRLN)

      CALL CNSCNO ( GRLT,' ','G',FISS//'.GRLTNO' )
      CALL CNSCNO ( GRLN,' ','G',FISS//'.GRLNNO' )

      IF (NIV.GT.1) THEN
        CALL IMPRSD('CHAMP',FISS//'.GRLTNO',IFM,'FISSURE.GRLTNO=')
        CALL IMPRSD('CHAMP',FISS//'.GRLNNO',IFM,'FISSURE.GRLNNO=')
      END IF
      
      CALL DETRSD ( 'CHAM_NO_S'  , GRLT )
      CALL DETRSD ( 'CHAM_NO_S'  , GRLN )
C
C-----------------------------------------------------------------------
C     CALCUL DE L'ENRICHISSEMENT ET DES POINTS DU FOND DE FISSURE
C-----------------------------------------------------------------------
C
      CALL NORMEV(VOR,NORME)
      IF (NORME.LT.1.D-10) CALL UTMESS('F','OP0041','LA NORME '//
     &                              'DU VECTEUR VECT_ORIE EST NULLE')
      
      CALL XENRCH(IFM,NIV,NOMA,CNSLT,CNSLN,PFI,VOR,ORI,FISS)
C
C-----------------------------------------------------------------------
C     FIN
C-----------------------------------------------------------------------
C
      CALL DETRSD('CHAM_NO_S',CNSLT)
      CALL DETRSD('CHAM_NO_S',CNSLN)

      CALL JEDEMA()
      END

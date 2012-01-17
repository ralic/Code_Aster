      SUBROUTINE LISSEN(LISCHA,NBPASE,INPSCO)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/01/2012   AUTEUR ABBAS M.ABBAS 
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
C
      IMPLICIT      NONE
      CHARACTER*19  LISCHA
      INTEGER       NBPASE
      CHARACTER*(*) INPSCO
C
C ----------------------------------------------------------------------
C
C ROUTINE UTILITAIRE (LISTE_CHARGES)
C
C ENREGISTREMENT DES CHARGEMENTS SENSIBLES
C
C ----------------------------------------------------------------------
C
C
C IN  LISCHA : NOM DE LA SD LISTE_CHARGES
C IN  NBPASE : NOMBRE DE PARAMETRES SENSIBLES
C I/O INPSCO : STRUCTURE CONTENANT LA LISTE DES NOMS SENSIBLES
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
      INTEGER      NRPASE
      INTEGER      IAUX,JAUX
      CHARACTER*8  NOPASE,K8BID,CHARGE
      INTEGER      NBCHAR,ICHAR
      INTEGER      IRET,IBID
      INTEGER      CODCHA
      LOGICAL      LISICO,LNEUM,LONDP,LSIGI,LDUAL
      CHARACTER*24 TYPEPS,MOTCLE,K24BID
      CHARACTER*24 NOMLIS
      INTEGER      JLISCI
      CHARACTER*13 PREFOB
      INTEGER      ICH,NBCH,IPOSIT,INDXCH
      LOGICAL      LEXIS,LISEXI
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL LISNNB(LISCHA,NBCHAR)
C
      DO 10 NRPASE = 1,NBPASE
        IAUX   = NRPASE
        JAUX   = 1
        CALL PSNSLE(INPSCO,IAUX  ,JAUX  ,NOPASE)
        IF (NOPASE.NE.' ') THEN
          DO 15 ICHAR = 1,NBCHAR
            CALL LISLCH(LISCHA,ICHAR ,CHARGE)
            CALL LISLCO(LISCHA,ICHAR ,CODCHA)
            CALL LISLLC(LISCHA,ICHAR ,PREFOB)
            CALL PSGENC(CHARGE,NOPASE,K8BID ,IRET  )
            IF (IRET.EQ.0) THEN
              LDUAL  = LISICO('DIRI_DUAL'    ,CODCHA)
              LNEUM  = LISICO('NEUM_MECA'    ,CODCHA)
              LONDP  = LISICO('ONDE_PLANE'   ,CODCHA)
              LSIGI  = LISICO('SIGM_INTERNE' ,CODCHA)
              TYPEPS = ' '
              IF (LDUAL) THEN
                TYPEPS = 'DIRICHLE'
              ELSEIF (LNEUM) THEN
                TYPEPS = 'FORCE'
                NOMLIS = '&&LISSEN.NOMLIS'
                CALL LISDEF('POEC','NEUM_MECA',IBID  ,K8BID ,IPOSIT)
                CALL LISDEF('IDNS',NOMLIS,IPOSIT,K8BID ,NBCH  )
                CALL JEVEUO(NOMLIS,'L',JLISCI)
                DO 30 ICH = 1,NBCH
                  INDXCH = ZI(JLISCI-1+ICH)
                  LEXIS  = LISEXI(PREFOB,INDXCH)
                  IF (LEXIS) THEN
                    CALL LISDEF('MOTC',K24BID,INDXCH,MOTCLE,IBID  )
                    IF (MOTCLE.EQ.'PESANTEUR') THEN
                      TYPEPS = '.PESAN'
                    ELSEIF (MOTCLE.EQ.'ROTATION') THEN
                      TYPEPS = '.ROTAT'
                    ELSEIF (MOTCLE.EQ.'EPSI_INIT') THEN
                      TYPEPS = '.EPSIN'
                    ENDIF
                  ENDIF
  30            CONTINUE
                CALL JEDETR(NOMLIS)
              ELSEIF (LONDP) THEN
                TYPEPS = '.ONDPL'
              ELSEIF (LSIGI) THEN
                TYPEPS = '.SIINT'
              ENDIF
              IF (TYPEPS.EQ.' ') CALL U2MESS('F','SENSIBILITE_93')
              CALL PSTYPA(NBPASE,INPSCO,CHARGE,NOPASE,
     &                    TYPEPS)
C
            ENDIF
  15      CONTINUE
        ENDIF
  10  CONTINUE
C
      CALL JEDEMA()
      END

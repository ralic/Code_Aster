      SUBROUTINE VTDEFS(CHPOUT,CHPIN,BASE,TYPC)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 16/06/2004   AUTEUR DURAND C.DURAND 
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
C     ------------------------------------------------------------------
C     DEFINITION DE LA STRUCTURE D'UN CHAM_NO OU CHAM_ELEM "CHPOUT"
C                    QUI S'APPUIE SUR LA MEME NUMEROTATION QUE "CHPIN",
C     LE CHAM_... "CHPOUT" EST CREEE SUR LA BASE "BASE".
C     LE CHAM_... "CHPOUT" EST A COEFFICIENTS "TYPE".
C     ------------------------------------------------------------------
C IN : CHPOUT : NOM DU CHAM_NO OU CHAM_ELEM A CREER
C IN : CHPIN  : NOM DU CHAM_NO OU CHAM_ELEM MODELE
C IN : BASE   : NOM DE LA BASE SUR LAQUELLE LE CHAM_... DOIT ETRE CREER
C IN : TYPC   : TYPE DES VALEURS DU CHAM_... A CREER
C                    'R'  ==> COEFFICIENTS REELS
C                    'C'  ==> COEFFICIENTS COMPLEXES
C                    ' '  ==> COEFFICIENTS DU TYPE DU CHAM_... CHPIN
C     ------------------------------------------------------------------
C     PRECAUTIONS D'EMPLOI :
C       1) LE CHAM_... "CHPOUT" NE DOIT PAS EXISTER
C       2) LES COEFFICIENTS DU CHAM_... "CHPOUT" NE SONT PAS AFFECTES
C     -----------------------------------------------------------------
C     ASTER INFORMATIONS:
C       16/01/04 (OB): MODIF POUR SOLVEUR FETI.
C----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT REAL*8 (A-H,O-Z)
      
C DECLARATION PARAMETRES D'APPELS
      CHARACTER*(*)     CHPOUT,CHPIN,BASE,TYPC
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32      JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------

C DECLARATION VARIABLES LOCALES
      INTEGER      LCHPOU,LCHPIN,IBID,IFETC,NBREFE,IREFE,IDIME,NBSD,IDD,
     &             KFETC,IFM,NIV
      CHARACTER*1  TYPE
      CHARACTER*4  TYCH
      CHARACTER*8  CBID
      CHARACTER*19 CH19,ARG1,ARG2
      CHARACTER*24 METHOD,SDFETI
      LOGICAL      LFETI
      
      CALL JEMARQ()
      CH19 = CHPIN

      CALL DISMOI('F','TYPE_CHAMP',CH19,'CHAMP',IBID,TYCH,IER)

C INIT. A CAUSE DE FETI
      NBSD=0
      LFETI=.FALSE.
      METHOD='XXXX'
      SDFETI='XXXX'
            
C RECUPERATION DU NIVEAU D'IMPRESSION
      CALL INFNIV(IFM,NIV)
      
C CHAM_NO      
      IF (TYCH.EQ.'NOEU') THEN  
C FETI OR NOT ?
        CALL JELIRA(CH19//'.REFE','LONMAX',NBREFE,CBID)
        IF (NBREFE.NE.4) THEN
          IF (NIV.GE.3)
     &      WRITE(IFM,*)'<FETI/VTDEFS> CHAM_NO NON ETENDU POUR FETI',
     &      CH19
        ELSE
          CALL JEVEUO(CH19//'.REFE','L',IREFE)     
          METHOD=ZK24(IREFE+2)
        ENDIF
        IF (METHOD.EQ.'FETI') THEN  
          SDFETI=ZK24(IREFE+3)    
          CALL JEVEUO(SDFETI(1:19)//'.DIME','L',IDIME)
          NBSD=ZI(IDIME)
          CALL JEVEUO(CH19//'.FETC','L',IFETC)
          CALL WKVECT(CHPOUT(1:19)//'.FETC','V V K24',NBSD,KFETC)
          LFETI=.TRUE.                            
        ENDIF  
      ENDIF
      
C-----------------------------------------------------------------------
C --- BOUCLE SUR LES SOUS-DOMAINES
C-----------------------------------------------------------------------
      DO 10 IDD=0,NBSD
        IF (IDD.GT.0) THEN
C CHAM_NO ET FETI       
          ARG2=ZK24(IFETC+IDD-1)
C CONVENTION DE NOM:
C POUR LES SOUS-DOMAINES: '&&'//NOMPRO(1:6)//'_S.'//NOMSD(1:8)
C POUR DOMAINE GLOBALE: '&&'//NOMPRO(1:6)//'_SOLUTION  '          
          ARG1=CHPOUT(1:10)//ARG2(11:19)
          ZK24(KFETC+IDD-1)=ARG1
        ELSE
          ARG1=CHPOUT
          ARG2=CHPIN      
        ENDIF
        CALL VTDEF1(ARG1,ARG2,BASE,TYPC,LFETI)

C MONITORING
        IF ((NIV.GE.3).AND.(LFETI)) THEN
          WRITE(IFM,*)
          WRITE(IFM,*)'DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD'
          IF (IDD.GT.0) THEN
            WRITE(IFM,*)'<FETI/VTDEFS> NUMERO DE SOUS-DOMAINE: ',IDD
          ELSE
            WRITE(IFM,*)'<FETI/VTDEFS> DOMAINE GLOBAL'          
          ENDIF                           
          WRITE(IFM,*)'<FETI/VTDEFS> CREATION OBJETS JEVEUX ',
     &         ARG2(1:19)
          WRITE(IFM,*)
          IF ((NIV.GE.4).AND.(IDD.GT.0))
     &      CALL UTIMSD(IFM,2,.FALSE.,.TRUE.,ARG2(1:19),
     &      1,' ')
          IF ((NIV.GE.4).AND.(IDD.EQ.NBSD))
     &      CALL UTIMSD(IFM,2,.FALSE.,.TRUE.,CHPOUT(1:19),
     &      1,' ')     
          WRITE(IFM,*)'DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD'        
          WRITE(IFM,*)
        ENDIF
                        
   10 CONTINUE   
      CALL JEDEMA()

      END

      SUBROUTINE FETREX(OPTION,IDD,NI,VI,NO,VO,SDFETI,COLAUI)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/11/2004   AUTEUR BOITEAU O.BOITEAU 
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
C-----------------------------------------------------------------------
C    - FONCTION REALISEE:  RESTRICTION-EXTRACTION AU SENS FETI
C
C      IN OPTION: IN   : 1 RESTRICTION, 2 EXTRACTION
C      IN    IDD: IN   : NUMERO DE SOUS-DOMAINE
C      IN     NI: IN   : NOMBRE DE DDLS DU VECTEUR INPUT
C      IN     VI: VR8  : VECTEUR INPUT DE TAILLE NI
C      IN     NO: IN   : NOMBRE DE DDLS DU VECTEUR OUTPUT
C      OUT    VO: VR8  : VECTEUR OUTPUT DE TAILLE NO 
C     IN SDFETI: CH19  : SD DECRIVANT LE PARTIONNEMENT FETI
C     IN COLAUI: CH24  : COLLECTION TEMPORAIRE D'ENTIER
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       26/01/04 (OB): CREATION.
C----------------------------------------------------------------------
C RESPONSABLE BOITEAU O.BOITEAU
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      INTEGER      OPTION,IDD,NI,NO
      REAL*8       VI(NI),VO(NO)
      CHARACTER*19 SDFETI
      CHARACTER*24 COLAUI
      
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      
C DECLARATION VARIABLES LOCALES
      INTEGER      IAUX1,IAUX2,IAUX3,J,IFETG,LONG,MOD,IFETB,IFETI,ICOL,
     &             NDDLCI,NBDDLI,IAUX21,IAUX31,NUMGD,NDDLCD,IFETJ,
     &             NBDDLD,K,IFM,NIV,VAL(2),I,IINF
      REAL*8       SIGN,RAUX2,RSIGN,UN
      CHARACTER*24 INFOFE
      CHARACTER*32 JEXNUM
      CHARACTER*8  K8BID
      
C CORPS DU PROGRAMME
      CALL JEMARQ()

C RECUPERATION DU NIVEAU D'IMPRESSION
      CALL INFNIV(IFM,NIV)
      CALL JEVEUO('&&'//SDFETI(1:17)//'.FINF','L',IINF)
      INFOFE=ZK24(IINF)      
      UN=1.D0
      
C INIT. VECTEUR SOLUTION
      DO 10 J=1,NO
        VO(J)=0.D0
   10 CONTINUE      

C STRUCTURE DE DONNEES DE RESTRICTION/EXTRACTION DU SOUS-DOMAINE IDD
C SUR L'INTERFACE (POINT PAR POINT)     
      CALL JEVEUO(JEXNUM(SDFETI//'.FETG',IDD),'L',IFETG)
      CALL JELIRA(JEXNUM(SDFETI//'.FETG',IDD),'LONMAX',LONG,K8BID)
      IF (MOD(LONG,2).NE.0) THEN
        CALL UTMESS('F','FETREX','PROBLEME OBJET '//SDFETI//
     &     '.FETG . IL EST DE LONGUEUR IMPAIRE !')
      ELSE
        LONG=(LONG/2)-1
      ENDIF

C STRUCTURE DE DONNEES LIEE AUX DDLS
      CALL JEVEUO(JEXNUM(SDFETI//'.FETB',IDD),'L',IFETB)
      CALL JEVEUO(SDFETI//'.FETI','L',IFETI)
      CALL JEVEUO(SDFETI//'.FETJ','L',IFETJ)      
      CALL JEVEUO(JEXNUM(COLAUI,IDD),'L',ICOL)
      
      IF ((OPTION.EQ.1).OR.(OPTION.EQ.2)) THEN
C ----------------------------------------------------------------------
C ----  OPERATEUR DE RESTRICTION RI/EXTRACTION (RI)T
C ----------------------------------------------------------------------

C MONITORING
        IF (INFOFE(1:1).EQ.'T') THEN
          WRITE(IFM,*)
          WRITE(IFM,*)'DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD'
          IF (OPTION.EQ.1) THEN
            WRITE(IFM,*)'<FETI/FETREX> OP. DE RESTRICTION RI I= ',IDD
          ELSE
            WRITE(IFM,*)'<FETI/FETREX> OP. D''EXTRACTION (RI)T I= ',IDD
          ENDIF       
        ENDIF                   
        DO 20 J=0,LONG

          IAUX1=IFETG+2*J
          
C INDICE DU JIEME NOEUD D'INTERFACE DU SOUS-DOMAINE IDD DANS LE VECTEUR
C D'INTERFACE .FETI       
          IAUX2=ZI(IAUX1)

C POUR CALCULS AUXILIAIRES                
          RAUX2=IAUX2*1.D0
          RSIGN=SIGN(UN,RAUX2)                  
          IAUX2=ABS(IAUX2)
          IAUX21=IFETI+4*(IAUX2-1)

C LE NBRE DE DDLS CUMULES AVANT LUI (NDDLCI)
C DANS LE VECTEUR D'INTERFACE/ SON NBRE DE DDLS (NBDDLI)
          NDDLCI=ZI(IAUX21+2)
          IF (IAUX2.EQ.1) THEN
            NBDDLI=NDDLCI
          ELSE
            NBDDLI=NDDLCI-ZI(IAUX21-2)
          ENDIF
          NDDLCI=NDDLCI-NBDDLI    

C NBRE DE DDLS CUMULES AVANT LUI DANS LE VECTEUR LOCAL AU SOUS-DOMAINE 
C PROF_CHNO(IDD) (NDDLCD)/ SON NOMBRE DE DDLS (NBDDLD)            
          NDDLCD=ZI(ICOL+2*J)-1
          NBDDLD=ZI(ICOL+2*J+1)                   
                          
C MONITORING
C          IF (NIV.GE.5) THEN
C            WRITE(IFM,*)IDD,ZI(IAUX21),RSIGN
C            WRITE(IFM,*)NDDLCI,NBDDLI,NDDLCD,NBDDLD
C          ENDIF
                  
C TEST DE COHERENCE DES DONNEES INDIVIDUELLEMENT
          CALL ASSERT(NDDLCI.GE.0)        
          CALL ASSERT(NBDDLI.GT.0)
          CALL ASSERT(NDDLCD.GE.0)        
          CALL ASSERT(NBDDLD.GT.0)
                                  
C TEST DE COHERENCE DES NOMBRES DE DDLS
          CALL ASSERT(NBDDLI.EQ.NBDDLD)   

          IF (OPTION.EQ.1) THEN
C RESTRICTION   
            DO 13 K=1,NBDDLD      
              VO(NDDLCI+K)=VO(NDDLCI+K)+RSIGN*VI(NDDLCD+K)
   13       CONTINUE
          ELSE
C EXTRACTION
            DO 16 K=1,NBDDLI      
              VO(NDDLCD+K)=VO(NDDLCD+K)+RSIGN*VI(NDDLCI+K)
   16       CONTINUE      
          ENDIF
                                                  
   20   CONTINUE
    

      ELSE
        CALL UTMESS('F','FETREX','OPTION NON PREVUE !')         
      ENDIF    

C MONITORING
      IF (INFOFE(4:4).EQ.'T') THEN
        WRITE(IFM,*)'<FETI/FETREX> INPUT I VI(I)'      
        DO 30 I=1,NI
          WRITE(IFM,*)I,'  ',VI(I)
   30   CONTINUE
        WRITE(IFM,*)'OUTPUT I VO(I)'         
        DO 31 I=1,NO
          WRITE(IFM,*)I,'  ',VO(I)
   31   CONTINUE          
      ENDIF
      IF (INFOFE(1:1).EQ.'T') THEN        
        WRITE(IFM,*)
        WRITE(IFM,*)'DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD'
      ENDIF
      CALL JEDEMA()
      END

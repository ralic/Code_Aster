      SUBROUTINE FETREX(OPTION,IDD,NI,VI,NO,VO,IREX)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/01/2005   AUTEUR BOITEAU O.BOITEAU 
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
C     IN IREX  : IN    : ADRESSE DU VECTEUR AUXILAIRE EVITANT DES APPELS
C                        JEVEUX.
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       26/01/04 (OB): CREATION.
C----------------------------------------------------------------------
C RESPONSABLE BOITEAU O.BOITEAU
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      INTEGER      OPTION,IDD,NI,NO,IREX
      REAL*8       VI(NI),VO(NO)
      
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
      INTEGER      IAUX1,IAUX2,IAUX3,J,IFETG,LONG,IFETB,IFETI,ICOL,
     &             NDDLCI,NBDDLI,IAUX21,IAUX31,NUMGD,NDDLCD,
     &             NBDDLD,K,VAL(2),I,IINF,TESTA,J2
      REAL*8       SIGN,RAUX2,RSIGN,UN
      
C ROUTINE AVEC PEU DE MONITORING,JEVEUX ... CAR APPELLEE TRES SOUVENT
      UN=1.D0
      
C INIT. VECTEUR SOLUTION
      DO 10 J=1,NO
        VO(J)=0.D0
   10 CONTINUE
      
C STRUCTURE DE DONNEES DE RESTRICTION/EXTRACTION DU SOUS-DOMAINE IDD
C SUR L'INTERFACE (POINT PAR POINT)
      IFETI=ZI(IREX)
      J=IREX+1+(IDD-1)*3
      IFETG=ZI(J)
      LONG=ZI(J+1)
      ICOL=ZI(J+2)
      IF ((OPTION.EQ.1).OR.(OPTION.EQ.2)) THEN
C ----------------------------------------------------------------------
C ----  OPERATEUR DE RESTRICTION RI/EXTRACTION (RI)T
C ----------------------------------------------------------------------

        DO 20 J=0,LONG
          J2=2*J
          IAUX1=IFETG+J2
          
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
          NDDLCD=ZI(ICOL+J2)-1
          NBDDLD=ZI(ICOL+J2+1)                   
                          
C MONITORING
C            WRITE(IFM,*)IDD,ZI(IAUX21),RSIGN,NO,NI
C            WRITE(IFM,*)NDDLCI,NBDDLI,NDDLCD,NBDDLD
                  
C TEST DE COHERENCE DES DONNEES INDIVIDUELLEMENT
C         TESTA=NDDLCI*NBDDLI*NDDLCD*NBDDLD
C          CALL ASSERT(TESTA.GT.0)
                                  
C TEST DE COHERENCE DES NOMBRES DE DDLS
C          CALL ASSERT(NBDDLI.EQ.NBDDLD)
   
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
C      IF (INFOFE(4:4).EQ.'T') THEN
C        WRITE(IFM,*)'<FETI/FETREX> INPUT I VI(I)'      
C        DO 30 I=1,NI
C          WRITE(IFM,*)I,'  ',VI(I)
C  30   CONTINUE
C       WRITE(IFM,*)'OUTPUT I VO(I)'         
C       DO 31 I=1,NO
C         WRITE(IFM,*)I,'  ',VO(I)
C  31   CONTINUE          
C     ENDIF

      END

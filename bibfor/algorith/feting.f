      SUBROUTINE FETING(NBSD,SDFETI,CHSECM,COLAUI,INFOFE,IFM)
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
C    - FONCTION REALISEE:  INITIALISE LA COLLECTION TEMPORAIRE COLAUI
C      EN CROISANT LES INFORMATIONS DU PROF_CHNO ET DE SD_FETI
C      SOIT V=COLAUI(NUM_SD) ET W=SDFETI.FETG(NUM_SD)
C      V(2*(I-1)+1)=NUMERO D'EQUATION DE LA PREMIERE COMPOSANTE DU
C                   NOEUD POINTE PAR W(2*(I-1)+2) DANS LE PROF_CHNO
C                  DU SOUS-DOMAINE NUM_SD
C      V(2*(I-1)+2)=SON NOMBRE DE COMPOSANTES
C
C     IN     NBSD  :  IN : NOMBRE DE SOUS-DOMAINES
C     IN     SDFETI: CH19: SD DECRIVANT LE PARTIONNEMENT FETI
C     IN    CHSECM : CH19: CHAM_NO SECOND MEMBRE GLOBAL POUR RECUPERER
C                          LES INFOS RELATIVES AUX PROF_CHNOS
C     IN/OUT COLAUI : CH24: NOM DE LA COLLECTION TEMPORAIRE
C
C ATTENTION: ON SUPPOSE QUE LA RENUMEROTATION A L'ORIGINE DU PROF_CHNO
C            TRAVAILLE PAR NOEUD ET NON PAR DDL. C'EST-A-DIRE QUE LES
C            NUMERO D'EQUATIONS DES DDLS SE SUIVENT.
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       13/02/04 (OB): CREATION.
C----------------------------------------------------------------------
C RESPONSABLE BOITEAU O.BOITEAU
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      INTEGER      NBSD,IFM
      CHARACTER*19 SDFETI,CHSECM
      CHARACTER*24 COLAUI,INFOFE

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
      INTEGER      IDD,IFETG,IAUX1,NB,K,IFETC,IDEEQ,NBDDL,IREFE,
     &             IAUX2,NBDDL1,IAUX3,ICMP,IEQ,NBCMP,IFETB,ICOL,J,TESTA
      CHARACTER*8  K8BID,NOMSD
      CHARACTER*19 CHSMDD,PRFCHN
      CHARACTER*24 CH24B
      CHARACTER*32 JEXNUM,JEXNOM
      LOGICAL      FIRST
      
C CORPS DU PROGRAMME
      CALL JEMARQ()
      
C INITIALISATION      
      CALL JEVEUO(CHSECM//'.FETC','L',IFETC)      
      CH24B=SDFETI//'.FETG'
      CALL JECREC(COLAUI,'V V I','NO','DISPERSE','VARIABLE',NBSD)
     
C----------------------------------------------------------------------
C BOUCLE SUR LES SOUS-DOMAINES
C----------------------------------------------------------------------
      DO 100 IDD=1,NBSD
        CALL JEMARQ()
C SECOND MEMBRE LOCAL AU SOUS-DOMAINE
        CHSMDD=ZK24(IFETC+IDD-1)(1:19)
        CALL JEVEUO(CHSMDD//'.REFE','L',IREFE)
C PROF_CHNO DU SOUS-DOMAINE IDD 
        PRFCHN=ZK24(IREFE+1)(1:19)

C NOMBRE DE DDL DU SOUS-DOMAINE IDD
        CALL JEVEUO(PRFCHN//'.DEEQ','L',IDEEQ)
        CALL JELIRA(PRFCHN//'.DEEQ','LONMAX',NBDDL,K8BID)
        NBDDL1=(NBDDL/2)-1
        
        CALL JEVEUO(JEXNUM(CH24B,IDD),'L',IFETG)
        CALL JELIRA(JEXNUM(CH24B,IDD),'LONMAX',NB,K8BID)

C CREATION DE LA COLLECTION COLAUI
        CALL JENUNO(JEXNUM(SDFETI//'.FETA',IDD),NOMSD)
        CALL JECROC(JEXNOM(COLAUI,NOMSD))
        CALL JEECRA(JEXNOM(COLAUI,NOMSD),'LONMAX',NB,K8BID)             
C NOMBRE D'ELEMENTS A RECHERCHER DANS .FETG     
        NB=(NB/2)-1
C ZONE DE STOCKAGE DANS COLAUI
        CALL JEVEUO(JEXNUM(COLAUI,IDD),'E',ICOL)

C STRUCTURE DE DONNEES LIEE AUX DDLS
        CALL JEVEUO(JEXNUM(SDFETI//'.FETB',IDD),'L',IFETB)
                
C----------------------------------------------------------------------
C BOUCLE SUR LES ELEMENTS DE SDFETI.FETG
C----------------------------------------------------------------------
        DO 50 K=0,NB

C NUMERO DU NOEUD DANS LE MAILLAGE              
          IAUX1=ZI(IFETG+2*K+1)
          IAUX1=ABS(ZI(IFETB+2*(IAUX1-1)))
          
          FIRST=.TRUE.
          NBCMP=0
          IEQ=0
C----------------------------------------------------------------------
C BOUCLE SUR LES ELEMENTS DE PROF_CHNO(IDD).DEEQ
C----------------------------------------------------------------------
          DO 30 J=0,NBDDL1
C SON NUMERO DE NOEUD     
            IAUX2=ZI(IDEEQ+2*J)

C ON A TROUVE LE MEME NOEUD DU MAILLAGE QUE DANS .FETG      
            IF (IAUX2.EQ.IAUX1) THEN

C SON NUMERO DE COMPOSANTE          
              IAUX3=ZI(IDEEQ+2*J+1)
              
              IF (IAUX3.GT.0) THEN
C CE N'EST PAS UN NOEUD DE DUALISATION DE LAGRANGE            
                IF (FIRST) THEN
                  FIRST=.FALSE.
C PREMIER NUMERO DE COMPOSANTE TROUVE POUR LE NOEUD IAUX2               
                  ICMP=IAUX3
C NUMERO D'EQUATION CORRESPONDANTE              
                  IEQ=J+1
                ENDIF

C TESTS DE VALIDITE POUR VERIFIER QUE LES COMPOSANTES SONT PAR ORDRE
C CROISSANT (RENUMEROTATION MD, MDA ET METIS)         
                IF (IAUX3.LT.ICMP) THEN
                  CALL UTDEBM('F','FETING','ICMP DANS LE DESORDRE POUR')
                  CALL UTIMPI('S',' NOEUD= ',1,IAUX2)
                  CALL UTIMPI('L',' ET SOUS-DOMAINE= ',1,IDD)
                  CALL UTFINM
                ELSE IF (IAUX3.GT.ICMP) THEN
                  ICMP=IAUX3                            
                ENDIF
              
                NBCMP=NBCMP+1
              ENDIF
            ENDIF 
          
   30     CONTINUE

C TESTS DE COHERENCE
          TESTA=IEQ*NBCMP      
          CALL ASSERT(TESTA.GT.0)
          
          ZI(ICOL+2*K)=IEQ
          ZI(ICOL+2*K+1)=NBCMP

   50   CONTINUE
        CALL JEDEMA()   
  100 CONTINUE

C MONITORING
      IF (INFOFE(1:1).EQ.'T') 
     &  WRITE (IFM,*)'<FETI/FETING> CREATION OBJET JEVEUX ',COLAUI(1:19)
      IF (INFOFE(2:2).EQ.'T')
     &  CALL UTIMSD(IFM,2,.FALSE.,.TRUE.,COLAUI(1:19),1,' ')
      CALL JEDEMA()
      END

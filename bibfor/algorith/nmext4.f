      SUBROUTINE NMEXT4(NOMA  ,CHAMP ,NOMCHA,NBCMP ,NBMA  ,
     &                  NBPI  ,NBSPI ,EXTRGA,EXTRCH,EXTRCP,
     &                  LISTMA,LISTPI,LISTSP,LISTCP,CHGAUS,
     &                  CHELGA,CHELES)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/01/2011   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT      NONE
      INTEGER       NBCMP,NBMA,NBPI,NBSPI
      CHARACTER*8   NOMA
      CHARACTER*16  NOMCHA
      CHARACTER*8   EXTRCP,EXTRCH,EXTRGA
      CHARACTER*24  LISTMA,LISTPI,LISTSP,LISTCP
      CHARACTER*19  CHAMP,CHGAUS,CHELGA
      CHARACTER*19  CHELES
C
C ----------------------------------------------------------------------
C
C ROUTINE *_NON_LINE (OBSERVATION - UTILITAIRE)
C
C EXTRAIRE LES VALEURS - CAS DES CHAMPS AUX POITNS DE GAUSS
C
C ----------------------------------------------------------------------
C
C
C IN  NOMA   : NOM DU MAILLAGE
C IN  CHAMP  : CHAMP OBSERVE
C IN  NOMCHA : NOM DU CHAMP
C IN  NBCMP  : NOMBRE DE COMPOSANTES DANS LA SD
C IN  NBMA   : NOMBRE DE MAILLES DANS LA SD
C IN  NBPI   : NOMBRE DE POINTS D'INTEGRATION 
C IN  NBSPI  : NOMBRE DE SOUS-POINTS D'INTEGRATION
C IN  EXTRGA : TYPE D'EXTRACTION SUR UNE MAILLE
C IN  EXTRCH : TYPE D'EXTRACTION SUR LE CHAMP
C IN  EXTRCP : TYPE D'EXTRACTION SUR LES COMPOSANTES
C IN  LISTMA : LISTE CONTENANT LES MAILLES
C IN  LISTCP : LISTE DES COMPOSANTES
C IN  LISTPI : LISTE CONTENANT LES POINTS D'EXTRACTION
C IN  LISTSP : LISTE CONTENANT LES SOUS-POINTS D'EXTRACTION
C IN  CHELGA : VECTEUR DE TRAVAIL CHAMPS AUX ELEMENTS
C IN  CHGAUS : VECTEUR DE TRAVAIL CHAMPS AUX POINTS DE GAUSS
C IN  CHELES : CHAM_ELEM_S REDUIT DE <CHAMP>
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      CHARACTER*32 JEXNUM
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
      INTEGER      NPARX
      PARAMETER    (NPARX=20)
      REAL*8       VALRES(NPARX)
C
      INTEGER      JGAUS,JELGA,JCESD
      INTEGER      JMA,JPI,JSPI
      INTEGER      IMA,IPI,ISPI,IMAR,IPIR,ISPIR,NUMMAI
      INTEGER      NUM,SNUM
      INTEGER      NTPT,NTSPT
      CHARACTER*8  NOMMAI
      INTEGER      IVALCP,NVALCP
      REAL*8       VALR,VAL2R
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- ACCES AUX CHAMPS DE TRAVAIL
C
      CALL JEVEUO(CHGAUS,'E',JGAUS)
      CALL JEVEUO(CHELGA,'E',JELGA) 
      CALL JEVEUO(CHELES(1:19)//'.CESD','L',JCESD)
      CALL ASSERT(NBCMP.LE.NPARX)
C
C --- ACCES LISTE DES MAILLES/POINTS/SOUS_POINTS         
C
      CALL JEVEUO(LISTMA,'L',JMA)
      IF (EXTRGA.EQ.'VALE') THEN
        IF (NBPI.NE.0)  CALL JEVEUO(LISTPI,'L',JPI)
        IF (NBSPI.NE.0) CALL JEVEUO(LISTSP,'L',JSPI)
      ENDIF   
C
C --- BOUCLE SUR LES MAILLES
C
      DO 30 IMA = 1,NBMA
C      
C ----- MAILLE COURANTE
C        
        NUMMAI  = ZI(JMA-1+IMA)
        CALL JENUNO(JEXNUM(NOMA(1:8)//'.NOMMAI',NUMMAI),NOMMAI)
C
C ----- NOMBRE DE POINTS/SOUS-POINTS
C
        IF (EXTRGA.EQ.'VALE') THEN
          NTPT   = NBPI
          NTSPT  = NBSPI
        ELSE
          NTPT   = ZI(JCESD+5+4*(IMA-1))
          NTSPT  = ZI(JCESD+5+4*(IMA-1)+1)
        ENDIF
C
C ----- BOUCLE SUR LES POINTS/SOUS_POINTS
C
        DO 45 IPI = 1,NTPT
          DO 46 ISPI = 1,NTSPT
C
C --------- NUMERO DES POINTS/SOUS-POINTS             
C
            IF (EXTRGA.EQ.'VALE') THEN
              NUM    = ZI(JPI-1+IPI  )
              SNUM   = ZI(JSPI-1+ISPI ) 
            ELSE
              NUM    = IPI
              SNUM   = ISPI              
            ENDIF 
C            
C --------- EXTRACTION DES VALEURS AUX POINTS DE GAUSS           
C
            CALL NMEXTJ(NOMA  ,NOMMAI,NOMCHA,CHAMP ,NBCMP ,
     &                  LISTCP,EXTRCP,NUM   ,SNUM  ,NVALCP,
     &                  VALRES) 
C
C --------- INDICE D'ACCES
C
            IF (EXTRGA.EQ.'VALE') THEN
              IPIR   = IPI
              ISPIR  = ISPI
            ELSE
              IPIR   = 1
              ISPIR  = 1               
            ENDIF
C                 
C --------- CALCUL DES VALEURS         
C
            DO 47 IVALCP = 1,NVALCP
              VALR   = VALRES(IVALCP)   
              VAL2R  = ZR(JGAUS+NPARX*(IVALCP-1)
     &                         +NBPI*(IPIR-1) 
     &                         +NBSPI*(ISPIR-1))        
              IF (EXTRGA.EQ.'VALE') THEN
                ZR(JGAUS+NPARX*(IVALCP-1)
     &                  +NBPI*(IPIR-1) 
     &                  +NBSPI*(ISPIR-1)) = VALR
              ELSEIF (EXTRGA.EQ.'MAX') THEN   
                ZR(JGAUS+NPARX*(IVALCP-1)
     &                  +NBPI*(IPIR-1) 
     &                  +NBSPI*(ISPIR-1)) = MAX(VALR,VAL2R)      
              ELSEIF (EXTRGA.EQ.'MIN') THEN
                ZR(JGAUS+NPARX*(IVALCP-1)
     &                  +NBPI*(IPIR-1) 
     &                  +NBSPI*(ISPIR-1)) = MIN(VALR,VAL2R)
                               
              ELSEIF (EXTRGA.EQ.'MOY') THEN
                ZR(JGAUS+NPARX*(IVALCP-1)
     &                  +NBPI*(IPIR-1) 
     &                  +NBSPI*(ISPIR-1)) = VALR+VAL2R                
              ELSE
                CALL ASSERT(.FALSE.)
              ENDIF
  47        CONTINUE
  46      CONTINUE
  45    CONTINUE
C
C ----- AFFECTATION DES VALEURS - CAS <VALE>: UNE VALEUR PAR MAILLE
C        
        IF (EXTRCH.EQ.'VALE') THEN 
          IMAR = IMA
          DO 75 IPI = 1,NBPI
            DO 76 ISPI = 1,NBSPI 
              IF (EXTRGA.EQ.'VALE') THEN
                IPIR   = IPI
                ISPIR  = ISPI
              ELSE
                IPIR   = 1
                ISPIR  = 1               
              ENDIF
C                                
              DO 77 IVALCP = 1,NVALCP    
                VALR = ZR(JGAUS+NPARX*(IVALCP-1)
     &                         +NBPI*(IPIR-1) 
     &                         +NBSPI*(ISPIR-1))
                ZR(JELGA+NBMA*(IMAR-1)
     &                  +NPARX*(IVALCP-1)
     &                  +NBPI*(IPIR-1) 
     &                  +NBSPI*(ISPIR-1)) = VALR
                
  77          CONTINUE
  76        CONTINUE
  75      CONTINUE
        ENDIF
  30  CONTINUE
C      
C --- AFFECTATION DES VALEURS - UNE VALEUR POUR TOUTES LES MAILLES
C  
      IF ((EXTRCH.EQ.'MOY').OR.(EXTRCH.EQ.'MIN').OR.
     &    (EXTRCH.EQ.'MAX')) THEN
        IMAR   = 1
        DO 175 IPI = 1,NBPI
          DO 176 ISPI = 1,NBSPI 
            IF (EXTRGA.EQ.'VALE') THEN
              IPIR   = IPI
              ISPIR  = ISPI
            ELSE
              IPIR   = 1
              ISPIR  = 1               
            ENDIF
C                            
            DO 177 IVALCP = 1,NVALCP             
              VALR   = ZR(JGAUS+NPARX*(IVALCP-1)
     &                         +NBPI*(IPIR-1) 
     &                         +NBSPI*(ISPIR-1))
              VAL2R  = ZR(JELGA+NBMA*(IMAR-1)
     &                         +NPARX*(IVALCP-1)
     &                         +NBPI*(IPIR-1) 
     &                         +NBSPI*(ISPIR-1))         
              IF (EXTRCH.EQ.'MAX') THEN
                ZR(JELGA+NBMA*(IMAR-1)
     &                         +NPARX*(IVALCP-1)
     &                         +NBPI*(IPIR-1) 
     &                         +NBSPI*(ISPIR-1)) =
     &          MAX(VALR,VAL2R)              
              ELSEIF (EXTRCH.EQ.'MIN') THEN
                ZR(JELGA+NBMA*(IMAR-1)
     &                         +NPARX*(IVALCP-1)
     &                         +NBPI*(IPIR-1) 
     &                         +NBSPI*(ISPIR-1)) =
     &          MIN(VALR,VAL2R)
              ELSEIF (EXTRCH.EQ.'MOY') THEN
                ZR(JELGA+NBMA*(IMAR-1)
     &                         +NPARX*(IVALCP-1)
     &                         +NBPI*(IPIR-1) 
     &                         +NBSPI*(ISPIR-1)) =
     &          VALR+VAL2R
              ELSE
                CALL ASSERT(.FALSE.)
              ENDIF
  177       CONTINUE
  176     CONTINUE
  175   CONTINUE        
        IF (EXTRCH.EQ.'MOY') THEN   
          DO 275 IPI = 1,NBPI
            DO 276 ISPI = 1,NBSPI 
C ----------- INDICE D'ACCES
              IF (EXTRGA.EQ.'VALE') THEN
                IPIR   = IPI
                ISPIR  = ISPI
              ELSE
                IPIR   = 1
                ISPIR  = 1               
              ENDIF
C                            
              DO 277 IVALCP = 1,NVALCP             
                ZR(JELGA+NBMA*(IMAR-1)
     &                         +NPARX*(IVALCP-1)
     &                         +NBPI*(IPIR-1) 
     &                         +NBSPI*(ISPIR-1)) =
     &          ZR(JELGA+NBMA*(IMAR-1)
     &                         +NPARX*(IVALCP-1)
     &                         +NBPI*(IPIR-1) 
     &                         +NBSPI*(ISPIR-1))/NBMA
  277         CONTINUE
  276       CONTINUE
  275     CONTINUE  
        ENDIF
      ENDIF   
C
      END

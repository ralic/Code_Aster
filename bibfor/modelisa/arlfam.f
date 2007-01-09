      SUBROUTINE ARLFAM(MAIL  ,NOMARL,NOM1  ,NOM2  ,NAPP  ,
     &                  TYPMAI,NOMC  ,ISMED ,QUADRA)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 09/01/2007   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                                                                       
C                                                                       
C ======================================================================
C RESPONSABLE ABBAS M.ABBAS
C 
      IMPLICIT NONE
      CHARACTER*10  NOM1,NOM2,NOMC
      CHARACTER*10  QUADRA
      CHARACTER*16  TYPMAI     
      CHARACTER*8   MAIL,NOMARL
      INTEGER       NAPP 
      LOGICAL       ISMED   
C      
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C REGROUPEMENT DES INTEGRALES A CALCULER POUR LA JONCTION
C
C ----------------------------------------------------------------------
C
C
C IN  MAIL   : NOM DU MAILLAGE
C IN  NOMARL : NOM DE LA SD PRINCIPALE ARLEQUIN
C IN  NOM1   : NOM DE LA SD DE STOCKAGE MAILLES GROUP_MA_1 
C IN  NOM2   : NOM DE LA SD DE STOCKAGE MAILLES GROUP_MA_2 
C IN  TYPMAI : SD CONTENANT NOM DES TYPES ELEMENTS (&&CATA.NOMTM)
C I/O NOMC   : NOM DE LA SD POUR LE COLLAGE 
C IN  ISMED  : VAUT .TRUE. SI ZONE DE COLLAGE EST LA ZONE MEDIATRICE
C I/O NAPP   : NOMBRE DE COUPLES D'APPARIEMENT
C OUT QUADRA : SD DES QUADRATURES A CALCULER
C
C
C SD PRODUITE
C   LES INTEGRALES A CALCULER SONT REGROUPEES SUIVANT DES FAMILLES
C   CHACUNE DES FAMILLES CORRESPOND A UN TYPE DE MAILLE SUR LEQUEL
C   SE FAIT L'INTEGRATION ET AU NUMERO DE LA FORMULE D'INTEGRATION
C   A UTILISER.
C
C QUAD.TYPEMA   : VECTEUR (K8) DES TYPES DE MAILLE ASSOCIES AUX FAMILLES
C                 (TMA FAM.1, TMA FAM.2, ...)
C QUAD.NUMERO   : VECTEUR DES NUMERO DE FORMULE D'INTEGRATION (NFI)
C                 (NFI FAM.1, NFI FAM.2, ...)
C QUAD.MAMA     : LISTE DES COUPLES DE MAILLES A INTEGRER
C                 (MA1.1, MA1.2, MA2.1, MA2.2, MA3.1, MA3.2, ... ) 
C                    SI MA*.1 > 0, INTEGRATION SUR MA*.1
C                    SI MA*.1 < 0, INTEGRATION SUR MA*.2
C                    SI MA*.2 > 0, INTEGRATION STANDARD (INCLUSION)
C                    SI MA*.2 < 0, INTEGRATION PAR SOUS-MAILLES 
C                       MA*.1 INDEX DANS .GROUPEMA ASSOCIE AUX MULTIPLIC
C                       MA*.2 INDEX DANS L'AUTRE .GROUPEMA 
C QUAD.LIMAMA   : LISTES COUPLES DE MAILLE ASSOCIEES AUX FAMILLES
C                 (XC V I NUMERO VARIABLE)
C                 [FAM. 1] (COUPLE1.1, COUPLE1.2, ...)
C                 [FAM. 2] (COUPLE2.1, COUPLE2.2, ...)
C                    ...
C                           COUPLE*.* INDEX DANS QUAD.MAMA
C
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*32       JEXNUM , JEXATR
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
C      
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      CHARACTER*16  NOMBO1,NOMBO2
      CHARACTER*8   TMS,TM1,TM2,K8BID
      CHARACTER*24  NOMAPP
      CHARACTER*19  NGRMA1,NGRMA2
      INTEGER       DIME,NMA,NFAM,M1,M2,I,J,LCOQUE
      INTEGER       A0,A1,A2,B0,B1,B2,B3,B4,B5,P0,P1
      INTEGER       C0,C1,C2,C3,C4,C5,D0,D1,D2,D3,D4,D5
      REAL*8        H1,H2
      REAL*8        ARLGER,PREC
      LOGICAL       LINCLU,MINCLU
      INTEGER       JTYPMM,JCOLM      
C      
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      PREC = ARLGER(NOMARL,'PRECBO')
      CALL JEVEUO(TYPMAI,'L',JTYPMM)
      CALL JEVEUO(NOMC(1:10)//'.MAILLE','E',JCOLM)
C
C --- LECTURE DONNEES MAILLAGE
C
      CALL JEVEUO(MAIL(1:8)//'.TYPMAIL','L',A0)
C
C --- LECTURE GRAPHE APPARIEMENT
C   
      NOMAPP = NOMARL(1:8)//'.GRAPH'
      CALL JEVEUO(NOMAPP,'L',A1)
      CALL JELIRA(NOMAPP,'NMAXOC',NMA,K8BID)
      CALL JEVEUO(JEXATR(NOMAPP,'LONCUM'),'L',A2)      
C
C --- LECTURE DONNEES GROUPE DE MAILLES
C         
      NGRMA1 = NOM1(1:10)//'.GROUPEMA'
      NGRMA2 = NOM2(1:10)//'.GROUPEMA'      
      CALL JEVEUO(NGRMA1(1:19),'L',B0)
      CALL JEVEUO(NGRMA2(1:19),'L',C0)       
C
C --- LECTURE DONNEES BOITES APPARIEMENT
C        
      NOMBO1 = NOM1(1:10)//'.BOITE'
      CALL JEVEUO(NOMBO1(1:16)//'.DIME','L',B1)
      CALL JEVEUO(NOMBO1(1:16)//'.MINMAX','L',B2)
      CALL JEVEUO(NOMBO1(1:16)//'.PAN','L',B3)
      CALL JEVEUO(NOMBO1(1:16)//'.SOMMET','L',B4)
      CALL JEVEUO(NOMBO1(1:16)//'.H','L',B5)
C
      NOMBO2 = NOM2(1:10)//'.BOITE'
      CALL JEVEUO(NOMBO2(1:16)//'.DIME','L',C1)
      CALL JEVEUO(NOMBO2(1:16)//'.MINMAX','L',C2)
      CALL JEVEUO(NOMBO2(1:16)//'.PAN','L',C3)
      CALL JEVEUO(NOMBO2(1:16)//'.SOMMET','L',C4)
      CALL JEVEUO(NOMBO2(1:16)//'.H','L',C5)
      DIME = ZI(C1)
C
      IF (DIME.EQ.2) THEN
        TMS = 'TRIA3'
      ELSEIF (DIME.EQ.3) THEN
        TMS = 'TETRA4'
      ELSE
        CALL ASSERT(.FALSE.)  
      ENDIF
C
C --- ALLOCATIONS OBJETS TEMPORAIRES
C
      CALL WKVECT('&&ARLFAM.TYPEMA' ,'V V K8',NAPP  ,D0)
      CALL WKVECT('&&ARLFAM.NUMERO' ,'V V I' ,NAPP  ,D1)
      CALL WKVECT('&&ARLFAM.NMAMA'  ,'V V I' ,NAPP  ,D2)
      CALL WKVECT('&&ARLFAM.FAMILLE','V V I' ,NAPP  ,D3)
      CALL WKVECT(QUADRA(1:10)//'.MAMA','V V I',2*NAPP,D4)
C
      D5 = D3
      NFAM = 0
      NAPP = 0
C
C --- ECRITURE QUADRATURE.MAMA ET FAMILLES TEMPORAIRES
C
      P1 = ZI(A2)

      DO 20 M1 = 1, NMA
         
        P0 = P1
        A2 = A2 + 1
        P1 = ZI(A2)

        IF (.NOT.ZL(JCOLM+M1-1)) GOTO 20

        TM1 = ZK8(JTYPMM+ZI(A0-1+ZI(B0-1+M1))-1)
        CALL TMACOQ(TM1,DIME,LCOQUE)
        H1 = ZR(B5-1+M1)

        DO 30 J = P0, P1-1

          M2  = ZI(A1-1+J)
          TM2 = ZK8(JTYPMM+ZI(A0-1+ZI(C0-1+M2))-1)
          CALL TMACOQ(TM2,DIME,LCOQUE)

C ------- M2 INCLUSE DANS M1 ?

          LINCLU = MINCLU(DIME  ,PREC  ,M2     ,ZI(C1) ,ZR(C2) ,
     &                    ZR(C4),M1    ,ZI(B1) ,ZR(B3) )
          IF (LINCLU) THEN
            IF (ISMED) THEN
              ZI(D4  ) = -M1
              ZI(D4+1) = M2
            ELSE
              ZI(D4  ) = M2
              ZI(D4+1) = M1
            ENDIF
            GOTO 40
          ENDIF

C ------- M1 INCLUSE DANS M2 ?

          LINCLU = MINCLU(DIME  ,PREC  ,M1     ,ZI(B1) ,ZR(B2) ,
     &                    ZR(B4),M2    ,ZI(C1) ,ZR(C3) )
          IF (LINCLU) THEN
            IF (ISMED) THEN
              ZI(D4  ) = M1
              ZI(D4+1) = M2
            ELSE
              ZI(D4  ) = -M2
              ZI(D4+1) = M1
            ENDIF
            GOTO 40
          ENDIF

C ------- INTEGRATION SPECIALE SUR LA MAILLE LA PLUS PETITE

          H2 = ZR(C5-1+M2)

          IF (ISMED) THEN
            IF (H1.LE.H2) THEN
              ZI(D4) = M1
            ELSE
              ZI(D4) = -M1
            ENDIF
            ZI(D4+1) = -M2
          ELSE
            IF (H2.LE.H1) THEN
              ZI(D4) = M2
            ELSE
              ZI(D4) = -M2
            ENDIF
            ZI(D4+1) = -M1
          ENDIF

C ------- FORMULE D'INTEGRATION ET CONSTRUCTION FAMILLES

 40       CONTINUE

          IF (ZI(D4+1).LT.0) THEN
            CALL ARLDEG(TMS   ,TM1   ,TM2   ,ZK8(D0),ZI(D1) ,
     &                  ZI(D2),NFAM  ,ZI(D5))
          ELSEIF ((ZI(D4).GT.0).EQV.ISMED) THEN
            CALL ARLDEG(TM1   ,TM1   ,TM2   ,ZK8(D0),ZI(D1) ,
     &                  ZI(D2),NFAM  ,ZI(D5))
          ELSE
            CALL ARLDEG(TM2   ,TM1   ,TM2   ,ZK8(D0),ZI(D1) ,
     &                  ZI(D2),NFAM  ,ZI(D5)) 
          ENDIF
C
          D4   = D4 + 2
          D5   = D5 + 1
          NAPP = NAPP + 1
C
 30     CONTINUE
 20   CONTINUE
C
C --- ECRITURE QUADRATURE.TYPEMA, .NUMERO ET .LIMAMA
C
      CALL WKVECT(QUADRA(1:10)//'.TYPEMA','V V K8',NFAM,C0)
      CALL WKVECT(QUADRA(1:10)//'.NUMERO','V V I',NFAM,C1)
      CALL JECREC(QUADRA(1:10)//'.LIMAMA','V V I','NU',
     &            'CONTIG','VARIABLE',NFAM)
      CALL JEECRA(QUADRA(1:10)//'.LIMAMA','LONT',NAPP,' ')
C
      DO 50 I = 1, NFAM
        CALL JECROC(JEXNUM(QUADRA(1:10)//'.LIMAMA',I))
        CALL JEECRA(JEXNUM(QUADRA(1:10)//'.LIMAMA',I),'LONMAX',
     &               ZI(D2-1+I),' ')
        ZK8(C0-1+I) = ZK8(D0-1+I)
        ZI(C1-1+I) = ZI(D1-1+I)
 50   CONTINUE
C
      CALL JEVEUO(QUADRA(1:10)//'.LIMAMA','E',C3)
      CALL JEVEUO(JEXATR(QUADRA(1:10)//'.LIMAMA','LONCUM'),'L',C2)
C
      DO 60 I = 1, NFAM
        ZI(D2-1+I) = ZI(C2-1+I) - 1
 60   CONTINUE
C
      DO 70 I = 1, NAPP
        D5 = D2-1+ZI(D3)
        D3 = D3 + 1
        ZI(C3+ZI(D5)) = I
        ZI(D5) = ZI(D5) + 1
 70   CONTINUE 
C
C --- DESALLOCATIONS
C
      CALL JEDETR('&&ARLFAM.TYPEMA')
      CALL JEDETR('&&ARLFAM.NUMERO')
      CALL JEDETR('&&ARLFAM.NMAMA')
      CALL JEDETR('&&ARLFAM.FAMILLE')
      CALL JEDETR('&&ARLFAM.QUADRA')     

      CALL JEDEMA()

      END

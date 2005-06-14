      SUBROUTINE ARLFAM(MAIL,NOM1Z,NOM2Z,NMM,NTM,COL,ID,NQUADZ)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 08/11/2004   AUTEUR DURAND C.DURAND 
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
C ----------------------------------------------------------------------
C    REGROUPEMENT DES INTEGRALES A CALCULER POUR LA JONCTION ARLEQUIN
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE 
C CHARACTER*8    MAIL      : SD MAILLAGE
C CHARACTER*(10) NOM1Z     : SD DOMAINE MECANIQUE 1
C CHARACTER*(10) NOM2Z     : SD DOMAINE MECANIQUE 2
C INTEGER        NMM       : NOMBRE DE COUPLES D'APPARIEMENT
C CHARACTER*8    NTM(*)    : VECTEUR NOMS TYPES DE MAILLE
C LOGICAL        COL(*)    : FILTRE DECRIVANT ZONE DE COLLAGE
C                            SOUS-ENSEMBLE DU DOMAINE 1
C LOGICAL        ID        : .TRUE. SI LES MULTIPLICATEURS SONT 
C                            DEFINIS SUR DOMAINE 1
C CHARACTER*(10) NQUADZ    : SD QUADRATURES A CALCULER 
C
C SD D'ENTREE
C NOM1.NOM2     : SD GRAPHE D'APPARIEMENT (CF ARLAPP)
C NOM1.GROUPEMA : LISTE DES MAILLES DOMAINE 1 
C NOM1.BOITE    : SD BOITES ENGLOBANTES (CF BOITE)
C NOM2.GROUPEMA : LISTE DES MAILLES DOMAINE 2 
C NOM2.BOITE    : SD BOITES ENGLOBANTES (CF BOITE)
C
C SD DE SORTIE
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
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------

C --- VARIABLES
      CHARACTER*(*) NOM1Z,NOM2Z,NQUADZ
      CHARACTER*10  NOM1,NOM2,NQUADR
      CHARACTER*8   MAIL,NTM(*),TMS,TM0,TM1,TM2
      INTEGER       DIM,NC,NMM,NMA,NF,M1,M2,I,J,L
      INTEGER       A0,A1,A2,B0,B1,B2,B3,B4,B5,P0,P1
      INTEGER       C0,C1,C2,C3,C4,C5,D0,D1,D2,D3,D4,D5
      REAL*8        H1,H2
      LOGICAL       COL(*),ID,IR

      NOM1 = NOM1Z
      NOM2 = NOM2Z
      NQUADR = NQUADZ
      
C --- LECTURE DONNEES

      CALL JEMARQ()

      CALL JEVEUO(MAIL//'.TYPMAIL','L',A0)
      CALL JEVEUO(NOM1//'.'//NOM2,'L',A1)
      CALL JEVEUO(JEXATR(NOM1//'.'//NOM2,'LONCUM'),'L',A2)
      CALL JELIRA(NOM1//'.'//NOM2,'NMAXOC',NMA,ZK8)

      CALL JEVEUO(NOM1//'.GROUPEMA','L',B0)
      CALL JEVEUO(NOM1//'.BOITE.DIME','L',B1)
      CALL JEVEUO(NOM1//'.BOITE.MINMAX','L',B2)
      CALL JEVEUO(NOM1//'.BOITE.PAN','L',B3)
      CALL JEVEUO(NOM1//'.BOITE.SOMMET','L',B4)
      CALL JEVEUO(NOM1//'.BOITE.H','L',B5)

      CALL JEVEUO(NOM2//'.GROUPEMA','L',C0)
      CALL JEVEUO(NOM2//'.BOITE.DIME','L',C1)
      CALL JEVEUO(NOM2//'.BOITE.MINMAX','L',C2)
      CALL JEVEUO(NOM2//'.BOITE.PAN','L',C3)
      CALL JEVEUO(NOM2//'.BOITE.SOMMET','L',C4)
      CALL JEVEUO(NOM2//'.BOITE.H','L',C5)

      DIM = ZI(C1)

      IF (DIM.EQ.2) THEN
        TMS = 'TRIA3'
      ELSE
        TMS = 'TETRA4'
      ENDIF

C --- ALLOCATIONS

      CALL WKVECT('&&ARLFAM.TYPEMA','V V K8',NMM,D0)
      CALL WKVECT('&&ARLFAM.NUMERO','V V I',NMM,D1)
      CALL WKVECT('&&ARLFAM.NMAMA','V V I',NMM,D2)
      CALL WKVECT('&&ARLFAM.FAMILLE','V V I',NMM,D3)
      CALL WKVECT(NQUADR//'.MAMA','V V I',2*NMM,D4)

      DO 10 I = 1, NMM
        ZI(D2-1+I) = 0
 10   CONTINUE

      D5 = D3
      NF = 0
      NMM = 0

C --- ECRITURE QUADRATURE.MAMA ET FAMILLES TEMPORAIRES

      P1 = ZI(A2)

      DO 20 M1 = 1, NMA
         
        P0 = P1
        A2 = A2 + 1
        P1 = ZI(A2)

        IF (.NOT.COL(M1)) GOTO 20

        TM1 = NTM(ZI(A0-1+ZI(B0-1+M1)))
        CALL TMACOQ(TM1,DIM,L)
        H1 = ZR(B5-1+M1)

        DO 30 J = P0, P1-1

          M2 = ZI(A1-1+J)
          TM2 = NTM(ZI(A0-1+ZI(C0-1+M2)))
          CALL TMACOQ(TM2,DIM,L)

C ------- M2 INCLUSE DANS M1 ?

          CALL MINCLU(DIM,M2,ZI(C1),ZR(C2),ZR(C4),M1,ZI(B1),ZR(B3),IR)
          IF (IR) THEN
            IF (ID) THEN
              ZI(D4  ) = -M1
              ZI(D4+1) = M2
            ELSE
              ZI(D4  ) = M2
              ZI(D4+1) = M1
            ENDIF
            GOTO 40
          ENDIF

C ------- M1 INCLUSE DANS M2 ?

          CALL MINCLU(DIM,M1,ZI(B1),ZR(B2),ZR(B4),M2,ZI(C1),ZR(C3),IR)
          IF (IR) THEN
            IF (ID) THEN
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

          IF (ID) THEN
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
            CALL ARLDEG(TMS,TM1,TM2,ZK8(D0),ZI(D1),ZI(D2),NF,ZI(D5))
          ELSEIF ((ZI(D4).GT.0).EQV.ID) THEN
            CALL ARLDEG(TM1,TM1,TM2,ZK8(D0),ZI(D1),ZI(D2),NF,ZI(D5))
          ELSE
            CALL ARLDEG(TM2,TM1,TM2,ZK8(D0),ZI(D1),ZI(D2),NF,ZI(D5)) 
          ENDIF

          D4 = D4 + 2
          D5 = D5 + 1
          NMM = NMM + 1

 30     CONTINUE

 20   CONTINUE

C --- ECRITURE QUADRATURE.TYPEMA, .NUMERO ET .LIMAMA

      CALL WKVECT(NQUADR//'.TYPEMA','V V K8',NF,C0)
      CALL WKVECT(NQUADR//'.NUMERO','V V I',NF,C1)
      CALL JECREC(NQUADR//'.LIMAMA','V V I','NU','CONTIG','VARIABLE',NF)
      CALL JEECRA(NQUADR//'.LIMAMA','LONT',NMM,' ')

      DO 50 I = 1, NF

        CALL JECROC(JEXNUM(NQUADR//'.LIMAMA',I))
        CALL JEECRA(JEXNUM(NQUADR//'.LIMAMA',I),'LONMAX',ZI(D2-1+I),' ')

        ZK8(C0-1+I) = ZK8(D0-1+I)
        ZI(C1-1+I) = ZI(D1-1+I)

 50   CONTINUE

      CALL JEVEUO(NQUADR//'.LIMAMA','E',C3)
      CALL JEVEUO(JEXATR(NQUADR//'.LIMAMA','LONCUM'),'L',C2)

      DO 60 I = 1, NF
        ZI(D2-1+I) = ZI(C2-1+I) - 1
 60   CONTINUE

      DO 70 I = 1, NMM

        D5 = D2-1+ZI(D3)
        D3 = D3 + 1
        ZI(C3+ZI(D5)) = I
        ZI(D5) = ZI(D5) + 1

 70   CONTINUE

C --- DESALLOCATIONS

      CALL JEDETR('&&ARLFAM.TYPEMA')
      CALL JEDETR('&&ARLFAM.NUMERO')
      CALL JEDETR('&&ARLFAM.NMAMA')
      CALL JEDETR('&&ARLFAM.FAMILLE')

      CALL JEDEMA()

      END

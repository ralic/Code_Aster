      SUBROUTINE ARLFAM(MAIL,NOMCZ,NOM1Z,NTM,NQUADZ)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 04/04/2002   AUTEUR VABHHTS J.PELLET 
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
C CHARACTER*(10) NOMCZ     : SD DOMAINE DE COLLAGE
C CHARACTER*(10) NOM1Z     : SD DOMAINE MECANIQUE
C CHARACTER*8    NTM(*)    : VECTEUR NOMS TYPES DE MAILLE
C CHARACTER*(10) NQUADZ    : SD QUADRATURES A CALCULER 
C
C SD D'ENTREE
C NOMC.GROUPEMA : LISTE DE MAILLES DOMAINE DE COLLAGE
C NOMC.BOITE    : SD BOITES ENGLOBANTES (CF BOITE)
C NOMC.NOM1     : SD GRAPHE D'APPARIEMENT (CF ARLAPP)
C NOM1.GROUPEMA : LISTE DES MAILLES DOMAINE 1 
C NOM1.BOITE    : SD BOITES ENGLOBANTES (CF BOITE)
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
C                 (MA1.C, MA1.1, MA2.C, MA2.1, MA3.C, MA3.1, ... ) 
C                    SI MA*.C = 0, COUPLAGE MA*.1 / MA*.1
C                    SI MA*.C > 0, INTEGRATION SUR MA*.C
C                    SI MA*.C < 0, INTEGRATION SUR MA*.1
C                    SI MA*.1 > 0, INTEGRATION STANDARD (INCLUSION)
C                    SI MA*.1 < 0, INTEGRATION PAR SOUS-MAILLE 
C                       MA*.C INDEX DANS NOMC.GROUPEMA
C                       MA*.1 INDEX DANS NOM1.GROUPEMA
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

C --- PARAMETRE
      REAL*8 PREC
      PARAMETER (PREC = 0.000001D0)

C --- VARIABLES
      CHARACTER*(*) NOMCZ,NOM1Z,NQUADZ
      CHARACTER*10  NOMC,NOM1,NQUADR
      CHARACTER*8   MAIL,NTM(*),TM1,TM2
      INTEGER       A0,A1,A2,B0,B1,B2,B3,B4,C0,C1,C2,C3 
      INTEGER       D0,D1,D2,D3,D4,D5,DIME,NM1,NMM,NF
      INTEGER       M1,M2,IM1,IM2,P0,P1,P2,I,J,L
      REAL*8        V1,V2
      LOGICAL       R

      NOMC = NOMCZ
      NOM1 = NOM1Z
      NQUADR = NQUADZ

C --- LECTURE DONNEES

      CALL JEMARQ()

      CALL JEVEUO(MAIL//'.TYPMAIL','L',A0)
      CALL JEVEUO(NOMC//'.'//NOM1,'L',A1)
      CALL JEVEUO(JEXATR(NOMC//'.'//NOM1,'LONCUM'),'L',A2)
      CALL JELIRA(NOMC//'.'//NOM1,'LONT',NMM,ZK8)
      CALL JELIRA(NOMC//'.'//NOM1,'NMAXOC',NM1,ZK8)

      CALL JEVEUO(NOMC//'.GROUPEMA','L',B0)
      CALL JEVEUO(NOMC//'.BOITE.DIME','L',B1)
      CALL JEVEUO(NOMC//'.BOITE.H','L',B2)
      CALL JEVEUO(NOMC//'.BOITE.MINMAX','L',B3)
      CALL JEVEUO(NOMC//'.BOITE.PAN','L',B4)

      CALL JEVEUO(NOM1//'.GROUPEMA','L',C0)
      CALL JEVEUO(NOM1//'.BOITE.DIME','L',C1)
      CALL JEVEUO(NOM1//'.BOITE.MINMAX','L',C2)
      CALL JEVEUO(NOM1//'.BOITE.SOMMET','L',C3)

      DIME = ZI(C1)

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

C --- ECRITURE QUADRATURE.MAMA ET FAMILLES TEMPORAIRES

      P1 = ZI(A2)

      DO 20 M1 = 1, NM1

        IM1 = ZI(B0-1+M1)
        TM1 = NTM(ZI(A0-1+IM1))
        CALL TMACOQ(TM1,DIME,L)

        P0 = P1
        A2 = A2 + 1
        P1 = ZI(A2)
        
C ----- UNE SEULE MAILLE EN VIS-A-VIS

        IF ((P1-P0).EQ.1) THEN

          M2 = ZI(A1-1+P0)
          IM2 = ZI(C0-1+M2)
          
C ------- MEME MAILLE ?

          IF (IM1.EQ.IM2) THEN

            ZI(D4) = 0
            D4 = D4 + 1
            TM2 = TM1

C ------- SINON M1 INCLUSE DANS M2

          ELSE

            ZI(D4) = M1
            D4 = D4 + 1
            TM2 = NTM(ZI(A0-1+IM2))
            CALL TMACOQ(TM2,DIME,L)

          ENDIF

          ZI(D4) = M2
          D4 = D4 + 1

          CALL ARLDEG(TM1,TM2,ZK8(D0),ZI(D1),ZI(D2),NF,ZI(D5))
          D5 = D5 + 1

          GOTO 20

        ENDIF

        P2 = B3 + 2*DIME*(M1-1)
        V1 = ZR(P2+1)-ZR(P2)
        DO 30 I = 2, DIME
          P2 = P2 + 2
          V1 = V1*(ZR(P2+1)-ZR(P2))
 30     CONTINUE

C ----- PLUSIEUR MAILLES EN VIS-A-VIS 

        DO 40 I = P0, P1-1

          M2 = ZI(A1-1+I)
          IM2 = ZI(C0-1+M2)
          TM2 = NTM(ZI(A0-1+IM2))
          CALL TMACOQ(TM2,DIME,L)

C ------- M2 INCLUSE DANS M1 ?

          CALL MINCLU(DIME,M2,ZI(C1),ZR(C2),ZR(C3),
     &                     M1,ZI(B1),ZR(B4),ZR(B2),2,PREC,R)
          
          IF (R) THEN

            ZI(D4) = -M1
            D4 = D4 + 1
            ZI(D4) = M2
            D4 = D4 + 1
            CALL ARLDEG(TM2,TM1,ZK8(D0),ZI(D1),ZI(D2),NF,ZI(D5))
            D5 = D5 + 1
          
C ------- INTEGRATION SPECIALE SUR LA MAILLE LA PLUS GRANDE

          ELSE
             
            P2 = C2 + 2*DIME*(M2-1)
            V2 = ZR(P2+1)-ZR(P2)
            DO 50 J = 2, DIME
              P2 = P2 + 2
              V2 = V2*(ZR(P2+1)-ZR(P2))
 50         CONTINUE

            IF (V1.LE.V2) THEN
              
              ZI(D4) = -M1
              D4 = D4 + 1
              CALL ARLDEG(TM2,TM1,ZK8(D0),ZI(D1),ZI(D2),NF,ZI(D5))
              D5 = D5 + 1

            ELSE

              ZI(D4) = M1
              D4 = D4 + 1
              CALL ARLDEG(TM1,TM2,ZK8(D0),ZI(D1),ZI(D2),NF,ZI(D5))
              D5 = D5 + 1

            ENDIF

            ZI(D4) = -M2
            D4 = D4 + 1

          ENDIF

 40     CONTINUE

 20   CONTINUE

C --- ECRITURE QUADRATURE.TYPEMA, .NUMERO ET .LIMAMA

      CALL WKVECT(NQUADR//'.TYPEMA','V V K8',NF,C0)
      CALL WKVECT(NQUADR//'.NUMERO','V V I',NF,C1)
      CALL JECREC(NQUADR//'.LIMAMA','V V I','NU','CONTIG','VARIABLE',NF)
      CALL JEECRA(NQUADR//'.LIMAMA','LONT',NMM,' ')

      DO 60 I = 1, NF

        CALL JECROC(JEXNUM(NQUADR//'.LIMAMA',I))
        CALL JEECRA(JEXNUM(NQUADR//'.LIMAMA',I),'LONMAX',ZI(D2-1+I),' ')

        ZK8(C0-1+I) = ZK8(D0-1+I)
        ZI(C1-1+I) = ZI(D1-1+I)

 60   CONTINUE

      CALL JEVEUO(NQUADR//'.LIMAMA','E',C3)
      CALL JEVEUO(JEXATR(NQUADR//'.LIMAMA','LONCUM'),'L',C2)

      DO 70 I = 1, NF
        ZI(D2-1+I) = ZI(C2-1+I) - 1        
 70   CONTINUE

      DO 80 I = 1, NMM

        D5 = D2-1+ZI(D3)
        D3 = D3 + 1
        ZI(C3+ZI(D5)) = I
        ZI(D5) = ZI(D5) + 1

 80   CONTINUE

C --- DESALLOCATIONS

      CALL JEDETR('&&ARLFAM.TYPEMA')
      CALL JEDETR('&&ARLFAM.NUMERO')
      CALL JEDETR('&&ARLFAM.NMAMA')
      CALL JEDETR('&&ARLFAM.FAMILLE')

      CALL JEDEMA()

      END

      SUBROUTINE APPARI(MAIL  ,TYPMAI,NORM  ,GRMAMA,DEGMAX,
     &                  NGRMA1,NOMBO1,NGRMA2,NOMBO2,NOMCOL,
     &                  NOMAPP,NOMARB,NBMAC ,TRAVR ,NHAPP ,
     &                  NAPP)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 12/02/2008   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_20 
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*8   MAIL
      CHARACTER*16  TYPMAI 
      CHARACTER*24  NOMCOL
      CHARACTER*10  NORM
      CHARACTER*24  NOMAPP 
      CHARACTER*24  GRMAMA     
      CHARACTER*16  NOMBO1,NOMBO2
      CHARACTER*16  NOMARB  
      CHARACTER*24  NGRMA1,NGRMA2  
      INTEGER       NHAPP  
      INTEGER       DEGMAX
      INTEGER       NBMAC
      INTEGER       NAPP
      CHARACTER*16  TRAVR
C      
C ----------------------------------------------------------------------
C
C APPARIEMENT DE DEUX GROUPES DE MAILLE PAR LA METHODE
C BOITES ENGLOBANTES + ARBRE BSP
C
C ROUTINE PRINCIPALE D'APPARIEMENT
C
C ----------------------------------------------------------------------
C
C
C APPARIEMENT DE NOMBO1 SUR NOMBO2, PAR FILTRAGE DES MAILLES SUR UNE
C ZONE DE COLLAGE (ZONE RESTREINTE DE GRMA2)
C
C
C IN  MAIL   : NOM UTILISATEUR DU MAILLAGE
C IN  TYPMAI : SD CONTENANT NOM DES TYPES ELEMENTS (&&CATA.NOMTM)
C IN  NOMCOL : VECTEUR INDIQUANT SI MAILLE A COLLER, DE LONGUEUR
C               NM2
C IN  NORM   : NOM DE LA SD POUR STOCKAGE DES NORMALES
C IN  NGRMA1 : NOM DE LA SD DE STOCKAGE MAILLES GROUP_MA_1 
C                SIMPLE LISTE CONTENANT LES NUMEROS ABSOLUS DES MAILLES
C                A APPARIER (LONG: NM2)
C IN  NOMBO1 : NOM DE LA SD DES BOITES POUR LES MAILLES DU GRMA1
C IN  NGRMA2 : NOM DE LA SD DE STOCKAGE MAILLES GROUP_MA_2 
C                SIMPLE LISTE CONTENANT LES NUMEROS ABSOLUS DES MAILLES
C                A APPARIER (LONG: NM2)
C IN  NOMBO2 : NOM DE LA SD DES BOITES POUR LES MAILLES DU GRMA1
C IN  GRMAMA : GRAPHE MAILLE-MAILLE ATTACHE A GRMA2 (VOIR ROUTINE 
C              GRMAMA)
C                 MAILLE -> LISTE DES MAILLES VOISINES  
C                 (IE. DIFFERENTES ET PARTAGEANT UN NOEUD)
C IN  DEGMAX : DEGRE MAXIMUM DU GRAPHE GRMAMA
C IN  NOMARB : ARBRE D'APPARIEMENT
C IN  NHAPP  : ECHANTILLONNAGE DE LA FRONTIERE
C IN  NBMAC  : NOMBRE DE MAILLES DE LA ZONE DE COLLAGE
C I/O TRAVR  : VECTEUR DE REELS DE TRAVAIL DE LONGUEUR 12+276*NHAPP     
C OUT NOMAPP : GRAPHE D'APPARIEMENT
C OUT NAPP   : NOMBRE DE COUPLES D'APPARIEMENT
C
C SD DE SORTIE
C NOMAPP : GRAPHE D'APPARIEMENT (XC V I NUMERO VARIABLE)
C              MAILLE DOMAINE 1 -> MAILLES DOMAINE 2 EN VIS-A-VIS
C              [MA1] : (MA2.1, MA2.2, MA2.3, ...)
C                      AVEC MA* INDEX NOM*.GROUPEMA
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*32  JEXATR,JEXNUM
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
      CHARACTER*8   TYPEMA,NOMMAI
      INTEGER       A0,A1,A2,A3,B0,B1,B2,B3
      INTEGER       C0,C1,C2,C3,C4,C5,C6,C7,C8
      INTEGER       Z0,Z1,E0(2),E1,E2,E3,E4
      INTEGER       DIME,NAPT,NVIS,NVIS0,NM1,NM2,NMA
      INTEGER       N
      INTEGER       IRET,LCOQUE
      INTEGER       JNORM
      INTEGER       NVOIS(2),NUMA,M1,M2,NNOH,DD1,DD2,I,J,K,L,P0,P1
      INTEGER       NOEARE(48),NOEPAN(60)
      INTEGER       NSOM,NPAN,NARE
      REAL*8        CNOEUD(81),R
      LOGICAL       LINTER,MINTER
      INTEGER       JTYPMM,JCOLM
      REAL*8        DDOT
      INTEGER       NBSOM,NBARE,NBPAN
      INTEGER       IFM,NIV
C      
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('ARLEQUIN',IFM,NIV)
C
C --- INITIALISATIONS
C 
      CALL JEVEUO(TYPMAI,'L',JTYPMM)
C
C --- FILTRE
C      
      CALL JEVEUO(NOMCOL(1:24),'E',JCOLM)
C
C --- LECTURE DONNEES MAILLAGE
C
      CALL JEVEUO(MAIL(1:8)//'.COORDO    .VALE','L', A0)
      CALL JEVEUO(MAIL(1:8)//'.CONNEX','L',A1)
      CALL JEVEUO(JEXATR(MAIL(1:8)//'.CONNEX','LONCUM'),'L',A2)
      CALL JEVEUO(MAIL(1:8)//'.TYPMAIL','L',A3)
C
C --- LECTURE DONNEES NORMALES
C  
      CALL JEEXIN(NORM,IRET)
      IF (IRET.NE.0) THEN
        CALL JEVEUO(NORM,'L',JNORM)
      ENDIF 
C
C --- LECTURE DONNEES GROUPE DE MAILLES
C         
      CALL JEVEUO(NGRMA1,'L',B0)
      CALL JEVEUO(NGRMA2,'L',C0)      
C
C --- LECTURE DONNEES BOITES APPARIEMENT
C           
      CALL JEVEUO(NOMBO1(1:16)//'.DIME','L',B1)
      CALL JEVEUO(NOMBO1(1:16)//'.MINMAX','L',B2)
      CALL JEVEUO(NOMBO1(1:16)//'.PAN','L',B3)
      DIME = ZI(B1)
      NM1  = ZI(B1+1)
C
      CALL JEVEUO(NOMBO2(1:16)//'.DIME','L',C1)
      CALL JEVEUO(NOMBO2(1:16)//'.MINMAX','L',C2)
      CALL JEVEUO(NOMBO2(1:16)//'.PAN','L',C3)      
      NM2  = ZI(C1+1)  
C
C --- LECTURE DONNEES GRAPHE MAILLE-MAILLE
C    
      IF (NM2.GT.1) THEN
        CALL JEVEUO(GRMAMA,'L',C6)
        CALL JEVEUO(JEXATR(GRMAMA,'LONCUM'),'L',C7)
      ENDIF      
C
C --- LECTURE DONNEES ARBRE APPARIEMENT
C        
      CALL JEVEUO(NOMARB(1:16)//'.CELL','L',C4)
      CALL JEVEUO(NOMARB(1:16)//'.LIMA','L',C5)
C
C --- CREATION GRAPHE APPARIEMENT
C            
      CALL JECREC(NOMAPP,'V V I','NU','CONTIG','VARIABLE',NM1)      
      DD1   = 2*DIME
      DD2   = DIME+2
C
C --- LECTURE VECTEUR DE TRAVAIL
C
      CALL JEVEUO(TRAVR,'E',Z0)      
C
C --- ALLOCATIONS OBJETS TEMPORAIRES
C
      NVIS0 = 2*DEGMAX*MAX(NBMAC,NM2)
      CALL WKVECT('&&APPARI.VOISIN1',      'V V I',NM2,E0(1))
      CALL WKVECT('&&APPARI.VOISIN2',      'V V I',NM2,E0(2))
      CALL WKVECT('&&APPARI.FILTRE',       'V V L',NM2,E1)
      CALL WKVECT('&&APPARI.LISTE.ENTETE', 'V V I',NM1,E2)
      CALL WKVECT('&&APPARI.LISTE.RESERVE','V V I',2*NVIS0,E3)
      CALL WKVECT('&&APPARI.NVISAVIS',     'V V I',NM1,E4)
C
C --- APPARIEMENT
C
      NAPP = 0
      NAPT = 0

      DO 10 M1 = 1, NM1

        IF (.NOT.ZL(JCOLM+M1-1)) GOTO 150
        
C --- CARACTERISTIQUES DE LA MAILLE
        NUMA   = ZI(B0-1+M1)
        CALL JENUNO(JEXNUM(MAIL(1:8)//'.NOMMAI',NUMA),NOMMAI)
        TYPEMA = ZK8(JTYPMM+ZI(A3-1+NUMA)-1)
        
C --- EXTENSION EVENTUELLE DES COQUES 
        CALL TMACOQ(TYPEMA,DIME,LCOQUE)

C --- COORDONNEES DES SOMMETS 
        IF (LCOQUE.EQ.0) THEN
          CALL COSOLI(NUMA  ,ZI(A1) ,ZI(A2) ,ZR(A0) ,DIME  ,
     &                CNOEUD)
        ELSE
          CALL COCOQU(NUMA  ,ZI(A1) ,ZI(A2) ,ZR(A0) ,'VARIABLE',
     &                0.D0  ,ZR(JNORM),DIME ,CNOEUD)
        ENDIF

C --- NOMBRE DE SOMMETS DE LA MAILLE
        NSOM = NBSOM(TYPEMA)
        
C --- NOMBRE D'ARETES DE LA MAILLE 
        NARE = NBARE(TYPEMA)       

C --- COORDONNEES DES NOEUDS DEFINISSANT LES ARETES DE LA MAILLE        
        CALL NOARE(TYPEMA,NOEARE)
        
        IF (DIME.EQ.3) THEN
C --- NOMBRE DE PANS DE LA MAILLE
          NPAN = NBPAN(TYPEMA) 
C --- COORDONNEES DES NOEUDS DEFINISSANT LES PANS DE LA MAILLE 
          CALL NOPAN (TYPEMA,NOEPAN)
        ELSE
          NPAN = 0  
        ENDIF  
        
C --- ECHANTILLONNAGE DE LA FRONTIERE DE LA MAILLE
        CALL ECHMAP(NOMMAI,TYPEMA,DIME  ,CNOEUD,NSOM  ,
     &              NOEARE,NARE  ,NOEPAN,NPAN  ,NHAPP ,
     &              ZR(Z0),NNOH)

C       
C --- APPARIEMENT PONCTUEL ?
C
        NVIS = 0
        DO 20 I = 1, NM2
          ZL(E1-1+I) = .TRUE.
 20     CONTINUE
C
        Z1 = Z0 - DIME
C       
C --- POUR CHAQUE POINT ECHANTILLONNE DE LA FRONTIERE
C
        DO 30 I = 1, NNOH

C
C --- QUELLES MAILLES POSSIBLEMENT APPARIEES AVEC CE POINT ?
C
          Z1 = Z1 + DIME
          CALL CERNE(ZR(Z1),DIME,ZI(C4),ZR(C3),NMA,J)
          C8 = C5 - 1 + J
C
C --- EXPLORATION DES MAILLES CANDIDATES
C
          DO 40 J = 1, NMA
            M2 = ZI(C8)
            C8 = C8 + 1
 
C
C --- FILTRAGE DES MAILLES DEJA APPARIEES
C                       
            IF (.NOT.ZL(E1-1+M2)) THEN

              GOTO 40
            ENDIF
C            
C --- TEST SUR BOITE MINMAX
C
            P0 = C2 + DD1*(M2-1)
C
            DO 50 K = 1, DIME
              R = ZR(Z1-1+K)
              IF ((R.LT.ZR(P0)).OR.(R.GT.ZR(P0+1))) THEN             
                GOTO 40
              ENDIF  
              P0 = P0 + 2
 50         CONTINUE
C
C --- TEST SUR LES PANS DE LA BOITE
C
            P0 = ZI(C1+2*M2)
            N  = ZI(C1+2*(M2+1)) - P0
            P0 = C3 + DD2*(P0-1)
C
            DO 60 K = 1, N
              R = DDOT(DIME,ZR(P0),1,ZR(Z1),1) + ZR(P0+DIME)
              IF (R.GT.0.D0) THEN  
                GOTO 40
              ENDIF  
              P0 = P0 + DD2
 60         CONTINUE
C
C ---  STOCKAGE: LE POINT EST DANS LA MAILLE
C
            ZL(E1-1+M2) = .FALSE.
            ZI(E3)      = M2
C            
            IF (NVIS.EQ.0) THEN
              ZI(E3+1) = 0
            ELSE
              ZI(E3+1) = ZI(E2-1+M1)
            ENDIF
C
            ZI(E2-1+M1) = E3
            NVIS        = NVIS  + 1
            E3          = E3 + 2
            NVIS0       = NVIS0 - 1
C            
            IF (NVIS0.LT.0) THEN
              CALL U2MESS('F','ARLEQUIN_13')
            ENDIF  
            GOTO 30
 40       CONTINUE
 30     CONTINUE
C
C --- CAS DE L'INCLUSION
C
        IF (NVIS.EQ.0) THEN

C --- POINT MILIEU DE LA PREMIERE MAILLE DOMAINE 2

C --- CARACTERISTIQUES DE LA MAILLE
          NUMA   = ZI(C0)
          TYPEMA = ZK8(JTYPMM+ZI(A3-1+NUMA)-1)
           
C --- EXTENSION EVENTUELLE DES COQUES 
          CALL TMACOQ(TYPEMA,DIME,LCOQUE)

C --- COORDONNEES DES SOMMETS 
          IF (LCOQUE.EQ.0) THEN
            CALL COSOLI(NUMA  ,ZI(A1) ,ZI(A2) ,ZR(A0) ,DIME  ,
     &                  CNOEUD)
          ELSE
            CALL COCOQU(NUMA  ,ZI(A1) ,ZI(A2) ,ZR(A0) ,'VARIABLE',
     &                  0.D0  ,ZR(JNORM),DIME ,CNOEUD)
          ENDIF
C
C --- COORDONNEES BARYCENTRIQUES DE LA MAILLE
C
          P0 = DIME
          DO 70 K = 1, NSOM
            DO 71 L = 1, DIME
              P0 = P0 + 1
              CNOEUD(L) = CNOEUD(L) + CNOEUD(P0)
 71         CONTINUE
 70       CONTINUE
C 
          DO 80 K = 1, DIME
            CNOEUD(K) = CNOEUD(K) / NSOM
 80       CONTINUE
C            
C --- TEST SUR BOITE MINMAX
C
          P0 = B2 + DD1*(M1-1)
C
          DO 90 K = 1, DIME
            R = CNOEUD(K)
            IF ((R.LT.ZR(P0)).OR.(R.GT.ZR(P0+1))) GOTO 140
            P0 = P0 + 2
 90       CONTINUE
C
C --- TEST SUR LES PANS DE LA BOITE
C
          P0 = ZI(B1+2*M1)
          N = ZI(B1+2*(M1+1)) - P0
          P0 = B3 + DD2*(P0-1)
C
          DO 100 K = 1, N
            R = DDOT(DIME,ZR(P0),1,CNOEUD,1) + ZR(P0+DIME)
            IF (R.GT.0.D0) GOTO 140
            P0 = P0 + DD2
 100      CONTINUE
C
C --- STOCKAGE
C
          ZL(E1)      = .FALSE.
          ZI(E3)      = 1
          ZI(E3+1)    = 0
          ZI(E2-1+M1) = E3
          E3          = E3 + 2
          NVIS        = 1

        ENDIF

C ----- INITIALISATION PARCOURS DU VOISINAGE

        IF ((NM2.EQ.1).OR.(NVIS.EQ.0)) GOTO 140

        K = 1
        L = 2
        P0 = E0(1)
        P1 = ZI(E2-1+M1)
        NVOIS(1) = NVIS

        DO 110 I = 1, NVIS
          ZI(P0-1+I) = ZI(P1)
          P1 = ZI(P1+1)
 110    CONTINUE
C
C --- PARCOURS DU VOISINAGE
C
 120    CONTINUE

        NVOIS(L) = 0
        NMA = NVOIS(K)
        P0 = E0(K)

        DO 130 I = 1, NMA

          J = ZI(P0)
          P0 = P0 + 1

          P1 = ZI(C7-1+J)
          N = ZI(C7+J) - P1
          P1 = C6 - 1 + P1

          DO 130 J = 1, N

            M2 = ZI(P1)
            P1 = P1 + 1

            IF (ZL(E1-1+M2)) THEN

              ZL(E1-1+M2) = .FALSE.

              LINTER = MINTER(DIME,M1,M2,ZI(B1),ZI(C1),
     &                        ZR(B2),ZR(C2),ZR(B3),ZR(C3))

              IF (LINTER) THEN

                ZI(E0(L)+NVOIS(L)) = M2
                NVOIS(L) = NVOIS(L) + 1
                ZI(E3) = M2
                ZI(E3+1) = ZI(E2-1+M1)
                ZI(E2-1+M1) = E3
                E3 = E3 + 2
                NVIS = NVIS + 1

                NVIS0 = NVIS0 - 1
                IF (NVIS0.LT.0) CALL U2MESS('F','ARLEQUIN_13')

              ENDIF

            ENDIF

 130    CONTINUE

        K = 3 - K
        L = 3 - L

        IF (NVOIS(K).NE.0) GOTO 120

 140    CONTINUE

        IF (NVIS.NE.0) THEN
          ZI(E4-1+M1) = NVIS
          NAPP = NAPP + NVIS
          NAPT = NAPT + NVIS
          GOTO 10
        ENDIF

        ZL(JCOLM+M1-1) = .FALSE.
        NBMAC = NBMAC - 1

 150    CONTINUE

        ZI(E4-1+M1) = 1
        NAPT = NAPT + 1

 10   CONTINUE
C
      IF (NBMAC.EQ.0) THEN
        CALL U2MESS('F','ARLEQUIN_12')
      ENDIF  
C
C --- COPIE LISTE -> SD. APPARIEMENT
C
      CALL JEECRA(NOMAPP,'LONT',NAPT,' ')
      DO 160 M1 = 1, NM1
        CALL JECROC(JEXNUM(NOMAPP,M1))
        CALL JEECRA(JEXNUM(NOMAPP,M1),'LONMAX',ZI(E4-1+M1),' ')
        IF (.NOT.ZL(JCOLM+M1-1)) GOTO 160
        CALL JEVEUO(JEXNUM(NOMAPP,M1),'E',P0)
        P1 = ZI(E2-1+M1)
 170    CONTINUE
        ZI(P0) = ZI(P1)
        P0 = P0 + 1
        P1 = ZI(P1+1)
        IF (P1.NE.0) GOTO 170
 160  CONTINUE
C
C --- DESALLOCATIONS
C
      CALL JEDETR('&&APPARI.VOISIN1')
      CALL JEDETR('&&APPARI.VOISIN2')
      CALL JEDETR('&&APPARI.FILTRE')
      CALL JEDETR('&&APPARI.LISTE.ENTETE')
      CALL JEDETR('&&APPARI.LISTE.RESERVE')
      CALL JEDETR('&&APPARI.NVISAVIS')     
      CALL JEDEMA()

      END

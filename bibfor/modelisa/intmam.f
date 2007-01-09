      SUBROUTINE INTMAM(DIME  ,NOMARL,
     &                  TYPEM1,COORD1,NBNO1 ,H1,
     &                  TYPEM2,COORD2,NBNO2 ,H2,
     &                  TRAVR ,TRAVI ,TRAVL ,NT)
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 09/01/2007   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      INTEGER      DIME
      CHARACTER*8  NOMARL
      INTEGER      TRAVI(*)
      LOGICAL      TRAVL(*)
      REAL*8       TRAVR(*)
      CHARACTER*8  TYPEM1,TYPEM2      
      INTEGER      NBNO1,NBNO2
      REAL*8       COORD1(*),COORD2(*)
      REAL*8       H1,H2
      INTEGER      NT
C      
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C   
C INTERSECTION MAILLE / MAILLE
C      
C ----------------------------------------------------------------------
C
C
C BUG
C LE PASSAGE EN ARGUMENT DE NHINT EST BIZARRE
C DEVRAIT ETRE NHAPP D'APRES CREATION SD (VOIR ARLCAR)
C ON S'EN SORT CAR MAX(NHINT,NHAPP) ET NHAPP>NHINT
C PAS DE DEBORDEMENT MEMOIRE MAIS...
C
C IN  DIME   : DIMENSION DE L'ESPACE
C IN  NOMARL : NOM DE LA SD PRINCIPALE ARLEQUIN
C IN  TYPEM1 : TYPE DE LA PREMIERE MAILLE
C IN  COORD1 : COORDONNEES DES NOEUDS DE LA PREMIERE MAILLE
C IN  NBNO1  : NOMBRE DE NOEUDS DE LA PREMIERE MAILLE
C IN  H1     : DIAMETRE DE LA PREMIERE MAILLE
C IN  TYPEM2 : TYPE DE LA SECONDE MAILLE
C IN  COORD2 : COORDONNEES DES NOEUDS DE LA SECONDE MAILLE
C IN  NBNO2  : NOMBRE DE NOEUDS DE LA SECONDE MAILLE
C IN  H2     : DIAMETRE DE LA SECONDE MAILLE
C I/O TRAVR  : VECTEURS DE TRAVAIL DE REELS 
C                DIME : 12 + 276*NHINT**2
C I/O TRAVI  : VECTEURS DE TRAVAIL D'ENTIERS
C                DIME : 56 + 1536*NHINT**2
C IN  TRAVL  : VECTEURS DE TRAVAIL DE BOOLEENS
C                DIME : 24*NHINT**2
C OUT NT     : NB TRIANGLES / TETRAEDRES PAVANT INTERSECTION
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
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
      INTEGER     NHINT,NCMAX,ITEMCP
      INTEGER     ARLGEI
      REAL*8      ARLGER
      REAL*8      PRECCP,PRECTR,PRECIT,PRECVM
      REAL*8      DDOT,PLVOL2,PLVOL3
      LOGICAL     DEDANS
      INTEGER     NBARE,NBPAN,NBSOM
C
C --- GENERATEUR PSEUDO-ALEATOIRE
C
      REAL*8      GRAMAX
      INTEGER     GRAIN0,GRAIN1,GRAINS(32)
      PARAMETER  (GRAMAX = 1073741823.D0)
      INTEGER     ARE1(48),ARE2(48)
      INTEGER     PAN1(60),PAN2(60)
      INTEGER     NSOM,NSOM1,NSOM2
      INTEGER     NARE,NARE1,NARE2
      INTEGER     NPAN1,NPAN2
      INTEGER     NFACE,NFACE1,NFACE2
      INTEGER     NC,PEQ,PZR,PFS,PAS,PAF,S1,S2,S3,I,P,Q
      INTEGER     JNSC
      REAL*8      NO1(81),NO2(81)
      REAL*8      V1,V2,VC,VOLMIN,PREC,G(3)
      LOGICAL     IR
C
C ----------------------------------------------------------------------
C
C
C --- PARAMETRES
C 
      PRECCP = ARLGER(NOMARL,'PRECCP')
      PRECIT = ARLGER(NOMARL,'PRECIT')
      PRECVM = ARLGER(NOMARL,'PRECVM')
      ITEMCP = ARLGEI(NOMARL,'ITEMCP')/10
      NHINT  = ARLGEI(NOMARL,'NHINT ')
      NCMAX  = ARLGEI(NOMARL,'NCMAX ')
      PRECTR = ARLGER(NOMARL,'PRECTR')          
C
C --- INITIALISATIONS
C
      IF (DIME.EQ.3) NCMAX = 2*NCMAX
      NT  = 0
      NC  = NHINT*NHINT
      PEQ = 13 + 144*NC
      PZR = 13 + 240*NC
      PFS = 33 + 888*NC
      PAS = 57 + 1176*NC
      PAF = 57 + 1356*NC  
      CALL WKVECT('&&INTMAM.NSC','V V I',NCMAX,JNSC)
      GRAIN0 = 0     
C      
C --- RECOPIE DES COORDONNES
C
      CALL DCOPY(DIME*NBNO1,COORD1,1,NO1,1)
      CALL DCOPY(DIME*NBNO2,COORD2,1,NO2,1)     
C      
C --- ORIENTATION DES MAILLES
C
      CALL ORIEM2(TYPEM1,NO1)
      CALL ORIEM2(TYPEM2,NO2)
C
C --- BRUITAGE DES SOMMETS POUR EVITER CAS DE COINCIDENCE PARFAITE
C
      NBNO1 = NBSOM(TYPEM1)
      PREC  = PRECIT*H1
C
      DO 10 I = 1, NBNO1*DIME
        CALL HASARD(GRAIN0,GRAIN1,GRAINS,32)        
        NO1(I) =  NO1(I) + PREC*(((GRAIN1-1)/GRAMAX)-1.D0)
 10   CONTINUE
C
      NBNO2 = NBSOM(TYPEM2)
      PREC  = PRECIT*H2
C
      DO 20 I = 1, NBNO2*DIME
        CALL HASARD(GRAIN0,GRAIN1,GRAINS,32)        
        NO2(I) =  NO2(I) + PREC*(((GRAIN1-1)/GRAMAX)-1.D0)
 20   CONTINUE
C
C --- CARACTERISTIQUES DES MAILLES
C
      CALL NOARE(TYPEM1,ARE1)
      NARE1 = NBARE(TYPEM1)
      CALL NOARE(TYPEM2,ARE2)
      NARE2 = NBARE(TYPEM2)
C      
      IF (DIME.EQ.3) THEN
        CALL NOPAN(TYPEM1,PAN1)
        NPAN1 = NBPAN(TYPEM1)
        CALL NOPAN(TYPEM2,PAN2)
        NPAN2 = NBPAN(TYPEM2)
      ENDIF       
C
C --- ECHANTILLONNAGE DE LA FRONTIERE
C
C --- QUESTION: POURQUOI NHINT ALORS QUE ECHANTILLONAGE FRONTIERE
C     (DONC NHAPP) ?????????
C
      CALL ECHMAP(DIME  ,NO1   ,NBNO1 ,ARE1  ,NARE1,
     &            PAN1  ,NPAN1 ,NHINT ,TRAVR ,NSOM1)
      CALL ECHMAP(DIME  ,NO2   ,NBNO2 ,ARE2  ,NARE2,
     &            PAN2  ,NPAN2 ,NHINT ,TRAVR(1+DIME*NSOM1),NSOM2)

      NSOM = NSOM1 + NSOM2
C
C --- CONNECTIVITE DE L'ECHANTILLONNAGE
C
      IF (DIME.EQ.2) THEN
        CALL ECHMC2(NBNO1 ,ARE1   ,NARE1  ,NHINT ,0      ,
     &              TRAVI(PFS)         ,NFACE1) 
        CALL ECHMC2(NBNO2 ,ARE2   ,NARE2  ,NHINT ,NSOM1  ,
     &              TRAVI(PFS+2*NFACE1),NFACE2)
      ELSEIF (DIME.EQ.3) THEN
        CALL ARLPAN(TYPEM1,ARE1,NARE1,NPAN1)
        CALL ARLPAN(TYPEM2,ARE2,NARE2,NPAN2)
      
        CALL ECHMC3(NBNO1 ,ARE1   ,NARE1  ,PAN1  ,NPAN1  ,
     &              NHINT ,0      ,TRAVI(PFS)    ,NFACE1)
        CALL ECHMC3(NBNO2 ,ARE2   ,NARE2  ,PAN2  ,NPAN2  ,
     &              NHINT ,NSOM1  ,TRAVI(PFS+3*NFACE1),NFACE2)
     
        CALL ARETE3(TRAVI(PFS)    ,0      ,NFACE1 ,
     &              TRAVI(PAS)    ,TRAVI(PAF),NARE1)
        CALL ARETE3(TRAVI(PFS)    ,NFACE1 ,NFACE2 ,
     &              TRAVI(PAS+2*NARE1),TRAVI(PAF+2*NARE1),NARE2)
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      NFACE = NFACE1 + NFACE2
C
C --- VOLUME DES DEUX POLYEDRES
C
      IF (DIME.EQ.2) THEN
        P = PAS
        Q = PFS
        TRAVI(P) = NSOM1
        DO 30 I = 1, NSOM1
          P = P + 1
          TRAVI(P) = TRAVI(Q)
          Q = Q + 2
 30     CONTINUE
        P = P + 1 
        TRAVI(P) = NSOM2
        DO 40 I = 1, NSOM2
          P = P + 1
          TRAVI(P) = TRAVI(Q)
          Q = Q + 2
 40     CONTINUE
        V1 = PLVOL2(DIME  ,TRAVR,TRAVR,TRAVI(PAS+1)      ,NSOM1)
        V2 = PLVOL2(DIME  ,TRAVR,TRAVR,TRAVI(PAS+NSOM1+2),NSOM2)
      ELSE
        V1 = PLVOL3(TRAVR,TRAVI(PFS)         ,NFACE1)
        V2 = PLVOL3(TRAVR,TRAVI(PFS+3*NFACE1),NFACE2)
      ENDIF
C
      VOLMIN = PRECVM*MIN(V1,V2)     
C
C --- EQUATIONS DE DROITES / PLANS
C
      IF (DIME.EQ.2) THEN
        DO 50 I = 1, NFACE
          P  = PFS + 2*I
          S1 = 2*TRAVI(P-2)
          S2 = 2*TRAVI(P-1)
          P  = PEQ + 3*I 
          TRAVR(P-3) = TRAVR(S2) - TRAVR(S1)
          TRAVR(P-2) = TRAVR(S1-1) - TRAVR(S2-1)
          TRAVR(P-1) = TRAVR(S2-1)*TRAVR(S1) - TRAVR(S1-1)*TRAVR(S2)
 50     CONTINUE
      ELSE
        DO 60 I = 1, NFACE
          P = PFS + 3*I
          S1 = 3*TRAVI(P-3)-2
          S2 = 3*TRAVI(P-2)-2
          S3 = 3*TRAVI(P-1)-2
          P = PEQ + 4*I - 4
          CALL PROVE3(TRAVR(S1),TRAVR(S2),TRAVR(S3),TRAVR(P))
          TRAVR(P+3) = -DDOT(3,TRAVR(P),1,TRAVR(S1),1)
 60     CONTINUE
      ENDIF
C
C --- INTERSECTION
C      
      IF (DIME.EQ.2) THEN
        CALL PLINT2(TRAVR ,NSOM  ,TRAVI(PFS),TRAVR(PEQ),NCMAX ,
     &              NFACE1,NFACE2,TRAVR(PZR),TRAVI(PAS),TRAVL ,
     &              NC)
      ELSE
        CALL PLINT3(TRAVR ,NSOM  ,TRAVI(PFS),TRAVR(PEQ),NCMAX,
     &              PRECTR,NFACE1,NFACE2,TRAVI(PAS),TRAVI(PAF),
     &              NARE1 ,NARE2 ,TRAVR(PZR),TRAVI,TRAVL,
     &              NC)   
        P = 0
        DO 70 I = 1, NC
          ZI(JNSC+I-1) = TRAVI(I)
          P            = P + TRAVI(I)
 70     CONTINUE
      ENDIF
C      
C --- TROP DE COMPOSANTES CONNEXES
C
      IF (NC.GT.NCMAX) THEN
        CALL U2MESS('A','ARLEQUIN_24')  
      ENDIF      
C
C --- CAS DE L'INCLUSION
C
      IF (NC.EQ.0) THEN
        IF (V1.LT.V2) THEN
          PREC = H1*PRECCP
          CALL PLCENT(DIME,TRAVR,TRAVI(PFS),NFACE1,G)
          CALL REFERE(G,NO2,DIME,TYPEM2,PREC,ITEMCP,.FALSE.,
     &                G,IR,TRAVR)
          IF (IR.AND.DEDANS(G,TYPEM2)) THEN
            NC = 1
            IF (DIME.EQ.2) THEN
              P = PAS
              Q = PFS
              TRAVI(P) = NSOM1
              DO 80 I = 1, NSOM1
                P = P + 1
                TRAVI(P) = TRAVI(Q)
                Q = Q + 2
 80           CONTINUE
            ELSE
              ZI(JNSC+1-1) = NFACE1
            ENDIF
          ENDIF
        ELSE
          PREC = H2*PRECCP
          CALL PLCENT(DIME,TRAVR,TRAVI(PFS+DIME*NFACE1),NFACE2,G)
          CALL REFERE(G,NO1,DIME,TYPEM1,PREC,ITEMCP,.FALSE.,
     &                G,IR,TRAVR)  
          IF (IR.AND.DEDANS(G,TYPEM1)) THEN
            NC = 1
            IF (DIME.EQ.2) THEN
              P = PAS
              Q = PFS+2*NFACE1
              TRAVI(P) = NSOM2
              DO 90 I = 1, NSOM2
                P = P + 1
                TRAVI(P) = TRAVI(Q)
                Q = Q + 2
 90           CONTINUE
            ELSE
              ZI(JNSC+1-1) = NFACE2
              PFS = PFS+3*NFACE1
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
C --- TRIANGULATION / TETRAEDRISATION
C
      P = 1
      IF (DIME.EQ.2) THEN
        DO 100 I = 1, NC
          NSOM = TRAVI(PAS)
          VC   = PLVOL2(2,TRAVR,TRAVR,TRAVI(PAS+1),NSOM)
          IF (VC.GT.VOLMIN) THEN
            CALL PLTRI2(2,TRAVR,TRAVR,TRAVI(PAS+1),NSOM,
     &                  PRECTR,TRAVI(P),NFACE)
            NT = NT + NFACE
            P = P + 3*NFACE
          ENDIF
          PAS = PAS + NSOM + 1
 100    CONTINUE
      ELSE
        DO 110 I = 1, NC
          NFACE = ZI(JNSC+1-1)
          VC    = PLVOL3(TRAVR,TRAVI(PFS),NFACE)
          IF (VC.GT.VOLMIN) THEN
            CALL PLTRI3(TRAVR,NSOM,TRAVI(PFS),NFACE,VOLMIN,
     &                  TRAVL,TRAVI(P),NARE)
            NT = NT + NARE
            P = P + 4*NARE
          ENDIF
          PFS = PFS + 3*NFACE
 110    CONTINUE
      ENDIF
C
      CALL JEDETR('&&INTMAM.NSC')
C
      END

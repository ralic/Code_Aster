      FUNCTION INTMAD(DIME  ,NOMARL,
     &                TYPEM1,NO1   ,NBNO1 ,CPAN1  ,
     &                NO2   ,NBNO2 ,ARE2  ,NARE2  ,PAN2,
     &                FA2   ,NPAN2 ,TRAVR ,TRAVI  ,TRAVL)
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
      REAL*8       INTMAD
      INTEGER      DIME
      CHARACTER*8  NOMARL
      CHARACTER*8  TYPEM1     
      INTEGER      NBNO1
      REAL*8       NO1(DIME,*) 
      REAL*8       CPAN1(DIME+2,*)
      INTEGER      NBNO2
      REAL*8       NO2(DIME,*)           
      INTEGER      ARE2(*),PAN2(*),FA2(*) 
      INTEGER      NARE2,NPAN2
      INTEGER      TRAVI(*)
      LOGICAL      TRAVL(*)
      REAL*8       TRAVR(*) 
C      
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C   
C RATIO VOLUME (MAILLE INTER DOMAINE) / VOLUME (MAILLE)
C      
C ----------------------------------------------------------------------
C
C
C BUG      
C LE STATUT DE FA2 DANS APPEL A ECHMC3 N'EST PAS CLAIR VIS-A-VIS
C DE INTMAM - A CREUSER         
C
C
C IN  DIME   : DIMENSION DE L'ESPACE
C IN  NOMARL : NOM DE LA SD PRINCIPALE ARLEQUIN
C
C IN  TYPEM1 : TYPE DE LA PREMIERE MAILLE
C IN  COORD1 : COORDONNEES DES NOEUDS DE LA PREMIERE MAILLE
C IN  NBNO1  : NOMBRE DE NOEUDS DE LA PREMIERE MAILLE
C IN  CPAN1  : PANS DE LA PREMIERE MAILLE (CF. BOITE)
C 
C IN  COORD2 : COORDONNEES DES NOEUDS DE LA FRONTIERE DU DOMAINE
C IN  NBNO2  : NOMBRE DE NOEUDS DE LA FRONTIERE DU DOMAINE
C IN  ARE2   : CONNECTIVITE ARETES DE FRONTIERE (CF. ARFACE)
C IN  NARE2  : NOMBRE D'ARETES DE LA FRONTIERE
C IN  PAN2   : CONNECTIVITE FACES DE FRONTIERE (CF. NAFINT)
C IN  FA2    : GRAPHE FACES -> ARETES (CF. ARFACE)
C IN  NPAN2  : NOMBRE DE FACES DE LA FRONTIERE
C
C I/O TRAVR  : VECTEURS DE TRAVAIL DE REELS 
C                DIME : 35*NBNO2*NHINT**2 + 66*NHINT**2 + 18*(NHINT+1)
C I/O TRAVI  : VECTEURS DE TRAVAIL D'ENTIERS
C                DIME : (NBNO2+6)*(50*NHINT**2+22*NHINT+22) + 
C                         162*NBNO2*NHINT**2
C IN  TRAVL  : VECTEURS DE TRAVAIL DE BOOLEENS
C                DIME : 2*(NBNO2+6)*NHINT**2
C
C ----------------------------------------------------------------------
C
      REAL*8      PRECVV,PRECTR,ARLGER
      INTEGER     NHINT,NCMAX,ARLGEI
      INTEGER     NBSOM,NBARE,NBPAN
      REAL*8      PLVOL2,PLVOL3,DDOT
      INTEGER     NARE1,NPAN1,NSOM,NSOM1,NSOM2
      INTEGER     NFACE,NFACE1,NFACE2
      INTEGER     N,NC,NS,NF
      INTEGER     ARE1(48),PAN1(60)     
      INTEGER     S1,S2,S3,PAS,PFS,PAF,PEQ,PZR,P,Q,I,J,K
      REAL*8      R,V0,VI,PREC,DIMH
C
C ----------------------------------------------------------------------
C
C
C --- PARAMETRES
C
      NHINT  = ARLGEI(NOMARL,'NHINT ')
      PRECVV = ARLGER(NOMARL,'PRECVV')
      NCMAX  = ARLGEI(NOMARL,'NCMAX ')      
C
C --- INITIALISATIONS
C
      IF (DIME.EQ.3) NCMAX = 2*NCMAX
      N = NHINT**2
      P = NHINT + 1
      Q = NBNO2 + 6
      PFS = Q*(26*N+12*P) + 96*NBNO2*N
      PAS = Q*(38*N+18*P) + 114*NBNO2*N
      PAF = Q*(44*N+20*P) + 138*NBNO2*N
      PEQ = 21*NBNO2*N + 18*(N+P)
      PZR = 29*NBNO2*N + 66*N + 18*P
C
C --- REORIENTATION DE MA1
C
      CALL ORIEM2(TYPEM1,NO1)
C
C --- CARACTERISTIQUES MA1
C 
      NBNO1 = NBSOM(TYPEM1)
      CALL NOARE(TYPEM1,ARE1)
      NARE1 = NBARE(TYPEM1)
      CALL NOPAN(TYPEM1,PAN1)
      NPAN1 = NBPAN(TYPEM1)
C      
C --- ECHANTILLONNAGE PONCTUEL
C
      P = 1
      DO 10 I = 1, NPAN1
        NS   = PAN1(P)
        DIMH = 0.D0
        DO 20 J = 1, DIME
          R    = CPAN1(J,I)
          DIMH = DIMH + R*R
 20     CONTINUE
        PREC = PRECVV/DIMH
        DO 30 J = 1, DIME
          R = PREC*CPAN1(J,I)
          DO 31 K = 1, ABS(NS)
            N = PAN1(P+K)
            NO1(J,N) = NO1(J,N) - R
 31       CONTINUE            
 30     CONTINUE
        P = P + ABS(NS) + 1
 10   CONTINUE
C
      CALL ECHMAP(DIME  ,NO1   ,NBNO1 ,ARE1  ,NARE1,
     &            PAN1  ,NPAN1 ,NHINT ,TRAVR ,NSOM1)
      CALL ECHMAP(DIME  ,NO2   ,NBNO2 ,ARE2  ,NARE2,
     &            PAN2  ,NPAN2 ,NHINT ,TRAVR(1+DIME*NSOM1),NSOM2)
C
      NSOM = NSOM1 + NSOM2
C
C --- CONNECTIVITE DE L'ECHANTILLONNAGE
C
      IF (DIME.EQ.2) THEN
        CALL ECHMC2(NBNO1 ,ARE1  ,NARE1 ,NHINT ,0     ,
     &              TRAVI(PFS),NFACE1)
        CALL ECHMC2(NBNO2 ,ARE2  ,NARE2 ,NHINT ,NSOM1 ,
     &              TRAVI(PFS+2*NFACE1),NFACE2)
      ELSE
        CALL ARLPAN(TYPEM1,ARE1,NARE1,NPAN1)      
      
        CALL ECHMC3(NBNO1 ,ARE1  ,NARE1  ,PAN1  ,NPAN1  ,
     &              NHINT ,0     ,TRAVI(PFS)    ,NFACE1)
        CALL ECHMC3(NBNO2 ,FA2   ,NARE2  ,PAN2  ,NPAN2  ,
     &              NHINT ,NSOM1 ,TRAVI(PFS+3*NFACE1),NFACE2)
        CALL ARETE3(TRAVI(PFS)   ,0      ,NFACE1,
     &              TRAVI(PAS)   ,TRAVI(PAF),NARE1)
        CALL ARETE3(TRAVI(PFS)   ,NFACE1 ,NFACE2,
     &              TRAVI(PAS+2*NARE1),TRAVI(PAF+2*NARE1),NARE2)
      ENDIF
C
      NFACE = NFACE1 + NFACE2
C
C --- EQUATIONS DE DROITES / PLANS
C
      IF (DIME.EQ.2) THEN
        DO 40 I = 1, NFACE
          P  = PFS + 2*I
          S1 = 2*TRAVI(P-2)
          S2 = 2*TRAVI(P-1)
          P  = PEQ + 3*I 
          TRAVR(P-3) = TRAVR(S2) - TRAVR(S1)
          TRAVR(P-2) = TRAVR(S1-1) - TRAVR(S2-1)
          TRAVR(P-1) = TRAVR(S2-1)*TRAVR(S1) - TRAVR(S1-1)*TRAVR(S2)
 40     CONTINUE
      ELSE
        DO 50 I = 1, NFACE
          P  = PFS + 3*I
          S1 = 3*TRAVI(P-3)-2
          S2 = 3*TRAVI(P-2)-2
          S3 = 3*TRAVI(P-1)-2
          P  = PEQ + 4*I - 4
          CALL PROVE3(TRAVR(S1),TRAVR(S2),TRAVR(S3),TRAVR(P))
          TRAVR(P+3) = -DDOT(3,TRAVR(P),1,TRAVR(S1),1)
 50     CONTINUE
      ENDIF
C
C --- VOLUME DU POLYEDRE MA1
C
      IF (DIME.EQ.2) THEN
        P = 1
        Q = PFS
        TRAVI(P) = NSOM1
        DO 60 I = 1, NSOM1
          P = P + 1
          TRAVI(P) = TRAVI(Q)
          Q = Q + 2
 60     CONTINUE
        V0 = PLVOL2(2,TRAVR,TRAVR,TRAVI,NSOM1)      
      ELSE
        V0 = PLVOL3(TRAVR,TRAVI(PFS),NFACE1)
      ENDIF
C
C --- INTERSECTION DE LA MAILLE AVEC LE DOMAINE 
C
      IF (DIME.EQ.2) THEN
        CALL PLINT2(TRAVR ,NSOM  ,TRAVI(PFS),TRAVR(PEQ),NCMAX ,
     &              NFACE1,NFACE2,TRAVR(PZR),TRAVI(PAS),TRAVL ,
     &              NC)
      ELSE     
        CALL PLINT3(TRAVR ,NSOM  ,TRAVI(PFS),TRAVR(PEQ),NCMAX,
     &              PRECTR,NFACE1,NFACE2,TRAVI(PAS),TRAVI(PAF),
     &              NARE1,NARE2,TRAVR(PZR),TRAVI,TRAVL,
     &              NC)
        P = 0
        DO 70 I = 1, NC
          PAN2(I) = TRAVI(I)
          P       = P + TRAVI(I)
 70     CONTINUE

      ENDIF
C      
C --- TROP DE COMPOSANTES CONNEXES
C
      IF (NC.GT.NCMAX) THEN
        
        CALL U2MESS('A','ARLEQUIN_24')  
      ENDIF      
C
C --- PAS D'INTERSECTION
C
      IF (NC.EQ.0) THEN
        INTMAD = 1.D0
        GOTO 100
      ENDIF
C
C --- VOLUME DE L'INTERSECTION
C      
      VI = 0.D0
      IF (DIME.EQ.2) THEN
        DO 80 I = 1, NC
          NS  = TRAVI(PAS)
          VI  = VI + PLVOL2(2,TRAVR,TRAVR,TRAVI(PAS+1),NS)
          PAS = PAS + NS + 1
 80     CONTINUE
      ELSE
        DO 90 I = 1, NC
          NF  = PAN2(I)
          VI  = VI + PLVOL3(TRAVR,TRAVI(PFS),NF)
          PFS = PFS + 3*NF
 90     CONTINUE
      ENDIF
C
C --- RATIO
C
      INTMAD = VI / V0

 100  CONTINUE
      
      END

      FUNCTION INTMAD(DIM,NH,TM1,NO1,N1,PAN1,NO2,N2,
     &                AS2,NA2,FS2,FA2,NP2,ZR,ZI,ZL)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 08/11/2004   AUTEUR DURAND C.DURAND 
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
C ----------------------------------------------------------------------
C         RATIO VOLUME (MAILLE INTER DOMAINE) / VOLUME (MAILLE)
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C INTEGER      DIM     : DIMENSION DE L'ESPACE
C INTEGER      NH      : NOMBRE D'ECHANTILLONNAGE
C CHARACTER*8  TM1     : TYPE DE LA PREMIERE MAILLE
C REAL*8       NO1(*)  : COORDONNEES DES NOEUDS DE LA MAILLE
C INTEGER      N1      : NOMBRE DE NOEUDS DE LA MAILLE
C REAL*8       PAN1(*) : PANS DE LA PREMIERE MAILLE (CF. BOITE) 
C REAL*8       NO2(*)  : COORDONNEES NOEUDS DE LA FRONTIERE DU DOMAINE
C INTEGER      N2      : NOMBRE DE NOEUDS DE LA FRONTIERE
C INTEGER      AS2(*)  : CONNECTIVITE ARETES DE FRONTIERE (CF. ARFACE)
C INTEGER      NA2     : NOMBRE D'ARETES DE LA FRONTIERE
C INTEGER      FS2(*)  : CONNECTIVITE FACES DE FRONTIERE (CF. NAFINT)
C INTEGER      FA2(*)  : GRAPHE FACES -> ARETES (CF. ARFACE)
C INTEGER      NP2     : NOMBRE DE FACES DE LA FRONTIERE
C
C VECTEURS DE TRAVAIL
C REAL*8       ZR(*)   : 35*N2*NH**2 + 66*NH**2 + 18*(NH+1)
C INTEGER      ZI(*)   : (N2+6)*(50*NH**2+22*NH+22) + 162*N2*NH**2
C LOGICAL      ZL(*)   : 2*(N2+6)*NH**2
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- PARAMETRE (CF NAFINT)
      REAL*8      PREC0
      PARAMETER   (PREC0=5.D-3)

C --- FONCTION
      INTEGER     NSOMMT
      REAL*8      INTMAD,PLVOL2,PLVOL3,R8DOT

C --- VARIABLES
      CHARACTER*8 TM1
      INTEGER     DIM,NH,N1,N2,NA1,NA2,NP1,NP2,NS,NS1,NS2,NF,NF1,NF2,NC
      INTEGER     ZI(*),AR1(48),AS2(*),FS1(60),FS2(*),FA2(*)
      INTEGER     S1,S2,S3,PAS,PFS,PAF,PEQ,PZR,P,Q,I,J,K,N
      REAL*8      NO1(DIM,*),NO2(DIM,*),PAN1(DIM+2,*),ZR(*),R,V0,VI,PREC
      LOGICAL     ZL(*)

      N = NH**2
      P = NH + 1
      Q = N2 + 6

      PFS = Q*(26*N+12*P) + 96*N2*N
      PAS = Q*(38*N+18*P) + 114*N2*N
      PAF = Q*(44*N+20*P) + 138*N2*N

      PEQ = 21*N2*N + 18*(N+P)
      PZR = 29*N2*N + 66*N + 18*P

C --- ORIENTATION DE MA1

      CALL ORIEM2(TM1,NO1)

C --- ECHANTILLONNAGE PONCTUEL

      N1 = NSOMMT(TM1)
      CALL NOARET(TM1,AR1,NA1)
      CALL NOPAN(TM1,FS1,NP1)

      P = 1

      DO 10 I = 1, NP1

        NS = FS1(P)
        PREC = 0.D0

        DO 20 J = 1, DIM
          R = PAN1(J,I)
          PREC = PREC + R*R
 20     CONTINUE

        PREC = PREC0/PREC

        DO 30 J = 1, DIM
          R = PREC*PAN1(J,I)
          DO 30 K = 1, ABS(NS)
            N = FS1(P+K)
            NO1(J,N) = NO1(J,N) - R
 30     CONTINUE

        P = P + ABS(NS) + 1

 10   CONTINUE

      CALL ECHMAP(DIM,NO1,N1,AR1,NA1,FS1,NP1,NH,ZR,NS1)
      CALL ECHMAP(DIM,NO2,N2,AS2,NA2,FS2,NP2,NH,ZR(1+DIM*NS1),NS2)

      NS = NS1 + NS2

C --- CONNECTIVITE DE L'ECHANTILLONNAGE

      IF (DIM.EQ.2) THEN

        CALL ECHMC2(N1,AR1,NA1,NH,0,ZI(PFS),NF1)
        CALL ECHMC2(N2,AS2,NA2,NH,NS1,ZI(PFS+2*NF1),NF2)

      ELSE

        CALL ARLPAN(TM1,AR1,NA1,NP1)

        CALL ECHMC3(N1,AR1,NA1,FS1,NP1,NH,0,ZI(PFS),NF1)
        CALL ECHMC3(N2,FA2,NA2,FS2,NP2,NH,NS1,ZI(PFS+3*NF1),NF2)

        CALL ARETE3(ZI(PFS),0,NF1,ZI(PAS),ZI(PAF),NA1)
        CALL ARETE3(ZI(PFS),NF1,NF2,ZI(PAS+2*NA1),ZI(PAF+2*NA1),NA2)

      ENDIF

      NF = NF1 + NF2

C --- EQUATIONS DE DROITES / PLANS

      IF (DIM.EQ.2) THEN

        DO 40 I = 1, NF

          P = PFS + 2*I
          S1 = 2*ZI(P-2)
          S2 = 2*ZI(P-1)

          P = PEQ + 3*I 
          ZR(P-3) = ZR(S2) - ZR(S1)
          ZR(P-2) = ZR(S1-1) - ZR(S2-1)
          ZR(P-1) = ZR(S2-1)*ZR(S1) - ZR(S1-1)*ZR(S2)

 40     CONTINUE

      ELSE

        DO 50 I = 1, NF

          P = PFS + 3*I
          S1 = 3*ZI(P-3)-2
          S2 = 3*ZI(P-2)-2
          S3 = 3*ZI(P-1)-2

          P = PEQ + 4*I - 4
          CALL PROVE3(ZR(S1),ZR(S2),ZR(S3),ZR(P))
          ZR(P+3) = -R8DOT(3,ZR(P),1,ZR(S1),1)

 50     CONTINUE

      ENDIF

C --- VOLUME DE MA1

      IF (DIM.EQ.3) THEN

        V0 = PLVOL3(ZR,ZI(PFS),NF1)

      ELSE

        P = 1
        Q = PFS

        ZI(P) = NS1
        DO 60 I = 1, NS1
          P = P + 1
          ZI(P) = ZI(Q)
          Q = Q + 2
 60     CONTINUE

        V0 = PLVOL2(2,ZR,ZR,ZI,NS1)

      ENDIF

C --- INTERSECTION MA1 / DOMAINE 

      IF (DIM.EQ.2) THEN

        CALL PLINT2(ZR,NS,ZI(PFS),ZR(PEQ),NF1,NF2,ZR(PZR),ZI(PAS),ZL,NC)
 
      ELSE
        
        CALL PLINT3(ZR,NS,ZI(PFS),ZR(PEQ),NF1,NF2,
     &              ZI(PAS),ZI(PAF),NA1,NA2,ZR(PZR),ZI,ZL,NC)
 
        P = 0
        DO 70 I = 1, NC
          FS2(I) = ZI(I)
          P = P + ZI(I)
 70     CONTINUE

      ENDIF

C --- PAS D'INTERSECTION

      IF (NC.EQ.0) THEN

        INTMAD = 1.D0
        GOTO 100

      ENDIF

C --- VOLUME DE L'INTERSECTION
      
      VI = 0.D0

      IF (DIM.EQ.2) THEN

        DO 80 I = 1, NC
          NS = ZI(PAS)
          VI = VI + PLVOL2(2,ZR,ZR,ZI(PAS+1),NS)
          PAS = PAS + NS + 1
 80     CONTINUE

      ELSE

        DO 90 I = 1, NC
          NF = FS2(I)
          VI = VI + PLVOL3(ZR,ZI(PFS),NF)
          PFS = PFS + 3*NF
 90     CONTINUE

      ENDIF

C --- RATIO

      INTMAD = VI / V0

 100  CONTINUE
      
      END

      SUBROUTINE INTMAM(DIM,NH,TM1,N1Z,N1,H1,TM2,N2Z,N2,H2,ZR,ZI,ZL,NT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 16/12/2004   AUTEUR VABHHTS J.PELLET 
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
C                    INTERSECTION MAILLE / MAILLE
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C INTEGER      DIM     : DIMENSION DE L'ESPACE
C INTEGER      NH      : NOMBRE D'ECHANTILLONNAGE
C CHARACTER*8  TM1     : TYPE DE LA PREMIERE MAILLE
C REAL*8       N1Z(*)  : COORDONNEES DES NOEUDS DE LA PREMIERE MAILLE
C INTEGER      N1      : NOMBRE DE NOEUDS DE LA PREMIERE MAILLE
C REAL*8       H1      : DIAMETRE DE LA PREMIERE MAILLE
C CHARACTER*8  TM2     : TYPE DE LA SECONDE MAILLE
C REAL*8       N2Z(*)  : COORDONNEES DES NOEUDS DE LA SECONDE MAILLE
C INTEGER      N2      : NOMBRE DE NOEUDS DE LA SECONDE MAILLE
C REAL*8       H2      : DIAMETRE DE LA SECONDE MAILLE
C
C VECTEURS DE TRAVAIL
C REAL*8       ZR(*)   : 12 + 276*NH**2
C INTEGER      ZI(*)   : 56 + 1536*NH**2
C LOGICAL      ZL(*)   : 24*NH**2
C
C VARIABLES DE SORTIE
C INTEGER      NT      : NB TRIANGLES / TETRAEDRES PAVANT INTERSECTION
C REAL*8       ZR(*)   : SOMMETS DES TRIANGLES / TETRAEDRES
C INTEGER      ZI(*)   : CONNECTIVITE DES TRIANGLES / TETRAEDRES
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- PARAMETRE
      INTEGER NCMAX,IMAX
      REAL*8  PREC0,PREC1,PREC2
      PARAMETER (NCMAX = 4)
      PARAMETER (IMAX = 20)
      PARAMETER (PREC0 = 5.D-6)
      PARAMETER (PREC1 = 1.D-3)
      PARAMETER (PREC2 = 1.D-2)

C --- FONCTION
      REAL*8      DDOT,PLVOL2,PLVOL3
      INTEGER     NSOMMT
      LOGICAL     DEDANS

C --- GENERATEUR PSEUDO-ALEATOIRE
      REAL*8      GRAMAX
      INTEGER     GRAIN0,GRAIN1,GRAINS(32)
      PARAMETER  (GRAMAX = 1073741823.D0)

C --- VARIABLES
      CHARACTER*8 TM1,TM2
      INTEGER     AR1(48),AR2(48),FS1(60),NP1,FS2(60),NP2
      INTEGER     ZI(*),DIM,NH,NS1,NS2,NA,NA1,NA2,NF,NF1,NF2,N1,N2
      INTEGER     NC,NS,NT,PEQ,PZR,PFS,PAS,PAF,S1,S2,S3,I,P,Q,NSC(NCMAX)
      REAL*8      N1Z(*),N2Z(*),NO1(81),NO2(81),ZR(*)
      REAL*8      H1,H2,V1,V2,VC,VM,PREC,G(3)
      LOGICAL     ZL(*),IR

      NT = 0
      NC = NH*NH
      PEQ = 13 + 144*NC
      PZR = 13 + 240*NC
      PFS = 33 + 888*NC
      PAS = 57 + 1176*NC
      PAF = 57 + 1356*NC

      GRAIN0 = 0
      
C --- ORIENTATION DES MAILLES

      CALL DCOPY(DIM*N1,N1Z,1,NO1,1)
      CALL DCOPY(DIM*N2,N2Z,1,NO2,1)

      CALL ORIEM2(TM1,NO1)
      CALL ORIEM2(TM2,NO2)

C --- BRUITAGE DES SOMMETS POUR EVITER CAS DE COINCIDENCE PARFAITE

      N1 = NSOMMT(TM1)
      PREC = PREC0*H1

      DO 10 I = 1, N1*DIM
        CALL HASARD(GRAIN0,GRAIN1,GRAINS,32)        
        NO1(I) =  NO1(I) + PREC*(((GRAIN1-1)/GRAMAX)-1.D0)
 10   CONTINUE

      N2 = NSOMMT(TM2)
      PREC = PREC0*H2

      DO 20 I = 1, N2*DIM
        CALL HASARD(GRAIN0,GRAIN1,GRAINS,32)        
        NO2(I) =  NO2(I) + PREC*(((GRAIN1-1)/GRAMAX)-1.D0)
 20   CONTINUE

C --- ECHANTILLONNAGE PONCTUEL DES MAILLES

      CALL NOARET(TM1,AR1,NA1)
      CALL NOARET(TM2,AR2,NA2)

      IF (DIM.EQ.3) THEN
        CALL NOPAN(TM1,FS1,NP1)
        CALL NOPAN(TM2,FS2,NP2)
      ENDIF

      CALL ECHMAP(DIM,NO1,N1,AR1,NA1,FS1,NP1,NH,ZR,NS1)
      CALL ECHMAP(DIM,NO2,N2,AR2,NA2,FS2,NP2,NH,ZR(1+DIM*NS1),NS2)

      NS = NS1 + NS2

C --- CONNECTIVITE DE L'ECHANTILLONNAGE

      IF (DIM.EQ.2) THEN

        CALL ECHMC2(N1,AR1,NA1,NH,0,ZI(PFS),NF1)
        CALL ECHMC2(N2,AR2,NA2,NH,NS1,ZI(PFS+2*NF1),NF2)

      ELSE

        CALL ARLPAN(TM1,AR1,NA1,NP1)
        CALL ARLPAN(TM2,AR2,NA2,NP2)

        CALL ECHMC3(N1,AR1,NA1,FS1,NP1,NH,0,ZI(PFS),NF1)
        CALL ECHMC3(N2,AR2,NA2,FS2,NP2,NH,NS1,ZI(PFS+3*NF1),NF2)

        CALL ARETE3(ZI(PFS),0,NF1,ZI(PAS),ZI(PAF),NA1)
        CALL ARETE3(ZI(PFS),NF1,NF2,ZI(PAS+2*NA1),ZI(PAF+2*NA1),NA2)

      ENDIF

      NF = NF1 + NF2

C --- VOLUME DES DEUX POLYEDRES

      IF (DIM.EQ.2) THEN

        P = PAS
        Q = PFS

        ZI(P) = NS1
        DO 30 I = 1, NS1
          P = P + 1
          ZI(P) = ZI(Q)
          Q = Q + 2
 30     CONTINUE

        P = P + 1 

        ZI(P) = NS2
        DO 40 I = 1, NS2
          P = P + 1
          ZI(P) = ZI(Q)
          Q = Q + 2
 40     CONTINUE

        V1 = PLVOL2(2,ZR,ZR,ZI(PAS+1),NS1)
        V2 = PLVOL2(2,ZR,ZR,ZI(PAS+NS1+2),NS2)

      ELSE

        V1 = PLVOL3(ZR,ZI(PFS),NF1)
        V2 = PLVOL3(ZR,ZI(PFS+3*NF1),NF2)

      ENDIF

      VM = PREC2*MIN(V1,V2)

C --- EQUATIONS DE DROITES / PLANS

      IF (DIM.EQ.2) THEN

        DO 50 I = 1, NF

          P = PFS + 2*I
          S1 = 2*ZI(P-2)
          S2 = 2*ZI(P-1)

          P = PEQ + 3*I 
          ZR(P-3) = ZR(S2) - ZR(S1)
          ZR(P-2) = ZR(S1-1) - ZR(S2-1)
          ZR(P-1) = ZR(S2-1)*ZR(S1) - ZR(S1-1)*ZR(S2)

 50     CONTINUE

      ELSE

        DO 60 I = 1, NF

          P = PFS + 3*I
          S1 = 3*ZI(P-3)-2
          S2 = 3*ZI(P-2)-2
          S3 = 3*ZI(P-1)-2

          P = PEQ + 4*I - 4
          CALL PROVE3(ZR(S1),ZR(S2),ZR(S3),ZR(P))
          ZR(P+3) = -DDOT(3,ZR(P),1,ZR(S1),1)

 60     CONTINUE

      ENDIF

C --- INTERSECTION
      
      IF (DIM.EQ.2) THEN

        CALL PLINT2(ZR,NS,ZI(PFS),ZR(PEQ),NF1,NF2,ZR(PZR),ZI(PAS),ZL,NC)

      ELSE
        
        CALL PLINT3(ZR,NS,ZI(PFS),ZR(PEQ),NF1,NF2,
     &              ZI(PAS),ZI(PAF),NA1,NA2,ZR(PZR),ZI,ZL,NC)
  
        P = 0
        DO 70 I = 1, NC
          NSC(I) = ZI(I)
          P = P + NSC(I)
 70     CONTINUE

      ENDIF

C --- CAS DE L'INCLUSION

      IF (NC.EQ.0) THEN

C ----- MA1 INCLUSE DANS MA2 ?

        IF (V1.LT.V2) THEN

          PREC = H1*PREC1
          CALL PLCENT(DIM,ZR,ZI(PFS),NF1,G)
          CALL REFERE(G,NO2,DIM,TM2,PREC,IMAX,.FALSE.,G,IR,ZR)

          IF (IR.AND.DEDANS(G,TM2)) THEN

            NC = 1

            IF (DIM.EQ.2) THEN

              P = PAS
              Q = PFS

              ZI(P) = NS1
              DO 80 I = 1, NS1
                P = P + 1
                ZI(P) = ZI(Q)
                Q = Q + 2
 80           CONTINUE

            ELSE

              NSC(1) = NF1

            ENDIF

          ENDIF

C ----- MA2 INCLUSE DANS MA1 ?

        ELSE

          PREC = H2*PREC1
          CALL PLCENT(DIM,ZR,ZI(PFS+DIM*NF1),NF2,G)
          CALL REFERE(G,NO1,DIM,TM1,PREC,IMAX,.FALSE.,G,IR,ZR)  

          IF (IR.AND.DEDANS(G,TM1)) THEN

            NC = 1

            IF (DIM.EQ.2) THEN

              P = PAS
              Q = PFS+2*NF1

              ZI(P) = NS2
              DO 90 I = 1, NS2
                P = P + 1
                ZI(P) = ZI(Q)
                Q = Q + 2
 90           CONTINUE

            ELSE

              NSC(1) = NF2
              PFS = PFS+3*NF1

            ENDIF

          ENDIF

        ENDIF

      ENDIF

C --- TRIANGULATION / TETRAEDRISATION

      P = 1
      IF (DIM.EQ.2) THEN

        DO 100 I = 1, NC

          NS = ZI(PAS)
          VC = PLVOL2(2,ZR,ZR,ZI(PAS+1),NS)

          IF (VC.GT.VM) THEN
            CALL PLTRI2(2,ZR,ZR,ZI(PAS+1),NS,ZI(P),NF)
            NT = NT + NF
            P = P + 3*NF
          ENDIF

          PAS = PAS + NS + 1

 100    CONTINUE

      ELSE

        DO 110 I = 1, NC

          NF = NSC(I)
          VC = PLVOL3(ZR,ZI(PFS),NF)

          IF (VC.GT.VM) THEN
            CALL PLTRI3(ZR,NS,ZI(PFS),NF,VM,ZL,ZI(P),NA)
            NT = NT + NA
            P = P + 4*NA
          ENDIF

          PFS = PFS + 3*NF

 110    CONTINUE

      ENDIF

      END

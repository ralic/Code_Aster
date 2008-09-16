      SUBROUTINE DISELL(P,A,B,H)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 15/09/2008   AUTEUR GENIAUT S.GENIAUT 
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
C RESPONSABLE GENIAUT S.GENIAUT
      IMPLICIT NONE
      REAL*8  P(2),A,B,H
C
C      CALCUL DE H : DISTANCE SIGNEE ENTRE LE POINT P ET L'ELLIPSE
C      DE CENTRE (0,0), DE DEMI-GRAND AXE A ET DE DEMI-PETIT AXE B
C
C IN  P      : POINT DU PLAN DONT ON CHERCHE LA DISTANCE A L'ELLIPSE
C IN  A      : DEMI-GRAND AXE DE L'ELLIPSE (SUIVANT L'AXE X)
C IN  B      : DEMI-PETIT AXE DE L'ELLIPSE (SUIVANT L'AXE Y)
C OUT H      : DISTANCE SIGNEE ENTRE LE POINT P ET L'ELLIPSE
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
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
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER  ITER,NITMX
      REAL*8   EPS,EPSC,BA,R,Z,COSX,SINX,T,A0,A1,A2,A3,A4,K,PHI,QQ,RR,DD
      REAL*8   TT,PHIT,THETA,R8DEPI,DPHI,DR,DZ,RAC,TRIGOM,TEMP
      PARAMETER    (EPS  = 1.D-6)
      PARAMETER    (EPSC = 1.D-6)
      PARAMETER    (NITMX=100)
      LOGICAL  LINSID
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()

C     ON NOTE R ET Z LES COORDONNES DANS LE PLAN POUR ETRE EN 
C     CONFORMITE AVEC LES NOTATIONS DU PAPIER DE RÉFÉRENCE
C     QUE L'ON PEUT TROUVER EN PIECE JOINTE DE LA FICHE 10385

C     CHOIX DU SIGNE DE LA DISTANCE H :
C     H EST NEGATIF A L'INTERIEUR DE L'ELLIPSE
C     H EST POSITIF A L'EXTERIEUR DE L'ELLIPSE
C
C     VERIFICATIONS
      CALL ASSERT(A.GT.0.D0 .AND. B.GT.0.D0)
      IF (A.LT.B) THEN
C       SI A EST PLUS PETIT QUE B, ON INVERSE A ET B 
C       ET AUSSI LES COORDONNÉES DU POINT P
        TEMP = A
        A = B
        B = TEMP
        TEMP = P(1)
        P(1) = P(2)
        P(2) = TEMP
      ENDIF

C     DEFINITION DE QUELQUES VARIABLES UTILES
C     ---------------------------------------

C     ABSCISSE (TOUJOURS POSITIVE) ET ORDONNEE DU POINT P
      R=ABS(P(1))       
      Z=P(2)

C     RAPPORT B/A = (1-F)
      BA=B/A
      CALL ASSERT(BA.LE.1.D0)

C     TRAITEMENT DU CAS PARTICULIER : POINT = CENTRE O
      IF (SQRT(R**2+Z**2).LT.EPS*A) THEN
        H = -B
        GOTO 9999
      ENDIF


C     ITERATION 0 (= PREDICTION)
C     --------------------------

C     TEST SI LE POINT EST A L'INTERIEUR DE L'ELLIPSE
      LINSID = BA**2*(R**2-A**2)+Z**2.LE.0.D0
      COSX   = R/SQRT(R**2+Z**2)
      SINX   = Z/SQRT(R**2+Z**2)
      T      = Z/(R+SQRT(R**2+Z**2))

C     DISTANCE A L'ELLIPSE LE LONG DE LA LIGNE COURRANTE :
C     PLUS PETITE RACINE DU TRINOME A2.K²-2A1.K+A0=0
      A2  = BA**2*COSX**2+SINX**2
      A1  = BA**2*R*COSX+Z*SINX
      A0  = BA**2*(R**2-A**2)+Z**2
      K   = A0/(A1+SQRT(A1**2-A2*A0))
      PHI = ATAN2(Z-K*SINX,BA**2*(R-K*COSX))

C     SI LA PREDICTION EST LA SOLUTION (CAS DU CERCLE PAR EX), ON SORT
      IF (ABS(K).LT.EPS*SQRT(R**2+Z**2)) THEN
        H = K
        GOTO 9999
      ENDIF       


C     BOUCLE PRINCIPALE
C     -----------------

      DO 10 ITER=1,NITMX

C       POLYNOME NORMALISE D'ORDRE 4 DECRIVANT LES INTERSECTIONS
C       ENTRE LE CERCLE ET L'ELLIPSE
C       TAU^4+A3.TAU^3+A2.TAU^2+A1.TAU+A0 = 0
        A4 = BA**2*((R+K)**2-A**2)+Z**2
        A3 = -4.D0*K*Z/A4
        A2 = 2.D0*(BA**2*(R**2-K**2-A**2)+2.D0*K**2+Z**2)/A4
        A1 = A3

C       REDUCTION DU POLYNOME EN DEGRE 3 PAR SUBSTITUTION DE LA
C       RACINE CONNUE (T)
        A3 = A3 + T
        A2 = A2 + T*A3
        A1 = A1 + T*A2

C       RECHERCHE DES AUTRES RACINES REELLES
        QQ = (3.D0*A2-A3**2)/9.D0
        RR = (A3*(9.D0*A2-2.D0*A3**2)-27.D0*A1)/54.D0
        DD = QQ**3+RR**2

        IF (DD.GE.0.D0) THEN

          TT   =  SIGN(1.D0,RR+SQRT(DD))*(ABS(RR+SQRT(DD))**(1.D0/3.D0))
     &           +SIGN(1.D0,RR-SQRT(DD))*(ABS(RR-SQRT(DD))**(1.D0/3.D0))
     &           -A3/3.D0
          PHIT = ATAN2(Z*(1.D0+TT**2)-2.D0*K*TT,BA**2*(R*(1.D0+TT**2)
     &                                                -K*(1.D0-TT**2)))
        ELSE
          QQ    = -QQ
          THETA = TRIGOM('ACOS', RR/(QQ*SQRT(QQ)))
          TT    = 2.D0*SQRT(QQ)*COS(THETA/3.D0)-A3/3.D0
          PHIT  =  ATAN2(Z*(1.D0+TT**2)-2.D0*K*TT,BA**2*(R*(1.D0+TT**2)
     &                                                 -K*(1.D0-TT**2)))
          IF (PHIT*PHI.LT.0.D0) THEN
            TT   = 2.D0*SQRT(QQ)*COS((THETA+R8DEPI())/3.D0)-A3/3.D0
            PHIT = ATAN2(Z*(1.D0+TT**2)-2.D0*K*TT,BA**2*(R*(1.D0+TT**2)
     &                                                 -K*(1.D0-TT**2)))
            IF (PHIT*PHI.LT.0.D0) THEN
              TT   = 2.D0*SQRT(QQ)*COS((THETA+2.D0*R8DEPI())/3.D0)
     &               -A3/3.D0
              PHIT = ATAN2(Z*(1.D0+TT**2)-2.D0*K*TT,BA**2*
     &                        (R*(1.D0+TT**2)-K*(1.D0-TT**2)))
            ENDIF
          ENDIF
        ENDIF  

C       POINT DONT L'ANGLE EST AU MILIEU DE PHI ET PHIT
        DPHI = ABS(PHIT-PHI)/2.D0
        PHI  = (PHI+PHIT)/2.D0

        RAC = SQRT(1.D0-(1.D0-BA)*(1.D0+BA)*(SIN(PHI))**2)

C       SI LA DIFFERENCE D'ANGLE ENTRE DEUX ITERATIONS SUCCESSIVES EST 
C       INFERIEURE A LA PRECISION, ON A CONVERGE ET ON SORT
        IF (DPHI.LT.EPSC) THEN
          H = R*COS(PHI)+Z*SIN(PHI)-A*RAC
          GOTO 9999
        ENDIF

        DR = R-A*COS(PHI)/RAC
        DZ = Z-A*BA**2*SIN(PHI)/RAC
        K = SQRT(DR**2+DZ**2)

        IF (LINSID) K = -K
        T = DZ/(DR+K)

 10   CONTINUE

C     NOMBRE D'ITERATIONS MAXI ATTEINT
      CALL U2MESS('F','XFEM_2') 

C
 9999 CONTINUE

      CALL JEDEMA()
      END

      SUBROUTINE BCOUDC (IGAU,ICOU,ISECT,L,H,A,M,OMEGA,
     &             NPG,XPG,NNO,NCOU,NSECT,FF,RAYON,THETA,MMT,B)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 27/04/2001   AUTEUR JMBHH01 J.M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
      REAL*8             L,H,A,B(4,*),FF(*),RAYON,THETA,OMEGA
      INTEGER       NPG,NNO,NCOU,NSECT,M,IGAU,ICOU,ISECT,MMT

C ......................................................................
C
C   BUT      : CALCUL DE LA MATRICE DE DEFORMATION B POUR LES ELEMENTS
C              TUYAU
C
C   ENTREES : <----
C           IGAU,ICOU,ISECT : LES INDICES DES POINTS D'INTEGRATION
C                RESPECTIVEMENT SUR LA LONGUEUR, DANS L'EPAISSEUR ET
C                SUR LA CIRCONFERENCE
C           NNO,NPG : NBRES DE NOEUDS, NBRES DE POINTS DE GAUSS
C           NCOU,NSECT : NBRES DE COUCHES, NBRES DE SECTEURS
C           FF : TABLEAU DES FONCTIONS DE FORMES
C           M : NBRES DE MODES DE FOURIER
C           L : LONGEUR DE L'ELEMENT
C           H : SON EPAISSEUR
C           A : SON RAYON MOYEN
C           XPG : COORDONNES DES POINTS DE GAUSS
C   SORTIES : ---->
C           B : LA MATRICE DE DEFORMATIONS
C ......................................................................
C
      REAL*8             PI,DEUXPI,COSFI,SINFI,R8PI
      REAL*8             ZETA,R,HK,DHK,D2HK,FI,TK(4)
      REAL*8             TE,COSTE,SINTE,COSMTE,SINMTE
      INTEGER            K,ICOLON,N,IBLOC
      REAL*8             XPG(4),CK,SK,DENA,DENR,DENT
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
C
      PI = R8PI()
      DEUXPI=2.D0*PI
C
      ZETA   = (ICOU-1)*H/(2.D0*NCOU)-H/2.D0
      FI     = (ISECT-1)*DEUXPI/(2.D0*NSECT)
      COSFI  = COS(FI)
      SINFI  = SIN(FI)
      TE     = FI - OMEGA
      COSTE  = COS(TE)
      SINTE  = SIN(TE)
      IF (MMT.EQ.0) THEN
         R      = A
      ELSE
         R      = A + ZETA
      ENDIF
      DENR   = RAYON + R*SINFI
      DENA   = RAYON + A*SINFI
      DENT   = (1.D0/DENR+A/(R*DENA))
      IF (NNO.EQ.3) THEN
         TK(1)  = 0.D0
         TK(2)  = THETA
         TK(3)  = THETA/2.D0
      ELSEIF (NNO.EQ.4) THEN
         TK(1)  = 0.D0
         TK(2)  = THETA
         TK(3)  = THETA/3.D0
         TK(4)  = 2.D0*THETA/3.D0
      ENDIF
C
C  REMLISSAGE DE LA MATRICE
C
      DO  10  K=1, NNO
C
      HK   = FF(NNO*(IGAU-1)+K)
      DHK  = FF(NNO*NPG+NNO*(IGAU-1)+K) * (2.D0/THETA)
      D2HK  = FF(2*NNO*NPG+NNO*(IGAU-1)+K)*(2.D0/THETA)*(2.D0/THETA)
      CK   = COS((1.D0+XPG(IGAU))*THETA/2.D0-TK(K))
      SK   = SIN((1.D0+XPG(IGAU))*THETA/2.D0-TK(K))

C
C LE 2/L EST DU AU PASSAGE DE L'ELEMENT DE REFERENCE A L'ELEMENT
C REEL
C
      IBLOC = (9+6*(M-1))*(K-1)
C
C  PARTIE POUTRE
C
C   1 ERE LIGNE
C
      B(1,IBLOC+1)=  DHK*CK/DENR
      B(1,IBLOC+2)=  DHK*SK/DENR
      B(1,IBLOC+3)=  0.D0
      B(1,IBLOC+4)=  R*COSFI*DHK*SK/DENR
      B(1,IBLOC+5)= -R*COSFI*DHK*CK/DENR
      B(1,IBLOC+6)=  R*SINFI*DHK/DENR
C
C   2 EME LIGNE
C
      B(2,IBLOC+1)=  0.D0
      B(2,IBLOC+2)=  0.D0
      B(2,IBLOC+3)=  0.D0
      B(2,IBLOC+4)=  0.D0
      B(2,IBLOC+5)=  0.D0
      B(2,IBLOC+6)=  0.D0
C
C   3 EME LIGNE
C
      B(3,IBLOC+1)=  COSFI*DHK*SK/DENR
      B(3,IBLOC+2)= -COSFI*DHK*CK/DENR
      B(3,IBLOC+3)=  SINFI*DHK/DENR
      B(3,IBLOC+4)= -(HK*SK*RAYON*SINFI+R*DHK*CK)/DENR
      B(3,IBLOC+5)=  (HK*CK*RAYON*SINFI-R*DHK*SK)/DENR
      B(3,IBLOC+6)=  RAYON*COSFI*HK/DENR
C
C   4 EME LIGNE
C
      B(4,IBLOC+1)=  SINFI*DHK*SK/DENR
      B(4,IBLOC+2)= -SINFI*DHK*CK/DENR
      B(4,IBLOC+3)= -COSFI*DHK/DENR
      B(4,IBLOC+4)=  RAYON*COSFI*HK*SK/DENR
      B(4,IBLOC+5)= -RAYON*COSFI*HK*CK/DENR
      B(4,IBLOC+6)=  RAYON*SINFI*HK/DENR
C
C  FIN PARTIE POUTRE ET DEBUT PARTIE SUPPLIMENTAIRE
C
C      PARTIE IN-PLANE
C
C
          DO 20 N=2,M
          ICOLON = IBLOC + 6 + 3*(N-2)
CC         COSMTE = COS(N*FI-OMEGA)
CC         SINMTE = SIN(N*FI-OMEGA)
          COSMTE = COS(N*(FI-OMEGA))
          SINMTE = SIN(N*(FI-OMEGA))
C
          B(1,ICOLON+1)=  DHK*COSMTE/DENA
          B(1,ICOLON+2)=  HK*SINMTE*COSFI*(1.D0+ZETA/A)/DENR
          B(1,ICOLON+3)=  - ZETA*D2HK*COSMTE/(DENR*DENA)
     &                    + HK*COSMTE*SINFI/DENR
     &                    + ZETA*N*HK*SINMTE*COSFI/(A*DENR)
C
          B(2,ICOLON+1)=  0.D0
          B(2,ICOLON+2)=  (N/R)*HK*COSMTE*(1.D0+ZETA/A)
          B(2,ICOLON+3)=  (1.D0/R)*HK*COSMTE*(1.D0+ZETA*N*N/A)
C
C
          B(3,ICOLON+1)= -(N/R)*HK*SINMTE
     &                   -HK*COSMTE*COSFI/DENR
     &                   -ZETA*HK*COSFI*SINFI*COSMTE/DENA*DENT
     &                   +ZETA*HK*(COSMTE*COSFI-N*SINMTE*SINFI)
     &                   /(R*DENA)
          B(3,ICOLON+2)= DHK*SINMTE*(1+ZETA/A)/DENR
          B(3,ICOLON+3)= ZETA*N*DHK*SINMTE
     &                  *(1.D0/(A*DENR)+1.D0/(R*DENA))
     &                  +ZETA*DHK*COSFI*COSMTE/DENA*DENT
C
C
          B(4,ICOLON+1)=  0.D0
          B(4,ICOLON+2)=  0.D0
          B(4,ICOLON+3)=  0.D0
C
C
20        CONTINUE
C
C  FIN PARTIE IN-PLANE DEBUT PARTIE OUT-OF-PLANE
C
          DO 30 N=2,M
          ICOLON = IBLOC + 6 + 3*(M-1) + 3*(N-2)
CC          COSMTE = COS(N*FI-OMEGA)
CC          SINMTE = SIN(N*FI-OMEGA)
          COSMTE = COS(N*(FI-OMEGA))
          SINMTE = SIN(N*(FI-OMEGA))
C
          B(1,ICOLON+1)=  DHK*SINMTE/DENA
          B(1,ICOLON+2)=  HK*COSMTE*COSFI*(1.D0+ZETA/A)/DENR
          B(1,ICOLON+3)=  - ZETA*D2HK*SINMTE/(DENR*DENA)
     &                    + HK*SINMTE*SINFI/DENR
     &                    - ZETA*N*HK*COSMTE*COSFI/(A*DENR)
C
          B(2,ICOLON+1)=  0.D0
          B(2,ICOLON+2)=  -(N/R)*HK*SINMTE*(1.D0+ZETA/A)
          B(2,ICOLON+3)=  (1.D0/R)*HK*SINMTE*(1.D0+ZETA*N*N/A)
C
C
          B(3,ICOLON+1)=  (N/R)*HK*COSMTE
     &                   -HK*SINMTE*COSFI/DENR
     &                   -ZETA*HK*COSFI*SINFI*SINMTE/DENA*DENT
     &                   +ZETA*HK*(SINMTE*COSFI+N*COSMTE*SINFI)
     &                   /(R*DENA)
          B(3,ICOLON+2)=  DHK*COSMTE*(1.D0+ZETA/A)/DENR
          B(3,ICOLON+3)= -ZETA*N*DHK*COSMTE
     &                   *(1.D0/(A*DENR)+1.D0/(R*DENA))
     &                   +ZETA*DHK*COSFI*SINMTE/DENA*DENT
C
C
          B(4,ICOLON+1)=  0.D0
          B(4,ICOLON+2)=  0.D0
          B(4,ICOLON+3)=  0.D0
C
C
30        CONTINUE
C
C  FIN OUT-OF-PLANE DEBUT PARTIE GONFLEMENT
C
          ICOLON = IBLOC + 6*(M-1) + 6
          B(1,ICOLON+1)=  HK*SINFI/DENR-ZETA*D2HK/(DENA*DENR)
          B(2,ICOLON+1)=  HK/R
          B(3,ICOLON+1)=  ZETA*DHK*COSFI/DENA*DENT
          B(4,ICOLON+1)=  0.D0
C
C  FIN PARTIE GONFLEMENT
C
C  1ERS MODES EN W
C
              COSTE = COS(TE)
              SINTE = SIN(TE)
          B(1,ICOLON+2)=  -ZETA*D2HK*COSTE/(DENR*DENA)
     &                    +2.D0*HK*COSFI*SINTE/DENR*ZETA/A
     &                    +HK*(COSTE*SINFI+COSFI*SINTE)/DENR
          B(2,ICOLON+2)=  (2.D0/R)*HK*COSTE*(1.D0+ZETA/A)
          B(3,ICOLON+2)=  DHK*SINTE
     &                   *((1.D0+2.D0*ZETA/A)/DENR+ZETA/(R*DENA))
     &                   +ZETA*DHK*COSTE*COSFI/DENA*DENT
          B(4,ICOLON+2)=  0.D0
C
          B(1,ICOLON+3)= -ZETA*D2HK*SINTE/(DENA*DENR)
     &                   +HK*(SINFI*SINTE-COSFI*COSTE)/DENR
     &                   -2.D0*ZETA*HK*COSFI*COSTE/(A*DENR)
          B(2,ICOLON+3)= (2.D0/R)*HK*SINTE*(1.D0+ZETA/A)
          B(3,ICOLON+3)= -DHK*COSTE
     &                   *((1.D0+2.D0*ZETA/A)/DENR+ZETA/(R*DENA))
     &                   +ZETA*DHK*SINTE*COSFI/DENA*DENT
          B(4,ICOLON+3)=  0.D0
C
C
C  FIN PARTIE GONFLEMENT
C
C FIN DES 1ERS MODES EN W
C
10    CONTINUE
C
C
C FIN REMPLISSAGE DE LA MATRICE
C
C
9999  CONTINUE
      END

      SUBROUTINE VECNUV( IPRE, IDER, GAMMA, PHINIT, DPHI, N, K, DIM,
     &                   VECTN, VECTU, VECTV )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 24/11/2003   AUTEUR F1BHHAJ J.ANGLES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE F1BHHAJ J.ANGLES
      IMPLICIT   NONE
      INTEGER    IPRE, IDER, N, K, DIM
      REAL*8     GAMMA, PHINIT, DPHI, VECTN(DIM), VECTU(DIM)
      REAL*8     VECTV(DIM)
C ----------------------------------------------------------------------
C BUT: DETERMINER LES COORDONNEES (X,Y,Z) DES VECTEURS NORMAUX.
C ----------------------------------------------------------------------
C ARGUMENTS :
C  IPRE     IN   I  : PREMIERE VALEUR DU POINTEUR.
C  IDER     IN   I  : DERNIERE VALEUR DU POINTEUR.
C  GAMMA    IN   R  : VALEUR DE GAMMA.
C  PHINIT   IN   R  : VALEUR INITIALE DE L'ANGLE PHI.
C  DPHI     IN   R  : INCREMENT DE PHI.
C  N        IN   I  : COMPTEUR.
C  K        IN   I  : VALEUR DU DECALAGE.
C  DIM      IN   I  : DIMENSION DES VECTEUR.
C  VECTN    OUT  R  : COORDONNEES DES VECTEURS NORMAUX.
C  VECTU    OUT  R  : COORDONNEES DES VECTEURS TANGENTS, COMPOSANTES U.
C  VECTV    OUT  R  : COORDONNEES DES VECTEURS TANGENTS, COMPOSANTES V.
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ------------------------------------------------------------------
      INTEGER       I
      REAL*8        PHI
C     ------------------------------------------------------------------
C
C234567                                                              012
C
      CALL JEMARQ()
C
      DO 10 I=IPRE, IDER
         N = N + 1
         PHI = PHINIT + (I-K)*DPHI
C
         VECTN((N-1)*3 + 1) = SIN(GAMMA)*COS(PHI)
         VECTN((N-1)*3 + 2) = SIN(GAMMA)*SIN(PHI)
         VECTN((N-1)*3 + 3) = COS(GAMMA)
C
         VECTU((N-1)*3 + 1) = -SIN(PHI)
         VECTU((N-1)*3 + 2) = COS(PHI)
         VECTU((N-1)*3 + 3) = 0.0D0
C
         VECTV((N-1)*3 + 1) = -COS(GAMMA)*COS(PHI)
         VECTV((N-1)*3 + 2) = -COS(GAMMA)*SIN(PHI)
         VECTV((N-1)*3 + 3) = SIN(GAMMA)

 10   CONTINUE
C
      CALL JEDEMA()
C
      END

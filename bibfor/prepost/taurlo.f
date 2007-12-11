      SUBROUTINE TAURLO ( NBVEC, JVECTN, JVECTU, JVECTV, NBORDR, KWORK,
     &                    SOMPGW, JRWORK, TSPAQ, IPG, JVECPG )
      IMPLICIT   NONE
      INTEGER    NBVEC, JVECTN, JVECTU, JVECTV, NBORDR, KWORK
      INTEGER    SOMPGW, JRWORK, TSPAQ, IPG, JVECPG
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 26/05/2003   AUTEUR F1BHHAJ J.ANGLES 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C RESPONSABLE F1BHHAJ J.ANGLES
C ---------------------------------------------------------------------
C BUT: CONSTRUIRE LES COMPOSANTES u ET v DU VECTEUR DE CISAILLEMENT TAU
C      DANS LE REPERE LOCAL PERPENDICULAIRE AU VECTEUR NORMAL, POUR
C      TOUS LES VECTEURS NORMAUX A TOUS LES NUMEROS D'ORDRE.
C ----------------------------------------------------------------------
C ARGUMENTS :
C     NBVEC   : IN  : NOMBRE DE VECTEURS NORMAUX.
C     JVECTN  : IN  : ADRESSE DU VECTEUR CONTENANT LES COMPOSANTES DES
C                     VECTEURS NORMAUX.
C     JVECTU  : IN  : ADRESSE DU VECTEUR CONTENANT LES COMPOSANTES DES
C                     VECTEURS u DU PLAN DE CISAILLEMENT.
C     JVECTV  : IN  : ADRESSE DU VECTEUR CONTENANT LES COMPOSANTES DES
C                     VECTEURS v DU PLAN DE CISAILLEMENT.
C     NBORDR  : IN  : NOMBRE DE NUMEROS D'ORDRE.
C     KWORK   : IN  : KWORK = 0 ON TRAITE LA 1ERE MAILLE DU PAQUET DE
C                               MAILLES ;
C                     KWORK = 1 ON TRAITE LA IEME (I>1) MAILLE DU PAQUET
C                               MAILLES.
C     SOMPGW  : IN  : SOMME DES POINTS DE GAUSS DES N MAILLES PRECEDANT 
C                     LA MAILLE COURANTE.
C     JRWORK  : IN  : ADRESSE DU VECTEUR DE TRAVAIL CONTENANT 
C                     L'HISTORIQUE DES TENSEURS DES CONTRAINTES
C                     ATTACHES A CHAQUE POINT DE GAUSS DES MAILLES
C                     DU <<PAQUET>> DE MAILLES.
C     TSPAQ   : IN  : TAILLE DU SOUS-PAQUET DU <<PAQUET>> DE MAILLES
C                     COURANT.
C     IPG     : IN  : IEME POINT DE GAUSS.
C     JVECPG  : IN  : ADRESSE DU VECTEUR DE TRAVAIL CONTENANT 
C                     LES COMPOSANTES u ET v DU VECTEUR TAU 
C                     (CISAILLEMENT), POUR TOUS LES NUMEROS
C                     D'ORDRE DE CHAQUE VECTEUR NORMAL.
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
      INTEGER    IVECT, IORDR, N, ADRS
      REAL*8     NX, NY, NZ, UX, UY, UZ, VX, VY, VZ
      REAL*8     SIXX, SIYY, SIZZ, SIXY, SIXZ, SIYZ, FX, FY, FZ
      REAL*8     NORM, TAUX, TAUY, TAUZ, CUTAU, CVTAU
C     ------------------------------------------------------------------
C
C234567                                                              012
C
      CALL JEMARQ()

      N = 0

      DO 10 IVECT=1, NBVEC
         NX = ZR(JVECTN + (IVECT-1)*3)
         NY = ZR(JVECTN + (IVECT-1)*3 + 1)
         NZ = ZR(JVECTN + (IVECT-1)*3 + 2)
C
         UX = ZR(JVECTU + (IVECT-1)*3)
         UY = ZR(JVECTU + (IVECT-1)*3 + 1)
         UZ = ZR(JVECTU + (IVECT-1)*3 + 2)
C
         VX = ZR(JVECTV + (IVECT-1)*3)
         VY = ZR(JVECTV + (IVECT-1)*3 + 1)
         VZ = ZR(JVECTV + (IVECT-1)*3 + 2)
C
         DO 20 IORDR=1, NBORDR
            ADRS = (IORDR-1)*TSPAQ+KWORK*SOMPGW*6+(IPG-1)*6
            SIXX = ZR(JRWORK + ADRS + 0)
            SIYY = ZR(JRWORK + ADRS + 1)
            SIZZ = ZR(JRWORK + ADRS + 2)
            SIXY = ZR(JRWORK + ADRS + 3)
            SIXZ = ZR(JRWORK + ADRS + 4)
            SIYZ = ZR(JRWORK + ADRS + 5)
C
C CALCUL DE vect_F = [SIG].vect_n
            FX = SIXX*NX + SIXY*NY + SIXZ*NZ      
            FY = SIXY*NX + SIYY*NY + SIYZ*NZ      
            FZ = SIXZ*NX + SIYZ*NY + SIZZ*NZ      
C
C CALCUL DE NORM = vect_F.vect_n
            NORM = FX*NX + FY*NY + FZ*NZ
C
C CALCUL DE vect_TAU = vect_F - NORM vect_n
            TAUX = FX - NORM*NX
            TAUY = FY - NORM*NY
            TAUZ = FZ - NORM*NZ
C
C PROJECTION DU vect_TAU SUR LES VECTEURS u ET v DU REPERE LOCAL
            CUTAU = UX*TAUX + UY*TAUY + UZ*TAUZ
            CVTAU = VX*TAUX + VY*TAUY + VZ*TAUZ
            N = N + 1
            ZR( JVECPG + (N-1)*2 )     = CUTAU
            ZR( JVECPG + (N-1)*2 + 1 ) = CVTAU

 20      CONTINUE

 10   CONTINUE
C
      CALL JEDEMA()
      END

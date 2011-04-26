      SUBROUTINE AVPHYD( NBORDR, VWORK, TDISP, KWORK, SOMMW, TSPAQ, I,
     &                   VPHYDR )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER    NBORDR, TDISP, KWORK, SOMMW, TSPAQ, I
      REAL*8     VWORK(TDISP), VPHYDR(NBORDR)
C ----------------------------------------------------------------------
C BUT: CALCULER LA PRESSION HYDROSTATIQUE MAXIMALE.
C ----------------------------------------------------------------------
C ARGUMENTS :
C  NBORDR : IN   I  : NOMBRE DE NUMEROS D'ORDRE.
C  VWORK  : IN   R  : VECTEUR DE TRAVAIL CONTENANT
C                     L'HISTORIQUE DES TENSEURS DES CONTRAINTES
C                     ATTACHES A CHAQUE POINT DE GAUSS DES MAILLES
C                     DU <<PAQUET>> DE MAILLES.
C  TDISP  : IN   I  : TAILLE DU VECTEUR DE TRAVAIL.
C  KWORK  : IN   I  : KWORK = 0 ON TRAITE LA 1ERE MAILLE DU PAQUET
C                               MAILLES OU LE 1ER NOEUD DU PAQUET DE
C                               NOEUDS;
C                     KWORK = 1 ON TRAITE LA IEME (I>1) MAILLE DU PAQUET
C                               MAILLES OU LE IEME NOEUD DU PAQUET
C                               DE NOEUDS.
C  SOMMW  : IN   I  : SOMME DES POINTS DE GAUSS OU DES NOEUDS DES N
C                     MAILLES PRECEDANT LA MAILLE COURANTE.
C  TSPAQ  : IN   I  : TAILLE DU SOUS-PAQUET DU <<PAQUET>> DE MAILLES
C                     OU DE NOEUDS COURANT.
C  I      : IN   I  : IEME POINT DE GAUSS OU IEME NOEUD.
C  VPHYDR : OUT  R  : VECTEUR CONTENANT LA PRESSION HYDROSTATIQUE A
C                     TOUS LES INSTANTS.
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
      INTEGER    IORDR, ADRS
      REAL*8     SIXX, SIYY, SIZZ
C     ------------------------------------------------------------------
C
C234567                                                              012
C
      CALL JEMARQ()
C
      DO 10 IORDR=1, NBORDR
         ADRS = (IORDR-1)*TSPAQ + KWORK*SOMMW*6 + (I-1)*6
         SIXX = VWORK(ADRS + 1)
         SIYY = VWORK(ADRS + 2)
         SIZZ = VWORK(ADRS + 3)
C
C CALCUL DE LA PRESSION HYDROSTATIQUE = 1/3 Tr[SIG] A TOUS
C LES INSTANTS
C
         VPHYDR(IORDR) = (SIXX + SIYY + SIZZ)/3.0D0
C
 10   CONTINUE
C
      CALL JEDEMA()
C
      END

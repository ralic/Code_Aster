      SUBROUTINE AVSIGN( NBVEC, NBORDR, VECTN, VWORK, TDISP, KWORK,
     &                   SOMMW, TSPAQ, I, VSIGN )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 26/09/2011   AUTEUR TRAN V-X.TRAN 
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
      IMPLICIT      NONE
      INTEGER       NBVEC, NBORDR, TDISP, KWORK, SOMMW, TSPAQ, I
      REAL*8        VECTN(3*NBVEC)
      REAL*8        VWORK(TDISP), VSIGN(NBVEC*NBORDR)
C ----------------------------------------------------------------------
C BUT: CALCULER LA CONTRAINTE NORMALE POUR TOUS LES VECTEURS NORMAUX
C      A TOUS LES NUMEROS D'ORDRE.
C ----------------------------------------------------------------------
C ARGUMENTS :
C  NBVEC  : IN   I  : NOMBRE DE VECTEURS NORMAUX.
C  NBORDR : IN   I  : NOMBRE DE NUMEROS D'ORDRE.
C  VECTN  : IN   R  : VECTEUR CONTENANT LES COMPOSANTES DES
C                     VECTEURS NORMAUX.
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
C  NOMCRI : IN  K16 : NOM DU CRITERE D'ENDOMMAGEMENT PAR FATIGUE.
C  VSIGN  : OUT  R  : VECTEUR CONTENANT LES VALEURS DE LA CONTRAINTE
C                     NORMALE, POUR TOUS LES NUMEROS D'ORDRE
C                     DE CHAQUE VECTEUR NORMAL.
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
      INTEGER    N, DECAL, IVECT, IORDR, ADRS 
      REAL*8     NX, NY, NZ
      REAL*8     SIXX, SIYY, SIZZ, SIXY, SIXZ, SIYZ, FX, FY, FZ, NORM
C     ------------------------------------------------------------------

C234567                                                              012

      CALL JEMARQ()
      
      N = 1
C       IF (( NOMCRI(1:16) .EQ. 'FATESOCI_MODI_AV' ) .OR. 
C      &  FORDEF ) THEN
C          DECAL = 12
C       ELSE
C          DECAL = 6
C       ENDIF
      DECAL = 12
      DO 10 IVECT=1, NBVEC
         NX = VECTN((IVECT-1)*3 + 1)
         NY = VECTN((IVECT-1)*3 + 2)
         NZ = VECTN((IVECT-1)*3 + 3)

         DO 20 IORDR=1, NBORDR
            ADRS = (IORDR-1)*TSPAQ + KWORK*SOMMW*DECAL + (I-1)*DECAL
            SIXX = VWORK(ADRS + 1)
            SIYY = VWORK(ADRS + 2)
            SIZZ = VWORK(ADRS + 3)
            SIXY = VWORK(ADRS + 4)
            SIXZ = VWORK(ADRS + 5)
            SIYZ = VWORK(ADRS + 6)

C CALCUL DE vect_F = [SIG].vect_n
            FX = SIXX*NX + SIXY*NY + SIXZ*NZ      
            FY = SIXY*NX + SIYY*NY + SIYZ*NZ      
            FZ = SIXZ*NX + SIYZ*NY + SIZZ*NZ      

C CALCUL DE NORM = vect_F.vect_n
            NORM = FX*NX + FY*NY + FZ*NZ
            VSIGN(N) = NORM
            N = N + 1
 20      CONTINUE
 10   CONTINUE

      CALL JEDEMA()

      END

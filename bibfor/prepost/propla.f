      SUBROUTINE PROPLA( NBVEC, VECTN, VECTU, VECTV, NBORDR, KWORK,
     &                   SOMMW, VWORK, TDISP, TSPAQ, I, NOMCRI,NOMFOR, 
     &                   FORDEF, FATSOC,VECTRA )
      IMPLICIT      NONE
      INCLUDE 'jeveux.h'
      INTEGER       NBVEC, NBORDR, KWORK
      INTEGER       SOMMW, TDISP, TSPAQ, I
      REAL*8        VECTN(3*NBVEC), VECTU(3*NBVEC), VECTV(3*NBVEC)
      REAL*8        VWORK(TDISP), FATSOC, VECTRA(2*NBVEC*NBORDR)
      LOGICAL       FORDEF
      CHARACTER*16  NOMCRI, NOMFOR
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 26/06/2012   AUTEUR TRAN V-X.TRAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C  NBVEC    IN  I  : NOMBRE DE VECTEURS NORMAUX.
C  VECTN    IN  R  : VECTEUR CONTENANT LES COMPOSANTES DES
C                    VECTEURS NORMAUX.
C  VECTU    IN  R  : VECTEUR CONTENANT LES COMPOSANTES DES
C                    VECTEURS u DU PLAN DE CISAILLEMENT.
C  VECTV    IN  R  : VECTEUR CONTENANT LES COMPOSANTES DES
C                    VECTEURS v DU PLAN DE CISAILLEMENT.
C  NBORDR   IN  I  : NOMBRE DE NUMEROS D'ORDRE.
C  KWORK    IN  I  : KWORK = 0 ON TRAITE LA 1ERE MAILLE DU PAQUET
C                              MAILLES OU LE 1ER NOEUD DU PAQUET DE
C                              NOEUDS;
C                    KWORK = 1 ON TRAITE LA IEME (I>1) MAILLE DU PAQUET
C                              MAILLES OU LE IEME NOEUD DU PAQUET
C                              DE NOEUDS.
C  SOMMW    IN  I  : SOMME DES POINTS DE GAUSS OU DES NOEUDS DES N
C                    MAILLES PRECEDANT LA MAILLE COURANTE.
C  VWORK    IN  R  : VECTEUR DE TRAVAIL CONTENANT 
C                    L'HISTORIQUE DES TENSEURS DES CONTRAINTES
C                    ATTACHES A CHAQUE POINT DE GAUSS OU NOEUD DES
C                    MAILLE OU NOEUD DU <<PAQUET>> DE MAILLES OU 
C                    DE NOEUDS.
C  TDISP    IN  I  : DIMENSION DU VECTEUR VWORK
C  TSPAQ    IN  I  : TAILLE DU SOUS-PAQUET DU <<PAQUET>> DE MAILLES
C                    OU DE NOEUDS COURANT.
C  I        IN  I  : IEME POINT DE GAUSS OU IEME NOEUD.
C  NOMCRI   IN  K16: NOM DU CRITERE D'ENDOMMAGEMENT PAR FATIGUE.
C  FATSOC   IN  R  : COEFFICIENT PERMETTANT D'UTILISER LES MEMES
C                    ROUTINES POUR LE TRAITEMENT DES CONTRAINTES ET
C                    DES DEFORMATIONS.
C  VECTRA   OUT R  : VECTEUR DE TRAVAIL CONTENANT 
C                    LES COMPOSANTES u ET v DU VECTEUR TAU 
C                    (CONTRAINTE DE CISAILLEMENT) OU
C                    GAMMA (DEFORMATION DE CISAILLEMENT), POUR TOUS LES
C                    NUMEROS D'ORDRE DE CHAQUE VECTEUR NORMAL.
C
C REMARQUE : CETTE ROUTINE SERT POUR LE TRAITEMENT DES POINTS DE GAUSS
C            ET DES NOEUDS.
C ----------------------------------------------------------------------
      INTEGER    IVECT, IORDR, N, DECAL, ADRS, DECPRO, PARACT(30)
      LOGICAL    LBID, CRSIGM, CREPST, CREPSE,CREPSP
      CHARACTER*16  TYPCHA
      REAL*8     NX, NY, NZ, UX, UY, UZ, VX, VY, VZ
      REAL*8     CMPXX, CMPYY, CMPZZ, CMPXY, CMPXZ, CMPYZ
      REAL*8     FX, FY, FZ
      REAL*8     NORM, CISX, CISY, CISZ
      REAL*8     CUCIS, CVCIS
C     ------------------------------------------------------------------
C
C234567                                                              012

      CALL JEMARQ()
      
      TYPCHA = 'NON_PERIODIQUE'

      N = 0
      DECPRO = 0
C    DECPRO POUR IDENTIFIER L'AXE A PRPJECTER
C---    ANALYSER LE CRITERE
C  INITIALISER
      CRSIGM = .FALSE. 
      CREPST = .FALSE. 
      CREPSE = .FALSE. 
      CREPSP = .FALSE.
      
      CALL ANACRI( NOMCRI,NOMFOR,TYPCHA,'NON', PARACT,
     &            LBID, CRSIGM, CREPST, CREPSE,CREPSP)
   
     
C TRAITEMENT DES PAQUETS DE NOEUDS.

      IF (( NOMCRI(1:16) .EQ. 'FATESOCI_MODI_AV' ) .OR.
     &    FORDEF ) THEN
         DECPRO = 6
         GOTO 50
      ENDIF
      
      IF (CRSIGM) THEN
            DECPRO = 0
      ELSE
         IF (CREPST) THEN
            DECPRO = 6
         ELSE
            IF (CREPSP) THEN
               DECPRO = 12
            ENDIF
         ENDIF
      ENDIF

50    CONTINUE
      
      DECAL = 18

      DO 10 IVECT=1, NBVEC
         NX = VECTN((IVECT-1)*3 + 1)
         NY = VECTN((IVECT-1)*3 + 2)
         NZ = VECTN((IVECT-1)*3 + 3)

         UX = VECTU((IVECT-1)*3 + 1)
         UY = VECTU((IVECT-1)*3 + 2)
         UZ = VECTU((IVECT-1)*3 + 3)

         VX = VECTV((IVECT-1)*3 + 1)
         VY = VECTV((IVECT-1)*3 + 2)
         VZ = VECTV((IVECT-1)*3 + 3)

         DO 20 IORDR=1, NBORDR

C ON PROJETTE LA DEFORMATION SI FORDEF = OUI
C             IF (( NOMCRI(1:16) .EQ. 'FATESOCI_MODI_AV' ) .OR.
C      &         FORDEF ) THEN
C                ADRS = (IORDR-1)*TSPAQ + KWORK*SOMMW*DECAL
C      &                             + (I-1)*DECAL + 6
C             ELSE
C                ADRS = (IORDR-1)*TSPAQ + KWORK*SOMMW*DECAL
C      &                             + (I-1)*DECAL
C            ENDIF
            
             ADRS = (IORDR-1)*TSPAQ + KWORK*SOMMW*DECAL
     &                             + (I-1)*DECAL + DECPRO
      
            CMPXX = VWORK(ADRS + 1)
            CMPYY = VWORK(ADRS + 2)
            CMPZZ = VWORK(ADRS + 3)
            CMPXY = VWORK(ADRS + 4)
            CMPXZ = VWORK(ADRS + 5)
            CMPYZ = VWORK(ADRS + 6)

C CALCUL DE vect_F = [CMP].vect_n  AVEC [CMP] = [EPS] OU [SIG]
            FX = CMPXX*NX + CMPXY*NY + CMPXZ*NZ
            FY = CMPXY*NX + CMPYY*NY + CMPYZ*NZ
            FZ = CMPXZ*NX + CMPYZ*NY + CMPZZ*NZ

C CALCUL DE NORM = vect_F.vect_n
            NORM = FX*NX + FY*NY + FZ*NZ

C CALCUL DE vect_CIS = vect_F - NORM vect_n
C vect_CIS = VECTEUR CISAILLEMENT EN CONTRAINTE OU EN DEFORMATION
            CISX = FX - NORM*NX
            CISY = FY - NORM*NY
            CISZ = FZ - NORM*NZ

C PROJECTION DU vect_CIS SUR LES VECTEURS u ET v DU REPERE LOCAL
            CUCIS = UX*CISX + UY*CISY + UZ*CISZ
            CVCIS = VX*CISX + VY*CISY + VZ*CISZ

            N = N + 1
            VECTRA(N*2 - 1) = FATSOC*CUCIS
            VECTRA(N*2) = FATSOC*CVCIS

 20      CONTINUE
 10   CONTINUE
C
      CALL JEDEMA()
C
      END

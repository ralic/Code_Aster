      SUBROUTINE AVPLCR(NBVEC, VECTN, VECTU, VECTV, NBORDR, KWORK,
     &                   SOMNOW, VWORK, TDISP, TSPAQ, I, NOMCRI, 
     &                   NOMFOR,GRDVIE, FORVIE,FORDEF,FATSOC,PROAXE,   
     &                   NOMMAT,VALA,COEFPA, CUDOMX, NXM, NYM, NZM )
      IMPLICIT      NONE
      INTEGER       NBORDR, KWORK, I, NBVEC
      INTEGER       SOMNOW, TDISP, TSPAQ
      LOGICAL       FORDEF
      REAL*8        VECTN(3*NBVEC), VECTU(3*NBVEC), VECTV(3*NBVEC)
      REAL*8        VWORK(TDISP), FATSOC
      CHARACTER*16  NOMCRI,PROAXE,NOMFOR,FORVIE
      CHARACTER*8   NOMMAT,GRDVIE
      REAL*8        VALA, COEFPA
      REAL*8        CUDOMX, NXM, NYM, NZM
      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 20/06/2011   AUTEUR TRAN V-X.TRAN 
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
C TOLE CRS_1404 CRP_21
C----------------------------------------------------------------------
C BUT:  POUR LA FATIGUE A AMPLITUDE VARIABLE 
C       DETERMINER LE PLAN CRITIQUE OU DOMMAGE EST MAX
C ---------------------------------------------------------------------
C ARGUMENTS :
C  NBVEC   IN  I  : NOMBRE MAX DE VECTEUR(209 POUR LA VERSION ACTUELLE)
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
C  PROAXE    IN   K16: TYPE DE PROJECTION (UN OU DEUX AXES).
C  NOMMAT   IN   K  : NOM DU MATERIAU.
C  VALA     IN   R  : VALEUR DU PARAMETRE a ASSOCIE AU CRITERE.
C  COEFPA   IN   R  : COEFFICIENT DE PASSAGE CISAILLEMENT - UNIAXIAL.
C  VNORMX   OUT  I  : NUMERO DU VECTEUR NORMAL ASSOCIE AU MAX DES CUMULS
C                     DE DOMMAGE.
C  CUDOMX   OUT  R  : VALEUR DU MAX DES CUMULS DE DOMMAGE.
C REMARQUE : CETTE ROUTINE SERT POUR LE TRAITEMENT DES POINTS DE GAUSS
C            ET DES NOEUDS.
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
      INTEGER       NCYCL(NBVEC), NBVEC1,NVAL
      INTEGER       OMIN(NBVEC*(NBORDR+2)), OMAX(NBVEC*(NBORDR+2))
      REAL*8        VMIN(NBVEC*(NBORDR+2)), VMAX(NBVEC*(NBORDR+2))
      CHARACTER*8  METHOD 
      REAL*8        PSEUIL, GAMMAM, PHIM, DPHI2, EPSILO,GAMMA
      INTEGER       VNORMX, IDEB,IFIN,N, K, DIM, J
      REAL*8        VECN2(3*NBVEC), VECU2(3*NBVEC), VECV2(3*NBVEC) 
      REAL*8        VECN1(3*NBVEC), VECU1(3*NBVEC), VECV1(3*NBVEC)
      REAL*8        DGAM2, PI, R8PI, PHI0  
C     --------------------------
      EPSILO = 1.0D-7
      PI = R8PI()
      
         NBVEC1 = 209
         
         METHOD = 'RAINFLOW'
    
         CALL GETVR8(' ','DELTA_OSCI',1,1,1,PSEUIL,NVAL)
            
         CALL  AVCIPR( NBVEC1, VECTN, VECTU, VECTV,
     &         NBORDR, KWORK, SOMNOW, VWORK, TDISP, TSPAQ,  
     &         I, NOMCRI, FORDEF, FATSOC, PROAXE, PSEUIL,  
     &         METHOD,NCYCL,VMIN,VMAX, OMIN, OMAX)
     
C REMPACER PAR SUBROUTINE AVGRDO          
 
         CALL AVGRDO(NBVEC1, NBORDR, VECTN, VWORK, TDISP, KWORK,
     &             SOMNOW, TSPAQ, I, NOMMAT, NOMCRI, 
     &             NOMFOR,GRDVIE, FORVIE, FORDEF,VALA,   
     &             COEFPA, NCYCL,VMIN,VMAX, OMIN, OMAX,CUDOMX,VNORMX)
  

C 9. PREMIER RAFFINEMENT CONCERNANT LA DETERMINATION DU VECTEUR NORMAL
C    CORRESPONDANT AU MAX DES CUMULS DE DOMMAGE.

         NXM = VECTN((VNORMX-1)*3 + 1)
         NYM = VECTN((VNORMX-1)*3 + 2)
         NZM = VECTN((VNORMX-1)*3 + 3)
         GAMMAM = ATAN2(SQRT(ABS(1.0D0-NZM**2)),NZM)
         IF (GAMMAM .LT. 0.0D0) THEN
            GAMMAM = GAMMAM + PI
         ENDIF

         IF ((ABS(NYM) .LT. EPSILO) .AND.
     &       (ABS(NXM) .LT. EPSILO)) THEN
           PHIM = 0.0D0
         ELSE
           PHIM = ATAN2(ABS(NYM),NXM)
         ENDIF
         IF (PHIM .LT. 0.0D0) THEN
           PHIM = PHIM + PI
         ENDIF

         IF (ABS(GAMMAM) .LT. EPSILO) THEN
            GAMMA = 5.0D0*(PI/180.0D0)
            DPHI2 = 60.0D0*(PI/180.0D0)
            IDEB = 1
            IFIN = 6
            N = 0
            K = 1
            DIM = 27
            PHI0 = 0.0D0

            CALL VECNUV(IDEB, IFIN, GAMMA, PHI0, DPHI2, N, K, DIM,
     &                  VECN2, VECU2, VECV2)
            GAMMA = 0.0D0
            PHI0 = PI
            IDEB = 1
            IFIN = 1
            K = 1

            CALL VECNUV(IDEB, IFIN, GAMMA, PHI0, DPHI2, N, K, DIM,
     &                  VECN2, VECU2, VECV2)

C 9.1 PROJECTION DE L'HISTORIQUE DU CISAILLEMENT SUR UN PLAN

            NBVEC1 = 7
            
            CALL  AVCIPR( NBVEC1, VECN2, VECU2, VECV2,
     &         NBORDR, KWORK, SOMNOW, VWORK, TDISP, TSPAQ,  
     &         I, NOMCRI, FORDEF, FATSOC, PROAXE, PSEUIL,  
     &         METHOD, NCYCL,VMIN,VMAX, OMIN, OMAX)  
       
     
         ELSE
            DGAM2 = 2.0D0*(PI/180.0D0)
            DPHI2 = DGAM2/SIN(GAMMAM)
            N = 0
            K = 2
            DIM = 27
            IDEB = 1
            IFIN = 3
            DO 430 J=1, 3
               GAMMA = GAMMAM + (J-K)*DGAM2
               CALL VECNUV(IDEB, IFIN, GAMMA, PHIM, DPHI2, N, K, DIM,
     &                     VECN2, VECU2, VECV2)
 430        CONTINUE

            NBVEC1 = 9

            CALL  AVCIPR( NBVEC1, VECN2, VECU2, VECV2,
     &         NBORDR, KWORK, SOMNOW, VWORK, TDISP, TSPAQ,  
     &         I, NOMCRI, FORDEF, FATSOC, PROAXE, PSEUIL,  
     &         METHOD, NCYCL,VMIN,VMAX, OMIN, OMAX )  
     
         ENDIF
         
C REMPACER PAR SUBROUTINE AVGRDO          
 
         CALL AVGRDO(NBVEC1, NBORDR, VECN2, VWORK, TDISP, KWORK,
     &             SOMNOW, TSPAQ, I, NOMMAT, NOMCRI, 
     &             NOMFOR,GRDVIE, FORVIE, FORDEF, VALA,   
     &             COEFPA,NCYCL,VMIN,VMAX, OMIN, OMAX,CUDOMX, VNORMX )
     


C 10. SECOND RAFFINEMENT CONCERNANT LA DETERMINATION DU VECTEUR NORMAL
C     CORRESPONDANT AU MAX DES CUMULS DE DOMMAGE.

         NXM = VECN2((VNORMX-1)*3+1)
         NYM = VECN2((VNORMX-1)*3+2)
         NZM = VECN2((VNORMX-1)*3+3)

         GAMMAM = ATAN2(SQRT(ABS(1.0D0-NZM**2)),NZM)
         IF (GAMMAM .LT. 0.0D0) THEN
            GAMMAM = GAMMAM + PI
         ENDIF

         IF ((ABS(NYM) .LT. EPSILO) .AND.
     &       (ABS(NXM) .LT. EPSILO)) THEN
           PHIM = 0.0D0
         ELSE
           PHIM = ATAN2(ABS(NYM),NXM)
         ENDIF
         IF (PHIM .LT. 0.0D0) THEN
           PHIM = PHIM + PI
         ENDIF

         IF (ABS(GAMMAM) .LT. EPSILO) THEN
            GAMMA = 5.0D0*(PI/180.0D0)
            DPHI2 = 60.0D0*(PI/180.0D0)
            IDEB = 1
            IFIN = 6
            N = 0
            K = 1
            DIM = 27
            PHI0 = 0.0D0
            CALL VECNUV(IDEB, IFIN, GAMMA, PHI0, DPHI2, N, K, DIM,
     &                  VECN1, VECU1, VECV1)

            GAMMA = 0.0D0
            PHI0 = PI
            IDEB = 1
            IFIN = 1
            K = 1
            CALL VECNUV(IDEB, IFIN, GAMMA, PHI0, DPHI2, N, K, DIM,
     &                  VECN1, VECU1, VECV1)

C 10.1 PROJECTION DE L'HISTORIQUE DU CISAILLEMENT SUR UN PLAN

            NBVEC1 = 7
            
            
            CALL  AVCIPR( NBVEC1, VECN1, VECU1, VECV1,
     &         NBORDR, KWORK, SOMNOW, VWORK, TDISP, TSPAQ,  
     &         I, NOMCRI, FORDEF, FATSOC, PROAXE, PSEUIL,  
     &         METHOD, NCYCL,VMIN,VMAX, OMIN, OMAX)  
     
         ELSE
            DGAM2 = 1.0D0*(PI/180.0D0)
            DPHI2 = DGAM2/SIN(GAMMAM)
            N = 0
            K = 2
            DIM = 27
            IDEB = 1
            IFIN = 3
            DO 440 J=1, 3
               GAMMA = GAMMAM + (J-K)*DGAM2
               CALL VECNUV(IDEB, IFIN, GAMMA, PHIM, DPHI2, N, K, DIM,
     &                     VECN1, VECU1, VECV1)
 440        CONTINUE

            NBVEC1 = 9
     
            CALL  AVCIPR( NBVEC1, VECN1, VECU1, VECV1,
     &         NBORDR, KWORK, SOMNOW, VWORK, TDISP, TSPAQ,  
     &         I, NOMCRI, FORDEF, FATSOC, PROAXE, PSEUIL,  
     &         METHOD, NCYCL,VMIN,VMAX, OMIN, OMAX)    
         ENDIF
         
C REMPACER PAR SUBROUTINE AVGRDO          
 
         CALL AVGRDO(NBVEC1, NBORDR, VECN1, VWORK, TDISP, KWORK,
     &             SOMNOW, TSPAQ, I, NOMMAT, NOMCRI, 
     &             NOMFOR,GRDVIE, FORVIE, FORDEF, VALA,   
     &             COEFPA,NCYCL,VMIN,VMAX,OMIN,OMAX, CUDOMX, VNORMX )
     
C  VECTEUR NORMAL ASSOCIE AUX PLAN CRITIQUE  TROUVE  
     
         NXM = VECN1((VNORMX-1)*3+1)
         NYM = VECN1((VNORMX-1)*3+2)
         NZM = VECN1((VNORMX-1)*3+3)

C
      END

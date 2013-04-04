      SUBROUTINE AVPLCR(NBVEC, VECTN, VECTU, VECTV, NBORDR, KWORK,
     &                   SOMNOW, VWORK, TDISP, TSPAQ, I, NOMCRI,
     &                   NOMFOR,GRDVIE, FORVIE,FORDEF,FATSOC,PROAXE,
     &                   NOMMAT,VALA,COEFPA,POST,CUDOMX, NXM, NYM, NZM)
      IMPLICIT      NONE
      INCLUDE 'jeveux.h'
      INTEGER       NBORDR, KWORK, I, NBVEC
      INTEGER       SOMNOW, TDISP, TSPAQ
      LOGICAL       FORDEF, POST
      REAL*8        VECTN(3*NBVEC), VECTU(3*NBVEC), VECTV(3*NBVEC)
      REAL*8        VWORK(TDISP), FATSOC
      CHARACTER*16  NOMCRI,PROAXE,NOMFOR,FORVIE
      CHARACTER*8   NOMMAT,GRDVIE
      REAL*8        VALA, COEFPA
      REAL*8        CUDOMX, NXM(2), NYM(2), NZM(2)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 02/04/2013   AUTEUR TRAN V-X.TRAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRS_1404 CRP_21 CRP_20
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
      INTEGER       NCYCL(NBVEC), NBVEC1,NVAL, IBID, DIR
      INTEGER       OMIN(NBVEC*(NBORDR+2)), OMAX(NBVEC*(NBORDR+2))
      INTEGER       VNORMX(2), IDEB,IFIN,N, K, DIM, J, KP, NBP
      INTEGER       IARG, NBPLAN,VNORM(2)
      REAL*8        VMIN(NBVEC*(NBORDR+2)), VMAX(NBVEC*(NBORDR+2))
      REAL*8        PSEUIL, GAMMAM, PHIM, DPHI2, EPSILO,GAMMA      
      REAL*8        VECN2(3*NBVEC), VECU2(3*NBVEC), VECV2(3*NBVEC)
      REAL*8        VECN1(3*NBVEC), VECU1(3*NBVEC), VECV1(3*NBVEC)
      REAL*8        DGAM2, PI, R8PI, PHI0, CUDOM1, CUDOM2
      REAL*8        PREC, R8PREM
      CHARACTER*8   METHOD
C     --------------------------
      EPSILO = 1.0D-7
      PI = R8PI()
      
      PREC=100.D0*R8PREM()
      
      NBVEC1 = 209

      METHOD = 'RAINFLOW'

      CALL GETVR8(' ','DELTA_OSCI',1,IARG,1,PSEUIL,NVAL)

      CALL  AVCIPR( NBVEC1, VECTN, VECTU, VECTV,
     &      NBORDR, KWORK, SOMNOW, VWORK, TDISP, TSPAQ,
     &      I, NOMCRI, NOMFOR,FORDEF, FATSOC, PROAXE, PSEUIL,
     &      METHOD,NCYCL,VMIN,VMAX, OMIN, OMAX)

C REMPACER PAR SUBROUTINE AVGRDO

         CALL AVGRDO(NBVEC1, NBORDR, VECTN, VWORK, TDISP, KWORK,
     &             SOMNOW, TSPAQ, I, NOMMAT, NOMCRI,
     &             NOMFOR,GRDVIE, FORVIE,VALA,COEFPA,NCYCL,
     &       VMIN,VMAX, OMIN,OMAX,POST,CUDOMX,VNORM, NBPLAN)


C 9. PREMIER RAFFINEMENT CONCERNANT LA DETERMINATION DU VECTEUR NORMAL
C    CORRESPONDANT AU MAX DES CUMULS DE DOMMAGE.

      IF ((POST) .AND. (NBPLAN .GT. 2)) THEN
         WRITE(6,*) 'IL EXISTE  PLUS DE 2 PLANS DU MAX DOMMAGE'
      ENDIF
      
C      IF (NBPLAN .EQ. 2) THEN
      
      DO 901 KP = 1, 2
         NXM(KP) = VECTN((VNORM(KP)-1)*3+1)
         NYM(KP) = VECTN((VNORM(KP)-1)*3+2)
         NZM(KP) = VECTN((VNORM(KP)-1)*3+3)
901   CONTINUE 

      DO 900 KP = 1, 2
         
         NXM(KP) = VECTN((VNORM(KP)-1)*3 + 1)
         NYM(KP) = VECTN((VNORM(KP)-1)*3 + 2)
         NZM(KP) = VECTN((VNORM(KP)-1)*3 + 3)        
      
         GAMMAM = ATAN2(SQRT(ABS(1.0D0-NZM(KP)**2)),NZM(KP))
         IF (GAMMAM .LT. 0.0D0) THEN
            GAMMAM = GAMMAM + PI
         ENDIF

         IF ((ABS(NYM(KP)) .LT. EPSILO) .AND.
     &       (ABS(NXM(KP)) .LT. EPSILO)) THEN
            PHIM = 0.0D0
         ELSE
            PHIM = ATAN2(ABS(NYM(KP)),NXM(KP))
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
     &               VECN2, VECU2, VECV2)
            GAMMA = 0.0D0
            PHI0 = PI
            IDEB = 1
            IFIN = 1
            K = 1

            CALL VECNUV(IDEB, IFIN, GAMMA, PHI0, DPHI2, N, K, DIM,
     &            VECN2, VECU2, VECV2)

C 9.1 PROJECTION DE L'HISTORIQUE DU CISAILLEMENT SUR UN PLAN

            NBVEC1 = 7

            CALL  AVCIPR( NBVEC1, VECN2, VECU2, VECV2,
     &      NBORDR, KWORK, SOMNOW, VWORK, TDISP, TSPAQ,
     &      I, NOMCRI, NOMFOR,FORDEF, FATSOC, PROAXE, PSEUIL,
     &      METHOD, NCYCL,VMIN,VMAX, OMIN, OMAX)


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
               CALL VECNUV(IDEB, IFIN, GAMMA, PHIM, DPHI2, N, K, 
     &                   DIM,  VECN2, VECU2, VECV2)
 430        CONTINUE

            NBVEC1 = 9

            CALL  AVCIPR( NBVEC1, VECN2, VECU2, VECV2,
     &         NBORDR, KWORK, SOMNOW, VWORK, TDISP, TSPAQ,
     &         I, NOMCRI, NOMFOR, FORDEF, FATSOC, PROAXE, PSEUIL,
     &         METHOD, NCYCL,VMIN,VMAX, OMIN, OMAX )

         ENDIF

C REMPACER PAR SUBROUTINE AVGRDO

         CALL AVGRDO(NBVEC1, NBORDR, VECN2, VWORK, TDISP, KWORK,
     &          SOMNOW, TSPAQ, I, NOMMAT, NOMCRI,
     &          NOMFOR,GRDVIE, FORVIE,VALA, COEFPA,NCYCL,
     &   VMIN,VMAX, OMIN,OMAX,POST,CUDOMX, VNORMX, IBID )



C 10. SECOND RAFFINEMENT CONCERNANT LA DETERMINATION DU VECTEUR NORMAL
C     CORRESPONDANT AU MAX DES CUMULS DE DOMMAGE.
C        C
         NXM(KP) = VECN2((VNORMX(KP)-1)*3+1)
         NYM(KP) = VECN2((VNORMX(KP)-1)*3+2)
         NZM(KP) = VECN2((VNORMX(KP)-1)*3+3)

         GAMMAM = ATAN2(SQRT(ABS(1.0D0-NZM(KP)**2)),NZM(KP))
         IF (GAMMAM .LT. 0.0D0) THEN
            GAMMAM = GAMMAM + PI
         ENDIF

         IF ((ABS(NYM(KP)) .LT. EPSILO) .AND.
     &       (ABS(NXM(KP)) .LT. EPSILO)) THEN
           PHIM = 0.0D0
         ELSE
           PHIM = ATAN2(ABS(NYM(KP)),NXM(KP))
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
     &         I, NOMCRI, NOMFOR, FORDEF, FATSOC, PROAXE, PSEUIL,
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
     &         I, NOMCRI,NOMFOR, FORDEF, FATSOC, PROAXE, PSEUIL,
     &         METHOD, NCYCL,VMIN,VMAX, OMIN, OMAX)
         ENDIF

C REMPACER PAR SUBROUTINE AVGRDO

         CALL AVGRDO(NBVEC1, NBORDR, VECN1, VWORK, TDISP, KWORK,
     &             SOMNOW, TSPAQ, I, NOMMAT, NOMCRI,
     &             NOMFOR,GRDVIE, FORVIE,VALA, COEFPA,NCYCL,
     &      VMIN,VMAX,OMIN,OMAX,POST,CUDOMX, VNORMX, IBID )


C 11. 3E RAFFINEMENT CONCERNANT LA DETERMINATION DU VECTEUR NORMAL
C     CORRESPONDANT AU MAX DES CUMULS DE DOMMAGE.
C        C 
         NXM(KP) = VECN1((VNORMX(KP)-1)*3+1)
         NYM(KP) = VECN1((VNORMX(KP)-1)*3+2)
         NZM(KP) = VECN1((VNORMX(KP)-1)*3+3)

         GAMMAM = ATAN2(SQRT(ABS(1.0D0-NZM(KP)**2)),NZM(KP))
         IF (GAMMAM .LT. 0.0D0) THEN
            GAMMAM = GAMMAM + PI
         ENDIF

         IF ((ABS(NYM(KP)) .LT. EPSILO) .AND.
     &       (ABS(NXM(KP)) .LT. EPSILO)) THEN
           PHIM = 0.0D0
         ELSE
           PHIM = ATAN2(ABS(NYM(KP)),NXM(KP))
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

C 11.1 PROJECTION DE L'HISTORIQUE DU CISAILLEMENT SUR UN PLAN

            NBVEC1 = 7


            CALL  AVCIPR( NBVEC1, VECN2, VECU2, VECV2,
     &         NBORDR, KWORK, SOMNOW, VWORK, TDISP, TSPAQ,
     &         I, NOMCRI, NOMFOR, FORDEF, FATSOC, PROAXE, PSEUIL,
     &         METHOD, NCYCL,VMIN,VMAX, OMIN, OMAX)

         ELSE
            DGAM2 = 0.5D0*(PI/180.0D0)
            DPHI2 = DGAM2/SIN(GAMMAM)
            N = 0
            K = 2
            DIM = 27
            IDEB = 1
            IFIN = 3
            DO 460 J=1, 3
               GAMMA = GAMMAM + (J-K)*DGAM2
               CALL VECNUV(IDEB, IFIN, GAMMA, PHIM, DPHI2, N, K, DIM,
     &                     VECN2, VECU2, VECV2)
 460        CONTINUE

            NBVEC1 = 9

            CALL  AVCIPR( NBVEC1, VECN2, VECU2, VECV2,
     &         NBORDR, KWORK, SOMNOW, VWORK, TDISP, TSPAQ,
     &         I, NOMCRI,NOMFOR, FORDEF, FATSOC, PROAXE, PSEUIL,
     &         METHOD, NCYCL,VMIN,VMAX, OMIN, OMAX)
         ENDIF

C REMPACER PAR SUBROUTINE AVGRDO

         CALL AVGRDO(NBVEC1, NBORDR, VECN2, VWORK, TDISP, KWORK,
     &             SOMNOW, TSPAQ, I, NOMMAT, NOMCRI,
     &             NOMFOR,GRDVIE, FORVIE,VALA,COEFPA, NCYCL,
     &      VMIN,VMAX,OMIN,OMAX,POST,CUDOMX, VNORMX, IBID )

C 12. 4E RAFFINEMENT CONCERNANT LA DETERMINATION DU VECTEUR NORMAL
C     CORRESPONDANT AU MAX DES CUMULS DE DOMMAGE.
C        C
         NXM(KP) = VECN2((VNORMX(KP)-1)*3+1)
         NYM(KP) = VECN2((VNORMX(KP)-1)*3+2)
         NZM(KP) = VECN2((VNORMX(KP)-1)*3+3)

         GAMMAM = ATAN2(SQRT(ABS(1.0D0-NZM(KP)**2)),NZM(KP))
         IF (GAMMAM .LT. 0.0D0) THEN
            GAMMAM = GAMMAM + PI
         ENDIF

         IF ((ABS(NYM(KP)) .LT. EPSILO) .AND.
     &       (ABS(NXM(KP)) .LT. EPSILO)) THEN
           PHIM = 0.0D0
         ELSE
           PHIM = ATAN2(ABS(NYM(KP)),NXM(KP))
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

C 12.1 PROJECTION DE L'HISTORIQUE DU CISAILLEMENT SUR UN PLAN

            NBVEC1 = 7


            CALL  AVCIPR( NBVEC1, VECN1, VECU1, VECV1,
     &         NBORDR, KWORK, SOMNOW, VWORK, TDISP, TSPAQ,
     &         I, NOMCRI, NOMFOR, FORDEF, FATSOC, PROAXE, PSEUIL,
     &         METHOD, NCYCL,VMIN,VMAX, OMIN, OMAX)

         ELSE
            DGAM2 = 0.25D0*(PI/180.0D0)
            DPHI2 = DGAM2/SIN(GAMMAM)
            N = 0
            K = 2
            DIM = 27
            IDEB = 1
            IFIN = 3
            DO 480 J=1, 3
               GAMMA = GAMMAM + (J-K)*DGAM2
               CALL VECNUV(IDEB, IFIN, GAMMA, PHIM, DPHI2, N, K, DIM,
     &                     VECN1, VECU1, VECV1)
 480        CONTINUE

            NBVEC1 = 9

            CALL  AVCIPR( NBVEC1, VECN1, VECU1, VECV1,
     &         NBORDR, KWORK, SOMNOW, VWORK, TDISP, TSPAQ,
     &         I, NOMCRI,NOMFOR, FORDEF, FATSOC, PROAXE, PSEUIL,
     &         METHOD, NCYCL,VMIN,VMAX, OMIN, OMAX)
         ENDIF

C REMPACER PAR SUBROUTINE AVGRDO

         CALL AVGRDO(NBVEC1, NBORDR, VECN1, VWORK, TDISP, KWORK,
     &             SOMNOW, TSPAQ, I, NOMMAT, NOMCRI,
     &             NOMFOR,GRDVIE, FORVIE,VALA, COEFPA,NCYCL,
     &      VMIN,VMAX,OMIN,OMAX,POST,CUDOMX, VNORMX, NBP )
C  VECTEUR NORMAL ASSOCIE AUX PLAN CRITIQUE  TROUVE

         NXM(KP) = VECN1((VNORMX(KP)-1)*3+1)
         NYM(KP) = VECN1((VNORMX(KP)-1)*3+2)
         NZM(KP) = VECN1((VNORMX(KP)-1)*3+3)
         
         IF (KP .EQ. 1) CUDOM1 = CUDOMX
         IF (KP .EQ. 2) CUDOM2 = CUDOMX
C
900   CONTINUE

C      ENDIF
      IF ( ABS(CUDOM1-CUDOM2) .LT. PREC ) THEN      
         IF ((POST) .AND. (NBPLAN .EQ. 2)) THEN
            WRITE(6,*) 'IL EXISTE  2 PLANS DU DOMMAGE MAXIMUM'
         ENDIF

      ENDIF

      IF ( (CUDOM1-CUDOM2) .GT. PREC )  THEN      
         IF ((POST) .AND. (NBPLAN .EQ. 2)) THEN
            WRITE(6,*) 'IL EXISTE  1 PLAN DU DOMMAGE MAXIMUM'
         ENDIF
         
         NXM(2) = NXM(1)
         NYM(2) = NYM(1)
         NZM(2) = NZM(1)
         CUDOMX = CUDOM1
      ENDIF

      IF ( (CUDOM2-CUDOM1) .GT. PREC ) THEN      
         IF ((POST) .AND. (NBPLAN .EQ. 2)) THEN
            WRITE(6,*) 'IL EXISTE  1 PLAN DU DOMMAGE MAXIMUM'
         ENDIF
         
         NXM(1) = NXM(2)
         NYM(1) = NYM(2)
         NZM(1) = NZM(2)
         CUDOMX = CUDOM2
      ENDIF
                  
      END

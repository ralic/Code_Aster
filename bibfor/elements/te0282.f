      SUBROUTINE TE0282 ( OPTION , NOMTE )
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 24/07/2012   AUTEUR PELLET J.PELLET 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C-----------------------------------------------------------------------
C
C      CALCUL DU TAUX DE RESTITUTION D'ENERGIE ELEMENTAIRE
C      BORDS ELEMENTS ISOPARAMETRIQUES 2D AVEC CHARGEMENT DE BORD
C      PRESSION-CISAILLEMENT ET FORCE REPARTIE
C
C      OPTION : 'CALC_G'    (G AVEC CHARGES REELLES)
C               'CALC_G_F'  (G AVEC CHARGES FONCTIONS)
C
C ENTREES  ---> OPTION : OPTION DE CALCUL
C          ---> NOMTE  : NOM DU TYPE ELEMENT
C
C VECTEURS DIMENSIONNES POUR  NNO = 3 , NPG = 4
C
C ----------------------------------------------------------------------
C
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      INCLUDE 'jeveux.h'
      CHARACTER*16        OPTION , NOMTE


      INTEGER NNO,NNOS,JGANO,NDIM,NPG,KP,IPOIDS,IVF,IDFDK,IGEOM,ICODE
      INTEGER IDEPL,IFORC,IPRES,ITHET,IGTHET,ITEMPS,COMPT,I,J,K
      INTEGER IPREF,IFORF
      INTEGER JDFD2,JCOOPG

      REAL*8 XG,YG,UX,UY,FX,FY,THX,THY,THE, R8PREM
      REAL*8 TCLA,TSURF,TSURP,EPSI,PRES,CISA,DIVTHE,VALPAR(3)
      REAL*8 VF,DFDE,DXDE,DYDE,DSDE,POIDS,DTHXDE,DTHYDE
      REAL*8 DFXDE,DFYDE,PRESNO,CISANO,FXNO,FYNO
C                               2*NNO     2*NNO
      REAL*8 PRESG(2),FORCG(2),PRESN(6),FORCN(6)
      REAL*8 PROD,
     &       DSDE2
      REAL*8 TSOM

      CHARACTER*2 CHELEM
      CHARACTER*8 NOMPAR(3),ELREFE

      LOGICAL FONC,CHARGN

C =====================================================================
C INITIALISATIONS
C =====================================================================
      CALL ELREF1(ELREFE)
      CALL JEMARQ()
      EPSI = R8PREM()
      CHELEM = NOMTE(3:4)

C RECUPERATION DES DONNEES GEOMETRIQUES LIEES AU CALCUL ELEMENTAIRE
      CALL ELREF5(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,JCOOPG,IVF,IDFDK,
     &            JDFD2,JGANO)


C INIT. POUR LE CALCUL DE G
      CHARGN = .FALSE.
      TCLA   = 0.D0
      TSURF   = 0.D0
      TSURP   = 0.D0
      CALL JEVECH ( 'PTHETAR', 'L', ITHET )
      CALL JEVECH ( 'PGTHETA', 'E', IGTHET )

C TEST SUR LA NULLITE DE THETA_FISSURE
      COMPT = 0
      DO 250 I=1,NNO
         THX = ZR(ITHET + 2*(I - 1) )
         THY = ZR(ITHET + 2*(I - 1) + 1 )
         IF((ABS(THX).LT.EPSI).AND.(ABS(THY).LT.EPSI)) THEN
            COMPT = COMPT + 1
         ENDIF
250   CONTINUE
      IF (COMPT.EQ.NNO)  GOTO 9999

C =====================================================================
C RECUPERATION DES CHAMPS LOCAUX
C =====================================================================

      CALL JEVECH ( 'PGEOMER', 'L', IGEOM )
      CALL JEVECH ( 'PDEPLAR', 'L', IDEPL )
      IF (OPTION.EQ.'CALC_G_F') THEN
         FONC = .TRUE.
         CALL JEVECH ( 'PFF1D2D', 'L', IFORF  )
         CALL JEVECH ( 'PPRESSF', 'L', IPREF  )
         CALL JEVECH ( 'PTEMPSR', 'L', ITEMPS )
         NOMPAR(1) = 'X'
         NOMPAR(2) = 'Y'
         NOMPAR(3) = 'INST'
         VALPAR(3) = ZR(ITEMPS)
      ELSE
         FONC =.FALSE.
         CALL JEVECH ( 'PFR1D2D', 'L', IFORC )
         CALL JEVECH ( 'PPRESSR', 'L', IPRES )
      ENDIF

C =====================================================================
C - SI CHARGE FONCTION RECUPERATION DES VALEURS AUX PG ET NOEUDS
C =====================================================================

      IF ( FONC ) THEN
         DO 70 I = 1 , NNO
            DO 80 J = 1 , 2
               VALPAR(J) = ZR(IGEOM+2*(I-1)+J-1)
 80         CONTINUE
            DO 75 J=1,2
               CALL FOINTE ('FM', ZK8(IPREF+J-1), 3,NOMPAR,VALPAR,
     &                                       PRESN(2*(I-1)+J), ICODE)
               CALL FOINTE ('FM', ZK8(IFORF+J-1), 3,NOMPAR,VALPAR,
     &                                       FORCN(2*(I-1)+J), ICODE)
 75         CONTINUE
 70      CONTINUE
      ENDIF

C ======================================================================
C BOUCLE PRINCIPALE SUR LES POINTS DE GAUSS
C ======================================================================

      DO 800 KP = 1 , NPG

C INITIALISATIONS
         K    = (KP-1)*NNO
         DXDE = 0.D0
         DYDE = 0.D0
         XG   = 0.D0
         YG   = 0.D0
         UX   = 0.D0
         UY   = 0.D0
         THX  = 0.D0
         THY  = 0.D0
         DFXDE = 0.D0
         DFYDE = 0.D0
         DTHXDE = 0.D0
         DTHYDE = 0.D0
         FX = 0.D0
         FY = 0.D0

C ===========================================
C CALCUL DES ELEMENTS GEOMETRIQUES
C ===========================================

C CALCUL DES DERIVEES PARTIELLES PREMIERES DU VECTEURS
C POSITIONS (DXDE,DYDE) AU POINT DE GAUSS,
C DU VECTEUR POSITION AU POINT DE GAUSS (XG,YG), DE SON VECTEUR
C DEPLACEMENT (UX,UY), DU CHAMP THETA FISSURE (THX,THY) ET DE SON
C GRADIENT (DTHXDE,DTHYDE).
         DO 10 I = 1 , NNO
            VF  = ZR(IVF  +K+I-1)
            DFDE = ZR(IDFDK+K+I-1)
            DXDE = DXDE    +   DFDE*ZR(IGEOM+2*(I-1))
            DYDE = DYDE    +   DFDE*ZR(IGEOM+2*(I-1)+1)
            XG  = XG      +   VF  *ZR(IGEOM+2*(I-1)  )
            YG  = YG      +   VF  *ZR(IGEOM+2*(I-1)+1)
            UX  = UX      +   VF  *ZR(IDEPL+2*(I-1)  )
            UY  = UY      +   VF  *ZR(IDEPL+2*(I-1)+1)
            THX = THX     +   VF  *ZR(ITHET+2*(I-1)  )
            THY = THY     +   VF  *ZR(ITHET+2*(I-1)+1)
            DTHXDE = DTHXDE + DFDE*ZR(ITHET+2*(I-1)  )
            DTHYDE = DTHYDE + DFDE*ZR(ITHET+2*(I-1)+1)
   10    CONTINUE

C ===========================================
C CALCUL DU CHARGEMENT ET DE SON GRADIENT
C ===========================================

         IF ( FONC ) THEN
            VALPAR(1) = XG
            VALPAR(2) = YG
            DO 65 J = 1 , 2
               CALL FOINTE ('FM', ZK8(IPREF+J-1), 3,NOMPAR,VALPAR,
     &                                       PRESG(J), ICODE)
               CALL FOINTE ('FM', ZK8(IFORF+J-1), 3,NOMPAR,VALPAR,
     &                                       FORCG(J), ICODE)
65          CONTINUE
         ELSE
            PRESG(1) = 0.D0
            PRESG(2) = 0.D0
            FORCG(1) = 0.D0
            FORCG(2) = 0.D0
            DO 4 I = 1 , NNO
              DO 6 J = 1 , 2
                 PRESG(J) = PRESG(J) +
     &                              ZR(IPRES+2*(I-1)+J-1)*ZR(IVF+K+I-1)
                 FORCG(J) = FORCG(J) +
     &                              ZR(IFORC+2*(I-1)+J-1)*ZR(IVF+K+I-1)
 6            CONTINUE
 4         CONTINUE
         ENDIF

C VALEURS DU CHARGEMENT AUX POINTS DE GAUSS (FX,FY)
         DSDE = SQRT(DXDE*DXDE+DYDE*DYDE)
         DSDE2 = DSDE*DSDE
         PRES = PRESG(1)
         CISA = PRESG(2)
         FX   = FORCG(1)-(DYDE*PRES-DXDE*CISA)/DSDE
         FY   = FORCG(2)+(DXDE*PRES+DYDE*CISA)/DSDE

C VALEURS DU CHARGEMENT AUX NOEUDS (FXNO,FYNO) ET DE SES DERIVEES
C AUX POINTS DE GAUSS (DFXDE,DFYDE,D2FXDE,D2FYDE)
         IF ( FONC ) THEN
           DO 300 I = 1,NNO
             DFDE   = ZR(IDFDK+K+I-1)
             PRESNO = PRESN(2*(I-1)+1)
             CISANO = PRESN(2*(I-1)+2)
             FXNO   = FORCN(2*(I-1)+1)-(DYDE*PRESNO-DXDE*CISANO)/DSDE
             FYNO   = FORCN(2*(I-1)+2)+(DXDE*PRESNO+DYDE*CISANO)/DSDE
             DFXDE  = DFXDE + DFDE*FXNO
             DFYDE  = DFYDE + DFDE*FYNO
300        CONTINUE
         ENDIF

C TESTS SUR LA NULLITE DES CHARGEMENTS ET DE LEURS GRADIENTS POUR EVITER
C DE FAIRE DES CALCULS INUTILES ET DETECTER LES VRAIS PROBLEMES
         IF ((FX.EQ.0.D0).AND.(FY.EQ.0.D0).AND.(DFXDE.EQ.0.D0).AND.
     &       (DFYDE.EQ.0.D0)) THEN
           CHARGN = .TRUE.
         ENDIF

C CAS PARTICULIER D'UN CALCUL SUR L'AXE
         IF (XG.EQ.0.D0) THEN

C ON EST SUR L'AXE AVEC CHARGEMENTS NULS DONC G (ET DG) = 0
           IF (CHARGN) THEN
             GOTO 799
           ELSEIF ( CHELEM .EQ. 'AX' )THEN
             CALL U2MESS('F','RUPTURE1_23')
           ENDIF
         ELSE

C CAS GENERAL AVEC CHARGEMENTS NULS DONC G (ET DG) = 0
           IF (CHARGN) GOTO 799
         ENDIF

C CALCUL DU TERME ELEMENTAIRE
         IF ( CHELEM .EQ. 'AX' ) THEN
           POIDS  = ZR(IPOIDS+KP-1)*DSDE*XG
         ELSE
           POIDS  = ZR(IPOIDS+KP-1)*DSDE
         ENDIF
         THE    = (THX*DXDE+THY*DYDE)/DSDE2
         DIVTHE = (DTHXDE*DXDE+DTHYDE*DYDE)/DSDE2

C =======================================================
C PRISE EN COMPTE DE LA MODELISATION POUR G
C =======================================================

         IF ( CHELEM .EQ. 'AX' ) DIVTHE = DIVTHE+(THX/XG)

C =======================================================
C CALCUL DU TAUX DE RESTITUTION G
C =======================================================
C
         PROD = (DIVTHE*FX+DFXDE*THE)*UX + (DIVTHE*FY+DFYDE*THE)*UY
C
         TCLA = TCLA + PROD*POIDS

C BRANCHEMENT POUR F=0 ET DF=0
  799    CONTINUE

C ======================================================================
C FIN DE BOUCLE PRINCIPALE SUR LES POINTS DE GAUSS
C ======================================================================
  800 CONTINUE

C EXIT EN CAS DE THETA FISSURE NUL PARTOUT
9999  CONTINUE

C ASSEMBLAGE FINAL DES TERMES DE G
       TSOM = TCLA + TSURF + TSURP
       ZR(IGTHET) = TSOM

      CALL JEDEMA()
      END

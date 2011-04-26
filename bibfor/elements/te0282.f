      SUBROUTINE TE0282 ( OPTION , NOMTE )
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C-----------------------------------------------------------------------
C
C      CALCUL DU TAUX DE RESTITUTION D'ENERGIE ELEMENTAIRE
C      BORDS ELEMENTS ISOPARAMETRIQUES 2D AVEC CHARGEMENT DE BORD
C      PRESSION-CISAILLEMENT ET FORCE REPARTIE
C
C      CALCUL DE SA DERIVEE PAR RAPPORT A UNE VARIATION DE DOMAINE (EN
C      ELASTIQUE ISOTROPE LINEAIRE PETIT OU GRAND DEPLACEMENTS).
C      LES CHARGEMENTS SONT SUPPOSES INDEPENDANTS DE LA VARIATION DE
C      DOMAINE:
C      DERIVEE LAGRANGIENNE(F) = GRADIENT(F) * THETA SENSIBILITE
C      CAR LA DERIVEE PARTIELLE DF/DN = 0, EN NOTANT N LE PARAMETRE
C      PILOTANT LA VARIATION (M = P + N * THETASENSIBILITE(P)).
C
C      OPTION : 'CALC_G'    (G AVEC CHARGES REELLES)
C               'CALC_G_F'  (G AVEC CHARGES FONCTIONS)
C               'CALC_DG'   (G + DG AVEC CHARGES REELLES)
C               'CALC_DG_F' (G + DG AVEC CHARGES FONCTIONS)
C               'CALC_DG_E'   ( DG/DE AVEC CHARGES REELLES)
C               'CALC_DG_E_F' ( DG/DE AVEC CHARGES FONCTIONS)
C               'CALC_DG_FORC'   ( DG/DF AVEC CHARGES REELLES)
C               'CALC_DG_FORC_F' ( DG/DF AVEC CHARGES FONCTIONS)
C
C ENTREES  ---> OPTION : OPTION DE CALCUL
C          ---> NOMTE  : NOM DU TYPE ELEMENT
C
C VECTEURS DIMENSIONNES POUR  NNO = 3 , NPG = 4
C   -------------------------------------------------------------------
C     SUBROUTINES APPELLEES:
C       JEVEUX AND CO: JEMARQ, JEDEMA, JEVETE, JEVECH.
C       ENVIMA: R8PREM.
C       DIVERS: FOINTE.
C
C     FONCTIONS INTRINSEQUES:
C       SQRT.
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       11/12/00 (OB): TOILETTAGE FORTRAN,
C                      DEPLACEMENT DU TEST DE LA NULLITE DU THETA_FISS,
C                      REFORMULATION DU CALCUL VIS A VIS DU POIDS,
C                      TOILETTAGE FORTRAN, COMMENTAIRES.
C                      MISE EN PLACE DE LA DERIVEE DE G.
C       28/02/01 (OB): PRISE EN COMPTE DE CHARGEMENTS IDENTIQUEMENT
C                      NULS (INITIALISES A ZERO OU NON). EN PARTICULIER
C                      SUR L'AXE (XG=0) (AL2001-060).
C      04/07/03 (GN): MISE EN PLACE DU MECANISME DES SENSIBILITES
C-----------------------------------------------------------------------
C TOLE CRP_20
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      CHARACTER*16        OPTION , NOMTE

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

      INTEGER NNO,NNOS,JGANO,NDIM,NPG,KP,IPOIDS,IVF,IDFDK,IGEOM,ICODE
      INTEGER IDEPL,IFORC,IPRES,ITHET,IGTHET,ITEMPS,COMPT,I,J,K
      INTEGER IPREF,IFORF,IPRESS,IPRESF,IFO12R
      INTEGER ITHETA,IDEPSE,IDEB,IFIN,JDFD2,JCOOPG,IFO12F,KK

      REAL*8 XG,YG,UX,UY,FX,FY,THX,THY,THE, R8PREM
      REAL*8 TCLA,TSURF,TSURP,EPSI,PRES,CISA,DIVTHE,VALPAR(3)
      REAL*8 VF,DFDE,DXDE,DYDE,DSDE,POIDS,DTHXDE,DTHYDE
      REAL*8 DFXDE,DFYDE,PRESNO,CISANO,FXNO,FYNO,FLAGX,FLAGY
C                               2*NNO     2*NNO
      REAL*8 PRESG(2),FORCG(2),PRESN(6),FORCN(6),DGNO(6),DGNOP(6)
      REAL*8 DTCLA,DFD2DE,DLUX,DLUY,THSX,THSY,THES,PROD,DIVTS,
     &       DLDIVT,DLFXDE,DLFYDE,PROD1,PROD2,THSXDE,
     &       THSYDE,D2FXDE,D2FYDE,DLFXDK,DLFYDK,DSDE2,DSDE4
      REAL*8 DUXDE,DUYDE,FLAGP,FLAGC,TSOM

      CHARACTER*2 CHELEM
      CHARACTER*8 NOMPAR(3),ELREFE

      LOGICAL FONC,DERIVL,TSENUL,CHARGN,DERIVE,DERFOR,DFORC,DPRES

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
      DERFOR = .FALSE.
      DFORC = .FALSE.
      DPRES = .FALSE.
      DERIVL = .FALSE.
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

C INIT. COMPLEMENTAIRES POUR LA DERIVATION DE G
      IF ((OPTION.EQ.'CALC_DG').OR.(OPTION.EQ.'CALC_DG_F')) THEN
        DERIVL = .TRUE.
        DTCLA  = 0.D0

      ENDIF


C =====================================================================
C RECUPERATION DES CHAMPS LOCAUX
C =====================================================================

      CALL JEVECH ( 'PGEOMER', 'L', IGEOM )
      CALL JEVECH ( 'PDEPLAR', 'L', IDEPL )
      IF ((OPTION.EQ.'CALC_G_F').OR.(OPTION.EQ.'CALC_DG_F')
     &      .OR.(OPTION.EQ.'CALC_DG_FORC_F')
     &      .OR.(OPTION.EQ.'CALC_DG_E_F')) THEN
         FONC = .TRUE.
         CALL JEVECH ( 'PFF1D2D', 'L', IFORF  )
         CALL JEVECH ( 'PPRESSF', 'L', IPREF  )
         CALL JEVECH ( 'PTEMPSR', 'L', ITEMPS )
         NOMPAR(1) = 'X'
         NOMPAR(2) = 'Y'
         NOMPAR(3) = 'INST'
         VALPAR(3) = ZR(ITEMPS)
        IF (OPTION.EQ.'CALC_DG_FORC_F') THEN
          CALL JEVECH('PFF12SS','L',IFO12F)
          CALL JEVECH('PPRESSSF','L',IPRESF)
          DERFOR = .TRUE.
        ENDIF
      ELSE
         FONC =.FALSE.
         CALL JEVECH ( 'PFR1D2D', 'L', IFORC )
         CALL JEVECH ( 'PPRESSR', 'L', IPRES )
         IF (OPTION.EQ.'CALC_DG_FORC') THEN
           CALL JEVECH('PFR12SS','L',IFO12R)
           CALL JEVECH('PPRESSSR','L',IPRESS)
           DERFOR = .TRUE.
         ENDIF
      ENDIF

C RECUPERATION DES CHAMPS LOCAUX (CARTE) ASSOCIES AU CALCUL DE LA
C DERIV.: PVECTTH (THETA SENSIBILITE), PDEPLSE (DERIV. DEPLACEMENT)
      IF (DERIVL) THEN
        CALL JEVECH('PVECTTH','L',ITHETA)
        CALL JEVECH('PDEPLSE','L',IDEPSE)

C TEST DE LA NULLITE DU THETA SENSIBILITE
        IDEB = ITHETA
        IFIN = ITHETA + 2*NNO - 1
        TSENUL = .TRUE.
        DO 11 I = IDEB , IFIN
          IF (ABS(ZR(I)).GT.EPSI) THEN
            TSENUL = .FALSE.
          ENDIF
11      CONTINUE
      ENDIF
C RECUPERATION DE LA DERIVEE DU DEPLACEMENT PAR RAPPORT A E OU F
      DERIVE = .FALSE.
      IF (OPTION(6:9).EQ.'DG_E'.OR.OPTION(6:12).EQ.'DG_FORC') THEN
        DERIVE = .TRUE.
        CALL JEVECH('PDEPLSE','L',IDEPSE)
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
          IF(DERFOR) THEN
             DO 76 J=1,2
              KK = 2*(I-1)+J
              CALL FOINTE('FM',ZK8(IFO12F+J-1),3,NOMPAR,VALPAR,DGNO(KK)
     &                    ,ICODE)
              IF(DGNO(KK).NE.0.0D0) DFORC=.TRUE.
              CALL FOINTE('FM',ZK8(IPRESF+J-1),3,NOMPAR,VALPAR,DGNOP(KK)
     &                    ,ICODE)
              IF(DGNOP(KK).NE.0.0D0) DPRES=.TRUE.
76           CONTINUE
           ENDIF
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
         FLAGX = 0.D0
         FLAGY = 0.D0
         FLAGP = 0.D0
         FLAGC = 0.D0
         DLUX = 0.D0
         DLUY = 0.D0
         THX  = 0.D0
         THY  = 0.D0
         DFXDE = 0.D0
         DFYDE = 0.D0
         DTHXDE = 0.D0
         DTHYDE = 0.D0
         FX = 0.D0
         FY = 0.D0
         IF (DERIVL) THEN
           DLUX = 0.D0
           DLUY = 0.D0
           THSX = 0.D0
           THSY = 0.D0
           THSXDE = 0.D0
           THSYDE = 0.D0
           D2FXDE = 0.D0
           D2FYDE = 0.D0
         ELSE IF (DERIVE) THEN
           DUXDE = 0.D0
           DUYDE = 0.D0
         ENDIF

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
         IF(DERFOR) THEN
            DO 12 I = 1 , NNO
               VF  = ZR(IVF  +K+I-1)
               FLAGX  = FLAGX      +   VF  *DGNO(2*I-1)
               FLAGY  = FLAGY      +   VF  *DGNO(2*I)
               FLAGP  = FLAGP      +   VF  *DGNOP(2*I-1)
               FLAGC  = FLAGC      +   VF  *DGNOP(2*I)
   12       CONTINUE
         ENDIF

C ===========================================
C 1 CALCULS COMPLEMENTAIRES POUR DG
C ===========================================

C CALCUL DE LA DERIVEE DU DEPLACEMENT (DLUX,DLUY),
C DU VECTEUR THETA SENSIBILITE (THSX,THSY) ET DE SON GRADIENT
C (THSXDE, THSYDE)
         IF (DERIVL) THEN
           DO 20 I = 1 , NNO
             VF  = ZR(IVF  +K+I-1)
             DFDE = ZR(IDFDK+K+I-1)
             DLUX = DLUX + VF*ZR(IDEPSE+2*(I-1))
             DLUY = DLUY + VF*ZR(IDEPSE+2*(I-1)+1)
             THSX = THSX + VF*ZR(ITHETA+2*(I-1))
             THSY = THSY + VF*ZR(ITHETA+2*(I-1)+1)
             THSXDE = THSXDE + DFDE*ZR(ITHETA+2*(I-1))
             THSYDE = THSYDE + DFDE*ZR(ITHETA+2*(I-1)+1)
20         CONTINUE
         ELSE IF(DERIVE) THEN
           DO 21 I = 1 , NNO
             VF  = ZR(IVF  +K+I-1)
             DUXDE = DUXDE + VF*ZR(IDEPSE+2*(I-1))
             DUYDE = DUYDE + VF*ZR(IDEPSE+2*(I-1)+1)
21         CONTINUE
         ENDIF

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
         DSDE4 = DSDE2*DSDE2
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
             IF (DERIVL.AND..NOT.TSENUL) THEN
               DFD2DE = ZR(JDFD2+K+I-1)
               D2FXDE  = D2FXDE + DFD2DE*FXNO
               D2FYDE  = D2FYDE + DFD2DE*FYNO
             ENDIF
300        CONTINUE
         ENDIF

C TESTS SUR LA NULLITE DES CHARGEMENTS ET DE LEURS GRADIENTS POUR EVITER
C DE FAIRE DES CALCULS INUTILES ET DETECTER LES VRAIS PROBLEMES
         IF ((FX.EQ.0.D0).AND.(FY.EQ.0.D0).AND.(DFXDE.EQ.0.D0).AND.
     &       (DFYDE.EQ.0.D0)) THEN
           IF (DERIVL) THEN
             IF ((D2FXDE.EQ.0.D0).AND.(D2FYDE.EQ.0.D0)) CHARGN = .TRUE.
           ELSE
             CHARGN = .TRUE.
           ENDIF
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
C 2 CALCULS COMPLEMENTAIRES POUR DG
C =======================================================

C CALCUL DU THETA SENSIBILITE SUR L'ELEMENT DE REFERENCE (THES), DE SA
C DIVERGENCE SURFACIQUE (DIVTS), DE LA DERIVEE LAGRANGIENNE DE LA
C DIVERGENCE DU THETA FISSURE (DLDIVT) ET DE CELLES DU GRADIENT DES
C FORCES SURFACIQUES (DLFXDE,DLFXDK,DLFYDE,DLFYDK)
         IF (DERIVL.AND..NOT.TSENUL) THEN

           THES = (THSX*DXDE+THSY*DYDE)/DSDE2
           DIVTS = (THSXDE*DXDE+THSYDE*DYDE)/DSDE2
           DLDIVT = -(DTHXDE*DXDE+DTHYDE*DYDE)*DIVTS/DSDE2

C ==========================================
C PRISE EN COMPTE DE LA MODELISATION POUR DG
C ==========================================

           IF ( CHELEM .EQ. 'AX' ) THEN
             DIVTS = DIVTS+(THSX/XG)
             DLDIVT = DLDIVT-(THX*THSX*DSDE/(XG*XG))
           ENDIF

           DLFXDE = D2FXDE*(DXDE*THSX+DYDE*THSY)*DXDE/DSDE4
           DLFXDK = D2FXDE*(DYDE*THSY+DXDE*THSX)*DYDE/DSDE4
           DLFYDE = D2FYDE*(DXDE*THSX+DYDE*THSY)*DXDE/DSDE4
           DLFYDK = D2FYDE*(DYDE*THSY+DXDE*THSX)*DYDE/DSDE4

         ENDIF

C =======================================================
C PRISE EN COMPTE DE LA MODELISATION POUR G
C =======================================================

         IF ( CHELEM .EQ. 'AX' ) DIVTHE = DIVTHE+(THX/XG)

C =======================================================
C CALCUL DU TAUX DE RESTITUTION G
C REMARQUE : POUR LA DERIVEE, TCLA EST INUTILE.
C            MAIS ON A BESOIN DE PROD SI TSENUL EST FAUX.
C =======================================================
C
        IF( DERIVE) THEN
C
         PROD = (DIVTHE*FX+DFXDE*THE)*DUXDE +(DIVTHE*FY+DFYDE*THE)*DUYDE
C
        ELSE IF ( .NOT.DERIVL .OR. .NOT.TSENUL ) THEN
C
         PROD = (DIVTHE*FX+DFXDE*THE)*UX + (DIVTHE*FY+DFYDE*THE)*UY
C
        ELSE
         PROD = 0.D0
        ENDIF
C
         TCLA = TCLA + PROD*POIDS
C
C =======================================================
C DANS LE CAS D'UNE DERIVEE PAR RAPPORT A UN CHARGEMENT SURFACIQUE
C (NEUMANN), IL Y A UN TERME DE PLUS
        IF ( DERFOR ) THEN
           TSURF = TSURF + DIVTHE*POIDS*(UX*FLAGX+UY*FLAGY)
           TSURP = TSURP + DIVTHE*POIDS*
     &             (UX*(-DYDE/DSDE)+UY*(DXDE/DSDE))*FLAGP
     &            +(UX*( DXDE/DSDE)+UY*(DYDE/DSDE))*FLAGC
        ENDIF
C
C =======================================================
C CALCUL DE LA DERIVEE DE G PAR RAPPORT A UNE VARIATION DE DOMAINE
C =======================================================
C
        IF ( DERIVL ) THEN
C
           PROD1 = (DIVTHE*FX+DFXDE*THE)*DLUX +
     &             (DIVTHE*FY+DFYDE*THE)*DLUY
           DTCLA = DTCLA + POIDS*PROD1
           IF (.NOT.TSENUL) THEN
             PROD2 = (DLDIVT*FX  + DIVTHE*DFXDE*THES +
     &                DLFXDE*THX + DLFXDK*THY)*UX    +
     &               (DLDIVT*FY  + DIVTHE*DFYDE*THES +
     &                DLFYDE*THX + DLFYDK*THY)*UY
             DTCLA = DTCLA + POIDS*(PROD2+PROD*DIVTS)
           ENDIF
         ENDIF

C BRANCHEMENT POUR F=0 ET DF=0
  799    CONTINUE

C ======================================================================
C FIN DE BOUCLE PRINCIPALE SUR LES POINTS DE GAUSS
C ======================================================================
  800 CONTINUE

C EXIT EN CAS DE THETA FISSURE NUL PARTOUT
9999  CONTINUE

C ASSEMBLAGE FINAL DES TERMES DE G OU DG
       IF (DERIVL) THEN
         ZR(IGTHET) = DTCLA
       ELSE
C SI LE PARAMETRE SENSIBLE FIGURE A LA FOIS DANS UNE FORCE ET UNE
C PRESSION IL FAUT MULTIPLIER PAR 2 LE TERME CLASSIQUE
         IF (DFORC.AND.DPRES) TCLA = 2.0D0*TCLA
         TSOM = TCLA + TSURF + TSURP
         ZR(IGTHET) = TSOM
       ENDIF

      CALL JEDEMA()
      END

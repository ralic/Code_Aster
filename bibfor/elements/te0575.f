      SUBROUTINE TE0575(OPTION,NOMTE)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16      OPTION,NOMTE
C.......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 08/10/2002   AUTEUR JMBHH01 J.M.PROIX 
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
C
C FONCTIONS REALISEES:
C
C      CALCUL DE LA DENSITE D'ENERGIE POTENTIELLE THERMOELASTIQUE
C      A L'EQUILIBRE POUR LES ELEMENTS ISOPARAMETRIQUES 2D
C      .SOIT AUX POINTS D'INTEGRATION : OPTION 'ENEL_ELGA'
C      .SOIT AUX NOEUDS               : OPTION 'ENEL_ELNO_ELGA'
C
C      OPTIONS : 'ENEL_ELGA'
C                'ENEL_ELNO_ELGA'
C
C      CALCUL DE LA DENSITE D'ENERGIE TOTALE
C      A L'EQUILIBRE POUR LES ELEMENTS ISOPARAMETRIQUES 2D
C      .SOIT AUX POINTS D'INTEGRATION : OPTION 'ETOT_ELGA'
C      .SOIT AUX NOEUDS               : OPTION 'ETOT_ELNO_ELGA'
C
C      OPTIONS : 'ETOT_ELGA'
C                'ETOT_ELNO_ELGA'
C
C ENTREES  ---> OPTION : OPTION DE CALCUL
C          ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................
C ----- DEBUT --- COMMUNS NORMALISES  JEVEUX  --------------------------
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
C------------FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      PARAMETER          (MXCMEL = 54)
      PARAMETER          (NBNOMX = 27)
      PARAMETER          (NBCONT =  4)
      INTEGER            NBSIGM
      REAL*8             EPSI(NBCONT), EPSIM(NBCONT), DELTA(NBCONT)
      REAL*8             INSTAN, NHARM, REPERE(7)
      REAL*8             ENERPG(NBNOMX), EPSS(MXCMEL)
      REAL*8             D1(NBCONT,NBCONT), XYZGAU(3)
      REAL*8             EPSSM(MXCMEL), SIGMM(NBCONT), SIGMA(NBCONT)
      REAL*8             INTEG1, INTEG2, INTEG
      REAL*8             DFDX(9), DFDY(9)
      CHARACTER*8        ELREFE,MODELI
      CHARACTER*24       CARAC,FF
C
C
      CALL ELREF1(ELREFE)
      MODELI(1:2) = NOMTE(3:4)
C
C ---- CARACTERISTIQUES DU TYPE D'ELEMENT :
C ---- GEOMETRIE ET INTEGRATION
C      ------------------------
C
      CARAC='&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO  = ZI(ICARAC)
      NPG1 = ZI(ICARAC+2)
C
      FF   ='&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF,'L',IFF)
C
      IPOIDS = IFF
      IVF    = IPOIDS+NPG1
      IDFDE  = IVF   +NPG1*NNO
      IDFDK  = IDFDE +NPG1*NNO
      NPG    = NPG1
C
      IF(NOMTE(5:7).EQ.'TR3' .OR. NOMTE(5:7).EQ.'QU4' ) THEN
        NNOS = NNO
      ELSE IF(NOMTE(5:7).EQ.'TR6' .OR. NOMTE(5:7).EQ.'QU8' .OR.
     +        NOMTE(5:7).EQ.'QS8' .OR. NOMTE(5:7).EQ.'QU9' ) THEN
        NNOS = NNO/2
      ENDIF
C
C ---- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
C      -----------------------------------------
      NBSIG  = NBSIGM(MODELI)
C
C --- INITIALISATIONS :
C     -----------------
      ZERO        = 0.0D0
      UNDEMI      = 0.5D0
      DEUX        = 2.0D0
      INSTAN      = ZERO
      NHARM       = ZERO
      BIDON       = ZERO
      NDIM        = 2
      ENELEM      = ZERO
C
      DO 10 I = 1, NBNOMX
         ENERPG(I) = ZERO
 10   CONTINUE
C
C ---- RECUPERATION DES COORDONNEES DES CONNECTIVITES
C      ----------------------------------------------
      CALL JEVECH('PGEOMER','L',IGEOM)
C
      IF (OPTION(1:4).EQ.'ENEL') THEN
C
C ----   RECUPERATION DU MATERIAU
C        ------------------------
        CALL JEVECH('PMATERC','L',IMATE)
C
C ----   RECUPERATION  DES DONNEES RELATIVES AU REPERE D'ORTHOTROPIE
C        -----------------------------------------------------------
        CALL ORTREP(ZI(IMATE),NDIM,REPERE)
C
C ----   RECUPERATION DU CHAMP DE TEMPERATURE SUR L'ELEMENT
C        --------------------------------------------------
        CALL JEVECH('PTEMPER','L',ITEMPE)
C
C ----   RECUPERATION DU CHAMP DE CONTRAINTES AUX POINTS D'INTEGRATION
C        -------------------------------------------------------------
        CALL JEVECH('PCONTRR','L',IDCONT)
C
C ----   RECUPERATION DE L'INSTANT DE CALCUL
C        -----------------------------------
        CALL TECACH(.TRUE.,.FALSE.,'PTEMPSR',1,ITEMPS)
        IF (ITEMPS.NE.0) THEN
           INSTAN = ZR(ITEMPS)
        ENDIF
C
      ENDIF
C
C --- CAS DU CALCUL DE LA DENSITE D'ENERGIE TOTALE :
C     ============================================
      IF (OPTION(1:4).EQ.'ETOT') THEN
C
C ---   RECUPERATION DU CHAMP DE DEPLACEMENT A L'INSTANT COURANT :
C       --------------------------------------------------------
        CALL JEVECH('PDEPLR','L',IDEPL)
C
C ---   RECUPERATION EVENTUELLE DU CHAMP DE DEPLACEMENT A 
C ---   L'INSTANT PRECEDENT :
C       -------------------
        CALL TECACH(.FALSE.,.FALSE.,'PDEPLM',1,IDEPLM)
        IF (IDEPLM.NE.0) THEN
           CALL JEVECH('PDEPLM','L',IDEPMM)
        ENDIF
C
C ---   RECUPERATION DU CHAMP DE CONTRAINTES AUX POINTS D'INTEGRATION
C ---   A L'INSTANT COURANT :
C       -------------------
        CALL JEVECH('PCONTPR','L',IDSIG)
C
C ---   RECUPERATION EVENTUELLE DU CHAMP DE CONTRAINTES A 
C ---   L'INSTANT PRECEDENT :
C       -------------------
        CALL TECACH(.FALSE.,.FALSE.,'PCONTMR',1,IDSIGM)
        IF (IDSIGM.NE.0) THEN
          CALL JEVECH('PCONTMR','L',IDSIGM)
        ENDIF
C
C ---   CALCUL DU CHAMP DE DEFORMATIONS AU PREMIER ORDRE
C ---   CORRESPONDANT AU CHAMP DE DEPLACEMENT COURANT :
C       ---------------------------------------------
        CALL EPS1MC(MODELI,NNO,NDIM,NBSIG,NPG1,ZR(IVF),ZR(IDFDE),
     +              ZR(IDFDK),BIDON,ZR(IPOIDS),ZR(IGEOM),
     +              ZR(IDEPL),NHARM,EPSS)
C
C ---   CALCUL EVENTUEL DU CHAMP DE DEFORMATIONS AU PREMIER ORDRE
C ---   CORRESPONDANT AU CHAMP DE DEPLACEMENT A L'INSTANT PRECEDENT :
C       -----------------------------------------------------------
        IF (IDEPLM.NE.0) THEN
          CALL EPS1MC(MODELI,NNO,NDIM,NBSIG,NPG1,ZR(IVF),ZR(IDFDE),
     +                ZR(IDFDK),BIDON,ZR(IPOIDS),ZR(IGEOM),
     +                ZR(IDEPMM),NHARM,EPSSM)
           ENDIF
C
      ENDIF
C
C ---- BOUCLE SUR LES POINTS D'INTEGRATION :
C      ===================================
      DO 20 IGAU = 1, NPG
C
         K=(IGAU-1)*NNO
C
C  --   CALCUL DES DERIVEES DES FONCTIONS DE FORME ET DU PRODUIT
C  --   JACOBIEN*POIDS_INTEGRATION (DANS LA VARIABLE POIDS)
C  --   AU POINT D'INTEGRATION COURANT :
C       ------------------------------
C
         CALL DFDM2D (NNO,ZR(IPOIDS+IGAU-1),ZR(IDFDE+K),ZR(IDFDK+K),
     +               ZR(IGEOM),DFDX,DFDY,POIDS)
C
         IF (MODELI(1:2).EQ.'AX') THEN
             RAYON = 0.D0
             DO 41 I = 1, NNO
                RAYON = RAYON + ZR(IVF+K+I-1)*ZR(IGEOM+2*(I-1))
  41         CONTINUE
             POIDS=POIDS*RAYON
         ENDIF
         DO 30 ISIG = 1, NBSIG
           EPSI(ISIG) = ZERO
  30     CONTINUE
C
C  --      COORDONNEES ET TEMPERATURE AU POINT D'INTEGRATION
C  --      COURANT
C          -------
         XYZGAU(1) = ZERO
         XYZGAU(2) = ZERO
         XYZGAU(3) = ZERO
         TEMPG     = ZERO
C
         DO 40 I = 1, NNO
C
            DO 50 IDIM = 1, NDIM
               XYZGAU(IDIM) = XYZGAU(IDIM) +
     +                        ZR(IVF+I+NNO*(IGAU-1)-1)*
     +                        ZR(IGEOM+IDIM+NDIM*(I-1)-1)
  50         CONTINUE
C
             IF (OPTION(1:4).EQ.'ENEL') THEN
               TEMPG     = TEMPG     + ZR(IVF+I+NNO*(IGAU-1)-1)*
     +                                 ZR(ITEMPE+I-1)
             ENDIF
C
  40     CONTINUE
C
C  --    CALCUL DE LA DENSITE D'ENERGIE POTENTIELLE THERMOELASTIQUE :
C        ==========================================================
         IF (OPTION(1:4).EQ.'ENEL') THEN
C
C  --      CALCUL DE L'INVERSE DE LA MATRICE DE HOOKE (LE MATERIAU
C  --      POUVANT ETRE ISOTROPE, ISOTROPE-TRANSVERSE OU ORTHOTROPE)
C          ---------------------------------------------------------
         CALL D1MAMC(MODELI, ZI(IMATE), TEMPG, INSTAN, REPERE, XYZGAU,
     +               NBSIG, D1)
C
C  --      DENSITE D'ENERGIE POTENTIELLE ELASTIQUE AU POINT
C  --      D'INTEGRATION COURANT
C          ---------------------
         DO 60 ISIG = 1, NBSIG
            DO 70 JSIG = 1, NBSIG
              EPSI(ISIG) = EPSI(ISIG) + D1(ISIG,JSIG)*
     +                     ZR(IDCONT+NBSIG*(IGAU-1)+JSIG-1)
  70        CONTINUE
C
            ENERPG(IGAU) = ENERPG(IGAU) +
     +               UNDEMI*ZR(IDCONT+NBSIG*(IGAU-1)+ISIG-1)*EPSI(ISIG)
  60     CONTINUE
C
C  --    CALCUL DE LA DENSITE D'ENERGIE TOTALE :
C        =====================================
         ELSEIF (OPTION(1:4).EQ.'ETOT') THEN
C
C  --      TENSEURS DES DEFORMATIONS  ET DES CONTRAINTES AU PAS DE 
C  --      TEMPS COURANT ET AU PAS DE TEMPS PRECEDENT S'IL Y A LIEU,
C  --      AU POINT D'INTEGRATION COURANT :
C          ------------------------------
           DO 80 I = 1, NBSIG
             EPSI(I) = EPSS(I+(IGAU-1)*NBSIG)
             IF (IDEPLM.NE.0) THEN
               EPSIM(I) = EPSSM(I+(IGAU-1)*NBSIG)
             ENDIF
             SIGMA(I) = ZR(IDSIG+(IGAU-1)*NBSIG+I-1)
             IF (IDSIGM.NE.0) THEN
               SIGMM(I) = ZR(IDSIGM+(IGAU-1)*NBSIG+I-1)
             ENDIF
  80       CONTINUE
C
           IF (IDEPLM.NE.0) THEN
             DO 90 I = 1, NBSIG
               DELTA(I) = EPSI(I) - EPSIM(I)
  90        CONTINUE
           ELSE
              DO 91 I = 1, NBSIG
               DELTA(I) = 0.D0
  91        CONTINUE
           ENDIF
C
C  --      CALCUL DES TERMES A SOMMER POUR OBTENIR LA DENSITE
C  --      D'ENERGIE TOTALE :
C          ----------------
           INTEG1 =       SIGMA(1)*DELTA(1) +      SIGMA(2)*DELTA(2)
     +             +      SIGMA(3)*DELTA(3) + DEUX*SIGMA(4)*DELTA(4)
C
           IF (IDEPLM.NE.0.AND.IDSIGM.NE.0) THEN
             INTEG2 =       SIGMM(1)*DELTA(1) +      SIGMM(2)*DELTA(2)
     +               +      SIGMM(3)*DELTA(3) + DEUX*SIGMM(4)*DELTA(4)
C
             ENERPG(IGAU) = UNDEMI*(INTEG1+INTEG2)
           ELSE
C
C  --        CAS D'ORDRE NUMERO 1 :
C            --------------------
             INTEG  =       SIGMA(1)*EPSI(1) +      SIGMA(2)*EPSI(2)
     +               +      SIGMA(3)*EPSI(3) + DEUX*SIGMA(4)*EPSI(4)
C
             ENERPG(IGAU) = UNDEMI*INTEG
C
           ENDIF
C
           ENELEM  = ENELEM + ENERPG(IGAU)*POIDS
C
         ENDIF
C
  20  CONTINUE
C
C ---- RECUPERATION DU CHAMP DES DENSITES D'ENERGIE DE DEFORMATION
C ---- ELASTIQUE EN SORTIE
C      -------------------
      CALL JEVECH('PENERDR','E',IDENER)
C
C ---- OPTIONS ENEL_ELGA ET ETOT_ELGA
C      ==============================
      IF (OPTION(6:9).EQ.'ELGA') THEN
         DO 100 IGAU = 1, NPG
           ZR(IDENER+IGAU-1) = ENERPG(IGAU)
 100     CONTINUE
C
C ---- OPTION ENEL_ELNO_ELGA ET ETOT_ELNO_ELGA
C      =======================================
      ELSEIF (OPTION(6:9).EQ.'ELNO') THEN
C
        IF (NPG.EQ.1) THEN
           DO 110 I = 1, NNOS
              ZR(IDENER+I-1) = ENERPG(1)
 110       CONTINUE
        ELSE
C
          NCMP = 1
        CALL PPGANO(NNOS,NPG,NCMP,ENERPG,ZR(IDENER))
        ENDIF
C
C ---- OPTION ETOT_ELEM
C      ================
      ELSEIF (OPTION(6:9).EQ.'ELEM') THEN
        ZR(IDENER) = ENELEM
      ENDIF
C
      END

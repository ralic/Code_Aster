      SUBROUTINE TE0576(OPTION,NOMTE)
      IMPLICIT   NONE
      CHARACTER*16        OPTION , NOMTE
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
C      A L'EQUILIBRE POUR LES ELEMENTS ISOPARAMETRIQUES 3D
C      .SOIT AUX POINTS D'INTEGRATION : OPTION 'ENEL_ELGA'
C      .SOIT AUX NOEUDS               : OPTION 'ENEL_ELNO_ELGA'
C
C      OPTIONS : 'ENEL_ELGA'
C                'ENEL_ELNO_ELGA'
C
C      CALCUL DE LA DENSITE D'ENERGIE TOTALE
C      A L'EQUILIBRE POUR LES ELEMENTS ISOPARAMETRIQUES 3D
C      .SOIT AUX POINTS D'INTEGRATION : OPTION 'ETOT_ELGA'
C      .SOIT AUX NOEUDS               : OPTION 'ETOT_ELNO_ELGA'
C
C      OPTIONS : 'ETOT_ELGA'
C                'ETOT_ELNO_ELGA'
C
C
C ENTREES  ---> OPTION : OPTION DE CALCUL
C          ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................
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
C
      INTEGER         NBPG(10), NBSIGM, JIN, NDIM, NNO, NBFPG, I,
     +                NNOS, JVAL, IPOIDS, IVF, NBNOMX, NBCONT, NPG1,
     +                NBSIG, IGAU, ISIG, INO, IGEOM, IDIM,
     +                ITEMPE, ITEMPS, IMATE, JSIG, IDENER, 
     +                IDFDE, IDFDN, IDFDK, IDEPL, IDEPLM, IDEPMM,
     +                IDSIG, IDSIGM, MXCMEL
      PARAMETER       (NBNOMX = 27)
      PARAMETER       (NBCONT =  6)
      PARAMETER       (MXCMEL =  162)
      REAL*8          EPSI(NBCONT), REPERE(7)
      REAL*8          INSTAN, ZERO, UNDEMI, TEMPG, ENELEM
      REAL*8          ENERPG(NBNOMX), ENERNO(NBNOMX)
      REAL*8          D1(NBCONT,NBCONT), XYZGAU(3)
      REAL*8          NHARM, DEUX, INTEG1, INTEG2, INTEG
      REAL*8          EPSIM(NBCONT), DELTA(NBCONT), EPSS(MXCMEL)
      REAL*8          EPSSM(MXCMEL), SIGMM(NBCONT), SIGMA(NBCONT)
      REAL*8          DFDX(27), DFDY(27), DFDZ(27), POIDS
      CHARACTER*8     ELREFE, MODELI
      CHARACTER*24    CHVAL, CHCTE
C DEB ------------------------------------------------------------------
C
      CALL ELREF1(ELREFE)
      MODELI(1:2) = NOMTE(3:4)
C
C ---- CARACTERISTIQUES DU TYPE D'ELEMENT :
C ---- GEOMETRIE ET INTEGRATION
C      ------------------------
      CHCTE = '&INEL.'//ELREFE//'.CARACTE'
      CALL JEVETE(CHCTE,'L',JIN)
      NDIM  = ZI(JIN+1-1)
      NNO   = ZI(JIN+2-1)
      NPG1  = ZI(JIN+4-1)
      NBFPG = ZI(JIN+3-1)
      DO 10 I = 1,NBFPG
         NBPG(I) = ZI(JIN+3-1+I)
  10  CONTINUE
      NNOS = ZI(JIN+3-1+NBFPG+1)
      NPG1 = NBPG(1)
      IF (OPTION(6:9).EQ.'ELNO') THEN
        IF(ELREFE.EQ.'TETRA10 '.OR.ELREFE.EQ.'HEXA20  '.OR.
     +     ELREFE.EQ.'HEXA27  ') THEN
           NPG1 = NBPG(3)
         ELSE IF(ELREFE.EQ.'PENTA15 ' ) THEN
          NPG1 = NBPG(2)
        ENDIF
      ENDIF
C
      CHVAL = '&INEL.'//ELREFE//'.FFORMES'
      CALL JEVETE(CHVAL,'L',JVAL)
      IPOIDS = JVAL + (NDIM+1)*NNO*NNO
      IF (OPTION(6:9).EQ.'ELNO') THEN
        IF(ELREFE.EQ.'TETRA10 '.OR.ELREFE.EQ.'HEXA20  ' ) THEN
          IPOIDS = IPOIDS + NBPG(1)*(1+(NDIM+1)*NNO)
     &                  + NBPG(2)*(1+(NDIM+1)*NNO)
         ELSE IF(ELREFE.EQ.'PENTA15 ' ) THEN
          IPOIDS = IPOIDS + NBPG(1)*(1+(NDIM+1)*NNO)
        ENDIF
      ENDIF
      IVF    = IPOIDS + NPG1
      IDFDE  = IVF    + NPG1*NNO
      IDFDN  = IDFDE  + 1
      IDFDK  = IDFDN  + 1
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
      ENELEM      = ZERO
C
      DO 20 I = 1, NBNOMX
         ENERPG(I) = ZERO
         ENERNO(I) = ZERO
 20   CONTINUE
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
         CALL JEVECH('PCONTRR','L',IDSIG)
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
     +              ZR(IDFDN),ZR(IDFDK),ZR(IPOIDS),ZR(IGEOM),
     +              ZR(IDEPL),NHARM,EPSS)
C
C ---   CALCUL EVENTUEL DU CHAMP DE DEFORMATIONS AU PREMIER ORDRE
C ---   CORRESPONDANT AU CHAMP DE DEPLACEMENT A L'INSTANT PRECEDENT :
C       -----------------------------------------------------------
        IF (IDEPLM.NE.0) THEN
          CALL EPS1MC(MODELI,NNO,NDIM,NBSIG,NPG1,ZR(IVF),ZR(IDFDE),
     +                ZR(IDFDN),ZR(IDFDK),ZR(IPOIDS),ZR(IGEOM),
     +                ZR(IDEPMM),NHARM,EPSSM)
           ENDIF
C
      ENDIF
C
C ---- BOUCLE SUR LES POINTS D'INTEGRATION :
C      ===================================
      DO 30 IGAU = 1, NPG1
C
C  --    CALCUL DU JACOBIEN AU POINT D'INTEGRATION COURANT :
C        -------------------------------------------------
            CALL DFDM3D(NNO,ZR(IPOIDS+IGAU-1),
     +                  ZR(IDFDE+(IGAU-1)*NNO*NDIM),
     +                  ZR(IDFDN+(IGAU-1)*NNO*NDIM),
     +                  ZR(IDFDK+(IGAU-1)*NNO*NDIM),
     +                  ZR(IGEOM),DFDX,DFDY,DFDZ,POIDS)
C
         DO 40 ISIG = 1, NBSIG
           EPSI(ISIG) = ZERO
  40     CONTINUE
C
C  --      COORDONNEES ET TEMPERATURE AU POINT D'INTEGRATION
C  --      COURANT
C          -------
         XYZGAU(1) = ZERO
         XYZGAU(2) = ZERO
         XYZGAU(3) = ZERO
         TEMPG     = ZERO
C
         DO 50 I = 1, NNO
C
            DO 60 IDIM = 1, NDIM
               XYZGAU(IDIM) = XYZGAU(IDIM) +
     +                        ZR(IVF+I+NNO*(IGAU-1)-1)*
     +                        ZR(IGEOM+IDIM+NDIM*(I-1)-1)
  60         CONTINUE
C
             IF (OPTION(1:4).EQ.'ENEL') THEN
               TEMPG     = TEMPG     + ZR(IVF+I+NNO*(IGAU-1)-1)*
     +                                 ZR(ITEMPE+I-1)
             ENDIF
C
  50     CONTINUE
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
         DO 70 ISIG = 1, NBSIG
            DO 80 JSIG = 1, NBSIG
              EPSI(ISIG) = EPSI(ISIG) + D1(ISIG,JSIG)*
     +                     ZR(IDSIG+NBSIG*(IGAU-1)+JSIG-1)
  80        CONTINUE
C
            ENERPG(IGAU) = ENERPG(IGAU) +
     +             UNDEMI*ZR(IDSIG+NBSIG*(IGAU-1)+ISIG-1)*EPSI(ISIG)
  70     CONTINUE
C
C  --    CALCUL DE LA DENSITE D'ENERGIE TOTALE :
C        =====================================
         ELSEIF (OPTION(1:4).EQ.'ETOT') THEN
C
C  --      TENSEURS DES DEFORMATIONS  ET DES CONTRAINTES AU PAS DE 
C  --      TEMPS COURANT ET AU PAS DE TEMPS PRECEDENT S'IL Y A LIEU,
C  --      AU POINT D'INTEGRATION COURANT :
C          ------------------------------
           DO 90 I = 1, NBSIG
             EPSI(I) = EPSS(I+(IGAU-1)*NBSIG)
             IF (IDEPLM.NE.0) THEN
               EPSIM(I) = EPSSM(I+(IGAU-1)*NBSIG)
             ENDIF
             SIGMA(I) = ZR(IDSIG+(IGAU-1)*NBSIG+I-1)
             IF (IDSIGM.NE.0) THEN
               SIGMM(I) = ZR(IDSIGM+(IGAU-1)*NBSIG+I-1)
             ENDIF
  90       CONTINUE
C
           IF (IDEPLM.NE.0) THEN
             DO 100 I = 1, NBSIG
               DELTA(I) = EPSI(I) - EPSIM(I)
 100         CONTINUE
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
     +             + DEUX*SIGMA(5)*DELTA(5) + DEUX*SIGMA(6)*DELTA(6)
C
           IF (IDEPLM.NE.0.AND.IDSIGM.NE.0) THEN
             INTEG2 =       SIGMM(1)*DELTA(1) +      SIGMM(2)*DELTA(2)
     +               +      SIGMM(3)*DELTA(3) + DEUX*SIGMM(4)*DELTA(4)
     +               + DEUX*SIGMM(5)*DELTA(5) + DEUX*SIGMM(6)*DELTA(6)
C
             ENERPG(IGAU) = UNDEMI*(INTEG1+INTEG2)
           ELSE
C
C  --        CAS D'ORDRE NUMERO 1 :
C            --------------------
             INTEG  =       SIGMA(1)*EPSI(1) +      SIGMA(2)*EPSI(2)
     +               +      SIGMA(3)*EPSI(3) + DEUX*SIGMA(4)*EPSI(4)
     +               + DEUX*SIGMA(5)*EPSI(5) + DEUX*SIGMA(6)*EPSI(6)
C
             ENERPG(IGAU) = UNDEMI*INTEG
C
           ENDIF
C
           ENELEM  = ENELEM + ENERPG(IGAU)*POIDS
C
         ENDIF
C
  30  CONTINUE
C
C ---- RECUPERATION DU CHAMP DES DENSITES D'ENERGIE DE DEFORMATION
C ---- ELASTIQUE EN SORTIE
C      -------------------
      CALL JEVECH('PENERDR','E',IDENER)
C
C ---- OPTIONS ENEL_ELGA ET ETOT_ELGA
C      ==============================
      IF (OPTION(6:9).EQ.'ELGA') THEN
         DO 110 IGAU = 1, NPG1
           ZR(IDENER+IGAU-1) = ENERPG(IGAU)
 110     CONTINUE
C
C ---- OPTION ENEL_ELNO_ELGA ET ETOT_ELNO_ELGA
C      =======================================
      ELSEIF (OPTION(6:9).EQ.'ELNO') THEN
C
C ----   DENSITE D'ENERGIE DE DEFORMATION AUX NOEUDS
C        -------------------------------------------
        CALL PPGANO ( NNOS, NPG1, 1, ENERPG , ENERNO)
C
         DO 120 INO = 1, NNO
           ZR(IDENER+INO-1) = ENERNO(INO)
 120     CONTINUE
C
C ---- OPTION ETOT_ELEM
C      ================
      ELSEIF (OPTION(6:9).EQ.'ELEM') THEN
        ZR(IDENER) = ENELEM
      ENDIF
C
      END

      SUBROUTINE TE0025 ( OPTION , NOMTE )
      IMPLICIT   NONE
      CHARACTER*16        OPTION , NOMTE
C.......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 04/04/2002   AUTEUR VABHHTS J.PELLET 
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
C     BUT: CALCUL DES DEFORMATIONS AUX POINTS D'INTEGRATION
C          OU AUX NOEUDS DES ELEMENTS ISOPARAMETRIQUES 3D
C
C          OPTIONS : 'EPSI_ELNO_DEPL'
C                    'EPSI_ELGA_DEPL'
C                    'EPSG_ELNO_DEPL'
C                    'EPSG_ELGA_DEPL'
C                    'EPME_ELNO_DEPL  '
C                    'EPME_ELGA_DEPL  '
C                    'EPMG_ELNO_DEPL  '
C                    'EPMG_ELGA_DEPL  '
C
C     ENTREES  ---> OPTION : OPTION DE CALCUL
C              ---> NOMTE  : NOM DU TYPE ELEMENT
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
      INTEGER          NBPG(10), NBSIGM, JIN, NDIM, NNO, NBFPG, I,
     +                 NNOS, NPG, JVAL, IPOIDS, IVF, IDFDE, IDFDN,
     +                 IDFDK, NBSIG, IGAU, ISIG, INO, IGEOM, IDEPL,
     +                 ITEMPE, ITREF, ITEMPS, IDEFO, IMATE, IHYDRE,
     +                 ISECHE
      REAL*8           EPSM(162), EPSNO(162), REPERE(7), TEMPE(27)
      REAL*8           HYDR(27), SECH(27)
      REAL*8           NHARM, INSTAN, ZERO, TREF
      CHARACTER*8      ELREFE, MODELI
      CHARACTER*24     CHVAL,CHCTE
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
      NBFPG = ZI(JIN+3-1)
      DO 10 I = 1,NBFPG
         NBPG(I) = ZI(JIN+3-1+I)
  10  CONTINUE
      NNOS = ZI(JIN+3-1+NBFPG+1)
      NPG  = NBPG(1)
      IF ( OPTION(6:9).EQ.'ELNO') THEN
         IF ( ELREFE.EQ.'TETRA10 ' .OR. ELREFE.EQ.'HEXA20  ' .OR.
     +        ELREFE .EQ.'HEXA27  '  ) THEN
            NPG = NBPG(3)
         ELSEIF( ELREFE.EQ. 'PENTA15 ' ) THEN
            NPG = NBPG(2)
         ENDIF
      ENDIF
C
      CHVAL = '&INEL.'//ELREFE//'.FFORMES'
      CALL JEVETE ( CHVAL, 'L', JVAL )
C
      IPOIDS = JVAL + (NDIM+1)*NNO*NNO
      IF (OPTION(6:9).EQ.'ELNO') THEN
        IF(ELREFE.EQ.'TETRA10 '.OR.ELREFE.EQ.'HEXA20  ' ) THEN
          IPOIDS = IPOIDS + NBPG(1)*(1+(NDIM+1)*NNO)
     &                    + NBPG(2)*(1+(NDIM+1)*NNO)
        ELSE IF(ELREFE.EQ.'PENTA15 ' ) THEN
          IPOIDS = IPOIDS + NBPG(1)*(1+(NDIM+1)*NNO)
        ENDIF
      ENDIF
      IVF    = IPOIDS + NPG
      IDFDE  = IVF    + NPG*NNO
      IDFDN  = IDFDE  + 1
      IDFDK  = IDFDN  + 1
C
C ---- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
C      -----------------------------------------
      NBSIG  = NBSIGM(MODELI)
C
C --- INITIALISATIONS :
C     -----------------
      ZERO    = 0.0D0
      INSTAN  = ZERO
      NHARM   = ZERO
      TREF    = ZERO
C
      DO 20 I = 1, NBSIG*NPG
         EPSM(I) = ZERO
 20   CONTINUE
C
      DO 30 I = 1, 27
         TEMPE(I) = ZERO
         HYDR(I)   = ZERO
         SECH(I)   = ZERO
 30   CONTINUE
C
C ---- RECUPERATION DES COORDONNEES DES CONNECTIVITES :
C      ----------------------------------------------
      CALL JEVECH('PGEOMER','L',IGEOM)
C
C
C ---- RECUPERATION DU CHAMP DE DEPLACEMENT SUR L'ELEMENT :
C      --------------------------------------------------
      CALL JEVECH('PDEPLAR','L',IDEPL)
C
C ---- RECUPERATION DU CHAMP DE TEMPERATURE SUR L'ELEMENT :
C      --------------------------------------------------
      CALL TECACH(.FALSE.,.FALSE.,'PTEMPER',1,ITEMPE)
      IF (ITEMPE.NE.0) THEN
        DO 40 I = 1, NNO
          TEMPE(I) = ZR(ITEMPE+I-1)
 40     CONTINUE
      ENDIF
C
C --- RECUPERATION DE L'HYDRATATION AUX POINTS DE GAUSS DE L'ELEMENT :
C     -----------------------------------------------------
      CALL TECACH(.FALSE.,.FALSE.,'PHYDRER',1,IHYDRE)
      IF(IHYDRE.NE.0) THEN
      DO 45 I = 1, NPG
         HYDR(I)   = ZR(IHYDRE+I-1)
  45  CONTINUE
      ELSE
      ENDIF
C
C --- RECUPERATION DU SECHAGE AUX NOEUDS DE L'ELEMENT :
C     -----------------------------------------------------
      CALL TECACH(.FALSE.,.FALSE.,'PSECHER',1,ISECHE)
      IF(ISECHE.NE.0) THEN
      DO 46 I = 1, NNO
         SECH(I)   = ZR(ISECHE+I-1)
  46  CONTINUE
      ELSE
      ENDIF
C
C ---- RECUPERATION DE LA TEMPERATURE DE REFERENCE :
C      -------------------------------------------
      CALL TECACH(.FALSE.,.FALSE.,'PTEREF',1,ITREF)
      IF (ITREF.NE.0) THEN
          TREF = ZR(ITREF)
      ENDIF
C
C ---- RECUPERATION DE L'INSTANT DE CALCUL :
C      -----------------------------------
      CALL TECACH(.FALSE.,.FALSE.,'PTEMPSR',1,ITEMPS)
      IF (ITEMPS.NE.0) THEN
          INSTAN = ZR(ITEMPS)
      ENDIF
C
C ---- RECUPERATION DU VECTEUR DES DEFORMATIONS EN SORTIE :
C      --------------------------------------------------
      CALL JEVECH('PDEFORR','E',IDEFO)
C
C ---- CALCUL DES DEFORMATIONS MECANIQUES AUX POINTS D'INTEGRATION
C ---- DE L'ELEMENT , I.E. SI ON NOTE EPSI_MECA = B*U
C ---- ON CALCULE SIMPLEMENT EPSI_MECA POUR LES OPTIONS EPSI ET EPSG
C ----                    ET EPSI_MECA - EPSI_THERMIQUES POUR LES
C ----                    OPTIONS EPME ET EPMG :
C      ---------------------------------------
      CALL TECACH(.FALSE.,.FALSE.,'PMATERC',1,IMATE)
      CALL EPSVMC(MODELI,NNO,NDIM,NBSIG,NPG,ZR(IVF),ZR(IDFDE),ZR(IDFDN)
     +            ,ZR(IDFDK),ZR(IPOIDS),ZR(IGEOM),ZR(IDEPL),TEMPE,
     +            TREF,HYDR,SECH,INSTAN,ZI(IMATE),REPERE,NHARM,OPTION,
     +            EPSM)
C
      IF (OPTION(6:9).EQ.'ELGA') THEN
C         --------------------
C ---- AFFECTATION DU VECTEUR EN SORTIE AVEC LES DEFORMATIONS AUX
C ---- POINTS D'INTEGRATION :
C      --------------------
        DO 50 IGAU = 1, NPG
        DO 50 ISIG = 1, NBSIG
          ZR(IDEFO+NBSIG*(IGAU-1)+ISIG-1) = EPSM(NBSIG*(IGAU-1)+ISIG)
 50     CONTINUE
C
      ELSE IF ( OPTION(6:9) .EQ. 'ELNO' ) THEN
C
C ---- DEFORMATIONS AUX NOEUDS :
C      -----------------------
C
        CALL PPGANO ( NNOS, NPG, NBSIG, EPSM, EPSNO)
C
C ---- AFFECTATION DU VECTEUR EN SORTIE AVEC LES DEFORMATIONS AUX
C ---- NOEUDS :
C      ------
        DO 60 INO  = 1, NNO
        DO 60 ISIG = 1, NBSIG
          ZR(IDEFO+NBSIG*(INO-1)+ISIG-1) = EPSNO(NBSIG*(INO-1)+ISIG)
 60     CONTINUE
C
      ENDIF
C
      END

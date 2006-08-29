      SUBROUTINE TE0087 ( OPTION , NOMTE )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 28/08/2006   AUTEUR CIBHHPD L.SALMONA 
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
C          OU AUX NOEUDS DES ELEMENTS ISOPARAMETRIQUES 2D
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
C ......................................................................
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
      INTEGER          NBSIG, NBSIG1, NBSIG2, NDIM, NNO, I,
     +                 NNOS, NPG, IPOIDS, IVF, IDFDE,
     +                 IGAU, ISIG, IGEOM, IDEPL,IRET,
     +                 ITEMPE, ITREF, ITEMPS, IDEFO, IMATE
      REAL*8           EPSM(54), REPERE(7)
      REAL*8           NHARM, INSTAN, TEMPE(27)
      CHARACTER*4      FAMI
      CHARACTER*8      MODELI
      CHARACTER*16     COMPOR
C DEB ------------------------------------------------------------------
C
      MODELI(1:2) = NOMTE(3:4)
C
      IF ( OPTION(6:9) .EQ.'ELNO' ) THEN
        CALL ELREF4(' ','GANO',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
        FAMI='GANO'
      ELSE
        CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
        FAMI='RIGI'
      ENDIF
C
C ---- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
C      -----------------------------------------
      NBSIG1  = NBSIGM(MODELI)
      NBSIG2  = 6
      NBSIG   = NBSIG1
C
C --- INITIALISATIONS :
C     -----------------
      ZERO     = 0.0D0
      INSTAN   = ZERO
      NHARM    = ZERO
      TREF     = ZERO
      COMPOR   = '                '
C
      DO 10 I = 1, NBSIG2*NPG
         EPSM(I)   = ZERO
 10   CONTINUE
C
      DO 20 I = 1, 27
         TEMPE(I)   = ZERO
 20   CONTINUE
C
C ---- RECUPERATION DES COORDONNEES DES CONNECTIVITES :
C      ----------------------------------------------
      CALL JEVECH('PGEOMER','L',IGEOM)
C
C ---- RECUPERATION DU MATERIAU :
C      ------------------------
      CALL JEVECH('PMATERC','L',IMATE)
C
C ---- RECUPERATION  DES DONNEEES RELATIVES AU REPERE D'ORTHOTROPIE :
C      ------------------------------------------------------------
      CALL ORTREP(ZI(IMATE),NDIM,REPERE)
C
C ---- RECUPERATION DU CHAMP DE DEPLACEMENT SUR L'ELEMENT :
C      --------------------------------------------------
      CALL JEVECH('PDEPLAR','L',IDEPL)
C
C ---- RECUPERATION DU CHAMP DE TEMPERATURE SUR L'ELEMENT :
C      --------------------------------------------------
      CALL TECACH('ONN','PTEMPER',1,ITEMPE,IRET)
      IF (ITEMPE.NE.0) THEN
        DO 30 I = 1, NNO
          TEMPE(I) = ZR(ITEMPE+I-1)
 30     CONTINUE
      ENDIF
C
C ---- RECUPERATION DE LA TEMPERATURE DE REFERENCE :
C      -------------------------------------------
      CALL TECACH('ONN','PTEREF',1,ITREF,IRET)
      IF (ITREF.NE.0) THEN
          TREF = ZR(ITREF)
      ENDIF

C ---- RECUPERATION DE L'INSTANT DE CALCUL :
C      -----------------------------------
      CALL TECACH('ONN','PTEMPSR',1,ITEMPS,IRET)
      IF (ITEMPS.NE.0) THEN
          INSTAN = ZR(ITEMPS)
      ENDIF
C
C ---- RECUPERATION DU COMPORTEMENT DANS LE CAS DES CONTRAINTES PLANES :
C      ---------------------------------------------------------------
      IF (MODELI(1:2).EQ.'CP') THEN
      CALL TECACH('NNN','PCOMPOR',1,ICOMPO,IRET)
        IF (ICOMPO.NE.0) THEN
          COMPOR = ZK16(ICOMPO)
          IF (COMPOR.NE.'ELAS'.AND.COMPOR.NE.'                ') THEN
            CALL UTMESS('A','TE0087','ATTENTION VOUS AVEZ UNE LOI DE'//
     +                  ' COMPORTEMENT INELASTIQUE ET VOUS ETES EN '//
     +                  'CONTRAINTES PLANES, LA COMPOSANTE DU TENSEUR'
     +                //' DE DEFORMATIONS EPZZ QUE VOUS ALLEZ CALCULER'
     +                //' N''EST VALABLE QUE TANT QUE VOUS RESTEZ '
     +                //'DANS LE DOMAINE ELASTIQUE.')
          ENDIF
        ENDIF
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
      CALL EPSVMC(FAMI,MODELI,NNO,NDIM,NBSIG1,NPG,IPOIDS,IVF,IDFDE,
     +            ZR(IGEOM),ZR(IDEPL),TEMPE,TREF,INSTAN,
     +            ZI(IMATE),REPERE,NHARM,OPTION,EPSM)
C
      IF (OPTION(6:9).EQ.'ELGA') THEN
C         --------------------
C ---- AFFECTATION DU VECTEUR EN SORTIE AVEC LES DEFORMATIONS AUX
C ---- POINTS D'INTEGRATION :
C      --------------------
        DO 80 IGAU = 1, NPG
        DO 80 ISIG = 1, NBSIG
          ZR(IDEFO+NBSIG*(IGAU-1)+ISIG-1) = EPSM(NBSIG*(IGAU-1)+ISIG)
 80     CONTINUE
C
      ELSE IF ( OPTION(6:9) .EQ. 'ELNO' ) THEN
C
C ---- DEFORMATIONS AUX NOEUDS :
C      -----------------------
C
         CALL PPGAN2 ( JGANO, NBSIG, EPSM, ZR(IDEFO) )
C
      ENDIF
C
      END

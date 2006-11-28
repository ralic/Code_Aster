      SUBROUTINE TE0284 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 27/11/2006   AUTEUR SALMONA L.SALMONA 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES EN 2D
C                      OPTION : 'CHAR_MECA_EPSI_R  ','CHAR_MECA_EPSI_F '
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      CHARACTER*8        MODELI
      REAL*8             SIGI(162), EPSI(162), BSIGMA(81), REPERE(7)
      REAL*8             INSTAN, NHARM, XYZ(81),BARY(3)
      INTEGER            NBSIGM, NBDIM, DIMCOO, IDIM
C
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
      MODELI(1:2) = NOMTE(3:4)
C
C
C ---- CARACTERISTIQUES DU TYPE D'ELEMENT :
C ---- GEOMETRIE ET INTEGRATION
C      ------------------------
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
      DIMCOO = NDIM
C
C --- INITIALISATIONS :
C     -----------------
      ZERO    = 0.0D0
      INSTAN  = ZERO
      NDIM    = NBDIM( NOMTE )
C
C ---- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
C      -----------------------------------------
      NBSIG  = NBSIGM(MODELI)
C
      DO 10 I = 1, NBSIG*NPG
         EPSI(I) = ZERO
         SIGI(I) = ZERO
 10   CONTINUE
C
      DO 20 I = 1, NDIM*NNO
         BSIGMA(I) = ZERO
 20   CONTINUE
C
C ---- RECUPERATION DE L'HARMONIQUE DE FOURIER
C      ---------------------------------------
      CALL TECACH('NNN','PHARMON',1,IHARMO,IRET)
      IF ( IHARMO .EQ. 0 ) THEN
         NHARM = ZERO
      ELSE
         NHARM = DBLE( ZI(IHARMO) )
      ENDIF
C
C ---- RECUPERATION DES COORDONNEES DES CONNECTIVITES
C      ----------------------------------------------
      CALL JEVECH('PGEOMER','L',IGEOM)
      IF ( NDIM .EQ. DIMCOO ) THEN
         DO 100 I = 1 , NDIM*NNO
            XYZ(I) = ZR(IGEOM+I-1)
 100     CONTINUE
      ELSE
         DO 110 I = 1, NNO
            DO 120 IDIM = 1 , NDIM
               IF ( IDIM .LE. DIMCOO ) THEN
                  XYZ(IDIM+NDIM*(I-1)) = ZR(IGEOM-1+IDIM+DIMCOO*(I-1))
               ELSE
                  XYZ(IDIM+NDIM*(I-1)) = 0.D0
               ENDIF
 120        CONTINUE
 110     CONTINUE
      ENDIF
C
C ---- RECUPERATION DU MATERIAU
C      ------------------------
      CALL JEVECH('PMATERC','L',IMATE)
C
C ---- RECUPERATION  DES DONNEEES RELATIVES AU REPERE D'ORTHOTROPIE
C      ------------------------------------------------------------
C     COORDONNEES DU BARYCENTRE ( POUR LE REPRE CYLINDRIQUE )

      BARY(1) = 0.D0
      BARY(2) = 0.D0
      BARY(3) = 0.D0
      DO 150 I = 1,NNO
        DO 140 IDIM = 1,DIMCOO
          BARY(IDIM) = BARY(IDIM)+ZR(IGEOM+IDIM+DIMCOO*(I-1)-1)/NNO
 140    CONTINUE
 150  CONTINUE
      CALL ORTREP(ZI(IMATE),DIMCOO,BARY,REPERE)
C
C ---- RECUPERATION DU CHAMP DE TEMPERATURE SUR L'ELEMENT
C      --------------------------------------------------
      CALL JEVECH('PTEMPER','L',ITEMPE)
C
C ---- RECUPERATION DE L'INSTANT
C      -------------------------
      CALL TECACH('NNN','PTEMPSR',1,ITEMPS,IRET)
      IF (ITEMPS.NE.0) INSTAN = ZR(ITEMPS)
C
C ---- CONSTRUCTION DU VECTEUR DES DEFORMATIONS INITIALES DEFINIES AUX
C ---- POINTS D'INTEGRATION A PARTIR DES DONNEES UTILISATEUR
C      -----------------------------------------------------
      CALL EPSIMC(OPTION,ZR(IGEOM),NNO,NPG,NDIM,NBSIG,ZR(IVF),EPSI)
C
C ---- CALCUL DU VECTEUR DES CONTRAINTES INITIALES AUX POINTS
C ---- D'INTEGRATION
C      -------------
      CALL SIGIMC(MODELI,NNO,NDIM,NBSIG,NPG,ZR(IVF),XYZ,
     +            ZR(ITEMPE),INSTAN,ZI(IMATE),REPERE,EPSI,SIGI)
C
C ---- CALCUL DU VECTEUR DES FORCES DUES AUX CONTRAINTES INITIALES
C ---- (I.E. BT*SIG_INITIALES)
C      ----------------------
      CALL BSIGMC ( MODELI,NNO,NDIM,NBSIG,NPG, IPOIDS, IVF, IDFDE,
     +              ZR(IGEOM),NHARM,SIGI,BSIGMA)
C
C ---- RECUPERATION ET AFFECTATION DU VECTEUR EN SORTIE AVEC LE
C ---- VECTEUR DES FORCES DUES AUX CONTRAINTES INITIALES
C      -------------------------------------------------
      CALL JEVECH('PVECTUR','E',IVECTU)
C
      DO 30 I = 1, NDIM*NNO
         ZR(IVECTU+I-1) = BSIGMA(I)
 30   CONTINUE
C
C FIN ------------------------------------------------------------------
      END

      SUBROUTINE TE0384 ( OPTION , NOMTE )
      IMPLICIT   NONE
      CHARACTER*16        OPTION , NOMTE
C.......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 30/03/2004   AUTEUR CIBHHLV L.VIVAN 
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
C    - FONCTION REALISEE:  CALCUL DES CHAMELEM  DE GRADIENTS
C                          DE DEPLACEMENT EN 2D
C                .AUX POINTS D'INTEGRATION (OPTION 'GRAD_ELGA_THETA')
C                .AUX NOEUDS               (OPTION 'GTHE_ELNO_ELGA')
C                          OPTION : 'GRAD_ELGA_THETA '
C                          OPTION : 'GTHE_ELNO_ELGA  '
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
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
      INTEGER            NBNOMX, NDIMAX, NNO, 
     +                   NNOS, IPOIDS, IVF, IDFDE, JGANO,
     +                   NPG, I, J ,K, NDIM, IGAU, IDIM, JDIM, INO,
     +                   IGEOM, IDEPL, IGRADU, NCMP
      PARAMETER         ( NBNOMX = 9 )
      PARAMETER         ( NDIMAX = 2 )
      REAL*8             GRADPG(NDIMAX,NDIMAX,NBNOMX), ZERO
      REAL*8             GRADNO(NDIMAX,NDIMAX,NBNOMX)
      REAL*8             DFDX(NBNOMX), DFDY(NBNOMX), JACGAU
C DEB ------------------------------------------------------------------
C
      IF (OPTION.EQ.'GTHE_ELNO_ELGA') THEN
      CALL ELREF4(' ','GANO',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
      ELSE
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
      ENDIF
C
      ZERO   = 0.0D0
      NDIM   = 2
C
      DO 10 K = 1, NBNOMX
        DO 20 I = 1, NDIMAX
        DO 20 J = 1, NDIMAX
          GRADPG(I,J,K) = ZERO
          GRADNO(I,J,K) = ZERO
 20     CONTINUE
 10   CONTINUE
C
C ---- RECUPERATION DES COORDONNEES DES CONNECTIVITES :
C      ----------------------------------------------
      CALL JEVECH('PGEOMER','L',IGEOM)
C
C ---- RECUPERATION DU CHAMNO DE DEPLACEMENT AUX NOEUDS DE L'ELEMENT:
C      -------------------------------------------------------------
      CALL JEVECH('PTHETAR','L',IDEPL)
C
C ---- RECUPERATION DU VECTEUR DES GRADIENTS DE DEPLACEMENTS
C ---  EN SORTIE DU TE :
C      --------------------------------
      CALL JEVECH('PGRADUR','E',IGRADU)
C
C ---  BOUCLE SUR LES POINTS D'INTEGRATION :
C      -----------------------------------
      DO 30 IGAU = 1, NPG
C
         K = NNO*(IGAU-1)
C
C ----    CALCUL DES DERIVEES DES FONCTIONS DE FORME SUR L'ELEMENT
C ----    REEL ET DU PRODUIT JACOBIEN*POIDS (DANS JACGAU) :
C         -----------------------------------------------
         CALL DFDM2D (NNO,IGAU,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,JACGAU)
C
         DO 40 INO = 1, NNO
           GRADPG(1,1,IGAU) = GRADPG(1,1,IGAU) +
     +                     DFDX(INO)*ZR(IDEPL+2*(INO-1)+1-1)
           GRADPG(1,2,IGAU) = GRADPG(1,2,IGAU) +
     +                     DFDY(INO)*ZR(IDEPL+2*(INO-1)+1-1)
           GRADPG(2,1,IGAU) = GRADPG(2,1,IGAU) +
     +                     DFDX(INO)*ZR(IDEPL+2*(INO-1)+2-1)
           GRADPG(2,2,IGAU) = GRADPG(2,2,IGAU) +
     +                     DFDY(INO)*ZR(IDEPL+2*(INO-1)+2-1)
  40     CONTINUE
C
  30  CONTINUE
C
C --- OPTION GRAD_ELGA_THETA :
C     ======================
      IF (OPTION(6:9).EQ.'ELGA') THEN
C         --------------------
C ---- AFFECTATION DU VECTEUR EN SORTIE AVEC LES GRADIENTS DE
C ---- DEPLACEMENT AUX POINTS D'INTEGRATION :
C      ------------------------------------
        DO 50 IGAU = 1, NPG
          DO 60 IDIM = 1, NDIM
            DO 70 JDIM = 1, NDIM
              ZR(IGRADU+NDIM*NDIM*(IGAU-1)+NDIM*(IDIM-1)+JDIM-1)
     +      =  GRADPG(IDIM,JDIM,IGAU)
  70        CONTINUE
  60      CONTINUE
  50    CONTINUE
C
      ELSE
C
C --- OPTION GTHE_ELNO_ELGA :
C     =====================
C
C --- PASSAGE DES GRADIENTS DE DEPLACEMENT DES POINTS D'INTEGRATION
C --- AUX NOEUDS :
C     ----------
C
        NCMP = 4
        CALL PPGAN2 ( JGANO, NCMP, GRADPG, GRADNO )
C
C ---- AFFECTATION DU VECTEUR EN SORTIE AVEC LES GRADIENTS DE
C ---- DEPLACEMENT AUX NOEUDS :
C      ----------------------
        DO 80 INO = 1, NNO
          DO 90 IDIM = 1, NDIM
            DO 100 JDIM = 1, NDIM
              ZR(IGRADU+NDIM*NDIM*(INO-1)+NDIM*(IDIM-1)+JDIM-1)
     +      =  GRADNO(IDIM,JDIM,INO)
 100        CONTINUE
  90      CONTINUE
  80    CONTINUE
C
      ENDIF
C
      END

      SUBROUTINE TE0286(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 25/04/2006   AUTEUR CIBHHPD L.SALMONA 
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
      CHARACTER*16      OPTION,NOMTE
C.......................................................................
C FONCTION REALISEE:
C
C      CALCUL DE L'ENERGIE POTENTIELLE THERMOELASTIQUE A L'EQUILIBRE
C      ELEMENTS ISOPARAMETRIQUES 2D
C
C      OPTION : 'EPOT_ELEM_DEPL'
C
C ENTREES  ---> OPTION : OPTION DE CALCUL
C          ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................
C
      CHARACTER*8        MODELI
      REAL*8             SIGMA(162), BSIGMA(81), REPERE(7)
      REAL*8             INSTAN, NHARM
      INTEGER            NBSIGM
C
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
      MODELI(1:2) = NOMTE(3:4)
C
C ---- CARACTERISTIQUES DU TYPE D'ELEMENT :
C ---- GEOMETRIE ET INTEGRATION
C      ------------------------
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
C
C --- INITIALISATIONS :
C     -----------------
      NH          = 0
      ZERO        = 0.0D0
      UNDEMI      = 0.5D0
      INSTAN      = ZERO
      NHARM       = ZERO
      NDIM2       = 2
      IF (MODELI(1:2).EQ.'FO') THEN
        NDIM = 3
      ENDIF
C
C ---- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
C      -----------------------------------------
      NBSIG  = NBSIGM(MODELI)
C
      DO 10 I = 1, NBSIG*NPG
         SIGMA(I) = ZERO
 10   CONTINUE
C
      DO 20 I = 1, NDIM*NNO
         BSIGMA(I) = ZERO
 20   CONTINUE
C
C ---- RECUPERATION DES COORDONNEES DES CONNECTIVITES
C      ----------------------------------------------
      CALL JEVECH('PGEOMER','L',IGEOM)
C
C ---- RECUPERATION DU MATERIAU
C      ------------------------
      CALL JEVECH('PMATERC','L',IMATE)
C
C ---- RECUPERATION  DES DONNEEES RELATIVES AU REPERE D'ORTHOTROPIE
C      ------------------------------------------------------------
      CALL ORTREP(ZI(IMATE),NDIM2,REPERE)
C
C ---- RECUPERATION DU CHAMP DE DEPLACEMENT SUR L'ELEMENT
C      --------------------------------------------------
      CALL JEVECH('PDEPLAR','L',IDEPL)
C
C ---- RECUPERATION DU CHAMP DE TEMPERATURE SUR L'ELEMENT
C      --------------------------------------------------
      CALL JEVECH('PTEMPER','L',ITEMPE)
C
C ---- RECUPERATION DE LA TEMPERATURE DE REFERENCE
C      -------------------------------------------
      CALL JEVECH('PTEREF','L',ITREF)
C
C ---- RECUPERATION  DU NUMERO D'HARMONIQUE
C      ------------------------------------
      CALL TECACH('NNN','PHARMON',1,IHARMO,IRET)
      IF (IHARMO.NE.0) THEN
        NH    = ZI(IHARMO)
        NHARM = DBLE(NH)
      ENDIF
C
C ---- CALCUL DES CONTRAINTES 'VRAIES' SUR L'ELEMENT
C ---- (I.E.  1/2*SIGMA_MECA - SIGMA_THERMIQUES)
C      ------------------------------------
      CALL SIMTEP('RIGI',MODELI,NNO,NDIM,NBSIG,NPG,IPOIDS,IVF,IDFDE,
     +            ZR(IGEOM),ZR(IDEPL),ZR(ITEMPE),ZR(ITREF),INSTAN,
     +            REPERE,ZI(IMATE),NHARM,SIGMA)
C
C ---- CALCUL DU VECTEUR DES FORCES INTERNES (BT*SIGMA)
C      -----------------------------------------------
      CALL BSIGMC ( MODELI, NNO, NDIM, NBSIG, NPG, IPOIDS, IVF, IDFDE,
     +              ZR(IGEOM), NHARM, SIGMA, BSIGMA )
C
C ---- CALCUL DU TERME EPSTH_T*D*EPSTH
C      -------------------------------
      CALL ETHDST('RIGI',MODELI,NNO,NDIM,NBSIG,NPG,IPOIDS,IVF,IDFDE,
     +            ZR(IGEOM),ZR(IDEPL),ZR(ITEMPE),ZR(ITREF),INSTAN,
     +            REPERE,ZI(IMATE),OPTION,ENTHTH)
C
C ---- CALCUL DE L'ENERGIE POTENTIELLE :
C ----        1/2*UT*K*U - UT*FTH + 1/2*EPSTHT*D*EPSTH :
C             ----------------------------------------

      EPOT = ZERO
C
      DO 30 I = 1, NDIM*NNO
         EPOT =  EPOT + BSIGMA(I)*ZR(IDEPL+I-1)
 30   CONTINUE
C
      EPOT = EPOT + UNDEMI*ENTHTH
C
C ---- RECUPERATION ET AFFECTATION DU REEL EN SORTIE
C ---- AVEC L'ENERGIE DE DEFORMATION
C      -----------------------------
      CALL JEVECH('PENERDR','E',IENER)
C
      ZR(IENER) = EPOT
C
      END

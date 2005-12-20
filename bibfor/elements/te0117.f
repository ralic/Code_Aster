      SUBROUTINE TE0117 ( OPTION , NOMTE )
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
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16        OPTION , NOMTE
C ----------------------------------------------------------------------
C FONCTION REALISEE:  CALCUL DE L'OPTION FORC_NODA
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
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
      CHARACTER*8        MODELI
      REAL*8             NHARM, BSIGM(18)
      INTEGER            NBSIGM,NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE
      INTEGER            JGANO
C DEB ------------------------------------------------------------------

      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
      MODELI(1:2) = NOMTE(3:4)
C
C --- INITIALISATIONS :
C     -----------------
      ZERO  = 0.0D0
      NHARM = ZERO
C
C ---- RECUPERATION  DU NUMERO D'HARMONIQUE
C      ------------------------------------
      CALL JEVECH('PHARMON','L',IHARMO)
      NH    = ZI(IHARMO)
      NHARM = DBLE(NH)
C
C ---- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
C      -----------------------------------------
      NBSIG = NBSIGM(MODELI)
C
C ---- PARAMETRES EN ENTREE
C      --------------------
C ----     COORDONNEES DES CONNECTIVITES
      CALL JEVECH('PGEOMER','L',IGEOM)
C ----     CONTRAINTES AUX POINTS D'INTEGRATION
      CALL JEVECH('PCONTMR','L',ICONTM)
C
C ---- PARAMETRES EN SORTIE
C      --------------------
C ----     VECTEUR DES FORCES INTERNES (BT*SIGMA)
      CALL JEVECH('PVECTUR','E',IVECTU)
C
C ---- CALCUL DU VECTEUR DES FORCES INTERNES (BT*SIGMA) :
C      --------------------------------------------------
      CALL BSIGMC (MODELI,NNO,NDIM,NBSIG,NPG,IPOIDS,IVF,IDFDE, 
     +              ZR(IGEOM), NHARM, ZR(ICONTM), BSIGM )
C
C ---- AFFECTATION DU VECTEUR EN SORTIE :
C      ----------------------------------
       DO 10 I=1,NDIM*NNO
          ZR(IVECTU+I-1) = BSIGM(I)
10     CONTINUE
C
C FIN ------------------------------------------------------------------
      END

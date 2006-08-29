      SUBROUTINE TE0116 ( OPTION , NOMTE )
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
C ......................................................................
      IMPLICIT REAL*8 (A-H,O-Z)
C     BUT: CALCUL DES CONTRAINTES AUX NOEUDS EN MECANIQUE
C          ELEMENTS ISOPARAMETRIQUES 2D FOURIER
C
C            OPTION : 'SIGM_ELNO_DEPL   '
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      CHARACTER*8      MODELI
      CHARACTER*16     OPTION, NOMTE
      CHARACTER*4      FAMI
      REAL*8           SIGMA(54), REPERE(7), R8BID1(9)
      REAL*8           NHARM, INSTAN
      INTEGER          NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO,DIMMOD
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
      IF (OPTION.EQ.'SIGM_ELNO_DEPL') THEN
        CALL ELREF4(' ','GANO',NDIM,NNO,NNOS,NPG,IPOIDS,
     &                                           IVF,IDFDE,JGANO)
        FAMI='GANO'
      ELSE
        CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,
     &                                           IVF,IDFDE,JGANO)
        FAMI='RIGI'
      ENDIF
      MODELI(1:2) = NOMTE(3:4)
      DIMMOD = 3
C
C ---- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
C      -----------------------------------------
      NBSIG  = NBSIGM(MODELI)
C
C --- INITIALISATIONS :
C     -----------------
      ZERO     = 0.0D0
      INSTAN   = ZERO
      NHARM    = ZERO
      CALL R8INIR(9,0.D0,R8BID1,1)

C
      DO 10 I = 1, NBSIG*NPG
         SIGMA(I) = ZERO
 10   CONTINUE
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
      CALL ORTREP(ZI(IMATE),NDIM,REPERE)
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
      CALL JEVECH('PHARMON','L',IHARMO)
      NH    = ZI(IHARMO)
      NHARM = DBLE(NH)
C
C ---- RECUPERATION DU VECTEUR DES CONTRAINTES EN SORTIE
C      -------------------------------------------------
      CALL JEVECH('PCONTRR','E',ICONT)
      
C
C ---- CALCUL DES CONTRAINTES 'VRAIES' AUX POINTS D'INTEGRATION
C ---- DE L'ELEMENT :
C ---- (I.E. SIGMA_MECA - SIGMA_THERMIQUES)
C      ------------------------------------
      CALL SIGVMC(FAMI,MODELI,NNO,DIMMOD,NBSIG,NPG,IPOIDS,IVF,
     +            IDFDE,ZR(IGEOM),ZR(IDEPL),ZR(ITEMPE),
     +            ZR(ITREF),INSTAN,REPERE,
     +            ZI(IMATE),NHARM,SIGMA,.FALSE.)
C
      CALL PPGAN2(JGANO,NBSIG,SIGMA,ZR(ICONT))
C
      END

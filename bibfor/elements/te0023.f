      SUBROUTINE TE0023 ( OPTION , NOMTE )
      IMPLICIT   NONE
      CHARACTER*16        OPTION , NOMTE
C.......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 02/10/2002   AUTEUR ASSIRE A.ASSIRE 
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
C     BUT: CALCUL DES CONTRAINTES AUX NOEUDS PAR EXTRAPOLATION
C                 DES CONTRAINTES AUX POINTS DE GAUSS
C
C          ELEMENTS ISOPARAMETRIQUES 3D
C
C          OPTIONS : 'SIGM_ELNO_DEPL'
C                    'SIGM_ELNO_DEPL_C'
C
C     ENTREES  ---> OPTION : OPTION DE CALCUL
C              ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      COMMON  / IVARJE / ZI(1)
      COMMON  / RVARJE / ZR(1)
      COMMON  / CVARJE / ZC(1)
      COMMON  / LVARJE / ZL(1)
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
      INTEGER            ZI
      REAL*8             ZR
      COMPLEX*16         ZC
      LOGICAL            ZL
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      CHARACTER*8    ELREFE, MODELI
      CHARACTER*24   CHVAL,CHCTE
      REAL*8         SIGMA(162), CONTNO(162), REPERE(7)
      REAL*8         NHARM, INSTAN, ZERO, DEPLA(81),SIGM2(162)
      LOGICAL        CPX,LSENS
      INTEGER        NBPG(10), NBSIGM, NITER, I, ICONT, IDEPL, ITER,
     +               IDEPLC, IDFDE, IDFDK, IDFDN, IGEOM, IMATE, J, INO,
     +               IPOIDS, ITEMPE, ITREF, IVF, JIN, JVAL, NBFPG,
     +               NBINCO, NBSIG, NDIM, NNO, NNOS, NPG, IDEPS
C     ------------------------------------------------------------------
C
      CALL ELREF1(ELREFE)
      MODELI(1:2) = NOMTE(3:4)
C
      CPX = OPTION .EQ. 'SIGM_ELNO_DEPL_C'
      IF ( CPX ) THEN
         NITER = 2
      ELSE
         NITER = 1
      ENDIF
C
C ---- CARACTERISTIQUES DU TYPE D'ELEMENT :
C ---- GEOMETRIE ET INTEGRATION
C      ------------------------
      CHCTE = '&INEL.'//ELREFE//'.CARACTE'
      CALL JEVETE(CHCTE,'L',JIN)
      NDIM = ZI(JIN+1-1)
      NNO = ZI(JIN+2-1)
      NBFPG = ZI(JIN+3-1)
      DO 10 I = 1,NBFPG
         NBPG(I) = ZI(JIN+3-1+I)
  10  CONTINUE
      NNOS = ZI(JIN+3-1+NBFPG+1)
      NPG = NBPG(1)
      IF ( ELREFE.EQ.'TETRA10 ' .OR. ELREFE.EQ.'HEXA20  ' .OR.
     &    ELREFE.EQ.'HEXA27  '                         ) THEN
         NPG = NBPG(3)
      ELSEIF ( ELREFE.EQ.'PENTA15 ' ) THEN
         NPG = NBPG(2)
      ENDIF
C
      CHVAL = '&INEL.'//ELREFE//'.FFORMES'
      CALL JEVETE(CHVAL,'L',JVAL)
C
      IPOIDS = JVAL + (NDIM+1)*NNO*NNO
      IF ( ELREFE.EQ.'TETRA10 ' .OR. ELREFE.EQ.'HEXA20  ' .OR.
     &    ELREFE.EQ.'HEXA27  '                         ) THEN
         IPOIDS = IPOIDS + NBPG(1)*(1+(NDIM+1)*NNO)
     &                   + NBPG(2)*(1+(NDIM+1)*NNO)
      ELSEIF ( ELREFE.EQ.'PENTA15 ' ) THEN
         IPOIDS = IPOIDS + NBPG(1)*(1+(NDIM+1)*NNO)
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
      ZERO   = 0.0D0
      INSTAN = ZERO
      NHARM  = ZERO
      IF (OPTION(11:14).EQ.'SENS') THEN
        LSENS = .TRUE.
      ELSE
        LSENS = .FALSE.
      ENDIF
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
      NBINCO = NNO*NDIM
      IF ( CPX ) THEN
         CALL JEVECH('PDEPLAC','L',IDEPLC)
      ELSE
         CALL JEVECH('PDEPLAR','L',IDEPL)
      ENDIF
C
C ---- RECUPERATION DU CHAMP DE DEPLACEMENT DERIVE SUR L'ELEMENT
C      ---------------------------------------------------------
      IF (LSENS) CALL JEVECH('PDEPSEN','L',IDEPS)
C
C ---- RECUPERATION DU CHAMP DE TEMPERATURE SUR L'ELEMENT
C      --------------------------------------------------
      CALL JEVECH('PTEMPER','L',ITEMPE)
C
C ---- RECUPERATION DE LA TEMPERATURE DE REFERENCE
C      -------------------------------------------
      CALL JEVECH('PTEREF','L',ITREF)
C
      DO 1000 ITER = 1,NITER
C
      DO 20 I = 1, NBSIG*NPG
         SIGMA(I) = ZERO
 20   CONTINUE
C
      IF ( CPX ) THEN
         IF ( ITER .EQ. 1 ) THEN
            DO 21 I=1,NBINCO
               DEPLA(I) = DBLE(ZC(IDEPLC-1+I))
   21       CONTINUE
         ELSE
            DO 22 I=1,NBINCO
               DEPLA(I) = DIMAG(DCMPLX(ZC(IDEPLC-1+I)))
   22       CONTINUE
         ENDIF
      ELSE
         DO 23 I=1,NBINCO
            DEPLA(I) = ZR(IDEPL-1+I)
   23    CONTINUE
      ENDIF
C
C ---- CALCUL DES CONTRAINTES 'VRAIES' AUX POINTS D'INTEGRATION
C ---- DE L'ELEMENT :
C ---- (I.E. SIGMA_MECA - SIGMA_THERMIQUES)
C      ------------------------------------
      CALL SIGVMC(MODELI,NNO,NDIM,NBSIG,NPG,ZR(IVF),ZR(IDFDE),ZR(IDFDN),
     +            ZR(IDFDK),ZR(IPOIDS),ZR(IGEOM),DEPLA,ZR(ITEMPE),
     +            ZR(ITREF),INSTAN,REPERE,ZI(IMATE),NHARM,SIGMA,.FALSE.)
C
C
C ---- CALC DU TERME COMPLEMENTAIRE DE CONTR 'VRAIES' SUR L'ELEMENT
C ---- DANS LE CAS DE LA SENSIBILITE (TERME DA/DP*B*U)
C ---- (I.E. SIGMA_MECA - SIGMA_THERMIQUES)
C ATTENTION!! POUR L'INSTANT(30/9/02) ON DOIT AVOIR SIGMA_THERMIQUE=0
C      ------------------------------------
      IF (LSENS) THEN
        DO 25 I=1,NBINCO
          DEPLA(I) = ZR(IDEPS-1+I)
   25   CONTINUE
        CALL SIGVMC(MODELI,NNO,NDIM,NBSIG,NPG,ZR(IVF),ZR(IDFDE),
     +            ZR(IDFDN),
     +            ZR(IDFDK),ZR(IPOIDS),ZR(IGEOM),DEPLA,ZR(ITEMPE),
     +            ZR(ITREF),INSTAN,REPERE,ZI(IMATE),NHARM,SIGM2,.TRUE.)
        DO 15 I=1, NBSIG*NPG
          SIGMA(I) = SIGMA(I) + SIGM2(I)
   15   CONTINUE
      ENDIF
C
      CALL PPGANO ( NNOS,NPG,NBSIG,SIGMA,CONTNO)
C
C ---- RECUPERATION ET AFFECTATION DU VECTEUR EN SORTIE
C ---- AVEC LE VECTEUR DES CONTRAINTES AUX NOEUDS
C      ------------------------------------------
C
      IF ( CPX ) THEN
         CALL JEVECH('PCONTRC','E',ICONT)
         IF ( ITER .EQ. 1 ) THEN
            DO 120 INO = 1,NNO
            DO 120 J = 1,6
               ZC(ICONT+6*(INO-1)-1+J) = CONTNO(6*(INO-1)+J)
120         CONTINUE
         ELSE
            DO 130 INO = 1,NNO
            DO 130 J = 1,6
                  ZC(ICONT+6*(INO-1)-1+J) =
     .  DCMPLX(DBLE(ZC(ICONT+6*(INO-1)-1+J)),DBLE(CONTNO(6*(INO-1)+J)))
130         CONTINUE
         ENDIF
      ELSE
         CALL JEVECH('PCONTRR','E',ICONT)
         DO 110 INO = 1,NNO
         DO 110 J = 1,6
            ZR(ICONT+6*(INO-1)-1+J) = CONTNO(6*(INO-1)+J)
110      CONTINUE
      ENDIF
 1000 CONTINUE
C
      END

      SUBROUTINE TE0086 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 04/05/2004   AUTEUR SMICHEL S.MICHEL-PONNELLE 
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
C    - FONCTION REALISEE:  CALCUL DES CONTRAINTES EN 2D
C                          OPTION : 'SIGM_ELNO_DEPL  '
C                             OU  : 'SIEF_ELGA_DEPL  '
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      CHARACTER*8      MODELI
      REAL*8           SIGMA(54), REPERE(7),SIGM2(54)
      REAL*8           NHARM, INSTAN, DEPLA(36),CONTNO(54)
      LOGICAL          CPX,LSENS
      INTEGER          IHYDR, ISECH, ISREF      
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
      IF ( OPTION(6:9) .EQ.'ELNO' ) THEN
        CALL ELREF4(' ','GANO',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
      ELSE
        CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
      ENDIF
      MODELI(1:2) = NOMTE(3:4)
      
      CPX = OPTION .EQ. 'SIGM_ELNO_DEPL_C'
      IF (CPX) THEN
        NITER = 2
      ELSE
        NITER = 1
      END IF
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
      IF (OPTION(11:14).EQ.'SENS') THEN
        LSENS = .TRUE.
      ELSE
        LSENS = .FALSE.
      ENDIF
C
      DO 10 I = 1, NBSIG2*NPG
         SIGMA(I)  = ZERO
10    CONTINUE
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
      IF (CPX) THEN
        CALL JEVECH('PDEPLAC','L',IDEPLC)
      ELSE
        CALL JEVECH('PDEPLAR','L',IDEPL)
      END IF
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

C ---- RECUPERATION DU CHAMP DE L'HDRATATION SUR L'ELEMENT
C      --------------------------------------------------
      CALL JEVECH('PHYDRER','L',IHYDR)

C ---- RECUPERATION DU CHAMP DU SECHAGE SUR L'ELEMENT
C      --------------------------------------------------
      CALL JEVECH('PSECHER','L',ISECH)      

C ---- RECUPERATION DU SECHAGE DE REFERENCE
C      -------------------------------------------
      CALL JEVECH('PSECREF','L',ISREF)

C-----------------------------------------------------------------

      DO 150 ITER = 1,NITER

        DO 20 I = 1,NBSIG*NPG
          SIGMA(I) = ZERO
20      CONTINUE

        IF (CPX) THEN
          IF (ITER.EQ.1) THEN
            DO 30 I = 1,NBINCO
              DEPLA(I) = DBLE(ZC(IDEPLC-1+I))
30          CONTINUE
          ELSE
            DO 40 I = 1,NBINCO
              DEPLA(I) = DIMAG(DCMPLX(ZC(IDEPLC-1+I)))
40          CONTINUE
          END IF
        ELSE
          DO 50 I = 1,NBINCO
            DEPLA(I) = ZR(IDEPL-1+I)
50        CONTINUE
        END IF

C
C ---- CALCUL DES CONTRAINTES 'VRAIES' AUX POINTS D'INTEGRATION
C ---- DE L'ELEMENT :
C ---- (I.E. SIGMA_MECA - SIGMA_THERMIQUES)
C      ------------------------------------
      CALL SIGVMC(MODELI,NNO,NDIM,NBSIG1,NPG,IPOIDS,IVF,IDFDE,
     +            ZR(IGEOM),DEPLA,ZR(ITEMPE),ZR(ITREF),
     +            ZR(IHYDR),ZR(ISECH),ZR(ISREF), INSTAN,REPERE,
     +            ZI(IMATE),NHARM,SIGMA,.FALSE.)
C
C
C ---- CALC DU TERME COMPLEMENTAIRE DE CONTR 'VRAIES' SUR L'ELEMENT
C ---- DANS LE CAS DE LA SENSIBILITE (TERME DA/DP*B*U)
C ---- (I.E. SIGMA_MECA - SIGMA_THERMIQUES)
C ATTENTION!! POUR L'INSTANT(30/9/02) ON DOIT AVOIR SIGMA_THERMIQUE=0
C      ------------------------------------
      IF (LSENS) THEN
          DO 60 I = 1,NBINCO
            DEPLA(I) = ZR(IDEPS-1+I)
60        CONTINUE
        CALL SIGVMC(MODELI,NNO,NDIM,NBSIG1,NPG,IPOIDS,IVF,IDFDE,
     +              ZR(IGEOM),DEPLA,ZR(ITEMPE),ZR(ITREF),
     +              ZR(IHYDR),ZR(ISECH),ZR(ISREF),INSTAN,REPERE,
     +              ZI(IMATE),NHARM,SIGM2,.TRUE.)
        DO 70 I=1, NBSIG*NPG
          SIGMA(I) = SIGMA(I) + SIGM2(I)
70      CONTINUE
      ENDIF
C
      IF (OPTION(6:9).EQ.'ELGA') THEN
      
        CALL JEVECH('PCONTRR','E',ICONT)
          
C         --------------------
C ---- AFFECTATION DU VECTEUR EN SORTIE AVEC LES CONTRAINTES AUX
C ---- POINTS D'INTEGRATION
C      --------------------
        DO 80 IGAU = 1, NPG
        DO 80 ISIG = 1, NBSIG
          ZR(ICONT+NBSIG*(IGAU-1)+ISIG-1) = SIGMA(NBSIG*(IGAU-1)+ISIG)
80     CONTINUE
C
      ELSE
C
        CALL PPGAN2(JGANO,NBSIG,SIGMA,CONTNO)
C
C
C ---- RECUPERATION ET AFFECTATION DU VECTEUR EN SORTIE
C ---- AVEC LE VECTEUR DES CONTRAINTES AUX NOEUDS
C      ------------------------------------------

        IF (CPX) THEN
          CALL JEVECH('PCONTRC','E',ICONT)
          IF (ITER.EQ.1) THEN
            DO 100 INO = 1,NNO
              DO 90 J = 1,NBSIG
                ZC(ICONT+NBSIG* (INO-1)-1+J) = CONTNO(NBSIG* (INO-1)+J)
   90         CONTINUE
  100       CONTINUE
          ELSE
            DO 120 INO = 1,NNO
              DO 110 J = 1,NBSIG
                ZC(ICONT+NBSIG* (INO-1)-1+J) = DCMPLX(DBLE(ZC(ICONT+
     &                                 NBSIG*(INO-1)-1+J)),DBLE(CONTNO
     &                                 (NBSIG*(INO-1)+J)))
  110         CONTINUE
  120       CONTINUE
          END IF
        ELSE 
          CALL JEVECH('PCONTRR','E',ICONT)
          DO 140 INO = 1,NNO
            DO 130 J = 1,NBSIG
              ZR(ICONT+NBSIG* (INO-1)-1+J) = CONTNO(NBSIG* (INO-1)+J)
  130       CONTINUE
  140     CONTINUE
        END IF
      END IF

  150 CONTINUE
  

      END

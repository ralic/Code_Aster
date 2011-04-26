      SUBROUTINE TE0008 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
      REAL*8             NHARM, BSIGM(81),GEO(81),SIGTMP(162),FTEMP(81)
      INTEGER            NBSIGM
C DEB ------------------------------------------------------------------
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDE,JGANO)
C
C --- INITIALISATIONS :
C     -----------------
      ZERO  = 0.0D0
      NHARM = ZERO
C
C ---- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
C      -----------------------------------------
      NBSIG = NBSIGM()
C
C ---- PARAMETRES EN ENTREE
C      --------------------
C ----     COORDONNEES DES CONNECTIVITES
      CALL JEVECH('PGEOMER','L',IGEOM)

C         CHAMPS POUR LA REACTUALISATION DE LA GEOMETRIE
      DO 30 I = 1,NDIM*NNO
         GEO(I)  =ZR(IGEOM-1+I)
30    CONTINUE

C
C ---- PARAMETRES EN SORTIE
C      --------------------
C ----     VECTEUR DES FORCES INTERNES (BT*SIGMA)
      CALL JEVECH('PVECTUR','E',IVECTU)

C ---- CALCUL DE FORC_NODA

      IF (OPTION.EQ.'FORC_NODA') THEN
C      --------------------

        CALL TECACH('ONN','PDEPLMR',1,IDEPL,IRETD)
        CALL TECACH('ONN','PCOMPOR',1,ICOMP,IRETC)
        IF ((IRETD.EQ.0).AND.(IRETC.EQ.0)) THEN
           IF (ZK16(ICOMP+2)(1:6).NE.'PETIT ') THEN
              DO 20 I = 1,NDIM*NNO
                 GEO(I)  =GEO(I)  + ZR(IDEPL-1+I)
20            CONTINUE
           ENDIF
        ENDIF

C ----     CONTRAINTES AUX POINTS D'INTEGRATION
        CALL JEVECH('PCONTMR','L',ICONTM)
C
C ---- CALCUL DU VECTEUR DES FORCES INTERNES (BT*SIGMA) :
C      --------------------------------------------------
      CALL BSIGMC (NNO,NDIM,NBSIG,NPG1,IPOIDS,IVF,IDFDE,
     +              GEO, NHARM, ZR(ICONTM), BSIGM )
C
C ---- AFFECTATION DU VECTEUR EN SORTIE :
C      ----------------------------------
         DO 10 I=1,NDIM*NNO
            ZR(IVECTU+I-1) = BSIGM(I)
10       CONTINUE

      ELSE IF (OPTION.EQ.'REFE_FORC_NODA') THEN

        CALL JEVECH('PREFCO','L',ICONTM)

        CALL R8INIR(NBSIG*NPG1,0.D0,SIGTMP,1)
        CALL R8INIR(NDIM*NNO,0.D0,FTEMP,1)
        DO 200 I=1,NBSIG*NPG1

          SIGTMP(I)=ZR(ICONTM)
          CALL BSIGMC ( NNO, NDIM, NBSIG, NPG1, IPOIDS, IVF,
     +                 IDFDE,GEO, NHARM, SIGTMP, BSIGM )


          DO 21 J=1,NDIM*NNO
            FTEMP(J) = FTEMP(J)+ABS(BSIGM(J))
21        CONTINUE

          SIGTMP(I)=0.D0

200      CONTINUE

         CALL DAXPY(NDIM*NNO,1.D0/NPG1,FTEMP,1,ZR(IVECTU),1)

      ENDIF
C
C FIN ------------------------------------------------------------------
      END

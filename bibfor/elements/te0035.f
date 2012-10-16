      SUBROUTINE TE0035 ( OPTION , NOMTE )
      IMPLICIT  NONE
      INCLUDE 'jeveux.h'
      CHARACTER*16        OPTION , NOMTE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 16/10/2012   AUTEUR SELLENET N.SELLENET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
C                          POUR LES ELEMENTS DKT, DST, DKQ, DSQ ET Q4G
C                          OPTIONS : 'CHAR_MECA_TEMP_R'
C                                    'CHAR_MECA_EPSI_R'
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
      INTEGER      NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDX,JGANO
      INTEGER      I,JGEOM,JCACO,JVECG,IDEFI,IRET
      REAL*8       PGL(3,3) , XYZL(3,4)
      REAL*8       EPSINI(6)
      REAL*8       BSIGMA(24), SIGT(32)
C ----------------------------------------------------------------------
C
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDX,JGANO)
C
      CALL JEVECH ( 'PGEOMER' , 'L' , JGEOM )
      CALL JEVECH ( 'PCACOQU' , 'L' , JCACO )
      CALL JEVECH ( 'PVECTUR' , 'E' , JVECG )
C
C --- DETERMINATION DE LA MATRICE DE PASSAGE DU REPERE GLOBAL
C --- AU REPERE LOCAL A L'ELEMENT
C     ---------------------------
      IF (     NNO .EQ. 3) THEN
         CALL DXTPGL ( ZR(JGEOM) , PGL )
      ELSE IF( NNO .EQ. 4) THEN
         CALL DXQPGL ( ZR(JGEOM) , PGL, 'S', IRET )
      ENDIF
C
C --- DETERMINATION DES COORDONNEES DES CONNECTIVITES DE L'ELEMENT
C --- DANS SON REPERE LOCAL
C     ---------------------
      CALL UTPVGL ( NNO , 3 , PGL , ZR(JGEOM) , XYZL )
C
C
C --- CALCUL DES EFFORTS GENERALISES D'ORIGINE THERMIQUE
C --- AUX POINTS D'INTEGRATION
C     ------------------------
      IF (OPTION .EQ.'CHAR_MECA_TEMP_R') THEN
C
        CALL DXEFGT(PGL, SIGT)
C
      ELSEIF (OPTION .EQ.'CHAR_MECA_EPSI_R') THEN
C
        CALL JEVECH ( 'PEPSINR' , 'L' , IDEFI )
C
        EPSINI(1) = ZR(IDEFI+1-1)
        EPSINI(2) = ZR(IDEFI+2-1)
        EPSINI(3) = ZR(IDEFI+3-1)
        EPSINI(4) = ZR(IDEFI+4-1)
        EPSINI(5) = ZR(IDEFI+5-1)
        EPSINI(6) = ZR(IDEFI+6-1)
C
        CALL DXEFGI(NOMTE, XYZL, PGL, EPSINI, SIGT)
C
      ENDIF
C
C --- CALCUL DES EFFORTS INTERNES D'ORIGINE THERMIQUE
C --- (I.E. SOMME_VOL(BT_SIG))
C     ------------------------
      CALL DXBSIG(NOMTE, XYZL, PGL, SIGT, BSIGMA)
C
C --- AFFECTATION DU VECTEUR DES FORCES ELEMENTAIRES EN SORTIE DU TE
C     --------------------------------------------------------------
      DO 20 I = 1, NNO*6
         ZR(JVECG+I-1) = BSIGMA(I)
  20  CONTINUE
C
      END

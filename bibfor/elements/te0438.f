      SUBROUTINE TE0438(OPTION,NOMTE)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16 OPTION,NOMTE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C ----------------------------------------------------------------------

C     BUT:       POUR LES ELEMENTS QUASI-INCOMPRESSIBLES 3D , CALCUL DES
C                GRANDEURS EQUIVALENTES SUIVANTES
C                AUX NOEUDS :
C                    POUR LES CONTRAINTES  A PARTIR DE SIGM_ELNO_DEPL
C                    POUR LES DEFORMATIONS A PARTIR DE EPSI_ELNO_DEPL

C                DANS CET ORDRE :

C                . CONTRAINTES EQUIVALENTES  :
C                        . VON MISES                    (= 1 VALEUR)
C                        . TRESCA                       (= 1 VALEUR)
C                        . CONTRAINTES PRINCIPALES      (= 3 VALEURS)
C                        . VON-MISES * SIGNE (PRESSION) (= 1 VALEUR)
C               . DEFORMATIONS EQUIVALENTES  :
C                        . SECOND INVARIANT             (= 1 VALEUR)
C                        . DEFORMATIONS PRINCIPALES     (= 3 VALEURS)
C                        . 2EME INV. * SIGNE (1ER.INV.) (= 1 VALEUR)

C     OPTIONS :  'EQUI_ELNO_SIGM'
C                'EQUI_ELNO_EPSI'

C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C     ------------------------------------------------------------------
      PARAMETER (NNOMAX=27,NEQMAX=6,NPGMAX=27)
      REAL*8 EQNO(NEQMAX*NNOMAX),EQPG(NEQMAX*NPGMAX)
C     ------------------------------------------------------------------

      IF (OPTION(11:14).EQ.'EPSI') THEN
        NCEQ = 5
      ELSE IF (OPTION(11:14).EQ.'SIGM') THEN
        NCEQ = 6
      END IF

      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)

      DO 10 I = 1,NCEQ*NNO
        EQNO(I) = 0.0D0
   10 CONTINUE

C -   DEFORMATIONS EQUIVALENTES AUX NOEUDS :
C     ------------------------------------

      IF (OPTION(11:14).EQ.'EPSI') THEN

        CALL JEVECH('PDEFORR','L',IDEFO)
        CALL JEVECH('PDEFOEQ','E',IEQUIF)

        DO 20 INO = 1,NNO
          IDCP = (INO-1)*NCEQ
          CALL FGEQUI(ZR(IDEFO+ (INO-1)*6),'EPSI',3,EQNO(IDCP+1))
   20   CONTINUE

C --- CONTRAINTES EQUIVALENTES AUX NOEUDS :
C     -----------------------------------

      ELSE IF (OPTION(11:14).EQ.'SIGM') THEN

        CALL JEVECH('PCONTRR','L',ICONT)
        CALL JEVECH('PCONTEQ','E',IEQUIF)

        DO 30 KP = 1,NPG
          IDCP = (KP-1)*NCEQ
          CALL FGEQUI(ZR(ICONT+ (KP-1)*7),'SIGM',3,EQPG(IDCP+1))
   30   CONTINUE

C -      EXTRAPOLATION AUX NOEUDS

        CALL PPGAN2(JGANO,NCEQ,EQPG,ZR(IEQUIF))

      ELSE
        CALL U2MESK('F','PREPOST3_82',1,OPTION)

      END IF


C -   STOCKAGE :
C     --------
      IF (OPTION(11:14).NE.'SIGM') THEN
        DO 50 INO = 1,NNO
          DO 40 J = 1,NCEQ
            ZR(IEQUIF-1+ (INO-1)*NCEQ+J) = EQNO((INO-1)*NCEQ+J)
   40     CONTINUE
   50   CONTINUE
      END IF

      END

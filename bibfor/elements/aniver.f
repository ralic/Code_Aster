      SUBROUTINE  ANIVER(MATER)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C.======================================================================
      IMPLICIT NONE
C
C      ANIVER --   CALCUL DES VALEURS PROPRES DE LA MATRICE
C                  HOOKE POUR S'ASSURER QUE CELLE EST BIEN
C                  DEFINIE POSITIVE DANS LE CAS DE L'ORTHOTROPIE
C                  OU DE L'ISOTROPIE TRANSVERSE
C
C   ARGUMENT        E/S  TYPE         ROLE
C    MATER          IN     K8       MATERIAU
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
      INCLUDE 'jeveux.h'
           CHARACTER*8  MATER
C -----  VARIABLES LOCALES
           CHARACTER*2  M2BLAN
           CHARACTER*2  K8BID
           CHARACTER*16 NOMRC
           CHARACTER*19 NOOBRC
C
           REAL*8 DORTH(6,6)
           REAL*8 NU12, NU21, NU13, NU31, NU23, NU32
C
C
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C ---- INITIALISATIONS
C      ---------------
C-----------------------------------------------------------------------
      INTEGER I ,IEL ,IEN ,IET ,IGLN ,IGLT ,IGTN
      INTEGER INULN ,INULT ,INUTN ,J ,JNOMRC ,JTYPFO ,JVALRK
      INTEGER JVALRM ,K ,NBCRME ,NBR ,NDIM, INDIK8
      REAL*8 C1 ,DELTA ,DEUX ,E1 ,E2 ,E3 ,G12
      REAL*8 G13 ,G23 ,UN ,UNDEMI ,ZERO
C-----------------------------------------------------------------------
      ZERO   = 0.0D0
      UNDEMI = 0.5D0
      UN     = 1.0D0
      DEUX   = 2.0D0
C
      M2BLAN = ' '
      K8BID  = ' '
C
      DO 10 I = 1, 6
      DO 10 J = 1, 6
        DORTH(I,J) = ZERO
  10  CONTINUE
C
      E1   = ZERO
      E2   = ZERO
      E3   = ZERO
      G12  = ZERO
      G23  = ZERO
      G13  = ZERO
      NU12 = ZERO
      NU23 = ZERO
      NU13 = ZERO
C
C --- RECUPERATION DU NOMBRE DE RELATIONS DE COMPORTEMENT :
C     ---------------------------------------------------
      CALL JELIRA(MATER//'.MATERIAU.NOMRC','LONMAX', NBCRME, K8BID)
C
C --- RECUPERATION DU TABLEAU DES RELATIONS DE COMPORTEMENT :
C     -----------------------------------------------------
      CALL JEVEUO(MATER//'.MATERIAU.NOMRC','L', JNOMRC)
C
C --- RECUPERATION DE L'INFORMATION MATERIAU FONCTION OU NON :
C     ------------------------------------------------------
      CALL JEVEUO('&&OP0005.TYPFON','L', JTYPFO)
C
C --- BOUCLE SUR LES RELATIONS DE COMPORTEMENT :
C     ----------------------------------------
      DO 20 K = 1, NBCRME
        NOMRC  = ZK16(JNOMRC+K-1)
        NOOBRC = MATER//'.'//ZK16(JNOMRC+K-1)(1:10)
C
C --- SI LE MATERIAU N'EST PAS UNE FONCTION :
C     -------------------------------------
        IF (.NOT.ZL(JTYPFO+K-1)) THEN
C
C ---   ON NE TRAITE QUE LES CAS ISOTROPE-TRANSVERSE ET ORTHOTROPE :
C       ----------------------------------------------------------
        IF (NOMRC.EQ.'ELAS_ISTR'.OR.NOMRC.EQ.'ELAS_ORTH') THEN
C
C ---     RECUPERATION DU NOM DES COMPOSANTES ET DES VALEURS
C ---     DEFINISSANT LE MATERIAU :
C         -----------------------
          CALL JEVEUO(NOOBRC//'.VALR','L', JVALRM)
          CALL JEVEUO(NOOBRC//'.VALK','L', JVALRK)
C
C ---     LONGUEUR DU TABLEAU DES COMPOSANTES :
C         -----------------------------------
          CALL JELIRA(NOOBRC//'.VALR','LONUTI',NBR, K8BID)
C
C ---     RECUPERATION DES INDICES DES COMPOSANTES RELATIVES
C ---     A L'ORTHOTROPIE ET A L'ISOTROPIE TRANSVERSE DANS
C ---     LE TABLEAU DU NOM DES COMPOSANTES :
C         ---------------------------------
          IEL = INDIK8(ZK8(JVALRK),'E_L',1,NBR)
          IET = INDIK8(ZK8(JVALRK),'E_T',1,NBR)
          IEN = INDIK8(ZK8(JVALRK),'E_N',1,NBR)
C
          IGLT = INDIK8(ZK8(JVALRK),'G_LT',1,NBR)
          IGTN = INDIK8(ZK8(JVALRK),'G_TN',1,NBR)
          IGLN = INDIK8(ZK8(JVALRK),'G_LN',1,NBR)
C
          INULT = INDIK8(ZK8(JVALRK),'NU_LT',1,NBR)
          INUTN = INDIK8(ZK8(JVALRK),'NU_TN',1,NBR)
          INULN = INDIK8(ZK8(JVALRK),'NU_LN',1,NBR)
C
C ---     RECUPERATION DES COMPOSANTES RELATIVES A L'ORTHOTROPIE
C ---     ET A L'ISOTROPIE TRANSVERSE :
C         ---------------------------
          IF (IEL.NE.0) E1 = ZR(JVALRM+IEL-1)
          IF (IET.NE.0) E2 = ZR(JVALRM+IET-1)
          IF (IEN.NE.0) E3 = ZR(JVALRM+IEN-1)
C
          IF (IGLT.NE.0) G12 = ZR(JVALRM+IGLT-1)
          IF (IGTN.NE.0) G23 = ZR(JVALRM+IGTN-1)
          IF (IGLN.NE.0) G13 = ZR(JVALRM+IGLN-1)
C
          IF (INULT.NE.0) NU12 = ZR(JVALRM+INULT-1)
          IF (INUTN.NE.0) NU23 = ZR(JVALRM+INUTN-1)
          IF (INULN.NE.0) NU13 = ZR(JVALRM+INULN-1)
C
C ---     TRAITEMENT DU CAS DE L'ISOTROPIE TRANSVERSE :
C         -------------------------------------------
          IF (NOMRC.EQ.'ELAS_ISTR') THEN
C
C ---       SI G13 = 0 , ON PEUT SUPPOSER QUE L'ON EST EN 2D
C ---       ON NE TRAITE QUE LE CAS DEFORMATIONS PLANES OU
C ---       AXISYMETRIQUE CAR LE CAS CONTRAINTES PLANES REVIENT
C ---       A L'ELASTICITE ISOTROPE :
C           -----------------------
            IF (IGLN.EQ.0) THEN
              NDIM = 2
              IF (IEN.EQ.0) GOTO 20
              IF (E3.EQ.ZERO) GOTO 20
C
              C1   = E1/(UN+NU12)
              DELTA = UN - NU12 - DEUX*NU13*NU13*E1/E3
C
              DORTH(1,1) = C1*(UN - NU13*NU13*E1/E3)/DELTA
              DORTH(1,2) = C1*((UN - NU13*NU13*E1/E3)/DELTA - UN)
              DORTH(1,3) = E1*NU13/DELTA
              DORTH(2,1) = DORTH(1,2)
              DORTH(2,2) = DORTH(1,1)
              DORTH(2,3) = DORTH(1,3)
              DORTH(3,1) = DORTH(1,3)
              DORTH(3,2) = DORTH(2,3)
              DORTH(3,3) = E3*(UN - NU12)/DELTA
              DORTH(4,4) = UNDEMI*C1
C
C ---         CALCUL DES VALEURS PROPRES DE LA MATRICE DORTH :
C             ----------------------------------------------
              CALL DORTVP(NDIM,NOMRC,DORTH,'DP')
C
C ---       TRAITEMENT DU CAS 3D :
C           --------------------
            ELSEIF (IGLN.NE.0) THEN
              NDIM = 3
              IF (IEN.EQ.0) GOTO 20
              IF (E3.EQ.ZERO) GOTO 20
C
              C1   = E1/(UN+NU12)
              DELTA = UN - NU12 - DEUX*NU13*NU13*E1/E3
C
              DORTH(1,1) = C1*(UN - NU13*NU13*E1/E3)/DELTA
              DORTH(1,2) = C1*((UN - NU13*NU13*E1/E3)/DELTA - UN)
              DORTH(1,3) = E1*NU13/DELTA
              DORTH(2,1) = DORTH(1,2)
              DORTH(2,2) = DORTH(1,1)
              DORTH(2,3) = DORTH(1,3)
              DORTH(3,1) = DORTH(1,3)
              DORTH(3,2) = DORTH(2,3)
              DORTH(3,3) = E3*(UN - NU12)/DELTA
              DORTH(4,4) = UNDEMI*C1
              DORTH(5,5) = G13
              DORTH(6,6) = DORTH(5,5)
C
C ---         CALCUL DES VALEURS PROPRES DE LA MATRICE DORTH :
C             ----------------------------------------------
              CALL DORTVP(NDIM,NOMRC,DORTH,M2BLAN)
C
            ENDIF
C
C ---     TRAITEMENT DU CAS DE L'ORTHOTROPIE :
C         ----------------------------------
          ELSEIF (NOMRC.EQ.'ELAS_ORTH') THEN
C
C ---       SI G13 = 0 , ON PEUT SUPPOSER QUE L'ON EST EN 2D :
C           ------------------------------------------------
            IF (IGLN.EQ.0) THEN
              NDIM = 2
              IF (IET.EQ.0) GOTO 20
              IF (E2.EQ.ZERO) GOTO 20
              IF (E3.EQ.ZERO) GOTO 20
              IF (IEN.EQ.0) THEN
                CALL U2MESS('A','ELEMENTS_9')
                GOTO 100
              ENDIF
C
C ---         TRAITEMENT DES CAS DES DEFORMATIONS PLANES
C ---         ET DE L'AXISYMETRIE :
C             -------------------
              NU21 = E2*NU12/E1
              NU31 = E1*NU13/E3
              NU32 = E2*NU23/E3
              DELTA = UN-NU23*NU32-NU31*NU13-NU21*NU12
     &               -DEUX*NU23*NU31*NU21
C
              DORTH(1,1) = (UN - NU23*NU32)*E1/DELTA
              DORTH(1,2) = (NU21 + NU13*NU32)*E1/DELTA
              DORTH(1,3) = (NU13 + NU21*NU23)*E1/DELTA
              DORTH(2,2) = (UN - NU13*NU31)*E2/DELTA
              DORTH(2,3) = (NU23 + NU13*NU12)*E2/DELTA
              DORTH(3,3) = (UN - NU21*NU12)*E3/DELTA
              DORTH(2,1) = DORTH(1,2)
              DORTH(3,1) = DORTH(1,3)
              DORTH(3,2) = DORTH(2,3)
C
              DORTH(4,4) = G12
C
C ---         CALCUL DES VALEURS PROPRES DE LA MATRICE DORTH :
C             ----------------------------------------------
              CALL DORTVP(NDIM,NOMRC,DORTH,'DP')
C
C ---         TRAITEMENT DU CAS DES CONTRAINTES PLANES :
C             ----------------------------------------
 100          CONTINUE
C
              DO 30 I = 1, 6
              DO 30 J = 1, 6
                DORTH(I,J) = ZERO
  30          CONTINUE
C
              NU21  = E2*NU12/E1
              DELTA = UN-NU12*NU21
C
              DORTH(1,1) = E1/DELTA
              DORTH(1,2) = NU12*E2/DELTA
              DORTH(2,2) = E2/DELTA
              DORTH(2,1) = DORTH(1,2)
C
              DORTH(4,4) = G12
C ---         CALCUL DES VALEURS PROPRES DE LA MATRICE DORTH :
C             ----------------------------------------------
              CALL DORTVP(NDIM,NOMRC,DORTH,'CP')
C
C ---       TRAITEMENT DU CAS 3D :
C           --------------------
            ELSEIF (IGLN.NE.0) THEN
              NDIM = 3
              IF (IET.EQ.0) GOTO 20
              IF (E2.EQ.ZERO) GOTO 20
              IF (E3.EQ.ZERO) GOTO 20
              IF (IEN.EQ.0) THEN
                NDIM = 2
                DO 31 I = 1, 6
                DO 31 J = 1, 6
                  DORTH(I,J) = ZERO
31              CONTINUE
C
                NU21  = E2*NU12/E1
                DELTA = UN-NU12*NU21
C
                DORTH(1,1) = E1/DELTA
                DORTH(1,2) = NU12*E2/DELTA
                DORTH(2,2) = E2/DELTA
                DORTH(2,1) = DORTH(1,2)
                DORTH(4,4) = G12
C ---           CALCUL DES VALEURS PROPRES DE LA MATRICE DORTH :
                CALL DORTVP(NDIM,NOMRC,DORTH,'CP')
              ENDIF
C
              NU21  = E2*NU12/E1
              NU31  = E1*NU13/E3
              NU32  = E2*NU23/E3
              DELTA = UN-NU23*NU32-NU31*NU13-NU21*NU12
     &               -DEUX*NU23*NU31*NU21
C
              DORTH(1,1) = (UN - NU23*NU32)*E1/DELTA
              DORTH(1,2) = (NU21 + NU13*NU32)*E1/DELTA
              DORTH(1,3) = (NU13 + NU21*NU23)*E1/DELTA
              DORTH(2,2) = (UN - NU13*NU31)*E2/DELTA
              DORTH(2,3) = (NU23 + NU13*NU12)*E2/DELTA
              DORTH(3,3) = (UN - NU21*NU12)*E3/DELTA
              DORTH(2,1) = DORTH(1,2)
              DORTH(3,1) = DORTH(1,3)
              DORTH(3,2) = DORTH(2,3)
C
              DORTH(4,4) = G12
              DORTH(5,5) = G13
              DORTH(6,6) = G23
C
C ---         CALCUL DES VALEURS PROPRES DE LA MATRICE DORTH :
C             ----------------------------------------------
              CALL DORTVP(NDIM,NOMRC,DORTH,M2BLAN)
C
            ENDIF
C
          ENDIF
C
        ENDIF
C
        ENDIF
C
  20  CONTINUE
C
C.============================ FIN DE LA ROUTINE ======================
      END

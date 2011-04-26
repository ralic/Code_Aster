      SUBROUTINE MDFEDY (NBPAL,NBMODE,NUMPAS,DT,DTSTO,TCF,VROTAT,
     &                   DPLMOD,DEPGEN,VITGEN, FEXGEN,
     &                   TYPAL,FINPAL,CNPAL,PRDEFF,CONV)
      IMPLICIT  REAL*8  (A-H,O-Z)
      REAL*8             DEPGEN(*),VITGEN(*),FEXGEN(*)
      REAL*8             DPLMOD(NBPAL,NBMODE,*),DT,DTSTO,TCF
      REAL*8             VROTAT,CONV
      INTEGER            NUMPAS
      LOGICAL            PRDEFF
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR DELMAS J.DELMAS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE GREFFET N.GREFFET
C TOLE CRS_1404
C ======================================================================
C
C              RECUPERATION DES FORCES VENANT D'EDYOS
C                 ET ENVOI DES CHAMPS CINEMATIQUES
C     ------------------------------------------------------------------
C IN  : NBMODE : NOMBRE DE MODES
C IN  : DEPGEN : DEPLACEMENTS GENERALISES
C IN  : VITGEN : VITESSES GENERALISEES
C VAR : FEXGEN : FORCES GENERALISEES
C IN  : NBPAL  : NOMBRE DE PALIERS
C IN  : DPLMOD : TABLEAU DES DEPL MODAUX AUX NOEUDS DE CHOC
C
C IN  : TCF    : INSTANT DE CALCUL
C IN  : DT     : PAS DE TEMPS
C IN  : DTSTO  : INSTANT DE STOCKAGE
C IN  : VROTAT : VITESSE DE ROTATION
C IN  : NUMDDL : NUMEROTATION DDL
C
C OUT  : CONV   : INDICATEUR DE CONVERGENCE EDYOS
C ----------------------------------------------------------------------
      INTEGER       I, J, K, L
      REAL*8        DEP(NBPAL,6),VIT(NBPAL,6),FORCE(NBPAL,3)
C
      INTEGER       PALMAX
      PARAMETER (PALMAX=20)
      CHARACTER*3   FINPAL(PALMAX)
      CHARACTER*6   TYPAL(PALMAX)
      CHARACTER*8   CNPAL(PALMAX)
C
      DO 30 J = 1,NBPAL
        DO 5 L=1,6
          DEP(J,L)= 0.D0
          VIT(J,L)= 0.D0
 5      CONTINUE
        DO 20 I=1,NBMODE
          DO 15 K=1,6
            DEP(J,K)= DEP(J,K)+ DPLMOD(J,I,K)*DEPGEN(I)
            VIT(J,K)= VIT(J,K)+ DPLMOD(J,I,K)*VITGEN(I)
15        CONTINUE
20      CONTINUE
30    CONTINUE
C   ENVOI DES CHAMPS CINEMTATIQUES A EDYOS
      CALL ENVDEP(NUMPAS,NBPAL,DT,DTSTO,TCF,
     &            DEP,VIT,VROTAT,FINPAL,PRDEFF)
C   RECEPTION DES EFFORTS VENANT D'EDYOS
      CALL RECFOR(NUMPAS,NBPAL,FORCE,
     &              TYPAL,FINPAL,CNPAL,PRDEFF,CONV)
C   COMBINAISON DES EFFORTS GENERALISES
      DO 200 J = 1,NBPAL
        DO 100 I=1,NBMODE
          FEXGEN(I)=FEXGEN(I)+DPLMOD(J,I,1)*FORCE(J,1)
     &                     +DPLMOD(J,I,2)*FORCE(J,2)
     &                     +DPLMOD(J,I,3)*FORCE(J,3)
100     CONTINUE
200   CONTINUE
C
      END

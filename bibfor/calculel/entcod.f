      INTEGER FUNCTION ENTCOD ( ADMODL, LCMODL, NEC, MODE, K, L )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C-----------------------------------------------------------------------
      IMPLICIT NONE
C     IN:
C     MODE: MODE_LOCAL DE TYPE CHNO,VECT,OU MATR.
C     NEC : NBRE D ENTIERS POUR LA GRANDEUR
C     K : NUMERO DE NOEUD ( LOCAL ) ; L : NUMERO D ENTIER CODE
C     OUT:
C     ENTCOD: KEME ENTIER CODE.
C
C-----------------------------------------------------------------------
      INCLUDE 'jeveux.h'
      INTEGER     ADMODL, LCMODL, MODE, M1, M2, CODE, CODE1
      CHARACTER*8 K8B1,K8B2,K8B3,K8B4
      CHARACTER*24 VALK(4)
C
C
C
C-----------------------------------------------------------------------
      INTEGER IAD ,IADM ,IADM1 ,K ,L ,N1 ,N2 
      INTEGER NEC 
C-----------------------------------------------------------------------
      IADM = ADMODL + ZI(LCMODL+MODE-1) - 1
      CODE = ZI(IADM)
      IF (CODE.GT.3) THEN
         IF (CODE.EQ.4) THEN
            M1 = ZI(IADM+3)
         ELSE IF (CODE.EQ.5) THEN
            M1 = ZI(IADM+3)
            M2 = ZI(IADM+4)
            IF (M1.NE.M2) THEN
               CALL CODENT(M1,'D',K8B1)
               CALL CODENT(M2,'D',K8B2)
                VALK(1) = K8B1
                VALK(2) = K8B2
                CALL U2MESK('F','CALCULEL2_46', 2 ,VALK)
            END IF
         END IF
         IADM1 = ADMODL + ZI(LCMODL+M1-1) - 1
         CODE1 = ZI(IADM1)
         IF (CODE1.GT.3) THEN
            CALL CODENT(MODE,'D',K8B1)
            CALL CODENT(CODE,'D',K8B2)
            CALL CODENT(M1,'D',K8B3)
            CALL CODENT(CODE1,'D',K8B4)
             VALK(1) = K8B1
             VALK(2) = K8B2
             VALK(3) = K8B3
             VALK(4) = K8B4
             CALL U2MESK('F','CALCULEL2_47', 4 ,VALK)
         END IF
      ELSE
         IADM1 = IADM
         M1 = MODE
      END IF
      N1 = ZI(IADM1+3)
C
      IF (N1.GT.10000) THEN
         N2 = N1 - 10000
         IF (K.GT.N2) THEN
            CALL CODENT(M1,'D',K8B1)
            CALL CODENT(N2,'D',K8B2)
            CALL CODENT(K,'D',K8B3)
             VALK(1) = K8B1
             VALK(2) = K8B2
             VALK(3) = K8B3
             CALL U2MESK('F','CALCULEL2_48', 3 ,VALK)
         END IF
         IAD = 4 + NEC* (K-1) + L
      ELSE
         IAD = 4 + L
      END IF
      ENTCOD = ZI(IADM1+IAD-1)
C
      END

      SUBROUTINE TE0009(OPTION,NOMTE)
      IMPLICIT NONE
      CHARACTER*16      OPTION,NOMTE
C ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/12/2011   AUTEUR PELLET J.PELLET 
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
C ======================================================================
C     CALCUL DES MATRICES D'AMORTISSEMENT GYROSCOPIQUE
C     DES ELEMENTS DISCRETS :
C                             MECA_DIS_TR_N
C     ------------------------------------------------------------------
C IN  : OPTION : NOM DE L'OPTION A CALCULER
C IN  : NOMTE  : NOM DU TYPE_ELEMENT
C     ------------------------------------------------------------------
C       --- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
      CHARACTER*80                                             ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) ,ZK80(1)
C       ---  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C     ------------------------------------------------------------------

      INTEGER        NDDLM,NL1, IPOINT, LORIEN
      PARAMETER     (NDDLM=6,NL1=(NDDLM+1)*NDDLM/2)
      INTEGER        I, NC, NNO, JDM, JDC, J, INFODI,IBID
      REAL*8         VXX, R8BID, PGL(3,3), KLV(NL1), KLW(NL1)
      CHARACTER*8    K8BID

C     ------------------------------------------------------------------
      CALL JEMARQ()

      IF (NOMTE.EQ.'MECA_DIS_TR_N') THEN
C        ON VERIFIE QUE LES CARACTERISTIQUES ONT ETE AFFECTEES
C        LE CODE DU DISCRET
         CALL INFDIS('CODE',IBID,R8BID,NOMTE)
C        LE CODE STOKE DANS LA CARTE
         CALL INFDIS('TYDI',INFODI,R8BID,K8BID)
         IF (INFODI.NE.IBID) THEN
            CALL U2MESK('F+','DISCRETS_25',1,NOMTE)
            CALL INFDIS('DUMP',IBID,R8BID,'F+')
         ENDIF
C        DISCRET DE TYPE MASSE
         CALL INFDIS('DISM',INFODI,R8BID,K8BID)
         IF (INFODI.EQ.0) THEN
            CALL U2MESK('A+','DISCRETS_26',1,NOMTE)
            CALL INFDIS('DUMP',IBID,R8BID,'A+')
         ENDIF
         NC   = 6
      ELSE
         CALL U2MESS('F','CALCULEL_17')
      ENDIF
C     OPTION DE CALCUL INVALIDE
      IF (OPTION.NE.'MECA_GYRO') CALL ASSERT(.FALSE.)
C
      CALL INFDIS('SYMM',INFODI,R8BID,K8BID)
      CALL JEVECH('PCADISM','L',JDC)
      IF (INFODI.EQ.1) THEN
         VXX   = ZR(JDC+10-1)
      ELSEIF (INFODI.EQ.2) THEN
         VXX   = ZR(JDC+22-1)
      ENDIF
      CALL JEVECH('PMATUNS','E',JDM)

      DO 60 I=1,NL1
         KLV(I)=0.D0
 60   CONTINUE

C     I : LIGNE ; J : COLONNE
      I = 5
      J = 6
      IPOINT = INT(J*(J-1)/2)+I
      KLV(IPOINT) = -VXX
C
      CALL JEVECH('PCAORIE','L',LORIEN)
      CALL MATROT(ZR(LORIEN),PGL)
      NNO  = 1
      CALL UTPALG(NNO,NC,PGL,KLV,KLW)
      CALL UPLETR(NDDLM,ZR(JDM),KLW)

      CALL JEDEMA()
      END

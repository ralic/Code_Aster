      SUBROUTINE INMAT6(ELREFA,FAPG,MGANOS)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/09/2003   AUTEUR JMBHH01 J.M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE VABHHTS J.PELLET
C----------------------------------------------------------------------
C BUT : CALCUL DE LA MATRICE MGANOS : GAUSS -> SOMMETS
C----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER NDIM,NNO,NNOS,NBFPG,NBPG(10)
      INTEGER I,KP,KDIM,LN,J,LM,NPG
      REAL*8 XNO(3*27),VOL,FF(27),M(27*27)
      REAL*8 P(27*27),MGANOS(27,27)
      REAL*8 XPG(3*27),POIPG(27),XG(3),DET
      CHARACTER*8 NOFPG(10),ELREFA,ELREF2,FAPG
      LOGICAL SINGU
C DEB ------------------------------------------------------------------

      CALL ELRACA(ELREFA,NDIM,NNO,NNOS,NBFPG,NOFPG,NBPG,XNO,VOL)
      CALL ELRAGA(ELREFA,FAPG,NDIM,NPG,XPG,POIPG)
      CALL ASSERT(NDIM.EQ.3)
      CALL ASSERT(NNO.LE.27)
      CALL ASSERT(NPG.LE.27)

C     CAS DU SHB8 NON INVERSIBLE

      IF (FAPG.EQ.'SHB5') THEN
        DO 110 I = 1,4
          MGANOS(1,I) = 1.D0
          DO 120 KP = 2,NPG
            MGANOS(KP,I) = 0.D0
 120      CONTINUE
 110    CONTINUE
        DO 130 I = 5,8
          MGANOS(5,I) = 1.D0
          DO 140 KP = 1,NPG-1
            MGANOS(KP,I) = 0.D0
 140      CONTINUE
 130    CONTINUE
        GOTO 80
      ENDIF

      IF ((ELREFA.EQ.'H20') .OR. (ELREFA.EQ.'H27')) THEN
        ELREF2 = 'HE8'
      ELSE IF (ELREFA.EQ.'P15') THEN
        ELREF2 = 'PE6'
      ELSE IF (ELREFA.EQ.'P13') THEN
        ELREF2 = 'PY5'
      ELSE IF (ELREFA.EQ.'T10') THEN
        ELREF2 = 'TE4'
      ELSE
        ELREF2 = ELREFA
      END IF


C     CALCUL DES MATRICES M ET P :
C     ----------------------------
      DO 10 I = 1,NNOS*NNOS
        M(I) = 0.D0
   10 CONTINUE

      DO 50 KP = 1,NPG
        DO 20,KDIM = 1,NDIM
          XG(KDIM) = XPG(NDIM* (KP-1)+KDIM)
   20   CONTINUE
        CALL ELRFVF(ELREF2,XG,27,FF,NNO)
        LN = (KP-1)*NNOS
        DO 40 I = 1,NNOS
          P(LN+I) = FF(I)
          DO 30 J = 1,NNOS
            LM = NNOS* (I-1) + J
            M(LM) = M(LM) + FF(I)*FF(J)
   30     CONTINUE
   40   CONTINUE
   50 CONTINUE


C     CALCUL DE LA MATRICE M-1*P :
C     ----------------------------
      DET = 0.D0
      SINGU = .FALSE.
      CALL MGAUSS(M,P,NNOS,NNOS,NPG,DET,SINGU)
      
      
      DO 70 I = 1,NNOS
        DO 60 KP = 1,NPG
          MGANOS(KP,I) = P((KP-1)*NNOS+I)
   60   CONTINUE
   70 CONTINUE


   80 CONTINUE
      END

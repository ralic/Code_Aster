      SUBROUTINE INMAT4(ELREFA,NNO,NNOS,NPG,NOFPG,MGANO)
      IMPLICIT NONE
      CHARACTER*8 ELREFA,NOFPG
      INTEGER NNO,NNOS,NPG
      REAL*8 MGANO(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 08/03/2004   AUTEUR REZETTE C.REZETTE 
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
C ======================================================================
C BUT : CALCULER LA MATRICE DE PASSAGE GAUSS -> NOEUDS
C       POUR UNE FAMILLE D'UN ELREFA
C ======================================================================

C ----------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C --------- FIN DECLARATIONS NORMALISEES  JEVEUX ---------------------

      INTEGER KPG,KNO,KNOS,K
      REAL*8 MGANOS(27,27),MGANO2(27,27)
C DEB ------------------------------------------------------------------


      CALL ASSERT(NPG.LE.27)
      CALL ASSERT(NNO.LE.27)
      CALL ASSERT(NNOS.LE.27)


C     -- MISES A ZERO :
C     ----------------------------------------------------------
      DO 30,KPG = 1,NPG
        DO 10,KNO = 1,NNO
          MGANO2(KPG,KNO) = 0.D0
   10   CONTINUE
        DO 20,KNOS = 1,NNOS
          MGANOS(KPG,KNOS) = 0.D0
   20   CONTINUE
   30 CONTINUE
      DO 40,K = 1,2 + NPG*NNO
        MGANO(K) = 0.D0
   40 CONTINUE


C     -- ON TRAITE LE CAS GENERIQUE NPG=1  (INCLUT NOFPG='FPG1')
C     ----------------------------------------------------------
      IF (NPG.EQ.1) THEN
        DO 50,KNO = 1,NNO
          MGANO2(1,KNO) = 1.D0
   50   CONTINUE
        GO TO 80
      END IF


C     -- ON TRAITE LE CAS GENERIQUE NOFPG='NOEU'
C     -------------------------------------------------
      IF (NOFPG.EQ.'NOEU') THEN
        CALL ASSERT(NNO.EQ.NPG)
        DO 60,K = 1,NNO
          MGANO2(K,K) = 1.D0
   60   CONTINUE
        GO TO 80
      END IF


C     -- ON TRAITE LE CAS GENERIQUE NOFPG='NOEU_S'
C     -------------------------------------------------
      IF (NOFPG.EQ.'NOEU_S') THEN
        CALL ASSERT(NNOS.EQ.NPG)
        DO 70,K = 1,NNOS
          MGANOS(K,K) = 1.D0
   70   CONTINUE
        CALL INMAT5(ELREFA,NNO,NNOS,NPG,MGANOS,MGANO2)
        GO TO 80
      END IF


C     -- AUTRES CAS : GAUSS -> SOMMETS -> NOEUDS
C     -------------------------------------------
      CALL INMAT6(ELREFA,NOFPG,MGANOS)
      CALL INMAT5(ELREFA,NNO,NNOS,NPG,MGANOS,MGANO2)
      GO TO 80


   80 CONTINUE
      MGANO(1) = NNO
      MGANO(2) = NPG
      DO 100,KPG = 1,NPG
        DO 90,KNO = 1,NNO
          MGANO(2+ (KNO-1)*NPG+KPG) = MGANO2(KPG,KNO)
   90   CONTINUE
  100 CONTINUE
      GO TO 110

  110 CONTINUE

      END

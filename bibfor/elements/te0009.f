      SUBROUTINE TE0009(OPTION,NOMTE)
      IMPLICIT NONE
      CHARACTER*16      OPTION,NOMTE
C ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 07/10/2008   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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

      INTEGER NDDLM,NL
      INTEGER I, N, NC, JDM, JDC, J
      REAL*8  VXX
      PARAMETER (NDDLM=6,NL=NDDLM*NDDLM)

C     ------------------------------------------------------------------
      CALL JEMARQ()

      IF (NOMTE.EQ.'MECA_DIS_TR_N') THEN
         NC   = 6
      ELSE
         CALL U2MESS('F','CALCULEL_17')
      ENDIF

      IF (OPTION.EQ.'MECA_GYRO') THEN
         CALL JEVECH('PCADISM','L',JDC)
         CALL JEVECH('PMATUNS','E',JDM)

         DO 60 I = 1,NL
             ZR(JDM+I-1) = 0.D0
 60      CONTINUE

         VXX   = ZR(JDC+10-1)
C     I : LIGNE ; J : COLONNE
      I = 5
      J = 6
                ZR(JDM-1+NC*(J-1) + I) = VXX
                ZR(JDM-1+NC*(I-1) + J) =-VXX
      ELSE
CC OPTION DE CALCUL INVALIDE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      CALL JEDEMA()
      END

      SUBROUTINE PRNIV0(T,NBCP,NBCO,NBSP,SN)
      IMPLICIT   NONE
      INTEGER NBCP,NBCO,NBSP
      REAL*8 T(*),SN(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 26/11/2002   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C IN  NBCP  : I : NBR DE CMP
C IN  NBCO  : I : NBR DE COUCHES
C IN  NBSP  : I : NBR DE SOUS-POINTS
C     ------------------------------------------------------------------
      INTEGER   NBVP, L, M, ISP, I10, ICP, I30
      REAL*8    VALE(6,4), EQUI(2)
C     -----------------------------------------------------------------

      IF (NBCO.GT.1) THEN
        CALL UTMESS('F','PRNIV0','ON NE TRAITE PAS LES MULTICOUCHES')
      END IF

      NBVP = 3
      L = 6*NBSP
      M = L*NBCO

      ISP = 1
      I10 = 6* (ISP-1)
      DO 10 ICP = 1,NBCP
        I30 = M* (ICP-1)
C        --- COMPOSANTE MOMENT_0 ---
        VALE(ICP,1) = T(I30+I10+1)
C        --- COMPOSANTE MOYE_INT ---
        VALE(ICP,3) = T(I30+I10+5)
   10 CONTINUE

      ISP = NBSP
      I10 = 6* (ISP-1)
      DO 20 ICP = 1,NBCP
        I30 = M* (ICP-1)
C        --- COMPOSANTE MOMENT_1 ---
        VALE(ICP,2) = 0.5D0*T(I30+I10+2)
C        --- COMPOSANTE MOYE_EXT ---
        VALE(ICP,4) = T(I30+I10+6)
   20 CONTINUE

C --- MEMBRANE
      CALL FGEQUI ( VALE(1,1), 'SIGM', NBVP, EQUI )
      SN(1) = EQUI(2)

C --- FLEXION
      CALL FGEQUI ( VALE(1,2), 'SIGM', NBVP, EQUI )
      SN(2) = EQUI(2)

C --- MEMBRANE + FLEXION A L'ORIGINE
      CALL FGEQUI ( VALE(1,3), 'SIGM', NBVP, EQUI )
      SN(3) = EQUI(2)

C --- MEMBRANE + FLEXION A L'EXTREMITE
      CALL FGEQUI ( VALE(1,4), 'SIGM', NBVP, EQUI )
      SN(4) = EQUI(2)

      END

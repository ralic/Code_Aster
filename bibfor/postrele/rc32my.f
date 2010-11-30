      SUBROUTINE RC32MY ( NBABSC, ABSC, VALE, MOMEN0, MOMEN1 )
      IMPLICIT   NONE
      INTEGER             NBABSC
      REAL*8              ABSC(*), VALE(*), MOMEN0, MOMEN1
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 01/10/2002   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
C
C     CALCUL DES MOYENNES
C     EXTRAIT DE LA ROUTINE RVPSTM
C
C     ------------------------------------------------------------------
C
      INTEGER       NBSGT, ISGT
      REAL*8        L, S1, S2, S12, T1, T2, T12, SMIL
C DEB ------------------------------------------------------------------
C
C --- LONGUEUR DU SEGMENT 
C
      L = 1.0D 0/ ( ABSC(NBABSC) - ABSC(1) )
      NBSGT = NBABSC - 1
C
      MOMEN0 = 0.0D0
      MOMEN1 = 0.0D0
C
      DO 10, ISGT = 1, NBSGT, 1
C
         S1  = ABSC(ISGT) - ABSC(1)
         S2  = ABSC(ISGT+1) - ABSC(1)
         S12 = S2 - S1
C
         T1 = VALE(ISGT)
         T2 = VALE(ISGT+1)
         T12  = (T1+T2)/2.0D0
C
         SMIL = (S1+S2)/2.0D0
         MOMEN0 = MOMEN0 + S12*(T1 + T2)
         MOMEN1 = MOMEN1 + S12/3.0D0 * (T1*S1 + 4.0D0*T12*SMIL + T2*S2)
C
 10   CONTINUE
C
      MOMEN0 = MOMEN0*L
      MOMEN1 = MOMEN1*L*L
C
      MOMEN0 = 0.5D0*MOMEN0
      MOMEN1 = 6.0D0*(MOMEN1 - MOMEN0)
C
      END

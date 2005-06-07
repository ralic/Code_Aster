      SUBROUTINE FOLIE3 ( NBFR, FREQ, ACCE, LISS, ELIM, NBELIM )
      IMPLICIT NONE
      INTEGER             NBFR, ELIM(*), NBELIM
      REAL*8              LISS, FREQ(*), ACCE(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 09/12/2002   AUTEUR CIBHHLV L.VIVAN 
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
C     LISSAGE DE LA FONCTION
C
C     ------------------------------------------------------------------
      INTEGER       IND1, IND2, IND3
      REAL*8        B, M, YA, YPA, CRLIS
C     ------------------------------------------------------------------
C
      NBELIM = 0  
C
      IND1 = 1
      IND2 = 2
      IND3 = 3
C
 100  CONTINUE
      IF ( IND3 .GT. NBFR ) GOTO 200
C
C --- EQUATION DE LA DROITE P1 P3
C
      M = ( ACCE(IND3) - ACCE(IND1) ) / ( FREQ(IND3) - FREQ(IND1) )
      B = ACCE(IND1) - ( M * FREQ(IND1) )
C
C --- VERIFICATION QUE LE POINT P2 EST EN DESSOUS DE (P1,P3)
C
      YPA = ( M * FREQ(IND2) ) + B
      YA  = ACCE(IND2)
C
      IF ( YA .LE. YPA ) THEN
C
         CRLIS = LOG(YA) + LOG(1.0D0+LISS)
C
         IF ( LOG(YPA) .LE. CRLIS ) THEN
C --------- ON ELIMINE LE POINT P2
            NBELIM = NBELIM + 1 
            ELIM(IND2) = 1
            IND1 = IND1
            IND2 = IND3
            IND3 = IND3 + 1
            GOTO 100
         ELSE
C --------- ON CONSERVE LE POINT P2
            IND1 = IND2
            IND2 = IND3
            IND3 = IND3 + 1
            GOTO 100
         ENDIF
      ELSE
C ------ ON CONSERVE LE POINT P2
         IND1 = IND2
         IND2 = IND3
         IND3 = IND3 + 1
         GOTO 100
      ENDIF
C
 200  CONTINUE
C
      END

      SUBROUTINE RC32F4 ( TYPASS, NBP12, NBP23, NBP13, NBSIGR, NBSG1,
     +                    NBSG2, NBSG3, NOCC, SALTIJ, NSITUP )
      IMPLICIT   NONE        
      INTEGER             NBP12, NBP23, NBP13, NBSIGR, NOCC(*), NSITUP,
     +                    NBSG1, NBSG2, NBSG3
      REAL*8              SALTIJ(*)
      CHARACTER*3         TYPASS
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 27/11/2007   AUTEUR VIVAN L.VIVAN 
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
C
C     SI IL N'EXISTE PLUS DE SITUATION DE PASSAGE ENTRE 2 GROUPES,
C     ON MET LES TERMES CROISES DE SALT A ZERO
C
C     ------------------------------------------------------------------
      INTEGER      I1, I2, ISL
C     ------------------------------------------------------------------
C
      IF ( TYPASS .EQ. '1_2' ) THEN
         IF ( NBP12 .EQ. 0 ) THEN
C           BLOC 1_2
            DO 100 I1 = 1,NBSG1
               ISL = 4*(I1-1)*NBSIGR + 4*NBSG1
               DO 102 I2 = 1,NBSG2
                  SALTIJ(ISL+4*(I2-1)+1) = 0.D0
                  SALTIJ(ISL+4*(I2-1)+2) = 0.D0
                  SALTIJ(ISL+4*(I2-1)+3) = 0.D0
                  SALTIJ(ISL+4*(I2-1)+4) = 0.D0
 102           CONTINUE
 100        CONTINUE
C           BLOC 2_1
            DO 104 I1 = 1,NBSG2
               ISL = 4*NBSIGR*NBSG1 + 4*(I1-1)*NBSIGR
               DO 106 I2 = 1,NBSG1
                  SALTIJ(ISL+4*(I2-1)+1) = 0.D0
                  SALTIJ(ISL+4*(I2-1)+2) = 0.D0
                  SALTIJ(ISL+4*(I2-1)+3) = 0.D0
                  SALTIJ(ISL+4*(I2-1)+4) = 0.D0
 106           CONTINUE
 104        CONTINUE
         ENDIF
C
      ELSEIF ( TYPASS .EQ. '2_3' ) THEN
         IF ( NBP23 .EQ. 0 ) THEN
C           BLOC 2_3
            DO 110 I1 = 1,NBSG2
               ISL = 4*NBSIGR*NBSG1 + 4*(I1-1)*NBSIGR + 4*(NBSG1+NBSG2)
               DO 112 I2 = 1,NBSG3
                  SALTIJ(ISL+4*(I2-1)+1) = 0.D0
                  SALTIJ(ISL+4*(I2-1)+2) = 0.D0
                  SALTIJ(ISL+4*(I2-1)+3) = 0.D0
                  SALTIJ(ISL+4*(I2-1)+4) = 0.D0
 112           CONTINUE
 110        CONTINUE
C           BLOC 3_2
            DO 114 I1 = 1,NBSG3
               ISL = 4*NBSIGR*(NBSG1+NBSG2) + 4*(I1-1)*NBSIGR + 4*NBSG1
               DO 116 I2 = 1,NBSG2
                  SALTIJ(ISL+4*(I2-1)+1) = 0.D0
                  SALTIJ(ISL+4*(I2-1)+2) = 0.D0
                  SALTIJ(ISL+4*(I2-1)+3) = 0.D0
                  SALTIJ(ISL+4*(I2-1)+4) = 0.D0
 116           CONTINUE
 114        CONTINUE
         ENDIF
C
      ELSEIF ( TYPASS .EQ. '1_3' ) THEN
         IF ( NBP13 .EQ. 0 ) THEN
C           BLOC 1_3
            DO 120 I1 = 1,NBSG1
               ISL = 4*(I1-1)*NBSIGR + 4*(NBSG1+NBSG2)
               DO 122 I2 = 1,NBSG3
                  SALTIJ(ISL+4*(I2-1)+1) = 0.D0
                  SALTIJ(ISL+4*(I2-1)+2) = 0.D0
                  SALTIJ(ISL+4*(I2-1)+3) = 0.D0
                  SALTIJ(ISL+4*(I2-1)+4) = 0.D0
 122           CONTINUE
 120        CONTINUE
C           BLOC 3_1
            DO 124 I1 = 1,NBSG3
               ISL = 4*NBSIGR*(NBSG1+NBSG2) + 4*NBSIGR*(I1-1)
               DO 126 I2 = 1,NBSG1
                  SALTIJ(ISL+4*(I2-1)+1) = 0.D0
                  SALTIJ(ISL+4*(I2-1)+2) = 0.D0
                  SALTIJ(ISL+4*(I2-1)+3) = 0.D0
                  SALTIJ(ISL+4*(I2-1)+4) = 0.D0
 126           CONTINUE
 124        CONTINUE
         ENDIF
      ENDIF
C
      END

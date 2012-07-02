      SUBROUTINE NBBIT1(EC,NB1)
      IMPLICIT NONE
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C
      INTEGER EC,NB1
C
C*********************************************************************
C
C       NB1 := NBR DE BIT A 1 DANS LA REPRESENTATION BINAIRE DE EC
C
C       (APPLICATION DIRECT AU COMPTAGE DE COMPOSANTES ACTIVES
C        D' UNE GRANDEUR DECRITE PAR ENTIER CODE)
C
C*********************************************************************
C
      INTEGER TEST,I
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      NB1  = 0
      TEST = 1
C
      DO 10, I= 1, 30, 1
C
         TEST = 2*TEST
C
         IF ( IAND(EC,TEST) .GT. 0) THEN
C
            NB1 = NB1 + 1
C
         ENDIF
C
10    CONTINUE
C
      END

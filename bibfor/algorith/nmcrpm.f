      SUBROUTINE NMCRPM(LIST  ,NBINST,DTMIN )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/01/2011   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE
      REAL*8       LIST(*),DTMIN
      INTEGER      NBINST
C
C ----------------------------------------------------------------------
C
C ROUTINE *_NON_LINE (UTILITAIRE)
C
C CALCUL DU DELTA TEMPS MINIMUM
C
C ----------------------------------------------------------------------
C
C
C IN  LIST   : LISTE DES INSTANTS
C IN  NBINST : NOMBRE D'INSTANTS DANS LA LISTE
C OUT DTMIN  : INCREMENT DE TEMPS MINIMUM DANS LA LISTE
C
C ----------------------------------------------------------------------
C
      INTEGER      I 
      REAL*8       DELTAT  
C
C ----------------------------------------------------------------------
C

C
C --- INITIALISATIONS
C
      DTMIN  = 0.D0
      IF (NBINST.EQ.1) GOTO 99
C
C --- DELTA MINIMUM
C
      DTMIN  = LIST(2)-LIST(1)
      DO 10 I = 2,NBINST-1
        DELTAT = LIST(I+1-1) - LIST(I-1)
        DTMIN  = MIN(DELTAT,DTMIN )
10    CONTINUE
99    CONTINUE
C
      END

        SUBROUTINE FGCOTA (NPIC,PIC,NCYC,SIGMIN,SIGMAX)
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 09/09/96   AUTEUR D6BHHAM A.M.DONORE 
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
C       ----------------------------------------------------------------
C      COMPTAGE DES CYCLES POUR LA METHODE TAHERI
C       ----------------------------------------------------------------
C      IN  NPIC    NOMBRE  DE PICS
C          PIC     VALEURS DES PICS
C      OUT NCYC    NOMBRE  DE  CYCLE
C      OUT SIGMAX  CONTRAINTES MAXIMALES DES CYCLES
C          SIGMIN  CONTRAINTES MINIMALES DES CYCLES
C       ----------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8          PIC(*),E1,E2,SIGMAX(*),SIGMIN(*)
      INTEGER         NPIC,  NCYC
C       ----------------------------------------------------------------
      NCYC  = 0
 1    CONTINUE
      I     = 1
C
  2   CONTINUE
      IF(I+2.GT.NPIC) THEN
        GOTO 100
      ENDIF
      E1 = ABS ( PIC(I+1) - PIC(I) )
      E2 = ABS ( PIC(I+2) - PIC(I+1) )
C
      IF ( E1. GE. E2 )  THEN
        NCYC = NCYC + 1
        IF(PIC(I).GE.PIC(I+1)) THEN
          SIGMAX(NCYC) = PIC(I)
          SIGMIN(NCYC) = PIC(I+1)
        ELSE
          SIGMAX(NCYC) = PIC(I+1)
          SIGMIN(NCYC) = PIC(I)
        ENDIF
      ELSE
        NCYC = NCYC + 1
        IF(PIC(I+1).GE.PIC(I+2)) THEN
          SIGMAX(NCYC) = PIC(I+1)
          SIGMIN(NCYC) = PIC(I+2)
        ELSE
          SIGMAX(NCYC) = PIC(I+2)
          SIGMIN(NCYC) = PIC(I+1)
        ENDIF
      ENDIF
      I= I+2
      GOTO 2
C
C  --- TRAITEMENT DU RESIDU -------
C
 100  CONTINUE
      IF (I+1.EQ.NPIC) THEN
        NCYC = NCYC+1
        IF(PIC(I).GE.PIC(I+1)) THEN
          SIGMAX(NCYC) = PIC(I)
          SIGMIN(NCYC) = PIC(I+1)
        ELSE
          SIGMAX(NCYC) = PIC(I+1)
          SIGMIN(NCYC) = PIC(I)
        ENDIF
      ENDIF
C
      END

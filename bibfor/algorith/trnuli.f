      SUBROUTINE TRNULI(ITAB,NBLIG,NBCOL,ICOL,NURES)
      IMPLICIT NONE
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C***********************************************************************
C    P. RICHARD     DATE 20/01/92
C-----------------------------------------------------------------------
C  BUT:  < TROUVER NUMERO DE  LIGNE DANS UN TABLEAU >
C
C   A PARTIR DES VALEURS DES COLONNES
C
C-----------------------------------------------------------------------
C
C NOM----- / /:
C
C ITAB     /I/: TABLEAU D'ENTIER
C NBLIG    /I/: NOMBRE DE LIGNES
C NBCOL    /I/: NOMBRE DE COLONNES
C ICOL     /I/: VALEURS DES COLONNES A TROUVER
C NURES    /I/: NUMERO DE LA LIGNE CHERCHEE
C
C
C-----------------------------------------------------------------------
C
      INTEGER   ITAB(NBLIG,NBCOL),ICOL(NBCOL)
      LOGICAL OK
C
C-----------------------------------------------------------------------
C     DATA
C-----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
      INTEGER I ,J ,NBCOL ,NBLIG ,NURES 
C-----------------------------------------------------------------------
      I=0
      NURES=0
C
 10   CONTINUE
      I=I+1
C
      OK=.TRUE.
C
      DO 20 J=1,NBCOL
        IF(ITAB(I,J).NE.ICOL(J)) OK=.FALSE.
20    CONTINUE
C
      IF(OK) THEN
        NURES=I
        GOTO 9999
      ELSE
        IF(I.LT.NBLIG) THEN
          GOTO 10
        ELSE
          GOTO 9999
        ENDIF
      ENDIF
C
C
 9999 CONTINUE
      END

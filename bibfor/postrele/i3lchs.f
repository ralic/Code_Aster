      SUBROUTINE I3LCHS(NIL,TETE,QUEUE,SUCC,PREC,PT,NBPT)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER NIL,TETE,QUEUE,SUCC(*),PREC(*),PT(*),NBPT
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 09/02/2004   AUTEUR REZETTE C.REZETTE 
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
C     SUPPRESSION DE PT DANS LE TYPE LISTE CHAINEE DE MAILLE
C     ------------------------------------------------------------------
C IN  NIL    : I : POINTEUR NIL
C IN  PT     : I : TABLE DES POINTEUR A SUPPRIMER
C IN  NBPT   : I : NBR DE POINTEUR A SUPPRIMER
C VAR TETE   : I : POINTEUR DE TETE
C VAR QUEUE  : I : POINTEUR DE QUEUE
C VAR SUCC   : I : POINTEUR SUR LA LISTE DES SUCCESEURS
C VAR PREC   : I : POINTEUR SUR LA LISTE DES PREDECESSEURS
C     ------------------------------------------------------------------
C     LA LISTE EST SUPPOSEE NON VIDE
C     ------------------------------------------------------------------
C
      INTEGER PTS,PTP,PTC,I
C
C======================================================================
C
      DO 10, I = 1, NBPT, 1
         PTC = PT(I)
         IF ( PTC .EQ. TETE ) THEN
            TETE = SUCC(TETE)
            IF ( TETE .NE. NIL ) THEN
               PREC(TETE) = NIL
            ENDIF
         ELSE IF ( PTC .EQ. QUEUE ) THEN
            QUEUE = PREC(QUEUE)
            IF ( QUEUE .NE. NIL ) THEN
               SUCC(QUEUE) = NIL
            ENDIF
         ELSE
            PTP       = PREC(PTC)
            PTS       = SUCC(PTC)
            PREC(PTS) = PTP
            SUCC(PTP) = PTS
         ENDIF
10    CONTINUE
      END

      SUBROUTINE I3LCHI(NIL,TETE,QUEUE,PT,INFO,DESC,SUCC,PREC)
      IMPLICIT NONE
C
      INTEGER NIL,TETE,QUEUE,PT,INFO,DESC(*),SUCC(*),PREC(*)
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C     ------------------------------------------------------------------
C     INSERTION EN TETE DANS LE TYPE LISTE CHAINEE DE MAILLE
C     ------------------------------------------------------------------
C IN  NIL    : I : POINTEUR NIL
C IN  PT     : I : POINTEUR A INSERER
C IN  INFO   : I : VALEUR DU CHAMP INFO POINTE
C VAR TETE   : I : POINTEUR DE TETE
C VAR QUEUE  : I : POINTEUR DE QUEUE
C VAR DESC   : I : TABLE IMPLEMENTANT LA LISTE
C VAR SUCC   : I : POINTEUR SUR LA LISTE DES SUCCESEURS
C VAR PREC   : I : POINTEUR SUR LA LISTE DES PREDECESSEURS
C     ------------------------------------------------------------------
C
C======================================================================
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      IF ( TETE .EQ. NIL ) THEN
         TETE        = PT
         QUEUE       = PT
         SUCC(QUEUE) = NIL
         PREC(TETE ) = NIL
      ELSE
         SUCC(PT)   = TETE
         PREC(TETE) = PT
         TETE       = PT
         PREC(TETE) = NIL
      ENDIF
      DESC(TETE ) = INFO
      END

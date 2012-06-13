      SUBROUTINE DEC2PN(ICODE,TABLE,N)
C---------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C---------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
C     DECODE UN ENTIER CODE EN BASE  2.
C---------------------------------------------------------------------
C IN  : ICODE : ENTIER CODE
C OUT : TABLE : VECTEUR DE LONGUEUR  N, NOMBRE DECODE EN BASE 2
C---------------------------------------------------------------------
C
      INCLUDE 'jeveux.h'
      INTEGER TABLE(*) 
      REAL*8  N2PI
C
C----------------------------------------------------------------------
       RCODE = DBLE(ICODE)
C
       DO 10 I=N,1,-1
         N2PI = EXP(DBLE(I)*LOG(2.D0))
         IF((RCODE/N2PI) .GE. 1.D0) THEN
           TABLE(I) = 1
           RCODE = RCODE - N2PI
         ENDIF
 10    CONTINUE
      END

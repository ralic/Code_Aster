      SUBROUTINE MDTRIB (IND,A,N)
      IMPLICIT   NONE
      INTEGER            N,IND(N)
      REAL*8             A(N)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SOUSTRUC  DATE 10/03/99   AUTEUR CIBHHME R.MEDDOURI 
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
C
C     CLASSEMENT DE VALEURS (TRI BULLE)
C     ------------------------------------------------------------------
C IN  : N
C IN  : IND
C IN  : A
C OUT : IND
C ----------------------------------------------------------------------
      INTEGER I,J,K
C     ------------------------------------------------------------------
C
      DO 10 I=N-1,1,-1
      DO 10 J=1,I
      IF (A(IND(J)).LT.A(IND(J+1))) THEN
      K=IND(J+1)
      IND(J+1)=IND(J)
      IND(J)=K
      ENDIF
   10 CONTINUE
      END

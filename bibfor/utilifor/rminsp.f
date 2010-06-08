      FUNCTION RMINSP (X1,X2,X3,X4,X5)
      IMPLICIT   NONE
      REAL*8  RMINSP, X1, X2, X3, X4, X5, XMIN
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 15/01/2008   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C      CALCULER LE "MIN" DE 5 VALEURS >=0. EN NE TENANT
C      COMPTE QUE DES VALEURS NON NULLES
C
C IN  : X1,X2,X3,X4,X5 : 5 REELS >= 0.D0
C OUT : RMINSP : LE "MIN" DES X1, ..., X5 NON NULS
C REMARQUE : X1 DOIT ETRE > 0
C     ------------------------------------------------------------------
C
C DEB------------------------------------------------------------------
      CALL ASSERT(X1.GT.0.D0)
      CALL ASSERT(X2.GE.0.D0)
      CALL ASSERT(X3.GE.0.D0)
      CALL ASSERT(X4.GE.0.D0)
      CALL ASSERT(X5.GE.0.D0)

      XMIN=X1
      IF (X2.GT.0.D0 .AND. X2.LT.XMIN) XMIN=X2
      IF (X3.GT.0.D0 .AND. X3.LT.XMIN) XMIN=X3
      IF (X4.GT.0.D0 .AND. X4.LT.XMIN) XMIN=X4
      IF (X5.GT.0.D0 .AND. X5.LT.XMIN) XMIN=X5

      RMINSP=XMIN

      END

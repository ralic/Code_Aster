        SUBROUTINE LCMCLI(COMP,NOMFAM,NBSYS,IS,PGL,SIGF,SICL)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/06/2011   AUTEUR PROIX J-M.PROIX 
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
C     CONTRAINTE DE CLIVAGE MAXI POUR LE MONOCRISTAL
C     ----------------------------------------------------------------
      INTEGER  I,NBSYS,IS,IR,J
      REAL*8 SIGF(6),PGL(3,3),MS(6),NG(3),SI(3,3),SING(3),SICL,P,LG(3)
      REAL*8 QM(3,3)
      CHARACTER*16 NOMFAM,COMP(*)
      
      IR=0
      
      CALL LCMMSG(NOMFAM,NBSYS,IS,PGL,MS,NG,LG,IR,QM)
C     SIGMA (3,3)
C     calcul du max de Ns.(SIGMA.Ns) 
      IF (COMP(3)(1:5).NE.'PETIT') THEN
         CALL TNSVEC(6, 3, SI, SIGF, 1.D0 ) 
      ELSE
         CALL TNSVEC(6, 3, SI, SIGF, 1.D0/SQRT(2.D0) )
      ENDIF 
      DO 9 I = 1,3
         SING(I) = 0.D0
  9   CONTINUE
      DO 11 I = 1 , 3
      DO 10 J = 1 , 3
         SING(I) = SING(I) + SI(I,J) * NG(J)
 10   CONTINUE
 11   CONTINUE
      P = 0.D0
      DO 1 I = 1 , 3
         P = P + SING(I)*NG(I)
 1    CONTINUE
      SICL = MAX(SICL, P)
      
      END

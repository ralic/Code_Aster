      SUBROUTINE MATECT ( MATERD, MATERF, NMAT, MACST)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/06/2012   AUTEUR PROIX J-M.PROIX 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C     ROUTINE GENERIQUE DE RECUPERATION DU MATERIAU A T ET T+DT
C     ----------------------------------------------------------------
C     IN  MATERD  :  COEFFICIENTS MATERIAU A T- 
C     IN  MATERF  :  COEFFICIENTS MATERIAU A T+
C         NMAT   :  DIMENSION  DE MATER
C     OUT MATCST :  'OUI' SI  MATERIAU A T = MATERIAU A T+DT
C                   'NON' SINON
C     ----------------------------------------------------------------
      INTEGER       NMAT,I
      REAL*8        MATERD(NMAT,2),MATERF(NMAT,2),EPSI,R8PREM
      CHARACTER*3   MACST
C     ----------------------------------------------------------------
      EPSI=R8PREM()
C
C -   MATERIAU CONSTANT ?
      MACST = 'OUI'
      DO 30 I = 1,NMAT
        IF (ABS(MATERD(I,1)-MATERF(I,1)).GT.EPSI*MATERD(I,1) )THEN
        MACST = 'NON'
        GOTO 9999
        ENDIF
 30   CONTINUE
      DO 40 I = 1,NMAT
        IF (ABS(MATERD(I,2)-MATERF(I,2)).GT.EPSI*MATERD(I,2) )THEN
        MACST = 'NON'
        GOTO 9999
        ENDIF
 40   CONTINUE
 9999 CONTINUE
      END

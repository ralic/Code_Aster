      SUBROUTINE NOR091 ( LINST, LCHAMP, NBMODE, NBSAUV, VAL, MAT )
      IMPLICIT   NONE
      INTEGER    NBSAUV, NBMODE
      REAL*8     LINST(*), LCHAMP(*), MAT(*), VAL
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/07/2002   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C ----------------------------------------------------------------------
C    
      INTEGER  I, J
      REAL*8   SUM
C DEB ------------------------------------------------------------------
C
      DO 10 I = 1, NBSAUV 
         SUM = 0.D0
         DO 12 J = 1, NBMODE
            SUM = SUM + (LCHAMP((I-1)*NBMODE+J))**2
 12      CONTINUE
         MAT(I) = SUM
 10   CONTINUE
C
      SUM = 0.D0
      DO 20 I = 1, NBSAUV-1
         SUM = SUM - (MAT(I) + MAT(I+1) )* (LINST(I) -LINST(I+1))/2.D0
 20   CONTINUE
C
      VAL = SQRT(SUM)
C
      END

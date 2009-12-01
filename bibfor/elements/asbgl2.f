      SUBROUTINE ASBGL2(BGLOB,B)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 12/08/2008   AUTEUR DESROCHES X.DESROCHES 
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
C
C Transformer B(3,20) dans global en BGLOB(6,60) en global
C
      IMPLICIT REAL *8(A-H,O-Z)
      REAL *8 BGLOB(6,60), B(3,20)
C
      CALL R8INIR(360,0.D0,BGLOB,1)
C
C Premiere ligne
C
      DO 10 J=1,20
         BGLOB(1,J)=B(1,J)
  10  CONTINUE
C
C Deuxieme ligne
C
      DO 20 J=21,40
         BGLOB(2,J)=B(2,J-20)
  20  CONTINUE
C
C Troisieme ligne
C
      DO 30 J=41,60
         BGLOB(3,J)=B(3,J-40)
  30  CONTINUE
C
C Quatrieme ligne
C
      DO 40 J=1,20
         BGLOB(4,J)=B(2,J)
  40  CONTINUE
C
      DO 50 J=21,40
         BGLOB(4,J)=B(1,J-20)
  50  CONTINUE
C
C Cinquieme ligne
C
      DO 60 J=21,40
         BGLOB(5,J)=B(3,J-20)
  60  CONTINUE
C
      DO 70 J=41,60
         BGLOB(5,J)=B(2,J-40)
  70  CONTINUE
C
C Sixieme ligne
C
      DO 80 J=1,20
         BGLOB(6,J)=B(3,J)
  80  CONTINUE 
C
      DO 90 J=41,60
         BGLOB(6,J)=B(1,J-40)
  90  CONTINUE
C
      END

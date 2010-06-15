       SUBROUTINE TRAN63(C66,C3333,ICAS)
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
C=======================================================================
C ICAS	= 1 ---> PASSAGE D'UN TENSEUR D'ORDRE 4 A UN TENSEUR D'ORDRE 2
C       = 2 ---> PASSAGE D'UN TENSEUR D'ORDRE 2 A UN TENSEUR D'ORDRE 4 
C
      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER ICAS,I1(6),J1(6),IJ,KL,IJ1(3,3),I,J,K,L
      REAL*8 C3333(3,3,3,3),C66(6,6)
C
      IF(ICAS.EQ.1)THEN
          DO 10 K=1,3
             I1(K)=K
             J1(K)=K
   10     CONTINUE
C	 
          I1(4)=1
          J1(4)=2
          I1(5)=2
          J1(5)=3
          I1(6)=1
          J1(6)=3
C
          DO 30 IJ=1,6
             DO 20 KL=1,6
                C66(IJ,KL)=C3333(I1(IJ),J1(IJ),I1(KL),J1(KL))
   20        CONTINUE
   30     CONTINUE
      ENDIF
C
      IF(ICAS.EQ.2)THEN
          DO 40 K=1,3
            IJ1(K,K)=K
   40     CONTINUE
C	 
          IJ1(1,2)=4
          IJ1(2,1)=4
          IJ1(2,3)=5
          IJ1(3,2)=5
          IJ1(1,3)=6
          IJ1(3,1)=6
C
          DO 80 I=1,3
             DO 70 J=1,3
                DO 60 K=1,3
                   DO 50 L=1,3
                     C3333(I,J,K,L)=C66(IJ1(I,J),IJ1(K,L))
   50             CONTINUE
   60           CONTINUE
   70        CONTINUE
   80     CONTINUE
      ENDIF
C
      END

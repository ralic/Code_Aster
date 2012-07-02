      SUBROUTINE EFFI(NOMTE,SIGMTD,VF,DFDS,JACP,SINA,COSA,R,EFFINT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
      IMPLICIT NONE
C
      CHARACTER*16 NOMTE
      REAL*8 SIGMTD(*),SINA,COSA,R,VF(*),DFDS(*),JACP,EFFINT(*)
      REAL*8 MATS(5,9),EFFINB(9)
C
C     CALCULS DES EFFORTS INTERIEURS
C
C-----------------------------------------------------------------------
      INTEGER I ,K 
C-----------------------------------------------------------------------
      IF ( NOMTE .EQ. 'MECXSE3' ) THEN
C     
      MATS(1,1)=-SINA*DFDS(1)
      MATS(1,2)= COSA*DFDS(1)
      MATS(1,3)= 0.D0
      MATS(1,4)=-SINA*DFDS(2)
      MATS(1,5)= COSA*DFDS(2)
      MATS(1,6)= 0.D0
      MATS(1,7)=-SINA*DFDS(3)
      MATS(1,8)= COSA*DFDS(3)
      MATS(1,9)= 0.D0
C
      MATS(2,1)= 0.D0
      MATS(2,2)= 0.D0
      MATS(2,3)= DFDS(1)
      MATS(2,4)= 0.D0
      MATS(2,5)= 0.D0
      MATS(2,6)= DFDS(2)
      MATS(2,7)= 0.D0
      MATS(2,8)= 0.D0
      MATS(2,9)= DFDS(3)
C
      MATS(3,1)= VF(1)/R
      MATS(3,2)= 0.D0
      MATS(3,3)= 0.D0
      MATS(3,4)= VF(2)/R
      MATS(3,5)= 0.D0
      MATS(3,6)= 0.D0
      MATS(3,7)= VF(3)/R
      MATS(3,8)= 0.D0
      MATS(3,9)= 0.D0
C
      MATS(4,1)= 0.D0
      MATS(4,2)= 0.D0
      MATS(4,3)= -SINA*VF(1)/R
      MATS(4,4)= 0.D0
      MATS(4,5)= 0.D0
      MATS(4,6)= -SINA*VF(2)/R
      MATS(4,7)= 0.D0
      MATS(4,8)= 0.D0
      MATS(4,9)= -SINA*VF(3)/R
C
      MATS(5,1)= COSA*DFDS(1)
      MATS(5,2)= SINA*DFDS(1)
      MATS(5,3)= VF(1)
      MATS(5,4)= COSA*DFDS(2)
      MATS(5,5)= SINA*DFDS(2)
      MATS(5,6)= VF(2)
      MATS(5,7)= COSA*DFDS(3)
      MATS(5,8)= SINA*DFDS(3)
      MATS(5,9)= VF(3)
C
C     CONSTRUCTION DES EFFORTS INTERIEURS
C
      DO 10 I=1,9
         EFFINB(I)=0.D0
      DO 20 K=1,5
         EFFINB(I)=EFFINB(I)+MATS(K,I)*SIGMTD(K)
 20   CONTINUE
         EFFINT(I)=EFFINT(I)+JACP*EFFINB(I)
 10   CONTINUE
C
      ELSE
C    
      MATS(1,1)=-SINA*DFDS(1)
      MATS(1,2)= COSA*DFDS(1)
      MATS(1,3)= 0.D0
      MATS(1,4)=-SINA*DFDS(2)
      MATS(1,5)= COSA*DFDS(2)
      MATS(1,6)= 0.D0
      MATS(1,7)=-SINA*DFDS(3)
      MATS(1,8)= COSA*DFDS(3)
      MATS(1,9)= 0.D0
C
      MATS(2,1)= 0.D0
      MATS(2,2)= 0.D0
      MATS(2,3)= DFDS(1)
      MATS(2,4)= 0.D0
      MATS(2,5)= 0.D0
      MATS(2,6)= DFDS(2)
      MATS(2,7)= 0.D0
      MATS(2,8)= 0.D0
      MATS(2,9)= DFDS(3)
C
      MATS(3,1)= COSA*DFDS(1)
      MATS(3,2)= SINA*DFDS(1)
      MATS(3,3)= VF(1)
      MATS(3,4)= COSA*DFDS(2)
      MATS(3,5)= SINA*DFDS(2)
      MATS(3,6)= VF(2)
      MATS(3,7)= COSA*DFDS(3)
      MATS(3,8)= SINA*DFDS(3)
      MATS(3,9)= VF(3)
C
      DO 30 I=1,9
         EFFINB(I)=0.D0
      DO 40 K=1,3
         EFFINB(I)=EFFINB(I)+MATS(K,I)*SIGMTD(K)
 40   CONTINUE
         EFFINT(I)=EFFINT(I)+JACP*EFFINB(I)
 30   CONTINUE
C
      ENDIF
      END

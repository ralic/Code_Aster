      SUBROUTINE VDEFGN(NOMTE,OPTION,NB2,EPAIS,ZIC,SIGMA,EFFGTG)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 28/01/2003   AUTEUR DURAND C.DURAND 
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
C--- INTEGRATION DES CONTRAINTES DANS L'EPAISSEUR D'UNE COUCHE DE COQUE
C--- 3D BASEE SUR LA METHODE DE NEWTON-COTES A 3 POINTS
C--- EN SORTIE: EFFGTG CONTIENT LES 5 EFFORTS ET 2 MOMEMTS AUX NOEUDS
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*16 NOMTE,OPTION
      REAL*8 EPAIS, ZIC, DEMIEP
      REAL*8 SIGMA(6,*),EFFGTG(8,*)
C
C--- DANS LE CAS DE EFGE_ELNO, ON CALCULE AUX NB1 NOEUDS
C
C--- INITIALISATION
C
      DO 5 I=1,NB2
      DO 6 J=1,8
         EFFGTG(J,I)=0.D0
 6    CONTINUE
 5    CONTINUE
C
      DEMIEP=EPAIS/2.D0
C
      WNC1 = 0.33333333333333D0
      WNC2 = 1.33333333333333D0
      WNC3 = 0.33333333333333D0
C
      ZIC1 = ZIC
      ZIC2 = ZIC1 + DEMIEP
      ZIC3 = ZIC2 + DEMIEP
C
C-- TEST TYPE D'ELEMENT
C
      IF (NOMTE(1:8).EQ.'MEC3QU9H') THEN
C
         EFFGTG(1,1)=DEMIEP*(WNC1*SIGMA(1,1)+WNC2*SIGMA(1,5)
     &                                      +WNC3*SIGMA(1,9)) 
         EFFGTG(1,2)=DEMIEP*(WNC1*SIGMA(1,2)+WNC2*SIGMA(1,6)
     &                                      +WNC3*SIGMA(1,10))
         EFFGTG(1,3)=DEMIEP*(WNC1*SIGMA(1,3)+WNC2*SIGMA(1,7)
     &                                      +WNC3*SIGMA(1,11))
         EFFGTG(1,4)=DEMIEP*(WNC1*SIGMA(1,4)+WNC2*SIGMA(1,8)
     &                                       +WNC3*SIGMA(1,12))
         EFFGTG(1,5)=(EFFGTG(1,1)+EFFGTG(1,2))/2.D0
         EFFGTG(1,6)=(EFFGTG(1,2)+EFFGTG(1,3))/2.D0
         EFFGTG(1,7)=(EFFGTG(1,3)+EFFGTG(1,4))/2.D0
         EFFGTG(1,8)=(EFFGTG(1,4)+EFFGTG(1,1))/2.D0
         EFFGTG(1,9)=(EFFGTG(1,5)+EFFGTG(1,6)
     &               +EFFGTG(1,7)+EFFGTG(1,8))/4.D0
C
         EFFGTG(2,1)=DEMIEP*(WNC1*SIGMA(2,1)+WNC2*SIGMA(2,5)
     &                                      +WNC3*SIGMA(2,9)) 
         EFFGTG(2,2)=DEMIEP*(WNC1*SIGMA(2,2)+WNC2*SIGMA(2,6)
     &                                      +WNC3*SIGMA(2,10))
         EFFGTG(2,3)=DEMIEP*(WNC1*SIGMA(2,3)+WNC2*SIGMA(2,7)
     &                                      +WNC3*SIGMA(2,11))
         EFFGTG(2,4)=DEMIEP*(WNC1*SIGMA(2,4)+WNC2*SIGMA(2,8)
     &                                      +WNC3*SIGMA(2,12))
         EFFGTG(2,5)=(EFFGTG(2,1)+EFFGTG(2,2))/2.D0
         EFFGTG(2,6)=(EFFGTG(2,2)+EFFGTG(2,3))/2.D0
         EFFGTG(2,7)=(EFFGTG(2,3)+EFFGTG(2,4))/2.D0
         EFFGTG(2,8)=(EFFGTG(2,4)+EFFGTG(2,1))/2.D0
         EFFGTG(2,9)=(EFFGTG(2,5)+EFFGTG(2,6)
     &               +EFFGTG(2,7)+EFFGTG(2,8))/4.D0
C
         EFFGTG(3,1)=DEMIEP*(WNC1*SIGMA(4,1)+WNC2*SIGMA(4,5)
     &                                      +WNC3*SIGMA(4,9)) 
         EFFGTG(3,2)=DEMIEP*(WNC1*SIGMA(4,2)+WNC2*SIGMA(4,6)
     &                                      +WNC3*SIGMA(4,10))
         EFFGTG(3,3)=DEMIEP*(WNC1*SIGMA(4,3)+WNC2*SIGMA(4,7)
     &                                      +WNC3*SIGMA(4,11))
         EFFGTG(3,4)=DEMIEP*(WNC1*SIGMA(4,4)+WNC2*SIGMA(4,8)
     &                                      +WNC3*SIGMA(4,12))
         EFFGTG(3,5)=(EFFGTG(3,1)+EFFGTG(3,2))/2.D0
         EFFGTG(3,6)=(EFFGTG(3,2)+EFFGTG(3,3))/2.D0
         EFFGTG(3,7)=(EFFGTG(3,3)+EFFGTG(3,4))/2.D0
         EFFGTG(3,8)=(EFFGTG(3,4)+EFFGTG(3,1))/2.D0
         EFFGTG(3,9)=(EFFGTG(3,5)+EFFGTG(3,6)
     &               +EFFGTG(3,7)+EFFGTG(3,8))/4.D0
C
        EFFGTG(4,1)=DEMIEP*(WNC1*ZIC1*SIGMA(1,1)
     &                     +WNC2*ZIC2*SIGMA(1,5)
     &                     +WNC3*ZIC3*SIGMA(1,9))
        EFFGTG(4,2)=DEMIEP*(WNC1*ZIC1*SIGMA(1,2)
     &                     +WNC2*ZIC2*SIGMA(1,6)
     &                     +WNC3*ZIC3*SIGMA(1,10))
        EFFGTG(4,3)=DEMIEP*(WNC1*ZIC1*SIGMA(1,3)
     &                     +WNC2*ZIC2*SIGMA(1,7)
     &                     +WNC3*ZIC3*SIGMA(1,11))
        EFFGTG(4,4)=DEMIEP*(WNC1*ZIC1*SIGMA(1,4)
     &                     +WNC2*ZIC2*SIGMA(1,8)
     &                     +WNC3*ZIC3*SIGMA(1,12))
        EFFGTG(4,5)=(EFFGTG(4,1)+EFFGTG(4,2))/2.D0
        EFFGTG(4,6)=(EFFGTG(4,2)+EFFGTG(4,3))/2.D0
        EFFGTG(4,7)=(EFFGTG(4,3)+EFFGTG(4,4))/2.D0
        EFFGTG(4,8)=(EFFGTG(4,4)+EFFGTG(4,1))/2.D0
        EFFGTG(4,9)=(EFFGTG(4,5)+EFFGTG(4,6)
     &              +EFFGTG(4,7)+EFFGTG(4,8))/4.D0
C
        EFFGTG(5,1)=DEMIEP*(WNC1*ZIC1*SIGMA(2,1)
     &                     +WNC2*ZIC2*SIGMA(2,5)
     &                     +WNC3*ZIC3*SIGMA(2,9))
        EFFGTG(5,2)=DEMIEP*(WNC1*ZIC1*SIGMA(2,2)
     &                     +WNC2*ZIC2*SIGMA(2,6)
     &                     +WNC3*ZIC3*SIGMA(2,10))
        EFFGTG(5,3)=DEMIEP*(WNC1*ZIC1*SIGMA(2,3)
     &                     +WNC2*ZIC2*SIGMA(2,7)
     &                     +WNC3*ZIC3*SIGMA(2,11))
        EFFGTG(5,4)=DEMIEP*(WNC1*ZIC1*SIGMA(2,4)
     &                     +WNC2*ZIC2*SIGMA(2,8)
     &                     +WNC3*ZIC3*SIGMA(2,12))
        EFFGTG(5,5)=(EFFGTG(5,1)+EFFGTG(5,2))/2.D0
        EFFGTG(5,6)=(EFFGTG(5,2)+EFFGTG(5,3))/2.D0
        EFFGTG(5,7)=(EFFGTG(5,3)+EFFGTG(5,4))/2.D0
        EFFGTG(5,8)=(EFFGTG(5,4)+EFFGTG(5,1))/2.D0
        EFFGTG(5,9)=(EFFGTG(5,5)+EFFGTG(5,6)
     &              +EFFGTG(5,7)+EFFGTG(5,8))/4.D0
C
        EFFGTG(6,1)=DEMIEP*(WNC1*ZIC1*SIGMA(4,1)
     &                     +WNC2*ZIC2*SIGMA(4,5)
     &                     +WNC3*ZIC3*SIGMA(4,9))
        EFFGTG(6,2)=DEMIEP*(WNC1*ZIC1*SIGMA(4,2)
     &                     +WNC2*ZIC2*SIGMA(4,6)
     &                     +WNC3*ZIC3*SIGMA(4,10))
        EFFGTG(6,3)=DEMIEP*(WNC1*ZIC1*SIGMA(4,3)
     &                     +WNC2*ZIC2*SIGMA(4,7)
     &                     +WNC3*ZIC3*SIGMA(4,11))
        EFFGTG(6,4)=DEMIEP*(WNC1*ZIC1*SIGMA(4,4)
     &                     +WNC2*ZIC2*SIGMA(4,8)
     &                     +WNC3*ZIC3*SIGMA(4,12))
        EFFGTG(6,5)=(EFFGTG(6,1)+EFFGTG(6,2))/2.D0
        EFFGTG(6,6)=(EFFGTG(6,2)+EFFGTG(6,3))/2.D0
        EFFGTG(6,7)=(EFFGTG(6,3)+EFFGTG(6,4))/2.D0
        EFFGTG(6,8)=(EFFGTG(6,4)+EFFGTG(6,1))/2.D0
        EFFGTG(6,9)=(EFFGTG(6,5)+EFFGTG(6,6)
     &              +EFFGTG(6,7)+EFFGTG(6,8))/4.D0
C
         EFFGTG(7,1)=DEMIEP*(WNC1*SIGMA(5,1)+WNC2*SIGMA(5,5)
     &                                      +WNC3*SIGMA(5,9)) 
         EFFGTG(7,2)=DEMIEP*(WNC1*SIGMA(5,2)+WNC2*SIGMA(5,6)
     &                                      +WNC3*SIGMA(5,10))
         EFFGTG(7,3)=DEMIEP*(WNC1*SIGMA(5,3)+WNC2*SIGMA(5,7)
     &                                      +WNC3*SIGMA(5,11))
         EFFGTG(7,4)=DEMIEP*(WNC1*SIGMA(5,4)+WNC2*SIGMA(5,8)
     &                                      +WNC3*SIGMA(5,12))
         EFFGTG(7,5)=(EFFGTG(7,1)+EFFGTG(7,2))/2.D0
         EFFGTG(7,6)=(EFFGTG(7,2)+EFFGTG(7,3))/2.D0
         EFFGTG(7,7)=(EFFGTG(7,3)+EFFGTG(7,4))/2.D0
         EFFGTG(7,8)=(EFFGTG(7,4)+EFFGTG(7,1))/2.D0
         EFFGTG(7,9)=(EFFGTG(7,5)+EFFGTG(7,6)
     &               +EFFGTG(7,7)+EFFGTG(7,8))/4.D0
C
         EFFGTG(8,1)=DEMIEP*(WNC1*SIGMA(6,1)+WNC2*SIGMA(6,5)
     &                                      +WNC3*SIGMA(6,9)) 
         EFFGTG(8,2)=DEMIEP*(WNC1*SIGMA(6,2)+WNC2*SIGMA(6,6)
     &                                      +WNC3*SIGMA(6,10))
         EFFGTG(8,3)=DEMIEP*(WNC1*SIGMA(6,3)+WNC2*SIGMA(6,7)
     &                                      +WNC3*SIGMA(6,11))
         EFFGTG(8,4)=DEMIEP*(WNC1*SIGMA(6,4)+WNC2*SIGMA(6,8)
     &                                      +WNC3*SIGMA(6,12))
         EFFGTG(8,5)=(EFFGTG(8,1)+EFFGTG(8,2))/2.D0
         EFFGTG(8,6)=(EFFGTG(8,2)+EFFGTG(8,3))/2.D0
         EFFGTG(8,7)=(EFFGTG(8,3)+EFFGTG(8,4))/2.D0
         EFFGTG(8,8)=(EFFGTG(8,4)+EFFGTG(8,1))/2.D0
         EFFGTG(8,9)=(EFFGTG(8,5)+EFFGTG(8,6)
     &               +EFFGTG(8,7)+EFFGTG(8,8))/4.D0
C
      ELSE IF (NOMTE(1:8).EQ.'MEC3TR7H') THEN
C
         EFFGTG(1,1)=DEMIEP*(WNC1*SIGMA(1,1)+WNC2*SIGMA(1,4)
     &                                      +WNC3*SIGMA(1,7)) 
         EFFGTG(1,2)=DEMIEP*(WNC1*SIGMA(1,2)+WNC2*SIGMA(1,5)
     &                                      +WNC3*SIGMA(1,8))
         EFFGTG(1,3)=DEMIEP*(WNC1*SIGMA(1,3)+WNC2*SIGMA(1,6)
     &                                      +WNC3*SIGMA(1,9))
         EFFGTG(1,4)=(EFFGTG(1,1)+EFFGTG(1,2))/2.D0
         EFFGTG(1,5)=(EFFGTG(1,2)+EFFGTG(1,3))/2.D0
         EFFGTG(1,6)=(EFFGTG(1,3)+EFFGTG(1,1))/2.D0
         EFFGTG(1,7)=(EFFGTG(1,1)+EFFGTG(1,2)+EFFGTG(1,3))/3.D0
C
         EFFGTG(2,1)=DEMIEP*(WNC1*SIGMA(2,1)+WNC2*SIGMA(2,4)
     &                                      +WNC3*SIGMA(2,7)) 
         EFFGTG(2,2)=DEMIEP*(WNC1*SIGMA(2,2)+WNC2*SIGMA(2,5)
     &                                      +WNC3*SIGMA(2,8))
         EFFGTG(2,3)=DEMIEP*(WNC1*SIGMA(2,3)+WNC2*SIGMA(2,6)
     &                                      +WNC3*SIGMA(2,9))
         EFFGTG(2,4)=(EFFGTG(2,1)+EFFGTG(2,2))/2.D0
         EFFGTG(2,5)=(EFFGTG(2,2)+EFFGTG(2,3))/2.D0
         EFFGTG(2,6)=(EFFGTG(2,3)+EFFGTG(2,1))/2.D0
         EFFGTG(2,7)=(EFFGTG(2,1)+EFFGTG(2,2)+EFFGTG(2,3))/3.D0
C
         EFFGTG(3,1)=DEMIEP*(WNC1*SIGMA(4,1)+WNC2*SIGMA(4,4)
     &                                      +WNC3*SIGMA(4,7)) 
         EFFGTG(3,2)=DEMIEP*(WNC1*SIGMA(4,2)+WNC2*SIGMA(4,5)
     &                                      +WNC3*SIGMA(4,8))
         EFFGTG(3,3)=DEMIEP*(WNC1*SIGMA(4,3)+WNC2*SIGMA(4,6)
     &                                      +WNC3*SIGMA(4,9))
         EFFGTG(3,4)=(EFFGTG(3,1)+EFFGTG(3,2))/2.D0
         EFFGTG(3,5)=(EFFGTG(3,2)+EFFGTG(3,3))/2.D0
         EFFGTG(3,6)=(EFFGTG(3,3)+EFFGTG(3,1))/2.D0
         EFFGTG(3,7)=(EFFGTG(3,1)+EFFGTG(3,2)+EFFGTG(3,3))/3.D0
C
        EFFGTG(4,1)=DEMIEP*(WNC1*ZIC1*SIGMA(1,1)
     &                     +WNC2*ZIC2*SIGMA(1,4)
     &                     +WNC3*ZIC3*SIGMA(1,7))
        EFFGTG(4,2)=DEMIEP*(WNC1*ZIC1*SIGMA(1,2)
     &                     +WNC2*ZIC2*SIGMA(1,5)
     &                     +WNC3*ZIC3*SIGMA(1,8))
        EFFGTG(4,3)=DEMIEP*(WNC1*ZIC1*SIGMA(1,3)
     &                     +WNC2*ZIC2*SIGMA(1,6)
     &                     +WNC3*ZIC3*SIGMA(1,9))
        EFFGTG(4,4)=(EFFGTG(4,1)+EFFGTG(4,2))/2.D0
        EFFGTG(4,5)=(EFFGTG(4,2)+EFFGTG(4,3))/2.D0
        EFFGTG(4,6)=(EFFGTG(4,3)+EFFGTG(4,1))/2.D0
        EFFGTG(4,7)=(EFFGTG(4,1)+EFFGTG(4,2)+EFFGTG(4,3))/3.D0
C
        EFFGTG(5,1)=DEMIEP*(WNC1*ZIC1*SIGMA(2,1)
     &                     +WNC2*ZIC2*SIGMA(2,4)
     &                     +WNC3*ZIC3*SIGMA(2,7))
        EFFGTG(5,2)=DEMIEP*(WNC1*ZIC1*SIGMA(2,2)
     &                     +WNC2*ZIC2*SIGMA(2,5)
     &                     +WNC3*ZIC3*SIGMA(2,8))
        EFFGTG(5,3)=DEMIEP*(WNC1*ZIC1*SIGMA(2,3)
     &                     +WNC2*ZIC2*SIGMA(2,6)
     &                     +WNC3*ZIC3*SIGMA(2,9))
        EFFGTG(5,4)=(EFFGTG(5,1)+EFFGTG(5,2))/2.D0
        EFFGTG(5,5)=(EFFGTG(5,2)+EFFGTG(5,3))/2.D0
        EFFGTG(5,6)=(EFFGTG(5,3)+EFFGTG(5,1))/2.D0
        EFFGTG(5,7)=(EFFGTG(5,1)+EFFGTG(5,2)+EFFGTG(5,3))/3.D0
C
        EFFGTG(6,1)=DEMIEP*(WNC1*ZIC1*SIGMA(4,1)
     &                     +WNC2*ZIC2*SIGMA(4,4)
     &                     +WNC3*ZIC3*SIGMA(4,7))
        EFFGTG(6,2)=DEMIEP*(WNC1*ZIC1*SIGMA(4,2)
     &                     +WNC2*ZIC2*SIGMA(4,5)
     &                     +WNC3*ZIC3*SIGMA(4,8))
        EFFGTG(6,3)=DEMIEP*(WNC1*ZIC1*SIGMA(4,3)
     &                     +WNC2*ZIC2*SIGMA(4,6)
     &                     +WNC3*ZIC3*SIGMA(4,9))
        EFFGTG(6,4)=(EFFGTG(6,1)+EFFGTG(6,2))/2.D0
        EFFGTG(6,5)=(EFFGTG(6,2)+EFFGTG(6,3))/2.D0
        EFFGTG(6,6)=(EFFGTG(6,3)+EFFGTG(6,1))/2.D0
        EFFGTG(6,7)=(EFFGTG(6,1)+EFFGTG(6,2)+EFFGTG(6,3))/3.D0
C
         EFFGTG(7,1)=DEMIEP*(WNC1*SIGMA(5,1)+WNC2*SIGMA(5,4)
     &                                      +WNC3*SIGMA(5,7)) 
         EFFGTG(7,2)=DEMIEP*(WNC1*SIGMA(5,2)+WNC2*SIGMA(5,5)
     &                                      +WNC3*SIGMA(5,8))
         EFFGTG(7,3)=DEMIEP*(WNC1*SIGMA(5,3)+WNC2*SIGMA(5,6)
     &                                      +WNC3*SIGMA(5,9))
         EFFGTG(7,4)=(EFFGTG(7,1)+EFFGTG(7,2))/2.D0
         EFFGTG(7,5)=(EFFGTG(7,2)+EFFGTG(7,3))/2.D0
         EFFGTG(7,6)=(EFFGTG(7,3)+EFFGTG(7,1))/2.D0
         EFFGTG(7,7)=(EFFGTG(7,1)+EFFGTG(7,2)+EFFGTG(7,3))/3.D0
C
         EFFGTG(8,1)=DEMIEP*(WNC1*SIGMA(6,1)+WNC2*SIGMA(6,4)
     &                                      +WNC3*SIGMA(6,7)) 
         EFFGTG(8,2)=DEMIEP*(WNC1*SIGMA(6,2)+WNC2*SIGMA(6,5)
     &                                      +WNC3*SIGMA(6,8))
         EFFGTG(8,3)=DEMIEP*(WNC1*SIGMA(6,3)+WNC2*SIGMA(6,6)
     &                                      +WNC3*SIGMA(6,9))
         EFFGTG(8,4)=(EFFGTG(8,1)+EFFGTG(8,2))/2.D0
         EFFGTG(8,5)=(EFFGTG(8,2)+EFFGTG(8,3))/2.D0
         EFFGTG(8,6)=(EFFGTG(8,3)+EFFGTG(8,1))/2.D0
         EFFGTG(8,7)=(EFFGTG(8,1)+EFFGTG(8,2)+EFFGTG(8,3))/3.D0
C
      ENDIF
C
      END

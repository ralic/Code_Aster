      SUBROUTINE DVDRT1(NPT,XI1,XI2,VF,DFD1,DFD2)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 15/05/96   AUTEUR F6BHHBU D.BUI 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER NPT
      REAL*8  XI1,XI2,VF(*),DFD1(*),DFD2(*)
C
C     VALEURS DES FONCTIONS DE FORME ET DE LEURS DERIVEES DES
C     ELEMENTS T6 (LAGRANGE P2) ET T6H
C
C     DESCRIPTION DE LA NUMEROTATION DES NOEUDS
C
C     ORIGINE = NOEUD 2   NOEUD 1 (0,1), NOEUD 3 (1,0) .......
C
C     +1                         +1
C     +  +                       +  +
C     +    +                     +    +
C     +4     +6                  +4     +6
C     +        +                 +  7     +
C     +2    5    +3              +2    5    +3
C     +++++++++++++              +++++++++++++
C
      IF (NPT.EQ.6) THEN
         VF(1)= XI2*(2.D0*XI2-1.D0)
         VF(2)=(1.D0-XI1-XI2)*(2.D0*(1.D0-XI1-XI2)-1.D0)
         VF(3)= XI1*(2.D0*XI1-1.D0)
         VF(4)= 4.D0*XI2*(1.D0-XI1-XI2)
         VF(5)= 4.D0*XI1*(1.D0-XI1-XI2)
         VF(6)= 4.D0*XI1*XI2
         DFD1(1)= 0.D0
         DFD1(2)=-4.D0*(1.D0-XI1-XI2)+1.D0
         DFD1(3)= 4.D0*XI1-1.D0
         DFD1(4)=-4.D0*XI2
         DFD1(5)= 4.D0*(1.D0-XI1-XI2)-4.D0*XI1
         DFD1(6)= 4.D0*XI2
         DFD2(1)= 4.D0*XI2-1.D0
         DFD2(2)=-4.D0*(1.D0-XI1-XI2)+1.D0
         DFD2(3)= 0.D0
         DFD2(4)= 4.D0*(1.D0-XI1-XI2)-4.D0*XI2
         DFD2(5)=-4.D0*XI1
         DFD2(6)= 4.D0*XI1
      ELSE IF (NPT.EQ.7) THEN
         VF(1)= XI2*(2.D0*XI2-1.D0)+3.D0*XI1*XI2*(1.D0-XI1-XI2)
         VF(2)=(1.D0-XI1-XI2)*(2.D0*(1.D0-XI1-XI2)-1.D0)
     &                               +3.D0*XI1*XI2*(1.D0-XI1-XI2)
         VF(3)= XI1*(2.D0*XI1-1.D0)+3.D0*XI1*XI2*(1.D0-XI1-XI2)
         VF(4)= 4.D0*XI2*(1.D0-XI1-XI2)-12.D0*XI1*XI2*(1.D0-XI1-XI2)
         VF(5)= 4.D0*XI1*(1.D0-XI1-XI2)-12.D0*XI1*XI2*(1.D0-XI1-XI2)
         VF(6)= 4.D0*XI1*XI2-12.D0*XI1*XI2*(1.D0-XI1-XI2)
         VF(7)= 27.D0*XI1*XI2*(1.D0-XI1-XI2)
         DFD1(1)=3.D0*XI2*(1.D0-XI1-XI2)-3.D0*XI1*XI2
         DFD1(2)=-4.D0*(1.D0-XI1-XI2)+1.D0
     &                         +3.D0*XI2*(1.D0-XI1-XI2)-3.D0*XI1*XI2
         DFD1(3)= 4.D0*XI1-1.D0+3.D0*XI2*(1.D0-XI1-XI2)-3.D0*XI1*XI2
         DFD1(4)=-4.D0*XI2-12.D0*XI2*(1.D0-XI1-XI2)+12.D0*XI1*XI2
         DFD1(5)= 4.D0*(1.D0-XI1-XI2)-4.D0*XI1
     &                    -12.D0*XI2*(1.D0-XI1-XI2)+12.D0*XI1*XI2
         DFD1(6)= 4.D0*XI2-12.D0*XI2*(1.D0-XI1-XI2)+12.D0*XI1*XI2
         DFD1(7)= 27.D0*XI2*(1.D0-XI1-XI2)-27.D0*XI1*XI2
         DFD2(1)= 4.D0*XI2-1.D0+3.D0*XI1*(1.D0-XI1-XI2)-3.D0*XI1*XI2
         DFD2(2)=-4.D0*(1.D0-XI1-XI2)+1.D0
     &                         +3.D0*XI1*(1.D0-XI1-XI2)-3.D0*XI1*XI2
         DFD2(3)= 3.D0*XI1*(1.D0-XI1-XI2)-3.D0*XI1*XI2
         DFD2(4)= 4.D0*(1.D0-XI1-XI2)-4.D0*XI2
     &                    -12.D0*XI1*(1.D0-XI1-XI2)+12.D0*XI1*XI2
         DFD2(5)=-4.D0*XI1-12.D0*XI1*(1.D0-XI1-XI2)+12.D0*XI1*XI2
         DFD2(6)= 4.D0*XI1-12.D0*XI1*(1.D0-XI1-XI2)+12.D0*XI1*XI2
         DFD2(7)= 27.D0*XI1*(1.D0-XI1-XI2)-27.D0*XI1*XI2
      ENDIF
      END

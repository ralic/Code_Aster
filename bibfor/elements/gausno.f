      SUBROUTINE GAUSNO(ALIAS,NPG,XPG,YPG,ZPG,HPG)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 20/03/2002   AUTEUR MJBHHPE J.L.FLEJOU 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C                                                                       
C                                                                       
C ======================================================================
C TOLE CRP_20
C.......................................................................
C
C BUT: CALCUL DES POIDS ET POINTS DE GAUSS
C
C ENTREES  ---> ALIAS : NOM D'ALIAS DE L'ELEMENT
C          ---> NPG   : NOMBRE DE POINTS DE GAUSS
C
C SORTIES  <--- XPG,YPG,ZPG : COORDONNEES DES POINTS DE GAUSS
C          <--- HPG         : POIDS DES POINTS DE GAUSS
C.......................................................................
C
      IMPLICIT NONE
      INTEGER NPG
      REAL*8 XPG(NPG),YPG(NPG),ZPG(NPG),HPG(NPG)
      CHARACTER*8  ALIAS
C
C  HEXA20D
      IF ( ALIAS(1:7).EQ.'HEXA20D') THEN
      XPG(1)=-1.D0
      YPG(1)=-1.D0
      ZPG(1)=-1.D0
      HPG(1) = 1.D0
C
      XPG(2)= 1.D0
      YPG(2)=-1.D0
      ZPG(2)=-1.D0
      HPG(2) = 1.D0
C
      XPG(3)= 1.D0
      YPG(3)= 1.D0
      ZPG(3)=-1.D0
      HPG(3) = 1.D0
C
      XPG(4)=-1.D0
      YPG(4)= 1.D0
      ZPG(4)=-1.D0
      HPG(4) = 1.D0
C
      XPG(5)=-1.D0
      YPG(5)=-1.D0
      ZPG(5)= 1.D0
      HPG(5) = 1.D0
C
      XPG(6)= 1.D0
      YPG(6)=-1.D0
      ZPG(6)= 1.D0
      HPG(6) = 1.D0
C
      XPG(7)= 1.D0
      YPG(7)= 1.D0
      ZPG(7)= 1.D0
      HPG(7) = 1.D0
C
      XPG(8)=-1.D0
      YPG(8)= 1.D0
      ZPG(8)= 1.D0
      HPG(8) = 1.D0
C
C  PENTA15D
      ELSE IF ( ALIAS(1:8).EQ.'PENTA15D') THEN
      XPG(1)=-1.D0
      YPG(1)= 1.D0
      ZPG(1)= 0.D0
      HPG(1) = 1.D0/6.D0
C
      XPG(2)=-1.D0
      YPG(2)= 0.D0
      ZPG(2)= 1.D0
      HPG(2) = 1.D0/6.D0
C
      XPG(3)=-1.D0
      YPG(3)= 0.D0
      ZPG(3)= 0.D0
      HPG(3) = 1.D0/6.D0
C
      XPG(4)= 1.D0
      YPG(4)= 1.D0
      ZPG(4)= 0.D0
      HPG(4) = 1.D0/6.D0
C
      XPG(5)= 1.D0
      YPG(5)= 0.D0
      ZPG(5)= 1.D0
      HPG(5) = 1.D0/6.D0
C
      XPG(6)= 1.D0
      YPG(6)= 0.D0
      ZPG(6)= 0.D0
      HPG(6) = 1.D0/6.D0
C
C  PYRAM13D
      ELSE IF ( ALIAS(1:8).EQ.'PYRAM13D') THEN
      XPG(1)= 1.D0
      YPG(1)= 0.D0
      ZPG(1)= 0.D0
      HPG(1) = 1.D0/5.D0
C
      XPG(2)= 0.D0
      YPG(2)= 1.D0
      ZPG(2)= 0.D0
      HPG(2) = 1.D0/5.D0
C
      XPG(3)=-1.D0
      YPG(3)= 0.D0
      ZPG(3)= 0.D0
      HPG(3) = 1.D0/5.D0
C
      XPG(4)= 0.D0
      YPG(4)=-1.D0
      ZPG(4)= 0.D0
      HPG(4) = 1.D0/5.D0
C
      XPG(5)= 0.D0
      YPG(5)= 0.D0
      ZPG(5)= 1.D0
      HPG(5) = 1.D0/5.D0
C
C  TETRA10D
      ELSE IF ( ALIAS(1:8).EQ.'TETRA10D') THEN
      XPG(1)= 0.D0
      YPG(1)= 1.D0
      ZPG(1)= 0.D0
      HPG(1) = 0.25D0
C
      XPG(2)= 0.D0
      YPG(2)= 0.D0
      ZPG(2)= 1.D0
      HPG(2) = 0.25D0
C
      XPG(3)= 0.D0
      YPG(3)= 0.D0
      ZPG(3)= 0.D0
      HPG(3) = 0.25D0
C
      XPG(4)= 1.D0
      YPG(4)= 0.D0
      ZPG(4)= 0.D0
      HPG(4) = 0.25D0
C
      ELSE
        CALL UTMESS('F','GAUSNO','ELEM THM LUMPE INCONNU ')
      ENDIF
      END

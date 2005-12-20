      SUBROUTINE ZZPOLY(NNO,INO,XINO,YINO,SIG,B)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 06/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
      IMPLICIT REAL*8 (A-H,O-Z)
C
C    ESTIMATEUR ZZ2 (VERSION 92)
C
C  CETTE ROUTINE CALCULE LA VALEUR DES CONTRAINTES LISSEES AU NOEUD INO
C
C  ENTREE :
C       NNO   :  NOMBRE DE NOEUDS DE L'ELEMENT
C       INO   :  NUMERO GLOBAL DU NOEUD OU ON CALCULE LES CONTRAINTES
C  XINO, YINO :  COORDONNEES DU NOEUD INO
C       B     :  COEFFICIENTS DES MONOMES DU POLYNOME DE BASE
C
C  SORTIE :
C       SIG   :  CONTRAINTES LISSEES
C
      REAL*8 XINO,YINO,SIG(1),B(9,4)
      IF(NNO.EQ.3) THEN
        DO 3 IC=1,4
           SIG(4*(INO-1)+IC) = SIG(4*(INO-1)+IC)
     &                       + B(1,IC)+B(2,IC)*XINO+B(3,IC)*YINO
3       CONTINUE
      ELSE IF(NNO.EQ.6) THEN
        DO 4 IC=1,4
          SIG(4*(INO-1)+IC) = SIG(4*(INO-1)+IC)
     &                      + B(1,IC)+B(2,IC)*XINO+B(3,IC)*YINO
     &                      + B(4,IC)*XINO*YINO
     &                      + B(5,IC)*XINO*XINO
     &                      + B(6,IC)*YINO*YINO
4       CONTINUE
      ELSE IF(NNO.EQ.4) THEN
        DO 5 IC=1,4
          SIG(4*(INO-1)+IC) = SIG(4*(INO-1)+IC)
     &                      + B(1,IC)+B(2,IC)*XINO+B(3,IC)*YINO
     &                      + B(4,IC)*XINO*YINO
5       CONTINUE
      ELSE IF(NNO.EQ.8) THEN
        DO 6 IC=1,4
          SIG(4*(INO-1)+IC) = SIG(4*(INO-1)+IC)
     &                      + B(1,IC)+B(2,IC)*XINO+B(3,IC)*YINO
     &                      + B(4,IC)*XINO*YINO
     &                      + B(5,IC)*XINO*XINO
     &                      + B(6,IC)*YINO*YINO
     &                      + B(7,IC)*XINO*XINO*YINO
     &                      + B(8,IC)*XINO*YINO*YINO
6       CONTINUE
      ELSE IF(NNO.EQ.9) THEN
        DO 7 IC=1,4
          SIG(4*(INO-1)+IC) = SIG(4*(INO-1)+IC)
     &                     + B(1,IC)+B(2,IC)*XINO+B(3,IC)*YINO
     &                     + B(4,IC)*XINO*YINO
     &                     + B(5,IC)*XINO*XINO
     &                     + B(6,IC)*YINO*YINO
     &                     + B(7,IC)*XINO*XINO*YINO
     &                     + B(8,IC)*XINO*YINO*YINO
     &                     + B(9,IC)*XINO*XINO*YINO*YINO
7       CONTINUE
      ELSE
       CALL UTMESS('F','ZZPOLY',' ERREUR: ELEMENT NON 2D')
      ENDIF
      END

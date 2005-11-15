      SUBROUTINE DFFNO ( ELREFE, NDIM, NNO, NNOS, DFF )
      IMPLICIT NONE
      CHARACTER*(*)      ELREFE
      INTEGER            NDIM, NNO, NNOS
      REAL*8             DFF(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 14/11/2005   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C BUT:   CALCUL DES DERIVEES DES FONCTIONS DE FORMES
C        AUX NOEUDS D'UN ELREFE
C
      REAL*8      X(27*3),VOL,TAB(3,27)
      INTEGER     DIMD,NBFPG,NBPG(10),INO,IDERI,IFONC,IBI1,IBI2
      CHARACTER*8 FAPG(10)
C ----------------------------------------------------------------------
C      
      CALL ELRACA(ELREFE,NDIM,NNO,NNOS,NBFPG,FAPG,NBPG,X,VOL)

      DIMD = NDIM*NNO

      DO 10 INO=1,NNO

        CALL ELRFDF(ELREFE,X(NDIM*(INO-1)+1),DIMD,TAB,IBI1,IBI2)
     
        DO 20 IDERI = 1 , NDIM
          DO 30 IFONC = 1 , NNO
             DFF((INO-1)*NNO*NDIM+(IDERI-1)*NNO+IFONC)=TAB(IDERI,IFONC)
 30       CONTINUE
 20     CONTINUE

 10   CONTINUE

      END

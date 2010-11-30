      SUBROUTINE  PMATHM(DIMMAT,DIMDEF,DIMCON,DIMUEL,
     +                                       DSDE,DRDS,CK,B,POIDS,MATRI)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/08/2005   AUTEUR ROMEO R.FERNANDES 
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
C ======================================================================
       IMPLICIT   NONE
       INTEGER    DIMDEF,DIMCON,DIMUEL,DIMMAT
       REAL*8     DSDE(DIMCON,DIMDEF),DRDS(DIMDEF,DIMCON),POIDS
       REAL*8     CK(DIMDEF),B(DIMDEF,DIMUEL),MATRI(DIMMAT,DIMMAT)
C ======================================================================
C --- BUT : PRODUIT DES MATRICES BT,C,DRDS,D,DSDE,F,B*POIDS ------------
C ---       CONTRIBUTION DU POINT D'INTEGRATION A DF -------------------
C ---       C,F,D SONT DIAGONALES --------------------------------------
C ======================================================================
       INTEGER    I,J,K
       REAL*8     G(DIMCON,DIMUEL),H(DIMDEF,DIMUEL)
C ======================================================================
C --- ON FAIT LE CALCUL EN TROIS FOIS ----------------------------------
C ======================================================================
       DO 10 I=1,DIMCON
          DO 20 J=1,DIMUEL
             G(I,J) = 0.D0
             DO 30 K=1,DIMDEF
                G(I,J) = G(I,J) + DSDE(I,K)*B(K,J)
 30          CONTINUE
 20       CONTINUE
 10    CONTINUE
       DO 40 I=1,DIMDEF
          DO 50 J=1,DIMUEL
             H(I,J)= 0.D0
             DO 60 K=1,DIMCON
                H(I,J) = H(I,J) + CK(I)*DRDS(I,K)*G(K,J)
 60          CONTINUE
 50       CONTINUE
 40    CONTINUE 
       DO 70 I=1,DIMUEL
          DO 80 J=1,DIMUEL
             DO 90 K=1,DIMDEF
                MATRI(I,J) = MATRI(I,J) + B(K,I)*H(K,J)*POIDS
 90          CONTINUE
 80       CONTINUE
 70    CONTINUE
C ======================================================================
       END

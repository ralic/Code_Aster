         SUBROUTINE CJSQQ( GAMMA, RCOS3T, Q, QII, HTQ,
     >                     DINVQ, QQ, QQII )
        IMPLICIT NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 27/03/2002   AUTEUR VABHHTS J.PELLET 
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
C     ------------------------------------------------------------------
C     CALCUL DU TENSEUR QQ, DE QQII ET DU TENSEUR DINVQ
C     ------------------------------------------------------------------
C     IN
C          GAMMA    :  PARAMETRE MATERIAU
C          HTQ      :
C          RCOS3T   :
C          Q        :
C          QII      :
C     OUT
C          DINVQ    :
C          QQ       :
C          QQII     :
C     ------------------------------------------------------------------

        INTEGER       NDT, NDI


        COMMON /TDIM/   NDT, NDI

        REAL*8        GAMMA
        REAL*8        HTQ,RCOS3T,Q(6), QII
        REAL*8        QQ(6), QQII
        REAL*8        INVQ(6), DINVQ(6)
        INTEGER       I
        REAL*8        COEF1,COEF2
        REAL*8        UN, D12,CINQ,SIX
C
        PARAMETER     ( D12  = .5D0   )
        PARAMETER     ( UN   = 1.D0   )
        PARAMETER     ( CINQ = 5.D0   )
        PARAMETER     ( SIX  = 6.D0   )
C
        CALL LCINVM(Q,INVQ)
        CALL LCDEVI(INVQ,DINVQ)
        COEF1 = ( UN + D12*GAMMA*RCOS3T ) / QII / HTQ**CINQ
        COEF2 = GAMMA * RCOS3T * QII / SIX / HTQ**CINQ
C
        DO 10 I=1, NDT
           QQ(I) = COEF1*Q(I) + COEF2*DINVQ(I)
 10     CONTINUE
        CALL LCPRSC(QQ,QQ,QQII)
        QQII = SQRT(QQII)
        END

         SUBROUTINE CJSC3Q( SIG, X, PA,QINIT,Q,QII,COS3TQ ,DEVNUL,TRAC)
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
C     CALCUL DE COS(3*( ANGLE DE LODES POUR Q))
C     ------------------------------------------------------------------
C     IN
C          SIG      :  CONTRAINTES
C          X        :  VARIABLES ECROUI CINE
C          PA       :  PRESS ATMOSPHERIQUE ( DONNEE AMTERIAU)
C          QINIT    :   DONNEE AMTERIAU
C
C     OUT
C          Q        : DEV(SIG)-TRACE(SIG)*X
C          QII      : SQRT(QIJ*QIJ)
C          COS3TQ   : SQRT(54)*DET(Q)/(QII**3)
C          DEVNUL   : VRAI SI DEVIATEUR DE Q NUL
C          TRAC     : VRAI SI I1  NUL
C
C     ------------------------------------------------------------------

        INTEGER       NDT, NDI


        COMMON /TDIM/   NDT, NDI

        REAL*8        SIG(6),X(6), PA,QINIT,Q(6),QII,COS3TQ
        LOGICAL       DEVNUL,TRAC
        REAL*8        I1,S(6),QIIREL
        REAL*8        DETQ,PREF
        INTEGER       I
        REAL*8        ZERO,UN,TROIS,EPSSIG
C
        PARAMETER     ( ZERO  = 0.D0   )
        PARAMETER     ( UN    = 1.D0   )
        PARAMETER     ( TROIS = 3.D0   )
        PARAMETER     ( EPSSIG = 1.D-8   )
C



C
        I1 = ZERO
        DO 10 I = 1 , NDI
         I1 = I1 + SIG(I)
   10   CONTINUE


         IF((I1+QINIT)  .EQ. 0.D0 ) THEN
          I1  = -QINIT+1.D-12 * PA
          PREF = ABS(PA)
         ELSE
          PREF = ABS(I1+QINIT)
         ENDIF



        IF ( (I1+QINIT).GT.0.D0) THEN
         TRAC = .TRUE.
        ELSE
         TRAC = .FALSE.
        ENDIF


        CALL LCDEVI(SIG,S)
        CALL CJSQIJ(S, I1, X, Q)
        CALL LCPRSC(Q,Q,QII)
        QII = SQRT(QII)
        QIIREL = QII/PREF
        CALL LCDETE(Q, DETQ)
        IF(QIIREL .GT. EPSSIG ) THEN
         DEVNUL = .FALSE.
         COS3TQ = SQRT(54.D0)*DETQ/QII**TROIS
        ELSE
         COS3TQ = UN
         DEVNUL = .TRUE.
        ENDIF
C
        IF( COS3TQ .GE.  1.D0 ) COS3TQ =  0.999999999999999D0
        IF( COS3TQ .LE. -1.D0 ) COS3TQ = -0.999999999999999D0
        END

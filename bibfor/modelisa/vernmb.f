        SUBROUTINE VERNMB(ICL,CNL,IER,IRTETI)
        IMPLICIT REAL*8 (A-H,O-Z)
C       ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 08/03/2004   AUTEUR REZETTE C.REZETTE 
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
C       ----------------------------------------------------------------
C       VERIFIE QUE L ITEM LUT EST UN NOMBRE ( NOMBRE ATTENDU )
C       ----------------------------------------------------------------
C       NOMBRE          =       REEL OU ENTIER (LXSCAN)
C       IN      ICL     =       CLASSE ITEM
C               CNL     =       NUMERO LIGNE
C       OUT     IER     =       0 > VRAI ( RETURN )
C                       =       1 > FAUX ( RETURN 1 )
C       ----------------------------------------------------------------
        INTEGER         ICL , IER
        CHARACTER*14    CNL
        CHARACTER*16    CMD
        COMMON          /OPMAIL/                CMD
C
        IRTETI = 0
        IF(ICL.NE.1.AND.ICL.NE.2)THEN
        CALL UTMESS('E',CMD,CNL//' UN NOMBRE EST ATTENDU')
        IER=1
        IRTETI = 1
        GOTO 9999
        ELSE
        IRTETI = 0
        GOTO 9999
        ENDIF
C
 9999   CONTINUE
        END

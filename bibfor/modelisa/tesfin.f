        SUBROUTINE TESFIN(ICL,IV,CV,IRTETI)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C       ----------------------------------------------------------------
C       TESTE SI  L ITEM LUT EN DEBUT DE LIGNE EST
C       LE MOT CLE FIN OU LE MOT CLE FINSF ( EVENTUALITE )
C       ----------------------------------------------------------------
C       IN      ICL     =       CLASSE ITEM
C               IV      =       TAILLE ITEM CARACTERE
C               CV      =       ITEM A TESTER
C       OUT     ( RETURN )      MOT CLE FIN ET FINSF NON RECONNUS
C               ( RETURN 1 )    MOT CLE FIN RECONNU
C               ( RETURN 2 )    MOT CLE FINSF RECONNU
C       ----------------------------------------------------------------
        INTEGER         ICL , IV
        CHARACTER*(*)   CV
        CHARACTER*8     MCL
C-----------------------------------------------------------------------
      INTEGER IRTETI 
C-----------------------------------------------------------------------
        IRTETI = 0
C
        IF(ICL.EQ.3.AND.IV.LE.8)THEN
        MCL='        '
        MCL(1:IV)=CV(1:IV)
        IF(MCL.EQ.'FIN     ')THEN
        IRTETI = 1
        GOTO 9999
        ENDIF
        IF(MCL.EQ.'FINSF   ')THEN
        IRTETI = 2
        GOTO 9999
        ENDIF
        ENDIF
C
        IRTETI = 0
 9999   CONTINUE
        END

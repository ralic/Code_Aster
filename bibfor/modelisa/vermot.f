        SUBROUTINE VERMOT(ICL,IV,CV,CNL,IER,IRTETI)
        IMPLICIT REAL*8 (A-H,O-Z)
C       ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 02/10/95   AUTEUR GIBHHAY A.Y.PORTABILITE 
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
C       VERIFIE QUE L ITEM LUT EST UN MOT ( MOT ATTENDU )
C       DIFFERENT DE FIN OU FINSF (RESERVES)
C       ----------------------------------------------------------------
C       MOT             =       IDENTIFICATEUR (LXSCAN) <= 8 CARACTERES
C       IN      ICL     =       CLASSE ITEM
C               IV      =       TAILLE ITEM CARACTERE
C               CNL     =       NUMERO LIGNE
C       OUT     IER     =       0       > VRAI  ( RETURN )
C                       =       1       > FAUX  ( RETURN 1 )
C       ----------------------------------------------------------------
        INTEGER         ICL , IV , IER
        CHARACTER*14    CNL
        CHARACTER*16    CMD , NOM
        CHARACTER*8     MCL
        CHARACTER*(*)   CV
        COMMON          /OPMAIL/        CMD
C
        IRTETI = 0
        IF(ICL.NE.3)THEN
        IF(IV.GT.16)JV=16
        IF(IV.LE.16)JV=IV
        NOM = CV(1:JV)
        CALL UTMESS('E',CMD,CNL//' UN IDENTIFICATEUR EST ATTENDU : '//
     +  '                            "'//NOM(1:JV)//'" N EN EST PAS UN')
        IER = 1
        IRTETI = 1
        GOTO 9999
        ENDIF
C
        IF(IV.GT.8)THEN
        CALL UTMESS('E',CMD,CNL//' UN IDENTIFICATEUR DEPASSE 8 '//
     +  'CARACTERES')
        IER = 1
        IRTETI = 1
        GOTO 9999
        ENDIF
C
        MCL = '        '
        MCL(1:IV) = CV(1:IV)
        IF(MCL.EQ.'FIN     ')THEN
        CALL UTMESS('E',CMD,CNL//' LE MOT CLE FIN N EST PAS ATTENDU')
        IER = 1
        IRTETI = 1
        GOTO 9999
        ENDIF
        IF(MCL.EQ.'FINSF   ')THEN
        CALL UTMESS('E',CMD,CNL//' LE MOT CLE FINSF N EST PAS ATTENDU')
        IER = 1
        IRTETI = 1
        GOTO 9999
        ENDIF
C
        IRTETI = 0
 9999   CONTINUE
        END

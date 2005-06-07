        SUBROUTINE LECINT(IFL,ICL,IV,RV,CV,CNL,MCL,NBM,NBG,IER,IRTETI)
        IMPLICIT REAL*8 (A-H,O-Z)
C       ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 06/04/2004   AUTEUR DURAND C.DURAND 
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
C       PREMIERE LECTURE POUR UN MOT CLE INTERFACE OU NON RECONNU
C       ----------------------------------------------------------------
C       IN      IFL,ICL,IV,RV,CV,CNL = VOIR LIRITM
C               MCL             = MOTS CLE TYPE INTERFACE
C               NBG             = NIVEAU DEBUG ASSISTANCE
C               NBM             = NB DE MOTS CLES TYPE INTERFACE
C       OUT     IER             = 0 > LECTURE CORRECTE
C                               = 1 > ERREUR EN LECTURE
C               (RETURN 1)      = EXIT            (MOT CLE FIN TROUVE)
C               (RETURN 2)      = LIGNE SUIVANTE  (MOT CLE FINSF TROUVE
C                                                  OU ERREUR DETECTE)
C       ----------------------------------------------------------------
C
        REAL*8          RV
        CHARACTER*8     MCL(NBM)
        INTEGER         DEBLIG
        CHARACTER*14    CNL
        CHARACTER*16    CMD,NOM
        COMMON          /OPMAIL/        CMD
        CHARACTER*(*)   CV
C
        IRTETI = 0
        IFM = IUNIFI('MESSAGE')
        IF(NBG.GE.1)WRITE(IFM,*)' ----- LECINT'
C
C -     ITEM = MOT CLE INTERFACE OU AUTRE ?
C
        DO 4 I = 1,NBM
        CALL TESMCL(ICL,IV,CV,MCL(I),IRTET)
        IF ( IRTET.GT.0 ) GOTO (4), IRTET
        GOTO 6
 4      CONTINUE
C
C -     MOT CLE NON RECONNU
C
        IF(ICL.EQ.1)CALL CODENT(IV,'G',NOM)
        IF(ICL.EQ.2)WRITE(NOM,'(F14.6)')RV
        IF(ICL.EQ.3.OR.ICL.EQ.4)NOM = CV(1:IV)
        CALL UTMESS('E',CMD,CNL//' ERREUR DE SYNTAXE : MOT CLE "'//NOM//
     +  '" NON RECONNU')
        IER = 1
        GOTO 5
C
C -     MOT CLE INTERFACE RECONNU
C       ON SORT PAR FIN OU FINSF OU FIN DE FICHIER
C
 6      CONTINUE
        NOM = CV(1:IV)
        CALL UTMESS('I',CMD,CNL//' MOT CLE "'//NOM//'" IGNORE')
C
 5      CONTINUE
        DEBLIG = -1
        CALL LIRITM(IFL,ICL,IV,RV,CV,CNL,DEBLIG,1)
        IF(NBG.GE.1)WRITE(IFM,*)'       LIRITM : ICL = ',ICL,
     +  ' IV = ',IV,' RV = ',RV,' CV(1:8) = ',CV(1:8),' DEBLIG =',DEBLIG
C
C -     ITEM = MOT  CLE FIN  OU FINSF ?
C
        CALL TESFIN(ICL,IV,CV,IRTET)
        IF ( IRTET.GT.0 ) GOTO (1,2), IRTET
C
        GOTO 5
C
 1      CONTINUE
        IRTETI = 1
        GOTO 9999
 2      CONTINUE
        IRTETI = 2
        GOTO 9999
C
 9999   CONTINUE
        END

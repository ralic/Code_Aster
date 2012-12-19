        SUBROUTINE LECDBG(IFL,ICL,IV,RV,CV,CNL,MCL,NBM,NBG,
     &  DIM,NOB,IRTETI)
      IMPLICIT NONE
C       ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 19/12/2012   AUTEUR PELLET J.PELLET 
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
C       ----------------------------------------------------------------
C       PREMIERE LECTURE DES DONNEES POUR UN MOT CLE DE TYPE GROUPE
C       ----------------------------------------------------------------
C       IN      IFL,ICL,IV,RV,CV,CNL = VOIR LIRITM
C               MCL             = MOTS CLE TYPE DEBUG
C               NBM             = NB DE MOTS CLES TYPE DEBUG
C                               = 1 > ERREUR EN LECTURE
C               DIM             = NB DE NOMS LUS PAR MOT CLE DEBUG
C               NOB             = NOMS LUS
C               NBG             = NIVEAU DEBUG
C               (RETURN)        = MOT CLE SUIVANT (MOT CLE NON RECONNU)
C               (RETURN 1)      = EXIT            (MOT CLE FIN TROUVE)
C               (RETURN 2)      = LIGNE SUIVANTE  (MOT CLE FINSF TROUVE
C                                                  OU ERREUR DETECTE)
C       ----------------------------------------------------------------
C
        REAL*8          RV
        CHARACTER*8     MCL(NBM)
        INTEGER         DIM(NBM),       DEBLIG
        CHARACTER*14    CNL
        CHARACTER*16    CMD
        CHARACTER*24    NOB(50,NBM),    B24,    MTC
        CHARACTER*(*)   CV
        COMMON          /OPMAIL/        CMD
        SAVE B24
C-----------------------------------------------------------------------
      INTEGER I ,ICL ,IFL ,IFM ,IRTET ,IRTETI
      INTEGER IUNIFI ,IV ,NBG ,NBM ,NUMTCL
C-----------------------------------------------------------------------
        DATA B24        /'                        '/
        IRTETI = 0
C
        IFM = IUNIFI('MESSAGE')
C
C ----- ITEM = MOT CLE TYPE  DEBUG ?
C
        DO 4 I = 1,NBM
        CALL TESMCL(ICL,IV,CV,MCL(I),IRTET)
        IF ( IRTET.GT.0 ) GOTO (4), IRTET
        NUMTCL = I
        GOTO 5
 4      CONTINUE
        GOTO 3
C
C ----- LECTURE DES NOMS D OBJETS A DUMPER ?
C
 5      CONTINUE
        WRITE(IFM,*)' ----- LECDBG'
 6      CONTINUE
        CALL LIRITM(IFL,ICL,IV,RV,CV,CNL,DEBLIG,1)
        WRITE(IFM,*)'       LIRITM : ICL = ',ICL,
     &  ' IV = ',IV,' RV = ',RV,' CV(1:8) = ',CV(1:8),' DEBLIG =',DEBLIG
        IF(DEBLIG.EQ.1) THEN
           CALL TESFIN(ICL,IV,CV,IRTET)
           IF ( IRTET.GT.0 ) GOTO (1,2), IRTET
        ENDIF
C
C - MOT CLE DUMP
C
        IF(NUMTCL.EQ.1)THEN
        IF(ICL.NE.4.AND.ICL.NE.3)THEN
        CALL U2MESK('F','MODELISA4_78',1,MCL(1))
        ELSEIF(IV.GT.24)THEN
        CALL U2MESK('F','MODELISA4_79',1,MCL(1))
        ENDIF
        DIM(1) = DIM(1) + 1
        MTC = B24
        MTC(1:IV) = CV(1:IV)
        NOB(DIM(1),1) = MTC
        GOTO 6
        ENDIF
C
C - MOT CLE DEBUG
C
        IF(NUMTCL.EQ.2)THEN
        IF(ICL.NE.1.AND.ICL.NE.2)THEN
        CALL U2MESK('F','MODELISA4_78',1,MCL(2))
        ENDIF
        IF(ICL.EQ.1)NBG = IV
        IF(ICL.EQ.2)NBG = NINT(RV)
        IF(NBG.GT.1)NBG = 1
        WRITE(IFM,*)' ------------ DEBUG NIVEAU ',NBG,' --------------'
        GOTO 6
        ENDIF
C
 1      CONTINUE
        IRTETI = 1
        GOTO 9999
 2      CONTINUE
        IRTETI = 2
        GOTO 9999
 3      CONTINUE
        IRTETI = 0
        GOTO 9999
C
 9999   CONTINUE
        END

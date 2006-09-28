        SUBROUTINE LECGRP(IFL,ICL,IV,RV,CV,CNL,MCL,NBM,NBG,
     &  DIM,NBT,IER,IRTETI)
        IMPLICIT REAL*8 (A-H,O-Z)
C       ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C       PREMIERE LECTURE DES DONNEES POUR UN MOT CLE DE TYPE GROUPE
C       ----------------------------------------------------------------
C       IN      IFL,ICL,IV,RV,CV,CNL = VOIR LIRITM
C               MCL             = MOTS CLE TYPE GROUPE
C               NBG             = NIVEAU DEBUG
C               NBM             = NB DE MOTS CLES TYPE GROUPE
C       OUT     IER             = 0 > LECTURE CORRECTE
C                               = 1 > ERREUR EN LECTURE
C               DIM             = DIMENSIONS DES OBJETS GROUPE..(NB GR.)
C               NBT             = NB TOTAL DE NOEUDS/MAILLES LUS
C               (RETURN)        = MOT CLE SUIVANT (MOT CLE NON RECONNU)
C               (RETURN 1)      = EXIT            (MOT CLE FIN TROUVE)
C               (RETURN 2)      = LIGNE SUIVANTE  (MOT CLE FINSF TROUVE
C                                                  OU ERREUR DETECTE)
C       ----------------------------------------------------------------
C
        REAL*8          RV
        CHARACTER*8     MCL(NBM) ,NOM
        INTEGER         DIM(NBM),       NBT(NBM),       DEBLIG
        CHARACTER*14    CNL
        CHARACTER*16    CMD
        COMMON          /OPMAIL/        CMD
        CHARACTER*(*)   CV
        IRTETI = 0
C
        IFM = IUNIFI('MESSAGE')
C
C ----- ITEM = MOT CLE TYPE  GROUPE ?
C
        DO 4 I = 1,NBM
        CALL TESMCL(ICL,IV,CV,MCL(I),IRTET)
        IF ( IRTET.GT.0 ) GOTO (4), IRTET
        NUMTCL = I
        NBTAV = NBT(NUMTCL)
        GOTO 5
 4      CONTINUE
        GOTO 3
C
C ----- LIRE ITEM SUIVANT ( = NOM DU GROUPE ?)
C
 5      CONTINUE
        INOM=1
        ILEC=1
        DEBLIG=0
        CALL LIRTET(IFL,IFM,ILEC,INOM,CNL,NOM,ICL,IV,RV,CV,DEBLIG)
C        WRITE(IFM,*)' NUMTCL             = ',NUMTCL
C        WRITE(IFM,*)' NBT(NUMTCL) ENTREE = ',NBT(NUMTCL)
C        WRITE(IFM,*)' DIM(NUMTCL) ENTREE = ',DIM(NUMTCL)
C        WRITE(IFM,*)' CV(1:8) APRES LIRTET = ',CV(1:8)
C        WRITE(IFM,*)' NOM AVANT =',NOM
           IF(NOM.EQ.'INDEFINI') THEN
C
C -----    LECTURE NOM DU GROUPE SI IL N Y A PAS D'ENTETE
C
           IF(NBG.GE.1)WRITE(IFM,*)'       LIRITM : ICL = ',ICL,
     &     ' IV = ',IV,' RV = ',RV,' CV(1:8) = ',CV(1:8),
     &     ' DEBLIG =',DEBLIG
           CALL VERDBL(DEBLIG,CNL,IER,IRTET)
           IF ( IRTET.GT.0 ) GOTO (2), IRTET
           CALL TESFIN(ICL,IV,CV,IRTET)
           IF ( IRTET.GT.0 ) GOTO (7,8), IRTET
           CALL VERMOT(ICL,IV,CV,CNL,IER,IRTET)
           IF ( IRTET.GT.0 ) GOTO (2), IRTET
           NOM = CV(1:IV)
           ELSE
C
C -----    LECTURE PREMIER NOM DE NOEUD / MAILLE OU FIN APRES L'ENTETE
C
           CALL TESFIN(ICL,IV,CV,IRTET)
           IF ( IRTET.GT.0 ) GOTO (7,8), IRTET
           CALL VERMOT(ICL,IV,CV,CNL,IER,IRTET)
           IF ( IRTET.GT.0 ) GOTO (2), IRTET
           NBT(NUMTCL) = NBT(NUMTCL) + 1
           ENDIF
C          WRITE(IFM,*)' NOM APRES =',NOM
C
C ----- LECTURE DES NOMS DE NOEUDS OU MAILLES DU GROUPE
C
 6      CONTINUE
        CALL LIRITM(IFL,ICL,IV,RV,CV,CNL,DEBLIG,1)
        IF(NBG.GE.1)WRITE(IFM,*)'       LIRITM : ICL = ',ICL,
     &  ' IV = ',IV,' RV = ',RV,' CV(1:8) = ',CV(1:8),' DEBLIG =',DEBLIG
C
        IF(DEBLIG.EQ.1)CALL TESFIN(ICL,IV,CV,IRTET)
        IF ( IRTET.GT.0 ) GOTO (7,8), IRTET
C
        CALL VERMOT(ICL,IV,CV,CNL,IER,IRTET)
        IF ( IRTET.GT.0 ) GOTO (2), IRTET
C
        NBT(NUMTCL) = NBT(NUMTCL) + 1
        GOTO 6
C
 7      CONTINUE
        IFN = 0
        GOTO 9
 8      CONTINUE
        IFN = 1
C
 9      CONTINUE
        IF((NBTAV-NBT(NUMTCL)).EQ.0)THEN
        CALL UTMESS('A',CMD,CNL//' LE GROUPE '//NOM//' EST VIDE')
C        CALL U2MESK('A','MODELISA4_80', 2 ,VALK)
        IER = 1
        GOTO 2
        ENDIF

        DIM(NUMTCL) = DIM(NUMTCL) + 1
C        WRITE(IFM,*)' NBT(NUMTCL) SORTIE = ',NBT(NUMTCL)
C        WRITE(IFM,*)' DIM(NUMTCL) SORTIE = ',DIM(NUMTCL)
        IF(IFN.EQ.0)GOTO 1
        IF(IFN.EQ.1)GOTO 2
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

        SUBROUTINE LECMAI(IFL,ICL,IV,RV,CV,CNL,MCL,NBM,NBG,
     +  FMT,DIM,NBT,IER,IRTETI)
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
C       PREMIERE LECTURE DES DONNEES POUR UN MOT CLE DE TYPE MAILLE
C       ----------------------------------------------------------------
C       IN      IFL,ICL,IV,RV,CV,CNL = VOIR LIRITM
C               MCL             = MOTS CLE TYPE MAILLE
C               NBM             = NB DE MOTS CLES TYPE MAILLE
C               NBG             = NIVEAU DEBUG
C               FMT             = NB NOEUDS A LIRE PAR MAILLE
C       OUT     IER             = 0 > LECTURE CORRECTE
C                               = 1 > ERREUR EN LECTURE
C               DIM             = DIMENSIONS DE L OBJET CONNEX (NB MAIL)
C               NBT             = NB TOTAL DE NOEUDS LUS
C               (RETURN)        = MOT CLE SUIVANT (MOT CLE NON RECONNU)
C               (RETURN 1)      = EXIT            (MOT CLE FIN TROUVE)
C               (RETURN 2)      = LIGNE SUIVANTE  (MOT CLE FINSF TROUVE
C                                                  OU ERREUR DETECTE)
C       ----------------------------------------------------------------
C
        REAL*8          RV
        CHARACTER*8     MCL(NBM),NOM
        INTEGER         DIM(NBM),       NBT(NBM),       DEBLIG
        CHARACTER*14    CNL
        CHARACTER*(*)   CV
        INTEGER         FMT(NBM)
        IRTETI = 0
C
        IFM = IUNIFI('MESSAGE')
C
C ----- ITEM = MOT CLE TYPE  MAILLE ?
C
        DO 4 I = 1,NBM
        CALL TESMCL(ICL,IV,CV,MCL(I),IRTET)
        IF ( IRTET.GT.0 ) GOTO (4), IRTET
        NUMTCL = I
        GOTO 7
 4      CONTINUE
        GOTO 3
C
C - LIRE ITEM SUIVANT
C
 7      CONTINUE
        IF(NBG.GE.1)WRITE(IFM,*)' ----- LECMAI'
C
C - LECTURE DE L'ENTETE
C
        INOM=0
        ILEC=1
        CALL LIRTET(IFL,IFM,ILEC,INOM,CNL,NOM,ICL,IV,RV,CV,DEBLIG)
        GO TO 9
 5      CONTINUE
        CALL LIRITM(IFL,IFM,ICL,IV,RV,CV,CNL,DEBLIG,1)
        IF(NBG.GE.1)WRITE(IFM,*)'       LIRITM : ICL = ',ICL,
     +  ' IV = ',IV,' RV = ',RV,' CV(1:8) = ',CV(1:8),' DEBLIG =',DEBLIG
 9      CONTINUE
C
C - ITEM EN DEBUT DE LIGNE ?
C
        CALL VERDBL(DEBLIG,CNL,IER,IRTET)
        IF ( IRTET.GT.0 ) GOTO (2), IRTET
C
C - ITEM = MOT  CLE FIN  OU FINSF ?
C
        CALL TESFIN(ICL,IV,CV,IRTET)
        IF ( IRTET.GT.0 ) GOTO (1,2), IRTET
C
C - ITEM = MOT (# FIN OU FINSF) = NOM DE LA MAILLE ?
C
        CALL VERMOT(ICL,IV,CV,CNL,IER,IRTET)
        IF ( IRTET.GT.0 ) GOTO (2), IRTET
C
C ---- LECTURE DES NOMS DES NOEUDS DE LA MAILLE
C
        DO 6 I = 1,FMT(NUMTCL)
C
C - LIRE ITEM SUIVANT
C
        CALL LIRITM(IFL,IFM,ICL,IV,RV,CV,CNL,DEBLIG,1)
        IF(NBG.GE.1)WRITE(IFM,*)'       LIRITM : ICL = ',ICL,
     +  ' IV = ',IV,' RV = ',RV,' CV(1:8) = ',CV(1:8),' DEBLIG =',DEBLIG
C
C - ITEM = MOT (# FIN OU FINSF) ?
C
        CALL VERMOT(ICL,IV,CV,CNL,IER,IRTET)
        IF ( IRTET.GT.0 ) GOTO (2), IRTET
C
C - INCREMENTATION DU NB TOTAL DE NOEUDS LUS
C
        NBT(NUMTCL) = NBT(NUMTCL) + 1
 6      CONTINUE
C
        DIM(NUMTCL) = DIM(NUMTCL) + 1
        GOTO 5
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

        SUBROUTINE LECTIT(IFL,ICL,IV,RV,CV,CNL,MCL,NBM,NBG,
     +  DIM,NBT,IER,IRTETI)
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
C       PREMIERE LECTURE DES DONNEES POUR  UN MOT CLE DE TYPE TITRE
C       ----------------------------------------------------------------
C       IN      IFL,ICL,IV,RV,CV,CNL =  VOIR LIRITM
C               MCL             = MOTS CLES TYPE TITRE
C               NBG             = NIVEAU DEBUG
C               NBM             = NB DE MOTS CLES TYPE TITRE
C       OUT     IER             = 0 > LECTURE CORRECTE
C                               = 1 > ERREUR EN LECTURE
C               DIM             = DIMENSION  OBJET TITRE(NB LIGNES)
C               NBT             = NB TOTAL DE LIGNES LUES(ICI NBT=DIM)
C               (RETURN)        = MOT CLE SUIVANT (MOT CLE NON RECONNU)
C               (RETURN 1)      = EXIT            (MOT CLE FIN TROUVE)
C               (RETURN 2)      = LIGNE SUIVANTE  (MOT CLE FINSF TROUVE)
C       ----------------------------------------------------------------
C
        REAL*8          RV
        CHARACTER*8     MCL(NBM)
        INTEGER         DIM(NBM),       NBT(NBM)
        CHARACTER*14    CNL
        CHARACTER*(*)   CV
        CHARACTER*16    NOP
        COMMON          /OPMAIL/        NOP
        CHARACTER*80    LIG
        IRTETI = 0
C
        IFM = IUNIFI('MESSAGE')
C
C - ITEM = MOT CLE  TITRE  ?
C
        CALL TESMCL(ICL,IV,CV,MCL(1),IRTET)
        IF ( IRTET.GT.0 ) GOTO (3), IRTET
        IF(NBG.GE.1)WRITE(IFM,*)' ----- LECTIT'
C
C - LIRE LIGNE SUIVANTE
C
 4      CONTINUE
        CALL LIRLIG(IFL,IFM,CNL,LIG,1)
C
        IF(NBG.GE.1)WRITE(IFM,*)'       LIRLIG :',CNL,LIG
C
C - LIRE PREMIER ITEM DE LA LIGNE
C
        IDEB = 1
        CALL LXSCAN(LIG,IDEB,ICL,IV,RV,CV)
C
        IF(NBG.GE.1)WRITE(IFM,*)'       LXSCAN : ICL = ',ICL,
     +  ' IV = ',IV,' RV = ',RV,' CV(1:8) = ',CV(1:8),' IDEB = ',IDEB
C
        IF(ICL.EQ.3)CALL LXCAPS(CV(1:IV))
C
C - ITEM = MOT  CLE FIN  OU FINSF ?
C
        CALL TESFIN(ICL,IV,CV,IRTET)
        IF ( IRTET.GT.0 ) GOTO (1,2), IRTET
        DIM(1)  = DIM(1) + 1
        NBT(1)  = NBT(1) + 1
C
        GOTO 4
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

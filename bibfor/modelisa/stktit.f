        SUBROUTINE STKTIT(IFL,ICL,IV,RV,CV,CNL,MCL,NBM,NLT,TIT,
     +                    DIM,IRTETI)
        IMPLICIT REAL*8 (A-H,O-Z)
C       ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 21/02/96   AUTEUR VABHHTS J.PELLET 
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
C       SECONDE LECTURE DES DONNEES POUR UN MOT CLE DE TYPE TITRE
C       ----------------------------------------------------------------
C       IN      IFL,ICL,IV,RV,CV,CNL = VOIR LIRITM
C               MCL             = MOTS CLES TYPE TITRE
C               NBM             = NB DE MOTS CLES TYPE TITRE
C               DIM             = DIMENSION OBJET TITRE (INUTILE ICI)
C               TIT             = NOMU//'           .TITR'
C               NLT             = NUMERO LIGNE COURANTE DU TITRE
C       OUT     (RETURN)        = MOT CLE SUIVANT (MOT CLE NON RECONNU)
C               (RETURN 1)      = EXIT            (MOT CLE FIN TROUVE)
C               (RETURN 2)      = LIGNE SUIVANTE  (MOT CLE FINSF TROUVE
C                                                  OU ERREUR DETECTE)
C       ----------------------------------------------------------------
C
        INTEGER         DEBLIG, DIM(NBM)
        REAL*8          RV
        CHARACTER*8     MCL(NBM)
        CHARACTER*14    CNL
        CHARACTER*(*)   CV
        CHARACTER*80    LIG
        CHARACTER*24    TIT
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
      CHARACTER*6        PGC
      COMMON  / NOMAJE / PGC
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CALL JEMARQ()
        PGC = 'OP0001'
        IRTETI = 0
        IFM = IUNIFI('MESSAGE')
C
C - ITEM = MOT CLE TYPE TITRE ?
C
        CALL TESMCL(ICL,IV,CV,MCL(1),IRTET)
        IF ( IRTET.GT.0 ) GOTO (3), IRTET
C
C - OUI > REQUETE EN ECRITURE POUR OBJET TITRE
C
        CALL JEVEUO(TIT,'E',IAD)
C
C - LIRE LIGNE SUIVANTE
C
 4      CONTINUE
        CALL LIRLIG(IFL,IFM,CNL,LIG,2)
C
C - LIRE PREMIER ITEM DE LA LIGNE
C
        IDEB   = 1
        CALL LXSCAN(LIG,IDEB,ICL,IV,RV,CV)
        IF(ICL.EQ.3)CALL LXCAPS(CV(1:IV))
C
C - ITEM = MOT  CLE FIN  OU FINSF ?
C
        CALL TESFIN(ICL,IV,CV,IRTET)
        IF ( IRTET.GT.0 ) GOTO (1,2), IRTET
C
C - STOCKAGE DE LA LIGNE NLT
C
        ZK80(IAD+NLT) = LIG
C
C - INCREMENTATION DU NUMERO DE LIGNE TITRE
C
        NLT = NLT + 1
C
C - LIGNE SUIVANTE
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
      CALL JEDEMA()
        END

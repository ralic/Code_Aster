        SUBROUTINE STKCOO(IFL,ICL,IV,RV,CV,CNL,MCL,NBM,NUM,COO,NNO,DIM,
     +  IRTETI)
        IMPLICIT REAL*8 (A-H,O-Z)
C       ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 03/05/2000   AUTEUR CIBHHAB N.RAHNI 
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
C       SECONDE LECTURE DES DONNEES POUR UN MOT CLE DE TYPE COORDONNEE
C       ----------------------------------------------------------------
C       IN      IFL,ICL,IV,RV,CV,CNL = VOIR LIRITM
C               MCL             = MOTS CLES TYPE COORDONNEE
C               NBM             = NB DE MOTS CLES TYPE COORDONNEE
C               DIM             = DIMENSION OBJET COORDONNEE
C               COO             = NOMU.COORDO.VALE
C               NNO             = NOMU.NOMNOE
C               NUM             = NUMERO DU NOEUD COURANT
C       OUT     (RETURN)        = MOT CLE SUIVANT (MOT CLE NON RECONNU)
C               (RETURN 1)      = EXIT            (MOT CLE FIN TROUVE)
C               (RETURN 2)      = LIGNE SUIVANTE  (MOT CLE FINSF TROUVE
C                                                  OU ERREUR DETECTE)
C       ----------------------------------------------------------------
C
        INTEGER         DEBLIG,         DIM(NBM)
        REAL*8          RV
        CHARACTER*8     MCL(NBM), NOM,  NOMN
        CHARACTER*14    CNL
        CHARACTER*(*)   CV
        CHARACTER*32    JEXNOM
        CHARACTER*24    COO,    NNO
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
      CALL JEMARQ()
      IRTETI = 0
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
        PGC = 'OP0001'
        IFM = IUNIFI('MESSAGE')
C
C - ITEM = MOTS CLES  TYPE COORDONNEES ?
C
        DO 4 I = 1,NBM
        CALL TESMCL(ICL,IV,CV,MCL(I),IRTET)
        IF ( IRTET.GT.0 ) GOTO (4), IRTET
        NUMTCL  = I
        GOTO 5
 4      CONTINUE
        GOTO 3
C
 5      CONTINUE
        CALL JEVEUO(COO,'E',IAD)
C
C - LECTURE DE L'ENTETE
C
        CALL LIRTET(IFL,IFM,2,0,CNL,NOM,ICL,IV,RV,CV,DEBLIG)
        GO TO 9
C
C - LIRE ITEM SUIVANT =  NOM DU NOEUD ?
C
 7      CONTINUE
        CALL LIRITM(IFL,IFM,ICL,IV,RV,CV,CNL,DEBLIG,2)
 9      CONTINUE
C
C - ITEM = MOT  CLE FIN  OU FINSF ?
C
        CALL TESFIN(ICL,IV,CV,IRTET)
        IF ( IRTET.GT.0 ) GOTO (1,2), IRTET
C
C - CREATION DE NOM_DU_NOEUD DANS LE REPERTOIRE NOMNOE
C
        NOMN        = '        '
        NOMN(1:IV)  = CV(1:IV)
        CALL JEEXIN (JEXNOM(NNO,NOMN),IRET)
        IF(IRET.EQ.0) THEN
           CALL JECROC(JEXNOM(NNO,NOMN))
        ELSE
           CALL UTMESS('F','STKCOO','LE NOM '//NOMN//' EXISTE DEJA')
        ENDIF
C
C - INCREMENTATION NUMERO DU NOEUD
C
        NUM = NUM + 1
        IDEC = IAD + (NUM-1) * 3
C       IDEC = IAD + (NUM-1) * NUMTCL
C
C - STOCKAGE DES  COORDONNEES DU NOEUD
C
        DO 10 I = 1 , 3
        ZR(IDEC+I-1) = 0.D0
 10     CONTINUE
C
        DO 6 I = 1,NUMTCL
        CALL LIRITM(IFL,IFM,ICL,IV,RV,CV,CNL,DEBLIG,2)
        IF(ICL.EQ.1)RV = IV
        ZR(IDEC+I-1) = RV
 6      CONTINUE
C
C - NOEUD SUIVANT
C
        GOTO 7
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

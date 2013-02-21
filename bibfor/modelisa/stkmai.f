        SUBROUTINE STKMAI(IFL,ICL,IV,RV,CV,CNL,MCL,NBM,NUME,NUMN,
     &  CNX,TYP,FMT,IRTETI)
      IMPLICIT NONE
C       ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 12/02/2013   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C       SECONDE LECTURE DES DONNEES POUR UN MOT CLE DE TYPE MAILLE
C       ----------------------------------------------------------------
C       IN      IFL,ICL,IV,RV,CV,CNL = VOIR LIRITM
C               MCL             = MOTS CLES TYPE MAILLE
C               NBM             = NB DE MOTS CLES TYPE MAILLE
C               FMT             = NB NOEUDS A LIRE / MAILLE
C               CNX             = NOMU.CONXV
C               TYP             = NOMU.TYPMAIL ASSOCIE A NOMU.NOMMAI
C               NUME            = NUMERO DE L ELEMENT COURANT
C               NUMN            = NUMERO DU NOEUD COURANT DANS CNX
C       OUT     (RETURN)        = MOT CLE SUIVANT (MOT CLE NON RECONNU)
C               (RETURN 1)      = EXIT            (MOT CLE FIN TROUVE)
C               (RETURN 2)      = LIGNE SUIVANTE  (MOT CLE FINSF TROUVE
C                                                  OU ERREUR DETECTE)
C       ----------------------------------------------------------------
C
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNOM
        REAL*8          RV
        CHARACTER*8     MCL(NBM),       NOMA,      B8
        INTEGER         DEBLIG,       FMT(NBM)
        CHARACTER*14    CNL
        CHARACTER*(*)   CV
        CHARACTER*24    CNX,    TYP,           NOM
        SAVE B8
C
C-----------------------------------------------------------------------
      INTEGER I ,IADC ,IADT ,ICL ,IFL ,IRET
      INTEGER IRTET ,IRTETI ,IV ,NBM ,NUME ,NUMN
      INTEGER NUMTCL
C-----------------------------------------------------------------------
        DATA B8         /'        '/
      CALL JEMARQ()
        IRTETI = 0
C
C
C - ITEM = MOT CLE  TYPE MAILLE ?
C
        DO 4 I = 1,NBM
        CALL TESMCL(ICL,IV,CV,MCL(I),IRTET)
        IF ( IRTET.GT.0 ) GOTO (4), IRTET
        NUMTCL = I
        GOTO 5
 4      CONTINUE
        GOTO 3
C
C
 5      CONTINUE
        CALL JEVEUO(CNX,'E',IADC)
        CALL JEVEUO(TYP,'E',IADT)
C
C - LECTURE DE L'ENTETE
C
        DEBLIG=0
        CALL LIRTET(IFL,2,0,CNL,NOM,ICL,IV,RV,CV,DEBLIG)
        GO TO 9
C
C - LIRE ITEM SUIVANT = NOM DE MAILLE ?
C
 7      CONTINUE
        CALL LIRITM(IFL,ICL,IV,RV,CV,CNL,DEBLIG,2)
 9      CONTINUE
C
C - ITEM = MOT  CLE FIN  OU FINSF ?
C
        CALL TESFIN(ICL,IV,CV,IRTET)
        IF ( IRTET.GT.0 ) GOTO (1,2), IRTET
C
C - CREATION DE CONXV.NOM_DE_MAILLE ET TYPMAIL.NOM_DE_MAILLE
C
        NOMA        = B8
        NOMA(1:IV)  = CV(1:IV)
        CALL JEEXIN (JEXNOM(TYP(1:8)//'.NOMMAI',NOMA),IRET)
        IF(IRET.EQ.0) THEN
           CALL JECROC(JEXNOM(TYP(1:8)//'.NOMMAI',NOMA))
           CALL JECROC(JEXNOM(CNX,NOMA))
           CALL JEECRA(JEXNOM(CNX,NOMA),'LONMAX',FMT(NUMTCL),' ')
        ELSE
           CALL U2MESK('F','MODELISA7_10',1,NOMA)
        ENDIF
C
C - STOCKAGE DES NOMS DES NOEUDS DE LA MAILLE ET DU TYPE DE MAILLE
C
        ZI(IADT+NUME) = NUMTCL
C
        DO 6      I = 1,FMT(NUMTCL)
        CALL LIRITM(IFL,ICL,IV,RV,CV,CNL,DEBLIG,2)
        NOM       = B8
        NOM(1:IV) = CV(1:IV)
C
        ZK8(IADC+NUMN) = NOM(1:8)
C
C - INCREMENTATION DU NB DE NOEUDS LUS
C
        NUMN      = NUMN + 1
 6      CONTINUE
C
C - INCREMENTATION DU NB D ELEMENTS LUS
C
        NUME = NUME + 1
C
C - MAILLE SUIVANTE
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

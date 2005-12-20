        SUBROUTINE STKGRP(IFL,ICL,IV,RV,CV,CNL,MCL,NBM,NUMN,NUMM,
     +  GRN,GRM,IRTETI)
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
C       SECONDE LECTURE DES DONNEES POUR UN MOT CLE DE TYPE GROUPE
C       ----------------------------------------------------------------
C       IN      IFL,ICL,IV,RV,CV,CNL = VOIR LIRITM
C               MCL             = MOTS CLES TYPE GROUPE
C               NBM             = NB DE MOTS CLES TYPE GROUPE
C               GRN             = NOMU.GROUPNOV
C               GRM             = NOMU.GROUPMAV
C               NUMN            = NUMERO DU NOEUD COURANT DANS GRN
C               NUMM            = NUMERO DE MAILLE COURANT DANS GRM
C       OUT     (RETURN)        = MOT CLE SUIVANT (MOT CLE NON RECONNU)
C               (RETURN 1)      = EXIT            (MOT CLE FIN TROUVE)
C               (RETURN 2)      = LIGNE SUIVANTE  (MOT CLE FINSF TROUVE
C                                                  OU ERREUR DETECTE)
C       ----------------------------------------------------------------
C
        REAL*8          RV
        CHARACTER*8     MCL(NBM),       NOMG,   NOM,    B8
        INTEGER         DEBLIG
        CHARACTER*14    CNL
        CHARACTER*(*)   CV
        CHARACTER*16    CMD
        CHARACTER*32    JEXNOM
        CHARACTER*24    GRN,    GRM,    GRP
        SAVE B8
        COMMON          /OPMAIL/        CMD
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
C     -------  FIN  COMMUNS NORMALISES  JEVEUX  ------------------------
        DATA B8         /'        '/
C
        PGC = 'OP0001'
        IFM = IUNIFI('MESSAGE')
C
C - ITEM = MOT CLE TYPE GROUPE ?
C
        DO 4 I = 1,NBM
        CALL TESMCL(ICL,IV,CV,MCL(I),IRTET)
        IF ( IRTET.GT.0 ) GOTO (4), IRTET
        IF(I.EQ.1)THEN
        GRP = GRN
        NUM = NUMN
        ELSE
        GRP = GRM
        NUM = NUMM
        ENDIF
        GOTO 10
 4      CONTINUE
        GOTO 3
C
 10     CONTINUE
        CALL JEVEUO(GRP,'E',IADG)
C
C - LIRE ITEM SUIVANT =  NOM DU GROUPE ?
C
        DEBLIG=0
        CALL LIRTET(IFL,IFM,2,1,CNL,NOMG,ICL,IV,RV,CV,DEBLIG)
C
C -----    LECTURE NOM DU GROUPE SI IL N Y A PAS D'ENTETE
C
           IF (NOMG.EQ.'INDEFINI') THEN
           NOMG        = B8
           NOMG(1:IV)  = CV(1:IV)
           CALL TESFIN(ICL,IV,CV,IRTET)
           IF ( IRTET.GT.0 ) GOTO (1,2), IRTET
           ELSE
C
C -----    STOCKAGE PREMIER NOM DE NOEUD / MAILLE OU FIN APRES L'ENTETE
C
           CALL TESFIN(ICL,IV,CV,IRTET)
           IF ( IRTET.GT.0 ) GOTO (1,2), IRTET
           NOM       = B8
           NOM(1:IV) = CV(1:IV)
           ZK8(IADG+NUM) = NOM
           NUM = NUM + 1
           ENDIF
C
C - STOCKAGE DES NOMS DES NOEUDS OU MAILLES DU GROUPE
C
 6      CONTINUE
        CALL LIRITM(IFL,ICL,IV,RV,CV,CNL,DEBLIG,2)
C
C - ITEM = MOT  CLE FIN  OU FINSF ?
C
        IF(DEBLIG.EQ.1) THEN
            CALL TESFIN(ICL,IV,CV,IRTET)
            IF ( IRTET.GT.0 ) GOTO (7,8), IRTET
        ENDIF
C
C - STOCKAGE DES NOEUDS OU MAILLES DU GROUPE
C
        NOM       = B8
        NOM(1:IV) = CV(1:IV)
        ZK8(IADG+NUM) = NOM
C
C - INCREMENTATION DU NB D'ITEM LUS
C
        NUM = NUM + 1
C
C - NOEUD OU MAILLE SUIVANT
C
        GOTO 6
C
C - SORTIE EN FIN OU FINSF
C
 7      CONTINUE
        IFN = 0
        GOTO 9
 8      CONTINUE
        IFN = 1
C
C - CREATION ET DIMENSIONNEMENT DE L OBJET GRP.NOM_DU_GROUPE
C
 9      CONTINUE
        IF(I.EQ.1)THEN
        NBITEM = NUM - NUMN
        NUMN = NUM
        ELSE
        NBITEM = NUM - NUMM
        NUMM = NUM
        ENDIF
C
          IF(NBITEM.NE.0) THEN
            CALL JEEXIN (JEXNOM(GRP,NOMG),IRET)
            IF(IRET.EQ.0) THEN
               CALL JECROC(JEXNOM(GRP,NOMG))
               CALL JEECRA(JEXNOM(GRP,NOMG),'LONMAX',NBITEM,' ')
            ELSE
               CALL UTMESS('F','STKGRP','LE GROUPE '//NOMG//
     +                     ' EXISTE DEJA')
            ENDIF
          ENDIF
C
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
      CALL JEDEMA()
        END

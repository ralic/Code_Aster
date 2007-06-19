        SUBROUTINE LIRLIG ( IFL, CNL, LIG, ILEC )
        IMPLICIT   NONE        
        INTEGER             IFL,           ILEC
        CHARACTER*14                  CNL
        CHARACTER*80                       LIG
C       ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 19/06/2007   AUTEUR VIVAN L.VIVAN 
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
C       LECTURE DE LA LIGNE SUIVANTE ET STOCKAGE DANS LE BUFFER LIG
C       ----------------------------------------------------------------
C       IN      IFL     = NUMERO UNITE FICHIER MAILLAGE
C               ILEC    = 1     >  PREMIERE LECTURE DU FICHIER
C                       = 2     >  SECONDE  LECTURE DU FICHIER
C       OUT     CNL     = NUMERO LIGNE LUE (CHAINE)
C               LIG     = LIGNE LUE
C       ----------------------------------------------------------------
        INTEGER         NL, NL1, NL2, I
        SAVE                NL1, NL2
        CHARACTER*16    CMD
        CHARACTER*255   LIRLG
        COMMON          /OPMAIL/        CMD
        DATA NL1,NL2    /0,0/
C
        CNL = ' '
        READ(UNIT=IFL,FMT=1,END=100) LIRLG
        DO 10 I = 81, 255
           IF ( LIRLG(I:I) .EQ. '%' ) GOTO 12
           IF ( LIRLG(I:I) .NE. ' ' ) THEN
              CALL U2MESK('F','MODELISA4_92',1,LIRLG)
           ENDIF
 10     CONTINUE
 12     CONTINUE
        LIG = LIRLG(1:80)
C
        IF(ILEC.EQ.1)THEN
          NL1 = NL1 + 1
          NL  = NL1
        ELSE
          NL2 = NL2 + 1
          NL  = NL2
        ENDIF
C
        CNL(1:14) = '(LIGNE       )'
        CALL CODENT(NL,'D',CNL(8:13))
C
        GOTO 9999
C
 100    CONTINUE
        IF ( NL1 .EQ. 0 ) THEN
           CALL U2MESS('F','MODELISA4_94')
        ELSE
           CALL U2MESI('F','MODELISA4_93',1,NL1)
        ENDIF
C
 1      FORMAT(A80)
 2      FORMAT(' <',I6,' >   ',A80)
C
 9999   CONTINUE
        END

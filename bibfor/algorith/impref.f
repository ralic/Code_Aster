      SUBROUTINE IMPREF(ICOD  ,SDSUIV,TITRE ,FORMA )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/12/2010   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      INTEGER      ICOD
      CHARACTER*24 SDSUIV
      CHARACTER*16 TITRE(3)
      INTEGER      FORMA
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (AFFICHAGE - ACCES SD)
C
C RETOURNE LES INFOS PAR DEFAUT D'UNE COLONNE DU TABLEAU DE CONVERGENCE
C
C ----------------------------------------------------------------------
C
C 
C  - LE TITRE DE LA COLONNE (SUR 3 LIGNES)
C  - LE TYPE DE LA COLONNE
C
C IN  ICOD   : CODE DE LA COLONNE
C IN  SDSUIV : NOM DE LA SD POUR SUIVI DDL
C OUT TITRE  : TITRES STANDARDS DE LA COLONNE
C OUT FORCOL : TYPE DE LA COLONNE
C              1: ENTIER
C              2: REEL
C              3: CHAINE
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      INTEGER      ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8       ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16   ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL      ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8  ZK8
      CHARACTER*16    ZK16
      CHARACTER*24        ZK24
      CHARACTER*32            ZK32
      CHARACTER*80                ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C 
      INTEGER      ZDEF
      PARAMETER    (ZDEF=32)
      INTEGER      J,ISUIV
      CHARACTER*16 TITCOL(3,ZDEF)
      INTEGER      FORCOL(ZDEF)


      DATA (TITCOL(J,1),J=1,3)/ '   ITERATIONS   ',
     &                          '     NEWTON     ',
     &                          '                '/
      DATA FORCOL(1) /1/


      DATA (TITCOL(J,2),J=1,3)/ '   INCREMENT    ',
     &                          '     TEMPS      ',
     &                          '                '/
      DATA FORCOL(2) /2/


      DATA (TITCOL(J,3),J=1,3)/ '     RESIDU     ',
     &                          '     RELATIF    ',
     &                          ' RESI_GLOB_RELA '/
      DATA FORCOL(3) /2/


      DATA (TITCOL(J,4),J=1,3)/ '     RESIDU     ',
     &                          '     ABSOLU     ',
     &                          ' RESI_GLOB_MAXI '/
      DATA FORCOL(4) /2/


      DATA (TITCOL(J,5),J=1,3)/ '     RESIDU     ',
     &                          '  PAR REFERENCE ',
     &                          '  CONTRAINTES   '/
      DATA FORCOL(5) /2/


      DATA (TITCOL(J,6),J=1,3)/ ' RESIDU RELATIF ',
     &                          '     MAXIMUM    ',
     &                          '    AU POINT    '/
      DATA FORCOL(6) /3/


      DATA (TITCOL(J,7),J=1,3)/ ' RESIDU ABSOLU  ',
     &                          '     MAXIMUM    ',
     &                          '    AU POINT    '/
      DATA FORCOL(7) /3/


      DATA (TITCOL(J,8),J=1,3)/ ' RESIDU REFE.   ',
     &                          '     MAXIMUM    ',
     &                          '    AU POINT    '/
      DATA FORCOL(8) /3/


      DATA (TITCOL(J,9),J=1,3)/ '  ITERATIONS    ',
     &                          '   RECH. LIN.   ',
     &                          '                '/
      DATA FORCOL(9) /1/


      DATA (TITCOL(J,10),J=1,3)/ '  COEFFICIENT   ',
     &                           '   RECH. LIN.   ',
     &                           '      RHO       '/
      DATA FORCOL(10) /2/


      DATA (TITCOL(J,11),J=1,3)/ '   PARAMETRE    ',
     &                           '    PILOTAGE    ',
     &                           '  ETA_PILOTAGE  '/
      DATA FORCOL(11) /2/


      DATA (TITCOL(J,12),J=1,3)/ '                ',
     &                           '                ',
     &                           '                '/
      DATA FORCOL(12) /2/


      DATA (TITCOL(J,13),J=1,3)/ '                ',
     &                           '                ',
     &                           '                '/
      DATA FORCOL(13) /2/

      DATA (TITCOL(J,14),J=1,3)/ '                ',
     &                           '                ',
     &                           '                '/
      DATA FORCOL(14) /1/


      DATA (TITCOL(J,15),J=1,3)/ '     OPTION     ',
     &                           '   ASSEMBLAGE   ',
     &                           '                '/
      DATA FORCOL(15) /3/


      DATA (TITCOL(J,16),J=1,3)/ '     DEBORST    ',
     &                           '                ',
     &                           '                '/
      DATA FORCOL(16) /3/


      DATA (TITCOL(J,17),J=1,3)/ '     CONTACT    ',
     &                           '    DISCRET     ',
     &                           '   ITERATIONS   '/
      DATA FORCOL(17) /1/


      DATA (TITCOL(J,18),J=1,3)/ '                ',
     &                           '                ',
     &                           '                '/
      DATA FORCOL(18) /2/


      DATA (TITCOL(J,19),J=1,3)/ '                ',
     &                           '                ',
     &                           '                '/
      DATA FORCOL(19) /3/


      DATA (TITCOL(J,20),J=1,3)/ '     CONTACT    ',
     &                           '    BCL. GEOM.  ',
     &                           '   ITERATIONS   '/
      DATA FORCOL(20) /1/


      DATA (TITCOL(J,21),J=1,3)/ '     CONTACT    ',
     &                           '    BCL. FROT.  ',
     &                           '   ITERATIONS   '/
      DATA FORCOL(21) /1/


      DATA (TITCOL(J,22),J=1,3)/ '     CONTACT    ',
     &                           '    BCL. CONT.  ',
     &                           '   ITERATIONS   '/
      DATA FORCOL(22) /1/


      DATA (TITCOL(J,23),J=1,3)/ '   SUIVI DDL    ',
     &                           '      01        ',
     &                           '                '/
      DATA FORCOL(23) /2/


      DATA (TITCOL(J,24),J=1,3)/ '   SUIVI DDL    ',
     &                           '      02        ',
     &                           '                '/
      DATA FORCOL(24) /2/


      DATA (TITCOL(J,25),J=1,3)/ '   SUIVI DDL    ',
     &                           '      03        ',
     &                           '                '/     
      DATA FORCOL(25) /2/


      DATA (TITCOL(J,26),J=1,3)/ '   SUIVI DDL    ',
     &                           '      04        ',
     &                           '                '/     
      DATA FORCOL(26) /2/


      DATA (TITCOL(J,27),J=1,3)/ '     FETI       ',
     &                           '                ',
     &                           '   ITERATIONS   '/
      DATA FORCOL(27) /1/


      DATA (TITCOL(J,28),J=1,3)/ '     CONTACT    ',
     &                           '       BCL.     ',
     &                           '    MAX. VAL.   '/
      DATA FORCOL(28) /2/


      DATA (TITCOL(J,29),J=1,3)/ '     CONTACT    ',
     &                           '       BCL.     ',
     &                           '    MAX. LIEU   '/
      DATA FORCOL(29) /3/

      DATA (TITCOL(J,30),J=1,3)/ '     RESIDU     ',
     &                           '     RELATIF    ',
     &                           ' RESI_COMP_RELA '/
      DATA FORCOL(30) /2/

      DATA (TITCOL(J,31),J=1,3)/ 'RESIDU RELA COMP',
     &                           '    MAXIMUM     ',
     &                           '    AU POINT    '/
      DATA FORCOL(31) /3/

      DATA (TITCOL(J,32),J=1,3)/ '                ',
     &                           '                ',
     &                           '                '/
      DATA FORCOL(32) /2/
C
C ----------------------------------------------------------------------
C 
      CALL JEMARQ()  
C
      IF ((ICOD.LE.0).OR.(ICOD.GT.32)) THEN
        WRITE(6,*) 'ICOD:',ICOD
        CALL ASSERT(.FALSE.)
      ENDIF

      FORMA   = FORCOL(ICOD)

      IF ((ICOD.GE.23).AND.(ICOD.LE.26)) THEN
        ISUIV  = (ICOD-22)
        CALL SUIIMP(SDSUIV,ISUIV ,3   ,TITRE )
      ELSE
        TITRE(1) = TITCOL(1,ICOD)
        TITRE(2) = TITCOL(2,ICOD)
        TITRE(3) = TITCOL(3,ICOD)
      ENDIF

      CALL JEDEMA()
      END

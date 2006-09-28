      SUBROUTINE TITRED(NIV,NOMCON,NOMCHA,NBTITR)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     NIV,NOMCON,NOMCHA
      INTEGER                             NBTITR
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C     GENERATION D'UN TITRE OU D'UN SOUS-TITRE PAR DEFAUT
C     ------------------------------------------------------------------
C IN  NIV    : K1  : NIVEAU  'T': TITRE, 'S': SOUS-TITRE
C IN  NOMCON : K8  : NOM DU RESULTAT
C IN  NOMCHA : K19 : NOM DU CONCEPT (UTILISATEUR OU INTERNE)
C OUT NBTITR : I   : NOMBRE DE LIGNE DU TITRE GENERE
C     ------------------------------------------------------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CHARACTER*8    TYPESD
C     ------------------------------------------------------------------
      PARAMETER            (MXDEF=9 , MXLIG= 8 )
      CHARACTER*72   DEFAUT(MXLIG,MXDEF)
      INTEGER        LONDEF(MXLIG,MXDEF), NBLIG(MXDEF)
C     ------------------------------------------------------------------
C
C     ------------------------------------------------------------------
C     1/ --- TITRE PAR DEFAUT ---
C               1         2         3         4         5         6
C      12345678901234567890123456789012345678901234567890123456789012345
      DATA NBLIG(1),(DEFAUT(I,1),LONDEF(I,1),I=1,3)/
     &       3,
     &'ASTER &VERSION',14,
     &' CONCEPT &RESULTAT CALCULE LE &DATE A &HEURE',44,
     &' DE TYPE &TYPE',14/
C     ------------------------------------------------------------------
C
C     2/ --- SOUS-TITRES PAR DEFAUT D'UN CHAM_NO
C               1         2         3         4         5         6
C      12345678901234567890123456789012345678901234567890123456789012345
      DATA NBLIG(2),(DEFAUT(I,2),LONDEF(I,2),I=1,1)/
     &       1,
     &'CHAMP AUX NOEUDS',16/
C     ------------------------------------------------------------------
C
C     3/ --- SOUS-TITRES PAR DEFAUT D'UN CHAM_ELEM
C               1         2         3         4         5         6
C      12345678901234567890123456789012345678901234567890123456789012345
      DATA NBLIG(3),(DEFAUT(I,3),LONDEF(I,3),I=1,2)/
     &       2,
     &'CHAMP PAR ELEMENT',17,
     &' &LOC(12345678)',15/
C     ------------------------------------------------------------------
C
C     4/ --- SOUS-TITRES PAR DEFAUT D'UN CHAM_NO D'UN RESULTAT
C               1         2         3         4         5         6
C      12345678901234567890123456789012345678901234567890123456789012345
      DATA NBLIG(4),(DEFAUT(I,4),LONDEF(I,4),I=1,7)/
     &       7,
     &'CHAMP AUX NOEUDS DE',19,
     &' NOM SYMBOLIQUE ',16,
     &' &NOM_SYMB(''1234567890123456789'',''1234567890123456789'')',55,
     &' &RL',4,
     &'NUMERO D''ORDRE: ',16,
     &'&NUME_ORDRE(''1234567890123456789'',''1234567890123456789'')',56,
     &' &ACCES(''1234567890123456789'',''1234567890123456789'')',52/
C     ------------------------------------------------------------------
C
C     5/ --- SOUS-TITRES PAR DEFAUT D'UN CHAM_ELEM D'UN RESULTAT
C               1         2         3         4         5         6
C      12345678901234567890123456789012345678901234567890123456789012345
      DATA NBLIG(5),(DEFAUT(I,5),LONDEF(I,5),I=1,8)/
     &       8,
     &'CHAMP PAR ELEMENT',17,
     &' &LOC(''1234567890123456789'') DE',31,
     &' NOM SYMBOLIQUE ',16,
     &' &NOM_SYMB(''1234567890123456789'',''1234567890123456789'')',55,
     &' &RL',4,
     &'NUMERO D''ORDRE: ',16,
     &'&NUME_ORDRE(''1234567890123456789'',''1234567890123456789'')',56,
     &' &ACCES(''1234567890123456789'',''1234567890123456789'')',52/
C     ------------------------------------------------------------------
C
C     6/ SOUS-TITRES PAR DEFAUT D'UN VECTEUR GENERALISE D'UN RESULTAT
C               1         2         3         4         5         6
C      12345678901234567890123456789012345678901234567890123456789012345
      DATA NBLIG(6),(DEFAUT(I,6),LONDEF(I,6),I=1,7)/
     &       7,
     &'VECTEUR GENERALISE DE',21,
     &' NOM SYMBOLIQUE ',16,
     &' &NOM_SYMB(''1234567890123456789'',''1234567890123456789'')',55,
     &' &RL',4,
     &'NUMERO D''ORDRE: ',16,
     &'&NUME_ORDRE(''1234567890123456789'',''1234567890123456789'')',56,
     &' &ACCES(''1234567890123456789'',''1234567890123456789'')',52/
C     ------------------------------------------------------------------
C
C     7/ --- SOUS-TITRES PAR DEFAUT D'UN VECTEUR GENERALISE
C               1         2         3         4         5         6
C      12345678901234567890123456789012345678901234567890123456789012345
      DATA NBLIG(7),(DEFAUT(I,7),LONDEF(I,7),I=1,1)/
     &       1,
     &'VECTEUR GENERALISE',18/
C     ------------------------------------------------------------------
C
C     8/ --- SOUS-TITRES PAR DEFAUT D'UNE CARTE D'UN RESULTAT
C               1         2         3         4         5         6
C      12345678901234567890123456789012345678901234567890123456789012345
      DATA NBLIG(8),(DEFAUT(I,8),LONDEF(I,8),I=1,7)/
     &       7,
     &'CARTE DE',8,
     &' NOM SYMBOLIQUE ',16,
     &' &NOM_SYMB(''1234567890123456789'',''1234567890123456789'')',55,
     &' &RL',4,
     &'NUMERO D''ORDRE: ',16,
     &'&NUME_ORDRE(''1234567890123456789'',''1234567890123456789'')',56,
     &' &ACCES(''1234567890123456789'',''1234567890123456789'')',52/
C     ------------------------------------------------------------------
C
C     2/ --- SOUS-TITRES PAR DEFAUT D'UNE CARTE
C               1         2         3         4         5         6
C      12345678901234567890123456789012345678901234567890123456789012345
      DATA NBLIG(9),(DEFAUT(I,9),LONDEF(I,9),I=1,1)/
     &       1,
     &'CARTE',5/
C     ------------------------------------------------------------------
C
C
C     --- CHOIX D'UN TITRE ---
      CALL JEMARQ()
      IF ( NIV .EQ. 'T' ) THEN
         ICHOIX = 1
      ELSE
         CALL DISMOI('I','TYPE_RESU',NOMCON,'RESULTAT',IBID,TYPESD,IERD)
         IF (IERD .NE.0 ) THEN
            CALL U2MESK('F','UTILITAI4_99',1,TYPESD)
         ELSEIF ( TYPESD  .EQ. 'CHAMP' ) THEN
            CALL DISMOI('I','TYPE_CHAMP',NOMCON,'CHAMP',IBID,TYPESD,
     &                                                             IERD)
            IF ( TYPESD(1:4) .EQ. 'NOEU') THEN
               ICHOIX = 2
            ELSEIF ( TYPESD(1:2) .EQ. 'EL') THEN
               ICHOIX = 3
               DEFAUT(2,ICHOIX)(07:14) = NOMCON
            ELSEIF ( TYPESD(1:4) .EQ. 'VGEN') THEN
               ICHOIX = 7
            ELSEIF ( TYPESD(1:4) .EQ. 'CART') THEN
               ICHOIX = 9
            ELSE
               CALL U2MESK('F','UTILITAI4_99',1,TYPESD)
            ENDIF
         ELSE
            CALL DISMOI('I','TYPE_CHAMP',NOMCHA,'CHAMP',IBID,TYPESD,
     &                                                             IERD)
CCCC +                                                             IERD)
            IF ( TYPESD(1:4) .EQ. 'NOEU') THEN
               ICHOIX = 4
               DEFAUT(3,ICHOIX)(13:31) = NOMCON
               DEFAUT(3,ICHOIX)(35:53) = NOMCHA
               DEFAUT(6,ICHOIX)(14:32) = NOMCON
               DEFAUT(6,ICHOIX)(36:54) = NOMCHA
               DEFAUT(7,ICHOIX)(10:28) = NOMCON
               DEFAUT(7,ICHOIX)(32:50) = NOMCHA
            ELSEIF ( TYPESD(1:2) .EQ. 'EL') THEN
               ICHOIX = 5
               DEFAUT(2,ICHOIX)(08:26) = NOMCHA
               DEFAUT(4,ICHOIX)(13:31) = NOMCON
               DEFAUT(4,ICHOIX)(35:53) = NOMCHA
               DEFAUT(7,ICHOIX)(14:32) = NOMCON
               DEFAUT(7,ICHOIX)(36:54) = NOMCHA
               DEFAUT(8,ICHOIX)(10:28) = NOMCON
               DEFAUT(8,ICHOIX)(32:50) = NOMCHA
            ELSEIF ( TYPESD(1:4) .EQ. 'VGEN') THEN
               ICHOIX = 6
               DEFAUT(3,ICHOIX)(13:31) = NOMCON
               DEFAUT(3,ICHOIX)(35:53) = NOMCHA
               DEFAUT(6,ICHOIX)(14:32) = NOMCON
               DEFAUT(6,ICHOIX)(36:54) = NOMCHA
               DEFAUT(7,ICHOIX)(10:28) = NOMCON
               DEFAUT(7,ICHOIX)(32:50) = NOMCHA
            ELSEIF ( TYPESD(1:4) .EQ. 'CART') THEN
               ICHOIX = 8
               DEFAUT(3,ICHOIX)(13:31) = NOMCON
               DEFAUT(3,ICHOIX)(35:53) = NOMCHA
               DEFAUT(6,ICHOIX)(14:32) = NOMCON
               DEFAUT(6,ICHOIX)(36:54) = NOMCHA
               DEFAUT(7,ICHOIX)(10:28) = NOMCON
               DEFAUT(7,ICHOIX)(32:50) = NOMCHA
            ELSE
               CALL U2MESK('F','UTILITAI4_99',1,TYPESD)
            ENDIF
         ENDIF
      ENDIF
C
C     --- REMPLISSAGE DU TITRE CHOISI ---
      NBTITR = NBLIG(ICHOIX)
      CALL WKVECT('&&TITRE .TAMPON.ENTREE','V V K80',NBTITR,LDON)
      CALL WKVECT('&&TITRE .LONGUEUR     ','V V I  ',NBTITR,LLON)
C-DEL WRITE(6,*)  ' DEBUT DU DEFAUT NUMERO ',ICHOIX,'---------------- '
      DO 10 I = 1, NBTITR
         ZK80(LDON+I-1) = DEFAUT(I,ICHOIX)
         ZI(LLON+I-1)   = LONDEF(I,ICHOIX)
 10   CONTINUE
C_DEL WRITE(6,*)  ' FIN DU DEFAUT ----------------------------------- '
C
      CALL JEDEMA()
      END

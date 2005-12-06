      SUBROUTINE RSACPA(NOMSDZ, NUMVA, NOMVA, CTYPE, IVAL, RVAL, IER)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 26/09/2003   AUTEUR DURAND C.DURAND 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      INTEGER NUMVA, IVAL(*), IER
      REAL*8  RVAL(*)
      CHARACTER*1   CTYPE
      CHARACTER*16  NOMVA
      CHARACTER*(*) NOMSDZ
C ---------------------------------------------------------------------
C  DETERMINE LE NOM D'UNE VARIABLE D'ACCES ET SES VALEURS
C  CONNAISSANT SON NUMERO D'ACCES DANS LA COLLECTION
C ---------------------------------------------------------------------
C IN  NOMSDZ K*  NOM DE LA SD
C IN  NUMVA   I  NUMERO DE LA VARIABLE D'ACCES A RECHERCHER
C                OU 0 POUR LES NUMEROS D'ORDRE  (NUME_ORDRE)
C OUT NOMVA  K16 NOM DE LA VARIABLE D'ACCES
C OUT CTYPE  K1  TYPE DE LA VARIABLE D'ACCES (I OU R)
C OUT IVAL    I  LISTE DES VALEURS DU PARAMETRE (CAS ENTIER)
C OUT RVAL    R  LISTE DES VALEURS DU PARAMETRE (CAS REEL)
C OUT IER     I  1 SI CE N'EST PAS UNE VAR. ACCES DE TYPE I OU R
C                0 SI OK
C ---------------------------------------------------------------------
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
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
      CHARACTER*32 JEXNUM
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
      INTEGER      IRET, IORD, NBORD, I, IAD, NUMORD
      CHARACTER*8  K8BID, KTYPE
      CHARACTER*19 NOMSD
C ---------------------------------------------------------------------
      CALL JEMARQ()
      NOMSD  = NOMSDZ
      IER    = 0
      NOMVA  = ' '
      CTYPE  = ' '
C    TRAITEMENT DE LA DEMANDE PROPRE A NUME_ORDRE
      IF (NUMVA .EQ. 0) THEN
        NOMVA = 'NUME_ORDRE'
        CTYPE = 'I'
        CALL JEVEUO(NOMSD // '.ORDR', 'L', IORD)
        CALL JELIRA(NOMSD // '.ORDR', 'LONUTI', NBORD, K8BID)
        DO 5 I = 1, NBORD
          IVAL(I) = ZI(IORD-1 + I)
 5      CONTINUE
        GOTO 9999
      END IF
C    ACCES AU NOM DU CHAMP
      CALL JENUNO(JEXNUM(NOMSD//'.NOVA',NUMVA), NOMVA)
C    ACCES AUX VALEURS DE LA VARIABLE
      CALL JEVEUO(NOMSD // '.ORDR', 'L', IORD)
      CALL JELIRA(NOMSD // '.ORDR', 'LONUTI', NBORD, K8BID)
      DO 10 I = 1, NBORD
        NUMORD = ZI(IORD-1 + I)
        CALL RSADPA(NOMSD, 'L', 1, NOMVA, NUMORD, 1, IAD, KTYPE)
        CTYPE = KTYPE
        IF (CTYPE .EQ. 'I') THEN
          IVAL(I) = ZI(IAD)
        ELSE IF (CTYPE .EQ. 'R') THEN
          RVAL(I) = ZR(IAD)
        ELSE
          IER = 1
          GOTO 9999
        END IF
 10   CONTINUE
 9999 CONTINUE
      CALL JEDEMA()
      END

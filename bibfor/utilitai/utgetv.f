      SUBROUTINE UTGETV ( MOTFAC, MOTCLE, IOCC, NOMVEC, NBVAL, TYPE )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 19/09/2005   AUTEUR DURAND C.DURAND 
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
C     ------------------------------------------------------------------
C  BUT : RECUEILLIR LA LISTE DES VALEURS D'UN MOT CLE DANS UN
C        VECTEUR JEVEUX
C MOTFAC : IN         K16 : NOM DU MOT CLE FACTEUR
C                           CHAINE BLANCHE POUR LE MOT-CLE SIMPLE SEUL
C MOTCLE : IN         K16 : NOM DU MOT CLE SIMPLE A SCRUTER
C IOCC   : IN         I   : NUMERO D'OCCURENCE DU MOT-CLE FACTEUR
C NOMVEC : IN/JXVAR   K24 : NOM DE L'OBJET JEVEUX RECEPTACLE
C                  (IL EST DETRUIT SI IL EXISTE DEJA)
C
C NBVAL  : OUT        I  : NOMBRE DE VALEURS LUES
C TYPE   : OUT        K3 : TYPE DES VALEURS : I/R/C/L/K8/K16/...
C                          SI TYPE='?', LE MOT CLE N'A PAS ETE TROUVE
C     ------------------------------------------------------------------
      IMPLICIT NONE
C
C 0.1. ==> ARGUMENTS
C
      CHARACTER*(*) MOTFAC, NOMVEC, MOTCLE, TYPE
      INTEGER IOCC, NBVAL
C
C 0.2. ==> COMMUNS
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C     ----------- FIN COMMUNS NORMALISES  JEVEUX -----------------------
C
C 0.3. ==> VARIABLES LOCALES
C
      CHARACTER*6 NOMPRO
      PARAMETER ( NOMPRO = 'UTGETV' )
C
      INTEGER LXLGUT
C
      INTEGER NBARG, IAUX, IBID, JVEC, JLMCLE, JLTYP, I
C
      CHARACTER*3 TYPE2
      CHARACTER*24 NOMVE2
      CHARACTER*16 MOTFA2, MOCLE2, K16BID
C
      REAL*8 RBID
C
      COMPLEX*16 CBID
C     ------------------------------------------------------------------
C====
C 1. PREALABLES
C====
C
      CALL JEMARQ()
C
C               1234567890123456
      MOTFA2 = '                '
      IAUX = LXLGUT(MOTFAC)
      IF ( IAUX.GT.0 ) THEN
        MOTFA2(1:IAUX) = MOTFAC(1:IAUX)
      ENDIF
C               1234567890123456
      MOCLE2 = '                '
      IAUX = LXLGUT(MOTCLE)
      MOCLE2(1:IAUX) = MOTCLE(1:IAUX)
C               123456789012345678901234
      NOMVE2 = '                        '
      IAUX = LXLGUT(NOMVEC)
      NOMVE2(1:IAUX) = NOMVEC(1:IAUX)
C
      CALL JEDETR (NOMVE2)
C
C====
C 2. TYPE DES ARGUMENTS DU MOT-CLE
C====
C
      CALL GETMJM (MOTFA2, IOCC, 0, K16BID, K16BID, NBARG )
      NBARG = ABS(NBARG)
      CALL WKVECT ('&&'//NOMPRO//'.LMOCLE','V V K16',
     >             MAX(1,NBARG),JLMCLE)
      CALL WKVECT ('&&'//NOMPRO//'.LTYP','V V K16',
     >             MAX(1,NBARG),JLTYP)
      CALL GETMJM (MOTFA2,IOCC,NBARG,ZK16(JLMCLE),ZK16(JLTYP),IBID)
C
      TYPE2 = '???'
      DO 21, IAUX = 1 , NBARG
        IF ( ZK16(JLMCLE-1+IAUX).EQ.MOCLE2 ) THEN
          TYPE2 = ZK16(JLTYP-1+IAUX)(1:2)
          GOTO 22
        ENDIF
   21 CONTINUE
C
   22 CONTINUE
C
      CALL JEDETR ( '&&'//NOMPRO//'.LMOCLE')
      CALL JEDETR ( '&&'//NOMPRO//'.LTYP')
C
C====
C 3. EXPLORATIONS
C====
C
      IF (TYPE2.EQ.'???') THEN
C     ------------------------------
        NBVAL = 0
        TYPE = '?'

      ELSE IF (TYPE2.EQ.'IS') THEN
C     ------------------------------
        CALL GETVIS ( MOTFA2, MOCLE2, IOCC, 1, 0, IBID, NBVAL )
        CALL WKVECT ( NOMVE2, 'V V I', MAX(1,-NBVAL), JVEC)
        CALL GETVIS ( MOTFA2, MOCLE2, IOCC, 1, -NBVAL,
     >                ZI(JVEC), IAUX )
        TYPE = 'I'

      ELSE IF (TYPE2.EQ.'R8') THEN
C     ------------------------------
        CALL GETVR8 ( MOTFA2, MOCLE2, IOCC, 1, 0, RBID, NBVAL )
        CALL WKVECT ( NOMVE2, 'V V R', MAX(1,-NBVAL), JVEC)
        CALL GETVR8 ( MOTFA2, MOCLE2, IOCC, 1, -NBVAL,
     >                ZR(JVEC), IAUX )
        TYPE = 'R'

      ELSE IF (TYPE2.EQ.'C8') THEN
C     ------------------------------
        CALL GETVC8 ( MOTFA2, MOCLE2, IOCC, 1,  0, CBID, NBVAL )
        CALL WKVECT ( NOMVE2, 'V V C', MAX(1,-NBVAL), JVEC)
        CALL GETVC8 ( MOTFA2, MOCLE2, IOCC, 1, -NBVAL,
     >                ZC(JVEC), IAUX )
        TYPE = 'C'

      ELSE IF (TYPE2.EQ.'CO') THEN
C     ------------------------------
        CALL GETVID ( MOTFA2, MOCLE2, IOCC, 1,  0, K16BID, NBVAL )
        CALL WKVECT ( NOMVE2, 'V V K8', MAX(1,-NBVAL), JVEC)
        CALL GETVID ( MOTFA2, MOCLE2, IOCC, 1, -NBVAL,
     >                ZK8(JVEC), IAUX )
        TYPE = 'K8'

      ELSE IF (TYPE2.EQ.'TX') THEN
C     ------------------------------
        CALL GETVTX ( MOTFA2, MOCLE2, IOCC, 1,  0, K16BID, NBVAL )
        CALL WKVECT ( NOMVE2, 'V V K80', MAX(1,-NBVAL), JVEC)
        CALL GETVTX ( MOTFA2, MOCLE2, IOCC, 1, -NBVAL,
     >                ZK80(JVEC), IAUX)
        TYPE = 'K80'

      ELSE
C     ------------------------------
        CALL UTMESS ( 'A', NOMPRO, 'TYPE DE DONNEES INCONNU : '//TYPE2 )
        CALL UTMESS ( 'F', NOMPRO, 'ERREUR DE PROGRAMMATION' )
      ENDIF
C
C====
C 4. NOMBRE DE PARAMETRES
C====
C
      NBVAL = ABS(NBVAL)
C
      CALL JEDEMA()
C
      END

      SUBROUTINE FIPOLO(NTERM,EXPRES , IER )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           NTERM,EXPRES(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 21/02/96   AUTEUR VABHHTS J.PELLET 
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
C     PASSAGE DE LA NOTATION INFIXEE EN NOTATION POLONAISE POSTFIXEE
C     ------------------------------------------------------------------
C     REFERENCE :
C       CONCEPTS OF PROGRAMMING LANGUAGES, MARK ELSON, APPENDIX 1
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
      COMMON /FISY01/ IPARG,IPARD,IVIRG,IEGAL,IPTVI,ILOAD,IPLUS,IFONC
C     ------------------------------------------------------------------
      INTEGER        IVAL
      REAL*8         RVAL
      COMPLEX*16     CVAL
      CHARACTER*72   KVAL
C     ------------------------------------------------------------------
      PARAMETER    (MXINCR=100)
C
C
C     --- ON MET UNE PARENTHESE OUVRANTE "(" EN BAS DE LA PILE DES
C     --- OPERATEURS COMME SA PRIORITE EST MINIMALE, LES OPERATEURS
C     --- SUIVANTS SERONT EMPILES
      CALL JEMARQ()
      IER       = 0
      MXPILE    = MXINCR
      CALL WKVECT('&&FIPOLO.PILE','V V I',MXPILE,LPILE)
      JPILE     = 1
      ZI(LPILE) = IPARG
C
      MXAUXI    = MXINCR
      CALL WKVECT('&&FIPOLO.AUXI','V V I',MXAUXI,LAUXI)
      JTERM   = 0
C
C     LECTURE DE L'ITEM SUIVANT
      DO 100 ITERM = 1, NTERM
         CALL FIEXTR(EXPRES(ITERM),ICLASS,IVAL,RVAL,CVAL,
     +                                              KVAL,IPRIOR,IARITE)
         IF (ICLASS.GT.30) ICLASS = 0
         ICLASS = MOD(ICLASS,10)
C        ---------------------------------------------------------------
C        ICLASS      CLASSE DE L'ITEM TROUVE
C           -- TYPE -----    ---- INFORMATION --------------------------
C       0   OPERATION
C       1,  ENTIER    2 REEL    5 COMPLEXE       6 LOGIQUE
C       ----------------------------------------------------------------
        IF (ICLASS.GE.1 .AND. ICLASS.LE.6) THEN
C          --- L'ITEM EST UNE VALEUR ALORS L'AJOUTER DANS LA QUEUE
           JTERM = JTERM + 1
           IF ( JTERM .GT. MXAUXI ) THEN
              MXAUXI = MXAUXI + MXINCR
              CALL JUVECA('&&FIPOLO.AUXI',MXAUXI)
              CALL JEVEUO('&&FIPOLO.AUXI','E',LAUXI)
           ENDIF
C
           ZI(LAUXI-1+JTERM) = EXPRES(ITERM)
C
         ELSEIF (ICLASS.EQ.0 .AND. EXPRES(ITERM).EQ. IVIRG ) THEN
C          --- L'ITEM EST UNE VIRGULE ALORS DEVERSER LA PILE DANS LA
C          --- QUEUE JUSQU'A RENCONTRER UNE PARENTHESE OUVRANTE "(".
C          --- REMETTRE UNE PARENTHESE OUVRANTE DANS LA PILE
 111       CONTINUE
           IF ( JTERM+JPILE .GT. MXAUXI ) THEN
              MXAUXI = MXAUXI + MXINCR
              CALL JUVECA('&&FIPOLO.AUXI',MXAUXI)
              CALL JEVEUO('&&FIPOLO.AUXI','E',LAUXI)
              GOTO 111
           ENDIF
           CALL FIDPIL(JTERM,ZI(LAUXI),JPILE,ZI(LPILE),IPARG,IER)
           IF (IER.NE.0) GOTO 900
           JPILE = JPILE + 1
           ZI(LPILE+JPILE-1) = IPARG
C
         ELSEIF (ICLASS.EQ.0 .AND. EXPRES(ITERM).EQ. IPARD ) THEN
C          --- L'ITEM EST UNE PARENTHESE FERMANTE ")" ALORS DEVERSER
C          --- LA PILE DANS LA QUEUE JUSQU'A RENCONTRER UNE PARENTHESE
C          --- OUVRANTE "(".
 112       CONTINUE
           IF ( JTERM+JPILE .GT. MXAUXI ) THEN
              MXAUXI = MXAUXI + MXINCR
              CALL JUVECA('&&FIPOLO.AUXI',MXAUXI)
              CALL JEVEUO('&&FIPOLO.AUXI','E',LAUXI)
              GOTO 112
           ENDIF
           CALL FIDPIL(JTERM,ZI(LAUXI),JPILE,ZI(LPILE),IPARG,IER)
           IF (IER.NE.0) GOTO 900
C
         ELSEIF (ICLASS.EQ.0 .AND. EXPRES(ITERM).EQ. IPTVI ) THEN
C          --- L'ITEM EST UN POINT-VIRGULE ALORS DEVERSER TOUTE LA PILE
C          --- DANS LA QUEUE ET FIN.
 113       CONTINUE
           IF ( JTERM+JPILE .GT. MXAUXI ) THEN
              MXAUXI = MXAUXI + MXINCR
              CALL JUVECA('&&FIPOLO.AUXI',MXAUXI)
              CALL JEVEUO('&&FIPOLO.AUXI','E',LAUXI)
              GOTO 113
           ENDIF
           IF (JPILE.NE.0)
     +           CALL FIDPIL(JTERM,ZI(LAUXI),JPILE,ZI(LPILE),IPARG,IER)
           GOTO 900
C
         ELSE
C          --- L'ITEM EST UN OPERATEUR OU UNE PARENTHESE OUVRANTE "("
C          1)  D'ABORD ENLEVER L'OPERATEUR DE LA PILE ET L'AJOUTER DANS
C          LA QUEUE SI SA PRIORITE EST SUPERIEURE CELLE DE L'OPERATEUR
C          COURANT  SINON LE REMETTRE DANS LA PILE
           IF (IARITE.LT.0) THEN
  300         CONTINUE
              CALL FIEXTR(ZI(LPILE+JPILE-1),JCLASS,JVAL,RVAL,CVAL,KVAL,
     +                                                   JPRIOR,JARITE)
              IF (JPRIOR .GE. IPRIOR) THEN
                 JTERM = JTERM+1
                 ZI(LAUXI-1+JTERM) = ZI(LPILE+JPILE-1)
                 JPILE = JPILE - 1
                 GOTO 300
               ELSE
               ENDIF
           ENDIF
C
C          2) METTRE L'OPERATEUR COURANT DANS LA PILE
            IF ( JPILE+ABS(IARITE)-1 .GT. MXPILE)  THEN
              MXPILE = MXPILE + MXINCR
              CALL JUVECA('&&FIPOLO.PILE',MXPILE)
              CALL JEVEUO('&&FIPOLO.PILE','E',LPILE)
            ENDIF
            JPILE = JPILE + 1
            ZI(LPILE+JPILE-1) = EXPRES(ITERM)
         ENDIF
  100 CONTINUE
C
C
  900 CONTINUE
      NTERM = JTERM
      DO 901 ITERM=1, NTERM
         EXPRES(ITERM) = ZI(LAUXI-1+ITERM)
  901 CONTINUE
      CALL FIPOID(NTERM,EXPRES,IRET)
      IER = IER + ABS(IRET)
C
      CALL JEDETR('&&FIPOLO.PILE')
      CALL JEDETR('&&FIPOLO.AUXI')
      CALL JEDEMA()
      END

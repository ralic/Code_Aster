      SUBROUTINE FIOPER(CODMES,NOMFON,ICLASS,IVAL,RVAL,IERR)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER            ICLASS, IVAL
      REAL*8             RVAL(2)
      CHARACTER*(*)      CODMES, NOMFON
      INTEGER            IERR
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 08/03/2004   AUTEUR REZETTE C.REZETTE 
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
C
C     INTERPRETER UNE FONCTION INTERPRETABLE
C     ------------------------------------------------------------------
C IN  CODMES : 'F','E','A','I',... PARAMETRE TRANSMIT A UTMESS.
C IN  NOMFON : NOM DE LA FONCTION INTERPRETABLE
C OUT ICLASS : CLASSE DU RESULTAT
C OUT IERR   : CODE D'ERREUR
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
      PARAMETER        (MXMPIL=100,MXMFON=10)
      CHARACTER*8  KVAL
      CHARACTER*19 NEWFON
      INTEGER      ZIVAL
      REAL*8       ZRVAL
      COMPLEX*16   ZCVAL
      CHARACTER*1  CODME2
      CHARACTER*8 K8BID

      SAVE MXPILE,LPILE,LCLASS,LMI,LMR,LMC,LML
      SAVE MXFONC,LFONC,LTERM,LMXTE


      SAVE IPREM
      DATA  IPREM /0/

C     ------------------------------------------------------------------
      CALL JEMARQ()
      IER   = 0
      IPILE = 0
      CODME2 = CODMES
C
      IF (IPREM.EQ.0) THEN
         MXPILE = MXMPIL
         CALL WKVECT('&&FIOPER.PILE ','L V I   ',MXPILE,LPILE )
         CALL JEVEUS('&&FIOPER.PILE ','E',LPILE )
         CALL WKVECT('&&FIOPER.CLASS','L V I   ',MXPILE,LCLASS)
         CALL JEVEUS('&&FIOPER.CLASS','E',LCLASS)
         CALL WKVECT('&&FIOPER.MI   ','L V I   ',MXPILE,LMI)
         CALL JEVEUS('&&FIOPER.MI   ','E',LMI)
         CALL WKVECT('&&FIOPER.MR   ','L V R   ',MXPILE,LMR)
         CALL JEVEUS('&&FIOPER.MR   ','E',LMR)
         CALL WKVECT('&&FIOPER.MC   ','L V C   ',MXPILE,LMC)
         CALL JEVEUS('&&FIOPER.MC   ','E',LMC)
         CALL WKVECT('&&FIOPER.ML   ','L V L   ',MXPILE,LML)
         CALL JEVEUS('&&FIOPER.ML   ','E',LML)

         MXFONC = MXMFON
         CALL WKVECT('&&FIOPER.FONCT.ADREX','L V I',MXFONC,LFONC)
         CALL JEVEUS('&&FIOPER.FONCT.ADREX','E',LFONC)
         CALL WKVECT('&&FIOPER.FONCT.ITERM','L V I',MXFONC,LTERM)
         CALL JEVEUS('&&FIOPER.FONCT.ITERM','E',LTERM)
         CALL WKVECT('&&FIOPER.FONCT.MXTER','L V I',MXFONC,LMXTE)
         CALL JEVEUS('&&FIOPER.FONCT.MXTER','E',LMXTE)
         IPREM=1
      END IF

      NEWFON = NOMFON
      CALL JEVEUT(NEWFON//'.POLO','L',LEXPR)
      CALL JELIRA(NEWFON//'.POLO','LONUTI',NTERM,K8BID)
      IFONC  = 1
      ZI(LFONC+IFONC-1) = LEXPR
      ZI(LMXTE+IFONC-1) = NTERM
      ITERM = 0
C
  200 CONTINUE
  201    CONTINUE
         ITERM = ITERM + 1
         IF ( ITERM .GT. NTERM ) THEN
            IF (IFONC .EQ. 1) GOTO 300
            IFONC = IFONC - 1
            LEXPR = ZI(LFONC+IFONC-1)
            NTERM = ZI(LMXTE+IFONC-1)
            ITERM = ZI(LTERM+IFONC-1)
            GOTO 201
         ENDIF
C
         NEXP   = ZI(LEXPR+ITERM-1)
         CALL FIEXTR(NEXP,ICLASS,ZIVAL,ZRVAL,ZCVAL,KVAL,IPRIOR,IARITE)
         IF ( KVAL(1:5) .EQ. '&LOAD' ) THEN
            NEXP = ZI(LPILE+IPILE-1)
            CALL FIEXTR(NEXP,ICLASS,ZIVAL,ZRVAL,ZCVAL,
     +                                           NEWFON,IPRIOR,IARITE)
            CALL FILOAD(IPILE,ZI(LPILE),
     +                  ZI(LCLASS),ZI(LMI),ZR(LMR),ZL(LML),ZC(LMC),IER)
            IF (IER.NE.0) THEN
               ICLASS = 0
               GOTO 301
            ENDIF
            ZI(LTERM+IFONC-1) = ITERM
            IFONC = IFONC + 1
            IF (IFONC.GT.MXFONC) THEN
                MXFONC = MXFONC + MXMFON
               CALL JUVECA('&&FIOPER.FONCT.ADREX',MXFON)
               CALL JEVEUS('&&FIOPER.FONCT.ADREX','E',LFONC)
               CALL JUVECA('&&FIOPER.FONCT.ITERM',MXFON)
               CALL JEVEUS('&&FIOPER.FONCT.ITERM','E',LTERM)
               CALL JUVECA('&&FIOPER.FONCT.MXTER',MXFON)
               CALL JEVEUS('&&FIOPER.FONCT.MXTER','E',LMXTE)
            ENDIF
            CALL JEVEUT(NEWFON//'.POLO','L',LEXPR)
            CALL JELIRA(NEWFON//'.POLO','LONUTI',NTERM,K8BID)
            ZI(LFONC+IFONC-1) = LEXPR
            ZI(LMXTE+IFONC-1) = NTERM
            ITERM = 0
            GOTO 200
         ELSE IF ( KVAL(1:5) .EQ. '&FONC') THEN
            NEXP = ZI(LPILE+IPILE-1)
            CALL FIEXTR(NEXP,ICLASS,ZIVAL,ZRVAL,ZCVAL,
     +                                           NEWFON,IPRIOR,IARITE)
            CALL FILOAD(IPILE,ZI(LPILE),
     +                  ZI(LCLASS),ZI(LMI),ZR(LMR),ZL(LML),ZC(LMC),IER)
            IF (IER.NE.0) THEN
               ICLASS = 0
               GOTO 301
            ENDIF
            GOTO 200
         ENDIF
         ICLASS = MOD(ICLASS,10)
         GOTO (210,220,220,220,220,220,220), ICLASS+1
         IER = IER + 1
         CALL UTMESS('E','SUPERVISEUR.(ERREUR.FIOPER.01)',
     +                                             'CLASSE NON PREVUE')
         ICLASS = 0
         GOTO 301
C
  210    CONTINUE
C          --- OPERATEUR ARITHMETIQUE ---
           IARITE = ABS(IARITE)
           IF (IARITE.EQ.1) THEN
              CALL FIOPE1(NEXP,IPILE,
     +                 ZI(LCLASS),ZI(LMI),ZR(LMR),ZL(LML),ZC(LMC),IER)
           ELSE IF (IARITE.EQ.2) THEN
              CALL FIOPE2(NEXP,IPILE,
     +                 ZI(LCLASS),ZI(LMI),ZR(LMR),ZL(LML),ZC(LMC),IER)
           ELSE
              IER = IER+1
              CALL UTMESS('F','SUPERVISEUR.(ERREUR.FIOPER.02)',
     +                'ARITE INVALIDE POUR UN OPERATEUR ARITHMETIQUE.')
              ICLASS = 0
              GOTO 301
           ENDIF
           ICLASS  = ZI(LCLASS+IPILE-1)
           ZIVAL   = ZI(LMI+IPILE-1)
           ZRVAL   = ZR(LMR+IPILE-1)
           ZCVAL   = ZC(LMC+IPILE-1)
           IPILE   = IPILE - 1
CCCCCC   GOTO 200
C
  220    CONTINUE
C           --- CONSTANTE OU VARIABLE  ENTIERE OU REELLE ---
            IPILE = IPILE + 1
            IF (IPILE.GT.MXPILE) THEN
               MXPILE = MXPILE+MXMPIL
               CALL JUVECA('&&FIOPER.PILE ',MXPILE)
               CALL JEVEUS('&&FIOPER.PILE ','E',LPILE )
               CALL JUVECA('&&FIOPER.CLASS',MXPILE)
               CALL JEVEUS('&&FIOPER.CLASS','E',LCLASS)
               CALL JUVECA('&&FIOPER.MI   ',MXPILE)
               CALL JEVEUS('&&FIOPER.MI   ','E',LMI)
               CALL JUVECA('&&FIOPER.MR   ',MXPILE)
               CALL JEVEUS('&&FIOPER.MR   ','E',LMR)
               CALL JUVECA('&&FIOPER.MC   ',MXPILE)
               CALL JEVEUS('&&FIOPER.MC   ','E',LMC)
               CALL JUVECA('&&FIOPER.ML   ',MXPILE)
               CALL JEVEUS('&&FIOPER.ML   ','E',LML)
            ENDIF
            ZI(LPILE+IPILE-1) = NEXP
            ZI(LCLASS+IPILE-1) = MOD(ICLASS,10)
            IF (ZI(LCLASS+IPILE-1).EQ.1) THEN
C              --- ENTIER ---
               ZI(LMI+IPILE-1) = ZIVAL
               ZR(LMR+IPILE-1) = ZIVAL
               ZC(LMC+IPILE-1) = ZIVAL
            ELSEIF (ZI(LCLASS+IPILE-1).EQ.2) THEN
C              --- REEL ---
               ZI(LMI+IPILE-1) = 0
               ZR(LMR+IPILE-1) = ZRVAL
               ZC(LMC+IPILE-1) = ZRVAL
            ELSEIF (ZI(LCLASS+IPILE-1).EQ.5) THEN
C              --- COMPLEXE ---
               ZI(LMI+IPILE-1) = ZCVAL
               ZR(LMR+IPILE-1) = ZCVAL
               ZC(LMC+IPILE-1) = ZCVAL
            ELSEIF (ZI(LCLASS+IPILE-1).EQ.6) THEN
C              --- LOGIQUE ---
               ZI(LMI+IPILE-1) = ZIVAL
               ZR(LMR+IPILE-1) = ZIVAL
               ZC(LMC+IPILE-1) = ZIVAL
               ZL(LML+IPILE-1) = ZIVAL.EQ.1
            ELSE
               IER = IER + 1
               CALL UTMESS('E','SUPERVISEUR.(ERREUR.FIOPER.03)',
     +                                     'TYPE DE RANGEMENT INCONNU')
               ICLASS = 0
               GOTO 301
            ENDIF
      GOTO 200
 300  CONTINUE
      ICLASS = ZI(LCLASS+IPILE-1)
      IVAL   = ZI(LMI+IPILE-1)
      RVAL(1)= ZR(LMR+IPILE-1)
      IF ( MOD(ICLASS,10).EQ.5) THEN
         RVAL(1)= ZC(LMC+IPILE-1)
         RVAL(2)= DIMAG(ZC(LMC+IPILE-1))
      ELSE
         RVAL(2)= 0.D0
      ENDIF
C
 301  CONTINUE
      IF (IER .NE. 0) THEN
         IERR = IERR + 1
         CALL UTMESS(CODME2,'FIOPER',
     +                  'ERREUR RENCONTREE LORS DE L''INTERPOLATION '
     +                //'DE LA FONCTION : '//NEWFON//' - SI MOT CLE '
     +                //'RESERVE : LE METTRE EN MAJUSCULES')
      ENDIF
      CALL JEDEMA()
      END

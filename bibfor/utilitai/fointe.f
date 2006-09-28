      SUBROUTINE FOINTE ( CODMES, NOMF, NBPU, NOMPU, VALPU, RESU, IER )
      IMPLICIT   NONE
      INTEGER             NBPU, IER
      CHARACTER*(*)       CODMES, NOMF, NOMPU(*)
      REAL*8              VALPU(*), RESU
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
C     INTERPOLATION POUR CALCULER RESU = F(X,Y,Z,...)
C     ------------------------------------------------------------------
C IN  CODMES : 'F','E','A','I',... PARAMETRE TRANSMIT A UTMESS.
C IN  NOMF   : NOM DE LA FONCTION OU DE LA NAPPE
C IN  NBPU   : NOMBRE DE PARAMETRES DANS NOMPU ET VALPU
C IN  NOMPU  : NOMS DES PARAMETRES "UTILISATEUR"
C IN  VALPU  : VALEURS DES PARAMETRES "UTILISATEUR"
C OUT RESU   : RESULTAT DE L'INTERPOLATION
C OUT IER    : CODE RETOUR
C
C CODE RETOUR DE FOLOCX :
C IER = 10  : MOINS DE 1 POINT
C IER = 20  : EXTRAPOLATION INCONNUE
C IER = 30  : ON DEBORDE A GAUCHE
C IER = 40  : ON DEBORDE A DROITE
C
C CODE RETOUR DE FOCOLI :
C IER = 200 : INTERPOLATION DE LA FONCTION NON PERMISE
C IER = 210 : PARAMETRE EN DOUBLE
C IER = 220 : PARAMETRE ATTENDUS,PARAMETRES RECUS
C IER = 230 : TYPE D'INTERPOLATION DE LA FONCTION INCONNU
C IER = 240 : RECHERCHE DE LA VALEUR INCONNUE (COLI)
C
C CODE RETOUR DE FOINTE :
C IER = 100 : TYPE DE FONCTION NON VALIDE
C IER = 110 : PAS ASSEZ DE PARAMETRES
C IER = 120 : PARAMETRE EN DOUBLE
C IER = 130 : PARAMETRE ATTENDUS,PARAMETRES RECUS
C IER = 140 : TYPE D'INTERPOLATION SUR LES PARA DE LA NAPPE INCONNU
C IER = 150 : TYPE DE FONCTION NON TRAITE
C IER = 160 : PAS ASSEZ DE PARAMETRES
C IER = 170 : INTERPOLATION SUR LES PARAMETRES DE LA NAPPE NON PERMISE
C     ------------------------------------------------------------------
C --------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ----------- COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER      NUPAR, I, I1, NBVN, IZERO, ISAVE, LPROL, LVAR, NBPT,
     &             LPARA, NBPF, IBID, IRET, LFON, IADZI, IAZK24
      CHARACTER*1  COLI, CBID,K1BID, BL,XOUS
      CHARACTER*2  CODME2
      CHARACTER*8  NOMAIL
      CHARACTER*19 NOMFON
      CHARACTER*24 CHPROL, CHVALE, CHPARA
      REAL*8       LINLIN, LINLOG, LOGLOG, LOGLIN, X, X1, Y1, X2, Y2
      REAL*8       RVAR, RPAR, TAB(4), EPSI, R8PREM, R8VIDE
C     ------------------------------------------------------------------
      INTEGER      MXSAVE, MXPARA, SVNBPA, SVPAR, ISVNXT, ISVIND, NEXTSV
      INTEGER      IAPROL, IAVALE, IAPARA, LUVALE, LUPARA
      REAL*8       SVRESU
      CHARACTER*1  SVTYPF
      CHARACTER*2  SVPRGD
      CHARACTER*16 SVINTE
      CHARACTER*16 SVNOMP
      CHARACTER*19 SVNOMF
      COMMON /IFOSAV/ MXSAVE, MXPARA, SVNBPA(4) , SVPAR(10,4) ,
     &                ISVNXT , ISVIND(4), NEXTSV(4)
      COMMON /JFOSAV/ IAPROL(4),IAVALE(4),IAPARA(4),LUVALE(4),LUPARA(4)
      COMMON /RFOSAV/ SVRESU(4)
      COMMON /KFOSAV/ SVNOMP(10,4) , SVNOMF(4) ,
     &                SVTYPF(4) , SVPRGD(4) , SVINTE(4)

C     ------------------------------------------------------------------
      INTEGER      IPAR(10)
      CHARACTER*16 NOMP(10)
      CHARACTER*16 TYPFON
C     ------------------------------------------------------------------
C     FONCTION EN LIGNE
C
      LINLIN(X,X1,Y1,X2,Y2)= Y1+(X-X1)*(Y2-Y1)/(X2-X1)
      LINLOG(X,X1,Y1,X2,Y2)=EXP(LOG(Y1)+(X-X1)*(LOG(Y2)-LOG(Y1))
     &                                        /(X2-X1))
      LOGLOG(X,X1,Y1,X2,Y2)=EXP(LOG(Y1)+(LOG(X)-LOG(X1))*(LOG(Y2)
     &                                     -LOG(Y1))/(LOG(X2)-LOG(X1)))
      LOGLIN(X,X1,Y1,X2,Y2)=Y1+(LOG(X)-LOG(X1))*(Y2-Y1)
     &                                         /(LOG(X2)-LOG(X1))
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      CODME2 = CODMES
      EPSI   = SQRT ( R8PREM() )
      BL     = ' '
      IER    = 0
      IZERO  = 0
      NOMFON = NOMF
      RESU   = R8VIDE()

      CHPROL = NOMFON//'.PROL'
      CHVALE = NOMFON//'.VALE'
      CHPARA = NOMFON//'.PARA'

      CALL JEVEUT(CHPROL,'L',LPROL)
      IF (ZK16(LPROL).EQ.'INTERPRE') THEN
C     ------------------------ CAS DES FORMULES ------------------------
         CALL FIINTF(NOMF,NBPU,NOMPU,VALPU,RESU)
         GOTO 9999
      ENDIF
C
      DO 10 I = 1, MXSAVE
         IF ( NOMFON .EQ. SVNOMF(I) ) THEN
            ISAVE = I
            LPROL=IAPROL(ISAVE)
            LVAR =IAVALE(ISAVE)
            NBPT =LUVALE(ISAVE)
            LPARA=IAPARA(ISAVE)
            NBVN=LUPARA(ISAVE)
            GOTO 11
         ENDIF
   10 CONTINUE
C     --- MEMORISATION DES INFORMATIONS NOUVELLES ---
      ISVNXT = NEXTSV(ISVNXT)
      ISAVE  = ISVNXT

      CALL JEVEUT(CHVALE,'L',LVAR)
C     -- SI L'OBJET .VALE EST UN OBJET SIMPLE, ON STOCKE 'LONUTI'
      CALL JELIRA(CHVALE,'XOUS',IBID,XOUS)
      IF (XOUS.EQ.'S') THEN
        CALL JELIRA(CHVALE,'LONUTI',NBPT,K1BID)
      ELSE
        NBPT=0
      END IF
      CALL JEEXIN(CHPARA,IRET)
      IF (IRET.GT.0) THEN
        CALL JEVEUT(CHPARA,'L',LPARA)
        CALL JELIRA(CHPARA,'LONUTI',NBVN,K1BID)
      ELSE
        LPARA=0
        NBVN=0
      END IF

      IAPROL(ISAVE) = LPROL
      IAVALE(ISAVE) = LVAR
      LUVALE(ISAVE) = NBPT
      IAPARA(ISAVE) = LPARA
      LUPARA(ISAVE) = NBVN

      SVTYPF(ISAVE) = ZK16(LPROL)
      SVINTE(ISAVE) = ZK16(LPROL+1)
      SVPRGD(ISAVE) = ZK16(LPROL+4)
C
   11 CONTINUE
C
C     --- CAS PARTICULIER DES CONSTANTES ---
      IF (SVTYPF(ISAVE) .EQ.'C') THEN
         IF ( NOMFON .NE. SVNOMF(ISAVE) ) THEN
            LVAR=IAVALE(ISAVE)
            SVRESU(ISAVE)  = ZR(LVAR+1)
            SVNOMF(ISAVE)  = NOMFON
         ENDIF
         RESU = SVRESU(ISAVE)
         GOTO 9998
      ENDIF
C
C
C     --- VERIFICATION DE LA VALIDITE DES PARAMETRES ----
      IF ( NOMFON .EQ. SVNOMF(ISAVE) ) THEN
         IF (NBPU.EQ.SVNBPA(ISAVE)) THEN
            DO 15 I=1,SVNBPA(ISAVE)
               IF (NOMPU(I).NE.SVNOMP(I,ISAVE)) THEN
                 GOTO 19
               ELSE
                  SVPAR(I,ISAVE)=I
               END IF
   15       CONTINUE
C           --- SI SUCCES ALORS ON SAUTE LES VERIFICATIONS ----
            GOTO 30
         ENDIF
      ENDIF
C
C     --- SI ECHEC PRECEDENT ALORS ON VERIFIE ---
   19 CONTINUE
      CALL FONBPA(NOMFON,ZK16(LPROL),CBID,MXPARA,
     &                                   SVNBPA(ISAVE),SVNOMP(1,ISAVE))
      IF (NBPU.LT.SVNBPA(ISAVE)) THEN
         IER = 160
         CALL UTDEBM('A','FOINT2','ERREUR A L''INTERPOLATION')
         CALL UTIMPK('S',' FONCTION',1,NOMFON)
         CALL UTIMPI('L',' PAS ASSEZ DE PARAMETRES : ',1,NBPU)
         CALL UTIMPI('S',' AU LIEU DE',1,SVNBPA(ISAVE))
         CALL UTFINM()
         GOTO 9998
      ENDIF
      DO 20 I=1,SVNBPA(ISAVE)
         SVPAR(I,ISAVE)=0
         DO 21 NUPAR=1,NBPU
            IF (NOMPU(NUPAR).EQ.SVNOMP(I,ISAVE)) THEN
               IF (SVPAR(I,ISAVE).EQ.0) THEN
                  SVPAR(I,ISAVE)=NUPAR
               ELSE
                  IER = 120
                  CALL UTDEBM('A','FOINT2','ERREUR A L''INTERPOLATION')
                  CALL UTIMPK('S',' FONCTION',1,NOMFON)
                  CALL UTIMPK('L',' PARAMETRE',NBPU,NOMPU)
                  CALL UTIMPK('S',' EN DOUBLE',0,BL)
                  CALL UTFINM()
                  GOTO 9998
               ENDIF
            ENDIF
   21    CONTINUE
         IF (SVPAR(I,ISAVE).EQ.0) THEN
            IER = 130
            CALL UTDEBM('A','FOINT2','ERREUR A L''INTERPOLATION')
            CALL UTIMPK('S',' FONCTION',1,NOMFON)
            CALL UTIMPK('L',' PARAMETRES ATTENDUS',SVNBPA(ISAVE),
     &                                            SVNOMP(1,ISAVE))
            CALL UTIMPK('L',' PARAMETRES RECUS   ',NBPU,NOMPU)
            CALL UTFINM()
            GOTO 9998
         ENDIF
   20 CONTINUE
C
C     ------------------------ INTERPOLATION --------------------------
   30 CONTINUE
C
      IF ( SVTYPF(ISAVE) .EQ. 'F') THEN
C
C        --- FONCTION ---
         LVAR=IAVALE(ISAVE)
         NBPT=LUVALE(ISAVE)
         NBPT   = NBPT/2
         LFON   = LVAR + NBPT
         RVAR   = VALPU(SVPAR(1,ISAVE))
         CALL FOLOCX(ZR(LVAR),NBPT,RVAR,SVPRGD(ISAVE),
     &                                  ISVIND(ISAVE),EPSI,COLI,IER)
         IF (IER.NE.0) GOTO 9998
         CALL FOCOLI ( ISVIND(ISAVE),COLI,SVINTE(ISAVE),ZR(LVAR),
     &                                ZR(LFON),RVAR, RESU , IER )
         IF (IER.NE.0) GOTO 9998
         SVRESU(ISAVE) = RESU
C
C     --- NAPPE ---
C
      ELSEIF ( SVTYPF(ISAVE) .EQ. 'N') THEN
         RPAR   = VALPU(SVPAR(1,ISAVE))
         RVAR   = VALPU(SVPAR(2,ISAVE))
         LPARA=IAPARA(ISAVE)
         NBVN=LUPARA(ISAVE)
         I = 1
         CALL FOLOCX(ZR(LPARA),NBVN,RPAR,SVPRGD(ISAVE),I,EPSI,COLI,IER)
         IF (IER.NE.0) GOTO 9998
C
         IF (COLI.EQ.'C') THEN
            CALL FOINTN ( IZERO, NOMF, RVAR, I, EPSI, RESU, IER )
            IF (IER.NE.0) GOTO 9998
         ELSEIF (COLI.EQ.'I') THEN
            IF (SVINTE(ISAVE)(1:3).EQ.'NON') THEN
               CALL U2MESS('A','UTILITAI2_19')
               IER = 170
               GOTO 9998
            ENDIF
            CALL FOINTN ( IZERO, NOMF, RVAR, I, EPSI, TAB(3), IER )
            IF (IER.NE.0) GOTO 9998
            CALL FOINTN ( IZERO, NOMF, RVAR, I+1, EPSI, TAB(4), IER )
            IF (IER.NE.0) GOTO 9998
C
C           --- INTERPOLATION FINALE SUR LES PARAMETRES ---
            TAB(1) = ZR(LPARA+I-1)
            TAB(2) = ZR(LPARA+I  )
            IF (SVINTE(ISAVE).EQ.'LIN LIN ') THEN
               RESU = LINLIN(RPAR,TAB(1),TAB(3),TAB(2),TAB(4))
            ELSEIF (SVINTE(ISAVE).EQ.'LIN LOG ') THEN
               RESU = LINLOG(RPAR,TAB(1),TAB(3),TAB(2),TAB(4))
            ELSEIF (SVINTE(ISAVE).EQ.'LOG LOG ') THEN
               RESU = LOGLOG(RPAR,TAB(1),TAB(3),TAB(2),TAB(4))
            ELSEIF (SVINTE(ISAVE).EQ.'LOG LIN ') THEN
               RESU = LOGLIN(RPAR,TAB(1),TAB(3),TAB(2),TAB(4))
            ENDIF
         ELSEIF (COLI.EQ.'E') THEN
            CALL FOINTN ( IZERO, NOMF, RVAR, I, EPSI, TAB(3), IER )
            IF (IER.NE.0) GOTO 9998
            CALL FOINTN ( IZERO, NOMF, RVAR, I+1, EPSI, TAB(4), IER )
            IF (IER.NE.0) GOTO 9998
            TAB(1) = ZR(LPARA+I-1)
            TAB(2) = ZR(LPARA+I  )
            RESU = LINLIN(RPAR,TAB(1),TAB(3),TAB(2),TAB(4))
         ELSE
            CALL U2MESK('A','UTILITAI2_20',1,COLI)
            IER = 140
            GOTO 9998
         ENDIF
C
      ELSE
         CALL U2MESK('A','UTILITAI2_21',1,SVTYPF(ISAVE))
         IER = 150
         GOTO 9998
      ENDIF
C
 9998 CONTINUE
      SVNOMF(ISAVE) = NOMFON
 9999 CONTINUE
C
      IF ( IER .NE. 0 ) THEN
        IF ( CODME2(1:1) .NE. ' ' ) THEN
           CALL UTDEBM ( CODME2(1:1), 'FOINTE', 'ERREUR RENCONTREE ' )
           CALL UTIMPK ( 'S', 'POUR LA FONCTION ', 1, NOMFON )
           IF ( CODME2(2:2) .EQ. 'M' ) THEN
              CALL TECAEL ( IADZI, IAZK24 )
              NOMAIL = ZK24(IAZK24-1+3)(1:8)
              CALL UTIMPK ( 'L', '  POUR LA MAILLE ', 1, NOMAIL )
           ENDIF
           CALL UTFINM
        ENDIF
      ENDIF
C
      CALL JEDEMA()
      END

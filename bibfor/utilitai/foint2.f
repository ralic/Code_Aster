      SUBROUTINE FOINT2 ( NOMF, NBPU, NOMPU, VALPU, EPSI, RESU, IER )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       NOMF,       NOMPU(*)
      REAL*8                                 VALPU(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 17/12/2002   AUTEUR CIBHHGB G.BERTRAND 
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
C
C     CETTE ROUTINE EST DUPLIQUEE,AVEC QUELQUES LIGNES EN MOINS
C     DANS FITABU. VEILLER A GARDER LA CONCORDANCE EN CAS DE
C     MODIFICATION.
C     ------------------------------------------------------------------
C IN  NOMF  : NOM DE LA FONCTION OU DE LA NAPPE
C IN  NBPU  : NOMBRE DE PARAMETRES DANS NOMPU ET VALPU
C IN  NOMPU : NOMS DES PARAMETRES "UTILISATEUR"
C IN  VALPU : VALEURS DES PARAMETRES "UTILISATEUR"
C IN  EPSI  : TOLERENCE POUR RECUPERER UNE VALEUR
C OUT RESU  : R : RESULTAT DE L'INTERPOLATION
C OUT IER   : CODE RETOUR
C     ------------------------------------------------------------------
C     REMARQUE: PAS DE "SAVE" POUR LES FONCTIONS INTERPRETEES.
C     ------------------------------------------------------------------
C     ----------- COMMUNS NORMALISES  JEVEUX  --------------------------
      CHARACTER*32 JEXNUM
      INTEGER       ZI
      COMMON/IVARJE/ZI(1)
      REAL*8        ZR
      COMMON/RVARJE/ZR(1)
      COMPLEX*16    ZC
      COMMON/CVARJE/ZC(1)
      LOGICAL       ZL
      COMMON/LVARJE/ZL(1)
      CHARACTER*8   ZK8
      CHARACTER*16         ZK16
      CHARACTER*24                 ZK24
      CHARACTER*32                         ZK32
      CHARACTER*80                                 ZK80
      COMMON/KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     ------------------------------------------------------------------
      INTEGER      NUPAR, I, JPAR, LVALF2, NBVN
      CHARACTER*1  COLI, CBID,K1BID, BL,XOUS
      CHARACTER*8  INTER1, INTER2
      CHARACTER*19 NOMFON
      CHARACTER*24 CHPROL, CHVALE, CHPARA
      REAL*8       RVAR, RPAR, TAB(4), LINLIN, LINLOG, LOGLIN, LOGLOG
C     ------------------------------------------------------------------
      INTEGER      IPREM, ISVIND, ISVNXT, SVNBPA, SVPAR, NEXTSV
      INTEGER      IAPROL,IAVALE,IAPARA,LUVALE,LAPARA
      REAL*8       SVRESU
      CHARACTER*1  SVTYPF
      CHARACTER*2  SVPRGD
      CHARACTER*16 SVINTE
      CHARACTER*16 SVNOMP
      CHARACTER*19 SVNOMF
      COMMON /IFOSAV/ MXSAVE, MXPARA, SVNBPA(4) , SVPAR(10,4) ,
     +                ISVNXT , ISVIND(4), NEXTSV(4)
      COMMON /JFOSAV/ IAPROL(4),IAVALE(4),IAPARA(4),LUVALE(4),LUPARA(4)
      COMMON /RFOSAV/ SVRESU(4)
      COMMON /KFOSAV/ SVNOMP(10,4) , SVNOMF(4) ,
     +                SVTYPF(4) , SVPRGD(4) , SVINTE(4)

C     ------------------------------------------------------------------
      INTEGER      IPAR(10)
      CHARACTER*16 NOMP(10)
      SAVE         IPREM
      DATA         IPREM / 0 /
C     ------------------------------------------------------------------
C     FONCTION EN LIGNE
C
      LINLIN(X,X1,Y1,X2,Y2)= Y1+(X-X1)*(Y2-Y1)/(X2-X1)
      LINLOG(X,X1,Y1,X2,Y2)=EXP(LOG(Y1)+(X-X1)*(LOG(Y2)-LOG(Y1))
     +                                        /(X2-X1))
      LOGLOG(X,X1,Y1,X2,Y2)=EXP(LOG(Y1)+(LOG(X)-LOG(X1))*(LOG(Y2)
     +                                     -LOG(Y1))/(LOG(X2)-LOG(X1)))
      LOGLIN(X,X1,Y1,X2,Y2)=Y1+(LOG(X)-LOG(X1))*(Y2-Y1)
     +                                         /(LOG(X2)-LOG(X1))
C     ------------------------------------------------------------------
      CALL JEMARQ()
      BL = ' '
      IF ( IPREM .EQ. 0 ) THEN
         CALL FOINT0()
         IPREM = 1
      ENDIF
      IER    = 0
      IZERO  = 0
      NOMFON = NOMF
      RESU   = 0.D0

      CHPROL = NOMFON//'.PROL'
      CHVALE = NOMFON//'.VALE'
      CHPARA = NOMFON//'.PARA'

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
      CALL JEVEUT(CHPROL,'L',LPROL)
      IF (ZK16(LPROL).EQ.'INTERPRE') THEN
C
C        -- CALCUL DE LA FONCTION INTERPRETEE ---
         CALL FONBPA(NOMFON,ZK16(LPROL),CBID,MXPARA,NBPF,NOMP)
         DO 70 I1 = 1,NBPF
            IPAR(I1) = 0
            DO 72 NUPAR = 1,NBPU
               IF (NOMPU(NUPAR).EQ.NOMP(I1)) THEN
                  IF (IPAR(I1).EQ.0) THEN
                     IPAR(I1) = NUPAR
                  ELSE
                     IER = 120
                   CALL UTDEBM('A','FOINT2','ERREUR A L''INTERPOLATION')
                     CALL UTIMPK('S',' FONCTION',1,NOMFON)
                     CALL UTIMPK('L',' PARAMETRE',NBPU,NOMPU)
                     CALL UTIMPK('S',' EN DOUBLE',0,BL)
                     CALL UTFINM()
                     GOTO 9999
                  ENDIF
               ENDIF
 72         CONTINUE
            IF (IPAR(I1).EQ.0) THEN
               IER = 130
               CALL UTDEBM('A','FOINT2','ERREUR A L''INTERPOLATION')
               CALL UTIMPK('S',' FONCTION',1,NOMFON)
               CALL UTIMPK('L',' PARAMETRES ATTENDUS',NBPF,NOMP)
               CALL UTIMPK('L',' PARAMETRES RECUS   ',NBPU,NOMPU)
               CALL UTFINM()
               GOTO 9999
            ENDIF
 70      CONTINUE
         CALL FIINTE('F',NOMF,NBPF,IPAR,VALPU,RESU,IER)
         GOTO 9999
      ENDIF
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
     +                                   SVNBPA(ISAVE),SVNOMP(1,ISAVE))
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
     +                                            SVNOMP(1,ISAVE))
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
     +                                  ISVIND(ISAVE),EPSI,COLI,IER)
         IF (IER.NE.0) GOTO 9998
         CALL FOCOLI ( ISVIND(ISAVE),COLI,SVINTE(ISAVE),ZR(LVAR),
     +                                ZR(LFON),RVAR, RESU , IER ,
     +              NOMF,ZK16(LPROL),MXPARA,NOMP,IPAR,NOMPU,NBPU,VALPU )
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
            CALL FOINTN ( IZERO, RVAR, I, EPSI, RESU, IER,
     +              NOMF,ZK16(LPROL),MXPARA,NOMP,IPAR,NOMPU,NBPU,VALPU )
            IF (IER.NE.0) GOTO 9998
         ELSEIF (COLI.EQ.'I') THEN
            IF (SVINTE(ISAVE)(1:3).EQ.'NON') THEN
               CALL UTMESS('A','FOINT2','INTERPOLATION SUR PARAMETRES'//
     +                                                  ' NON PERMISE.')
               IER = 170
               GOTO 9998
            ENDIF
            CALL FOINTN ( IZERO, RVAR, I, EPSI, TAB(3), IER,
     +              NOMF,ZK16(LPROL),MXPARA,NOMP,IPAR,NOMPU,NBPU,VALPU )
            IF (IER.NE.0) GOTO 9998
            CALL FOINTN ( IZERO, RVAR, I+1, EPSI, TAB(4), IER,
     +              NOMF,ZK16(LPROL),MXPARA,NOMP,IPAR,NOMPU,NBPU,VALPU )
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
            CALL FOINTN ( IZERO, RVAR, I, EPSI, TAB(3), IER,
     +              NOMF,ZK16(LPROL),MXPARA,NOMP,IPAR,NOMPU,NBPU,VALPU )
            IF (IER.NE.0) GOTO 9998
            CALL FOINTN ( IZERO, RVAR, I+1, EPSI, TAB(4), IER,
     +              NOMF,ZK16(LPROL),MXPARA,NOMP,IPAR,NOMPU,NBPU,VALPU )
            IF (IER.NE.0) GOTO 9998
            TAB(1) = ZR(LPARA+I-1)
            TAB(2) = ZR(LPARA+I  )
            RESU = LINLIN(RPAR,TAB(1),TAB(3),TAB(2),TAB(4))
         ELSE
            CALL UTMESS('A',NOMFON,
     +                           'INTERPOLATION "'//COLI//'" INCONNUE.')
            IER = 140
            GOTO 9998
         ENDIF
C
      ELSE
         CALL UTMESS('A','FOINT2','"'//SVTYPF(ISAVE)//
     +                                   '" TYPE DE FONCTION INCONNU.')
         IER = 150
         GOTO 9998
      ENDIF
C
 9998 CONTINUE
      SVNOMF(ISAVE) = NOMFON
 9999 CONTINUE
C
      CALL JEDEMA()
      END

      SUBROUTINE FITABU(NOMF,NBPU,NOMPU,VALPU,RESU,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER                NBPU,                 IER
      CHARACTER*(*)     NOMF,     NOMPU(NBPU)
      REAL*8                            VALPU(NBPU),RESU
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 10/03/98   AUTEUR VABHHTS J.PELLET 
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
C     F (DE NOM NOMFON) EST UNE FONCTION OU UNE NAPPE
C
C     CETTE ROUTINE EST UNE DUPLICATION DE FOINTE POUR
C     EVITER LA RECURSIVITE DANS L'INTERPOLATION DE FONCTIONS
C     INTERPRETEES. IL Y A DONC QUELQUES LIGNES DE SUPPRIMEES
C     VEILLER A GARDER LA CONCORDANCE.
C     ------------------------------------------------------------------
C IN  NOMF  : NOM DE LA FONCTION OU DE LA NAPPE
C IN  NBPU  : NOMBRE DE PARAMETRES DANS NOMPU ET VALPU
C IN  NOMPU : NOMS DES PARAMETRES "UTILISATEUR"
C IN  VALPU : VALEURS DES PARAMETRES "UTILISATEUR"
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
      CHARACTER*1  COLI, CBID, K1BID
      CHARACTER*8  INTER1, INTER2
      CHARACTER*19 NOMFON
      CHARACTER*24 CHPROL, CHVALE, CHPARA
      REAL*8       RVAR, RPAR, TAB(4)
C     ------------------------------------------------------------------
C     DECLARATIONS POUR LES SAUVEGARDES.
      PARAMETER   (MXSAVE=4, MXPARA=10)
      INTEGER      IPAR(MXPARA)
      CHARACTER*16 NOMP(MXPARA)
      REAL*8       SVRESU(MXSAVE)
      INTEGER      ISVNXT,NEXTSV(MXSAVE),SVNBPA(MXSAVE)
      INTEGER      SVPAR(MXPARA,MXSAVE)
      INTEGER      ISVIND(MXSAVE)
      CHARACTER*1  SVTYPF(MXSAVE)
      CHARACTER*2  SVPRGD(MXSAVE)
      CHARACTER*8  SVINTE(MXSAVE)
      CHARACTER*16 SVNOMP(MXPARA,MXSAVE)
      CHARACTER*19 SVNOMF(MXSAVE)
      SAVE         SVRESU, SVNOMP, SVNOMF, SVTYPF, SVPRGD, SVNBPA
      SAVE         SVPAR , SVINTE
      SAVE         ISVNXT, ISVIND
      DATA         SVRESU/MXSAVE*0.D0/,SVNOMF/MXSAVE*'????????'/
      DATA         ISVNXT/MXSAVE/
      DATA         NEXTSV/2,3,4,1/
      DATA         ISVIND/MXSAVE*1/
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
C
      CALL JEMARQ()
      IER    = 0
      IZERO  = 0
      NOMFON = NOMF
      RESU   = 0.D0
      EPSI   = SQRT( R8PREM() )
      CHPROL = NOMFON//'.PROL'
      CHVALE = NOMFON//'.VALE'
C
C
      DO 10 I = 1, MXSAVE
         IF ( NOMFON .EQ. SVNOMF(I) ) THEN
            ISAVE = I
            GOTO 11
         ENDIF
   10 CONTINUE
C     --- MEMORISATION DES INFORMATIONS NOUVELLES ---
      CALL JEVEUO(CHPROL,'L',LPROL)
      ISVNXT = NEXTSV(ISVNXT)
      ISAVE  = ISVNXT
      SVTYPF(ISAVE) = ZK8(LPROL)
      SVINTE(ISAVE) = ZK8(LPROL+1)
      SVPRGD(ISAVE) = ZK8(LPROL+4)
C
   11 CONTINUE
C
C     --- CAS PARTICULIER DES CONSTANTES ---
      IF (SVTYPF(ISAVE) .EQ.'C') THEN
         IF ( NOMFON .NE. SVNOMF(ISAVE) ) THEN
            CHVALE = NOMFON//'.VALE'
            CALL JEVEUO(CHVALE,'L',LVAR)
            SVRESU(ISAVE)  = ZR(LVAR+1)
            SVNOMF(ISAVE)  = NOMFON
         ENDIF
         RESU = SVRESU(ISAVE)
         GOTO 9999
      ENDIF
C
C
C     --- VERIFICATION DE LA VALIDITE DES PARAMETRES ----
      IF ( NOMFON .EQ. SVNOMF(ISAVE) ) THEN
         IF (NBPU.EQ.SVNBPA(ISAVE)) THEN
            DO 15 I=1,SVNBPA(ISAVE)
               IF (NOMPU(I).NE.SVNOMP(I,ISAVE)) GOTO 19
   15       CONTINUE
C           --- SI SUCCES ALORS ON SAUTE LES VERIFICATIONS ----
            GOTO 30
         ENDIF
      ENDIF
C
C     --- SI ECHEC PRECEDENT ALORS ON VERIFIE ---
   19 CONTINUE
      CALL JEVEUO(CHPROL,'L',LPROL)
      CALL FONBPA(NOMFON,ZK8(LPROL),CBID,MXPARA,
     +                                   SVNBPA(ISAVE),SVNOMP(1,ISAVE))
      IF (NBPU.LT.SVNBPA(ISAVE)) THEN
         CALL UTDEBM('F','FITABU','ERREUR A L''INTERPOLATION')
         CALL UTIMPK('S',' FONCTION',1,NOMFON)
         CALL UTIMPI('L',' PAS ASSEZ DE PARAMETRES : ',1,NBPU)
         CALL UTIMPI('S',' AU LIEU DE',1,SVNBPA(ISAVE))
         CALL UTFINM()
         IER=IER+1
      ENDIF
      DO 20 I=1,SVNBPA(ISAVE)
         SVPAR(I,ISAVE)=0
         DO 21 NUPAR=1,NBPU
            IF (NOMPU(NUPAR).EQ.SVNOMP(I,ISAVE)) THEN
               IF (SVPAR(I,ISAVE).EQ.0) THEN
                  SVPAR(I,ISAVE)=NUPAR
               ELSE
                  CALL UTDEBM('F','FITABU','ERREUR A L''INTERPOLATION')
                  CALL UTIMPK('S',' FONCTION',1,NOMFON)
                  CALL UTIMPK('L',' PARAMETRE',NBPU,NOMPU)
                  CALL UTIMPK('S',' EN DOUBLE',0,' ')
                  CALL UTFINM()
                  IER=IER+1
                  GOTO 9999
                  ENDIF
              ENDIF
   21      CONTINUE
         IF (SVPAR(I,ISAVE).EQ.0) THEN
            CALL UTDEBM('F','FITABU','ERREUR A L''INTERPOLATION')
            CALL UTIMPK('S',' FONCTION',1,NOMFON)
            CALL UTIMPK('L',' PARAMETRES ATTENDUS',SVNBPA(ISAVE),
     +                                            SVNOMP(1,ISAVE))
            CALL UTIMPK('L',' PARAMETRES RECUS   ',NBPU,NOMPU)
            CALL UTFINM()
            IER=IER+1
            GOTO 9999
         ENDIF
   20 CONTINUE
C
C     ------------------------ INTERPOLATION --------------------------
   30 CONTINUE
C
      IF ( SVTYPF(ISAVE) .EQ. 'F') THEN
C
C        --- FONCTION ---
         CALL JEVEUO(CHVALE,'L',LVAR)
         CALL JELIRA(CHVALE,'LONUTI',NBPT,K1BID)
         NBPT   = NBPT/2
         LFON   = LVAR + NBPT
         RVAR   = VALPU(SVPAR(1,ISAVE))
         CALL FOLOCX(ZR(LVAR),NBPT,RVAR,SVPRGD(ISAVE),
     +                                  ISVIND(ISAVE),EPSI,COLI,IER)
         IF (IER.NE.0) GOTO 9999
         CALL FOCOLI ( ISVIND(ISAVE),COLI,SVINTE(ISAVE),ZR(LVAR),
     +                                ZR(LFON),RVAR, RESU , IER ,
     +              NOMF,ZK8(LPROL),MXPARA,NOMP,IPAR,NOMPU,NBPU,VALPU )
         IF (IER.NE.0) GOTO 9999
         SVRESU(ISAVE) = RESU
C
C     --- NAPPE ---
C
      ELSEIF ( SVTYPF(ISAVE) .EQ. 'N') THEN
         RPAR   = VALPU(SVPAR(1,ISAVE))
         RVAR   = VALPU(SVPAR(2,ISAVE))
         CHPARA = NOMFON//'.PARA'
         CALL JEVEUO(CHPARA,'L',LPARA)
         CALL JELIRA(CHPARA,'LONUTI',NBVN,K1BID)
         I = 1
         CALL FOLOCX(ZR(LPARA),NBVN,RPAR,SVPRGD(ISAVE),I,EPSI,COLI,IER)
         IF (IER.NE.0) GOTO 9999
C
         IF (COLI.EQ.'C') THEN
            CALL FOINTN ( IZERO, RVAR, I, RESU, EPSI, IER,
     +              NOMF,ZK8(LPROL),MXPARA,NOMP,IPAR,NOMPU,NBPU,VALPU )
            IF (IER.NE.0) GOTO 9999
C
         ELSEIF (COLI.EQ.'I') THEN
            IF (SVINTE(ISAVE)(1:3).EQ.'NON') THEN
               CALL UTMESS('A','FITABU','INTERPOLATION SUR PARAMETRES'//
     +                                                  ' NON PERMISE.')
               IER = IER + 1
               GOTO 9999
            ENDIF
            CALL FOINTN ( IZERO, RVAR, I, TAB(3), EPSI, IER,
     +              NOMF,ZK8(LPROL),MXPARA,NOMP,IPAR,NOMPU,NBPU,VALPU )
            IF (IER.NE.0) GOTO 9999
            CALL FOINTN ( IZERO, RVAR, I+1, TAB(4), EPSI, IER,
     +              NOMF,ZK8(LPROL),MXPARA,NOMP,IPAR,NOMPU,NBPU,VALPU )
            IF (IER.NE.0) GOTO 9999
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
C
         ELSEIF (COLI.EQ.'E') THEN
            CALL FOINTN ( IZERO, RVAR, I, TAB(3), EPSI, IER,
     +              NOMF,ZK8(LPROL),MXPARA,NOMP,IPAR,NOMPU,NBPU,VALPU )
            IF (IER.NE.0) GOTO 9999
            CALL FOINTN ( IZERO, RVAR, I+1, TAB(4), EPSI, IER,
     +              NOMF,ZK8(LPROL),MXPARA,NOMP,IPAR,NOMPU,NBPU,VALPU )
            IF (IER.NE.0) GOTO 9999
            TAB(1) = ZR(LPARA+I-1)
            TAB(2) = ZR(LPARA+I  )
            RESU = LINLIN(RPAR,TAB(1),TAB(3),TAB(2),TAB(4))
         ELSE
            CALL UTMESS('A',NOMFON,
     +                           'INTERPOLATION "'//COLI//'" INCONNUE.')
            IER = IER + 1
            GOTO 9999
         ENDIF
C
      ELSE
         CALL UTMESS('A','FITABU','"'//SVTYPF(ISAVE)//
     +                                   '" TYPE DE FONCTION INCONNU.')
      ENDIF
C
 9999 CONTINUE
      SVNOMF(ISAVE) = NOMFON
      CALL JEDEMA()
      END

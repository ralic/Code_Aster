      SUBROUTINE FOINT1 ( IPIF, NBPU, NOMPU, VALPU, EPSI, RESU, IER )
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8                                 VALPU(*)
      CHARACTER*(*)                   NOMPU(*)
C ----------------------------------------------------------------------
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
C ----------------------------------------------------------------------
C IN  IPIF  : POINTEUR DANS LE MATERIAU CODE (FONCTION OU NAPPE)
C IN  NBPU  : NOMBRE DE PARAMETRES DANS NOMPU ET VALPU
C IN  NOMPU : NOMS DES PARAMETRES "UTILISATEUR"
C IN  VALPU : VALEURS DES PARAMETRES "UTILISATEUR"
C IN  EPSI  : TOLERANCE POUR RECUPERER UNE VALEUR
C OUT RESU  : RESULTAT DE L'INTERPOLATION
C OUT IER   : CODE RETOUR
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
      PARAMETER   ( MXPARA = 10 )
      INTEGER      IPAR(MXPARA), NPAR(2)
      REAL*8       TAB(4), LINLIN, LINLOG, LOGLOG, LOGLIN
      CHARACTER*1  COLI
      CHARACTER*16 NOMPF(2), NOMP(MXPARA)
      CHARACTER*19 NOMF
C ----------------------------------------------------------------------
C PARAMETER ASSOCIE AU MATERIAU CODE
C
      PARAMETER  ( INDFCT = 7 )
C ----------------------------------------------------------------------
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
      IER = 0
      NPAR(1) = 0
      NPAR(2) = 0
      NOMF = ' '
      JPRO = ZI(IPIF+1)
      JPAR = ZI(IPIF+2)
C
      IF (ZK16(JPRO).EQ.'CONSTANT') THEN
C
C ----- CAS PARTICULIER DES CONSTANTES
C
        RESU = ZR(JPAR+1)
        GOTO 9999
      ENDIF
C
C --- VERIFICATION DE LA VALIDITE DES PARAMETRES
C
      IF (ZK16(JPRO).EQ.'FONCTION') THEN
        NBPARA = 1
        NOMPF(1) = ZK16(JPRO+2)
      ELSE IF (ZK16(JPRO).EQ.'NAPPE') THEN
        NBPARA = 2
        NOMPF(1) = ZK16(JPRO+2)
        NOMPF(2) = ZK16(JPRO+5)
      ELSE
        IER = 100
        CALL UTDEBM('F','FOINT1','ERREUR DE PROGRAMMATION')
        CALL UTIMPK('L','TYPE DE FONCTION NON VALIDE',1,ZK16(JPRO))
        CALL UTFINM()
        GOTO 9999
      ENDIF
      IF (NBPU.LT.NBPARA) THEN
        IER = 110
        CALL UTDEBM('A','FOINT1','ERREUR A L''INTERPOLATION')
        CALL UTIMPI('L',' PAS ASSEZ DE PARAMETRES : ',1,NBPU)
        CALL UTIMPI('S',' AU LIEU DE',1,NBPARA)
        CALL UTFINM()
        GOTO 9999
      ENDIF
      DO 20 I=1,NBPARA
        DO 21 NUPAR=1,NBPU
          IF (NOMPU(NUPAR).EQ.NOMPF(I)) THEN
            IF (NPAR(I).EQ.0) THEN
              NPAR(I)=NUPAR
            ELSE
              IER = 120
              CALL UTDEBM('A','FOINT1','ERREUR A L''INTERPOLATION')
              CALL UTIMPK('L',' PARAMETRE',NBPU,NOMPU)
              CALL UTIMPK('S',' EN DOUBLE',0,' ')
              CALL UTFINM()
              GOTO 9999
            ENDIF
          ENDIF
   21   CONTINUE
        IF (NPAR(I).EQ.0) THEN
          IER = 130
          CALL UTDEBM('A','FOINT1','ERREUR A L''INTERPOLATION')
          CALL UTIMPK('L',' PARAMETRES ATTENDUS',NBPA,NOMPF)
          CALL UTIMPK('L',' PARAMETRES RECUS   ',NBPU,NOMPU)
          CALL UTFINM()
          GOTO 9999
        ENDIF
   20 CONTINUE
C
C =====================================================================
C                          F O N C T I O N
C =====================================================================
C
      IF ( ZK16(JPRO) .EQ. 'FONCTION') THEN
        NBPT = ZI(IPIF)
        JVAL = JPAR + NBPT
        RVAR = VALPU(NPAR(1))
        CALL FOLOCX ( ZR(JPAR), NBPT, RVAR, ZK16(JPRO+4),
     +                          ZI(IPIF+INDFCT), EPSI, COLI, IER )
        IF ( IER .NE. 0 ) GOTO 9999
        CALL FOCOLI ( ZI(IPIF+INDFCT),COLI,ZK16(JPRO+1),ZR(JPAR),
     +                                ZR(JVAL), RVAR, RESU , IER ,
     +              NOMF,ZK16(JPRO),MXPARA,NOMP,IPAR,NOMPU,NBPU,VALPU )
        IF ( IER .NE. 0 ) GOTO 9999
C
C =====================================================================
C                            N A P P E
C =====================================================================
C
      ELSE IF ( ZK16(JPRO) .EQ. 'NAPPE   ') THEN
        RPAR   = VALPU(NPAR(1))
        RVAR   = VALPU(NPAR(2))
        LPARA = ZI(IPIF+4)
        NBVN  = ZI(IPIF+5)
        CALL FOLOCX ( ZR(LPARA), NBVN, RPAR, ZK16(JPRO+4),
     +                          ZI(IPIF+INDFCT), EPSI, COLI, IER )
        IF ( IER .NE. 0 ) GOTO 9999
        INUME = ZI(IPIF+INDFCT)
C
        IF (COLI.EQ.'C') THEN
          CALL FOINTN ( IPIF, RVAR, INUME, EPSI, RESU, IER ,
     +              NOMF,ZK16(JPRO),MXPARA,NOMP,IPAR,NOMPU,NBPU,VALPU )
          IF ( IER .NE. 0 ) GOTO 9999
C
        ELSE IF (COLI.EQ.'I') THEN
          IF (ZK16(JPRO+1)(1:3).EQ.'NON') THEN
            CALL UTMESS('A','FOINTA_08','INTERPOLATION SUR PARAMETRES'
     +                  //' NON PERMISE')
            IER = 170
            GOTO 9999
          ENDIF
          CALL FOINTN ( IPIF, RVAR, INUME, EPSI, TAB(3), IER ,
     +              NOMF,ZK16(JPRO),MXPARA,NOMP,IPAR,NOMPU,NBPU,VALPU )
          IF ( IER .NE. 0 ) GOTO 9999
          CALL FOINTN ( IPIF, RVAR, INUME+1, EPSI, TAB(4), IER ,
     +              NOMF,ZK16(JPRO),MXPARA,NOMP,IPAR,NOMPU,NBPU,VALPU )
          IF ( IER .NE. 0 ) GOTO 9999
C
C ------- INTERPOLATION FINALE SUR LES PARAMETRES
C
          TAB(1) = ZR(LPARA+INUME-1)
          TAB(2) = ZR(LPARA+INUME  )
          IF (ZK16(JPRO+1).EQ.'LIN LIN ') THEN
            RESU = LINLIN(RPAR,TAB(1),TAB(3),TAB(2),TAB(4))
          ELSE IF (ZK16(JPRO+1).EQ.'LIN LOG ') THEN
            RESU = LINLOG(RPAR,TAB(1),TAB(3),TAB(2),TAB(4))
          ELSE IF (ZK16(JPRO+1).EQ.'LOG LOG ') THEN
            RESU = LOGLOG(RPAR,TAB(1),TAB(3),TAB(2),TAB(4))
          ELSE IF (ZK16(JPRO+1).EQ.'LOG LIN ') THEN
            RESU = LOGLIN(RPAR,TAB(1),TAB(3),TAB(2),TAB(4))
          ENDIF
C
        ELSE IF (COLI.EQ.'E') THEN
          CALL FOINTN ( IPIF, RVAR, INUME, EPSI, TAB(3), IER ,
     +              NOMF,ZK16(JPRO),MXPARA,NOMP,IPAR,NOMPU,NBPU,VALPU )
          IF ( IER .NE. 0 ) GOTO 9999
          CALL FOINTN ( IPIF, RVAR, INUME+1, EPSI, TAB(4), IER ,
     +              NOMF,ZK16(JPRO),MXPARA,NOMP,IPAR,NOMPU,NBPU,VALPU )
          IF ( IER .NE. 0 ) GOTO 9999
          TAB(1) = ZR(LPARA+INUME-1)
          TAB(2) = ZR(LPARA+INUME  )
          RESU = LINLIN(RPAR,TAB(1),TAB(3),TAB(2),TAB(4))
C
        ELSE
          IER = 140
          CALL UTMESS('A','FOINT1',
     +                         'INTERPOLATION "'//COLI//'" INCONNUE')
          GOTO 9999
        ENDIF
C
      ELSE
        IER = 150
        CALL UTMESS('A','FOINT1','"'//ZK16(JPRO)//
     +                              '" TYPE DE FONCTION INCONNU')
        GOTO 9999
      ENDIF
C
 9999 CONTINUE
      END

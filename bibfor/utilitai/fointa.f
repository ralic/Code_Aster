      SUBROUTINE FOINTA ( IPIF, NBPU, NOMPU, VALPU, RESU )
      IMPLICIT NONE
      INTEGER             IPIF, NBPU
      REAL*8              VALPU(*), RESU
      CHARACTER*(*)       NOMPU(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 07/05/2008   AUTEUR COURTOIS M.COURTOIS 
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
C OUT RESU  : R : RESULTAT DE L'INTERPOLATION
C ----------------------------------------------------------------------
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
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
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
      INTEGER      NPAR(2), INDFCT, JPRO, JPAR, LPARA, NBVN, NBPARA, I
      INTEGER      NUPAR, NBPT, JVAL, INUME, IER
      REAL*8       TAB(4), RPAR, RVAR, EPSI, R8PREM, R8VIDE, VALR(4)
      REAL*8       LINLIN, LINLOG, LOGLOG, LOGLIN, X, X1, Y1, X2, Y2
      CHARACTER*1  COLI
      CHARACTER*19 NOMF
      CHARACTER*24 NOMPF(2)
C     ------------------------------------------------------------------
      INTEGER      IADZI, IAZK24
      CHARACTER*24 VALK(4)
C ----------------------------------------------------------------------
C PARAMETER ASSOCIE AU MATERIAU CODE
C
      PARAMETER  ( INDFCT = 7 )
C ----------------------------------------------------------------------
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
      NPAR(1) = 0
      NPAR(2) = 0
      NOMF = ' '
      JPRO = ZI(IPIF+1)
      JPAR = ZI(IPIF+2)
      RESU = R8VIDE()
      IER=0
C
      EPSI = SQRT ( R8PREM() )
C
C --- FONCTION "CONSTANT"
C
      IF (ZK24(JPRO).EQ.'CONSTANT') THEN
C         ------------------------
        RESU = ZR(JPAR+1)
        GOTO 9999

C
C --- FONCTION "INTERPRE"
C
      ELSEIF (ZK24(JPRO).EQ.'INTERPRE') THEN
C             ------------------------
         NOMF = ZK24(JPRO+5)
         CALL FIINTF(NOMF,NBPU,NOMPU,VALPU,RESU)
         GOTO 9999
C
C --- AUTRES TYPES DE FONCTION
C
      ELSEIF (ZK24(JPRO).EQ.'FONCTION') THEN
        NBPARA = 1
        NOMPF(1) = ZK24(JPRO+2)
        NOMF = ZK24(JPRO+5)

      ELSE IF (ZK24(JPRO).EQ.'NAPPE') THEN
        NBPARA = 2
        NOMPF(1) = ZK24(JPRO+2)
        NOMPF(2) = ZK24(JPRO+6)
        NOMF = ZK24(JPRO+5)

      ELSE
        CALL U2MESK('F','CALCULEL6_61',1,ZK24(JPRO))
      ENDIF
C
      DO 20 I=1,NBPARA
        DO 21 NUPAR=1,NBPU
          IF (NOMPU(NUPAR).EQ.NOMPF(I)) THEN
C           -- SI UN PARAMETRE EST FOURNI PLUSIEURS FOIS
C              ON PREND LE DERNIER (VOIR RCVALB)
            NPAR(I)=NUPAR
          ENDIF
   21   CONTINUE
        IF (NPAR(I).EQ.0) THEN
          VALK(1)=NOMF
          VALK(2)=NOMPF(I)
          CALL TECAEL(IADZI,IAZK24)
          VALK(3) = ZK24(IAZK24-1+3)
          CALL U2MESK('F','CALCULEL6_62',3,VALK)
        ENDIF
   20 CONTINUE
C
C =====================================================================
C                          F O N C T I O N
C =====================================================================
C
      IF ( ZK24(JPRO) .EQ. 'FONCTION') THEN
        NBPT = ZI(IPIF)
        JVAL = JPAR + NBPT
        RVAR = VALPU(NPAR(1))
        CALL FOLOCX ( ZR(JPAR), NBPT, RVAR, ZK24(JPRO+4),
     &                          ZI(IPIF+INDFCT), EPSI, COLI, IER )
        IF ( IER .NE. 0 ) GOTO 9999
        CALL FOCOLI ( ZI(IPIF+INDFCT),COLI,ZK24(JPRO+1),ZR(JPAR),
     &                                ZR(JVAL), RVAR, RESU , IER  )
        IF ( IER .NE. 0 ) GOTO 9999
C
C =====================================================================
C                            N A P P E
C =====================================================================
C
      ELSE IF ( ZK24(JPRO) .EQ. 'NAPPE   ') THEN
        RPAR   = VALPU(NPAR(1))
        RVAR   = VALPU(NPAR(2))
        LPARA = ZI(IPIF+4)
        NBVN  = ZI(IPIF+5)
        CALL FOLOCX ( ZR(LPARA), NBVN, RPAR, ZK24(JPRO+4),
     &                          ZI(IPIF+INDFCT), EPSI, COLI, IER )
        IF ( IER .NE. 0 ) GOTO 9999
        INUME = ZI(IPIF+INDFCT)
C
        IF (COLI.EQ.'C') THEN
          CALL FOINTN ( IPIF, NOMF, RVAR, INUME, EPSI, RESU, IER )
          IF ( IER .NE. 0 ) GOTO 9999
C
        ELSE IF (COLI.EQ.'I') THEN
          IF (ZK24(JPRO+1)(1:3).EQ.'NON') THEN
            CALL U2MESS('F','UTILITAI2_12')
          ENDIF
          CALL FOINTN ( IPIF, NOMF, RVAR, INUME, EPSI, TAB(3), IER )
          IF ( IER .NE. 0 ) GOTO 9999
          CALL FOINTN ( IPIF, NOMF, RVAR, INUME+1, EPSI, TAB(4), IER )
          IF ( IER .NE. 0 ) GOTO 9999
C
C ------- INTERPOLATION FINALE SUR LES PARAMETRES
C
          TAB(1) = ZR(LPARA+INUME-1)
          TAB(2) = ZR(LPARA+INUME  )
          IF (ZK24(JPRO+1).EQ.'LIN LIN ') THEN
            RESU = LINLIN(RPAR,TAB(1),TAB(3),TAB(2),TAB(4))
          ELSE IF (ZK24(JPRO+1).EQ.'LIN LOG ') THEN
            RESU = LINLOG(RPAR,TAB(1),TAB(3),TAB(2),TAB(4))
          ELSE IF (ZK24(JPRO+1).EQ.'LOG LOG ') THEN
            RESU = LOGLOG(RPAR,TAB(1),TAB(3),TAB(2),TAB(4))
          ELSE IF (ZK24(JPRO+1).EQ.'LOG LIN ') THEN
            RESU = LOGLIN(RPAR,TAB(1),TAB(3),TAB(2),TAB(4))
          ENDIF
C
        ELSE IF (COLI.EQ.'E') THEN
          CALL FOINTN ( IPIF, NOMF, RVAR, INUME, EPSI, TAB(3), IER )
          IF ( IER .NE. 0 ) GOTO 9999
          CALL FOINTN ( IPIF, NOMF, RVAR, INUME+1, EPSI, TAB(4), IER )
          IF ( IER .NE. 0 ) GOTO 9999
          TAB(1) = ZR(LPARA+INUME-1)
          TAB(2) = ZR(LPARA+INUME  )
          RESU = LINLIN(RPAR,TAB(1),TAB(3),TAB(2),TAB(4))
C
        ELSE
          CALL U2MESK('F','UTILITAI2_13',1,COLI)
        ENDIF
C
      ELSE
        CALL U2MESK('F','UTILITAI2_14',1,ZK24(JPRO))
      ENDIF
C
 9999 CONTINUE
      IF ( IER .NE. 0 ) THEN
         CALL U2MESG('F','CALCULEL6_63',1,NOMF,1,IER,0,VALR)
      ENDIF
C
C
      END

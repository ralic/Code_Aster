      SUBROUTINE FOCOLI ( IPT, COLI, INTERP, X, Y, RVAR, RESU, IER ,
     +                    NOMF,PROL,MXPARA,NOMP,IPAR,NOMPU,NBPU,VALPU )
      IMPLICIT REAL*8 (A-H,O-Z)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 08/09/1999   AUTEUR CIBHHLV L.VIVAN 
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
C ----------------------------------------------------------------------
      INTEGER       IPAR(*)
      REAL*8        X(*), Y(*), VALPU(*)
      CHARACTER*1   COLI, CBID
      CHARACTER*8   INTERP
      CHARACTER*19  NOMFON
      CHARACTER*(*) NOMF, NOMP(*), NOMPU(*), PROL(*)
      REAL*8        LINLIN, LINLOG, LOGLOG, LOGLIN
      CHARACTER*24  NOMFIT, CHFITA
      INTEGER       LFITA
C ----------------------------------------------------------------------
      LINLIN(X0,X1,Y1,X2,Y2)= Y1+(X0-X1)*(Y2-Y1)/(X2-X1)
      LINLOG(X0,X1,Y1,X2,Y2)=EXP(LOG(Y1)+(X0-X1)*(LOG(Y2)-LOG(Y1))
     +                                        /(X2-X1))
      LOGLOG(X0,X1,Y1,X2,Y2)=EXP(LOG(Y1)+(LOG(X0)-LOG(X1))*(LOG(Y2)
     +                                     -LOG(Y1))/(LOG(X2)-LOG(X1)))
      LOGLIN(X0,X1,Y1,X2,Y2)=Y1+(LOG(X0)-LOG(X1))*(Y2-Y1)
     +                                         /(LOG(X2)-LOG(X1))
C ----------------------------------------------------------------------
      CALL JEMARQ()
      NOMFON = NOMF
C
C     --- PAS D'INTERPOLATION ---
C
      IF ( COLI .EQ. 'C' ) THEN
          RESU = Y(IPT)
C
C     --- INTERPOLATION ---
C
      ELSE IF (COLI.EQ.'I') THEN
         IF (INTERP.EQ.'LIN LIN ') THEN
            RESU = LINLIN(RVAR,X(IPT),Y(IPT),X(IPT+1),Y(IPT+1))
C
         ELSE IF (INTERP.EQ.'LIN LOG ') THEN
            RESU = LINLOG(RVAR,X(IPT),Y(IPT),X(IPT+1),Y(IPT+1))
C
         ELSE IF (INTERP.EQ.'LOG LOG ') THEN
            RESU = LOGLOG(RVAR,X(IPT),Y(IPT),X(IPT+1),Y(IPT+1))
C
         ELSE IF (INTERP.EQ.'LOG LIN ') THEN
            RESU = LOGLIN(RVAR,X(IPT),Y(IPT),X(IPT+1),Y(IPT+1))
C
         ELSE IF (INTERP(1:3).EQ.'NON') THEN
            EPSI = SQRT ( R8PREM() )
            TOLE = EPSI * ABS( X(IPT) - X(IPT+1) )
            IF ( ABS(X(IPT)-RVAR) .LE. TOLE ) THEN
               RESU = Y(IPT)
            ELSEIF ( ABS(X(IPT+1)-RVAR) .LE. TOLE ) THEN
               RESU = Y(IPT+1)
            ELSE
               IER = 200
               CALL UTDEBM('A','FOCOLI','INTERPOLATION NON PERMISE.')
               CALL UTIMPR('L',' VALEUR A INTERPOLER:',1,RVAR)
               CALL UTIMPR('L','    BORNE INFERIEURE:',1,X(IPT))
               CALL UTIMPR('L','    BORNE SUPERIEURE:',1,X(IPT+1))
               CALL UTFINM()
               GOTO 9999
            ENDIF
C
         ELSE IF (INTERP(1:3).EQ.'INT') THEN
            CALL FONBPA(NOMFON,PROL,CBID,MXPARA,NBPF,NOMP)
            DO 80 I1 = 1,NBPF
               IPAR(I1) = 0
               DO 82 NUPAR = 1,NBPU
                  IF (NOMPU(NUPAR).EQ.NOMP(I1)) THEN
                     IF (IPAR(I1).EQ.0) THEN
                        IPAR(I1) = NUPAR
                     ELSE
                        IER = 210
                   CALL UTDEBM('A','FOCOLI','ERREUR A L''INTERPOLATION')
                        CALL UTIMPK('S',' FONCTION',1,NOMFON)
                        CALL UTIMPK('L',' PARAMETRE',NBPU,NOMPU)
                        CALL UTIMPK('S',' EN DOUBLE',0,' ')
                        CALL UTFINM()
                        GOTO 9999
                     ENDIF
                  ENDIF
 82            CONTINUE
               IF (IPAR(I1).EQ.0) THEN
                  IER = 220
                  CALL UTDEBM('A','FOCOLI','ERREUR A L''INTERPOLATION')
                  CALL UTIMPK('S',' FONCTION',1,NOMFON)
                  CALL UTIMPK('L',' PARAMETRES ATTENDUS',NBPF,NOMP)
                  CALL UTIMPK('L',' PARAMETRES RECUS   ',NBPU,NOMPU)
                  CALL UTFINM()
                  GOTO 9999
               ENDIF
 80         CONTINUE
            CHFITA=NOMFON//'.FITA'
            CALL JEVEUO(CHFITA,'L',LFITA)
            NOMFIT=ZK24(LFITA)(1:19)
            CALL FIINTE('F',NOMFIT,NBPF,IPAR,VALPU,RESU,IER)
C
         ELSE
            IER = 230
            CALL UTMESS('A','FOCOLI','INTERPOLATION '//INTERP//
     +                                           ' NON IMPLANTEE')
         ENDIF
C
C     --- EXTRAPOLATION ---
C
      ELSEIF (COLI.EQ.'E') THEN
         RESU = LINLIN(RVAR,X(IPT),Y(IPT),X(IPT+1),Y(IPT+1))
C
C     --- FONCTION TABULEE ---
C
      ELSEIF (COLI.EQ.'T') THEN
         CALL FONBPA(NOMFON,PROL,CBID,MXPARA,NBPF,NOMP)
         DO 90 I1 = 1,NBPF
            IPAR(I1) = 0
            DO 92 NUPAR = 1,NBPU
               IF (NOMPU(NUPAR).EQ.NOMP(I1)) THEN
                  IF (IPAR(I1).EQ.0) THEN
                     IPAR(I1) = NUPAR
                  ELSE
                     IER = 210
                   CALL UTDEBM('A','FOCOLI','ERREUR A L''INTERPOLATION')
                     CALL UTIMPK('S',' FONCTION',1,NOMFON)
                     CALL UTIMPK('L',' PARAMETRE',NBPU,NOMPU)
                     CALL UTIMPK('S',' EN DOUBLE',0,' ')
                     CALL UTFINM()
                     GOTO 9999
                  ENDIF
               ENDIF
 92         CONTINUE
            IF (IPAR(I1).EQ.0) THEN
               IER = 220
               CALL UTDEBM('A','FOCOLI','ERREUR A L''INTERPOLATION')
               CALL UTIMPK('S',' FONCTION',1,NOMFON)
               CALL UTIMPK('L',' PARAMETRES ATTENDUS',NBPF,NOMP)
               CALL UTIMPK('L',' PARAMETRES RECUS   ',NBPU,NOMPU)
               CALL UTFINM()
               GOTO 9999
            ENDIF
 90      CONTINUE
         CALL FIINTE('F',NOMFON,NBPF,IPAR,VALPU,RESU,IER)
C
      ELSE
         IER = 240
         CALL UTMESS('A','FOCOLI','RECHERCHE "'//COLI//'" INCONNUE')
      ENDIF
 9999 CONTINUE
C
      CALL JEDEMA()
      END

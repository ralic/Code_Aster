      SUBROUTINE FOCOLI ( IPT, COLI, INTERP, X, Y, RVAR, RESU, IER )
      IMPLICIT NONE
      INTEGER             IPT, IER
      REAL*8              X(*), Y(*), RVAR, RESU
      CHARACTER*1         COLI
      CHARACTER*24        INTERP
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 19/02/2008   AUTEUR MACOCCO K.MACOCCO 
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
      REAL*8    LINLIN, LINLOG, LOGLOG, LOGLIN, X0, X1, Y1, X2, Y2
      REAL*8    EPSI, TOLE, R8PREM
      REAL*8 VALR(3)
C ----------------------------------------------------------------------
      LINLIN(X0,X1,Y1,X2,Y2)= Y1+(X0-X1)*(Y2-Y1)/(X2-X1)
      LINLOG(X0,X1,Y1,X2,Y2)=EXP(LOG(Y1)+(X0-X1)*(LOG(Y2)-LOG(Y1))
     &                                        /(X2-X1))
      LOGLOG(X0,X1,Y1,X2,Y2)=EXP(LOG(Y1)+(LOG(X0)-LOG(X1))*(LOG(Y2)
     &                                     -LOG(Y1))/(LOG(X2)-LOG(X1)))
      LOGLIN(X0,X1,Y1,X2,Y2)=Y1+(LOG(X0)-LOG(X1))*(Y2-Y1)
     &                                         /(LOG(X2)-LOG(X1))
C ----------------------------------------------------------------------
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
               VALR (1) = RVAR
               VALR (2) = X(IPT)
               VALR (3) = X(IPT+1)
               CALL U2MESG('A', 'UTILITAI6_14',0,' ',0,0,3,VALR)
            ENDIF
C
         ELSE
            IER = 230
            CALL U2MESK('A','UTILITAI_84',1,INTERP)
         ENDIF
C
C     --- EXTRAPOLATION ---
C
      ELSEIF (COLI.EQ.'E') THEN
         RESU = LINLIN(RVAR,X(IPT),Y(IPT),X(IPT+1),Y(IPT+1))
C
      ELSE
         IER = 240
         CALL U2MESK('A','UTILITAI_85',1,COLI)
      ENDIF
C
      END

      SUBROUTINE VPNOR1 ( NORM, NEQ, NBMODE, DDLEXC, VECPRO, ISIGN,
     +                    NUMDDL, COEF )
      IMPLICIT   NONE
      INTEGER           NBMODE, NEQ, DDLEXC(*), ISIGN, NUMDDL
      REAL*8            VECPRO(NEQ,*), COEF(*)
      CHARACTER*(*)     NORM
      CHARACTER*24 VALK
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C     NORMALISATION DE VECTEURS D'UN CONCEPT MODE_FLAMB
C     ------------------------------------------------------------------
C IN  NORM   : TYPE DE NORMALISATION
C          = 'AVEC_CMP'
C          = 'EUCL', 'EUCL_TRAN', ...
C IN  NEQ    : NOMBRE D'EQUATIONS
C IN  NBMODE : NOMBRE DE MODES
C IN  DDLEXC : TABLEAU DES DDL EXCLUS
C              = 0 SI EXCLUS
C              = 1 SI NON EXCLUS
C VAR VECPRO : TABLEAU DES VECTEURS PROPRES
C OUT COEF   : COEFFICIENTS
C     ------------------------------------------------------------------
      INTEGER  IBID, IM, IE
      INTEGER VALI
      REAL*8   XNORM, XX1 
C DEB ------------------------------------------------------------------
C
      IF ( NORM.EQ.'AVEC_CMP' .OR. NORM(1:4) .EQ.'EUCL' ) THEN
C
C     --- NORMALISATION SUR LES DDL NON EXCLUS
C
         DO 2 IM = 1,NBMODE
            XNORM = 0.0D0
            IF (NORM(1:4).EQ.'EUCL') THEN
               DO 4 IE = 1,NEQ
                  XX1 = VECPRO(IE,IM) * DDLEXC(IE)
                  XNORM = XNORM + XX1*XX1
 4             CONTINUE
               XNORM = SQRT(XNORM)
            ELSE
               DO 6 IE = 1,NEQ
                  XX1 = VECPRO(IE,IM)*DDLEXC(IE)
                  IF (ABS(XNORM) .LT. ABS(XX1)) THEN
                    XNORM = XX1
                  ENDIF
 6             CONTINUE
            ENDIF
            XX1 = 1.0D0 / XNORM
            COEF(IM) = XX1
            DO 8 IE = 1,NEQ
               VECPRO(IE,IM) = VECPRO(IE,IM) * XX1
 8          CONTINUE
 2       CONTINUE
C
      ELSE
C
         VALK = NORM
         CALL U2MESG('F', 'ALGELINE4_77',1,VALK,0,0,0,0.D0)
C
      ENDIF
C
      IF ( ISIGN .EQ. 0 ) THEN
      ELSEIF ( ISIGN .EQ. 1 ) THEN
         DO 100 IM = 1,NBMODE
            XX1 = VECPRO(NUMDDL,IM)
            IF ( XX1 .LT. 0.0D0 ) THEN
               COEF(IM) = -COEF(IM)
               DO 102 IE = 1,NEQ
                  VECPRO(IE,IM) = -VECPRO(IE,IM)
 102           CONTINUE
            ENDIF
 100     CONTINUE
      ELSEIF ( ISIGN .EQ. -1 ) THEN
         DO 110 IM = 1,NBMODE
            XX1 = VECPRO(NUMDDL,IM)
            IF ( XX1 .GT. 0.0D0 ) THEN
               COEF(IM) = -COEF(IM)
               DO 112 IE = 1,NEQ
                  VECPRO(IE,IM) = -VECPRO(IE,IM)
 112           CONTINUE
            ENDIF
 110     CONTINUE
      ENDIF
C
      END

      SUBROUTINE MTXCNL(CUMUL,TYPCST,CONST,TYPMAT,LMAT,TYPRES,LRES,NEQ)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER                                     LMAT,       LRES
      CHARACTER*(*)     CUMUL, TYPCST
      CHARACTER*1                           TYPMAT   , TYPRES
      REAL*8                         CONST(2)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 09/06/99   AUTEUR ADBHHPM P.MASSIN 
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
C     MIXAGE DES .CONL    ---> LIRE AVERTISSEMENT CI DESSOUS
C     ------------------------------------------------------------------
C     CECI EST UNE ROUTINE INTERNE VOUS N'AVEZ PAS LE DROIT DE L'APPELER
C     DIRECTEMENT, NI MEME DE CRITIQUER.
C     NEANMOINS SI VOUS VOULEZ LA REECRIRE A VOTRE AISE.
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
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      REAL*8      UN,ZERO,RCUM
      COMPLEX*16 CUN,C8CST
C
      ZERO= 0.D0
      UN  = 1.D0
      CUN = UN
      IF ( CUMUL .EQ. 'CUMU' ) THEN
         RCUM = UN
      ELSE
         RCUM = ZERO
         IF ( TYPRES .EQ. 'R' ) THEN
            DO 10 IVAL = 0, NEQ-1
               ZR(LRES+IVAL) = UN
  10        CONTINUE
         ELSEIF ( TYPRES .EQ. 'C' ) THEN
            DO  20 IVAL = 0, NEQ-1
               ZC(LRES+IVAL) = CUN
  20        CONTINUE
         ENDIF
      ENDIF

      IF ( TYPRES .EQ. 'R' ) THEN
         DO 110 IVAL = 0, NEQ-1
            IF ( ZR(LMAT+IVAL) .NE. UN ) THEN
               ZR(LRES+IVAL)=RCUM*ZR(LRES+IVAL)+CONST(1)*ZR(LMAT+IVAL)
            ENDIF
 110     CONTINUE
      ELSEIF ( TYPMAT .EQ. 'R' ) THEN
         IF ( TYPCST(1:1) .EQ. 'R') THEN
            DO 210 IVAL = 0, NEQ-1
               IF ( ZR(LMAT+IVAL) .NE. UN ) THEN
                 ZC(LRES+IVAL)=RCUM*ZC(LRES+IVAL)+CONST(1)*ZR(LMAT+IVAL)
               ENDIF
 210        CONTINUE
          ELSEIF ( TYPCST(1:1) .EQ. 'C') THEN
            C8CST = DCMPLX(CONST(1),CONST(2))
            DO 220 IVAL = 0, NEQ-1
               IF ( ZR(LMAT+IVAL) .NE. UN ) THEN
                  ZC(LRES+IVAL)=RCUM*ZC(LRES+IVAL)+C8CST*ZR(LMAT+IVAL)
               ENDIF
 220        CONTINUE
          ENDIF
      ELSE
         IF( TYPCST(1:1).EQ. 'R') THEN
           DO 310 IVAL = 0, NEQ-1
              IF ( ZR(LMAT+IVAL) .NE. UN ) THEN
                ZC(LRES+IVAL)=RCUM*ZC(LRES+IVAL)+CONST(1)*ZC(LMAT+IVAL)
              ENDIF
 310       CONTINUE
         ELSEIF ( TYPCST(1:1) .EQ. 'C' ) THEN
           C8CST = DCMPLX(CONST(1),CONST(2))
           DO 320 IVAL = 0, NEQ-1
              IF ( ZR(LMAT+IVAL) .NE. UN ) THEN
                 ZC(LRES+IVAL)=RCUM*ZC(LRES+IVAL)+C8CST*ZC(LMAT+IVAL)
              ENDIF
 320       CONTINUE
         ENDIF
      ENDIF
      END

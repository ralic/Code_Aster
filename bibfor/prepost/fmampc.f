      SUBROUTINE FMAMPC ( NBFONC, NBPTOT, SIGM, RAMPMX )
      IMPLICIT   NONE
      INTEGER             NBFONC, NBPTOT
      REAL*8              SIGM(*), RAMPMX
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 01/10/97   AUTEUR CIBHHLV L.VIVAN 
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
C     -----------------------------------------------------------------
C     NBFONC  : IN  : NOMBRE DE FONCTIONS (6 EN 3D 4 EN 2D)
C     NBPTOT  : IN  : NOMBRE DE PAS DE TEMPS DE CALCUL
C     SIGM    : IN  : VECTEUR DES CONTRAINTES EN TOUS LES PAS DE TEMPS
C     RAMPMX  : OUT : VALEUR AMPLITUDE DE CISSION
C     -----------------------------------------------------------------
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
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ------------------------------------------------------------------
      INTEGER    IDEV, I1, I2, J
      REAL*8     SIG(6), RAMPC
C     ------------------------------------------------------------------
C
C------- CALCUL DU DEVIATEUR -------
C
      CALL WKVECT ( '&&FMAMPC.DEV', 'V V R', NBFONC*NBPTOT, IDEV )
      CALL FMDEVI ( NBFONC, NBPTOT, SIGM, ZR(IDEV) )
C
C -------- CALCUL AMPLITUDE DE CISSION ------
C
      RAMPMX = 0.D0
      DO 100 I1 = 1 , NBPTOT-1
         DO 200 I2 = I1+1 , NBPTOT
            DO 300 J = 1 , NBFONC
               SIG(J) = ZR(IDEV+(I2-1)*NBFONC+J-1)-
     +                                     ZR(IDEV+(I1-1)*NBFONC+J-1)
 300        CONTINUE
            IF ( NBFONC .EQ. 6 ) THEN
               RAMPC = (SIG(1)*SIG(1)+SIG(2)*SIG(2)+SIG(3)*SIG(3))/2.D0
     +                 + SIG(4)*SIG(4) + SIG(5)*SIG(5) + SIG(6)*SIG(6)
            ELSEIF ( NBFONC .EQ. 4 ) THEN
               RAMPC = (SIG(1)*SIG(1)+SIG(2)*SIG(2)+SIG(3)*SIG(3))/2.D0
     +                 + SIG(4)*SIG(4)
            ENDIF
            IF ( RAMPC .GT. RAMPMX ) RAMPMX = RAMPC
 200     CONTINUE
 100  CONTINUE
      RAMPMX = 1.D0/2.D0*SQRT(RAMPMX)
C
      CALL JEDETR ( '&&FMAMPC.DEV' )
C
      END

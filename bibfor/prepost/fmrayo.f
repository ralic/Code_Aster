      SUBROUTINE FMRAYO ( NBFONC, NBPTOT, SIGM, RAYON )
      IMPLICIT   NONE
      INTEGER             NBFONC, NBPTOT
      REAL*8              SIGM(*), RAYON
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 29/06/98   AUTEUR D6BHHAM A.M.DONORE 
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
C     RAYON   : OUT : VALEUR RAYON SPHERE CIRCONSCRITE AU CHARGEMENT
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
      INTEGER    IDEV, NBR, I, J, N2
      REAL*8     EPS, X, SIG(6), RAU(6), P, PMAC, A
C     ------------------------------------------------------------------
C
      EPS = 1.D-3
      X   = 5.D-2
C
C------- CALCUL DU DEVIATEUR -------
C
      CALL WKVECT ( '&&FMRAYO.DEVIAT', 'V V R', NBFONC*NBPTOT, IDEV )
      CALL FMDEVI ( NBFONC, NBPTOT, SIGM, ZR(IDEV) )
C
C---- CALCUL DE LA SPHERE CIRCONSCRITE AU CHARGEMENT ----
C
C---- INITIALISATION
C
      RAYON = 0.D0
      DO 10 J = 1 , NBFONC
         RAU(J) = 0.D0
         DO 20 I = 1 , NBPTOT
            RAU(J) = RAU(J) + ZR(IDEV+(I-1)*NBFONC+J-1)
 20      CONTINUE
         RAU(J) = RAU(J) / NBPTOT
 10   CONTINUE
      NBR = 0
C
C-----CALCUL RECURRENT
C
      N2 = 1
 30   CONTINUE
      N2 = N2 + 1
      IF ( N2 .GT. NBPTOT ) N2 = N2 - NBPTOT
      DO 40 J = 1 , NBFONC
         SIG(J) = ZR(IDEV+(N2-1)*NBFONC+J-1)-RAU(J)
 40   CONTINUE
      IF ( NBFONC .EQ. 6 ) THEN
         PMAC = ( SIG(1)*SIG(1)+SIG(2)*SIG(2)+SIG(3)*SIG(3) )/2.D0
     +          + SIG(4)*SIG(4) + SIG(5)*SIG(5) + SIG(6)*SIG(6)
      ELSEIF ( NBFONC .EQ. 4 ) THEN
         PMAC = ( SIG(1)*SIG(1)+SIG(2)*SIG(2)+SIG(3)*SIG(3) )/2.D0
     +          + SIG(4)*SIG(4)
      ENDIF
      PMAC = SQRT(PMAC)
      P = PMAC - RAYON
      IF ( P .GT. EPS ) THEN
         NBR = 0
         RAYON = RAYON + X*P
         A = ( PMAC - RAYON ) / PMAC
         DO 50 J = 1 , NBFONC
            RAU(J) =  RAU(J) + A*SIG(J)
 50      CONTINUE
      ELSE
         NBR = NBR + 1
      ENDIF
      IF ( NBR .LT. NBPTOT ) GOTO 30
C
      CALL JEDETR ( '&&FMRAYO.DEVIAT' )
C
      END

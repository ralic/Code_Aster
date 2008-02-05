      SUBROUTINE DINON3(NEQ,UL,DUL,UTL,NNO,
     &                  NBCOMP,VARIMO,RAIDE,NBPAR,PARAM,OKDIRE,
     &                  VARIPL)
C ----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  NEQ,NBCOMP,NNO,NBPAR
      REAL*8   UL(NEQ),DUL(NEQ),UTL(NEQ)
      REAL*8   VARIMO(NBCOMP*3),VARIPL(NBCOMP*3)
      REAL*8   RAIDE(NBCOMP),PARAM(6,NBPAR)
      LOGICAL  OKDIRE(6)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 05/02/2008   AUTEUR FLEJOU J-L.FLEJOU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================

C ======================================================================
C
C     RELATION DE COMPORTEMENT "CINEMATIQUE" (DISCRET NON LINEAIRE).
C
C     f = |F - X| - Me
C     .       .      .
C     F = Ke.(Utot - Uan)
C
C     X = Kr.a/(1+(Kr.a/FU)^n)^(1/n)
C
C        Ke   : raideur elastique
C        Fu   : limite en effort
C        n    : coefficient de non-linearite (dans catalogue > 1)
C        Kr   : raideur de la loi cinematique
C        Me   : seuil elastique
C
C======================================================================
C
C IN  :
C       NEQ    : NOMBRE DE DDL DE L'ELEMENT
C       UL     : DEPLACEMENT PRECEDENT REPERE LOCAL (DIM NEQ)
C       DUL    : INCREMENT DE DEPLACEMENT REPERE LOCAL (DIM NEQ)
C       UTL    : DEPLACEMENT COURANT REPERE LOCAL (DIM NEQ)
C       NNO    : NOMBRE DE NOEUDS
C       NBCOMP : NOMBRE DE COMPOSANTES
C       VARIMO : VARIABLES INTERNES A T- (3 PAR COMPOSANTES)
C       RAIDE  : RAIDEUR ELASTIQUE DES DISCRETS
C       NBPAR  : NOMBRE MAXIMAL DE PARAMETRE DE LA LOI
C       PARAM  : PARAMETRES DE LA LOI
C       OKDIRE : VRAI SI LE COMPORTEMENT AFFECTE CETTE DIRECTION
C
C OUT :
C       RAIDE  : RAIDEUR QUASI-TANGENTE AU COMPORTEMENT DES DISCRETS
C       VARIPL : VARIABLES INTERNES INTERNES A T+ (3 PAR COMPOSANTES)
C
C***************** DECLARATION DES VARIABLES LOCALES *******************
C
      INTEGER II
      REAL*8  R8MIEM,ULEL,DULEL,UTLEL,ZERO,UN,R8MIN

      REAL*8  PUIS,XXX,MU,KR,KE,MEL,DENO,DROTX,DROTXC
      REAL*8  MOMP,MOMM,MXPLUS,MXMOIN
      INTEGER IPLAS,ICUMU,IENER
C
C************ FIN DES DECLARATIONS DES VARIABLES LOCALES ***************

C ----------------------------------------------------------------------
      R8MIN  = R8MIEM()
      ZERO   = 0.0D0
      UN     = 1.0D0

      DO 20, II=1,NBCOMP
C        INDEX DES VARIABLES INTERNES
         IPLAS = 3*(II-1)+1
         ICUMU = 3*(II-1)+2
         IENER = 3*(II-1)+3
C        PAR DEFAUT LES VARIABLES N'EVOLUENT PAS
         VARIPL(IPLAS) = VARIMO(IPLAS)
         VARIPL(ICUMU) = VARIMO(ICUMU)
         VARIPL(IENER) = VARIMO(IENER)
C        SI LE COMPORTEMENT EST CINEMATIQUE
         IF ( OKDIRE(II) ) THEN
            MEL = PARAM(II,4)
            IF ( NNO .EQ. 1 ) THEN
               DULEL = DUL(II)
               ULEL  = UL(II)
               UTLEL = UTL(II)
            ELSE
               DULEL = DUL(II+NBCOMP) - DUL(II)
               ULEL  = UL(II+NBCOMP)  - UL(II)
               UTLEL = UTL(II+NBCOMP) - UTL(II)
            ENDIF
            IF ( ABS(DULEL) .GT. R8MIN ) THEN
               KE   = RAIDE(II)
               MU   = PARAM(II,1)
               PUIS = PARAM(II,2)
               KR   = PARAM(II,3)
C              CALCUL DE DEPASSEMENT DU SEUIL
               MOMM = KE*( ULEL - VARIMO(IPLAS))
               MOMP = KE*(UTLEL - VARIMO(IPLAS))
C              CALCUL DE MX(-)
               IF ( PUIS .LE. ZERO ) THEN
                  MXMOIN = VARIMO(ICUMU)*KR
               ELSE
                  XXX  = ABS(VARIMO(ICUMU))*KR/MU
                  DENO = (UN+XXX**PUIS)**(UN/PUIS)
                  MXMOIN = VARIMO(ICUMU)*KR/DENO
               ENDIF
               IF ( ABS(MOMP - MXMOIN) .GT. MEL ) THEN
                  IF ( DULEL .GE. ZERO ) THEN
C                    ACTUALISATION DE LA ROTATION CINEMATIQUE CUMULEE
                     DROTXC = DULEL - (MEL - (MOMM - MXMOIN))/KE
                     VARIPL(ICUMU) = VARIMO(ICUMU) + DROTXC
C                    CALCUL DE MX(+)
                     IF ( PUIS .LT. ZERO ) THEN
                        MXPLUS = VARIPL(ICUMU)*KR
                     ELSE
                        XXX  = ABS(VARIPL(ICUMU))*KR/MU
                        DENO = (UN+XXX**PUIS)**(UN/PUIS)
                        MXPLUS = VARIPL(ICUMU)*KR/DENO
                     ENDIF
C                    ACTUALISATION DE LA ROTATION CINEMATIQUE
                     DROTX = DROTXC - ABS(MXPLUS - MXMOIN)/KE
                     VARIPL(IPLAS) = VARIMO(IPLAS) + DROTX
                  ELSE
C                    ACTUALISATION DE LA ROTATION CINEMATIQUE CUMULEE
                     DROTXC = DULEL + (MEL + (MOMM - MXMOIN))/KE
                     VARIPL(ICUMU) = VARIMO(ICUMU) + DROTXC
C                    CALCUL DE MX(+)
                     IF ( PUIS .LT. ZERO ) THEN
                        MXPLUS = VARIPL(ICUMU)*KR
                     ELSE
                        XXX  = ABS(VARIPL(ICUMU))*KR/MU
                        DENO = (UN+XXX**PUIS)**(UN/PUIS)
                        MXPLUS = VARIPL(ICUMU)*KR/DENO
                     ENDIF
C                    ACTUALISATION DE LA ROTATION CINEMATIQUE
                     DROTX = DROTXC + ABS(MXPLUS - MXMOIN)/KE
                     VARIPL(IPLAS) = VARIMO(IPLAS) + DROTX
                  ENDIF
C                 CALCUL DU MOMENT +
                  MOMP = KE*(UTLEL - VARIPL(IPLAS))
C                 TANGENTE AU COMPORTEMENT
                  RAIDE(II) = ABS((MOMP - MOMM) / DULEL)
C                 CALCUL DE L'ENERGIE DISSIPEE
C                 Si petits pas : ABS(MEL*DROTX)+MXPLUS*DROTX
C                 Pour minimiser l'erreur, on utilise une intégration
C                 de degré 1. Pour un écrouissage cinématique linéaire
C                 cela donne la solution exacte.
                  VARIPL(IENER) = VARIMO(IENER) + ABS(MEL*DROTX)
     &                        + (MXPLUS+MXMOIN)*DROTX*0.5D0
               ENDIF
            ENDIF
         ENDIF
20    CONTINUE

      END

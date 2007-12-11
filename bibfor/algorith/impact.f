      SUBROUTINE IMPACT ( NMTAB, NBPT, FN, VN, WK3, OFFSET, T, ELAPSE,
     &                    NBCHOC, FNMAXA, FNMMOY, FNMETY,
     +                    NPARI, LPARI, VALEK )
      IMPLICIT   NONE
      INTEGER             NBPT, NBCHOC, NPARI
      REAL*8              FN(*), T(*), VN(*), OFFSET, ELAPSE, WK3(*),
     +                    FNMAXA, FNMETY, FNMMOY
      CHARACTER*16        LPARI(*), VALEK(*)
      CHARACTER*(*)       NMTAB
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 11/02/98   AUTEUR CIBHHLV L.VIVAN 
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
C ----------------------------------------------------------------------
C        COMPTAGE DES CHOCS AMV
C
C IN  : NBPT   : NB DE POINTS DU SIGNAL
C IN  : FN     : TABLEAU DU SIGNAL
C IN  : T      : TABLEAU DU TEMPS
C IN  : OFFSET : VALEUR DU SEUIL DE DETECTION D UN CHOC
C IN  : ELAPSE : TEMPS MINIMUM POUR VRAI FIN DE CHOC
C OUT : NBCHOC : NB DE CHOC GLOBAUX ( CRITERE ELAPSE )
C ----------------------------------------------------------------------
C
      INTEGER       IPAR(2), IREBO, ICHOC, IDEBUT, IFIN, NBPAS,
     +              I, J, IDECH, NBREBO, K
      REAL*8        IMPULS, PARA(5), ZERO, FNMAX, TCHOC, VINIT, DT,
     +              TFNMAX
      COMPLEX*16    C16B
      CHARACTER*8   K8B
C ----------------------------------------------------------------------
C
      ZERO   = 0.0D0
      NBCHOC = 0
      NBREBO = 0
      FNMAX  = ZERO
      FNMAXA = ZERO
      FNMMOY = ZERO
      FNMETY = ZERO
      TFNMAX = ZERO
      IMPULS = ZERO
      TCHOC  = ZERO
      VINIT  = ZERO
      IREBO  = 0
      ICHOC  = 0
      IDEBUT = 1
      IFIN   = 1
      DT = T(4) - T(3)
      NBPAS = NINT ( ELAPSE / DT )
C
      K = 0
      DO 10 I = 1 , NBPT
C
         IF ( ABS(FN(I)) .LE. OFFSET ) THEN
C
C           SI SOUS LE SEUIL DE FORCE
C
            IF ( IREBO .EQ. 1 ) THEN
C
C              ET QUE ETAIT EN REBOND ALORS COMPTER REBOND
C
               NBREBO = NBREBO + 1
            ENDIF
C
            IDECH = 0
C
            DO 15 J = 1,NBPAS
C
C              EST CE QUE C'EST LA FIN D'UN CHOC GLOBAL
C
               IF ( ABS(FN(I+J)) .GT. OFFSET ) IDECH = 1
 15         CONTINUE
C
            IF ( IDECH.EQ.0 .AND. ICHOC.EQ.1 ) THEN
C
C              OUI C'EST LA FIN D'UN CHOC GLOBAL
C
               IFIN   = I
               TCHOC  = T(IFIN) - T(IDEBUT)
               FNMMOY = FNMMOY + FNMAX
               FNMETY = FNMETY + FNMAX*FNMAX
               NBCHOC = NBCHOC + 1
               ICHOC  = 0
               K = K + 1
               WK3(K) = FNMAX
               PARA(1) = TFNMAX
               PARA(2) = FNMAX
               PARA(3) = IMPULS
               PARA(4) = TCHOC
               PARA(5) = VINIT
               IPAR(1) = NBCHOC
               IPAR(2) = NBREBO
               CALL TBAJLI ( NMTAB, NPARI, LPARI,
     +                          IPAR, PARA, C16B, VALEK, 0 )
            ENDIF
C
            IREBO = 0
C
         ELSE
C
            IF ( ICHOC .EQ. 0 ) THEN
C              DEBUT D'UN CHOC GLOBAL
               IDEBUT = I
               VINIT  = VN(IDEBUT)
               FNMAX  = ZERO
               IMPULS = ZERO
               NBREBO = 0
            ENDIF
            IF (I.EQ.1) THEN
               IMPULS = IMPULS + FN(I)*T(I)/2.D0
            ELSEIF (I.LT.NBPT) THEN
               IMPULS = IMPULS + FN(I)*(T(I+1)-T(I-1))/2.D0
            ELSE
               IMPULS = IMPULS + FN(I)*T(I)/2.D0
            ENDIF
            IF ( FN(I) .GE. FNMAX ) THEN
               FNMAX  = FN(I)
               TFNMAX = T(I)
            ENDIF
            IF ( FNMAX .GE. FNMAXA )  FNMAXA = FNMAX
            IREBO = 1
            ICHOC = 1
C
         ENDIF
C
 10   CONTINUE
C
      IF ( NBCHOC .NE. 0 ) THEN
         FNMMOY = FNMMOY / NBCHOC
         FNMETY = SQRT( FNMETY/NBCHOC - FNMMOY*FNMMOY )
      ELSE
         K = K + 1
         WK3(K) = FNMAX
         FNMMOY  = ZERO
         FNMETY  = ZERO
         PARA(1) = TFNMAX
         PARA(2) = FNMAX
         PARA(3) = IMPULS
         PARA(4) = TCHOC
         PARA(5) = VINIT
         NBREBO = 0
         IPAR(1) = NBCHOC
         IPAR(2) = NBREBO
         CALL TBAJLI ( NMTAB, NPARI, LPARI,
     +                        IPAR, PARA, C16B, VALEK, 0 )
      ENDIF
C
      END

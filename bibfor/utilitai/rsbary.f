      SUBROUTINE RSBARY(LR8,NR8,TOUS,LEXI,X,I1,I2,IPOSIT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 06/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     ARGUMENTS:
C     ----------
      INTEGER NR8,I1,I2,IPOSIT
      REAL*8 LR8(*),X
      LOGICAL TOUS,LEXI(*)
C ----------------------------------------------------------------------
C     BUT:
C      TROUVER DANS UNE LISTE DE R8 QUELS SONT LES 2 REELS LES PLUS
C      PROCHES DU REEL X DONNE. (POUR FAIRE UN BARYCENTRE)
C     (ON PEUT NE PAS PRENDRE EN COMPTE TOUS LES REELS DE LA LISTE GRACE
C       A L'ARGUMENT LEXI)
C     IN:
C     NR8    : NOMBRE DE REELS DANS LA LISTE LR8.
C     LR8    : LISTE DE REELS (PAS FORCEMENT ORDONNEE).
C     TOUS   : INDIQUE QUE TOUS LES REELS DE LA LISTE SONT A CONSIDERER.
C     LEXI   : INDIQUE QUELS SONT LES REELS A CONSIDERER (SI TOUS=FALSE)
C              SI TOUS=.TRUE. CET ARGUMENT EST INUTILISE.
C      X     : REEL DONT ON CHERCHE LES COORDONEES BARYCENTRIQUES.
C
C     OUT:
C     I1,I2  : INDICES DES 2 REELS DE LA LISTE QUI "ENCADRENT" X
C              (LR8(I1) =< LR8(I2))
C              (EVENTUELLEMENT I1 PEUT ETRE EGAL A I2)
C     IPOSIT : CODE LA POSITION DE X PAR RAPPORT A LR8(I1) ET LR8(I2)
C         IPOSIT=0  -->  LR8(I1)  =<   X  =<   LR8(I2)
C         IPOSIT=1  -->  LR8(I1) =<  LR8(I2) =< X  (PROL_DR)
C         IPOSIT=-1 -->  X =< LR8(I1) =<  LR8(I2)  (PROL_GA)
C         IPOSIT=-2 -->  ERREUR : LA LISTE DE REELS EST VIDE.
C
C
C ----------------------------------------------------------------------
      INTEGER IPP,IP,IS,ISS
      REAL*8 XPP,XP,XS,XSS
      LOGICAL AFAIRE
C
C DEB-------------------------------------------------------------------
C
C     --------XI-----XPP--XP-------X---XS-------XSS-------XJ-->
C
C     ON APPELLE : XP : LE REEL PRECEDENT X DANS LA LISTE
C                XPP: LE REEL PRECEDENT XP DANS LA LISTE
C                XS : LE REEL SUIVANT  X DANS LA LISTE
C                XSS: LE REEL SUIVANT  XS DANS LA LISTE
C               XMAX: LE REEL MAX DE LA LISTE
C               XMIN: LE REEL MIN DE LA LISTE
C
      IP = 0
      IPP = 0
      IS = 0
      ISS = 0
C
C     -- CAS DE LA LISTE VIDE:
C     ------------------------
      DO 100,I = 1,NR8
         IF (TOUS) THEN
            AFAIRE = .TRUE.
         ELSE
            IF (LEXI(I)) THEN
               AFAIRE = .TRUE.
            ELSE
               AFAIRE = .FALSE.
            END IF
         END IF
         IF (AFAIRE) THEN
            IMIN = I
            IMAX = I
            XMAX = LR8(I)
            XMIN = LR8(I)
            GO TO 101
         END IF
  100 CONTINUE
      IPOSIT = -2
      GO TO 9999
  101 CONTINUE
C
C     RECHERCHE DE XMAX ET XMIN:
      DO 1,I = 1,NR8
         IF (TOUS) THEN
            AFAIRE = .TRUE.
         ELSE
            IF (LEXI(I)) THEN
               AFAIRE = .TRUE.
            ELSE
               AFAIRE = .FALSE.
            END IF
         END IF
         IF (AFAIRE) THEN
            IF (LR8(I).GE.XMAX) THEN
               IMAX = I
               XMAX = LR8(I)
            END IF
            IF (LR8(I).LE.XMIN) THEN
               IMIN = I
               XMIN = LR8(I)
            END IF
         END IF
    1 CONTINUE
C
C     -- 1ER CAS X EST INCLU DANS L'INTERVALLE DE LA LISTE:
      IF ((X.GE.XMIN) .AND. (X.LE.XMAX)) THEN
         IPOSIT = 0
         IP = IMIN
         IS = IMAX
         XP = XMIN
         XS = XMAX
         DO 2,I = 1,NR8
            IF (TOUS) THEN
               AFAIRE = .TRUE.
            ELSE
               IF (LEXI(I)) THEN
                  AFAIRE = .TRUE.
               ELSE
                  AFAIRE = .FALSE.
               END IF
            END IF
            IF (AFAIRE) THEN
               IF ((LR8(I).GE.X) .AND. (LR8(I).LE.XS)) THEN
                  IS = I
                  XS = LR8(I)
               END IF
               IF ((LR8(I).LE.X) .AND. (LR8(I).GE.XP)) THEN
                  IP = I
                  XP = LR8(I)
               END IF
            END IF
    2    CONTINUE
         I1 = IP
         I2 = IS
         GO TO 9999
      END IF
C
C     -- 2EME CAS X EST A DROITE DE L'INTERVALLE DE LA LISTE:
      IF (X.GE.XMAX) THEN
         IPOSIT = 1
         IP = IMAX
         XP = XMAX
         IPP = IMIN
         XPP = XMIN
         DO 31,I = 1,NR8
            IF (TOUS) THEN
               AFAIRE = .TRUE.
            ELSE
               IF (LEXI(I)) THEN
                  AFAIRE = .TRUE.
               ELSE
                  AFAIRE = .FALSE.
               END IF
            END IF
            IF (AFAIRE) THEN
               IF (I.EQ.IMAX) GO TO 31
               IF (LR8(I).GE.XPP) THEN
                  IPP = I
                  XPP = LR8(I)
               END IF
            END IF
   31    CONTINUE
         I1 = IPP
         I2 = IP
         GO TO 9999
      END IF
C
C     -- 3EME CAS X EST A GAUCHE DE L'INTERVALLE DE LA LISTE:
      IF (X.LE.XMIN) THEN
         IPOSIT = -1
         IS = IMIN
         XS = XMIN
         ISS = IMAX
         XSS = XMAX
         DO 41,I = 1,NR8
            IF (TOUS) THEN
               AFAIRE = .TRUE.
            ELSE
               IF (LEXI(I)) THEN
                  AFAIRE = .TRUE.
               ELSE
                  AFAIRE = .FALSE.
               END IF
            END IF
            IF (AFAIRE) THEN
               IF (I.EQ.IMIN) GO TO 41
               IF (LR8(I).LE.XSS) THEN
                  ISS = I
                  XSS = LR8(I)
               END IF
            END IF
   41    CONTINUE
         I1 = IS
         I2 = ISS
         GO TO 9999
      END IF
C
C
C
 9999 CONTINUE
      END

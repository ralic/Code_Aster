      SUBROUTINE MOINSR(J,N,IDIL,IDIICH,IDSUIV,NOSUIV,IDIP,NOIP,IILIB,
     &                  IIMAX)
      IMPLICIT NONE
      INTEGER J,N,IDIL,IDIICH,IDSUIV,IDIP,IILIB,IIMAX
      INTEGER K,IDEBCH,II1,KIL,KIP
      CHARACTER*(*) NOSUIV,NOIP
C     -----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ASSEMBLA  DATE 06/05/2008   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================

C     INSERTION DU TABLEAU D'ENTIERS ORDONNES IL DANS LA CHAINE ORDONNEE
C     J DE LA STRUCTURE CHAINEE:  IICH,SUIV,IP
C     -----------------------------------------------------------------
C     ATTENTION CE PROGRAMME CONTIENT DES POSSIBILITES D'EXTENSION
C     DES VECTEURS D'ENTIERS DE NOM KSUIV ET KIP.
C     -----------------------------------------------------------------
C IN  J         NUMERO DE LA CHAINE DANS LAQUELLE ON INSERE IL
C IN  N         LONGUEUR DU TABLEAU IL
C IN  IDIL      AD. JEVEUX DU TABLEAU IL QUE L'ON INSERE DANS LA
C               CHAINE J
C VAR IDIICH    AD. JEVEUX DE LA TABLE DES ADRESSES DANS LE VECTEUR DE
C               NOM KIP DU DEBUT DE LA CHAINE K.
C               SI IICH(K) <= 0 LA CHAINE K EST VIDE .
C VAR IDSUIV    AD. JEVEUX DU VECTEUR DE NOM NOSUIV.
C VAR NOSUIV    NOM DU TABLEAU DE CHAINAGE : SUIV(K) DONNE L'ADRESSE
C               DANS IP DE L'ELEMENT QUI SUIT L'ELEMENT K.
C               SI SUIV(K) <= 0 IP(K) EST LE DERNIER ELEMENT DE LA
C               CHAINE -SUIV(K)
C VAR IDIP      AD. JEVEUX DU VECTEUR DE NOM NOIP
C VAR NOIP      NOM DE LA TABLE DES ELEMENTS CHAINES
C VAR IILIB     NUMERO DE LA PREMIERE ADRESSE LIBRE DE IP
C VAR IIMAX     DIMENSION (IN: INITIALE, OUT :  FINALE) DES VECTEURS
C               SUIV ET IP QUI SONT AGRANDIS SI C'EST NECESSAIRE
C------------------------------------------------------------------
C- PRECAUTIONS D'EMPLOI:  N               > 0
C                         IL(I+1)         > IL(I)
C                         IP(IISUIV(K)) > IP(K)
C------------------------------------------------------------------
C     -------------------------------------------------------------
C     FONCTIONS JEVEUX
C     -------------------------------------------------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
C     -------------------------------------------------------------
C     COMMUNS   JEVEUX
C     -------------------------------------------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
C----------------------------------------------------------------------
C                DEBUT DES INSTRUCTIONS
C----------------------------------------------------------------------
      IF (ZI(IDIICH-1+J).LE.0) THEN


C        --- LA CHAINE J EST VIDE. ON L'INITIALISE PAR IL(1:N) ---
        ZI(IDIICH-1+J) = IILIB
        IF ((IILIB+N).GE.IIMAX) THEN
          IIMAX = 1.5D0*IIMAX
          CALL JUVECA(NOIP,IIMAX)
          CALL JEVEUO(NOIP,'E',IDIP)
          CALL JUVECA(NOSUIV,IIMAX)
          CALL JEVEUO(NOSUIV,'E',IDSUIV)
        END IF
        DO 10 K = 1,N
          ZI(IDSUIV-1+IILIB) = IILIB + 1
          ZI(IDIP-1+IILIB) = ZI(IDIL-1+K)
          IILIB = IILIB + 1
   10   CONTINUE
C        MARQUAGE FIN DE CHAINE
        ZI(IDSUIV-1+IILIB-1) = -J

      ELSE
C---
C        LA CHAINE J EST NON VIDE:  DETERMINATION DES PREMIERS INDICES
C        KIL ET II1 TELS QUE IP(II1) < IL(KIL)
        IDEBCH = ZI(IDIICH-1+J)
        IF (ZI(IDIL).LT.ZI(IDIP-1+IDEBCH)) THEN

C           INSERTION DE IL(1) EN DEBUT DE CHAINE
          ZI(IDIICH-1+J) = IILIB
          ZI(IDIP-1+IILIB) = ZI(IDIL)
          ZI(IDSUIV-1+IILIB) = IDEBCH
          II1 = IILIB
          KIL = 2
          IF ((IILIB+1).GE.IIMAX) THEN
            IIMAX = 1.5D0*IIMAX
            CALL JUVECA(NOIP,IIMAX)
            CALL JEVEUO(NOIP,'E',IDIP)
            CALL JUVECA(NOSUIV,IIMAX)
            CALL JEVEUO(NOSUIV,'E',IDSUIV)
          END IF
          IILIB = IILIB + 1

        ELSE IF (ZI(IDIL).EQ.ZI(IDIP-1+IDEBCH)) THEN

C           IL(1) EXISTE DEJA DANS LA CHAINE J . PAS D'INSERTION
          II1 = IDEBCH
          KIL = 2

        ELSE

C           IP(IDEBCH) < IL(1)
          II1 = IDEBCH
          KIL = 1
        END IF

C        INSERTION DU RESTE DE IL
        KIP = ZI(IDSUIV-1+II1)

   20   CONTINUE
        IF (KIL.LE.N) THEN

C           TOUS LES ELEMENTS DE IL N'ONT PAS ETE TRAITES
   30     CONTINUE
          IF (KIP.GT.0) THEN

C              LA CHAINE J N A PAS ETE ENTIEREMENT PARCOURUE.
C              INSERTION EVENTUELLE DE IL(KIL) EN MILIEU DE CHAINE.

C              ASSERTION : IL(KIL) > IP(II1)

            IF (ZI(IDIL-1+KIL).EQ.ZI(IDIP-1+KIP)) THEN

C                  L'ELEMENT IL(KIL) EXISTE DEJA DANS LA CHAINE J
C                  PAS D'INSERTION
              KIL = KIL + 1
              II1 = KIP
              KIP = ZI(IDSUIV-1+II1)
              GO TO 20

            ELSE IF (ZI(IDIL-1+KIL).GT.ZI(IDIP-1+KIP)) THEN

C                  L'ELEMENT IL(KIL) NE S'INSERE PAS AVANT IP(KIP)
              II1 = KIP
              KIP = ZI(IDSUIV-1+II1)
              GO TO 30

            ELSE

C                  IP(II1) <IL(KIL) <IP(KIP) INSERTION DE IL(KIL)
C                  ENTRE CES 2 ELEMENTS
              ZI(IDSUIV-1+II1) = IILIB
              II1 = IILIB
              ZI(IDSUIV-1+II1) = KIP
              ZI(IDIP-1+IILIB) = ZI(IDIL-1+KIL)
              IF ((IILIB+1).GE.IIMAX) THEN
                IIMAX = 1.5D0*IIMAX
                CALL JUVECA(NOIP,IIMAX)
                CALL JEVEUO(NOIP,'E',IDIP)
                CALL JUVECA(NOSUIV,IIMAX)
                CALL JEVEUO(NOSUIV,'E',IDSUIV)
              END IF
              IILIB = IILIB + 1
              KIL = KIL + 1
              GO TO 20

            END IF

          ELSE

C              LA CHAINE J A ETE ENTIEREMENT PARCOURUE.
C              INSERTION DES ELEMENT RESTANT DE IL EN FIN DE CHAINE  .
            ZI(IDSUIV-1+II1) = IILIB
            IF ((IILIB+1+N-KIL).GE.IIMAX) THEN
              IIMAX = 1.5D0*IIMAX
              CALL JUVECA(NOIP,IIMAX)
              CALL JEVEUO(NOIP,'E',IDIP)
              CALL JUVECA(NOSUIV,IIMAX)
              CALL JEVEUO(NOSUIV,'E',IDSUIV)
            END IF
            DO 40 K = KIL,N
              ZI(IDSUIV-1+IILIB) = IILIB + 1
              ZI(IDIP-1+IILIB) = ZI(IDIL-1+K)
              IILIB = IILIB + 1
   40       CONTINUE
C              MARQUAGE FIN DE CHAINE
            ZI(IDSUIV-1+IILIB-1) = -J

          END IF
        END IF
      END IF
      END

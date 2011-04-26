      SUBROUTINE UTTRIF(VALE,NB,TYPFON)
      IMPLICIT NONE
      INTEGER       NB
      REAL*8        VALE(*)
      CHARACTER*(*) TYPFON
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C     TRI DES FONCTIONS PAR ABSCISSES CROISSANTES
C     (METHODE DE REMONTEE DES BULLES REPRIS DE UTTRIR)
C ----------------------------------------------------------------------
C POUR LES FONCTIONS A VALEURS REELLES :
C IN/OUT : VALE   : ABSCISSES, VALEUR
C                   SOUS LA FORME X1,X2,... Y1,Y2,...
C POUR LES FONCTIONS A VALEURS COMPLEXES :
C IN/OUT : VALE   : ABSCISSES, PARTIE REELLE, PARTIE IMAGINAIRE
C                   SOUS LA FORME X1,Y1,Z1, X2,Y2,Z2, ...
C IN     : NB     : NBRE DE POINTS DE LA FONCTION
C IN     : TYPFON : TYPE DE LA FONCTION A REORDONNER
C ----------------------------------------------------------------------
      INTEGER      J,L,INCRS,IS9
      REAL*8       XT
      CHARACTER*16 NOMCMD,K16B1,K16B2
C
      IF(TYPFON.EQ.'FONCTION')THEN
C        --- TRI BULLE ---
         IF ( NB .GT. 1 ) THEN
C            --- CHOIX DE L'INCREMENT ---
             INCRS = 1
             IS9   = NB / 9
 10          CONTINUE
             IF (INCRS .LT. IS9) THEN
                INCRS = 3*INCRS+1
                GOTO 10
             ENDIF
C            --- REMONTEE DES BULLES ---
120          CONTINUE
             DO 150 J=INCRS+1,NB
                L = J-INCRS
130             CONTINUE
                IF ( L.GT.0) THEN
                   IF ( VALE(L) .GT. VALE(L+INCRS) ) THEN
C                     --- PERMUTATION DES ABSCISSES ---
                      XT            = VALE(L)
                      VALE(L)       = VALE(L+INCRS)
                      VALE(L+INCRS) = XT
C                     --- PERMUTATION DES ORDONNEES ---
                      XT               = VALE(L+NB)
                      VALE(L+NB)       = VALE(L+NB+INCRS)
                      VALE(L+NB+INCRS) = XT
                      L = L - INCRS
                      GOTO 130
                  ENDIF
                ENDIF
150          CONTINUE
             INCRS = INCRS/3
             IF (INCRS.GE.1) GOTO 120
         ENDIF
      ELSEIF(TYPFON.EQ.'FONCT_C')THEN
C        --- TRI BULLE ---
         IF ( NB .GT. 1 ) THEN
C            --- CHOIX DE L'INCREMENT ---
             INCRS = 1
             IS9   = NB / 9
 11          CONTINUE
             IF (INCRS .LT. IS9) THEN
                INCRS = 3*INCRS+1
                GOTO 11
             ENDIF
C            --- REMONTEE DES BULLES ---
121          CONTINUE
             DO 151 J=INCRS+1,NB
                L = J-INCRS
131             CONTINUE
                IF ( L.GT.0) THEN
                   IF ( VALE(L) .GT. VALE(L+INCRS) ) THEN
C                     --- PERMUTATION DES ABSCISSES ---
                      XT            = VALE(L)
                      VALE(L)       = VALE(L+INCRS)
                      VALE(L+INCRS) = XT
C                     --- PERMUTATION DES PARTIES REELLES ---
                      XT                       = VALE(NB+2*(L-1)+1)
                      VALE(NB+2*(L-1)+1)      = VALE(NB+2*(L+INCRS-1)+1)
                      VALE(NB+2*(L+INCRS-1)+1) = XT
C                     --- PERMUTATION DES PARTIES IMAGINAIRES ---
                      XT                       = VALE(NB+2*(L-1)+2)
                      VALE(NB+2*(L-1)+2)      = VALE(NB+2*(L+INCRS-1)+2)
                      VALE(NB+2*(L+INCRS-1)+2) = XT
                      L = L - INCRS
                      GOTO 131
                  ENDIF
                ENDIF
151          CONTINUE
             INCRS = INCRS/3
             IF (INCRS.GE.1) GOTO 121
         ENDIF
      ELSE
         CALL GETRES(K16B1,K16B2,NOMCMD)
       CALL U2MESS('F','UTILITAI5_58')
      ENDIF
C
      END

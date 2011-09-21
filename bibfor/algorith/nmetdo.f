      SUBROUTINE NMETDO(NOMCMD)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
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
C NON-LINEAIRE MECANIQUE - ERREUR EN TEMPS - DONNEES
C *            *           *         *       **
C
      IMPLICIT NONE
      CHARACTER*16  NOMCMD
C ----------------------------------------------------------------------
C
C COMMANDE STAT_NON_LINE : POUR CALCUL DE L'INDICATEUR D'ERREUR
C                          TEMPORELLE, ON VERIFIE QU'ON EST BIEN DANS LE
C                          CADRE DES MODELISATIONS HM SATUREES AVEC
C                          COMPORTEMENT MECANIQUE ELASTIQUE
C
C ----------------------------------------------------------------------

C     IN   NOMCMD    : NOM DE LA COMMANDE
C
      INTEGER      NBOCC,N1,N2,DIMAKI,II,JJ,K
C DIMAKI = DIMENSION MAX DE LA LISTE DES RELATIONS KIT
      PARAMETER (DIMAKI=9)
C
      INTEGER      IAUX,JAUX,LXLGUT
      LOGICAL      ELLISQ
      CHARACTER*16 CHAINE,INERTE
      CHARACTER*16 COMP1,COMEL(DIMAKI),ARGII,ARGJJ
      INTEGER      IARG
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C======================================================================
C 1. QUELLE EST LA DEMANDE SUR L'INDICATEUR D'ERREUR EN TEMPS ?
C======================================================================
C
C               1234567890123456
      INERTE = 'NON             '
C
      IF ( NOMCMD(1:13).EQ.'STAT_NON_LINE' ) THEN
        CALL GETFAC('INCREMENT',N1)
C
        IF (N1.GT.0) THEN
          CALL GETVTX( 'INCREMENT', 'ERRE_TEMPS', 1,IARG,1, CHAINE, N1)
          IF ( N1.GT.0 ) THEN
              INERTE = CHAINE
          ENDIF
        ENDIF
C
C======================================================================
C 2. SI ON DEMANDE LE CALCUL DE L'INDICATEUR D'ERREUR EN TEMPS, ON
C    VERIFIE QUE C'EST POSSIBLE
C======================================================================
C
        IAUX = LXLGUT(CHAINE)
        IF ( IAUX.EQ.3 ) THEN
C
          IF ( INERTE(1:3).NE.'NON' ) THEN
C
            JAUX = 1
C
C 2.1 ==> ON VERIFIE QUE LE MOT-CLEF COMP_INCR EST RENSEIGNE
C
            IF (NOMCMD(1:4).EQ.'STAT') THEN
C
              CALL GETFAC('COMP_INCR',NBOCC)
C
              IF (NBOCC.EQ.0) THEN
                CALL U2MESS('F','INDICATEUR_25')
C
              ELSE
                DO 10 K = 1,NBOCC
C
C 2.2 ==> SI OUI, ON VERIFIE QU'ON EST BIEN DANS LE CAS D'UNE
C         MODELISATION HM SATUREE AVEC UN COMPORTEMENT MECANIQUE
C         ELASTIQUE
C
                  CALL GETVTX('COMP_INCR','RELATION',K,IARG,1,COMP1,N1)
C
                  IF (COMP1(1:6).EQ.'KIT_HM') THEN
C
                    CALL GETVTX('COMP_INCR','RELATION_KIT',K,IARG,
     &                          DIMAKI,COMEL(1),N2)
C
                    IF (N2.EQ.0) THEN
                      CALL U2MESS('F','ALGORITH7_56')
                    ELSE IF (N2.GT.DIMAKI) THEN
                      CALL U2MESS('F','ALGORITH7_57')
                    ELSE
C
                      ELLISQ = .FALSE.
                      DO 101 II = 1,N2
                        ARGII = COMEL(II)
                        IF ((ARGII(1:4).EQ.'ELAS').OR.
     &                      (ARGII(1:9).EQ.'LIQU_SATU')) THEN
                          IAUX = II + 1
                          DO 102 JJ = IAUX,N2
                            ARGJJ = COMEL(JJ)
                            IF ((ARGJJ(1:4).EQ.'ELAS').OR.
     &                          (ARGJJ(1:9).EQ.'LIQU_SATU')) THEN
                              ELLISQ = .TRUE.
                            ENDIF
 102                      CONTINUE
                        ENDIF
 101                  CONTINUE
C
C 2.3 ==> SI OUI, ET SI L'UTILISATEUR L'A DEMANDE, ON CALCULE
C         L'INDICATEUR D'ERREUR TEMPORELLE
C
                      IF (ELLISQ) THEN
                        JAUX = 0
                      ENDIF
                    ENDIF
                  ENDIF
 10             CONTINUE
C
              ENDIF
C
            ENDIF
C
C 2.4. ==> CE N'EST PAS BON ...
C
            IF ( JAUX.NE.0 ) THEN
              CALL U2MESS('F','INDICATEUR_23')
            ENDIF
C
          ENDIF
C
        ENDIF
C
      ENDIF
C
      CALL JEDEMA()
C
      END

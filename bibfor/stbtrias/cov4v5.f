      SUBROUTINE COV4V5 (CODDES,CODGRA)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF STBTRIAS  DATE 18/09/2000   AUTEUR JMBHH01 J.M.PROIX 
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
C     ============================================================
C     !                                                          !
C     !  AUTEURS:J.F.LAMAUDIERE                   DATE:18/04/91  !
C     !                                                          !
C     !                                                          !
C     ============================================================
C     !                                                          !
C     !  FONCTION:ASSURE LA CORRESPONDANCE ENTRE LE CODE DESCRI- !
C     !           PTEUR ( SUPERTAB I-DEAS 5.0 ) ET LE CODE GRA   !
C     !           PHIQUE ( SUPERTAB I-DEAS 4.0) POUR LES MAILLES !
C     !                                                          !
C     ============================================================
C     !                                                          !
C     !  SOUS PROGRAMMES APPELES : NEANT                         !
C     !                                                          !
C     !  SOUS PROGRAMME APPELANT : PRESUP                        !
C     !                                                          !
C     ============================================================
C     !                                                          !
C     !                   ***************                        !
C     !                   *  ARGUMENTS  *                        !
C     !                   ***************                        !
C     !                                                          !
C     !  ******************************************************  !
C     !  *   NOM    *  TYPE * MODE *MODIFIE*      ROLE        *  !
C     !  ******************************************************  !
C     !  *          *       *      *       *                  *  !
C     !  * CODDES   *INTEGER*ENTREE* NON   *CODE DESCRIPTEUR  *  !
C     !  *          *       *      *       * POUR UNE MAILLE  *  !
C     !  *          *       *      *       * (I-DEAS 5.0)     *  !
C     !  *          *       *      *       *                  *  !
C     !  * CODGRA   *INTEGER*SORTIE* NON   *CODE GRAPHIQUE    *  !
C     !  *          *       *      *       * DE LA MEME MAILLE*  !
C     !  *          *       *      *       *                  *  !
C     !  ******************************************************  !
C     !                                                          !
C     ============================================================
C
C  --> DECLARATION DES ARGUMENTS
       INTEGER CODDES,CODGRA
C
        IF (CODDES.EQ.171) THEN
          CODGRA=1
        ELSE IF (CODDES.EQ.91 .OR. CODDES.EQ.61 .OR.
     &           CODDES.EQ.41 .OR. CODDES.EQ.51 .OR.
     &           CODDES.EQ.74 .OR. CODDES.EQ.81) THEN
          CODGRA=2
        ELSE IF (CODDES.EQ.11 .OR. CODDES.EQ.21 .OR.
     &           CODDES.EQ.22.OR.CODDES.EQ.23) THEN
          CODGRA=1
        ELSE IF (CODDES.EQ.24) THEN
          CODGRA=35
        ELSE IF (CODDES.EQ.92 .OR. CODDES.EQ.62 .OR.
     &           CODDES.EQ.42 .OR. CODDES.EQ.52 .OR.
     &           CODDES.EQ.72 .OR. CODDES.EQ.82) THEN
          CODGRA=3
        ELSE IF (CODDES.EQ.93 .OR. CODDES.EQ.63 .OR.
     &           CODDES.EQ.43 .OR. CODDES.EQ.53 .OR.
     &           CODDES.EQ.73) THEN
          CODGRA=4
        ELSE IF (CODDES.EQ.94 .OR. CODDES.EQ.64 .OR.
     &           CODDES.EQ.44 .OR. CODDES.EQ.54 .OR.
     &           CODDES.EQ.71 .OR. CODDES.EQ.84) THEN
          CODGRA=5
        ELSE IF (CODDES.EQ.95 .OR. CODDES.EQ.65 .OR.
     &           CODDES.EQ.55 .OR. CODDES.EQ.45 .OR.
     &           CODDES.EQ.75 .OR. CODDES.EQ.85) THEN
          CODGRA=6
        ELSE IF (CODDES.EQ.96 .OR. CODDES.EQ.66 .OR.
     &           CODDES.EQ.46 .OR. CODDES.EQ.56 .OR.
     &           CODDES.EQ.76) THEN
          CODGRA=7
        ELSE IF (CODDES.EQ.101) THEN
          CODGRA=8
        ELSE IF (CODDES.EQ.102) THEN
          CODGRA=9
        ELSE IF (CODDES.EQ.103) THEN
          CODGRA=10
        ELSE IF (CODDES.EQ.104) THEN
          CODGRA=11
        ELSE IF (CODDES.EQ.105) THEN
          CODGRA=12
        ELSE IF (CODDES.EQ.106) THEN
          CODGRA=13
        ELSE IF (CODDES.EQ.111) THEN
          CODGRA=14
        ELSE IF (CODDES.EQ.118) THEN
          CODGRA=15
        ELSE IF (CODDES.EQ.112) THEN
          CODGRA=16
        ELSE IF (CODDES.EQ.113) THEN
          CODGRA=17
        ELSE IF (CODDES.EQ.114) THEN
          CODGRA=18
        ELSE IF (CODDES.EQ.115) THEN
          CODGRA=19
        ELSE IF (CODDES.EQ.116) THEN
          CODGRA=20
        ELSE IF (CODDES.EQ.117) THEN
          CODGRA=21
        ELSE IF (CODDES.EQ.136 .OR. CODDES.EQ.137) THEN
          CODGRA=29
        ELSE IF (CODDES.EQ.138 .OR. CODDES.EQ.139) THEN
          CODGRA=30
        ELSE IF (CODDES.EQ.141) THEN
          CODGRA=31
        ELSE IF (CODDES.EQ.142) THEN
          CODGRA=32
        ELSE IF (CODDES.EQ.161) THEN
          CODGRA=33
        ELSE IF (CODDES.EQ.121) THEN
          CODGRA=34
        ELSE IF (CODDES.EQ.172) THEN
          CODGRA=35
        ENDIF
C
      END

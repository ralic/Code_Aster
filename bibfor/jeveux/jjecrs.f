      SUBROUTINE JJECRS ( IADMI, IADYN, ICLAS, IDOS, IDCO, CUS, IMARQ )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C RESPONSABLE LEFEBVRE J-P.LEFEBVRE
C MODIF JEVEUX  DATE 03/09/2012   AUTEUR LEFEBVRE J-P.LEFEBVRE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_18 CRS_508 CRS_505
      IMPLICIT NONE
      INTEGER             IADMI , ICLAS , IDOS , IDCO ,       IMARQ(2)
      CHARACTER*(*)                                     CUS
C ----------------------------------------------------------------------
C ACTUALISE LES ENTIERS ENCADRANT UN SEGMENT DE VALEURS
C
C IN  IADMI  : ADRESSE DU PREMIER MOT DU SEGMENT DE VALEUR
C IN  IADYN  : ADRESSE DYNAMIQUE (SI DIFFERENTE DE 0)
C IN  ICLAS  : CLASSE DE L'OBJET JEVEUX
C IN  IDOS   : IDENTIFICATEUR D'OBJET SIMPLE OU D'OBJET DE COLLECTION
C IN  IDCO   : IDENTIFICATEUR DE COLLECTION
C IN  CUS    : USAGE DU SEGMENT DE VALEUR EN ACCES U
C OUT IMARQ  : IMAR(1) MARQUE AFFECTEE AU SEGMENT DE VALEUR ASSOCIE
C              IMAR(2) ADRESSE DE L'OBJET DANS LE DESCRIPTEUR DESMA
C ----------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
C ----------------------------------------------------------------------
      INTEGER          LBIS , LOIS , LOLS , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOR8 , LOC8
      INTEGER          ISTAT
      COMMON /ISTAJE/  ISTAT(4)
C ---                  ISTAT(1)->X , (2)->U , (3)->A , (4)->D
      INTEGER          IPGC,KDESMA(2),LGD,LGDUTI,KPOSMA(2),LGP,LGPUTI
      COMMON /IADMJE/  IPGC,KDESMA,   LGD,LGDUTI,KPOSMA,   LGP,LGPUTI
      REAL *8          SVUSE,SMXUSE
      COMMON /STATJE/  SVUSE,SMXUSE
C ----------------------------------------------------------------------
      INTEGER          ISTA1,ISTA2,IS,KTEMPO(2)
C DEB ------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER IADMA ,IADRS ,IADYN ,K ,LSI 
C-----------------------------------------------------------------------
      ISTA1 = ISZON(JISZON+IADMI-1)
      IS    = JISZON+ISZON(JISZON+IADMI-4)
      ISTA2 = ISZON(IS-4)
C
C --- ACCES EN ECRITURE : ON PASSE A UD
C
      IF ( CUS .EQ. 'E' ) THEN
        IF ( ISTA1 .EQ. ISTAT(1) ) THEN
          ISZON(JISZON+IADMI-1) = ISTAT(2)
          ISZON(JISZON+IADMI-2) = IDOS
          ISZON(IS-3)   = IDCO
          ISZON(IS-2)   = ICLAS
          SVUSE = SVUSE + (ISZON(JISZON+IADMI-4) - IADMI + 4)
          SMXUSE = MAX(SMXUSE,SVUSE)
        ENDIF
        ISZON(IS-4) = ISTAT(4)
C
C --- ACCES EN LECTURE : 1/ XD ET UD PASSENT A UD
C                        2/ XX ET XA PASSENT A UA
C
      ELSE IF ( CUS .EQ. 'L' ) THEN
        IF ( ISTA1 .EQ. ISTAT(1) ) THEN
          SVUSE = SVUSE + (ISZON(JISZON+IADMI-4) - IADMI + 4)
          SMXUSE = MAX(SMXUSE,SVUSE)
        ENDIF
        IF ( ISTA2 .EQ. ISTAT(4) ) THEN
          ISZON(JISZON+IADMI-1) = ISTAT(2)
        ELSE
          ISZON(JISZON+IADMI-1) = ISTAT(2)
          ISZON(JISZON+IADMI-2) = IDOS
          ISZON(IS-4)   = ISTAT(3)
          ISZON(IS-3)   = IDCO
          ISZON(IS-2)   = ICLAS
        ENDIF
      ENDIF
      IF (ISTA1 .EQ. ISTAT(1) ) THEN
        IMARQ(1) = IPGC
        IF (IPGC .GT. 0 ) THEN
          IF ( LGDUTI .EQ. LGD ) THEN
C
C ------- AGRANDISSEMENT DE L'OBJET CONTENANT LES ADRESSES
C
            LSI = LGD
            LGD = 2*LGD
            CALL JJALLS (LGD*LOIS,0,'V','I',LOIS,'INIT',IADMA,IADRS,
     +                   KTEMPO(1) ,KTEMPO(2))
            ISZON(JISZON+KTEMPO(1)-1) = ISTAT(2)
            ISZON(JISZON+ISZON(JISZON+KTEMPO(1)-4)-4) = ISTAT(4)
            SVUSE = SVUSE + (ISZON(JISZON+KTEMPO(1)-4) - KTEMPO(1) + 4)
            SMXUSE = MAX(SMXUSE,SVUSE)
            DO 100 K=1,LSI
              ISZON(JISZON+KTEMPO(1)+K-1) = ISZON(JISZON+KDESMA(1)+K-1)
 100        CONTINUE
            CALL JJLIDY ( KDESMA(2) , KDESMA(1) )
            KDESMA(1) = KTEMPO(1)
            KDESMA(2) = KTEMPO(2)
          ENDIF
          LGDUTI = LGDUTI+1
          ISZON(JISZON + KDESMA(1) + LGDUTI - 1) = IADMI
          IMARQ(2) = LGDUTI
        ENDIF
      ELSE IF ( IPGC .EQ. -1 )  THEN
        IF ( IMARQ(1) .NE. -3 ) THEN
          IMARQ(1) = IPGC
          IF ( IMARQ(2) .GT . 0 ) THEN
            ISZON(JISZON + KDESMA(1) + IMARQ(2) - 1 ) = 0
            IMARQ(2) = 0
          ENDIF
        ENDIF
      ELSE IF ( IPGC .EQ. -3 )  THEN
        IMARQ(1) = IPGC
        IF ( IMARQ(2) .GT . 0 ) THEN
          ISZON(JISZON + KDESMA(1) + IMARQ(2) - 1 ) = 0
          IMARQ(2) = 0
        ENDIF
      ENDIF
C FIN ------------------------------------------------------------------
      END

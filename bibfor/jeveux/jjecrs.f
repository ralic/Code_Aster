      SUBROUTINE JJECRS ( IADMI , ICLAS , IDOS , IDCO , CUS , IMARQ )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 10/03/98   AUTEUR VABHHTS J.PELLET 
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
C TOLE CFT_720 CFT_726 CRP_18 CRS_508
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             IADMI , ICLAS , IDOS , IDCO ,       IMARQ(2)
      CHARACTER*(*)                                     CUS
C ----------------------------------------------------------------------
C ACTUALISE LES ENTIERS ENCADRANT UN SEGMENT DE VALEURS
C
C IN  IADMI  : ADRESSE DU PREMIER MOT DU SEGMENT DE VALEUR
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
      INTEGER          LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      INTEGER          ISTAT
      COMMON /ISTAJE/  ISTAT(4)
C ---                  ISTAT(1)->X , (2)->U , (3)->A , (4)->D
      INTEGER          IPGC, KDESMA, LGD, LGDUTI, KPOSMA, LGP, LGPUTI
      COMMON /IADMJE/  IPGC, KDESMA, LGD, LGDUTI, KPOSMA, LGP, LGPUTI
C ----------------------------------------------------------------------
      INTEGER          ISTA1,ISTA2,IS,KTEMPO
C DEB ------------------------------------------------------------------
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
        ENDIF
        ISZON(IS-4) = ISTAT(4)
C
C --- ACCES EN LECTURE : 1/ XD ET UD PASSENT A UD
C                        2/ XX, XA ET UD PASSENT A UA
C
      ELSE IF ( CUS .EQ. 'L' ) THEN
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
            CALL JJALLS (LGD*LOIS,'V','I',LOIS,'INIT',IADMA,IADRS,
     +                   KTEMPO )
            ISZON(JISZON+KTEMPO-1) = ISTAT(2)
            ISZON(JISZON+ISZON(JISZON+KTEMPO-4)-4) = ISTAT(4)
            DO 100 K=1,LSI
              ISZON(JISZON+KTEMPO+K-1) = ISZON(JISZON+KDESMA+K-1)
 100        CONTINUE
            CALL JJLIBP ( KDESMA )
            KDESMA = KTEMPO
          ENDIF
          LGDUTI = LGDUTI+1
          ISZON(JISZON + KDESMA + LGDUTI - 1) = IADMI
          IMARQ(2) = LGDUTI
        ENDIF
      ELSE IF ( IPGC .EQ. -1 )  THEN
        IF ( IMARQ(1) .NE. -3 ) THEN
          IMARQ(1) = IPGC
          IF ( IMARQ(2) .GT . 0 ) THEN
            ISZON(JISZON + KDESMA + IMARQ(2) - 1 ) = 0
            IMARQ(2) = 0
          ENDIF
        ENDIF
      ELSE IF ( IPGC .EQ. -3 )  THEN
        IMARQ(1) = IPGC
        IF ( IMARQ(2) .GT . 0 ) THEN
          ISZON(JISZON + KDESMA + IMARQ(2) - 1 ) = 0
          IMARQ(2) = 0
        ENDIF
      ENDIF
C FIN ------------------------------------------------------------------
      END

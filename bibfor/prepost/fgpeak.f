      SUBROUTINE FGPEAK(NOMFON,PSEUIL,COEMUL,NBPOIN,VALPOI)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'jeveux.h'
      CHARACTER*(*)     NOMFON
      REAL*8                   PSEUIL,       VALPOI(*), COEMUL
      INTEGER                         NBPOIN
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C     -----------------------------------------------------------------
C     EXTRACTION DES PICS D'UNE FONCTION
C     ------------------------------------------------------------------
C IN  NOMFOM : K8  : NOM DE LA FONCTION
C IN  PSEUIL : R   : SEUIL DE DETECTION DES PICS
C OUT NBPOIN : I   : NOMBRE DE PICS DETECTES
C OUT VALPOI : R   : TABLEAU DES VALEURS DES PICS
C     ------------------------------------------------------------------
C
      CHARACTER*8  K8BID
      CHARACTER*32 FVALE
      INTEGER      IFONC,PASS,SORTIE
      REAL*8       MAX,MIN,VALEUR
C
      CALL JEMARQ()
C
      FVALE = NOMFON//'           .VALE       '
      CALL JELIRA(FVALE,'LONMAX',NBPTS,K8BID)
      CALL JEVEUO(FVALE,'L',IFONC)
C
C     -------  LE PREMIER POINT EST UN PIC ------
C
      NBPOIN    = 1
      VALPOI(NBPOIN) = ZR(IFONC+NBPTS/2+1-1)*COEMUL
      MAX            = VALPOI (NBPOIN)
      MIN            = VALPOI (NBPOIN)
      PASS   = 0
      SORTIE = 2
C
C     -------  RECHERCHE DES PICS INTERMEDIAIRES  -----
C
      DO 10 I=2,NBPTS/2
        VALEUR = ZR(IFONC+NBPTS/2+I-1)*COEMUL
        IF(MAX.LT.VALEUR) THEN
          MAX = VALEUR
        ENDIF
        IF(MIN.GT.VALEUR) THEN
          MIN = VALEUR
        ENDIF
        IF (PASS.EQ.0) THEN
          IF((VALEUR-MIN).GT.PSEUIL) THEN
            SORTIE = 1
            PASS   = 1
          ENDIF
          IF((MAX-VALEUR).GT.PSEUIL) THEN
            SORTIE = 0
            PASS   = 1
          ENDIF
        ENDIF
        IF((SORTIE.EQ.1).AND.(MAX-VALEUR).GT.PSEUIL) THEN
            NBPOIN = NBPOIN + 1
            VALPOI(NBPOIN) = MAX
            MIN = VALEUR
            SORTIE = 0
        ENDIF
        IF((SORTIE.EQ.0).AND.(VALEUR-MIN).GT.PSEUIL) THEN
            NBPOIN = NBPOIN + 1
            VALPOI(NBPOIN) = MIN
            MAX = VALEUR
            SORTIE = 1
        ENDIF
 10   CONTINUE
C
      IF(SORTIE.EQ.0) THEN
        NBPOIN = NBPOIN + 1
        VALPOI (NBPOIN) = MIN
      ENDIF
      IF(SORTIE.EQ.1) THEN
        NBPOIN = NBPOIN + 1
        VALPOI (NBPOIN) = MAX
      ENDIF
C
      CALL JEDEMA()
      END

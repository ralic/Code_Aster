      SUBROUTINE NMEXT3(NOMA  ,CHAMP ,NOMCHA,NOMCHS,NBCMP ,
     &                  NBMA  ,NBPI  ,NBSPI ,EXTRGA,EXTRCH,
     &                  EXTRCP,LISTMA,LISTPI,LISTSP,LISTCP,
     &                  CHGAUS,CHELGA)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT      NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM
      INTEGER       NBCMP,NBMA,NBPI,NBSPI
      CHARACTER*8   NOMA
      CHARACTER*24  NOMCHA,NOMCHS
      CHARACTER*8   EXTRCP,EXTRCH,EXTRGA
      CHARACTER*24  LISTMA,LISTPI,LISTSP,LISTCP
      CHARACTER*19  CHGAUS,CHELGA
      CHARACTER*19  CHAMP
C
C ----------------------------------------------------------------------
C
C ROUTINE *_NON_LINE (EXTRACTION - UTILITAIRE)
C
C EXTRAIRE LES VALEURS - CAS DES CHAMPS AUX POINTS DE GAUSS
C
C ----------------------------------------------------------------------
C
C
C IN  NOMA   : NOM DU MAILLAGE
C IN  CHAMP  : CHAMP EXTRACTE
C IN  NOMCHA : NOM DU CHAMP
C IN  NOMCHS : NOM DU CHAMP SIMPLE
C IN  NBCMP  : NOMBRE DE COMPOSANTES DANS LA SD
C IN  NBMA   : NOMBRE DE MAILLES DANS LA SD
C IN  NBPI   : NOMBRE DE POINTS D'INTEGRATION
C IN  NBSPI  : NOMBRE DE SOUS-POINTS D'INTEGRATION
C IN  EXTRGA : TYPE D'EXTRACTION SUR UNE MAILLE
C IN  EXTRCH : TYPE D'EXTRACTION SUR LE CHAMP
C IN  EXTRCP : TYPE D'EXTRACTION SUR LES COMPOSANTES
C IN  LISTMA : LISTE CONTENANT LES MAILLES
C IN  LISTCP : LISTE DES COMPOSANTES
C IN  LISTPI : LISTE CONTENANT LES POINTS D'EXTRACTION
C IN  LISTSP : LISTE CONTENANT LES SOUS-POINTS D'EXTRACTION
C IN  CHELGA : VECTEUR DE TRAVAIL CHAMPS AUX ELEMENTS
C IN  CHGAUS : VECTEUR DE TRAVAIL CHAMPS AUX POINTS DE GAUSS
C IN  CHAMP  : CHAMP A EXTRAIRE
C
C ----------------------------------------------------------------------
C
      INTEGER      NPARX
      PARAMETER    (NPARX=20)
      REAL*8       VALRES(NPARX)
C
      INTEGER      JGAUS,JELGA
      INTEGER      JMA,JPI,JSPI
      INTEGER      IMA,IPI,ISPI,IMAR,IPIR,ISPIR,NUMMAI
      INTEGER      NUM,SNUM,IRET
      INTEGER      NMAPT,NMASPT,NPI,NSPI
      CHARACTER*8  NOMMAI
      INTEGER      IVALCP,NVALCP
      REAL*8       VALR,VAL2R
      INTEGER      JCESD,JCESL,JCESV,JCESC
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- CONVERSION EN CHAM_ELEM_S
C
      CALL JEEXIN(NOMCHS,IRET)
      IF (IRET.EQ.0) THEN
        CALL SDMPIC('CHAM_ELEM',CHAMP )
        CALL CELCES(CHAMP ,'V',NOMCHS)
      ENDIF
C
C --- ACCES AUX CHAMPS DE TRAVAIL
C
      CALL JEVEUO(CHGAUS,'E',JGAUS)
      CALL JEVEUO(CHELGA,'E',JELGA)
      CALL JEVEUO(NOMCHS(1:19)//'.CESD','L',JCESD)
      CALL JEVEUO(NOMCHS(1:19)//'.CESL','L',JCESL)
      CALL JEVEUO(NOMCHS(1:19)//'.CESV','L',JCESV)
      CALL JEVEUO(NOMCHS(1:19)//'.CESC','L',JCESC)
      CALL ASSERT(NBCMP.LE.NPARX)
C
C --- ACCES LISTE DES MAILLES/POINTS/SOUS_POINTS
C
      CALL JEVEUO(LISTMA,'L',JMA)
      CALL JEVEUO(LISTPI,'L',JPI)
      CALL JEVEUO(LISTSP,'L',JSPI)
C
C --- BOUCLE SUR LES MAILLES
C
      DO 30 IMA = 1,NBMA
C
C ----- MAILLE COURANTE
C
        NUMMAI  = ZI(JMA-1+IMA)
        CALL JENUNO(JEXNUM(NOMA(1:8)//'.NOMMAI',NUMMAI),NOMMAI)
C
C ----- NOMBRE EFFECTIF DE POINTS/SOUS-POINTS SUR LA MAILLE
C
        NMAPT  = ZI(JCESD+5+4*(NUMMAI-1)+1-1)
        NMASPT = ZI(JCESD+5+4*(NUMMAI-1)+2-1)
C
C ----- PLAFONNEMENT
C
        NPI    = NBPI
        NSPI   = NBSPI
        IF (NPI.GT.NMAPT)   NPI  = NMAPT
        IF (NSPI.GT.NMASPT) NSPI = NMASPT
C
C ----- CALCUL DES COMPOSANTES SUR LES POINTS/SOUS_POINTS
C
        DO 45 IPI = 1,NPI
          DO 46 ISPI = 1,NSPI
C
C --------- NUMERO DES POINTS/SOUS-POINTS
C
            NUM    = ZI(JPI-1+IPI  )
            SNUM   = ZI(JSPI-1+ISPI)
C
C --------- EXTRACTION DES VALEURS AUX POINTS DE GAUSS
C
            CALL NMEXTJ(NOMCHA,NBCMP ,LISTCP,EXTRCP,NUM   ,
     &                  SNUM  ,NVALCP,NUMMAI,JCESD ,JCESV ,
     &                  JCESL ,JCESC ,VALRES)
C
C --------- INDICE D'ACCES
C
            IF (EXTRGA.EQ.'VALE') THEN
              IPIR   = IPI
              ISPIR  = ISPI
            ELSE
              IPIR   = 1
              ISPIR  = 1
            ENDIF
C
C --------- CALCUL DES VALEURS
C
            DO 47 IVALCP = 1,NVALCP
              VALR   = VALRES(IVALCP)
              VAL2R  = ZR(JGAUS+NBCMP*(IVALCP-1)
     &                         +NBPI*(IPIR-1)
     &                         +NBSPI*(ISPIR-1))
              IF (EXTRGA.EQ.'VALE') THEN
                ZR(JGAUS+NBPI*NBSPI*(IVALCP-1)
     &                  +NBSPI*(IPIR-1)
     &                  +(ISPIR-1)) = VALR
              ELSEIF (EXTRGA.EQ.'MAX') THEN
                ZR(JGAUS+NBPI*NBSPI*(IVALCP-1)
     &                  +NBSPI*(IPIR-1)
     &                  +(ISPIR-1)) = MAX(VALR,VAL2R)
              ELSEIF (EXTRGA.EQ.'MIN') THEN
                ZR(JGAUS+NBPI*NBSPI*(IVALCP-1)
     &                  +NBSPI*(IPIR-1)
     &                  +(ISPIR-1)) = MIN(VALR,VAL2R)
              ELSEIF (EXTRGA.EQ.'MOY') THEN
                ZR(JGAUS+NBPI*NBSPI*(IVALCP-1)
     &                  +NBSPI*(IPIR-1)
     &                  +(ISPIR-1)) = VALR+VAL2R
              ELSE
                CALL ASSERT(.FALSE.)
              ENDIF
  47        CONTINUE
  46      CONTINUE
  45    CONTINUE
C
C ----- AFFECTATION DES VALEURS AUX MAILLES
C
        DO 75 IPI = 1,NPI
          DO 76 ISPI = 1,NSPI
            IF (EXTRGA.EQ.'VALE') THEN
              IPIR   = IPI
              ISPIR  = ISPI
            ELSE
              IPIR   = 1
              ISPIR  = 1
            ENDIF
C
            DO 77 IVALCP = 1,NVALCP
              IF (EXTRCH.EQ.'VALE') THEN
                IMAR = IMA
              ELSE
                IMAR = 1
              ENDIF
              VALR   = ZR(JGAUS+NBPI*NBSPI*(IVALCP-1)
     &                  +NBSPI*(IPIR-1)
     &                  +(ISPIR-1))
              VAL2R  = ZR(JELGA+NBCMP*NBPI*NBSPI*(IMAR-1)
     &                       +NBPI*NBSPI*(IVALCP-1)
     &                       +NBSPI*(IPIR-1)
     &                       +(ISPIR-1))
              IF (EXTRCH.EQ.'VALE') THEN
                ZR(JELGA+NBCMP*NBPI*NBSPI*(IMAR-1)
     &                       +NBPI*NBSPI*(IVALCP-1)
     &                       +NBSPI*(IPIR-1)
     &                       +(ISPIR-1)) =
     &          VALR
              ELSEIF (EXTRCH.EQ.'MAX') THEN
                ZR(JELGA+NBCMP*NBPI*NBSPI*(IMAR-1)
     &                       +NBPI*NBSPI*(IVALCP-1)
     &                       +NBSPI*(IPIR-1)
     &                       +(ISPIR-1)) =
     &          MAX(VALR,VAL2R)
              ELSEIF (EXTRCH.EQ.'MIN') THEN
                ZR(JELGA+NBCMP*NBPI*NBSPI*(IMAR-1)
     &                       +NBPI*NBSPI*(IVALCP-1)
     &                       +NBSPI*(IPIR-1)
     &                       +(ISPIR-1)) =
     &          MIN(VALR,VAL2R)
              ELSEIF (EXTRCH.EQ.'MAXI_ABS') THEN
                ZR(JELGA+NBCMP*NBPI*NBSPI*(IMAR-1)
     &                       +NBPI*NBSPI*(IVALCP-1)
     &                       +NBSPI*(IPIR-1)
     &                       +(ISPIR-1)) =
     &          MAX(ABS(VAL2R),ABS(VALR))
              ELSEIF (EXTRCH.EQ.'MINI_ABS') THEN
                ZR(JELGA+NBCMP*NBPI*NBSPI*(IMAR-1)
     &                       +NBPI*NBSPI*(IVALCP-1)
     &                       +NBSPI*(IPIR-1)
     &                       +(ISPIR-1)) =
     &          MIN(ABS(VAL2R),ABS(VALR))
              ELSEIF (EXTRCH.EQ.'MOY') THEN
                ZR(JELGA+NBCMP*NBPI*NBSPI*(IMAR-1)
     &                       +NBPI*NBSPI*(IVALCP-1)
     &                       +NBSPI*(IPIR-1)
     &                       +(ISPIR-1)) =
     &          VALR+VAL2R
              ELSE
                CALL ASSERT(.FALSE.)
              ENDIF
  77        CONTINUE
  76      CONTINUE
  75    CONTINUE
  30  CONTINUE
C
C --- CALCUL DE LA MOYENNE
C
      IF (EXTRCH.EQ.'MOY') THEN
        IMAR = 1
        DO 55 IPI = 1,NPI
          DO 56 ISPI = 1,NSPI
            DO 57 IVALCP = 1,NVALCP
              VAL2R  = ZR(JELGA+NBCMP*NBPI*NBSPI*(IMAR-1)
     &                       +NBPI*NBSPI*(IVALCP-1)
     &                       +NBSPI*(IPIR-1)
     &                       +(ISPIR-1))
              ZR(JELGA+NBCMP*NBPI*NBSPI*(IMAR-1)
     &                       +NBPI*NBSPI*(IVALCP-1)
     &                       +NBSPI*(IPIR-1)
     &                       +(ISPIR-1)) =
     &        VAL2R/ NBMA
 57         CONTINUE
 56       CONTINUE
 55     CONTINUE
      ENDIF
C
      CALL JEDEMA()
C
      END

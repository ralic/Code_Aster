      SUBROUTINE RCFODE(IFON,TEMP,F,DF)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
      INTEGER           IFON
      REAL*8                 TEMP,F,DF
C ......................................................................
C     OBTENTION DE LA VALEUR DE LA FONCTION ET DE SA DERIVEE POUR UNE
C     FONCTION DE LA TEMPERATURE LINEAIRE PAR MORCEAU
C IN   IFON   : ADRESSE DANS LE MATERIAU CODE DE LA FONCTION
C IN   TEMP   : TEMPERATURE AU POINT DE GAUSS CONSIDERE
C OUT  F      : VALEUR DE LA FONCTION
C OUT  DF     : VALEUR DE LA DERIVEE DE LA FONCTION
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER            JPRO,JVALF,JV,JP,NBVF
      LOGICAL            TESINF,TESSUP,ENTRE,DEJA,AVANT
      CHARACTER*8        K8BID
C ----------------------------------------------------------------------
C PARAMETER ASSOCIE AU MATERIAU CODE
C
      PARAMETER  ( INDFCT = 7 )
C DEB ------------------------------------------------------------------
      JPRO  = ZI(IFON+1)
      JVALF = ZI(IFON+2)
      IF (ZK16(JPRO)(1:1).EQ.'C') THEN
        F  = ZR(JVALF+1)
        DF = 0.D0
        GOTO 101
      ENDIF
      ISAVE = ZI(IFON+INDFCT)
      IF (ZK16(JPRO)(1:1).EQ.'N') THEN
C
C---- NAPPE - IMPOSSIBLE
C
        CALL U2MESS('F','MODELISA6_58')
      ENDIF
      NBVF = ZI(IFON)
C
      DEJA   = TEMP.GE.ZR(JVALF+ISAVE-1) .AND. TEMP.LE.ZR(JVALF+ISAVE)
      AVANT  = TEMP.LT.ZR(JVALF+ISAVE-1)
      TESINF = TEMP.LT.ZR(JVALF)
      TESSUP = TEMP.GT.ZR(JVALF+NBVF-1)
      ENTRE  = .NOT. TESINF .AND. .NOT. TESSUP
      IF (DEJA) THEN
        JP = JVALF + ISAVE
        JV = JP    + NBVF
        DF = (ZR(JV)-ZR(JV-1))/(ZR(JP)-ZR(JP-1))
        F = DF*(TEMP-ZR(JP-1))+ZR(JV-1)
        GOTO 100
      ELSE
        IF ( AVANT ) THEN
          IFIN = JVALF
          IDEB = JVALF+ISAVE-1
          INCR = -1
        ELSE
          IDEB = JVALF+ISAVE
          IFIN = JVALF+NBVF-1
          INCR = 1
        ENDIF
      ENDIF
      IF (ENTRE) THEN
        IF ( INCR .GT. 0 ) THEN
          DO 8 JP=IDEB,IFIN,INCR
            JV = JP + NBVF
            IF (ZR(JP).GE.TEMP) THEN
              DF = (ZR(JV)-ZR(JV-1))/(ZR(JP)-ZR(JP-1))
               F = DF*(TEMP-ZR(JP-1))+ZR(JV-1)
              ISAVE = JP-JVALF
              GOTO 5
            END IF
 8        CONTINUE
 5        CONTINUE
        ELSE
          DO 9 JP=IDEB,IFIN,INCR
            JV = JP + NBVF
            IF (ZR(JP).LE.TEMP) THEN
              DF = (ZR(JV+1)-ZR(JV))/(ZR(JP+1)-ZR(JP))
               F = DF*(TEMP-ZR(JP))+ZR(JV)
              ISAVE = JP-JVALF+1
              GOTO 6
            END IF
 9        CONTINUE
 6        CONTINUE
        ENDIF
      ELSE IF (TESINF) THEN
        JV = JVALF+NBVF
        JP = JVALF
        IF (ZK16(JPRO+4)(2:2).EQ.'C') THEN
          DF = 0.0D0
           F = ZR(JV)
        ELSE IF (ZK16(JPRO+4)(1:1).EQ.'L') THEN
          DF = (ZR(JV+1)-ZR(JV))/(ZR(JP+1)-ZR(JP))
           F = DF*(TEMP-ZR(JP))+ZR(JV)
        ELSE IF (ZK16(JPRO+4)(1:1).EQ.'E') THEN
          CALL UTDEBM('F','RCFODE','EVALUATION IMPOSSIBLE '//
     &         'D UNE FONCTION MATERIAU - ON DEBORDE A GAUCHE '//
     &         'POUR LA TEMPERATURE ')
          CALL UTIMPR('S','TEMP :',1,TEMP)
          CALL UTFINM()
        END IF
        ISAVE = 1
      ELSE IF (TESSUP) THEN
        JV = JVALF + 2*NBVF - 1
        JP = JVALF + NBVF - 1
        IF (ZK16(JPRO+4)(2:2).EQ.'C') THEN
          DF = 0.0D0
           F = ZR(JV)
        ELSE IF (ZK16(JPRO+4)(2:2).EQ.'L') THEN
          DF = (ZR(JV)-ZR(JV-1))/(ZR(JP)-ZR(JP-1))
           F = DF*(TEMP-ZR(JP-1))+ZR(JV-1)
        ELSE IF (ZK16(JPRO+4)(2:2).EQ.'E') THEN
          CALL UTDEBM('F','RCFODE','EVALUATION IMPOSSIBLE '//
     &         'D UNE FONCTION MATERIAU - ON DEBORDE A DROITE '//
     &         'POUR LA TEMPERATURE ')
          CALL UTIMPR('S','TEMP :',1,TEMP)
          CALL UTFINM()
        END IF
        ISAVE = NBVF - 1
      END IF
 100  CONTINUE
      ZI(IFON+INDFCT) = ISAVE
 101  CONTINUE
C FIN ------------------------------------------------------------------
      END

      SUBROUTINE FODERI ( NOMFON, TEMP, F, DF )
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*(*)       NOMFON
      REAL*8                      TEMP, F, DF
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C ......................................................................
C     OBTENTION DE LA VALEUR DE LA FONCTION ET DE SA DERIVEE POUR UNE
C     FONCTION LINEAIRE PAR MORCEAU
C IN   NOMFON : NOM DE LA FONCTION
C IN   TEMP   : TEMPERATURE AU POINT DE GAUSS CONSIDERE
C OUT  F      : VALEUR DE LA FONCTION
C OUT  DF     : VALEUR DE LA DERIVEE DE LA FONCTION
C ......................................................................
C
C
C
C
C     ------------------------------------------------------------------
      INTEGER      MXSAVE, ISVNXT, NEXTSV
      INTEGER      IAPROL, IAVALE, LUVALE
      CHARACTER*2  SVPRGD
      CHARACTER*8  SVNOMF
      COMMON /IFDSAV/ MXSAVE, ISVNXT, NEXTSV(5)
      COMMON /JFDSAV/ IAPROL(5),IAVALE(5),LUVALE(5)
      COMMON /KFDSAV/ SVNOMF(5), SVPRGD(5)
C     ------------------------------------------------------------------
      LOGICAL            TESINF, TESSUP
      INTEGER            ISAVE, KK, JPRO, JVALF, JV, JP, NBVF
      CHARACTER*8        K8BID
      CHARACTER*19       CH19
      CHARACTER*24       CHPRO,CHVAL
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      DO 100 KK = 1 , MXSAVE
        IF ( NOMFON(1:8) .EQ. SVNOMF(KK) ) THEN
          ISAVE = KK
          JPRO = IAPROL(ISAVE)
          JVALF= IAVALE(ISAVE)
          NBVF = LUVALE(ISAVE)
          GOTO 101
        ENDIF
 100  CONTINUE
C
      CH19  = NOMFON(1:8)
      CHPRO = CH19//'.PROL'
      CHVAL = CH19//'.VALE'
      CALL JEVEUT(CHPRO,'L',JPRO)
      IF (ZK24(JPRO)(1:1).EQ.'I') THEN
C
C --- FONCTION INTERPRETEE NON-UTILISABLE
C
        CALL U2MESS('F','MODELISA4_61')
      ELSE IF (ZK24(JPRO)(1:1).EQ.'N') THEN
C
C --- NAPPE - IMPOSSIBLE
C
        CALL U2MESS('F','MODELISA4_62')
      ENDIF
C
      CALL JEVEUT(CHVAL,'L',JVALF)
      CALL JELIRA(CHVAL,'LONMAX',NBVF,K8BID)
      NBVF = NBVF/2
C
      ISVNXT = NEXTSV(ISVNXT)
      ISAVE  = ISVNXT
      IAPROL(ISAVE) = JPRO
      IAVALE(ISAVE) = JVALF
      LUVALE(ISAVE) = NBVF
      SVNOMF(ISAVE) = NOMFON(1:8)
      SVPRGD(ISAVE) = ZK24(JPRO+4)(1:2)

 101  CONTINUE
C
      TESINF = TEMP.LT.ZR(JVALF)
      TESSUP = TEMP.GT.ZR(JVALF+NBVF-1)

      IF (TESINF) THEN
        JV = JVALF+NBVF
        JP = JVALF
        IF (SVPRGD(ISAVE)(1:1).EQ.'C') THEN
          DF = 0.0D0
           F = ZR(JV)
        ELSE IF (SVPRGD(ISAVE)(1:1).EQ.'L') THEN
          DF = (ZR(JV+1)-ZR(JV))/(ZR(JP+1)-ZR(JP))
           F = DF*(TEMP-ZR(JP))+ZR(JV)
        ELSE IF (SVPRGD(ISAVE)(1:1).EQ.'E') THEN
          CALL U2MESS('F','MODELISA4_63')
        ELSE
          CALL U2MESS('F','MODELISA4_64')
        END IF

      ELSE IF (TESSUP) THEN
        JV = JVALF + 2*NBVF - 1
        JP = JVALF + NBVF - 1
        IF (SVPRGD(ISAVE)(2:2).EQ.'C') THEN
          DF = 0.0D0
           F = ZR(JV)
        ELSE IF (SVPRGD(ISAVE)(2:2).EQ.'L') THEN
          DF = (ZR(JV)-ZR(JV-1))/(ZR(JP)-ZR(JP-1))
           F = DF*(TEMP-ZR(JP-1))+ZR(JV-1)
        ELSE IF (SVPRGD(ISAVE)(2:2).EQ.'E') THEN
          CALL U2MESS('F','MODELISA4_65')
        ELSE
          CALL U2MESS('F','MODELISA4_66')
        END IF

      ELSE
        DO 8 JP=JVALF+1,JVALF+NBVF-1
          JV = JP + NBVF
          IF (ZR(JP).GE.TEMP) THEN
            DF = (ZR(JV)-ZR(JV-1))/(ZR(JP)-ZR(JP-1))
             F = DF*(TEMP-ZR(JP-1))+ZR(JV-1)
            GOTO 5
          END IF
 8      CONTINUE
        CALL U2MESS('F','MODELISA4_67')
 5      CONTINUE

      END IF
C FIN ------------------------------------------------------------------
      CALL JEDEMA()

      END

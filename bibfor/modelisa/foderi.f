      SUBROUTINE FODERI(NOMFON,TEMP,F,DF)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 27/03/2002   AUTEUR VABHHTS J.PELLET 
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
C
      CHARACTER*(*)     NOMFON
      REAL*8                   TEMP,F,DF
C ......................................................................
C     OBTENTION DE LA VALEUR DE LA FONCTION ET DE SA DERIVEE POUR UNE
C     FONCTION LINEAIRE PAR MORCEAU
C IN   NOMFON : NOM DE LA FONCTION
C IN   TEMP   : TEMPERATURE AU POINT DE GAUSS CONSIDERE
C OUT  F      : VALEUR DE LA FONCTION
C OUT  DF     : VALEUR DE LA DERIVEE DE LA FONCTION
C ......................................................................
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      CHARACTER*32       JEXNUM , JEXNOM
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
      LOGICAL            TESINF,TESSUP
      INTEGER            JPRO,JVALF,JV,JP,NBVF
      PARAMETER        ( NB = 5 )
      INTEGER            JPROS(NB),JVALFS(NB),NBVFS(NB),NEXT(NB)
      CHARACTER*8        K8BID,FOPREV(NB)
      CHARACTER*19       CH19
      CHARACTER*24       CHPRO,CHVAL
      SAVE               FOPREV,JPROS,JVALFS,NBVFS,ISAVE
      DATA               FOPREV,ISAVE/NB*'     ',NB/
      DATA               NEXT/2,3,4,5,1/
C

      CALL JEMARQ()
      DO 100 KK=1,NB
        IF ( NOMFON(1:8) .EQ. FOPREV(KK) ) THEN
          ISAVE = KK
          JPRO = JPROS(KK)
          JVALF= JVALFS(KK)
          NBVF = NBVFS(KK)
          GOTO 101
        ENDIF
 100  CONTINUE
C
      CH19 = NOMFON(1:8)
      CHPRO=CH19//'.PROL'
      CHVAL=CH19//'.VALE'
      CALL JEVEUS(CHPRO,'L',JPRO)
      IF (ZK8(JPRO)(1:1).EQ.'I') THEN
C
C --- FONCTION INTERPRETEE NON-UTILISABLE
C
        CALL UTMESS ('F','FODERI_01','LES FONCTIONS INTERPRETEES '
     &             //'DOIVENT ETRE TABULEES AUPARAVANT ')
      ELSE IF (ZK8(JPRO)(1:1).EQ.'N') THEN
C
C --- NAPPE - IMPOSSIBLE
C
        CALL UTMESS ('F','FODERI_02','NAPPE INTERDITE POUR'
     &             //' DEFINIR LE FLUX')
      ENDIF
      CALL JEVEUS(CHVAL,'L',JVALF)
      CALL JELIRA(CHVAL,'LONMAX',NBVF,K8BID)
      NBVF = NBVF/2
      ISAVE = NEXT(ISAVE)
      FOPREV(ISAVE) = NOMFON(1:8)
      NBVFS(ISAVE)  = NBVF
      JVALFS(ISAVE) = JVALF
      JPROS(ISAVE)  = JPRO
 101  CONTINUE
C
      TESINF = TEMP.LT.ZR(JVALF)
      TESSUP = TEMP.GT.ZR(JVALF+NBVF-1)
      IF (TESINF) THEN
        JV = JVALF+NBVF
        JP = JVALF
        IF (ZK8(JPRO+4)(2:2).EQ.'C') THEN
          DF = 0.0D0
           F = ZR(JV)
        ELSE IF (ZK8(JPRO+4)(1:1).EQ.'L') THEN
          DF = (ZR(JV+1)-ZR(JV))/(ZR(JP+1)-ZR(JP))
           F = DF*(TEMP-ZR(JP))+ZR(JV)
        ELSE IF (ZK8(JPRO+4)(1:1).EQ.'E') THEN
          CALL UTMESS ('F','FODERI_03',' ON DEBORDE A GAUCHE')
        END IF
      ELSE IF (TESSUP) THEN
        JV = JVALF + 2*NBVF - 1
        JP = JVALF + NBVF - 1
        IF (ZK8(JPRO+4)(2:2).EQ.'C') THEN
          DF = 0.0D0
           F = ZR(JV)
        ELSE IF (ZK8(JPRO+4)(2:2).EQ.'L') THEN
          DF = (ZR(JV)-ZR(JV-1))/(ZR(JP)-ZR(JP-1))
           F = DF*(TEMP-ZR(JP-1))+ZR(JV-1)
        ELSE IF (ZK8(JPRO+4)(2:2).EQ.'E') THEN
          CALL UTMESS ('F','FODERI_04',' ON DEBORDE A DROITE')
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
        CALL UTMESS ('F','FODERI_05',' ON EST EN DEHORS DES BORNES')
 5      CONTINUE
      END IF
C FIN ------------------------------------------------------------------
      CALL JEDEMA()

      END

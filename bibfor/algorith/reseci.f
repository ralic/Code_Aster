      SUBROUTINE RESECI(CARELE,NUMMAI,AI1,AI2)
C**********************************************************************C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/05/2000   AUTEUR VABHHTS J.PELLET 
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
C***1.****RECUPERATION DU TYPGRANDEUR, DU NOMBRE MAX DE GRANDEUR
C         DU NOMBRE DE GRANDEUR EFFECTIF
C
C
C***2.****POUR CHAQUE GRANDEUR  ON RECUPER LE CODE
C         SI 1: TOUTES LES MAILLES ONT LA MEME GRANDEUR
C         SI 3: ON A UN GROUPMA TARDIF DECRIT DANS LE LIMA : ON
C               REUPERE SON NUMERO, ON VA VOIR SI NUMMAI Y EST
C               SI NON ON PASSE A LA GRANDEUR SUIVANTE
C               SI OUI ON RECUPER SON ENTIER CODE, ON VERI......
C**********************************************************************C
      IMPLICIT REAL*8 (A-H,O-Z)
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CHARACTER*1 KBID
      CHARACTER*8 CARELE,K8BID1
      CHARACTER*24 K24BI1
      CHARACTER*32 JEXNOM,JEXNUM,JEXR8


C**********************************************************************C
C***1.      RECUPERATION DU TYPE GRANDEUR NB ASSOCIATION***************C
C**********************************************************************C
      CALL JEMARQ()
      K24BI1 = CARELE//'.CARGENPO  .DESC'
      CALL JEVEUO(K24BI1,'L',IAD1)
      IGD = ZI(IAD1)
      INECGD = 1
      INASMX = ZI(IAD1+1)
C**********************************************************************C
C***        BOUCLE SUR LES ASSOCIATIONS *******************************C
C**********************************************************************C
      DO 101 IASS1 = 1,INASMX
        ICODE1 = ZI(IAD1-1+3+2* (IASS1-1)+1)
        IF (ICODE1.EQ.1) THEN
          INULIM = 1
          IEC1 = ZI(IAD1-1+3+2* (INASMX)+INECGD* (IASS1-1)+1)

        ELSE IF (ICODE1.EQ.3) THEN
          INULIM = ZI(IAD1-1+3+2* (IASS1-1)+2)
          IEC1 = ZI(IAD1-1+3+2* (INASMX)+INECGD* (IASS1-1)+1)

        ELSE
        END IF
C   ON REGARDE SI LA MAILLE EST DANS LE .LIMA
        CALL JEVEUO(JEXNUM(K24BI1(1:19)//'.LIMA',INULIM),'L',IALIMA)
        CALL JELIRA(JEXNUM(K24BI1(1:19)//'.LIMA',INULIM),'LONMAX',
     +              INBMAI,K8BID1)
        DO 103 IMAI1 = 1,INBMAI
          NUMAI1 = ZI(IALIMA-1+IMAI1)
          IF (NUMMAI.EQ.NUMAI1) THEN
            GO TO 104

          END IF

  103   CONTINUE
  101 CONTINUE
  104 CONTINUE
C**********************************************************************C
C***        ON RECHERCHE LE NOMBRE DE CMP DE LA GRANDEUR **************C
C**********************************************************************C
      CALL JELIRA(JEXNUM('&CATA.GD.NOMCMP',IGD),'LONMAX',IGDNCM,KBID)
C**********************************************************************C
C***        ON RECHERCHE LES NUMERO DE AI1 ET AI2 DANS LA GD***********C
C**********************************************************************C
      CALL JENONU(JEXNOM('&CATA.GD.NOMGD','CAGNPO'),NUMGD)
      CALL JEVEUO(JEXNUM('&CATA.GD.NOMCMP',NUMGD),'L',IAD1)
      DO 105 ICMP = 1,IGDNCM
        K8BID1 = ZK8(IAD1-1+ICMP)
        IF (K8BID1(1:3).EQ.'AI1') THEN
          IRGAI1 = ICMP

        ELSE IF (K8BID1(1:3).EQ.'AI2') THEN
          IRGAI2 = ICMP
        END IF

  105 CONTINUE
C**********************************************************************C
C***        ON RECUPERE LES VALEURS ***********************************C
C**********************************************************************C
      K24BI1 = CARELE//'.CARGENPO  .VALE'
      CALL JEVEUO(K24BI1,'L',IAVALE)
      IF (IRGAI1.GT.30) CALL VERI32()
      IF (IRGAI2.GT.30) CALL VERI32()
      ITEST1 = MOD((IEC1-MOD(IEC1,2**IRGAI1))/2**IRGAI1,2)
      ITEST2 = MOD((IEC1-MOD(IEC1,2**IRGAI2))/2**IRGAI2,2)
      IF ((ITEST1*ITEST2).NE.1) THEN

      ELSE
        IBID1 = (IASS1-1)*IGDNCM + IRGAI1
        AI1 = ZR(IAVALE-1+IBID1)
        IBID1 = (IASS1-1)*IGDNCM + IRGAI2
        AI2 = ZR(IAVALE-1+IBID1)
      END IF

      CALL JEDEMA()
      END

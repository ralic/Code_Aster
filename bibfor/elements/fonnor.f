      SUBROUTINE FONNOR ( RESU, NOMA)
      IMPLICIT NONE
      CHARACTER*8         RESU, NOMA
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 21/06/2011   AUTEUR MACOCCO K.MACOCCO 
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
C-----------------------------------------------------------------------
C FONCTION REALISEE:
C
C     CALCUL DE LA NORMALE AU FOND DE FISSURE POUR DEFI_FOND_FISS 
C     EN 2D DE LA NORMALE
C
C     ENTREES:
C        RESU   : NOM DU CONCEPT RESULTAT DE L'OPERATEUR
C        NOMA   : NOM DU MAILLAGE
C-----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32     JEXNOM, JEXNUM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER       IDLINO, JNOLS, JLIMA, JNOE1, JNOE2, IDCOOR
      INTEGER       JNORM,  IERA,  INOLS
      INTEGER       NBMAI,  INIV,  INO
      INTEGER       IRET,   IRE1,  IRE2,  IN
      INTEGER       NVENOR, NBNOE,  N1,   NBNOLS, IM
      INTEGER       NUMORI, NUMFIN, COMPT,INDICE, NBNO
      REAL*8        ZRBID,D
      REAL*8        X1,Y1,Z1,X2,Y2,Z2,DMAX,XSUP,YSUP,ZSUP
      REAL*8        PSUP(3),VZ(3),VNORM(6)
      CHARACTER*9   TYPLEV(2),MOTFAC, VALK(2)
      CHARACTER*6   NOMPRO
      CHARACTER*8   K8B, TYPE
      
      CHARACTER*24  OBJ3,NOMOB1,NOMOB2
C     -----------------------------------------------------------------
C
      CALL JEMARQ()
      NOMPRO = 'FONNOR'
      TYPLEV(1) = 'LEVRE_SUP'
      TYPLEV(2) = 'LEVRE_INF'
C
      CALL GETVID ( ' ', 'MAILLAGE', 1,1,1, NOMA , N1 )
      CALL JEVEUO ( NOMA//'.COORDO    .VALE', 'L', IDCOOR )
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNOE,K8B,IRET)

C     -----------------------------------------------------------------
C
C OBJETS DE MAILLAGE : OBJ1 A OBJ3

      OBJ3 = NOMA//'.NOMNOE'
C
      DO 10 INIV=1,2
        MOTFAC = TYPLEV(INIV)
        IF ( MOTFAC .EQ. 'LEVRE_SUP' ) THEN
          NOMOB1 = RESU//'.LEVRESUP  .MAIL'
          NOMOB2 = RESU//'.SUPNORM   .NOEU'
        ELSE
          NOMOB1 = RESU//'.LEVREINF  .MAIL'
          NOMOB2 = RESU//'.INFNORM   .NOEU'
        ENDIF
        CALL JELIRA(NOMOB1 , 'LONUTI', NBMAI, K8B)
        CALL JEVEUO(NOMOB1 ,'L',JNOE2)
        CALL WKVECT ( '&&'//NOMPRO//'_TRAV'  , 'V V I', NBNOE, IDLINO )
        CALL WKVECT ( '&&'//NOMPRO//'_NOEU_NORM_SUP', 
     &                                           'V V I', NBNOE, JNOLS)
        CALL WKVECT ( '&&'//NOMPRO//'_MAILLE_LEV_SUP', 
     &                                           'V V I', NBMAI, JLIMA)
        DO 601 IM = 1 , NBMAI
           CALL JENONU(JEXNOM(NOMA//'.NOMMAI',ZK8(JNOE2-1 + IM)),
     &                                               ZI(JLIMA-1 + IM) )
 601    CONTINUE
        CALL GMGNRE ( NOMA, NBNOE, ZI(IDLINO), ZI(JLIMA), NBMAI,
     &                                      ZI(JNOLS), NBNOLS, 'TOUS' )

        CALL JEEXIN(RESU//'.FOND      .NOEU',IRET)
        IF(IRET.NE.0) THEN
          CALL JELIRA(RESU//'.FOND      .NOEU' , 'LONUTI', NBNO, K8B)
          CALL JEVEUO(RESU//'.FOND      .NOEU' ,'L',JNOE1)
        ELSE
          IF (MOTFAC .EQ. 'LEVRE_SUP') THEN
            CALL JELIRA(RESU//'.FOND_SUP  .NOEU' , 'LONUTI', NBNO, K8B)
            CALL JEVEUO(RESU//'.FOND_SUP  .NOEU' ,'L',JNOE1)
          ELSEIF (MOTFAC .EQ. 'LEVRE_INF') THEN
            CALL JELIRA(RESU//'.FOND_INF  .NOEU' , 'LONUTI', NBNO, K8B)
            CALL JEVEUO(RESU//'.FOND_INF  .NOEU' ,'L',JNOE1)
          ENDIF
        ENDIF
        CALL JENONU(JEXNOM(OBJ3,ZK8(JNOE1)), NUMORI )
        X1 = ZR(IDCOOR-1 + 3*(NUMORI-1)+1)
        Y1 = ZR(IDCOOR-1 + 3*(NUMORI-1)+2)
        Z1 = ZR(IDCOOR-1 + 3*(NUMORI-1)+3)
        DMAX = 0.D0
        XSUP = 0.D0
        YSUP = 0.D0
        ZSUP = 0.D0
        COMPT = 0
        DO 602 IN = 1 , NBNOLS
          INO = JNOLS+IN-1
          IF ( ZI(INO) .EQ. NUMORI ) THEN
            COMPT = COMPT + 1
            GOTO 602
          ENDIF
          X2 = ZR(IDCOOR-1 + 3*(ZI(INO)-1)+1)
          Y2 = ZR(IDCOOR-1 + 3*(ZI(INO)-1)+2)
          Z2 = ZR(IDCOOR-1 + 3*(ZI(INO)-1)+3)
          D = SQRT( (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2 )
          XSUP = XSUP + X2
          YSUP = YSUP + Y2
          ZSUP = ZSUP + Z2
          IF  (D .GT. DMAX) THEN
            DMAX = D
            NUMFIN = ZI(INO)
          ENDIF
602     CONTINUE

        IF(COMPT .EQ. 0)  THEN
           VALK(1) = ZK8(JNOE1)
           VALK(2) = MOTFAC
           CALL U2MESK('F','RUPTURE0_76',2, VALK)
        ENDIF
        CALL OREINO ( NOMA, ZI(JNOLS), NBNOLS, NUMORI ,
     &         NUMFIN,ZR(IDCOOR),'RELATIF',0.1D0,IERA,IRET)
        CALL WKVECT(NOMOB2,'G V K8',20,INOLS)
        DO 603 IN = 1 , MIN(NBNOLS,20)
          CALL JENUNO(JEXNUM(OBJ3,ZI(JNOLS-1 + IN)),
     &                                           ZK8(INOLS-1 + IN))
603     CONTINUE
C CALCUL DE LA NORMALE
        PSUP(1) = X1 - XSUP/(NBNOLS-1)
        PSUP(2) = Y1 - YSUP/(NBNOLS-1)
        PSUP(3) = Z1 - ZSUP/(NBNOLS-1)
        VZ(1) = 0.D0
        VZ(2) = 0.D0
        VZ(3) = 1.D0
        INDICE = 3*(INIV-1)
        VNORM(INDICE+1) = 0.D0
        VNORM(INDICE+2) = 0.D0
        VNORM(INDICE+3) = 0.D0
        CALL PROVEC ( VZ,PSUP,  VNORM(INDICE+1) )
        CALL NORMEV(VNORM(INDICE+1),ZRBID)
        CALL JEDETR ( '&&'//NOMPRO//'_TRAV' )
        CALL JEDETR ( '&&'//NOMPRO//'_NOEU_NORM_SUP' )
        CALL JEDETR ( '&&'//NOMPRO//'_MAILLE_LEV_SUP' )
  10  CONTINUE

      CALL WKVECT(RESU//'.NORMALE','G V R8',3,JNORM)
      CALL GETVR8 (' ','NORMALE',1,1,3,ZR(JNORM),NVENOR)
      CALL JEEXIN(RESU//'.SUPNORM   .NOEU',IRE1)
      CALL JEEXIN(RESU//'.INFNORM   .NOEU',IRE2)
      CALL ASSERT(IRE1.NE.0)
      IF(IRE2.NE.0)THEN
        ZR(JNORM-1 + 1) =   (VNORM(1)+VNORM(4))/2.D0
        ZR(JNORM-1 + 2) = (VNORM(2)+VNORM(5))/2.D0
        ZR(JNORM-1 + 3) = (VNORM(3)+VNORM(6))/2.D0
      ELSE
        ZR(JNORM-1 + 1)   = VNORM(1)
        ZR(JNORM-1 + 2) = VNORM(2)
        ZR(JNORM-1 + 3) = VNORM(3)
      ENDIF

C
      CALL JEDEMA()
      END

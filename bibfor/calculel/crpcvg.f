      SUBROUTINE CRPCVG ( MA1, MA2, GMA1, GMA2, TRAN, PREC, 
     +                    LIMA1, LIMA2, LINOEU )
      IMPLICIT   NONE
      REAL*8              TRAN(3), PREC
      INTEGER             LINOEU(*)
      CHARACTER*8         MA1, MA2, GMA1, GMA2
      CHARACTER*(*)       LIMA1, LIMA2
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C
C     COMMANDE:  CREA_RESU
C     TRAITEMENT DU MOT CLE FACTEUR "PERM_CHAMP"
C
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
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
      CHARACTER*32       JEXNUM , JEXNOM  , JEXATR
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER        NBMA1, NBMA2, JTYMA1, JTYMA2, JGMA1,
     +               JGMA2, IMA, IMA1, IMA2, INO, INO1, INO2, NBNOMA,  
     +               NUTYP1, NUMGL1, IAMAC1, ILMAC1, 
     +               NUTYP2, NUMGL2, IAMAC2, ILMAC2, 
     +               JCOOR1, JCOOR2, JNUM1, JNUM2
      REAL*8         X1, Y1, Z1, X2, Y2, Z2, V1, V2, V3
      REAL*8         VALR(3)
      LOGICAL        ERREUR
      CHARACTER*8    K8B, NOMA1, NOMA2
      CHARACTER*24   VALK(4)
      CHARACTER*24   GRPMA1, GRPMA2, COOVA1, COOVA2, TYPMA1, TYPMA2,
     +               CONNE1, CONNE2
C
C     NBNOMA(IMA) = NOMBRE DE NOEUDS DE LA MAILLE IMA 
      NBNOMA(IMA) = ZI(ILMAC1-1+IMA+1) - ZI(ILMAC1-1+IMA)
C
C     NUMGLM(IMA,INO) = NUMERO GLOBAL DU NOEUD INO DE LA MAILLE IMA
      NUMGL1(IMA,INO) = ZI(IAMAC1-1+ZI(ILMAC1+IMA-1)+INO-1)
      NUMGL2(IMA,INO) = ZI(IAMAC2-1+ZI(ILMAC2+IMA-1)+INO-1)
C
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
      GRPMA1 = MA1//'.GROUPEMA       '
      GRPMA2 = MA2//'.GROUPEMA       '
      COOVA1 = MA1//'.COORDO    .VALE'
      COOVA2 = MA2//'.COORDO    .VALE'
      TYPMA1 = MA1//'.TYPMAIL        '
      TYPMA2 = MA2//'.TYPMAIL        '
      CONNE1 = MA1//'.CONNEX         '
      CONNE2 = MA2//'.CONNEX         '
C
      CALL JEVEUO ( COOVA1, 'L', JCOOR1 )
      CALL JEVEUO ( COOVA2, 'L', JCOOR2 )
C
      CALL JEVEUO ( TYPMA1, 'L', JTYMA1 )
      CALL JEVEUO ( TYPMA2, 'L', JTYMA2 )
C
      CALL JEVEUO ( CONNE1, 'L', IAMAC1 )
      CALL JEVEUO ( JEXATR(CONNE1,'LONCUM'), 'L', ILMAC1 )
      CALL JEVEUO ( CONNE2, 'L', IAMAC2 )
      CALL JEVEUO ( JEXATR(CONNE2,'LONCUM'), 'L', ILMAC2 )
C
      CALL JELIRA ( JEXNOM(GRPMA1,GMA1), 'LONMAX', NBMA1, K8B )
      CALL JELIRA ( JEXNOM(GRPMA2,GMA2), 'LONMAX', NBMA2, K8B )
      IF ( NBMA1 .NE. NBMA2 ) THEN
               VALK (1) = GMA1
               VALK (2) = GMA2
         CALL U2MESG('F', 'CALCULEL5_67',2,VALK,0,0,0,0.D0)
      ENDIF
C
      CALL JEVEUO ( JEXNOM(GRPMA1,GMA1), 'L', JGMA1 )
      CALL JEVEUO ( JEXNOM(GRPMA2,GMA2), 'L', JGMA2 )
C
      CALL WKVECT ( LIMA1, 'V V I', NBMA1, JNUM1 )
      CALL WKVECT ( LIMA2, 'V V I', NBMA1, JNUM2 )
C
      DO 10  IMA = 1 , NBMA1
C
         IMA1 = ZI(JGMA1+IMA-1)
         IMA2 = ZI(JGMA2+IMA-1)
C
         NUTYP1 = ZI(JTYMA1-1+IMA1)
         NUTYP2 = ZI(JTYMA2-1+IMA2)
         IF ( NUTYP1 .NE. NUTYP2 ) THEN
               VALK (1) = GMA1
               VALK (2) = GMA2
            CALL U2MESG('F', 'CALCULEL5_68',2,VALK,0,0,0,0.D0)
         ENDIF
C
         DO 20 INO = 1 , NBNOMA(IMA1)
            INO1 = NUMGL1(IMA1,INO)
            INO2 = NUMGL2(IMA2,INO)
            X1 =  ZR(JCOOR1-1+3*(INO1-1)+1)
            Y1 =  ZR(JCOOR1-1+3*(INO1-1)+2)
            Z1 =  ZR(JCOOR1-1+3*(INO1-1)+3)
            X2 =  ZR(JCOOR2-1+3*(INO2-1)+1)
            Y2 =  ZR(JCOOR2-1+3*(INO2-1)+2)
            Z2 =  ZR(JCOOR2-1+3*(INO2-1)+3)
            V1 = ABS ( X2 - X1 - TRAN(1) )
            V2 = ABS ( Y2 - Y1 - TRAN(2) )
            V3 = ABS ( Z2 - Z1 - TRAN(3) )
            ERREUR = .FALSE.
            IF ( V1 .GT. PREC )  ERREUR = .TRUE.
            IF ( V2 .GT. PREC )  ERREUR = .TRUE.
            IF ( V3 .GT. PREC )  ERREUR = .TRUE.
            IF ( ERREUR ) THEN
               CALL JENUNO(JEXNUM(MA1//'.NOMMAI', IMA1 ), NOMA1 )
               CALL JENUNO(JEXNUM(MA2//'.NOMMAI', IMA2 ), NOMA2 )
               VALK (1) = NOMA1
               VALK (2) = MA1
               VALK (3) = NOMA2
               VALK (4) = MA2
               VALR(1)  = TRAN(1)
               VALR(2)  = TRAN(2)
               VALR(3)  = TRAN(3)
               CALL U2MESG('F', 'CALCULEL5_69',4,VALK,0,0,1,VALR)
            ENDIF
C
            LINOEU(INO2) = INO1
C
 20      CONTINUE
C
         ZI(JNUM1+IMA-1) = IMA1
         ZI(JNUM2+IMA-1) = IMA2
C
 10   CONTINUE
C
      CALL JEDEMA()
      END

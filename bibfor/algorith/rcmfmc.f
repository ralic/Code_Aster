      SUBROUTINE RCMFMC ( CHMAT , MATE )
      IMPLICIT   NONE
      CHARACTER*(*)       CHMAT , MATE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 11/12/2001   AUTEUR D6BHHJP J.P.LEFEBVRE 
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
C     CREATION DE LA CARTE DU MATERIAU CODE A PARTIR DU CHAMP_MATER
C
C IN  MODELE  : NOM DU CHAMP_MATER
C VAR MATE    : NOM DE LA CARTE DE MATERIAU CODE
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
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER        NBVAL, JCART, IRET, IRETC, JVALE, IGD, JDESC, KK
      CHARACTER*4    KNUMAT
      CHARACTER*8    K8B, MATERI
      CHARACTER*19   CODI, CH19
      CHARACTER*24   CH24
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      MATERI = CHMAT(1:8)
      CH24 = MATERI//'.CHAMP_MAT'
      MATE = MATERI//'.MATE_CODE'
      CALL JELIRA ( CH24(1:19)//'.VALE', 'LONMAX', NBVAL, K8B )
      CALL JEVEUO ( CH24(1:19)//'.VALE', 'L', JCART )
      CALL JEEXIN ( MATE(1:19)//'.VALE', IRET )
      IF ( IRET .NE. 0 ) THEN
        CH19 = ZK8(JCART)
        CALL JEEXIN ( CH19//'.CODI', IRETC )
        IF ( IRETC .NE. 0 ) GOTO 9999
      ENDIF
C
      CALL COPISD ( 'CHAMP_GD', 'V', CH24(1:19), MATE(1:19) )
      CALL JEDETR ( MATE(1:19)//'.VALE' )
      CALL WKVECT ( MATE(1:19)//'.VALE', 'V V I', NBVAL, JVALE )
      CALL JENONU ( JEXNOM('&CATA.GD.NOMGD','ADRSJEVE'), IGD )
      CALL JEVEUO ( MATE(1:19)//'.DESC', 'E', JDESC )
      ZI(JDESC) = IGD
C
C     --- CODAGE DU MATERIAU ---
C
      CODI = ' '
      DO 10 KK = 1,NBVAL
         CALL RCMACO ( CHMAT(1:8), ZK8(JCART+KK-1), KK )
         CALL CODENT ( KK, 'D0', KNUMAT )
         CODI(1:8)  = ZK8(JCART+KK-1)
         CODI(9:13) = '.'//KNUMAT
         CALL JEVEUO ( CODI//'.CODI', 'L', ZI(JVALE+KK-1) )
 10   CONTINUE
C
 9999 CONTINUE
C
      CALL JEDEMA()
      END

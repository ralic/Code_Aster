      SUBROUTINE UTNUAV ( NOMAZ, K, IOCC, LMAZ, LNOZ )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER                   K, IOCC
      CHARACTER*(*)       NOMAZ,          LMAZ, LNOZ
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/02/96   AUTEUR VABHHTS J.PELLET 
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
C     COMMANDE:  PROJ_CHAMP
C ----------------------------------------------------------------------
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
      CHARACTER*8   NOMA
      CHARACTER*19  LNO, LMA
      CHARACTER*24  NO1, MA1, NO2, NOT
C
      CALL JEMARQ()
      LNO  = LNOZ
      LMA  = LMAZ
      NOMA = NOMAZ
      NO1 = '&&MON_GROUPE_NOEUD'
      MA1 = '&&MON_GROUPE_MAILLE'
C
      IF ( K .EQ. 1 ) THEN
        CALL PALINO (NOMA,'VIS_A_VIS','GROUP_NO_1','NOEUD_1' ,IOCC,NO1)
        CALL PALIMA (NOMA,'VIS_A_VIS','GROUP_MA_1','MAILLE_1',IOCC,MA1)
      ELSE
        CALL PALINO (NOMA,'VIS_A_VIS','GROUP_NO_2','NOEUD_2' ,IOCC,NO1)
        CALL PALIMA (NOMA,'VIS_A_VIS','GROUP_MA_2','MAILLE_2',IOCC,MA1)
      ENDIF
C
      CALL JEVEUO ( NO1, 'L', JNO )
      NBNO = ZI(JNO)
      DO 710 I = 1 , NBNO
 710  CONTINUE
C
      CALL JEVEUO ( MA1, 'L', JMA )
      NBMA = ZI(JMA)
      DO 700 I = 1 , NBMA
 700  CONTINUE
C
      IF ( NBMA .NE. 0 ) THEN
C
C        ---   LISTE DE MAILLES ---> LISTE DE NOEUDS   ---
         NO2 = '&&MON_GROUPE_MA_NO'
         CALL UTMANO ( NOMA, MA1, NO2 )
      CALL JEVEUO ( NO2, 'L', JNO2 )
      NBNO2 = ZI(JNO2)
      DO 720 I = 1 , NBNO2
 720  CONTINUE
C
         IF ( NBNO .NE. 0 ) THEN
C           --- ON CONCATENE LES LISTES ET ON ELIMINE LES DOUBLONS ---
            NOT = '&&MON_GROUPE_NOEUD_T'
            CALL UTELIM ( NO1, NO2 , NOT )
            CALL JEVEUO ( NOT, 'L', JNOT )
            NBNO = ZI(JNOT)
         ELSE
            CALL JEVEUO ( NO2, 'L', JNOT )
            NBNO = ZI(JNOT)
         ENDIF
C
         CALL WKVECT(LMA//'.LSMA','V V I',NBMA,JLSMA)
         DO 20 IMA = 1 , NBMA
            ZI(JLSMA+IMA-1) = ZI(JMA+1+2*(IMA-1))
 20      CONTINUE
C
      ELSE
C
         CALL JEVEUO ( NO1, 'L', JNOT )
         NBNO = ZI(JNOT)
      ENDIF
C
      CALL WKVECT(LNO//'.LSNO','V V I',NBNO,JLSNO)
      DO 10 I = 1 , NBNO
         ZI(JLSNO+I-1) = ZI(JNOT+I)
 10   CONTINUE
C
      CALL JEDETC ( 'V' , '&&MON_GROUPE_' , 1 )
C
      CALL JEDEMA()
      END

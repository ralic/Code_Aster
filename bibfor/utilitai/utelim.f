      SUBROUTINE UTELIM ( LNO1 , LNO2 , LNO )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       LNO1 , LNO2 , LNO
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 21/02/96   AUTEUR VABHHTS J.PELLET 
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
      CALL JEMARQ()
      CALL JEVEUO ( LNO1, 'L', JLNO1 )
      CALL JEVEUO ( LNO2, 'L', JLNO2 )
      NBNOT = ZI(JLNO1) + ZI(JLNO2)
      CALL WKVECT ( '&&UTELIM.NOEUD' , 'V V I' , NBNOT , JNOEU )
      DO 10 INO1 = 1, ZI(JLNO1)
         ZI(JNOEU+INO1-1) = ZI(JLNO1+INO1)
 10   CONTINUE
      DO 12 INO2 = 1, ZI(JLNO2)
         ZI(JNOEU+ZI(JLNO1)+INO2-1) = ZI(JLNO2+INO2)
 12   CONTINUE
C
C     -- ELIMINATION DES REDONDANCES EVENTUELLES DES NOEUDS
C        DE LA LISTE
C    -------------------------------------------------------------
      CALL WKVECT ( '&&UTELIM.INDICE' , 'V V I', NBNOT , JIND )
C
      DO 130 INO = 1, NBNOT
         DO 140 IN1 = INO+1, NBNOT
            IF (ZI(JNOEU+IN1-1).EQ.ZI(JNOEU+INO-1)) ZI(JIND+IN1-1) = 1
 140     CONTINUE
 130  CONTINUE
      NBNTT = 0
      DO 132 INO = 1, NBNOT
         IF ( ZI(JIND+INO-1) .EQ. 0 ) NBNTT = NBNTT + 1
 132  CONTINUE
C
      CALL WKVECT ( LNO , 'V V I' , NBNTT+1 , JLNO )
      ZI(JLNO) = NBNTT
      INDLIS = 0
      DO 150 INO = 1, NBNOT
         IF ( ZI(JIND+INO-1) .EQ. 0 ) THEN
            INDLIS = INDLIS + 1
            ZI(JLNO+INDLIS) = ZI(JNOEU+INO-1)
         ENDIF
 150  CONTINUE
C
      CALL JEDETR ( '&&UTELIM.NOEUD'  )
      CALL JEDETR ( '&&UTELIM.INDICE' )
C
      CALL JEDEMA()
      END

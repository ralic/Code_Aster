      SUBROUTINE OP0158 ( IER )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 23/08/2004   AUTEUR CIBHHLV L.VIVAN 
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
C     OPERATEUR   IMPR_CHARGE
C     ------------------------------------------------------------------
C
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER      VERSIO, IUNIFI
      LOGICAL      ULEXIS
      CHARACTER*8  K8B, FORMAT
      CHARACTER*16 FICHIE
C     ------------------------------------------------------------------
      CALL JEMARQ()
      CALL INFMAJ()
C
      CALL GETVTX (' ','FORMAT' ,1,1,1,FORMAT,N1)
C
      CALL GETVIS (' ','VERSION',1,1,1,VERSIO,N2)
C
      IFIC   = 0
      FICHIE = ' ' 
      CALL GETVIS ( ' ', 'UNITE'  , 1,1,1, IFIC  , N3 )
      IF ( .NOT. ULEXIS( IFIC ) ) THEN
         CALL ULOPEN ( IFIC, ' ', FICHIE, 'NEW', 'O' )
      ENDIF
C
      NBCHAR = 0
      CALL GETVID (' ','CHARGE',1,1,0,K8B   ,N4)
      IF ( N4 .NE. 0 ) THEN
         NBCHAR = -N4
         CALL WKVECT('&&OP0158.CHARGES','V V K8',NBCHAR,LCHA)
         CALL GETVID(' ','CHARGE',1,1,NBCHAR,ZK8(LCHA),N4)
      ENDIF
C
C     ------------------------------------------------------------------
      IF ( FORMAT .EQ. 'IDEAS' ) THEN
         CALL IRCHSU ( NBCHAR, ZK8(LCHA), IFIC, VERSIO )
      ELSE
         CALL UTMESS('F','OP0158','ERREUR DE FORMAT D''IMPRESSION')
      ENDIF
C
      IF ( NBCHAR .NE. 0 )  CALL JEDETR ( '&&OP0158.CHARGES' )
C
      CALL JEDEMA()
      END

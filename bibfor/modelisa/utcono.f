      SUBROUTINE UTCONO ( MCFAC, MOCLE, IOCC, NOMAIL, NDIM, COOR, IRET )
      IMPLICIT  NONE
      INTEGER             IOCC, NDIM, IRET
      REAL*8              COOR(*)
      CHARACTER*8         NOMAIL
      CHARACTER*(*)       MCFAC, MOCLE(3)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 04/04/2000   AUTEUR CIBHHLV L.VIVAN 
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
C
C ----------------------------------------------------------------------
C     ----------- COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER       ZI
      COMMON/IVARJE/ZI(1)
      REAL*8        ZR
      COMMON/RVARJE/ZR(1)
      COMPLEX*16    ZC
      COMMON/CVARJE/ZC(1)
      LOGICAL       ZL
      COMMON/LVARJE/ZL(1)
      CHARACTER*8   ZK8
      CHARACTER*16         ZK16
      CHARACTER*24                 ZK24
      CHARACTER*32                         ZK32
      CHARACTER*80                                 ZK80
      COMMON/KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32 JEXNUM,JEXNOM
C     ------------------------------------------------------------------
      INTEGER      N1, N2, N3, NUMNO, I, IER, JCOOR
      REAL*8       R8B
      CHARACTER*8  K8B, NOEUD
      CHARACTER*24 COORD, NOMNOE
C     ------------------------------------------------------------------
      CALL JEMARQ()
      IRET = 0
C
      CALL GETVR8 ( MCFAC, MOCLE(1), IOCC,1,0, R8B, N1 )
      IF ( N1 .NE. 0 ) THEN
         CALL GETVR8 ( MCFAC, MOCLE(1), IOCC,1,NDIM, COOR, N1 )
         IF ( N1 .NE. NDIM ) THEN
            CALL UTDEBM('F','UTCONO','ERREUR DONNEES')
            CALL UTIMPK('L','IL MANQUE DES VALEURS POUR ',1,MOCLE(1))
            CALL UTFINM()
         ENDIF
         IRET = 1
         GOTO 9999
      ENDIF
C
      COORD  = NOMAIL//'.COORDO    .VALE'
      NOMNOE = NOMAIL//'.NOMNOE         '
      CALL JEVEUO ( COORD, 'L', JCOOR )
C
      CALL GETVID ( MCFAC, MOCLE(2), IOCC,1,0, K8B, N2 )
      IF ( N2 .NE. 0 ) THEN
         CALL GETVID ( MCFAC, MOCLE(2), IOCC,1,1, NOEUD, N2 )
          CALL JENONU ( JEXNOM(NOMNOE,NOEUD), NUMNO )
          IF ( NUMNO .EQ. 0 ) THEN
             CALL UTDEBM('F','UTCONO','ERREUR DONNEES')
             CALL UTIMPK('L','LE NOEUD N''EXISTE PAS ',1,NOEUD)
             CALL UTFINM()
          ENDIF
          DO 10 I = 1 , NDIM
             COOR(I) = ZR(JCOOR+3*(NUMNO-1)+I-1)
 10       CONTINUE
          IRET = 1
          GOTO 9999
       ENDIF
C
       CALL GETVID ( MCFAC, MOCLE(3), IOCC,1,1, K8B, N3 )
       IF ( N3 .NE. 0 ) THEN
          CALL GETVID ( MCFAC, MOCLE(3), IOCC,1,1, NOEUD, N3 )
          CALL UTNONO ( ' ', NOMAIL, 'NOEUD', NOEUD, K8B, IER )
          IF ( IER .EQ. 10 ) THEN
             CALL UTDEBM('F','UTCONO','ERREUR DONNEES')
             CALL UTIMPK('L','LE GROUP_NO N''EXISTE PAS ',1,NOEUD)
             CALL UTFINM()
          ELSEIF ( IER .EQ. 1 ) THEN
             CALL UTDEBM('A','UTCONO','TROP DE NOEUDS DANS LE GROUP_NO')
             CALL UTIMPK('L','  NOEUD UTILISE: ',1,K8B)
             CALL UTFINM( )
          ENDIF
          CALL JENONU ( JEXNOM(NOMNOE,K8B), NUMNO )
          DO 20 I = 1 , NDIM
             COOR(I) = ZR(JCOOR+3*(NUMNO-1)+I-1)
 20       CONTINUE
          IRET = 1
          GOTO 9999
       ENDIF
C
 9999 CONTINUE
      CALL JEDEMA()
      END

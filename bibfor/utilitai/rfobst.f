      SUBROUTINE RFOBST ( OBSTAC )
      IMPLICIT   NONE
      CHARACTER*(*)       OBSTAC
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 28/06/2000   AUTEUR CIBHHLV L.VIVAN 
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
C     OPERATEUR "RECU_FONCTION"  MOT CLE "OBSTACLE"
C     ------------------------------------------------------------------
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER       N1, I, NBVAL, LPRO, LVAL, IDRAY, IDTHE
      REAL*8        R8DGRD, R , THETA
      CHARACTER*8   K8B, REPERE, NOPARA, NORESU
      CHARACTER*16  NOMCMD, TYPCON
      CHARACTER*19  NOMFON, RESU
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL GETRES ( NOMFON , TYPCON , NOMCMD )
      RESU = OBSTAC
C
      REPERE = 'POLAIRE'
      CALL GETVTX ( ' ', 'REPERE', 0,1,1, REPERE, N1 )
      IF ( REPERE .EQ. 'POLAIRE' ) THEN
         NOPARA = 'THETA'
         NORESU = 'R'
      ELSE
         NOPARA = 'X'
         NORESU = 'Y'
      ENDIF
C
C     --- REMPLISSAGE DU .PROL ---
C
      CALL WKVECT ( NOMFON//'.PROL', 'G V K8', 5, LPRO )
      ZK8(LPRO)   = 'FONCTION'
      ZK8(LPRO+1) = 'LIN LIN '
      ZK8(LPRO+2) = NOPARA
      ZK8(LPRO+3) = NORESU
      ZK8(LPRO+4) = 'EE      '
C
      CALL JEVEUO ( RESU//'.VALR', 'L', IDRAY )
      CALL JEVEUO ( RESU//'.VALT', 'L', IDTHE )             
      CALL JELIRA ( RESU//'.VALT', 'LONMAX', NBVAL, K8B )             
C
C     --- REMPLISSAGE DU .VALE ---
C
      CALL WKVECT ( NOMFON//'.VALE', 'G V R8', 2*NBVAL, LVAL )
C
      IF ( REPERE .EQ. 'POLAIRE' ) THEN
         DO 10 I = 1 , NBVAL
            ZR(LVAL+I-1) = ZR(IDTHE+I-1) / R8DGRD()
            ZR(LVAL+NBVAL+I-1) = ZR(IDRAY+I-1)
 10      CONTINUE
      ELSE
         DO 12 I = 1 , NBVAL
            R = ZR(IDRAY+I-1)
            THETA = ZR(IDTHE+I-1)
            ZR(LVAL+I-1) = R * COS(THETA)
            ZR(LVAL+NBVAL+I-1) = R * SIN(THETA)
 12      CONTINUE
      ENDIF
C
      CALL JEDEMA()
      END

      SUBROUTINE RFMGE2  ( MODGEN )
      IMPLICIT NONE
      CHARACTER*(*)        MODGEN
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 30/01/2006   AUTEUR LEBOUVIE F.LEBOUVIER 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C     OPERATEUR "RECU_FONCTION"  MOT CLE "RESU_GENE" 
C                              CONCEPT MODE_GENE SANS SOUS-STRUCTURATION
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
      CHARACTER*32     JEXNUM, JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER      N1, IRET, NBORDR, JORDR, LPRO, LVAR, LFON
      REAL*8       EPSI 
      CHARACTER*8  CRIT, INTERP(2)
      CHARACTER*14 NUMDDL
      CHARACTER*16 TYPCON, NOMCMD
      CHARACTER*19 NOMFON
      CHARACTER*24 KNUME
C-----------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL GETRES ( NOMFON , TYPCON , NOMCMD )
C
      CALL UTMESS('F','RFMGE2','FONCTIONNALITE NON DEVELOPPEE' )
C
      INTERP(1) = 'NON '
      INTERP(2) = 'NON '
C
      CALL GETVTX ( ' ', 'CRITERE'  , 1,1,1, CRIT  , N1 )
      CALL GETVR8 ( ' ', 'PRECISION', 1,1,1, EPSI  , N1 )
      CALL GETVTX ( ' ', 'INTERPOL' , 1,1,2, INTERP, N1 )
      IF ( N1 .EQ. 1 ) INTERP(2) = INTERP(1)
C
      KNUME = '&&RFMGE2.NUME_ORDR'
      CALL RSUTNU ( MODGEN, ' ', 1, KNUME, NBORDR, EPSI, CRIT, IRET )
      IF (IRET.NE.0) THEN
         CALL UTMESS('F','RFMGE2','PROBLEME(S) RENCONTRE(S) LORS DE '//
     +                          'L''ACCES AU RESU_GENE' )
      ENDIF
      CALL JEVEUO ( KNUME, 'L', JORDR )
C
C     --- CREATION DE LA FONCTION ---
C
      CALL WKVECT ( NOMFON//'.PROL', 'G V K16', 5, LPRO )
      ZK16(LPRO)   = 'FONCTION        '
      ZK16(LPRO+1) = INTERP(1)//INTERP(2)
      ZK16(LPRO+2) = 'FREQ            '
      ZK16(LPRO+4) = 'EE              '
C
      CALL WKVECT ( NOMFON//'.VALE', 'G V R', 2*NBORDR, LVAR )
CCC      LFON = LVAR + NBORDR - 1

      CALL JEDEMA()
      END

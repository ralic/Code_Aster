      SUBROUTINE MEFSM1 ( VALE, MATGEN, BASE, NOMNUM, NOMSTO,
     +                                           NBMODE, NBLOC, NTERM )
      IMPLICIT   NONE
      INTEGER             NBMODE, NBLOC, NTERM
      REAL*8              VALE(*)
      CHARACTER*8         MATGEN, BASE
      CHARACTER*19        NOMNUM, NOMSTO
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 16/09/2003   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*32     JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER       IBID, I, J, IBLO, IADESC, IALIME, IACONL, IAREFE, 
     +              LDBLO
      CHARACTER*8   K8B
      CHARACTER*19  MATRGE
C DEB------------------------------------------------------------------
C
      CALL JEMARQ()
C
      MATRGE = MATGEN
C
      CALL WKVECT ( MATRGE//'.DESC', 'G V I', 3, IADESC )
      ZI(IADESC)   = 2
      ZI(IADESC+1) = NBMODE
      ZI(IADESC+2) = 2
C
      CALL WKVECT ( MATRGE//'.LIME', 'G V K8', 1, IALIME )
      ZK8(IALIME) = '        '
C
      CALL WKVECT ( MATRGE//'.CONL', 'G V R' , NBMODE, IACONL )
      DO 10 I = 1 , NBMODE
         ZR(IACONL+I-1) = 1.0D0
 10   CONTINUE
C
      CALL WKVECT ( MATRGE//'.REFA', 'G V K24', 4, IAREFE )
      CALL JEECRA ( MATRGE//'.REFA', 'DOCU', IBID, 'SLCS' )
      ZK24(IAREFE)   = BASE
      ZK24(IAREFE+1) = NOMNUM
      ZK24(IAREFE+2) = NOMSTO
C
      CALL JECREC ( MATRGE//'.VALE', 'G V R', 'NU', 'DISPERSE', 
     &                                            'CONSTANT', NBLOC )
      CALL JEECRA ( MATRGE//'.VALE', 'LONMAX', NTERM, K8B )
      CALL JEECRA ( MATRGE//'.VALE', 'DOCU', IBID, 'MS' )
C
      IBLO = 1
C
      CALL JECROC ( JEXNUM(MATRGE//'.VALE',IBLO) )
      CALL JEVEUO ( JEXNUM(MATRGE//'.VALE',IBLO), 'E', LDBLO )
C
      NTERM = 0
      DO 20 I = 1 , NBMODE
         DO 22 J = 1 , I
            NTERM = NTERM + 1
            ZR(LDBLO+NTERM-1) = VALE( J + (I-1)*NBMODE )
 22      CONTINUE
 20   CONTINUE
C
      CALL JELIBE ( JEXNUM(MATRGE//'.VALE', IBLO) )
C      
      CALL JEDEMA()
C
      END

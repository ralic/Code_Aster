      SUBROUTINE OP0128 ( IER )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 11/03/2003   AUTEUR DURAND C.DURAND 
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
      IMPLICIT REAL*8 (A-H,O-Z)
C
C***********************************************************************
C    P. RICHARD     DATE 13/07/90
C-----------------------------------------------------------------------
C  BUT: ASSEMBLER UNE MATRICE ISSUE D'UN MODELE GENERALISE
C
C     CONCEPT CREE: MATR_ASSE_GEN
C
C-----------------------------------------------------------------------
C
      INTEGER          IER
C
C-------- DEBUT COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C-----  FIN  COMMUNS NORMALISES  JEVEUX  -------------------------------
C
      CHARACTER*8 NOMRES,NUMEG
      CHARACTER*9 OPTION
      CHARACTER*19 NOMNUM,NOMSTO
      CHARACTER*16 NOMCON,NOMOPE
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL INFMAJ()
C
      CALL GETRES(NOMRES,NOMCON,NOMOPE)
C
C
C-------------------RECUPERATION CONCEPTS AMONT-------------------------
C
      CALL GETVID(' ','NUME_DDL_GENE',1,1,1,NUMEG,IBID)
      NOMNUM=NUMEG//'      .NUME'
      NOMSTO=NUMEG//'      .SLCS'
C
C-------------------------RECUPERATION DE L'OPTION----------------------
C
      CALL GETVTX(' ','OPTION',1,1,1,OPTION,IOC)
C
C---------------------------------ASSEMBLAGE----------------------------
C
      CALL ASSGEN(NOMRES,OPTION,NOMNUM,NOMSTO)
C
C
C
C      CALL UTIMSD('MESSAGE',2,.TRUE.,.TRUE.,NOMRES,1,'G')
C
      END

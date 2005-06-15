      SUBROUTINE MXMOAM(MASGEN,BASMOD,LMODAL)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*24 MASGEN, BASMOD
      LOGICAL      LMODAL

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 15/06/2005   AUTEUR VABHHTS J.PELLET 
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
C RESPONSABLE BOYERE E.BOYERE
C ======================================================================
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNOM, JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CHARACTER*6  PGC
      CHARACTER*8  K8B
      CHARACTER*8  MODMEC, MAILLA
      CHARACTER*14 NUMDDL
      CHARACTER*24 DEEQ
      CHARACTER*24 KBID, MATRIC, NOMCHA
      CHARACTER*80 TITRE
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      LMODAL = .FALSE.
      CALL GETFAC('PROJ_MODAL',NMODA)
      IF (NMODA.EQ.0) GOTO 9999
      LMODAL = .TRUE.
      CALL GETVID('PROJ_MODAL','MODE_MECA',1,1,1,MODMEC,NBMD)
      CALL JEVEUO(MODMEC//'           .REFD','L',IADRIF)
      MATRIC =  ZK24(IADRIF+2)(1:8)
      CALL DISMOI('F','NOM_MAILLA'  ,MATRIC,'MATR_ASSE',IBI,MAILLA,IER)
      CALL DISMOI('F','NOM_NUME_DDL',MATRIC,'MATR_ASSE',IBI,NUMDDL,IRET)
      DEEQ = NUMDDL//'.NUME.DEEQ'
      CALL JEVEUO(DEEQ,'L',IDDEEQ)
      CALL DISMOI('F','NB_EQUA',MATRIC,'MATR_ASSE',NEQ,K8B,IRET)
C
C --- ALLOCATION DESCRIPTEUR DE LA MATRICE
C
      CALL MTDSCR(MATRIC(1:8))
      CALL JEVEUO(MATRIC(1:19)//'.&INT','E',LMAT)
C
      CALL JELIRA(MODMEC//'           .ORDR','LONMAX',NBMODE,KBID)
      CALL GETVIS('PROJ_MODAL','NB_MODE',1,1,1,NBMAX,NM)
      NBMODE = MIN(NBMODE,NBMAX) 
      CALL WKVECT(MASGEN,'V V R',NBMODE,JVALMO)
      CALL WKVECT(BASMOD,'V V R',NBMODE*NEQ,JBASMO)

C --- ALLOCATION VECTEUR DE TRAVAIL
C
      DO 10 I=1,NBMODE
        CALL RSADPA(MODMEC,'L',1,'MASS_GENE',I,0,LPAR,K8B)
        ZR(JVALMO+I-1) = ZR(LPAR)
        CALL RSEXCH(MODMEC,'DEPL',I,NOMCHA,IRET)
        CALL JEVEUO(NOMCHA(1:19)//'.VALE','L',JVAL)
C
C ----- CALCUL PRODUIT MATRICE DEFORMEE
C
        CALL DCOPY(NEQ,ZR(JVAL),1,ZR(JBASMO+(I-1)*NEQ),1)
C
C     --- MISE A ZERO DES DDL DE LAGRANGE
        CALL ZERLAG(ZR(JBASMO+(I-1)*NEQ),NEQ,ZI(IDDEEQ))
 10   CONTINUE
C
C      CALL JEDETC('V','&&MXMOAM',1)
C
 9999 CONTINUE
      CALL JEDEMA()
      END

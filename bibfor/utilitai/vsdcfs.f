      SUBROUTINE VSDCFS(NOMZ,M80,NVU,NOBJ)

      IMPLICIT NONE
      CHARACTER*(*) NOMZ,M80
      INTEGER NVU,NOBJ

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 05/02/2007   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C BUT : VERISD/MELASFLU
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     ----- FIN COMMUNS NORMALISES  JEVEUX  --------------------------
C -DEB------------------------------------------------------------------

      CHARACTER*8    NOM,KBID
      CHARACTER*24   TRAV,PRCHNO,TABMAS,TABAMO,TABRIG,FSVI
      CHARACTER*19   TABLE
      INTEGER        JTBLP,JDESC,ICHAM,NBCHAM,NBMODE,JREFE,IER,JTRAV
      INTEGER        VERIOB,JNUMO,IMODE,JREMF,JFSIC,ITYPFL,NBVITE,IFSVI
      INTEGER        IVITE,I,JLTBLP,NBMATR,JMASS,JAMOR,JRIGI,NBVAL
      INTEGER        IMATR,NBZEX
      
      
      
      CALL JEMARQ()
      
      NOM=NOMZ(1:8)

C---- 1. VERIFICATION DE L OBJET TABLE ET DES CHAMPS STOCKES DEDANS

      CALL VERIJA('O','TABLE',NOM,M80,NVU,NOBJ)
      CALL JEVEUO(NOM//'           .TBLP','L',JTBLP)

C     L OBJET TRAV EST LE NOM DE L OBJET CONTENANT LA LISTE DES CHAMPS
C     DE DEFORMES
      CALL ASSERT(ZK24(JTBLP).EQ.'NOM_CHAM')
      TRAV=ZK24(JTBLP+2)
      CALL JELIRA(TRAV,'LONUTI',NBCHAM,KBID)
      CALL JEVEUO(TRAV,'L',JTRAV)
      
      DO 10 ICHAM=1,NBCHAM
        CALL VERIJA('O','CHAM_NO',ZK24(JTRAV-1+ICHAM),M80,NVU,NOBJ)
10    CONTINUE

C   TOUS CES CHAMPS S APPUIENT SUR UN (ET UN SEUL!) PROF_CHNO CREE

      CALL JEVEUO(ZK24(JTRAV)(1:19)//'.REFE','L',JREFE)
      PRCHNO=ZK24(JREFE+1)
      
C   VERIFICATION DU PROF_CHNO CREE
      CALL VERIJA('O','PROF_CHNO',PRCHNO,M80,NVU,NOBJ)
      
C -------------------------------------------------------------------

C---- 2. VERIFICATION DE L OBJET .DESC

      IER=VERIOB('O',NOM//'           .DESC','EXIS_V',0,' ')
      IER=VERIOB('O',NOM//'           .DESC','TYPE_K16',0,' ')
      IER=VERIOB('O',NOM//'           .DESC','LONMAX',1,' ')
      CALL JEVEUO(NOM//'           .DESC','L',JDESC)
      CALL ASSERT(ZK16(JDESC).EQ.'DEPL')

C -------------------------------------------------------------------

C---- 3. VERIFICATION DE L OBJET .NUMO

      IER=VERIOB('O',NOM//'           .NUMO','EXIS_V',0,' ')
      IER=VERIOB('O',NOM//'           .NUMO','TYPE_I',0,' ')
      CALL JELIRA(NOM//'           .NUMO','LONMAX',NBMODE,KBID)
      CALL JEVEUO(NOM//'           .NUMO','L',JNUMO)

      DO 20 IMODE=1,NBMODE
        CALL ASSERT(ZI(JNUMO-1+IMODE).GT.0)
 20   CONTINUE
C -------------------------------------------------------------------

C---- 3. VERIFICATION DE L OBJET .REMF

      IER=VERIOB('O',NOM//'           .REMF','EXIS_V',0,' ')
      IER=VERIOB('O',NOM//'           .REMF','TYPE_K8',0,' ')
      IER=VERIOB('O',NOM//'           .REMF','LONMAX',2,' ')
      
      CALL JEVEUO(NOM//'           .REMF','L',JREMF)
      CALL VERIJA('O','TYPE_FLUI_STRU',ZK8(JREMF),M80,NVU,NOBJ)
      
C     RECUPERATION DU TYPE D INTERACTION FLUIDE-STRUCTURE
      CALL JEVEUO(ZK8(JREMF)//'           .FSIC','L',JFSIC)
      ITYPFL=ZI(JFSIC)

C     CONCEPT MODE_MECA OBLIGATOIREMENT
      CALL VERIJA('O','SD_RESULTAT_DYN',ZK8(JREMF+1),M80,NVU,NOBJ)
      
C -------------------------------------------------------------------

C---- 4. VERIFICATION DE L OBJET .VITE

      IER=VERIOB('O',NOM//'           .VITE','EXIS_V',0,' ')
      IER=VERIOB('O',NOM//'           .VITE','TYPE_R',0,' ')
      CALL JELIRA(NOM//'           .VITE','LONMAX',NBVITE,KBID)
      
C -------------------------------------------------------------------

C---- 5. VERIFICATION DE L OBJET .FACT

      IER=VERIOB('O',NOM//'           .FACT','EXIS_V',0,' ')
      IER=VERIOB('O',NOM//'           .FACT','TYPE_R',0,' ')

      IF (ITYPFL.EQ.3) THEN
C     FAISCEAU_AXIAL
        IER=VERIOB('O',NOM//'           .FACT','LONMAX',
     &             3*NBMODE*NBVITE,' ')
      ELSE
C     AUTRE CONFIGURATION
        IER=VERIOB('O',NOM//'           .FACT','LONMAX',3*NBMODE,' ')
      ENDIF

C -------------------------------------------------------------------

C---- 6. VERIFICATION DE L OBJET .MASG

      IER=VERIOB('O',NOM//'           .MASG','EXIS_V',0,' ')
      IER=VERIOB('O',NOM//'           .MASG','TYPE_R',0,' ')

      IF (ITYPFL.EQ.3) THEN
C     FAISCEAU_AXIAL
        IER=VERIOB('O',NOM//'           .MASG','LONMAX',
     &             NBMODE*NBVITE,' ')
      ELSE
C     AUTRE CONFIGURATION
        IER=VERIOB('O',NOM//'           .MASG','LONMAX',NBMODE,' ')
      ENDIF

C -------------------------------------------------------------------

C---- 7. VERIFICATION DE L OBJET .FREQ

      IER=VERIOB('O',NOM//'           .FREQ','EXIS_V',0,' ')
      IER=VERIOB('O',NOM//'           .FREQ','TYPE_R',0,' ')
      IER=VERIOB('O',NOM//'           .FREQ','LONMAX',
     &             2*NBMODE*NBVITE,' ')

C -------------------------------------------------------------------

C---- 8. VERIFICATION DE L OBJET L_TABLE

C     CET OBJET N EXISTE QUE POUR LES FAISCEAU_AXIAL
      IF (ITYPFL.EQ.3) THEN
        CALL VERIJA('O','L_TABLE',NOM,M80,NVU,NOBJ)
        CALL LTNOTB(NOM,'MATR_GENE',TABLE)
        CALL VERIJA('O','TABLE',TABLE,M80,NVU,NOBJ)
C    CETTE TABLE CONTIENT DONC LES NOMS DES OBJETS JEVEUX STOCKANT LES
C    DIFFERENTES MATRICES GENERALISEES
        CALL JEVEUO(TABLE//'.TBLP','L',JLTBLP)

C    TABLE CONTENANT LE NOM DE TOUTES LES MATRICES DE MASSES
        TABMAS=ZK24(JLTBLP+10)
C    TABLE CONTENANT LE NOM DE TOUTES LES MATRICES D AMORTISSMENT
        TABAMO=ZK24(JLTBLP+14)
C    TABLE CONTENANT LE NOM DE TOUTES LES MATRICES DE RIGITE
        TABRIG=ZK24(JLTBLP+18)

        CALL VERIJA('O','TABLE',TABMAS,M80,NVU,NOBJ)
        CALL VERIJA('O','TABLE',TABAMO,M80,NVU,NOBJ)
        CALL VERIJA('O','TABLE',TABRIG,M80,NVU,NOBJ)

        CALL JELIRA(TABMAS,'LONUTI',NBMATR,KBID)
        CALL JEVEUO(TABMAS,'L',JMASS)
        CALL JEVEUO(TABAMO,'L',JAMOR)
        CALL JEVEUO(TABRIG,'L',JRIGI)

        DO 50 IMATR=1,NBMATR
          CALL VERIJA('O','MATR_ASSE_GENE',ZK24(JMASS-1+IMATR),
     &                M80,NVU,NOBJ)
          CALL VERIJA('O','MATR_ASSE_GENE',ZK24(JAMOR-1+IMATR),
     &                M80,NVU,NOBJ)
          CALL VERIJA('O','MATR_ASSE_GENE',ZK24(JRIGI-1+IMATR),
     &                M80,NVU,NOBJ)
 50     CONTINUE


      ENDIF

C -------------------------------------------------------------------

C---- 8. VERIFICATION DES OBJETS .VCN

      IER=VERIOB('F',NOM//'.VCN','EXIS_V',0,' ')
      IER=VERIOB('F',NOM//'.VCN','TYPE_R',0,' ')
      IF (IER.EQ.1) THEN
        FSVI = ZK8(JREMF)//'           .FSVI'
        CALL JEVEUO(FSVI,'L',IFSVI)
        NBZEX  = ZI(IFSVI+1)

        NBVAL=1
        DO 60 I=1,NBZEX
           NBVAL=NBVAL*ZI(IFSVI+1+NBZEX+I)
  60    CONTINUE

        IER=VERIOB('F',NOM//'.VCN','LONMAX',
     &               NBMODE*NBVAL,' ')
      ENDIF

C---- 8. VERIFICATION DES OBJETS .VEN
      IER=VERIOB('F',NOM//'.VEN','EXIS_V',0,' ')
      IER=VERIOB('F',NOM//'.VEN','TYPE_R',0,' ')
      IF (IER.EQ.1) THEN
        IER=VERIOB('F',NOM//'.VEN','LONMAX',
     &               NBMODE,' ')
      ENDIF

      CALL JEDEMA()

      END

      SUBROUTINE GFMACO(NOMA,NBNOEU,NBNO,ICOOVA,AXEP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 13/12/2011   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------
C
      IMPLICIT NONE
C     IN
      INTEGER         NBNOEU,NBNO,ICOOVA
      CHARACTER*8     NOMA
      REAL*8          AXEP(2)
C     ------------------------------------------------------------------
C     NOMA   : NOM DU MAILLAGE GLOBAL DE LA SD G_FIBRE
C     NBNOEU : NB DE NOEUDS DEJA ENTRES DANS .COORDO  .VALE
C     NBNO   : NB DE NOEUDS DU GROUPE DE FIBRES CONCERNE
C     ICOVA  : ADRESSE DU .COORDO .VALE DU MAILLAGE DU GROUPE DE FIBRES
C     AXEP   : COOR_AXE_POUTRE DU GROUPE DE FIBRES
C     ------------------------------------------------------------------
C     COPIE DES COORDONNEES DES NOEUDS DU MAILLAGE DU GROUPE DE FIBRES
C     (MOT CLE SECTION) DANS LE MAILLAGE GLOBAL DE SECTION
C     (DEFI_GEOM_FIBRE)
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C

C
C ----- DECLARATIONS
C
      INTEGER         ICOORV,I
      CHARACTER*24    COOVAL

      CALL JEMARQ ( )

      COOVAL  = NOMA// '.COORDO    .VALE'
C
C --- REMPLISSAGE DE L'OBJET .COORDO    .VALE :
C     -------------------------
      CALL JEVEUO(COOVAL,'E',ICOORV)

      DO 20 I=1,NBNO
         ZR(ICOORV+(NBNOEU+I-1)*3-1+1)=ZR(ICOOVA+3*I-1+1)-AXEP(1)
         ZR(ICOORV+(NBNOEU+I-1)*3-1+2)=ZR(ICOOVA+3*I-1+2)-AXEP(2)
  20  CONTINUE

      CALL JEDEMA ( )
      END

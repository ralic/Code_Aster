      SUBROUTINE FONNO7 (NOMA,NDIM,NA,VECDIR,HMAX)
      IMPLICIT NONE
      INTEGER             NA,NDIM
      REAL*8              VECDIR(3),HMAX
      CHARACTER*8         NOMA
      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 30/01/2012   AUTEUR MACOCCO K.MACOCCO 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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

C       ----------------------------------------------------------------
C       DETERMINATION DE LA TAILLE MAXIMALE DES MAILLES CONNECTEES AU 
C       NOEUD DU FOND
C       ----------------------------------------------------------------
C    ENTREES
C       NOMA  : NOM DU MAILLAGE
C       NDIM  : DIMENSION DU MODELE
C       NA    : NUMERO DU NOEUD SOMMET COURANT
C       VECDIR: VECTEUR TANGENT
C    SORTIE
C       HMAX  : TAILLE MAXIMALE DES MAILLES
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
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32     JEXNUM,JEXATR
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER       ADRA, AR(12,3)
      INTEGER       IATYMA, IAR, IBID, IMA, INO1, INO2, IRET, ITYP
      INTEGER       JCNCIN, JCONX1, JCONX2, JCOOR, JDRVLC
      INTEGER       NBAR, NBMACA, NDIME, NNO, NNO1, NNO2, NUMAC
      REAL*8        COOR(3), VECT(3), P
      CHARACTER*8   K8B, TYPE
      CHARACTER*24  NCNCIN
C     -----------------------------------------------------------------

      CALL JEMARQ()
C
C     RECUPERATION DES DONNEES SUR LE MAILLAGE
      CALL JEVEUO( NOMA//'.CONNEX','L', JCONX1 )
      CALL JEVEUO( JEXATR(NOMA//'.CONNEX','LONCUM'),'L', JCONX2 )
      CALL JEVEUO( NOMA//'.TYPMAIL','L', IATYMA )
      CALL JEVEUO( NOMA//'.COORDO    .VALE','L', JCOOR )

C     RECUPERATION DES COORDONNNES DE NA      
      COOR(1) = ZR(JCOOR-1 + (NA-1)*3 + 1)
      COOR(2) = ZR(JCOOR-1 + (NA-1)*3 + 2)
      COOR(3) = ZR(JCOOR-1 + (NA-1)*3 + 3)

C     RECUPERATION DE LA CONNECTIVITE INVERSE
      NCNCIN = '&&OP0055.CONNECINVERSE'
      CALL JEVEUO ( JEXATR(NCNCIN,'LONCUM'), 'L', JDRVLC )
      CALL JEVEUO ( JEXNUM(NCNCIN,1)       , 'L', JCNCIN )

C     MAILLES CONNECTEES A NA
      ADRA   = ZI(JDRVLC-1 + NA)
      NBMACA = ZI(JDRVLC-1 + NA+1) - ZI(JDRVLC-1 + NA)
C
      HMAX=0

      DO 10 IMA=1,NBMACA
C       NUMERO DE LA MAILLE
        NUMAC = ZI(JCNCIN-1 + ADRA+IMA-1)
        ITYP = IATYMA-1+NUMAC
        CALL JENUNO( JEXNUM('&CATA.TM.NOMTM',ZI(ITYP)), TYPE )        
        CALL DISMOI('F','DIM_TOPO',TYPE,'TYPE_MAILLE',NDIME,K8B,IRET)
        
C       ON ZAPPE LES MAILLES DE BORDS
        IF (NDIME.NE.NDIM) GOTO 10

        CALL CONARE(TYPE,AR,NBAR)

C       BOUCLE SUR LE NOMBRE D'ARETES DE LA MAILLE NUMAC        
        DO 100 IAR=1,NBAR

           INO1 = AR(IAR,1)
           NNO1 = ZI(JCONX1-1 + ZI(JCONX2+NUMAC-1) +INO1-1)
           INO2 = AR(IAR,2)
           NNO2 = ZI(JCONX1-1 + ZI(JCONX2+NUMAC-1) +INO2-1)

           IF (NA.EQ.NNO1) THEN
              NNO=NNO2            
           ELSE IF (NA.EQ.NNO2) THEN
              NNO=NNO1
           ELSE
              GOTO 100
           ENDIF

C          VECTEUR REPRESENTANT L'ARETE NA-NNO           
           VECT(1) = ZR(JCOOR-1+ (NNO-1)*3+1) - COOR(1)
           VECT(2) = ZR(JCOOR-1+ (NNO-1)*3+2) - COOR(2)
           VECT(3) = ZR(JCOOR-1+ (NNO-1)*3+3) - COOR(3)
           
C          PROJECTION DE L'ARETE SUR LE VECTEUR TANGENT 
           P = VECT(1)*VECDIR(1)+VECT(2)*VECDIR(2)+VECT(3)*VECDIR(3) 
           P = ABS(P)

           IF (P.GE.HMAX) HMAX = P
           
 100      CONTINUE

 10   CONTINUE
C
      CALL JEDEMA()
      END

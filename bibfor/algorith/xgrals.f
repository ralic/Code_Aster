      SUBROUTINE XGRALS(IFM,MODE,NOMA,FISS,GRLT,GRLN)
      IMPLICIT NONE
      INTEGER       IFM
      CHARACTER*8   MODE,NOMA,FISS
      CHARACTER*19  GRLT,GRLN


C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 30/03/2004   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE CIBHHLV L.VIVAN
C                       CALCUL DES GRADIENTS DES LEVEL-SETS
C
C
C    ENTREE :
C              IFM    :   FICHIER D'IMPRESSION
C              MODE   :   OBJET MODELE
C              NOMA   :   OBJET MAILLAGE
C              FISS  :    SD_FISS
C
C    SORTIE : 
C              GRLN  :   GRADIENT DE LA LEVEL-SET NORMALE
C              GRLT  :   GRADIENT DE LA LEVEL-SET TANGENTE
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C     
      INTEGER         IBID,NCHIN
      INTEGER         IADRMA             
      CHARACTER*8     LPAIN(2),LPAOUT(1)
      CHARACTER*16    K16BID     
      CHARACTER*19    CHGRLT,CHGRLN,CHAMS
      CHARACTER*24    OBJMA,LCHIN(2),LCHOUT(1),LIGRMO,COURB
C     ------------------------------------------------------------------
      CALL JEMARQ()

      CHGRLT   = '&&OP0112.CHGRLT'
      CHGRLN   = '&&OP0112.CHGRLN' 
      CHAMS    = '&&OP0112.CHAMS'
            
C     GRADIENT DE LST
C     ---------------

      LPAIN(1)='PGEOMER'
      LCHIN(1)=NOMA//'.COORDO'
      LPAIN(2)='PNEUTER'
      LCHIN(2)=FISS//'.LTNO'
      LPAOUT(1)='PGNEUTR'
      LCHOUT(1)=CHGRLT
      LIGRMO=MODE//'.MODELE'
      NCHIN=2
      CALL CALCUL('S','GRAD_NEUT_R',LIGRMO,NCHIN,LCHIN,LPAIN,1,
     &                  LCHOUT,LPAOUT,'V')
      
C     PASSAGE D'UN CHAM_ELNO EN UN CHAM_NO     
      CALL CELCES ( LCHOUT, 'V', CHAMS ) 
      CALL CESCNS ( CHAMS, ' ', 'V', GRLT )


C     GRADIENT DE LSN
C     ---------------

      LPAIN(1)='PGEOMER'
      LCHIN(1)=NOMA//'.COORDO'
      LPAIN(2)='PNEUTER'
      LCHIN(2)=FISS//'.LNNO'
      LPAOUT(1)='PGNEUTR'
      LCHOUT(1)=CHGRLN
      LIGRMO=MODE//'.MODELE'
      NCHIN=2
      CALL CALCUL('S','GRAD_NEUT_R',LIGRMO,NCHIN,LCHIN,LPAIN,1,
     &                  LCHOUT,LPAOUT,'V')
      
C     PASSAGE D'UN CHAM_ELNO EN UN CHAM_NO      
      CALL CELCES ( LCHOUT, 'V', CHAMS ) 
      CALL CESCNS ( CHAMS, ' ', 'V', GRLN )

      WRITE(IFM,*)'GRADIENTS DES LEVEL-SETS CALCULES'

      CALL DETRSD ( 'CHAM_ELEM_S', CHAMS )

      CALL JEDEMA()
      END

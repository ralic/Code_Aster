      SUBROUTINE CENGRA(NOMA,NMAABS,COORG)
      IMPLICIT NONE

      INTEGER       NMAABS
      REAL*8        COORG(3)
      CHARACTER*8   NOMA

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 11/04/2012   AUTEUR LADIER A.LADIER 
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
C
C                 CALCUL DU CENTRE DE GRAVITE D'UNE MAILLE
C
C     ENTREE
C       NOMA     : NOM DU MAILLAGE
C       NMAABS   : INDICE DE LA MAILLE
C
C     SORTIE
C       COORG    : COORDONNEES DU CENTRE DE GRAVITE DE LA MAILLE
C
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
      CHARACTER*32    JEXATR
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------

      INTEGER         INO,ITYPMA,JCOOR,JCONX1,JCONX2,JMA,NBNOTT(3),NUNO
C ----------------------------------------------------------------------
      CALL JEMARQ()

C     RECUPERATION DES DONNEES DU MAILLAGE
      CALL JEVEUO(NOMA//'.COORDO    .VALE','L',JCOOR)
      CALL JEVEUO(NOMA//'.CONNEX','L',JCONX1)
      CALL JEVEUO(JEXATR(NOMA//'.CONNEX','LONCUM'),'L',JCONX2)
      CALL JEVEUO(NOMA//'.TYPMAIL','L',JMA)

      ITYPMA = ZI(JMA-1+NMAABS)
      CALL PANBNO(ITYPMA,NBNOTT)

C     CALCUL DES COORDONNEES DU CENTRE DE GRAVITE
      COORG(1) = 0
      COORG(2) = 0
      COORG(3) = 0

      DO 10 INO=1,NBNOTT(1)
        NUNO = ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+INO-1)

        COORG(1) = COORG(1) + ZR(JCOOR-1+3*(NUNO-1)+1)
        COORG(2) = COORG(2) + ZR(JCOOR-1+3*(NUNO-1)+2)
        COORG(3) = COORG(3) + ZR(JCOOR-1+3*(NUNO-1)+3)
  10  CONTINUE
      COORG(1) = COORG(1) / NBNOTT(1)
      COORG(2) = COORG(2) / NBNOTT(1)
      COORG(3) = COORG(3) / NBNOTT(1)

      CALL JEDEMA()
      END

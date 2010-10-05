      SUBROUTINE CCNETT(NOBASE,NOPOUT)
      IMPLICIT NONE
C     --- ARGUMENTS ---
      INTEGER      NOPOUT
      CHARACTER*8  NOBASE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 05/10/2010   AUTEUR SELLENET N.SELLENET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C  CALC_CHAMP - NETTOYAGE
C  -    -       ----
C ----------------------------------------------------------------------
C
C IN  :
C   NOBASE  K8   BASE DU NOM A PARTIR DE LAQUELLE LE NOM DES OBJETS DE
C                CCLIOP ONT ETE CONSTRUITS
C   NOPOUT  I    TAILLE DE LA LISTE OUT
C ----------------------------------------------------------------------
C RESPONSABLE SELLENET N.SELLENET
C     ----- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER        ZI
      COMMON /IVARJE/ZI(1)
      REAL*8         ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16     ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL        ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8    ZK8
      CHARACTER*16          ZK16
      CHARACTER*24                  ZK24
      CHARACTER*32                          ZK32
      CHARACTER*80                                  ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     ----- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      
      INTEGER      IOP
      
      CHARACTER*5  NUMOPT
      CHARACTER*11 NOBAOP
      CHARACTER*24 NOLIOP,NOLORI,NOLDEP,NOLIIN,LISINS,NOLISD
      
      CALL JEMARQ()
      
      NOLIOP = NOBASE//'.LISOPT'
      NOLORI = NOBASE//'.LISORI'
      NOLDEP = NOBASE//'.LISDEP'
      NOLIIN = NOBASE//'.LNOINS'
      NOLISD = NOBASE//'.ISODEP'
      
      NOBAOP = NOBASE//'.OP'
      
      DO 30 IOP = 1,NOPOUT
        CALL CODENT(IOP,'D0',NUMOPT)
        LISINS = NOBAOP//NUMOPT
        CALL JEDETR(LISINS)
   30 CONTINUE
      
      CALL JEDETR(NOLIOP)
      CALL JEDETR(NOLORI)
      CALL JEDETR(NOLDEP)
      CALL JEDETR(NOLIIN)
      CALL JEDETR(NOLISD)
      
      CALL JEDEMA()
      
      END

      SUBROUTINE UTACCE ( VAR, NOMSD, ACCES1, ACCES2, TYPE, NBORDR )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       VAR, NOMSD, ACCES1, ACCES2, TYPE
      INTEGER                                        NBORDR
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 14/01/98   AUTEUR VABHHTS J.PELLET 
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
C ----------------------------------------------------------------------
C     ----------- COMMUNS NORMALISES  JEVEUX  --------------------------
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
      CHARACTER*32   JEXNUM, JEXNOM, JEXATR, JEXR8
C ---------------- FIN COMMUNS NORMALISES  JEVEUX  --------------------
      CHARACTER*4   ACCE2
      CHARACTER*8   TYPACC, UN
      CHARACTER*19  NOMS2
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      NOMS2  = NOMSD
      ACCE2  = ACCES2
      RUNDEF = R8VIDE()
      IUNDEF = ISNNEM()
      UN     = '1       '
C
      IF ( TYPE(1:1) .EQ. 'R' ) THEN
         CALL WKVECT(NOMS2//'.'//ACCE2,'G V R',NBORDR,LINST)
         DO 10 I = 0 , NBORDR-1
            ZR(LINST+I) = RUNDEF
 10      CONTINUE
C
      ELSEIF ( TYPE(1:1) .EQ. 'I' ) THEN
         CALL WKVECT(NOMS2//'.'//ACCE2,'G V I',NBORDR,LINST)
         DO 12 I = 0 , NBORDR-1
            ZI(LINST+I) = IUNDEF
 12      CONTINUE
C
      ELSEIF ( TYPE(1:3) .EQ. 'K80' ) THEN
         CALL WKVECT(NOMS2//'.'//ACCE2,'G V K80',NBORDR,LINST)
C
      ELSEIF ( TYPE(1:3) .EQ. 'K32' ) THEN
         CALL WKVECT(NOMS2//'.'//ACCE2,'G V K32',NBORDR,LINST)
C
      ELSEIF ( TYPE(1:3) .EQ. 'K24' ) THEN
         CALL WKVECT(NOMS2//'.'//ACCE2,'G V K24',NBORDR,LINST)
C
      ELSEIF ( TYPE(1:3) .EQ. 'K16' ) THEN
         CALL WKVECT(NOMS2//'.'//ACCE2,'G V K16',NBORDR,LINST)
C
      ELSEIF ( TYPE(1:2) .EQ. 'K8' ) THEN
         CALL WKVECT(NOMS2//'.'//ACCE2,'G V K8',NBORDR,LINST)
      ENDIF
C
      IF ( VAR(1:1) .EQ. 'A' ) THEN
         TYPACC = 'ACCES'
      ELSEIF ( VAR(1:1) .EQ. 'P' ) THEN
         TYPACC = 'PARA'
      ELSE
         TYPACC = ' '
      ENDIF
      CALL JENONU(JEXNOM(NOMS2//'.NOVA',ACCES1),IBID)
      CALL JEVEUO(JEXNUM(NOMS2//'.TAVA',IBID),'E',IATAVA)
      ZK8(IATAVA-1+1) = '.'//ACCE2
      ZK8(IATAVA-1+2) = UN
      ZK8(IATAVA-1+3) = UN
      ZK8(IATAVA-1+4) = TYPACC
C
      CALL JEDEMA()
      END

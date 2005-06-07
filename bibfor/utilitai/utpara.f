      SUBROUTINE UTPARA ( NOMSD, NBPARA, NBACCE, NOMPAR, NBORDR )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       NOMSD,                 NOMPAR(*)
      INTEGER                                            NBORDR
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
      CHARACTER*8  CH8
      CHARACTER*19 NOMS2
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      NOMS2  = NOMSD
      RUNDEF = R8VIDE()
C
      NBP = NBPARA - NBACCE
      NBPART = NBP * NBORDR
      CALL CODENT ( NBP , 'G' , CH8 )
C
      CALL WKVECT(NOMS2//'.PARA','G V R',NBPART,LPARA)
      DO 10 I = 1,NBPART
         ZR(LPARA+I-1) = RUNDEF
 10   CONTINUE
C
      DO 12 I = NBACCE+1 , NBPARA
         CALL JENONU(JEXNOM(NOMS2//'.NOVA',NOMPAR(I)),IBID)
         CALL JEVEUO(JEXNUM(NOMS2//'.TAVA',IBID),'E',IATAVA)
         ZK8(IATAVA)   = '.PARA'
         CALL CODENT(I-NBACCE,'G',ZK8(IATAVA+1) )
         ZK8(IATAVA+2) = CH8
         ZK8(IATAVA+3) = 'PARA'
 12   CONTINUE
C
      CALL JEDEMA()
      END

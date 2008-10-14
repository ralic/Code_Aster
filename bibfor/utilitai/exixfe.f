      SUBROUTINE EXIXFE ( MODELE, IRET )
      IMPLICIT NONE
      CHARACTER*(*)  MODELE
      INTEGER IRET
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 14/10/2008   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C----------------------------------------------------------------------
C
C     BUT:
C         DETECTER SI LE MODELE EST UNE MODELISATION XFEM.


C     ARGUMENTS:
C     ----------
C
C      ENTREE :
C-------------
C IN   MODELE : NOM DU MODELE
C
C      SORTIE :
C-------------
C OUT  IRET : 0 -> LA MODELISATION N'EST PAS XFEM
C             1 -> LA MODELISATION EST XFEM
C ......................................................................
C
C --------- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------
C
C      CHARACTER*32 JEEXIN
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
C
C --------- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------
C
      INTEGER IFISS
C
C ......................................................................
C
        CALL JEMARQ()
C
        IRET=0
C
        CALL JEEXIN(MODELE(1:8)//'.FISS',IFISS)
        IF (IFISS.NE.0) IRET=1
C
        CALL JEDEMA()
C
      END

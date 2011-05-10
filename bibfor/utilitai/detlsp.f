      SUBROUTINE DETLSP(MATASZ,SOLVEZ)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 09/05/2011   AUTEUR TARDIEU N.TARDIEU 
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
      IMPLICIT      NONE
      CHARACTER*(*) MATASZ,SOLVEZ
C
C ----------------------------------------------------------------------
C
C  DETRUIRE LES INSTANCES MUMPS DU PRECONDITIONNEUR LDLT_SP
C  ***                                              *    **
C
C ----------------------------------------------------------------------
C
C IN  MATASZ : MATRICE ASSEMBLEE
C IN  SOLVEZ : SD SOLVEUR
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ---------------------------
C
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
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX -----------------------------
C
       CHARACTER*19 SOLVEU,MATASS,SOLVBD,METRES,PRECON
       INTEGER      JSLVK,IRET
       REAL*8       R8BID
       COMPLEX*16   CBID
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      SOLVEU = SOLVEZ
      MATASS = MATASZ
C
      CALL JEVEUO(SOLVEU//'.SLVK','L',JSLVK)
      METRES=ZK24(JSLVK)
      IF (METRES.EQ.'PETSC'.OR.METRES.EQ.'GCPC') THEN
        PRECON=ZK24(JSLVK-1+2)
        IF (PRECON.EQ.'LDLT_SP') THEN
          SOLVBD=ZK24(JSLVK-1+3)
          CALL CRSMSP(SOLVBD,MATASS)
          CALL AMUMPH('DETR_MAT',SOLVBD,MATASS,R8BID,CBID,
     &                ' ',0,IRET,.TRUE.)
          CALL DETRSD('SOLVEUR',SOLVBD)
        ENDIF
      ENDIF
C
      CALL JEDEMA()
      END

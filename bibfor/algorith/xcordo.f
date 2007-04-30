      SUBROUTINE XCORDO(MODELE,DEFICO,LCONTX)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 30/04/2007   AUTEUR ABBAS M.ABBAS 
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
C
      IMPLICIT NONE
      CHARACTER*24  DEFICO
      CHARACTER*8   MODELE
      LOGICAL       LCONTX
C      
C ----------------------------------------------------------------------
C
C ROUTINE XFEM (CALCUL)
C
C VERIFIER LA COHERENCE ENTRE LES DONNEES DU CONTACT
C
C ----------------------------------------------------------------------
C
C
C IN  DEFICO : SD DE CONTACT
C IN  MODELE : NOM DU MODELE
C OUT LCONTX : .TRUE. SI CONTACT XFEM
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER      IFISS,NFISS,IRET
      INTEGER      JXC,JXSDC,JNFIS,JMOFIS
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- ACCES A LA SD FISS_XFEM
C
      CALL JEEXIN(MODELE(1:8)//'.FISS',IRET)
C      
      IF (IRET.EQ.0) THEN
        CALL ASSERT(.FALSE.)
      ELSE
        CALL JEVEUO(MODELE(1:8)//'.NFIS'  ,'L',JNFIS)
        CALL JEVEUO(MODELE(1:8)//'.SDCONT','L',JXSDC)
        CALL JEVEUO(MODELE(1:8)//'.CONT'  ,'L',JXC)
        CALL JEVEUO(MODELE(1:8)//'.FISS'  ,'L',JMOFIS)        
      ENDIF
C 
      NFISS  = ZI(JNFIS)
      LCONTX = ZI(JXC-1+1) .NE. 0
C
      DO 100 IFISS = 2,NFISS    
        IF (LCONTX) THEN
          IF (ZI(JXC-1+IFISS) .NE. 0) THEN
            LCONTX = .TRUE.
          ELSE
            CALL U2MESS('F','XFEM_4')
          ENDIF
        ELSE
          IF (ZI(JXC-1+IFISS) .EQ. 0) THEN
            LCONTX = .FALSE.
          ELSE
            CALL U2MESS('F','XFEM_4')
          ENDIF 
        ENDIF
 100  CONTINUE   
C
C --- CREATION SD CONTACT BIDON
C
      IF (.NOT.LCONTX) THEN
        CALL XCOBID(MODELE,DEFICO)
        LCONTX = .TRUE. 
      ENDIF
C
      CALL JEDEMA()
      END

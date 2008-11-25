      SUBROUTINE ASMARI(FONACT,DEFICO,MEELEM,NUMEDD,SOLVEU,
     &                  LISCHA,MATRIG)
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/11/2008   AUTEUR DESOZA T.DESOZA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE MABBAS M.ABBAS

      IMPLICIT NONE
      CHARACTER*19  MEELEM(*)
      CHARACTER*24  NUMEDD,DEFICO
      CHARACTER*19  MATRIG,SOLVEU,LISCHA
      LOGICAL       FONACT(*)
C      
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (CALCUL)
C
C ASSEMBLAGE DE LA MATRICE DE RIGIDITE GLOBALE 
C
C ----------------------------------------------------------------------
C 
C
C IN  FONACT : FONCTIONNALITES ACTIVEES
C IN  MEELEM : ARIABLE CHAPEAU POUR NOM DES MATR_ELEM
C IN  NUMEDD : NOM DE LA NUMEROTATION MECANIQUE
C IN  LISCHA : SD L_CHARGE
C IN  SOLVEU : NOM DU SOLVEUR DE NEWTON
C IN  DEFICO : SD DU CONTACT
C OUT MATRIG : MATRICE DE RIGIDITE ASSEMBLEE
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
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
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER      NBMAT
      REAL*8       R8BID
      INTEGER      IBID
      CHARACTER*19 NMCHEX,MERIGI,MEDIRI,MECTCC,MECTCF
      CHARACTER*19 MEXFEC,MEXFEF,MEXFTC,MEXFTF
      CHARACTER*19 TLIMAT(8)
      CHARACTER*24 K24BID,K24BLA 
      LOGICAL      ISFONC,LCTCC,LCTCF,LXFCM,LTFCM                
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C  
      K24BLA = ' '  
      NBMAT  = 0    
C
C --- FONCTIONNALITES ACTIVEES
C 
      LCTCC  = ISFONC(FONACT,'CONT_CONTINU') 
      LCTCF  = ISFONC(FONACT,'FROT_CONTINU')
      LXFCM  = ISFONC(FONACT,'CONT_XFEM')
      LTFCM = .FALSE.
      IF (LXFCM) THEN
        CALL MMINFP(0    ,DEFICO,K24BLA,'XFEM_GG',
     &              IBID ,R8BID ,K24BID,LTFCM)
      ENDIF 
C
C --- MATR_ELEM RIGIDITE
C
      MERIGI        = NMCHEX(MEELEM,'MEELEM','MERIGI')
      NBMAT         = NBMAT + 1
      TLIMAT(NBMAT) = MERIGI
C
C --- MATR_ELEM DIRICHLET
C      
      MEDIRI        = NMCHEX(MEELEM,'MEELEM','MEDIRI')
      NBMAT         = NBMAT + 1
      TLIMAT(NBMAT) = MEDIRI      
C
C --- MATR_ELEM DE CONTACT/FROTTEMENT METHODE CONTINUE
C
       IF (LCTCC.AND..NOT.LXFCM) THEN
         MECTCC        = NMCHEX(MEELEM,'MEELEM','MECTCC')  
         NBMAT         = NBMAT + 1  
         TLIMAT(NBMAT) = MECTCC   
         IF (LCTCF) THEN
           MECTCF        = NMCHEX(MEELEM,'MEELEM','MECTCF')  
           NBMAT         = NBMAT + 1    
           TLIMAT(NBMAT) = MECTCF    
         ENDIF                
       ENDIF
C
C --- MATR_ELEM DE CONTACT/FROTTEMENT XFEM
C
       IF (LXFCM) THEN       
         IF (LTFCM) THEN       
           MEXFTC        = NMCHEX(MEELEM,'MEELEM','MEXFTC')  
           NBMAT         = NBMAT + 1   
           TLIMAT(NBMAT) = MEXFTC
           MEXFTF        = NMCHEX(MEELEM,'MEELEM','MEXFTF')  
           NBMAT         = NBMAT + 1  
           TLIMAT(NBMAT) = MEXFTF
         ELSE 
           MEXFEC        = NMCHEX(MEELEM,'MEELEM','MEXFEC') 
           NBMAT         = NBMAT + 1  
           TLIMAT(NBMAT) = MEXFEC   
           MEXFEF        = NMCHEX(MEELEM,'MEELEM','MEXFEF')  
           NBMAT         = NBMAT + 1  
           TLIMAT(NBMAT) = MEXFEF
         ENDIF 
       ENDIF  
C
       IF (NBMAT.GT.8) THEN
         CALL ASSERT(.FALSE.)
       ENDIF          
C 
C --- ASSEMBLAGE LISTE DES MATR_ELEM
C         
      CALL ASMATR(NBMAT ,TLIMAT,' ',NUMEDD,SOLVEU,
     &            LISCHA,'ZERO','V',1     ,MATRIG)
C
      CALL JEDEMA()
      END

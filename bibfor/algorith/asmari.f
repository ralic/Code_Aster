      SUBROUTINE ASMARI(FONACT,MEELEM,NUMEDD,SOLVEU,LISCHA,
     &                  MATRIG)
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/12/2009   AUTEUR ABBAS M.ABBAS 
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
      CHARACTER*24  NUMEDD
      CHARACTER*19  MATRIG,SOLVEU,LISCHA
      INTEGER       FONACT(*)
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
C IN  MEELEM : VARIABLE CHAPEAU POUR NOM DES MATR_ELEM
C IN  NUMEDD : NOM DE LA NUMEROTATION MECANIQUE
C IN  LISCHA : SD L_CHARGE
C IN  SOLVEU : NOM DU SOLVEUR DE NEWTON
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
      CHARACTER*19 MERIGI,MEDIRI,MEELTC,MEELTF
      CHARACTER*19 TLIMAT(8)
      LOGICAL      ISFONC,LELTC,LELTF               
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C   
      NBMAT  = 0    
C
C --- FONCTIONNALITES ACTIVEES
C 
      LELTC  = ISFONC(FONACT,'ELT_CONTACT')
      LELTF  = ISFONC(FONACT,'ELT_FROTTEMENT')   
C
C --- MATR_ELEM RIGIDITE
C
      CALL NMCHEX(MEELEM,'MEELEM','MERIGI',MERIGI)
      NBMAT         = NBMAT + 1
      TLIMAT(NBMAT) = MERIGI
C
C --- MATR_ELEM DIRICHLET
C      
      CALL NMCHEX(MEELEM,'MEELEM','MEDIRI',MEDIRI)
      NBMAT         = NBMAT + 1
      TLIMAT(NBMAT) = MEDIRI      
C
C --- MATR_ELEM DE CONTACT/FROTTEMENT 
C
      IF (LELTC) THEN
        CALL NMCHEX(MEELEM,'MEELEM','MEELTC',MEELTC)  
        NBMAT         = NBMAT + 1  
        TLIMAT(NBMAT) = MEELTC   
        IF (LELTF) THEN
          CALL NMCHEX(MEELEM,'MEELEM','MEELTF',MEELTF)  
          NBMAT         = NBMAT + 1    
          TLIMAT(NBMAT) = MEELTF    
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

      SUBROUTINE ASMARI(MERIGI,MEDIRI,NUMEDD,SOLVEU,LISCHA,
     &                  SDCONT,MATRIG)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/10/2006   AUTEUR MABBAS M.ABBAS 
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
      CHARACTER*24       MERIGI,MEDIRI,NUMEDD,SDCONT
      CHARACTER*(*)      MATRIG
      CHARACTER*19       SOLVEU,LISCHA
C      
C ----------------------------------------------------------------------
C
C ROUTINE POUR OP0070 (*_NON_LINE)
C
C ASSEMBLAGE DE LA MATRICE DE RIGIDITE GLOBALE 
C
C ----------------------------------------------------------------------
C 
C      
C MATRIG = MERIGI+MEDIRI
C
C IN  MERIGI : MATRICES ELEMENTAIRES DE RIGIDITE
C IN  MEDIRI : MATRICES ELEMENTAIRES DE DIRICHLET 
C               MATRICES MODIFIEES ET STOCKEES DANS MCONEL POUR
C               METHODE CONTINUE
C IN  NUMEDD : NOM DE LA NUMEROTATION MECANIQUE
C IN  LISCHA : SD L_CHARGE
C IN  SOLVEU : NOM DU SOLVEUR DE NEWTON
C IN  SDCONT : SD DU CONTACT
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
      INTEGER      NBMAT,JMED,IRET
      CHARACTER*8  TLIMAT(3)
      INTEGER      JRESO,JELEM
      CHARACTER*24 RESOCO,MCONEL      
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C --- GLUTE: A VIRER ET REMPLACER PAR CI-DESSOUS DES RESTIT 9987
C
C
C      CALL JEVEUO(SDCONT(1:14)//'.RESOC','L',JRESO)
C      RESOCO = ZK24(JRESO)      
C      CALL JEVEUO(RESOCO(1:14)//'.ELEM','L',JELEM) 
C      MCONEL = ZK24(JELEM)
C      CALL JEEXIN(MCONEL(1:8)//'.LISTE_RESU',IRET)    
C      

      CALL JEVEUO(MEDIRI,'L',JMED)
      IF ( ZK24(JMED)(1:8) .EQ. '        ' ) THEN
        NBMAT = 1
        TLIMAT(1) = MERIGI(1:8)         
      ELSE
        NBMAT = 2
        TLIMAT(1) = MERIGI(1:8)         
        TLIMAT(2) = MEDIRI(1:8)
      ENDIF
C GLUTE     
      CALL JEEXIN('&&CFMMEL.LISTE_RESU',IRET)   
      MCONEL = '&&CFMMEL'
C GLUTE            
      IF (IRET.GT.0) THEN
        NBMAT = NBMAT + 1      
        TLIMAT(NBMAT) = MCONEL(1:8)     
      END IF
C          
      CALL ASMATR(NBMAT,TLIMAT,' ',NUMEDD,SOLVEU,LISCHA,
     &           'ZERO','V',1,MATRIG)
C
      CALL JEDEMA()
      END

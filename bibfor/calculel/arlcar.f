      SUBROUTINE ARLCAR(NOMARL,BASE)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 09/01/2007   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*1 BASE
      CHARACTER*8 NOMARL      
C      
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C CREATION DE LA SD PRINCIPALE ARLEQUIN
C
C ----------------------------------------------------------------------
C
C
C IN  NOMARL : NOM DE LA SD PRINCIPALE ARLEQUIN
C IN  BASE   : TYPE DE BASE ('V' OU 'G')
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER      IBID 
      INTEGER      NHAPP,NHINT,NH 
      INTEGER      ARLGEI    
C      
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- LONGUEURS
C
      NHAPP = ARLGEI(NOMARL,'NHAPP ')
      NHINT = ARLGEI(NOMARL,'NHINT ')      
      NH    = MAX(NHAPP,NHINT)**2
C
C --- CREATION VECTEURS JEVEUX 
C
      CALL WKVECT(NOMARL(1:8)//'.TRAVR',BASE//' V R',12+276*NH,
     &            IBID )
      CALL WKVECT(NOMARL(1:8)//'.TRAVI',BASE//' V I',56+1536*NH,
     &            IBID )
      CALL WKVECT(NOMARL(1:8)//'.TRAVL',BASE//' V L',24*NH,
     &            IBID )  
      CALL WKVECT(NOMARL(1:8)//'.POIDS',BASE//' V K24',1,
     &            IBID )       
C
      CALL JEDEMA()
      END

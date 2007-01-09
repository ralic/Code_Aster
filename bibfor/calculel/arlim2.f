      SUBROUTINE ARLIM2(UNIT,MAIL,NOM1,NOM2,NOMC)
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
      CHARACTER*8  MAIL
      CHARACTER*10 NOM1,NOM2,NOMC
      INTEGER      UNIT
C      
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C IMPRESSION D'UNE SD SPECIFIQUE: MATRICES MORSES
C
C ----------------------------------------------------------------------
C
C
C IN  UNIT   : UNITE D'IMPRESSION 
C IN  MAIL   : NOM UTILISATEUR DU MAILLAGE
C IN  NOM1   : NOM DE LA SD POUR LES MAILLES GROUPE 1 
C IN  NOM2   : NOM DE LA SD POUR LES MAILLES GROUPE 2 
C IN  NOMC   : NOM DE LA SD POUR LE COLLAGE 
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
      CHARACTER*16  NOMMO1,NOMMO2,NOMINO
      INTEGER       IRET
      CHARACTER*8   K8BID
      INTEGER       JDIM1,JDIM2
      INTEGER       N1,N2,I1,I2
      INTEGER       NC,IC
C      
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      NOMMO1 = NOM1(1:10)//'.MORSE'
      NOMMO2 = NOM2(1:10)//'.MORSE'
      NOMINO = NOMC(1:10)//'.INO'
      CALL JEEXIN(NOMINO,IRET)
      IF (IRET.EQ.0) THEN
        WRITE(UNIT,*) '<ARLEQUIN> SD MORSE: <',
     &            NOMINO,'> N''EXISTE PAS'        
      ELSE
        CALL JELIRA(NOMINO,'LONMAX',NC,K8BID)
C
C --- PREMIER GROUPE
C
        CALL JEVEUO(NOMMO1(1:16)//'.DIME','L',JDIM1)
        N1 = ZI(JDIM1)
        WRITE(UNIT,*) '<ARLEQUIN> MATRICE MORSE - GROUP_MA_1...'
        WRITE(UNIT,*) '<ARLEQUIN> ...NOMBRE NOEUDS: ',N1    
        DO 10 IC = 1, NC            
          DO 20 I1 = 1, N1
      
      
  20      CONTINUE            
  10    CONTINUE  
C
C --- SECOND GROUPE
C
        CALL JEVEUO(NOMMO2(1:16)//'.DIME','L',JDIM2)
        N2 = ZI(JDIM2)
        WRITE(UNIT,*) '<ARLEQUIN> MATRICE MORSE - GROUP_MA_2...'
        WRITE(UNIT,*) '<ARLEQUIN> ...NOMBRE NOEUDS: ',N2    
        DO 11 IC = 1, NC            
          DO 21 I2 = 1, N2
      
      
  21      CONTINUE            
  11    CONTINUE
      ENDIF 
C
      CALL JEDEMA()
      END

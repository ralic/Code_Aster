      SUBROUTINE CAVEIS (CHARGZ)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 18/12/2012   AUTEUR SELLENET N.SELLENET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
C
C       CAVEIS -- TRAITEMENT DU MOT CLE FORCE_SOL
C
C      TRAITEMENT DU MOT CLE FORCE_SOL DE AFFE_CHAR_MECA
C
C -------------------------------------------------------
C  CHARGE        - IN    - K8   - : NOM DE LA SD CHARGE
C                - JXVAR -      -   LA  CHARGE STOCKE DANS L'OBJET
C                                   CHAR//'CHME.VEISS'
C -------------------------------------------------------
C
C.========================= DEBUT DES DECLARATIONS ====================
C
C -----  ARGUMENTS
      INCLUDE 'jeveux.h'
      CHARACTER*(*) CHARGZ
C ------ VARIABLES LOCALES
      CHARACTER*8   CHARGE, MAILLE
      CHARACTER*24  OBJ, GNINTF
      INTEGER      IARG
C
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C-----------------------------------------------------------------------
      INTEGER IDVEIS ,IFFOR ,IFMIS ,N1 ,NG ,NM ,NV 

C-----------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL GETFAC('FORCE_SOL',NV)

      IF (NV.EQ.0) GOTO 9999
C
C
      CHARGE = CHARGZ
      OBJ    = CHARGE//'.CHME.VEISS'
C
      CALL WKVECT(OBJ,'G V K24',6,IDVEIS)
      IFMIS=0
      CALL GETVIS('FORCE_SOL','UNITE_RESU_RIGI',1,IARG,1,IFMIS,N1)
      CALL CODENT(IFMIS,'D',ZK24(IDVEIS))
      IFMIS=0
      CALL GETVIS('FORCE_SOL','UNITE_RESU_MASS',1,IARG,1,IFMIS,N1)
      CALL CODENT(IFMIS,'D',ZK24(IDVEIS+1))
      IFMIS=0
      CALL GETVIS('FORCE_SOL','UNITE_RESU_AMOR',1,IARG,1,IFMIS,N1)
      CALL CODENT(IFMIS,'D',ZK24(IDVEIS+2))
      IFFOR=0
      CALL GETVIS('FORCE_SOL','UNITE_RESU_FORC',1,IARG,1,IFFOR,N1)
      CALL CODENT(IFFOR,'D',ZK24(IDVEIS+3))
      GNINTF = ' '
      CALL GETVTX('FORCE_SOL','GROUP_NO_INTERF',1,IARG,1,GNINTF,NG)
      ZK24(IDVEIS+4) = GNINTF
      MAILLE = ' ' 
      CALL GETVTX('FORCE_SOL','SUPER_MAILLE',1,IARG,1,MAILLE,NM) 
      ZK24(IDVEIS+5) = MAILLE
C
9999  CONTINUE
C
      CALL JEDEMA()
C.============================ FIN DE LA ROUTINE ======================
      END

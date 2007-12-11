      SUBROUTINE ARLBOI(MAIL  ,NOMARL,TYPMAI,DIME  ,NOMA  ,
     &                  NOMB  ,NORM  )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 09/01/2007   AUTEUR ABBAS M.ABBAS 
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
      CHARACTER*16 TYPMAI
      CHARACTER*8  MAIL,NOMARL
      CHARACTER*10 NOMA,NOMB,NORM
      INTEGER      DIME
C      
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C MISE EN BOITES DES MAILLES DES MODELES
C
C ----------------------------------------------------------------------
C
C
C IN  MAIL   : NOM DU MAILLAGE
C IN  NOMARL : NOM DE LA SD PRINCIPALE ARLEQUIN
C IN  NOMA   : NOM DE LA SD POUR STOCKAGE MAILLES GROUP_MA_1
C IN  NOMB   : NOM DE LA SD POUR STOCKAGE MAILLES GROUP_MA_2
C IN  NORM   : NOM DE LA SD POUR STOCKAGE DES NORMALES
C IN  TYPMAI : SD CONTENANT NOM DES TYPES ELEMENTS (&&CATA.NOMTM)
C IN  DIME   : DIMENSION DE L'ESPACE GLOBAL
C
C SD PRODUITE: NOMX(1:10) (X VAUT A OU B)  
C     NOMX(1:10)//'.BOITE'   : LISTE DES BOITES ENGLOBANTES
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
      INTEGER      IFM,NIV 
      CHARACTER*16 NOMBOA,NOMBOB  
      CHARACTER*19 NGRMA,NGRMB              
C      
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)      
C
C --- MISE EN BOITES DES MAILLES DES MODELES
C
      IF (NIV.GE.2) THEN
        WRITE(IFM,*) '<ARLEQUIN> GROUP_MA_1 - MISE EN BOITES...'
      ENDIF
C       
      NOMBOA = NOMA(1:10)//'.BOITE'
      NGRMA  = NOMA(1:10)//'.GROUPEMA'
      CALL ARLBO0(MAIL  ,NOMARL,NGRMA ,NORM  ,DIME  ,
     &            TYPMAI,NOMBOA)
C     
      IF (NIV.GE.2) THEN
        WRITE(IFM,*) '<ARLEQUIN> GROUP_MA_1 - BOITES...'
        CALL ARLIMP(IFM,'BOITE',NOMA)
      ENDIF     
C
      IF (NIV.GE.2) THEN
        WRITE(IFM,*) '<ARLEQUIN> GROUP_MA_2 - MISE EN BOITES...'
      ENDIF  
C       
      NOMBOB = NOMB(1:10)//'.BOITE'     
      NGRMB  = NOMB(1:10)//'.GROUPEMA'              
      CALL ARLBO0(MAIL  ,NOMARL,NGRMB ,NORM  ,DIME  ,
     &            TYPMAI,NOMBOB)
C      
      IF (NIV.GE.2) THEN
        WRITE(IFM,*) '<ARLEQUIN> GROUP_MA_2 - BOITES...'
        CALL ARLIMP(IFM,'BOITE',NOMB)
      ENDIF           
C
      CALL JEDEMA()
      END

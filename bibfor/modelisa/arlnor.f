      SUBROUTINE ARLNOR(MOTCLE,IOCC,CINE  ,NORM  ,TANG  ,
     &                  TYPMAI,DIME  )
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
      CHARACTER*16 MOTCLE
      INTEGER      IOCC
      CHARACTER*10 NORM,TANG
      CHARACTER*8  CINE(3)
      INTEGER      DIME
      CHARACTER*16 TYPMAI      
C      
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C LISSAGE DES NORMALES POUR LES COQUES
C
C ----------------------------------------------------------------------
C
C IN  MOTCLE : MOT-CLEF FACTEUR POUR ARLEQUIN
C IN  IOCC   : OCCURRENCE DU MOT CLEF-FACTEUR ARLEQUIN
C IN  MODELE : NOM DU MODELE
C I/O NORM   : NOM DE LA SD POUR STOCKAGE NORMALES
C I/O TANG   : NOM DE LA SD POUR STOCKAGE TANGENTES
C IN  CINE   :  CINEMATIQUE ASSOCIEE AUX MAILLES
C                'SOLIDE' OU 'COQUE'
C                UN POUR CHAQUE GROUPE + ZONE DE COLLAGE
C IN  DIME   : DIMENSION DE L'ESPACE GLOBAL
C IN  TYPMAI : SD CONTENANT NOM DES TYPES ELEMENTS (&&CATA.NOMTM)
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
      INTEGER      NOCC
      CHARACTER*8  CARA
      INTEGER      IRET
      INTEGER      IFM,NIV      
C      
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)      
C
      IF ((CINE(1).EQ.'COQUE   ').OR.(CINE(2).EQ.'COQUE   ')) THEN
        IF (NIV.GE.2) THEN
          WRITE(IFM,*) '<ARLEQUIN> CALCUL DES NORMALES '//
     &               'POUR LES COQUES...'
        ENDIF                             
        CALL JEEXIN(NORM,IRET)
        IF (IRET.EQ.0) THEN
          CALL GETVID(MOTCLE,'CARA_ELEM',IOCC,1,1,CARA,NOCC)    
          IF (NOCC.EQ.0) THEN
            CALL U2MESS('F','ARLEQUIN_8')
          ELSE
            CALL JEEXIN(CARA(1:8)//'.CARCOQUE  .DESC',IRET)
            IF (IRET.EQ.0) THEN
              CALL ASSERT(.FALSE.)
            ELSE
              CALL LISNOR(CARA,DIME,TYPMAI,NORM,TANG)
              IF (NIV.GE.2) THEN
               CALL ARLNIM(IFM,DIME,NORM,TANG)
              ENDIF
            ENDIF  
          ENDIF  
        ENDIF
      ENDIF 
C
      CALL JEDEMA()
      END

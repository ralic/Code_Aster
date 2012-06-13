      SUBROUTINE EXNODE(NOMA  ,MOTFAC,NZOCU ,NNOCU ,NOLINO)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C
      IMPLICIT      NONE
      INCLUDE 'jeveux.h'
      CHARACTER*8   NOMA
      CHARACTER*16  MOTFAC
      INTEGER       NZOCU      
      CHARACTER*24  NOLINO          
      INTEGER       NNOCU          
C     
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (LIAISON_UNILATERALE - LECTURE)
C
C AFFECTATION DES NOEUDS AFFECTES PAR ZONE
C      
C ----------------------------------------------------------------------
C
C
C IN  NOMA   : NOM DU MAILLAGE
C IN  MOTFAC : MOT_CLEF FACTEUR POUR LIAISON UNILATERALE
C IN  NZOCU  : NOMBRE DE ZONES DE LIAISON_UNILATERALE
C OUT NOLINO : NOM DE L'OBJET JEVEUX CONTENANT LA LISTE DES NOEUDS
C IN  NNOCU  : NOMBRE DE TOTAL DE NOEUDS POUR TOUTES LES OCCURRENCES
C
C
C
C
      CHARACTER*8  K8BLA
      INTEGER      IZONE,INO,JDECAL,JLIST
      INTEGER      JNL
      INTEGER      NBMOCL
      CHARACTER*16 LIMOCL(2),TYMOCL(2) 
      CHARACTER*24 LISTMN,LISTNN 
      INTEGER      NBMANO,NBNONO      
      
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C  
      JDECAL = 1 
      NBMOCL = 2 
      K8BLA  = ' '
C
C --- NOM DES SD TEMPORAIRES
C          
      LISTMN = '&&EXNODE.MAIL.NOEU'
      LISTNN = '&&EXNODE.NOEU.NOEU'        
C
C --- CREATION DU VECTEUR
C      
      CALL WKVECT(NOLINO,'V V I',NNOCU ,JNL  )         
C                             
C --- ON LIT LES NOEUDS DES ZONES 
C     
      DO 10 IZONE = 1,NZOCU
        TYMOCL(1) = 'GROUP_MA'
        TYMOCL(2) = 'MAILLE'
        LIMOCL(1) = 'GROUP_MA'
        LIMOCL(2) = 'MAILLE'
        CALL RELIEM(K8BLA ,NOMA  ,'NU_NOEUD',MOTFAC,IZONE ,
     &              NBMOCL,LIMOCL,TYMOCL,LISTMN,NBMANO)
        IF (NBMANO.NE.0) THEN        
          CALL JEVEUO(LISTMN,'L',JLIST)
          DO 60 INO = 1, NBMANO
            ZI(JNL+JDECAL-1) = ZI(JLIST+INO-1)
            JDECAL = JDECAL +1
  60      CONTINUE
        ENDIF
        CALL JEDETR(LISTMN)
           
        TYMOCL(1) = 'GROUP_NO'
        TYMOCL(2) = 'NOEUD'
        LIMOCL(1) = 'GROUP_NO'
        LIMOCL(2) = 'NOEUD'
        CALL RELIEM(K8BLA ,NOMA  ,'NU_NOEUD',MOTFAC,IZONE ,
     &              NBMOCL,LIMOCL,TYMOCL,LISTNN,NBNONO)
        
        IF (NBNONO.NE.0) THEN
          CALL JEVEUO(LISTNN,'L',JLIST)
          DO 61 INO = 1, NBNONO
            ZI(JNL+JDECAL-1) = ZI(JLIST+INO-1)
            JDECAL = JDECAL +1
  61      CONTINUE
        ENDIF
        CALL JEDETR(LISTNN)

 10   CONTINUE 
C
      CALL JEDEMA()
C
      END

      SUBROUTINE DBGCAL(OPTIOZ,IFM,
     &                  NBIN  ,LPAIN ,LCHIN ,
     &                  NBOUT ,LPAOUT,LCHOUT)     
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 30/04/2007   AUTEUR ABBAS M.ABBAS 
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
      CHARACTER*(*) OPTIOZ
      INTEGER       IFM
      INTEGER       NBIN,NBOUT
      CHARACTER*8   LPAIN(NBIN),LPAOUT(NBOUT)      
      CHARACTER*19  LCHIN(NBIN),LCHOUT(NBOUT)           
C      
C ----------------------------------------------------------------------
C
C ROUTINE UTILITAIRE POUR CALCUL
C
C DEBUGAGE DES CHAMPS IN/OUT POUR CALCUL
C
C ----------------------------------------------------------------------
C
C
C IN  OPTION : OPTION CALCULEE
C IN  IFM    : UNITE LOGIQUE D'IMPRESSION
C IN  NBIN   : NOMBRE DE CHAMPS IN
C IN  NBOUT  : NOMBRE DE CHAMPS OUT
C IN  LPAIN  : NOM DES TYPES DE CHAMP D'ENTREE
C IN  LCHIN  : NOM DES CHAMPS D'ENTREE
C IN  LPAOUT : NOM DES TYPES DE CHAMP DE SORTIE
C IN  LCHOUT : NOM DES CHAMPS DE SORTIE
C
C ----------------------------------------------------------------------
C 
      INTEGER      ICH
      CHARACTER*16 OPTION
      
      CHARACTER*8  K8BID
      INTEGER      NBVAL,NBOBJ
C
C ---------------------------------------------------------------------
C     
      OPTION = OPTIOZ
C      
      WRITE(IFM,*) '***** CALCUL DE L OPTION <',OPTION,'>'
      WRITE(IFM,*) ' ** NBRE CHAMPS IN : ',NBIN
      WRITE(IFM,*) ' ** NBRE CHAMPS OUT: ',NBOUT
C     
      WRITE(IFM,*) '***** <CHAMPS_IN>' 
      DO 100 ICH = 1,NBIN
        WRITE(IFM,*) ' * CHAMP IN  <',ICH,'>'
        WRITE(IFM,*) ' * PARAMETRE <',LPAIN(ICH),'>'
        WRITE(IFM,*) ' * CHAMP     <',LCHIN(ICH),'>'
        IF (LPAIN(ICH)(1:1).EQ.' ') THEN
          CALL U2MESI('A','PRECALCUL_60',1,ICH)
        ENDIF
        IF (LCHIN(ICH)(1:1).EQ.' ') THEN
          CALL U2MESI('A','PRECALCUL_61',1,ICH)
        ENDIF       
C        
        CALL JELSTC(' ',LCHIN(ICH)(1:19),1,0,K8BID,NBVAL)
        NBOBJ  = -NBVAL   
        IF (NBOBJ.EQ.0) THEN   
          CALL JELSTC(' ',LCHIN(ICH),1,0,K8BID,NBVAL)
          NBOBJ  = -NBVAL 
          IF (NBOBJ.EQ.0) THEN
            WRITE(IFM,*) ' * SD INTROUVABLE !'
          ELSE  
            WRITE(IFM,*) ' * RESUME DE LA SD :'   
            CALL UTIMSD(IFM,-1,.TRUE.,.TRUE.,LCHIN(ICH),1,' ')
          ENDIF  
        ELSE 
          WRITE(IFM,*) ' * RESUME DE LA SD :' 
          CALL UTIMSD(IFM,-1,.TRUE.,.TRUE.,LCHIN(ICH)(1:19),1,' ')
        ENDIF  
  100 CONTINUE
C  
      WRITE(IFM,*) '***** <CHAMPS_OUT>'   
      DO 200 ICH = 1,NBOUT
        WRITE(IFM,*) ' * CHAMP OUT <',ICH,'>'
        WRITE(IFM,*) ' * PARAMETRE <',LPAOUT(ICH),'>'
        WRITE(IFM,*) ' * CHAMP     <',LCHOUT(ICH),'>'
        IF (LPAOUT(ICH)(1:1).EQ.' ') THEN
          CALL U2MESI('A','PRECALCUL_62',1,ICH)
        ENDIF
        IF (LCHOUT(ICH)(1:1).EQ.' ') THEN
          CALL U2MESI('A','PRECALCUL_63',1,ICH)
        ENDIF 
C
        CALL JELSTC(' ',LCHOUT(ICH)(1:19),1,0,K8BID,NBVAL)
        NBOBJ  = -NBVAL   
        IF (NBOBJ.EQ.0) THEN  
          CALL JELSTC(' ',LCHOUT(ICH),1,0,K8BID,NBVAL)
          NBOBJ  = -NBVAL    
          IF (NBOBJ.EQ.0) THEN
            WRITE(IFM,*) ' * SD INTROUVABLE !'
          ELSE  
            WRITE(IFM,*) ' * RESUME DE LA SD :'   
            CALL UTIMSD(IFM,-1,.TRUE.,.TRUE.,LCHOUT(ICH),1,' ')
          ENDIF
        ELSE  
          WRITE(IFM,*) ' * RESUME DE LA SD :' 
          CALL UTIMSD(IFM,-1,.TRUE.,.TRUE.,LCHOUT(ICH)(1:19),1,' ')
        ENDIF                        
  200 CONTINUE  
      END

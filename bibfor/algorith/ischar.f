      LOGICAL FUNCTION ISCHAR(LISCHA,TYPCHA,SOUTYP,ICHAR )     
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/10/2012   AUTEUR DESOZA T.DESOZA 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*19  LISCHA
      CHARACTER*4   TYPCHA
      CHARACTER*4   SOUTYP     
      INTEGER       ICHAR   
C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (UTILITAIRE)
C
C DIT SI ON A DES CHARGEMENTS DE TYPE DIRICHLET
C      
C ----------------------------------------------------------------------
C
C
C IN  LISCHA : SD L_CHARGES
C IN  TYPCHA : TYPE DE CHARGE
C                'DIRI' - CHARGEMENT DE DIRICHLET
C                'NEUM' - CHARGEMENT DE NEUMANN
C IN  SOUTYP : * POUR LES CHARGEMENTS DE DIRICHLET
C                'DUAL' - PAR DUALISATION (AFFE_CHAR_MECA)
C                'ELIM' - PAR ELIMINATION (AFFE_CHAR_CINE)
C                'DIDI' - DIFFFERENTIEL
C                '    ' - PAS DE SOUS-TYPE
C              * POUR LES CHARGEMENTS DE NEUMANN
C                'ONDE' - ONDE PLANE
C                'SIGM' - SIGMA_INTERNE
C                'LAPL' - FORCE DE LAPLACE
C                'TARD' - ELEMENTS TARDIFS
C                'SUIV' - CHARGEMENT SUIVEUR
C                '    ' - PAS DE SOUS-TYPE
C IN  ICHAR  : INDICE DU CHARGEMENT DANS LA SD
C                 SI ZERO -> ON BOUCLE SUR TOUS LES CHARGEMENTS
C
C
C
C
      INTEGER      IRET,ICHA,DEB,FIN
      CHARACTER*8  K8BID
      INTEGER      NCHAR
      LOGICAL      LDIRI,LELIM,LDUAL,LDIDI
      LOGICAL      LNEUM,LONDE,LLAPL,LSIGM,LELEM,LSUIV     
      CHARACTER*24 CHARGE,INFCHA
      INTEGER      JALICH,JINFCH          
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C 
      ISCHAR = .FALSE.
      LELIM  = .FALSE.
      LDUAL  = .FALSE. 
      LDIRI  = .FALSE.
      LDIDI  = .FALSE.
      LNEUM  = .FALSE. 
      LONDE  = .FALSE. 
      LLAPL  = .FALSE. 
      LSIGM  = .FALSE.
      LSUIV  = .FALSE.                  
      LELEM  = .FALSE.
C
C --- ACCES SD
C
      CHARGE = LISCHA(1:19)//'.LCHA'
      INFCHA = LISCHA(1:19)//'.INFC'
C
      CALL JEEXIN(CHARGE,IRET  )
      IF (IRET.EQ.0) THEN
        ISCHAR = .FALSE.
        GOTO 99
      ELSE
        CALL JEVEUO(INFCHA,'L',JINFCH)
        IF(ZI(JINFCH).EQ.0)THEN
          ISCHAR = .FALSE.
          GOTO 99
        ENDIF
        CALL JELIRA(CHARGE,'LONMAX',NCHAR,K8BID)
        IF (ICHAR.EQ.0) THEN
          DEB    = 1
          FIN    = NCHAR
        ELSE
          IF ((ICHAR.LE.0).OR.(ICHAR.GT.NCHAR)) CALL ASSERT(.FALSE.) 
          DEB    = ICHAR
          FIN    = ICHAR 
        ENDIF  
        
        CALL JEVEUO(CHARGE,'L',JALICH)
C
        DO 10 ICHA = DEB,FIN
C
C ------- DIRICHLETS          
C
          IF (ZI(JINFCH+ICHA).EQ.-1) THEN
            LDIRI  = .TRUE.  
            LELIM  = .TRUE.
          ELSEIF (ZI(JINFCH+ICHA).EQ.-2) THEN
            LDIRI  = .TRUE.  
            LELIM  = .TRUE.        
          ELSEIF (ZI(JINFCH+ICHA).EQ.-3) THEN
            LDIRI  = .TRUE.  
            LELIM  = .TRUE.
          ELSEIF (ZI(JINFCH+ICHA).EQ.1) THEN
            LDIRI  = .TRUE.  
            LDUAL  = .TRUE.
          ELSEIF (ZI(JINFCH+ICHA).EQ.2) THEN
            LDIRI  = .TRUE.  
            LDUAL  = .TRUE.           
          ELSEIF (ZI(JINFCH+ICHA).EQ.3) THEN
            LDIRI  = .TRUE.  
            LDUAL  = .TRUE.            
          ELSEIF (ZI(JINFCH+ICHA).EQ.5) THEN  
            LDIRI  = .TRUE.  
            LDUAL  = .TRUE.          
          ELSEIF (ZI(JINFCH+ICHA).EQ.6) THEN  
            LDIRI  = .TRUE.  
            LDUAL  = .TRUE.
          ELSEIF (ZI(JINFCH+ICHA).EQ.0) THEN 
C
C ------- NEUMANN        
C               
            IF (ZI(JINFCH+NCHAR+ICHA).EQ.1) THEN
              LNEUM  = .TRUE.
            ELSEIF (ZI(JINFCH+NCHAR+ICHA).EQ.2) THEN
              LNEUM  = .TRUE.
            ELSEIF (ZI(JINFCH+NCHAR+ICHA).EQ.3) THEN
              LNEUM  = .TRUE. 
            ELSEIF (ZI(JINFCH+NCHAR+ICHA).EQ.4) THEN
              LNEUM  = .TRUE.
              LSUIV  = .TRUE.               
            ELSEIF (ZI(JINFCH+NCHAR+ICHA).EQ.5) THEN
              LNEUM  = .TRUE.                     
            ELSEIF (ZI(JINFCH+NCHAR+ICHA).EQ.6) THEN
              LNEUM  = .TRUE.
              LONDE  = .TRUE.
            ELSEIF (ZI(JINFCH+NCHAR+ICHA).EQ.55) THEN
              LNEUM  = .TRUE.
              LSIGM  = .TRUE. 
            ELSEIF (ZI(JINFCH+NCHAR+ICHA).EQ.10) THEN
              LNEUM  = .TRUE.
              LELEM  = .TRUE.
            ELSEIF (ZI(JINFCH+NCHAR+ICHA).EQ.20) THEN
              LNEUM  = .TRUE.           
            ELSEIF (ZI(JINFCH+NCHAR+ICHA).EQ.0) THEN
              IF (ZI(JINFCH+2*NCHAR+2).NE.0) THEN
                LNEUM = .TRUE.
                LLAPL = .TRUE.
              ENDIF
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF  
          ELSE
            CALL ASSERT(.FALSE.)  
          ENDIF 
          IF (LDIRI) THEN
            IF (ZI(JINFCH+3*NCHAR+2+ICHA).EQ.1) THEN
              LDIDI = .TRUE.
            ENDIF
          ENDIF 
  10    CONTINUE             
      ENDIF
C
C --- REPONSE SUIVANT QUESTION
C      
      IF (TYPCHA.EQ.'DIRI') THEN
        IF (LDIRI) THEN
          IF (SOUTYP.EQ.'DUAL') THEN
            ISCHAR = LDUAL
          ELSEIF (SOUTYP.EQ.'ELIM') THEN
            ISCHAR = LELIM       
          ELSEIF (SOUTYP.EQ.'DIDI') THEN
            ISCHAR = LDIDI
          ELSEIF (SOUTYP.EQ.'    ') THEN
            ISCHAR = LDIRI
          ELSE
            WRITE(6,*) 'SOUTYP: ',SOUTYP
            CALL ASSERT(.FALSE.)
          ENDIF
        ELSEIF (LNEUM) THEN
          ISCHAR = .FALSE.
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
      ELSEIF (TYPCHA.EQ.'NEUM') THEN
        IF (LNEUM) THEN
          IF (SOUTYP.EQ.'ONDE') THEN
            ISCHAR = LONDE
          ELSEIF (SOUTYP.EQ.'SIGM') THEN
            ISCHAR = LSIGM       
          ELSEIF (SOUTYP.EQ.'LAPL') THEN
            ISCHAR = LLAPL
          ELSEIF (SOUTYP.EQ.'TARD') THEN
            ISCHAR = LELEM  
          ELSEIF (SOUTYP.EQ.'SUIV') THEN
            ISCHAR = LSUIV                     
          ELSEIF (SOUTYP.EQ.'    ') THEN
            ISCHAR = LNEUM
          ELSE
            WRITE(6,*) 'SOUTYP: ',SOUTYP
            CALL ASSERT(.FALSE.)
          ENDIF
        ELSEIF (LDIRI) THEN
          ISCHAR = .FALSE.
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF      
      ELSE
        WRITE(6,*) 'TYPCHA: ',TYPCHA
        CALL ASSERT(.FALSE.)
      ENDIF
C
  99  CONTINUE
C
      CALL JEDEMA()
      END

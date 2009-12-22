      SUBROUTINE CFIMP2(DEFICO,RESOCO,NOMA  ,IFM   ,ILIAI ,
     &                  TYPLIA,TYPOPE,TYPEOU,JEU)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/12/2009   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT     NONE
      INTEGER      IFM
      CHARACTER*8  NOMA
      CHARACTER*24 DEFICO,RESOCO      
      INTEGER      ILIAI
      CHARACTER*2  TYPLIA
      CHARACTER*1  TYPOPE
      CHARACTER*3  TYPEOU
      REAL*8       JEU
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE DISCRETE - APPARIEMENT - UTILITAIRE)
C
C IMPRESSION DE L'ACTIVATION/DESACTIVATION DE LA LIAISON ESCLAVE/MAITRE
C
C ----------------------------------------------------------------------
C
C
C IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
C IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C IN  NOMA   : NOM DU MAILLAGE
C IN  ILIAI  : NUMERO DE LA LIAISON (INDICE DANS LE TABLEAU GLOBAL DE
C              TOUTE LES LIAISONS POSSIBLES -APPARIEES-)
C IN  TYPLIA : TYPE DE LA LIAISON
C                'C0': CONTACT
C                'F0': FROTTEMENT SUIVANT LES DEUX DIRECTIONS
C                       SIMULTANEES (3D)
C                'F1': FROTTEMENT SUIVANT LA PREMIERE DIRECTION (3D)
C                'F2': FROTTEMENT SUIVANT LA SECONDE DIRECTION (3D)
C                'F3': FROTTEMENT (2D)
C IN  TYPOPE : TYPE D'OPERATION DANS LE VECTEUR DES LIAISONS
C                'A' : AJOUTER UNE LIAISON
C                'S' : SUPPRIMER UNE LIAISON
C                'N' : NI AJOUTER, NI SUPPRIMER
C IN  TYPEOU : LIEU OU L'OPERATION A ETE FAITE
C                'ALG' : ALGO PRINCIPAL DE CONTACT
C                'NEG' : SUPPRESSION D'UNE LIAISON A PRESSION NEGATIVE
C                'GLI' : SUPPRESSION D'UNE LIAISON GLISSANTE
C                'ADH' : AJOUT D'UNE LIAISON ADHERENTE
C                'PIV' : SUPPRESSION D'UNE LIAISON A PIVOT NUL 
C                'ALJ' : ALARME LORSQU'UN JEU DEPASSE LA VALEUR SEUIL
C                         DANS LE CAS DU CONTACT GLISSIERE
C                'SIN' : AFFICHAGE DE LA LIAISON PROVOQUANT UNE MATRICE
C                         DE CONTACT SINGULIERE
C                'AGC' : ALARME LORSQUE LE NOMBRE D'ITERATIONS MAX
C                         DU GCP EST DEPASSE
C IN  JEU    : JEU DE LA LIAISON OU LAMBDA DANS LE CAS 'PRE'
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      CHARACTER*24 APPARI,APMEMO
      INTEGER      JAPPAR,JAPMEM
      INTEGER      CFMMVD,ZAPME
      INTEGER      POSNOE,POSAPP
      CHARACTER*8  NOMENT,NOMNOE,NOMMAI
      INTEGER      CODRET
      CHARACTER*40 CHAIAC
      CHARACTER*10 TYPLI
      CHARACTER*4  TYPE2
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ ()
C
C --- ACCES SD CONTACT
C
      APPARI = RESOCO(1:14)//'.APPARI'
      APMEMO = RESOCO(1:14)//'.APMEMO'       
      CALL JEVEUO(APPARI,'L',JAPPAR)
      CALL JEVEUO(APMEMO,'L',JAPMEM)       
      ZAPME  = CFMMVD('ZAPME')
C
C --- REPERAGE DE L'ESCLAVE
C
      POSNOE = ZI(JAPPAR+ILIAI)
C
C --- NOM DU NOEUD ESCLAVE
C
      CALL CFNOMM(NOMA  ,DEFICO,'NOEU',POSNOE,NOMENT,
     &            CODRET)        
      IF (CODRET.EQ.-1) THEN
        NOMNOE = 'ERREUR'
      ELSE  
        NOMNOE = NOMENT
      ENDIF           
C
C --- REPERAGE DU MAITRE
C
      POSAPP = ZI(JAPMEM+ZAPME*(POSNOE-1)+3-1)     
C
C --- NOM ET TYPE DU MAITRE
C
      IF (POSAPP.LT.0) THEN
        CALL CFNOMM(NOMA  ,DEFICO,'NOEU',POSAPP,NOMENT,
     &              CODRET)
        IF (CODRET.LT.0) THEN
          TYPE2  = ' '
          NOMMAI = 'ERREUR'          
        ELSE   
          TYPE2  = '/ND '
          NOMMAI = NOMENT   
        ENDIF         
      ELSEIF (POSAPP.GT.0) THEN
        CALL CFNOMM(NOMA  ,DEFICO,'MAIL',POSAPP,NOMENT,
     &              CODRET)
        IF (CODRET.LT.0) THEN
          TYPE2  = ' '
          NOMMAI = 'ERREUR'          
        ELSE
          TYPE2  = '/EL '
          NOMMAI = NOMENT   
        ENDIF    
      ELSE  
        TYPE2  = ' NON'
        NOMMAI = ' APPARIE'               
      ENDIF         
C
C --- ETAT DE LA LIAISON
C
      IF (TYPOPE.EQ.'A') THEN
        CHAIAC = ' ACTIVEE       (JEU:'
      ELSE IF (TYPOPE.EQ.'S') THEN
        CHAIAC = ' DESACTIVEE    (JEU:'
      ELSE
        CHAIAC = ' INCONNUE      (JEU:'        
      ENDIF
C
C --- TYPE DE LA LIAISON
C      
      IF (TYPLIA.EQ.'C0') THEN
        TYPLI = 'CONTACT   '
      ELSE IF (TYPLIA.EQ.'F0') THEN
        TYPLI = 'FROT. 1&2 '
      ELSE IF (TYPLIA.EQ.'F1') THEN
        TYPLI = 'FROT. 1   '
      ELSE IF (TYPLIA.EQ.'F2') THEN
        TYPLI = 'FROT. 2   '
      ELSE IF (TYPLIA.EQ.'F3') THEN
        TYPLI = 'FROT.     '
      ELSE
        TYPLI = 'ERREUR'  
      ENDIF
C
C --- AFFICHAGE LIAISON
C
      IF (TYPEOU.EQ.'ALG') THEN
        WRITE (IFM,1000) ILIAI,'(',NOMNOE,TYPE2,NOMMAI,'): ',
     &                   CHAIAC,JEU,',TYPE: ',TYPLI,
     &                   ')'
      ELSE IF (TYPEOU.EQ.'PRE') THEN
        CHAIAC = ' PRES. NEGATIVE (MU:'
        WRITE (IFM,1000) ILIAI,'(',NOMNOE,TYPE2,NOMMAI,'): ',
     &                   CHAIAC,JEU,',TYPE: ',TYPLI,
     &                   ')'
      ELSE IF (TYPEOU.EQ.'NEG') THEN
        CHAIAC = ' PRES. NEGATIVE (MU:'
        WRITE (IFM,1000) ILIAI,'(',NOMNOE,TYPE2,NOMMAI,'): ',
     &                   CHAIAC,JEU,',TYPE: ',TYPLI,
     &                   ')'     
      ELSE IF (TYPEOU.EQ.'PIV') THEN
        CHAIAC = ' PIVOT NUL         ('
        WRITE (IFM,1001) ILIAI,'(',NOMNOE,TYPE2,NOMMAI,'): ',
     &                   CHAIAC,' TYPE: ',TYPLI,
     &                   ')'
      ELSE IF (TYPEOU.EQ.'GLI') THEN
        CHAIAC = ' GLISSANTE - SUPP. ('
        WRITE (IFM,1001) ILIAI,'(',NOMNOE,TYPE2,NOMMAI,'): ',
     &                   CHAIAC,' TYPE: ',TYPLI,
     &                   ')'
      ELSE IF (TYPEOU.EQ.'ADH') THEN
        CHAIAC = ' ADHERENTE - ADD.  ('
        WRITE (IFM,1001) ILIAI,'(',NOMNOE,TYPE2,NOMMAI,'): ',
     &                   CHAIAC,' TYPE: ',TYPLI,
     &                   ')'
      ELSE IF (TYPEOU.EQ.'ALJ') THEN
        CHAIAC = ' DECOLLE DU JEU     '
        WRITE (IFM,1002) ILIAI,'(',NOMNOE,TYPE2,NOMMAI,'): ',
     &                   CHAIAC,JEU
      ELSE IF (TYPEOU.EQ.'SIN') THEN
        CHAIAC = ' PIVOT NUL         ('
        WRITE (IFM,1001) ILIAI,'(',NOMNOE,TYPE2,NOMMAI,'): ',
     &                   CHAIAC,' TYPE: ',TYPLI,
     &                   ')'
      ELSE IF (TYPEOU.EQ.'AGC') THEN
        CHAIAC = ') PRESENTE UNE INTERPENETRATION : JEU='
        WRITE (IFM,1003) ILIAI,' (NOEUD ',NOMNOE,
     &                   CHAIAC, JEU
      ENDIF


    
 1000 FORMAT (' <CONTACT> <> LIAISON ',I5,A1,A8,A4,A8,A3,A20,E10.3,
     &         A7,A10,A1)
 1001 FORMAT (' <CONTACT> <> LIAISON ',I5,A1,A8,A4,A8,A3,A20,
     &         A7,A10,A1)
 1002 FORMAT (' <CONTACT> <> LIAISON ',I5,A1,A8,A4,A8,A3,A20,E10.3)
 1003 FORMAT (' <CONTACT> <> LA LIAISON ',I5,A8,A8,A38,E10.3)
C
      CALL JEDEMA()     
C      
      END

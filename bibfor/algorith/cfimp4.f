      SUBROUTINE CFIMP4(DEFICO,RESOCO,NOMA  ,IFM   )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C TOLE CRP_20
C
      IMPLICIT     NONE
      INCLUDE 'jeveux.h'
      CHARACTER*24 DEFICO
      CHARACTER*24 RESOCO
      INTEGER      IFM
      CHARACTER*8  NOMA
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE DISCRETE - APPARIEMENT - UTILITAIRE)
C
C IMPRESSION DES INFOS DETAILLES DE L'APPARIEMENT
C
C ----------------------------------------------------------------------
C
C IN  IFM    : UNITE D'IMPRESSION DU MESSAGE
C IN  NOMA   : NOM DU MAILLAGE
C IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
C IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C
C
C
C
      INTEGER      CFMMVD,ZTACF
      CHARACTER*24 TACFIN
      INTEGER      JTACF
      CHARACTER*24 APCOEF,TANGCO
      INTEGER      JAPCOE,JTANGO
      CHARACTER*24 APCOFR
      INTEGER      JAPCOF
      CHARACTER*24 JEUSUP,JEUITE
      INTEGER      JJEUSU,JJEUIT
      CHARACTER*24 APPOIN,NUMLIA
      INTEGER      JAPPTR,JNUMLI    
      CHARACTER*24 NBDDL,APDDL
      INTEGER      JNBDDL,JAPDDL
      INTEGER      CFDISI,CFDISD
      INTEGER      TYPAPP,ENTAPP
      INTEGER      NZOCO,NTNOE,NBLIAI,NNOCO,NDIMG
      INTEGER      K,IZONE,INO,ILIAI,IDDLE,INOM,IDDLM,IP
      INTEGER      POSNO,POSNOE,POSMAM
      CHARACTER*8  NOMNO,NOMNOE,NOMMAM,NOMNOM,NOMNO2,NOMAPP
      INTEGER      POSNO2
      INTEGER      JDECE,JDECM,JDECNO
      INTEGER      NBDDLT,NBDDLE,NBDDLM,NBNOM
      CHARACTER*4  TYPNO,TYPE2
      CHARACTER*19 SDAPPA
      CHARACTER*16 NOMPT
      REAL*8       JEUOLD,DISSUP
      REAL*8       COEFFF,COEFPN,COEFPT,R8BID
      REAL*8       TAU1(3),TAU2(3),NORM(3)
      LOGICAL      CFCALD,LNODAL,CFDISL,LCTFD,LFROT
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INFOS SUR LA CHARGE DE CONTACT
C
      LCTFD  = CFDISL(DEFICO,'FROT_DISCRET')
      LFROT  = CFDISL(DEFICO,'FROTTEMENT')
C
      APCOEF = RESOCO(1:14)//'.APCOEF'
      JEUSUP = RESOCO(1:14)//'.JSUPCO'
      TACFIN = RESOCO(1:14)//'.TACFIN'
      TANGCO = RESOCO(1:14)//'.TANGCO'         
      NUMLIA = RESOCO(1:14)//'.NUMLIA' 
      APPOIN = RESOCO(1:14)//'.APPOIN'
      NBDDL  = RESOCO(1:14)//'.NBDDL'
      APDDL  = RESOCO(1:14)//'.APDDL'
      JEUITE = RESOCO(1:14)//'.JEUITE'
C
      CALL JEVEUO(APCOEF,'L',JAPCOE)          
      IF (LCTFD) THEN
        APCOFR = RESOCO(1:14)//'.APCOFR'
        CALL JEVEUO(APCOFR,'L',JAPCOF)
      ENDIF
      CALL JEVEUO(JEUSUP,'L',JJEUSU)
      CALL JEVEUO(TACFIN,'L',JTACF )   
      CALL JEVEUO(TANGCO,'L',JTANGO)   
      CALL JEVEUO(NUMLIA,'L',JNUMLI)
      CALL JEVEUO(APPOIN,'L',JAPPTR)
      CALL JEVEUO(NBDDL, 'L',JNBDDL)
      CALL JEVEUO(APDDL ,'L',JAPDDL)
      CALL JEVEUO(JEUITE,'L',JJEUIT)
C
      ZTACF  = CFMMVD('ZTACF')
C
C --- SD APPARIEMENT
C
      SDAPPA = RESOCO(1:14)//'.APPA'
C
C --- INITIALISATIONS
C
      NDIMG  = CFDISI(DEFICO,'NDIM')                
      NZOCO  = CFDISI(DEFICO,'NZOCO') 
      NTNOE  = CFDISI(DEFICO,'NTNOE')
      NBLIAI = CFDISD(RESOCO,'NBLIAI')
      NNOCO  = CFDISI(DEFICO,'NNOCO')
C
      WRITE(IFM,*) '<CONTACT><APPA> RESULTATS DE L''APPARIEMENT' 
C
C ----------------------------------------------------------------------
C --- INFOS SUR LES ZONES DE CONTACT
C ----------------------------------------------------------------------
C
      WRITE(IFM,*) '<CONTACT><APPA> ------ ZONES ------ '

      WRITE(IFM,1000) NZOCO
      WRITE(IFM,1001) NTNOE
      WRITE(IFM,1002) NBLIAI
C
1000  FORMAT (' <CONTACT><APPA> NOMBRE DE ZONES DE CONTACT        : ',
     & I6)
1001  FORMAT (' <CONTACT><APPA> NOMBRE MAXIMAL DE NOEUDS ESCLAVES : ',
     &I6)
1002  FORMAT (' <CONTACT><APPA> NOMBRE EFFECTIF DE LIAISONS       : ',
     &I6)
      
C
C ----------------------------------------------------------------------
C --- INFOS SUR TOUS LES NOEUDS
C ----------------------------------------------------------------------
C
      WRITE(IFM,*) '<CONTACT><APPA> ------ NOEUDS DE CONTACT ------ '

      DO 30 INO = 1,NNOCO
C
C ----- TYPE DU NOEUD
C
        POSNO  = INO      
        CALL CFTYPN(DEFICO,POSNO ,TYPNO )
C
C ----- NOM DU NOEUD
C   
        CALL CFNOMM(NOMA  ,DEFICO,'NOEU',POSNO ,NOMNO )     
C
        WRITE(IFM,3000) INO,NOMNO,TYPNO 
C
C ----- ZONE
C
        CALL CFZONN(DEFICO,POSNO ,IZONE )
C
C ----- RECUPERATIONS DES TANGENTES AU NOEUD
C
        CALL APVECT(SDAPPA,'APPARI_NOEUD_TAU1',POSNO ,TAU1  )
        CALL APVECT(SDAPPA,'APPARI_NOEUD_TAU2',POSNO ,TAU2  )           
C
C ----- NORMALE
C  
        IF (TYPNO.EQ.'MAIT') THEN
          IF (CFCALD(DEFICO,IZONE ,'MAIT')) THEN
            WRITE(IFM,3003) (TAU1(K),K=1,3)
            IF (NDIMG.EQ.3) THEN
              WRITE(IFM,3005) (TAU2(K),K=1,3)
            ENDIF  
            CALL MMNORM(NDIMG,TAU1,TAU2,NORM,R8BID)
            WRITE(IFM,3006) (NORM(K),K=1,3)         
          ELSE
            WRITE(IFM,3007)              
          ENDIF  
        ELSE
          IF (CFCALD(DEFICO,IZONE ,'ESCL')) THEN
            WRITE(IFM,3003) (TAU1(K),K=1,3)
            IF (NDIMG.EQ.3) THEN
              WRITE(IFM,3005) (TAU2(K),K=1,3)
            ENDIF            
            CALL MMNORM(NDIMG,TAU1,TAU2,NORM,R8BID)  
            WRITE(IFM,3006) (NORM(K),K=1,3)
          ELSE
            WRITE(IFM,3007)    
          ENDIF                  
        ENDIF
        
        
3000  FORMAT (' <CONTACT><APPA> NOEUD NUMERO ',I6,' (',A8,') -> NOEUD ',
     &           A4)
3003  FORMAT (' <CONTACT><APPA>  * TANGENTE 1  : ',3(1PE15.8,2X))
3005  FORMAT (' <CONTACT><APPA>  * TANGENTE 2  : ',3(1PE15.8,2X))
3006  FORMAT (' <CONTACT><APPA>  * NORMALE     : ',3(1PE15.8,2X))
3007  FORMAT (' <CONTACT><APPA>  * TANGENTE ET NORMALE NON CALCULEES')


  30  CONTINUE
C
C ----------------------------------------------------------------------
C --- INFOS SUR LES NOEUDS ESCLAVES
C ----------------------------------------------------------------------
C
      WRITE(IFM,*) '<CONTACT><APPA> ----- NOEUDS ESCLAVES ----- '

      DO 40 ILIAI = 1,NBLIAI
C
C ----- POINT DE CONTACT
C      
        IP     = ZI(JNUMLI+4*(ILIAI-1)+1-1)
C
C ----- NOEUD ESCLAVE
C      
        POSNOE = ZI(JNUMLI+4*(ILIAI-1)+2-1)        
C
C ----- INFOS APPARIEMENT
C
        CALL APINFI(SDAPPA,'APPARI_TYPE'  ,IP    ,TYPAPP )
        CALL APINFI(SDAPPA,'APPARI_ENTITE',IP    ,ENTAPP )
        CALL APINFI(SDAPPA,'APPARI_ZONE'  ,IP    ,IZONE  ) 
C
C ----- NOM DU NOEUD ESCLAVE
C
        CALL APNOMP(SDAPPA,IP    ,NOMPT ) 
        NOMNOE = NOMPT(9:16)
        WRITE(IFM,4000) ILIAI,NOMPT
C
C ----- NOM ET TYPE DU MAITRE
C
        CALL CFNOAP(NOMA  ,DEFICO,TYPAPP,ENTAPP,NOMAPP,
     &              TYPE2 )
        IF (TYPAPP.LT.0) THEN
          WRITE(IFM,4003)
          LNODAL = .FALSE.
        ELSEIF (TYPAPP.EQ.1) THEN
          WRITE(IFM,4001) NOMAPP
          NOMNOM = NOMAPP
          LNODAL = .TRUE.
        ELSEIF (TYPAPP.EQ.2) THEN
          WRITE(IFM,4002) NOMAPP
          NOMMAM = NOMAPP
          LNODAL = .FALSE.
        ELSE
          CALL ASSERT(.FALSE.)  
        ENDIF
C
C --- NOMBRE DE DDLS TOTAL: NBDDLT
C
        NBDDLT = ZI(JAPPTR+ILIAI)  - ZI(JAPPTR+ILIAI-1)
        NBDDLE = ZI(JNBDDL+POSNOE) - ZI(JNBDDL+POSNOE-1)       
C
C --- AFFICHAGES
C
        WRITE(IFM,4004) NBDDLT,NBDDLE
C
C ----- TANGENTES ET NORMALE
C        
        TAU1(1) = ZR(JTANGO+6*(ILIAI-1)+1-1)
        TAU1(2) = ZR(JTANGO+6*(ILIAI-1)+2-1)
        TAU1(3) = ZR(JTANGO+6*(ILIAI-1)+3-1)
        TAU2(1) = ZR(JTANGO+6*(ILIAI-1)+4-1)
        TAU2(2) = ZR(JTANGO+6*(ILIAI-1)+5-1)
        TAU2(3) = ZR(JTANGO+6*(ILIAI-1)+6-1) 
        CALL MMNORM(NDIMG,TAU1,TAU2,NORM,R8BID)
        WRITE(IFM,4007) (NORM(K),K=1,3)
        WRITE(IFM,4008) (TAU1(K),K=1,3)
        IF (NDIMG.EQ.3) THEN
          WRITE(IFM,5008) (TAU2(K),K=1,3)
        ENDIF
C
C ----- JEUX
C
        JEUOLD = ZR(JJEUIT+3*(ILIAI-1)+1-1)
        DISSUP = ZR(JJEUSU+ILIAI-1)
        WRITE(IFM,4006) JEUOLD,DISSUP  
C 
C ----- PARAMETRES PENALISATION ET FROTTEMENT
C 
        COEFFF = ZR(JTACF+ZTACF*(ILIAI-1)+0)
        COEFPN = ZR(JTACF+ZTACF*(ILIAI-1)+1)
        COEFPT = ZR(JTACF+ZTACF*(ILIAI-1)+2)
        WRITE(IFM,7000) COEFPN
        WRITE(IFM,7001) COEFPT
        WRITE(IFM,7003) COEFFF       
C
C ----- DDL ET COEF CONTACT POUR NOEUD ESCLAVE
C
        JDECE = ZI(JAPPTR+ILIAI-1)
        IF (NDIMG.EQ.3) THEN
          WRITE (IFM,4015) NOMNOE,
     &      (ZI(JAPDDL+JDECE+IDDLE-1),IDDLE=1,NBDDLE),
     &      (ZR(JAPCOE+JDECE+IDDLE-1),IDDLE=1,NBDDLE)
        ELSE
          WRITE (IFM,4018) NOMNOE,
     &      (ZI(JAPDDL+JDECE+IDDLE-1),IDDLE=1,NBDDLE),
     &      (ZR(JAPCOE+JDECE+IDDLE-1),IDDLE=1,NBDDLE)        
        ENDIF
C
C ----- DDL ET COEF FROTTEMENT POUR NOEUD ESCLAVE
C        
        JDECE = ZI(JAPPTR+ILIAI-1)
        IF (LFROT) THEN
          IF (NDIMG.EQ.3) THEN
            WRITE (IFM,4016) NOMNOE,
     &        (ZI(JAPDDL+JDECE+IDDLE-1),IDDLE=1,NBDDLE),
     &        (ZR(JAPCOF+JDECE+IDDLE-1),IDDLE=1,NBDDLE)
            WRITE (IFM,4017) NOMNOE,
     &        (ZI(JAPDDL+JDECE+IDDLE-1),IDDLE=1,NBDDLE),
     &        (ZR(JAPCOF+30*NTNOE+JDECE+IDDLE-1),IDDLE=1,NBDDLE)     
          ELSE
            WRITE (IFM,4019) NOMNOE,
     &        (ZI(JAPDDL+JDECE+IDDLE-1),IDDLE=1,NBDDLE),
     &        (ZR(JAPCOF+JDECE+IDDLE-1),IDDLE=1,NBDDLE)        
          ENDIF
        ENDIF
C
C ----------------------------------------------------------------------
C --- DDL ET COEF POUR NOEUDS MAITRES
C ----------------------------------------------------------------------
C
        JDECM  = JDECE + NBDDLE
C 
C ----- APPARIEMENT NODAL
C
        IF (LNODAL) THEN  
          NBNOM  = 1
          INOM   = 1
          NBDDLM  = NBDDLT - NBDDLE
C
C ------- COEFFICIENTS POUR CONTACT
C          
          IF (NDIMG.EQ.3) THEN
            WRITE (IFM,5011) NOMNOM,
     &        (ZI(JAPDDL+JDECM+(INOM-1)*NBDDLM+IDDLM-1),
     &             IDDLM=1,NBDDLM),
     &        (ZR(JAPCOE+JDECM+(INOM-1)*NBDDLM+IDDLM-1),
     &             IDDLM=1,NBDDLM) 
          ELSE
            WRITE (IFM,6011) NOMNOM,
     &        (ZI(JAPDDL+JDECM+(INOM-1)*NBDDLM+IDDLM-1),
     &             IDDLM=1,NBDDLM),
     &        (ZR(JAPCOE+JDECM+(INOM-1)*NBDDLM+IDDLM-1),
     &             IDDLM=1,NBDDLM)
          ENDIF
C
C ------- COEFFICIENTS POUR FROTTEMENT 
C
          IF (LFROT) THEN
            IF (NDIMG.EQ.3) THEN
              WRITE (IFM,5012) NOMNOM,
     &         (ZI(JAPDDL+JDECM+(INOM-1)*NBDDLM+IDDLM-1),
     &              IDDLM=1,NBDDLM),
     &         (ZR(JAPCOE+JDECM+(INOM-1)*NBDDLM+IDDLM-1),
     &              IDDLM=1,NBDDLM)
              WRITE (IFM,5013) NOMNOM,
     &         (ZI(JAPDDL+JDECM+(INOM-1)*NBDDLM+IDDLM-1),
     &              IDDLM=1,NBDDLM),
     &         (ZR(JAPCOF+30*NTNOE+JDECM+(INOM-1)*NBDDLM+IDDLM-1),
     &              IDDLM=1,NBDDLM)
            ELSE
              WRITE (IFM,6012) NOMNOM,
     &         (ZI(JAPDDL+JDECM+(INOM-1)*NBDDLM+IDDLM-1),
     &              IDDLM=1,NBDDLM),
     &         (ZR(JAPCOE+JDECM+(INOM-1)*NBDDLM+IDDLM-1),
     &              IDDLM=1,NBDDLM)
            ENDIF
          ENDIF
C 
C ----- APPARIEMENT MAITRE/ESCLAVE
C
        ELSE
          POSMAM = ENTAPP
          CALL CFNBEN(DEFICO,POSMAM,'CONNEX',NBNOM ,JDECNO)
          DO 50 INOM = 1,NBNOM 
            CALL CFCONN(DEFICO,JDECNO,INOM  ,POSNO2)  
            NBDDLM = ZI(JNBDDL+POSNO2) - ZI(JNBDDL+POSNO2-1)
C   
            CALL CFNOMM(NOMA  ,DEFICO,'NOEU',POSNO2,NOMNO2)        
C
C --- COEFFICIENTS POUR CONTACT
C             
            IF (NDIMG.EQ.3) THEN
              WRITE (IFM,5001) NOMMAM,NOMNO2,
     &        (ZI(JAPDDL+JDECM+(INOM-1)*NBDDLM+IDDLM-1),
     &             IDDLM=1,NBDDLM),
     &        (ZR(JAPCOE+JDECM+(INOM-1)*NBDDLM+IDDLM-1),
     &             IDDLM=1,NBDDLM)
            ELSE
              WRITE (IFM,6001) NOMMAM,NOMNO2,
     &        (ZI(JAPDDL+JDECM+(INOM-1)*NBDDLM+IDDLM-1),
     &             IDDLM=1,NBDDLM),
     &        (ZR(JAPCOE+JDECM+(INOM-1)*NBDDLM+IDDLM-1),
     &             IDDLM=1,NBDDLM)
            ENDIF
C
C --- COEFFICIENTS POUR FROTTEMENT 
C
            IF (LFROT) THEN
              IF (NDIMG.EQ.3) THEN            
                WRITE (IFM,5002) NOMMAM,NOMNO2,
     &          (ZI(JAPDDL+JDECM+(INOM-1)*NBDDLM+IDDLM-1),
     &              IDDLM=1,NBDDLM),
     &          (ZR(JAPCOE+JDECM+(INOM-1)*NBDDLM+IDDLM-1),
     &              IDDLM=1,NBDDLM)
                WRITE (IFM,5003) NOMMAM,NOMNO2,
     &          (ZI(JAPDDL+JDECM+(INOM-1)*NBDDLM+IDDLM-1),
     &              IDDLM=1,NBDDLM),
     &          (ZR(JAPCOF+30*NTNOE+JDECM+(INOM-1)*NBDDLM+IDDLM-1),
     &              IDDLM=1,NBDDLM)
              ELSE
                WRITE (IFM,6002) NOMMAM,NOMNO2,
     &          (ZI(JAPDDL+JDECM+(INOM-1)*NBDDLM+IDDLM-1),
     &               IDDLM=1,NBDDLM),
     &          (ZR(JAPCOE+JDECM+(INOM-1)*NBDDLM+IDDLM-1),
     &               IDDLM=1,NBDDLM)
              ENDIF
            ENDIF
   50     CONTINUE

        ENDIF
     
  40  CONTINUE

4000  FORMAT (' <CONTACT><APPA> LIAISON NUMERO ',I6,' (',
     &        A16,')') 

4001  FORMAT (' <CONTACT><APPA>  * APPARIEMENT AVEC NOEUD  ',A8)
4002  FORMAT (' <CONTACT><APPA>  * APPARIEMENT AVEC MAILLE ',A8)
4003  FORMAT (' <CONTACT><APPA>  * NON APPARIE')
4004  FORMAT (' <CONTACT><APPA>  * NOMBRE DE DDLS : ',I6,' DONT ',I6,
     &                        ' POUR NOEUD ESCLAVE',I6)

4006  FORMAT (' <CONTACT><APPA>  * JEU: ',1PE15.8,' DONT :',1PE15.8,
     &        ' VENANT DE DIST_*')
4007  FORMAT (' <CONTACT><APPA>  * NORMALE LISSEE/MOYENNEE: ',
     &         3(1PE15.8,2X))
4008  FORMAT (' <CONTACT><APPA>  * TANGENTE DIRECTION 1   : ',
     &         3(1PE15.8,2X))
5008  FORMAT (' <CONTACT><APPA>  * TANGENTE DIRECTION 2   : ',
     &         3(1PE15.8,2X))

C
C --- 3D - ESCLAVE
C
4015  FORMAT ((' <CONTACT><APPA>  * DDL ESCL. CONTACT ( ',A8,'):',
     &           3(I8,2X),' / ',
     &           3(1PE15.8,2X)))

4016  FORMAT ((' <CONTACT><APPA>  * DDL ESCL. FROT1   ( ',A8,'):',
     &           3(I8,2X),' / ',
     &           3(1PE15.8,2X)))
4017  FORMAT ((' <CONTACT><APPA>  * DDL ESCL. FROT2   ( ',A8,'):',
     &           3(I8,2X),' / ',
     &           3(1PE15.8,2X)))
     
C
C --- 2D - ESCLAVE
C     
4018  FORMAT ((' <CONTACT><APPA>  * DDL ESCL. CONTACT ( ',A8,'):',
     &           2(I8,2X),' / ',
     &           2(1PE15.8,2X)))

4019  FORMAT ((' <CONTACT><APPA>  * DDL ESCL. FROT1   ( ',A8,'):',
     &           2(I8,2X),' / ',
     &           2(1PE15.8,2X)))
   
C
C --- 3D MAITRE/ESCL - MAITRE
C
5001  FORMAT ((' <CONTACT><APPA>  * DDL MAIT. CONTACT ( ',A8,
     &           '/',A8,'):',
     &           3(I8,2X),' / ',
     &           3(1PE15.8,2X)))
5002  FORMAT ((' <CONTACT><APPA>  * DDL MAIT. FROT1   ( ',A8,
     &           '/',A8,'):',
     &           3(I8,2X),' / ',
     &           3(1PE15.8,2X)))
5003  FORMAT ((' <CONTACT><APPA>  * DDL MAIT. FROT2   ( ',A8,
     &           '/',A8,'):',
     &           3(I8,2X),' / ',
     &           3(1PE15.8,2X)))
C
C --- 3D NODAL - MAITRE
C    
5011  FORMAT ((' <CONTACT><APPA>  * DDL MAIT. CONTACT ( ',A8,'):',
     &           3(I8,2X),' / ',
     &           3(1PE15.8,2X)))
5012  FORMAT ((' <CONTACT><APPA>  * DDL MAIT. FROT1   ( ',A8,'):',
     &           3(I8,2X),' / ',
     &           3(1PE15.8,2X)))
5013  FORMAT ((' <CONTACT><APPA>  * DDL MAIT. FROT2   ( ',A8,'):',
     &           3(I8,2X),' / ',
     &           3(1PE15.8,2X)))  
C
C --- 2D MAITRE/ESCL - MAITRE
C     
6001  FORMAT ((' <CONTACT><APPA>  * DDL MAIT. CONTACT ( ',A8,
     &           '/',A8,'):',
     &           2(I8,2X),' / ',
     &           2(1PE15.8,2X)))
6002  FORMAT ((' <CONTACT><APPA>  * DDL MAIT. FROT1   ( ',A8,
     &           '/',A8,'):',
     &           2(I8,2X),' / ',
     &           2(1PE15.8,2X)))
C
C --- 2D NODAL - MAITRE
C
6011  FORMAT ((' <CONTACT><APPA>  * DDL MAIT. CONTACT ( ',A8,'):',
     &           2(I8,2X),' / ',
     &           2(1PE15.8,2X)))
6012  FORMAT ((' <CONTACT><APPA>  * DDL MAIT. FROT1   ( ',A8,'):',
     &           2(I8,2X),' / ',
     &           2(1PE15.8,2X)))
C
7000  FORMAT (' <CONTACT><APPA>  * E_N              :',1PE15.8)
7001  FORMAT (' <CONTACT><APPA>  * E_T              :',1PE15.8)
7003  FORMAT (' <CONTACT><APPA>  * COULOMB          :',1PE15.8)
C
      
C
      CALL JEDEMA()
C
      END

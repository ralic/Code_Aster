      SUBROUTINE CFIMP4(DEFICO,RESOCO,NOMA  ,IFM   )
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
C TOLE CRP_20
C
      IMPLICIT     NONE
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
      INTEGER      CFMMVD,ZAPME,ZTACF
      CHARACTER*24 APPARI,APJEU,APMEMO,TACFIN
      INTEGER      JAPPAR,JAPJEU,JAPMEM,JTACF
      CHARACTER*24 APPOIN,APCOEF,TGNOEU,TANGCO
      INTEGER      JAPPTR,JAPCOE,JTGNOE,JTANGO
      CHARACTER*24 APCOFR,PNOMA,NOMACO
      INTEGER      JAPCOF,JPONO,JNOMA
      CHARACTER*24 NOZOCO,APDDL,PDDLCO,JEUSUP
      INTEGER      JZOCO,JAPDDL,JPDDL,JJSUP
      INTEGER      CFDISI,CFDISD
      INTEGER      NZOCO,NTNOE,NBLIAI,NNOCO,NDIM
      INTEGER      I,K,IZONE,INO,ILIAI,IDDLE,IMAIT,IDDLM
      INTEGER      POSNO,POSNOE,POSAPP,POSMA2,POSNOM,POSMAM
      CHARACTER*8  NOMNO,NOMNOE,NOMMAM,NOMNOM,NOMMA2
      INTEGER      JDECE,JDECM
      INTEGER      CODRET
      INTEGER      NDDLT,NDDLE,NDDLM,NBNOM
      CHARACTER*20 TYPNO
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
      APPARI = RESOCO(1:14)//'.APPARI'
      APDDL  = RESOCO(1:14)//'.APDDL'
      APCOEF = RESOCO(1:14)//'.APCOEF'
      PDDLCO = RESOCO(1:14)//'.PDDLCO'
      APJEU  = RESOCO(1:14)//'.APJEU'
      APMEMO = RESOCO(1:14)//'.APMEMO'
      APPOIN = RESOCO(1:14)//'.APPOIN'
      NOMACO = DEFICO(1:16)//'.NOMACO'
      PNOMA  = DEFICO(1:16)//'.PNOMACO'           
      NOZOCO = DEFICO(1:16)//'.NOZOCO'  
      JEUSUP = RESOCO(1:14)//'.JSUPCO'
      TACFIN = RESOCO(1:14)//'.TACFIN'   
      TGNOEU = RESOCO(1:14)//'.TGNOEU' 
      TANGCO = RESOCO(1:14)//'.TANGCO'           
C
      CALL JEVEUO(NOZOCO,'L',JZOCO ) 
      CALL JEVEUO(APPARI,'L',JAPPAR)
      CALL JEVEUO(APJEU, 'L',JAPJEU)
      CALL JEVEUO(APMEMO,'L',JAPMEM)
      CALL JEVEUO(APPOIN,'L',JAPPTR)
      CALL JEVEUO(PDDLCO,'L',JPDDL )
      CALL JEVEUO(APDDL ,'L',JAPDDL)
      CALL JEVEUO(APCOEF,'L',JAPCOE)
      CALL JEVEUO(PNOMA ,'L',JPONO )
      CALL JEVEUO(NOMACO,'L',JNOMA )           
      IF (LCTFD) THEN
        APCOFR = RESOCO(1:14)//'.APCOFR'
        CALL JEVEUO(APCOFR,'L',JAPCOF)
      ENDIF
      CALL JEVEUO(JEUSUP,'L',JJSUP )
      CALL JEVEUO(TACFIN,'L',JTACF )  
      CALL JEVEUO(TGNOEU,'L',JTGNOE) 
      CALL JEVEUO(TANGCO,'L',JTANGO)          
C
      ZAPME  = CFMMVD('ZAPME')
      ZTACF  = CFMMVD('ZTACF')
C
C --- INITIALISATIONS
C
      NDIM   = CFDISI(DEFICO,'NDIM')                
      NZOCO  = CFDISI(DEFICO,'NZOCO') 
      NTNOE  = CFDISI(DEFICO,'NTNOE')
      NBLIAI = CFDISD(RESOCO,'NBLIAI')
      NNOCO  = CFDISI(DEFICO,'NNOCO')
C
      WRITE(IFM,*) '<CONTACT_DVLP> *** APPARIEMENT *** '
C
C ----------------------------------------------------------------------
C --- INFOS SUR LES ZONES DE CONTACT
C ----------------------------------------------------------------------
C
      WRITE(IFM,*) '<CONTACT_DVLP> ------ ZONES ------ '

      WRITE(IFM,1000) NZOCO
      WRITE(IFM,1001) NTNOE
      WRITE(IFM,1002) NBLIAI
C
1000  FORMAT (' <CONTACT_DVLP> NOMBRE DE ZONES DE CONTACT        : ',I6)
1001  FORMAT (' <CONTACT_DVLP> NOMBRE MAXIMAL DE NOEUDS ESCLAVES : ',I6)
1002  FORMAT (' <CONTACT_DVLP> NOMBRE EFFECTIF DE LIAISONS       : ',I6)
      
C
C ----------------------------------------------------------------------
C --- INFOS SUR TOUS LES NOEUDS
C ----------------------------------------------------------------------
C
      WRITE(IFM,*) '<CONTACT_DVLP> ------ NOEUDS DE CONTACT ------ '

      DO 30 INO = 1,NNOCO
        POSNO  = INO
        IZONE  = ZI(JZOCO+POSNO-1)       
        CALL CFNOMM(NOMA  ,DEFICO,'NOEU',POSNO ,NOMNO ,
     &              CODRET)
     
        IF (ZI(JAPMEM+ZAPME*(POSNO-1)+1-1).EQ.1) THEN
           TYPNO  = 'ESCLAVE             '
        ELSEIF (ZI(JAPMEM+ZAPME*(POSNO-1)+1-1).EQ.2) THEN
           TYPNO  = 'ESCLAVE             '           
        ELSE IF (ZI(JAPMEM+ZAPME*(POSNO-1)+1-1).EQ.0) THEN
           TYPNO  = 'MAITRE              '
        ELSE IF (ZI(JAPMEM+ZAPME*(POSNO-1)+1-1).EQ.-1) THEN
           TYPNO  = 'EXCLU -SANS_NOEUD   '
        ELSE IF (ZI(JAPMEM+ZAPME*(POSNO-1)+1-1).EQ.-2) THEN
           TYPNO  = 'EXCLU -TOLE_APPA    ' 
        ELSE IF (ZI(JAPMEM+ZAPME*(POSNO-1)+1-1).EQ.-3) THEN
           TYPNO  = 'EXCLU -TOLE_PROJ_EXT'                              
        ELSE
           TYPNO  = 'ETAT INCONNU        '  
        ENDIF
        
        IF (CODRET.LT.0) THEN
          NOMNO  = 'ERREUR'
        ENDIF

        WRITE(IFM,3000) INO,NOMNO,TYPNO 
        
        DO 10 I = 1,3
          TAU1(I) = ZR(JTGNOE+6*(POSNO-1)+I-1)   
          TAU2(I) = ZR(JTGNOE+6*(POSNO-1)+3+I-1)
 10     CONTINUE      
  
        IF (TYPNO.EQ.'MAITRE') THEN
          IF (CFCALD(DEFICO,IZONE ,'MAIT')) THEN
            WRITE(IFM,3003) (TAU1(K),K=1,3)
            IF (NDIM.EQ.3) THEN
              WRITE(IFM,3005) (TAU2(K),K=1,3)
            ENDIF  
            CALL MMNORM(NDIM,TAU1,TAU2,NORM,R8BID)
            WRITE(IFM,3006) (NORM(K),K=1,3)         
          ELSE
            WRITE(IFM,3007)              
          ENDIF  
        ELSE
          IF (CFCALD(DEFICO,IZONE ,'ESCL')) THEN
            WRITE(IFM,3003) (TAU1(K),K=1,3)
            IF (NDIM.EQ.3) THEN
              WRITE(IFM,3005) (TAU2(K),K=1,3)
            ENDIF            
            CALL MMNORM(NDIM,TAU1,TAU2,NORM,R8BID)  
            WRITE(IFM,3006) (NORM(K),K=1,3)
          ELSE
            WRITE(IFM,3007)    
          ENDIF                  
        ENDIF
        
        
3000  FORMAT (' <CONTACT_DVLP> NOEUD NUMERO ',I6,' (',A8,') -> NOEUD ',
     &           A20)
3003  FORMAT (' <CONTACT_DVLP>  * TANGENTE 1  : ',3(1PE15.8,2X))
3005  FORMAT (' <CONTACT_DVLP>  * TANGENTE 2  : ',3(1PE15.8,2X))
3006  FORMAT (' <CONTACT_DVLP>  * NORMALE     : ',3(1PE15.8,2X))
3007  FORMAT (' <CONTACT_DVLP>  * TANGENTE ET NORMALE NON CALCULEES')


  30  CONTINUE
C
C ----------------------------------------------------------------------
C --- INFOS SUR LES NOEUDS ESCLAVES
C ----------------------------------------------------------------------
C
      WRITE(IFM,*) '<CONTACT_DVLP> ----- NOEUDS ESCLAVES ----- '

      DO 40 ILIAI = 1,NBLIAI
C
C --- INFO SUR NOEUD ESCLAVE
C      
        POSNOE = ZI(JAPPAR+ILIAI)
        CALL CFNOMM(NOMA  ,DEFICO,'NOEU',POSNOE,NOMNOE,
     &              CODRET)
        
        IF (CODRET.LT.0) THEN
          GOTO 40
        ELSE 
          WRITE(IFM,4000) ILIAI,NOMNOE
        ENDIF  
C
C --- TYPE D'APPARIEMENT
C       
        POSAPP = ZI(JAPMEM+ZAPME*(POSNOE-1)+3-1)
        IF (POSAPP.EQ.0) THEN
          WRITE(IFM,4009)
          GOTO 40
        ELSE
C
C --- ENTITE APPARIEE
C        
          IF (POSAPP.LT.0) THEN
            POSNOM = ABS(ZI(JAPMEM+ZAPME*(POSNOE-1)+3-1))
            CALL CFNOMM(NOMA  ,DEFICO,'NOEU',POSNOM,NOMNOM,
     &                  CODRET)
            LNODAL = .TRUE.
          ELSEIF (POSAPP.GT.0) THEN
            POSMAM = ZI(JAPMEM+ZAPME*(POSNOE-1)+3-1)        
            CALL CFNOMM(NOMA  ,DEFICO,'MAIL',POSMAM,NOMMAM,
     &                  CODRET)
            LNODAL = .FALSE.
          ELSE
            LNODAL = .FALSE.
            CODRET = -1
          ENDIF
          
          
          IF (CODRET.LT.0) THEN
            GOTO 40
          ENDIF

          IF (LNODAL) THEN
            WRITE(IFM,4001) NOMNOM
          ELSE
            CALL CFNOMM(NOMA  ,DEFICO,'NOEU',POSNOM,NOMNOM,
     &                  CODRET)
            WRITE(IFM,4002) NOMMAM
          ENDIF  
        ENDIF    
        JDECE = ZI(JAPPTR+ILIAI-1)
C
C --- NOMBRE DE DDL TOTAL: NDDLT
C
        NDDLT = ZI(JAPPTR+ILIAI) - ZI(JAPPTR+ILIAI-1)
C
C --- NOMBRE DE DDL POUR NOEUD ESCLAVE
C
        NDDLE = ZI(JPDDL+POSNOE) - ZI(JPDDL+POSNOE-1)
C
C --- TANGENTES ET NORMALE
C        
        TAU1(1) = ZR(JTANGO+6*(ILIAI-1)+1-1)
        TAU1(2) = ZR(JTANGO+6*(ILIAI-1)+2-1)
        TAU1(3) = ZR(JTANGO+6*(ILIAI-1)+3-1)
        TAU2(1) = ZR(JTANGO+6*(ILIAI-1)+4-1)
        TAU2(2) = ZR(JTANGO+6*(ILIAI-1)+5-1)
        TAU2(3) = ZR(JTANGO+6*(ILIAI-1)+6-1) 
        CALL MMNORM(NDIM,TAU1,TAU2,NORM,R8BID)
C
C --- AFFICHAGES
C
        WRITE(IFM,4004) NDDLT,NDDLE
        WRITE(IFM,4006) ZR(JAPJEU+ILIAI-1),ZR(JJSUP+ILIAI-1)
        WRITE(IFM,4007) (NORM(K),K=1,3)
        WRITE(IFM,4008) (TAU1(K),K=1,3)
        IF (NDIM.EQ.3) THEN
          WRITE(IFM,5008) (TAU2(K),K=1,3)
        ENDIF
C ----------------------------------------------------------------------
C --- PARAMETRES PENALISATION
C ----------------------------------------------------------------------

        COEFFF = ZR(JTACF+ZTACF*(ILIAI-1)+0)
        COEFPN = ZR(JTACF+ZTACF*(ILIAI-1)+1)
        COEFPT = ZR(JTACF+ZTACF*(ILIAI-1)+2)
 
 
        WRITE(IFM,7000) COEFPN
        WRITE(IFM,7001) COEFPT
        WRITE(IFM,7003) COEFFF
        
C
C ----------------------------------------------------------------------
C --- DDL ET COEF POUR NOEUD ESCLAVE
C ----------------------------------------------------------------------
C
C
C --- COEFFICIENTS POUR CONTACT
C
        IF (NDIM.EQ.3) THEN
          WRITE (IFM,4015) NOMNOE,
     &      (ZI(JAPDDL+JDECE+IDDLE-1),IDDLE=1,NDDLE),
     &      (ZR(JAPCOE+JDECE+IDDLE-1),IDDLE=1,NDDLE)
        ELSE
          WRITE (IFM,4018) NOMNOE,
     &      (ZI(JAPDDL+JDECE+IDDLE-1),IDDLE=1,NDDLE),
     &      (ZR(JAPCOE+JDECE+IDDLE-1),IDDLE=1,NDDLE)        
        ENDIF
C
C --- COEFFICIENTS POUR FROTTEMENT 
C
        IF (LFROT) THEN
          IF (NDIM.EQ.3) THEN
            WRITE (IFM,4016) NOMNOE,
     &        (ZI(JAPDDL+JDECE+IDDLE-1),IDDLE=1,NDDLE),
     &        (ZR(JAPCOF+JDECE+IDDLE-1),IDDLE=1,NDDLE)
            WRITE (IFM,4017) NOMNOE,
     &        (ZI(JAPDDL+JDECE+IDDLE-1),IDDLE=1,NDDLE),
     &        (ZR(JAPCOF+30*NTNOE+JDECE+IDDLE-1),IDDLE=1,NDDLE)     
          ELSE
            WRITE (IFM,4019) NOMNOE,
     &        (ZI(JAPDDL+JDECE+IDDLE-1),IDDLE=1,NDDLE),
     &        (ZR(JAPCOF+JDECE+IDDLE-1),IDDLE=1,NDDLE)        
          ENDIF
        ENDIF
C
C ----------------------------------------------------------------------
C --- DDL ET COEF POUR NOEUDS MAITRES
C ----------------------------------------------------------------------
C
        JDECM  = JDECE + NDDLE
C 
C --- APPARIEMENT NODAL
C
        IF (LNODAL) THEN  
          NBNOM  = 1
          IMAIT  = 1
          NDDLM  = NDDLT - NDDLE
C
C --- COEFFICIENTS POUR CONTACT
C          
          IF (NDIM.EQ.3) THEN
            WRITE (IFM,5011) NOMNOM,
     &        (ZI(JAPDDL+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM),
     &        (ZR(JAPCOE+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM) 
          ELSE
            WRITE (IFM,6011) NOMNOM,
     &        (ZI(JAPDDL+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM),
     &        (ZR(JAPCOE+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM)
          ENDIF
C
C --- COEFFICIENTS POUR FROTTEMENT 
C
          IF (LFROT) THEN
            IF (NDIM.EQ.3) THEN
              WRITE (IFM,5012) NOMNOM,
     &         (ZI(JAPDDL+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM),
     &         (ZR(JAPCOE+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM)
              WRITE (IFM,5013) NOMNOM,
     &          (ZI(JAPDDL+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),
     &                        IDDLM=1,NDDLM),
     &          (ZR(JAPCOF+30*NTNOE+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),
     &                        IDDLM=1,NDDLM)
            ELSE
              WRITE (IFM,6012) NOMNOM,
     &         (ZI(JAPDDL+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM),
     &         (ZR(JAPCOE+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM)
            ENDIF
          ENDIF
C 
C --- APPARIEMENT MAITRE/ESCLAVE
C
        ELSE
          NBNOM  = ZI(JPONO+POSMAM) - ZI(JPONO+POSMAM-1)
          DO 50 IMAIT = 1,NBNOM    
            POSMA2 = ZI(JNOMA+ZI(JPONO+POSMAM-1)+IMAIT-1)
            NDDLM  = ZI(JPDDL+POSMA2) - ZI(JPDDL+POSMA2-1)
            CALL CFNOMM(NOMA  ,DEFICO,'NOEU',POSMA2,NOMMA2,
     &                  CODRET)
            IF (CODRET.LT.0) THEN
              GOTO 40
            ENDIF
             
C
C --- COEFFICIENTS POUR CONTACT
C             
            IF (NDIM.EQ.3) THEN
              WRITE (IFM,5001) NOMMAM,NOMMA2,
     &        (ZI(JAPDDL+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM),
     &        (ZR(JAPCOE+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM)
            ELSE
              WRITE (IFM,6001) NOMMAM,NOMMA2,
     &        (ZI(JAPDDL+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM),
     &        (ZR(JAPCOE+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM)
            ENDIF
C
C --- COEFFICIENTS POUR FROTTEMENT 
C
            IF (LFROT) THEN
              IF (NDIM.EQ.3) THEN            
                WRITE (IFM,5002) NOMMAM,NOMMA2,
     &         (ZI(JAPDDL+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM),
     &         (ZR(JAPCOE+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM)
                WRITE (IFM,5003) NOMMAM,NOMMA2,
     &            (ZI(JAPDDL+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),
     &                        IDDLM=1,NDDLM),
     &            (ZR(JAPCOF+30*NTNOE+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),
     &                        IDDLM=1,NDDLM)
              ELSE
                WRITE (IFM,6002) NOMMAM,NOMMA2,
     &         (ZI(JAPDDL+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM),
     &         (ZR(JAPCOE+JDECM+(IMAIT-1)*NDDLM+IDDLM-1),IDDLM=1,NDDLM)
              ENDIF
            ENDIF
   50     CONTINUE

        ENDIF
     
  40  CONTINUE

4000  FORMAT (' <CONTACT_DVLP> LIAISON NUMERO ',I6,' (',
     &        A8,')') 

4001  FORMAT (' <CONTACT_DVLP>  * APPARIEMENT AVEC NOEUD  ',A8)
4002  FORMAT (' <CONTACT_DVLP>  * APPARIEMENT AVEC MAILLE ',A8)
4004  FORMAT (' <CONTACT_DVLP>  * NOMBRE DE DDLS : ',I6,' DONT ',I6,
     &                        ' POUR NOEUD ESCLAVE',I6)

4006  FORMAT (' <CONTACT_DVLP>  * JEU: ',1PE15.8,' DONT :',1PE15.8,
     &        ' VENANT DE DIST_*')
4007  FORMAT (' <CONTACT_DVLP>  * NORMALE LISSEE/MOYENNEE: ',
     &         3(1PE15.8,2X))
4008  FORMAT (' <CONTACT_DVLP>  * TANGENTE DIRECTION 1   : ',
     &         3(1PE15.8,2X))
5008  FORMAT (' <CONTACT_DVLP>  * TANGENTE DIRECTION 2   : ',
     &         3(1PE15.8,2X))

4009  FORMAT (' <CONTACT_DVLP>  * ERREUR ')

C
C --- 3D - ESCLAVE
C
4015  FORMAT ((' <CONTACT_DVLP>  * DDL ESCL. CONTACT ( ',A8,'):',
     &           3(I8,2X),' / ',
     &           3(1PE15.8,2X)))

4016  FORMAT ((' <CONTACT_DVLP>  * DDL ESCL. FROT1   ( ',A8,'):',
     &           3(I8,2X),' / ',
     &           3(1PE15.8,2X)))
4017  FORMAT ((' <CONTACT_DVLP>  * DDL ESCL. FROT2   ( ',A8,'):',
     &           3(I8,2X),' / ',
     &           3(1PE15.8,2X)))
     
C
C --- 2D - ESCLAVE
C     
4018  FORMAT ((' <CONTACT_DVLP>  * DDL ESCL. CONTACT ( ',A8,'):',
     &           2(I8,2X),' / ',
     &           2(1PE15.8,2X)))

4019  FORMAT ((' <CONTACT_DVLP>  * DDL ESCL. FROT1   ( ',A8,'):',
     &           2(I8,2X),' / ',
     &           2(1PE15.8,2X)))
   
C
C --- 3D MAITRE/ESCL - MAITRE
C
5001  FORMAT ((' <CONTACT_DVLP>  * DDL MAIT. CONTACT ( ',A8,
     &           '/',A8,'):',
     &           3(I8,2X),' / ',
     &           3(1PE15.8,2X)))
5002  FORMAT ((' <CONTACT_DVLP>  * DDL MAIT. FROT1   ( ',A8,
     &           '/',A8,'):',
     &           3(I8,2X),' / ',
     &           3(1PE15.8,2X)))
5003  FORMAT ((' <CONTACT_DVLP>  * DDL MAIT. FROT2   ( ',A8,
     &           '/',A8,'):',
     &           3(I8,2X),' / ',
     &           3(1PE15.8,2X)))
C
C --- 3D NODAL - MAITRE
C    
5011  FORMAT ((' <CONTACT_DVLP>  * DDL MAIT. CONTACT ( ',A8,'):',
     &           3(I8,2X),' / ',
     &           3(1PE15.8,2X)))
5012  FORMAT ((' <CONTACT_DVLP>  * DDL MAIT. FROT1   ( ',A8,'):',
     &           3(I8,2X),' / ',
     &           3(1PE15.8,2X)))
5013  FORMAT ((' <CONTACT_DVLP>  * DDL MAIT. FROT2   ( ',A8,'):',
     &           3(I8,2X),' / ',
     &           3(1PE15.8,2X)))  
C
C --- 2D MAITRE/ESCL - MAITRE
C     
6001  FORMAT ((' <CONTACT_DVLP>  * DDL MAIT. CONTACT ( ',A8,
     &           '/',A8,'):',
     &           2(I8,2X),' / ',
     &           2(1PE15.8,2X)))
6002  FORMAT ((' <CONTACT_DVLP>  * DDL MAIT. FROT1   ( ',A8,
     &           '/',A8,'):',
     &           2(I8,2X),' / ',
     &           2(1PE15.8,2X)))
C
C --- 2D NODAL - MAITRE
C
6011  FORMAT ((' <CONTACT_DVLP>  * DDL MAIT. CONTACT ( ',A8,'):',
     &           2(I8,2X),' / ',
     &           2(1PE15.8,2X)))
6012  FORMAT ((' <CONTACT_DVLP>  * DDL MAIT. FROT1   ( ',A8,'):',
     &           2(I8,2X),' / ',
     &           2(1PE15.8,2X)))
C
C --- PENALISATION
C
7000  FORMAT (' <CONTACT_DVLP>  * E_N              :',1PE15.8)
7001  FORMAT (' <CONTACT_DVLP>  * E_T              :',1PE15.8)
7003  FORMAT (' <CONTACT_DVLP>  * COULOMB          :',1PE15.8)
C
      
C
      CALL JEDEMA()
C
      END

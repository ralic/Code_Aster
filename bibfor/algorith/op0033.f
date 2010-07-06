      SUBROUTINE OP0033()
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/07/2010   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE PROIX J-M.PROIX
      IMPLICIT NONE
C-----------------------------------------------------------------------
C           OPERATEUR    CALC_POINT_MAT
C-----------------------------------------------------------------------
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
      INTEGER ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16 ZC,CBID
      COMMON /CVARJE/ ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24,K24BID
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
      INTEGER      NDIM,IRET,N1,NBVARI,NBPAR,I,J,K,INCELA,IER
      INTEGER      JVAL,IMATE,KPG,KSP,ITER,PRED
      INTEGER      NVIMAX,NTAMAX,MATREL,IROTA,DEFIMP
      INTEGER      INDIMP(6),NUMINS,ACTION,ITGT,NBVITA
      PARAMETER    ( NVIMAX = 10000 )
      PARAMETER    ( NTAMAX = NVIMAX+16 )
      CHARACTER*4  FAMI,CARGAU
      CHARACTER*8  TYPMOD(2),MATER,K8B,TABLE,FONIMP(6)
      CHARACTER*16 OPTION,COMPOR(8),NOMPAR(NTAMAX),OPT2
      CHARACTER*19 CODI,SDDISC,K19B,SDCRIT
      REAL*8       INSTAM,INSTAP,ANG(7),R8B,CARCRI(9)
      REAL*8       DEPS(6),SIGM(6),SIGP(6),EPSP(6),EPSM(6),EPS(6)
      REAL*8       VIM(NVIMAX),VIP(NVIMAX),VR(NTAMAX),EQUI(16),R1(12)
      REAL*8       VALIMP(6),R(12),RINI(12),DY(12),DDY(12),Y(12),RAC2
      REAL*8       DSIDEP(6,6),DRDY(12,12),KEL(6,6),CIMPO(6,12),YM(12)
      REAL*8       WORK(10),SDEPS(6),SSIGP(6),SVIP(NVIMAX),SMATR(36)
      REAL*8       MATPER(36),VARIA(2*36),EPSILO,PGL(3,3),VIMP33(3,3)
      REAL*8       VIMP2(3,3),COEF,PARCRI(12),DIINST
      LOGICAL      FINPAS,DIDERN,ITEMAX,ECHLDC,ECHGAU
      DATA SDDISC            /'&&OP0033.SDDISC'/
      DATA SDCRIT            /'&&OP0033.SDCRIT'/
      DATA FINPAS            /.FALSE./  
      DATA ITEMAX            /.FALSE./  
      DATA ECHLDC            /.FALSE./  
      DATA ECHGAU            /.FALSE./  

C ======================================================================
C --- RECUPERATION DES ARGUMENTS  DE LA COMMANDE
C ======================================================================
      CALL JEMARQ()
      NDIM=3
      RAC2=SQRT(2.D0)
      FAMI=' '
      KPG=1
      KSP=1
      K19B=' '
      ITER = 0
      ACTION=1
      
C     RECUPERATION DES OPTIONS DEMANDEES
C     ----------------------------------
      CALL GETVID(' ','MATER',0,1,6,MATER,N1)
      
C     RECUPERATION DU COMPORTEMENT
C     ----------------------------
      CALL PMDORC(COMPOR,CARCRI,NBVARI,INCELA)
      IF (NBVARI.GT.NVIMAX) THEN
         CALL U2MESS('F','COMPOR2_7')
      ENDIF

C-------------------------------------------------------------------
C     LECTURE MATERIAU ET CODAGE
C-------------------------------------------------------------------
      CALL R8INIR(10, 0.D0, WORK, 1)
      CALL PMMACO ( MATER, CODI )
      CALL JEVEUT(CODI//'.CODI','L',IMATE)
C-------------------------------------------------------------------
      
C     INITIALISATIONS SD    
      CALL PMINIT(IMATE,NBVARI,NDIM,TYPMOD,TABLE,NBPAR,NBVITA,NOMPAR,
     &  ANG,PGL,IROTA,EPSM,SIGM,VIM,VIP,DEFIMP,COEF,INDIMP,FONIMP,CIMPO,
     &  KEL,SDDISC,PARCRI,PRED,MATREL,OPTION)

C --- CREATION DE LA SD POUR ARCHIVAGE DES INFORMATIONS DE CONVERGENCE
C
      CALL NMCRCV(SDCRIT)
      NUMINS=1
      
C==================================
C     BOUCLE SUR lES INSTANTS
C==================================

 200   CONTINUE
 
C        RECUPERATION DU NUMERO D'ORDRE ET DE L'INSTANT COURANTS
C        DECOUPE INITIALE DU PAS DE TEMPS
C      
         CALL NMDCIN(SDDISC,K19B,NUMINS)
         INSTAM = DIINST(SDDISC, NUMINS-1)
         INSTAP = DIINST(SDDISC, NUMINS  )
         
C        VALEURS IMPOSEES DE CONTRAINTES OU DEFORMATIONS         
         DO 15 I=1,6
            CALL FOINTE('F',FONIMP(I),1,'INST',INSTAP,VALIMP(I),IER)
C           NORMALISATION DES TERMES EN CONTRAINTES            
            IF (INDIMP(I).EQ.0) VALIMP(I)=VALIMP(I)/COEF
  15     CONTINUE      
         IF (IROTA.EQ.1) THEN
            CALL TNSVEC(6,NDIM,VIMP33,VALIMP,1.0D0)
            CALL UTBTAB('ZERO',3,3,VIMP33,PGL,WORK,VIMP2)      
            CALL TNSVEC(3,NDIM,VIMP2,VALIMP,1.0D0)
         ENDIF
C        CISAILLEMENTS*SQRT(2) POUR NMCOMP         
         CALL DSCAL(3,RAC2,VALIMP(4),1)

C        6 CMP DE EPSI DONNEES : PAS BESOIN DE NEWTON         
         IF ((DEFIMP.EQ.1).AND.(ABS(CARCRI(2)).LT.0.1D0)) THEN
            DO 16 I=1,6
              DEPS(I)=VALIMP(I)-EPSM(I)
 16         CONTINUE
            OPT2='RAPH_MECA'
            IF (INCELA.EQ.1) THEN
               CALL NMCOMP (FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,
     &         CARCRI,INSTAM,INSTAP,EPSM,DEPS,SIGM,VIM,OPT2,ANG,WORK,
     &             SIGP,VIP,DSIDEP,IRET)
            ELSEIF (INCELA.EQ.2) THEN
               CALL DCOPY(6,EPSM,1,EPS,1)
               CALL DAXPY(6,1.D0,DEPS,1,EPS,1)
               CALL NMCPEL(FAMI,KPG,1,'+',NDIM,TYPMOD,ANG,IMATE,COMPOR,
     &           CARCRI,OPTION,EPS,SIGP,VIP,DSIDEP,IRET)
            ELSE
               CALL ASSERT(.FALSE.)
            ENDIF
            IF(IRET.NE.0) GOTO 500
            GOTO 550
         ENDIF
         
C        INITIALISATION DE L'ALGO DE NEWTON 

         CALL R8INIR(12,0.D0,DY, 1)
         CALL R8INIR(6,0.D0,DEPS, 1)
         CALL DCOPY(6,SIGM,1,YM,1)
         CALL DSCAL(6,1.D0/COEF,YM,1)
         CALL DCOPY(6,EPSM,1,YM(7),1)

         IF (PRED.EQ.1) THEN
            OPT2='RIGI_MECA_TANG'         
            CALL DCOPY(NBVARI,VIM,1,SVIP,1)
            IF (INCELA.EQ.1) THEN
               CALL NMCOMP (FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,
     &       CARCRI,INSTAM,INSTAP,EPSM,DEPS,SIGM,SVIP,OPT2,ANG,WORK,
     &             SSIGP,SVIP,DSIDEP,IRET)
            ELSEIF (INCELA.EQ.2) THEN
               CALL NMCPEL(FAMI,KPG,1,'+',NDIM,TYPMOD,ANG,IMATE,COMPOR,
     &           CARCRI,OPTION,EPSM,SIGP,VIP,DSIDEP,IRET)
            ENDIF
            IF (IRET.NE.0) THEN
               PRED=0
            ELSE
               CALL PMDRDY(DSIDEP,COEF,CIMPO,VALIMP,YM,SIGM,R,DRDY)
            ENDIF
         ENDIF
         IF (PRED.EQ.0) THEN
            CALL PMDRDY(KEL,COEF,CIMPO,VALIMP,YM,SIGM,R,DRDY)
         ENDIF
C        SAUVEGARDE DE R(DY0) POUR TEST DE CONVERGENCE
         CALL DCOPY(12,R,1,RINI,1)
         CALL PMIMPR(0,INSTAP,FONIMP,VALIMP,0,EPSM,SIGM,VIM,NBVARI,R,
     &   R8B,R8B)
         
         ITER = 0
         
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  
C           ITERATIONS DE NEWTON
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    
 300     CONTINUE
 
            ITER = ITER + 1
            CALL DCOPY(12,R,1,DDY,1)
            
C           RESOLUTION DE DRDY*DDY = - R(Y)  CARGAU = 'NCSP'
            CARGAU = 'NCWP'  
            CALL MGAUSS ( CARGAU,DRDY,DDY,12,12,1,R8B,IRET )
            IF(IRET.NE.0) THEN
               ECHGAU=.TRUE.
               GOTO 500
            ENDIF
            
C           REACTUALISATION DE DY = DY + DDY
            CALL DAXPY(12,1.D0,DDY,1,DY,1)
            CALL DCOPY(6,DY(7),1,DEPS,1)

C           POUR LE CALCUL DE LA MATRICE TANGENTE PAR PERTURBATION
 1000       CONTINUE
 
C           CALCUL DU RESIDU
            ECHLDC=.FALSE.
            IF (INCELA.EQ.1) THEN
              CALL NMCOMP (FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,CARCRI,
     &             INSTAM,INSTAP,EPSM,DEPS,SIGM,VIM,OPTION,ANG,WORK,
     &             SIGP,VIP,DSIDEP,IRET)
            ELSEIF (INCELA.EQ.2) THEN
               CALL DCOPY(6,EPSM,1,EPS,1)
               CALL DAXPY(6,1.D0,DEPS,1,EPS,1)
               CALL NMCPEL(FAMI,KPG,1,'+',NDIM,TYPMOD,ANG,IMATE,COMPOR,
     &           CARCRI,OPTION,EPS,SIGP,VIP,DSIDEP,IRET)
            ENDIF
            
            CALL PMIMPR(1,INSTAP,FONIMP,VALIMP,ITER,DEPS,SIGP,VIP,
     &                  NBVARI,R,R8B,R8B)
            IF(IRET.NE.0) THEN
               ECHLDC=.TRUE.
               GOTO 500
            ENDIF
            
C           CALCUL EVENTUEL DE LA MATRICE TGTE PAR PERTURBATION
            CALL PMVTGT(OPTION,CARCRI,DEPS,SIGP,VIP,NBVARI,EPSILO,
     &               VARIA,MATPER,DSIDEP,SMATR,SDEPS,SSIGP,SVIP,ITGT)
            IF (ITGT.NE.0) THEN
               GOTO 1000
            ENDIF
            
            CALL DCOPY(12,YM,1,Y,1)
            CALL DAXPY(12,1.D0,DY,1,Y,1)
            IF (MATREL.EQ.1) THEN
               CALL PMDRDY(KEL,COEF,CIMPO,VALIMP,Y,SIGP,R,DRDY)
            ELSE
               CALL PMDRDY(DSIDEP,COEF,CIMPO,VALIMP,Y,SIGP,R,DRDY)
            ENDIF    
            
C           VERIFICATION DE LA CONVERGENCE EN DY  ET RE-INTEGRATION ?
            CALL PMCONV(R,RINI,R1,INSTAP,SIGP,COEF,ITER,
     &                  PARCRI,IRET,ITEMAX)

C           ENREGISTRE LES ERREURS A CETTE ITERATION
            CALL DIERRE(SDDISC,SDCRIT,ITER)      

  540       CONTINUE
  
C           ON A CONVERGE ON FINIT LE PAS DE TEMPS
            IF (IRET.EQ.0)  GOTO 550

C           CA NE SE PASSE PAS BIEN... -> ON VA TENTER DE REDECOUPER
            IF ( IRET.EQ.2) GOTO 500
            
C           ITERATION SUIVANTE
            CALL ASSERT(IRET.EQ.1)
            GOTO 300

C ======================================================================
C     FIN DES ITERATIONS DE NEWTON
C ======================================================================

 500     CONTINUE
C
C        GESTION DE LA DECOUPE DU PAS DE TEMPS
C        EN L'ABSENCE DE CONVERGENCE ON CHERCHE A SUBDIVISER LE PAS 
C        DE TEMPS SI L'UTILISATEUR A FAIT LA DEMANDE
         CALL PMDECO(PARCRI,SDDISC,ITER,NUMINS,ITEMAX,ECHLDC,ECHGAU,
     &               ACTION) 
C
C ---    ACTION
C          0 ARRET DU CALCUL
C          1 NOUVEAU PAS DE TEMPS
C          2 ON FAIT DES ITERATIONS DE NEWTON EN PLUS
C          3 ON FINIT LE PAS DE TEMPS
         IF (ACTION.EQ.1) THEN
           GOTO 600
         ELSEIF (ACTION.EQ.2) THEN
           GOTO 540
         ELSEIF (ACTION.EQ.3) THEN
           GOTO 550
         ELSEIF (ACTION.EQ.0) THEN
           GOTO 550
         ENDIF
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
 550     CONTINUE

C        ---------------------------------------------------------------
C        CONVERGENCE => MISE A JOUR DE SIGM,VIM, TABLE
C        ---------------------------------------------------------------
         CALL PMIMPR(2,INSTAP,FONIMP,VALIMP,ITER,DEPS,SIGP,VIP,NBVARI,R,
     &    R8B,R8B)
         CALL DCOPY(6,SIGP,1,SIGM,1)
         CALL DCOPY(6,SIGP,1,VR(8),1)
         CALL DSCAL(3,1.D0/RAC2,VR(11),1)
         CALL DCOPY(6,EPSM,1,EPSP,1)
         CALL DAXPY(6,1.D0,DEPS,1,EPSP,1)
         CALL DCOPY(6,EPSP,1,EPSM,1)
         CALL DCOPY(6,EPSP,1,VR(2),1)
         CALL DSCAL(3,1.D0/RAC2,VR(5),1)
         CALL FGEQUI ( VR(8), 'SIGM_DIR', 3, EQUI)
         VR(14)=EQUI(16)
         VR(15)=EQUI(1)
         CALL DCOPY(NBVARI,VIP,1,VIM,1)
         CALL DCOPY(NBVITA,VIP,1,VR(16),1)
         INSTAM=INSTAP
         VR(1)=INSTAP
         VR(NBPAR)=ITER
         CALL TBAJLI(TABLE,NBPAR,NOMPAR,I,VR,CBID,K8B,0)

C        ADAPTATION DU NOUVEAU PAS DE TEMPS
C        PAS DE GESTION DE DELTA_GRANDEUR ACTUELLEMENT
         FINPAS = DIDERN(SDDISC, NUMINS)
         CALL NMADAT(SDDISC,NUMINS,ITER,K19B,FINPAS) 
         NUMINS=NUMINS+1     
         
 600  CONTINUE
C 
C --- DERNIER INSTANT DE CALCUL ? -> ON SORT DE STAT_NON_LINE
C
      IF (FINPAS.OR.(ACTION.EQ.0)) THEN
        GOTO 900
      ENDIF
      GOTO 200
C==================================
C     FIN BOUCLE SUR LES INSTANTS
C==================================

 900  CONTINUE  
      CALL JEDEMA()

      END

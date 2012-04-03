      SUBROUTINE OP0033()
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/04/2012   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE PROIX J-M.PROIX
C
      IMPLICIT NONE
C
C ----------------------------------------------------------------------
C  OPERATEUR    CALC_POINT_MAT
C ----------------------------------------------------------------------
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
      INTEGER ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER      NDIM,IRET,N1,NBVARI,NBPAR,I,INCELA,IER
      INTEGER      IMATE,KPG,KSP,ITER,PRED,NCMP,IMPTGT
      INTEGER      NTAMAX,MATREL,IROTA,DEFIMP,LICCVG(5)
      INTEGER      INDIMP(9),NUMINS,ACTITE,ACTION,ITGT,IFORTA
C     NOMBRE MAXI DE COLONNES DANS UNE TABLE 9999 (CF D4.02.05)
      PARAMETER    ( NTAMAX = 9999 )
      INTEGER      NCMPMA,DIMAKI,DIMANV,IGRAD,NBVITA
C    DIMAKI = DIMENSION MAX DE LA LISTE DES RELATIONS KIT
      PARAMETER    (DIMAKI=9)
C    DIMANV = DIMENSION MAX DE LA LISTE DU NOMBRE DE VAR INT EN THM
      PARAMETER    (DIMANV=4)
      PARAMETER    (NCMPMA=7+DIMAKI+DIMANV)
      CHARACTER*4  FAMI,CARGAU
      CHARACTER*8  TYPMOD(2),MATER,TABLE,FONIMP(9),TYPPAR(NTAMAX)
      CHARACTER*16 OPTION,COMPOR(NCMPMA),NOMPAR(NTAMAX),OPT2
      CHARACTER*19 CODI,SDDISC,K19B,SDCRIT
      REAL*8       INSTAM,INSTAP,ANG(7),R8B,CARCRI(10),FEM(9)
      REAL*8       DEPS(9),SIGM(6),SIGP(6),EPSM(9),EPS(9),VR(NTAMAX)
      REAL*8       VALIMP(9),R(12),RINI(12),DY(12),DDY(12),Y(12),RAC2
      REAL*8       DSIDEP(6,9),DRDY(12,12),KEL(6,6),CIMPO(6,12),YM(12)
      REAL*8       WORK(10),SDEPS(6),SSIGP(6),SMATR(36),R1(12)
      REAL*8       MATPER(36),VARIA(2*36),EPSILO,PGL(3,3),VIMP33(3,3)
      REAL*8       VIMP2(3,3),COEF,PARCRI(12),DIINST,JM,JP,JD,RBID
      LOGICAL      FINPAS,DIDERN,ITEMAX,CONVER
      INTEGER      IARG
      CHARACTER*19 NOMVI
      CHARACTER*19 VIM,VIP,VIM2,SVIP
      INTEGER      LVIM,LVIP,LVIM2,LSVIP,LNOMVI
      
      DATA SDDISC  /'&&OP0033.SDDISC'/
      DATA SDCRIT  /'&&OP0033.SDCRIT'/
      
      DATA VIM     /'&&OP0033.VIM'/
      DATA VIP     /'&&OP0033.VIP'/
      DATA SVIP    /'&&OP0033.SVIP'/
      DATA VIM2    /'&&OP0033.VIM2'/
      DATA NOMVI   /'&&OP0033.NOMVI'/

C ======================================================================
C --- RECUPERATION DES ARGUMENTS  DE LA COMMANDE
C ======================================================================
      CALL INFMAJ
      CALL JEMARQ()
      NDIM=3
      RAC2=SQRT(2.D0)
      FAMI=' '
      KPG=1
      KSP=1
      K19B=' '
      ITER = 0
      ACTION=1
      FINPAS=.FALSE.
      ITEMAX=.FALSE.
      DO 10 I=1,5
        LICCVG(I)=0
 10   CONTINUE
      
C     RECUPERATION DES OPTIONS DEMANDEES
C     ----------------------------------
      CALL GETVID(' ','MATER',0,IARG,6,MATER,N1)
      
C     RECUPERATION DU COMPORTEMENT
C     ----------------------------
      CALL PMDORC(COMPOR,CARCRI,NBVARI,INCELA)

      CALL WKVECT(VIM,'V V R',NBVARI,LVIM)
      CALL WKVECT(VIP,'V V R',NBVARI,LVIP)
      CALL WKVECT(SVIP,'V V R',NBVARI,LSVIP)
      CALL WKVECT(VIM2,'V V R',NBVARI,LVIM2)
      CALL WKVECT(NOMVI,'V V K8',NBVARI,LNOMVI)


C-------------------------------------------------------------------
C     LECTURE MATERIAU ET CODAGE
C-------------------------------------------------------------------
      CALL R8INIR(10, 0.D0, WORK, 1)
      CALL PMMACO ( MATER, CODI )
      CALL JEVEUT(CODI//'.CODI','L',IMATE)
C-------------------------------------------------------------------
      
C     INITIALISATIONS SD    
      CALL PMINIT(IMATE ,NBVARI,NDIM  ,TYPMOD,TABLE ,
     &            NBPAR ,IFORTA,NOMPAR,TYPPAR,ANG   ,PGL   ,
     &            IROTA ,EPSM  ,SIGM  ,ZR(LVIM) ,ZR(LVIP),VR,
     &            DEFIMP,COEF  ,INDIMP,FONIMP,CIMPO ,
     &            KEL   ,SDDISC,PARCRI,PRED  ,MATREL,IMPTGT,
     &            OPTION, ZK8(LNOMVI), NBVITA)
      CALL R8INIR(54,0.D0,DSIDEP, 1)

C --- CREATION DE LA SD POUR ARCHIVAGE DES INFORMATIONS DE CONVERGENCE
C
      CALL NMCRCV(SDCRIT)
      NUMINS=1
      
C==================================
C     BOUCLE SUR lES INSTANTS
C==================================

 200   CONTINUE
         DO 11 I=1,5
           LICCVG(I)=0
 11      CONTINUE 
C        RECUPERATION DU NUMERO D'ORDRE ET DE L'INSTANT COURANTS
C        DECOUPE INITIALE DU PAS DE TEMPS
C      
         CALL NMDCIN(SDDISC,NUMINS)
         INSTAM = DIINST(SDDISC, NUMINS-1)
         INSTAP = DIINST(SDDISC, NUMINS  )
         
         IF (DEFIMP.LT.2) THEN
             IGRAD=0
C            VALEURS IMPOSEES DE CONTRAINTES OU DEFORMATIONS
             DO 20 I=1,6
                CALL FOINTE('F',FONIMP(I),1,'INST',INSTAP,VALIMP(I),IER)
C               NORMALISATION DES TERMES EN CONTRAINTES
                IF (INDIMP(I).EQ.0) VALIMP(I)=VALIMP(I)/COEF
  20         CONTINUE
C            NORMALEMENT DEJA VERIFIE PAR SIMU_POINT_MAT_OPS  
             CALL ASSERT(COMPOR(3).EQ.'PETIT')
         ELSEIF (DEFIMP.EQ.2) THEN
            IGRAD=1
C           VALEURS IMPOSEES DE GRADIENTS F         
             DO 21 I=1,9
                CALL FOINTE('F',FONIMP(I),1,'INST',INSTAP,VALIMP(I),IER)
  21         CONTINUE
         ENDIF
         
         IF (IROTA.EQ.1) THEN
            CALL TNSVEC(6,NDIM,VIMP33,VALIMP,1.0D0)
            CALL UTBTAB('ZERO',3,3,VIMP33,PGL,WORK,VIMP2)      
            CALL TNSVEC(3,NDIM,VIMP2,VALIMP,1.0D0)
         ENDIF
C        CISAILLEMENTS*SQRT(2) POUR NMCOMP         
         IF (DEFIMP.LT.2) THEN
            CALL DSCAL(3,RAC2,VALIMP(4),1)
         ENDIF

C        6 CMP DE EPSI OU 9 CMP DE GRAD DONNEES : PAS BESOIN DE NEWTON 
         IF ((DEFIMP.GE.1).AND.(ABS(CARCRI(2)).LT.0.1D0)) THEN
            OPT2='RAPH_MECA'
            IF (IMPTGT.EQ.1) OPT2='FULL_MECA'
            IF (DEFIMP.EQ.1) THEN
               NCMP=6
               DO 30 I=1,NCMP
                 DEPS(I)=VALIMP(I)-EPSM(I)
 30            CONTINUE
            ELSEIF (DEFIMP.EQ.2) THEN
               NCMP=9
               CALL MATINV('S',3,EPSM,FEM,JM)
               CALL PMAT(3,VALIMP,FEM,DEPS)         
               CALL LCDETF(3,DEPS,JD)
               JP = JM*JD
            ENDIF
            IF (INCELA.EQ.1) THEN
               CALL DCOPY(NBVARI,ZR(LVIM),1,ZR(LVIM2),1)
               CALL NMCOMP (FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,
     &        CARCRI,INSTAM,INSTAP,NCMP,EPSM,DEPS,6,SIGM,ZR(LVIM2),OPT2,
     &         ANG,10,WORK,SIGP,ZR(LVIP),6*NCMP,DSIDEP,1,RBID,IRET)
               IF(COMPOR(3).EQ.'SIMO_MIEHE') THEN
                  CALL DSCAL(2*NDIM,1.D0/JP,SIGP,1)
               ENDIF
            ELSEIF (INCELA.EQ.2) THEN
               CALL DCOPY(NCMP,EPSM,1,EPS,1)
               CALL DAXPY(NCMP,1.D0,DEPS,1,EPS,1)
               CALL NMCPEL(FAMI,KPG,1,'+',NDIM,TYPMOD,ANG,IMATE,
     &           COMPOR,CARCRI,OPTION,EPS,SIGP,ZR(LVIP),DSIDEP,IRET)
            ELSE
               CALL ASSERT(.FALSE.)
            ENDIF
            CALL PMIMPR(0,INSTAP,INDIMP,FONIMP,VALIMP,0,EPSM,SIGM,
     &            ZR(LVIM),NBVARI,R,R8B,R8B)
            IF(IRET.NE.0) THEN
               LICCVG(2) = 1
               GOTO 500
            ENDIF
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
            CALL DCOPY(NBVARI,ZR(LVIM),1,ZR(LSVIP),1)
            IF (INCELA.EQ.1) THEN
               CALL NMCOMP (FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,
     &          CARCRI,INSTAM,INSTAP,6,EPSM,DEPS,6,SIGM,ZR(LSVIP),OPT2,
     &              ANG,10,WORK,SSIGP,ZR(LSVIP),36,DSIDEP,1,RBID,IRET)
            ELSEIF (INCELA.EQ.2) THEN
               CALL NMCPEL(FAMI,KPG,1,'+',NDIM,TYPMOD,ANG,IMATE,COMPOR,
     &           CARCRI,OPTION,EPSM,SIGP,ZR(LVIP),DSIDEP,IRET)
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
         CALL PMIMPR(0,INSTAP,INDIMP,FONIMP,VALIMP,0,EPSM,SIGM,
     &               ZR(LVIM),NBVARI,R,R8B,R8B)
         
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
               LICCVG(5) = 1
               CONVER = .FALSE.
               GOTO 500
            ENDIF
            
C           REACTUALISATION DE DY = DY + DDY
            CALL DAXPY(12,1.D0,DDY,1,DY,1)
            CALL DCOPY(6,DY(7),1,DEPS,1)

C           POUR LE CALCUL DE LA MATRICE TANGENTE PAR PERTURBATION
 1000       CONTINUE
 
C           CALCUL DU RESIDU
            LICCVG(2) = 0
            IF (INCELA.EQ.1) THEN
              CALL DCOPY(NBVARI,ZR(LVIM),1,ZR(LVIM2),1)
              CALL NMCOMP (FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,CARCRI,
     &             INSTAM,INSTAP,6,EPSM,DEPS,6,SIGM,ZR(LVIM2),OPTION,
     &             ANG,10,WORK,SIGP,ZR(LVIP),36,DSIDEP,1,RBID,IRET)
            ELSEIF (INCELA.EQ.2) THEN
               CALL DCOPY(6,EPSM,1,EPS,1)
               CALL DAXPY(6,1.D0,DEPS,1,EPS,1)
               CALL NMCPEL(FAMI,KPG,1,'+',NDIM,TYPMOD,ANG,IMATE,COMPOR,
     &           CARCRI,OPTION,EPS,SIGP,ZR(LVIP),DSIDEP,IRET)
            ENDIF
            
            CALL PMIMPR(1,INSTAP,INDIMP,FONIMP,VALIMP,ITER,DEPS,SIGP,
     &                  ZR(LVIP),NBVARI,R,R8B,R8B)
            IF(IRET.NE.0) THEN
               CONVER = .FALSE.
               LICCVG(2) = 1
               GOTO 500
            ENDIF
            
C           CALCUL EVENTUEL DE LA MATRICE TGTE PAR PERTURBATION
            CALL PMVTGT(OPTION,CARCRI,DEPS,SIGP,ZR(LVIP),NBVARI,EPSILO,
     &             VARIA,MATPER,DSIDEP,SMATR,SDEPS,SSIGP,ZR(LSVIP),ITGT)
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
            CALL PMCONV(R     ,RINI  ,R1    ,INSTAP,SIGP  ,
     &                  COEF  ,ITER  ,INDIMP,PARCRI,CONVER,
     &                  ITEMAX)

C           ENREGISTRE LES RESIDUS A CETTE ITERATION
            CALL DIERRE(SDDISC,SDCRIT,ITER)
            
C           VERIFICATION DES EVENT-DRIVEN
  500       CONTINUE
            CALL PMSTA1(SIGM,SIGP,DEPS,ZR(LVIM),ZR(LVIP),NBVARI,NBVITA,
     &                  IFORTA,NBPAR,NOMPAR,VR,IGRAD,TYPPAR,ZK8(LNOMVI),
     &                  SDDISC, LICCVG,ITEMAX,CONVER,ACTITE)
C
C           ON CONTINUE NEWTON
            IF (ACTITE.EQ.2) GOTO 300

C ======================================================================
C     FIN DES ITERATIONS DE NEWTON
C ======================================================================

C        GESTION DE LA DECOUPE DU PAS DE TEMPS
C        EN L'ABSENCE DE CONVERGENCE ON CHERCHE A SUBDIVISER LE PAS 
C        DE TEMPS SI L'UTILISATEUR A FAIT LA DEMANDE        
         CALL PMACTN(SDDISC,PARCRI,ITER  ,NUMINS,ITEMAX,
     &               LICCVG,ACTITE,ACTION)
C
C ---    ACTION
C          0 ARRET DU CALCUL
C          1 NOUVEAU PAS DE TEMPS
C          2 ON FAIT DES ITERATIONS DE NEWTON EN PLUS
C          3 ON FINIT LE PAS DE TEMPS
         IF (ACTION.EQ.1) THEN
           GOTO 600
         ELSEIF (ACTION.EQ.2) THEN
           GOTO 300
         ELSEIF (ACTION.EQ.3) THEN
           GOTO 550
         ELSEIF (ACTION.EQ.0) THEN
           GOTO 550
         ENDIF
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
 550     CONTINUE

C        ---------------------------------------------------------------
C        CONVERGENCE => MISE A JOUR DE SIGM,ZR(LVIM), TABLE
C        ---------------------------------------------------------------

C        ADAPTATION DU NOUVEAU PAS DE TEMPS
C        PAS DE GESTION DE DELTA_GRANDEUR ACTUELLEMENT
         FINPAS = DIDERN(SDDISC, NUMINS)
         CALL NMADAT(SDDISC,NUMINS,ITER,K19B) 
         NUMINS=NUMINS+1
C        STOCKAGE EFFECTIF DU RESULTAT DANS LA TABLE
        CALL PMSTAB(SIGM,SIGP,EPSM,DEPS,NBVARI,ZR(LVIM),ZR(LVIP),IFORTA,
     &    INSTAM,INSTAP,ITER,NBPAR,NOMPAR,TABLE,VR,IGRAD,VALIMP,IMPTGT,
     &    DSIDEP,ZK8(LNOMVI),NBVITA)
         CALL PMIMPR(2,INSTAP,INDIMP,FONIMP,VALIMP,ITER,DEPS,SIGP,
     &               ZR(LVIP),NBVARI,R,R8B,R8B)

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
      CALL JEDETC(' ','&&CPM',1)
      CALL JEDEMA()
      END

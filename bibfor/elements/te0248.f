      SUBROUTINE TE0248(OPTION,NOMTE)
      IMPLICIT NONE
      CHARACTER*(*)     OPTION,NOMTE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 10/04/2002   AUTEUR JMBHH01 J.M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C
C     CALCUL DES OPTIONS FULL_MECA, RAPH_MECA ET RIGI_MECA_TANG
C     POUR COMPORTEMENTS NON LINEAIRES
C     DES ELEMENTS DE BARRE 'MECA_BARRE'
C
C ----------------------------------------------------------------------
C IN  : OPTION : NOM DE L'OPTION A CALCULER (K16)
C IN  : NOMTE  : NOM DU TYPE_ELEMENT (K16)
C ----------------------------------------------------------------------
C
C **************** DEBUT COMMUNS NORMALISES JEVEUX *********************
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
      CHARACTER*80                                             ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) ,ZK80(1)
C
C ***************** FIN COMMUNS NORMALISES JEVEUX **********************
C
C
C *************** DECLARATION DES VARIABLES LOCALES ********************
C
      INTEGER NEQ,NBT,NVAMAX,IMATE,IGEOM,IORIE,ITREF,ISECT,IINSTM
      INTEGER IINSTP,IDEPLM,IDEPLP,ICONTM,IVARIM,ITEMPM,ITEMPP,ICOMPO
      INTEGER ICARCR,IMATUU,IVECTU,ICONTP,NNO,NC,IVARIP,JCRET,NBVARI
      PARAMETER    (NEQ = 6, NBT = 21 ,NVAMAX=2000)
      CHARACTER*16 OPTIOZ,COMPEL
C
C   CONSTANTES POUR INTO MENEGOTTO
C
      INTEGER    NCSTPM
      PARAMETER (NCSTPM=13)
      REAL*8     CSTPM(NCSTPM)
C
      REAL*8       E,ALPHA
      REAL*8       A,XLONG0,XLONGM,TREF,SIGY,DSDE
      REAL*8       PGL(3,3)
      REAL*8       DUL(NEQ),UML(NEQ),DLONG
      REAL*8       KLV(NBT),VIP(NVAMAX),VIM(NVAMAX)
      REAL*8       TEMPM,TEMPP,EFFNOM,EFFNOP,FONO(NEQ)
      REAL*8       W(6),ANG1(3),XD(3),MATUU(21),VECTU(6)
      REAL*8       DEPLM(6),DEPLP(6),DSIDEP(6,6)
      REAL*8       SIGP(6),SIGM(6),EPS(6),DEPS(6),R8MIEM
      INTEGER      I,J,IK
C
      LOGICAL      VECTEU
C
C *********** FIN DES DECLARATIONS DES VARIABLES LOCALES ***************
C
C ********************* DEBUT DE LA SUBROUTINE *************************
C
C --- PARAMETRES EN ENTREE
C
      CALL JEVECH ('PMATERC','L',IMATE)
      CALL JEVECH ('PGEOMER','L',IGEOM)
      CALL JEVECH ('PCAORIE','L',IORIE)
      CALL JEVECH ('PTEREF' ,'L',ITREF)
      CALL JEVECH ('PCAGNBA','L',ISECT)
      CALL JEVECH ('PINSTMR','L',IINSTM)
      CALL JEVECH ('PINSTPR','L',IINSTP)
      CALL JEVECH ('PDEPLMR','L',IDEPLM)
      CALL JEVECH ('PDEPLPR','L',IDEPLP)
      CALL JEVECH ('PCONTMR','L',ICONTM)
      CALL JEVECH ('PVARIMR','L',IVARIM)
      CALL JEVECH ('PTEMPMR','L',ITEMPM)
      CALL JEVECH ('PTEMPPR','L',ITEMPP)
      CALL JEVECH ('PCOMPOR','L',ICOMPO)
      CALL JEVECH ('PCARCRI','L',ICARCR)
C
C --- PARAMETRES EN SORTIE
C
C      
      IF( OPTION .EQ. 'RIGI_MECA_TANG') THEN
         CALL JEVECH('PMATUUR','E',IMATUU)
         IVARIP=IVARIM
         ICONTP=ICONTM
      ELSEIF( OPTION .EQ. 'FULL_MECA' ) THEN
         CALL JEVECH('PMATUUR','E',IMATUU)
         CALL JEVECH('PVECTUR','E',IVECTU)
         CALL JEVECH('PCONTPR','E',ICONTP)
         CALL JEVECH('PVARIPR','E',IVARIP)
      ELSEIF( OPTION .EQ. 'RAPH_MECA' ) THEN
         CALL JEVECH('PVECTUR','E',IVECTU)
         CALL JEVECH('PCONTPR','E',ICONTP)
         CALL JEVECH('PVARIPR','E',IVARIP)
      ENDIF
C
      IF ( ZK16(ICOMPO+3) .EQ. 'COMP_ELAS' ) THEN
         CALL UTMESS('F','BARRE','COMP_ELAS NON VALIDE')
      ENDIF
C
C --- RECUPERATION DE LA SECTION DE LA BARRE
C
      A = ZR(ISECT)
      NNO = 2
      NC  = 3
C
C --- RECUPERATION DES ORIENTATIONS ALPHA,BETA,GAMMA
C --- ET CALCUL DES MATRICES DE CHANGEMENT DE REPERE
C
      IF ( ZK16(ICOMPO+2)(6:10) .EQ. '_REAC' ) THEN
       IF (NOMTE.EQ.'MECA_BARRE') THEN
        DO 10 I = 1,3
          W(I)   = ZR(IGEOM-1+I) + ZR(IDEPLM-1+I) + ZR(IDEPLP-1+I)
          W(I+3) = ZR(IGEOM+2+I) + ZR(IDEPLM+2+I) + ZR(IDEPLP+2+I)
          XD(I)  = W(I+3) - W(I)
10      CONTINUE
       ELSE IF (NOMTE.EQ.'MECA_2D_BARRE') THEN
          W(1)   = ZR(IGEOM-1+1) + ZR(IDEPLM-1+1) + ZR(IDEPLP-1+1)
          W(2)   = ZR(IGEOM-1+2) + ZR(IDEPLM-1+2) + ZR(IDEPLP-1+2)
          W(3)   = 0.D0
          W(4)   = ZR(IGEOM-1+3) + ZR(IDEPLM-1+3) + ZR(IDEPLP-1+3)
          W(5)   = ZR(IGEOM-1+4) + ZR(IDEPLM-1+4) + ZR(IDEPLP-1+4)
          W(6)   = 0.D0
          XD(1)  = W(4) - W(1)
          XD(2)  = W(5) - W(2)
          XD(3)  = 0.D0
       ENDIF
       CALL ANGVX(XD,ANG1(1),ANG1(2))
       ANG1(3) = ZR(IORIE+2)
       CALL MATROT ( ANG1 , PGL )
      ELSE
       IF (NOMTE.EQ.'MECA_BARRE') THEN
        DO 20 I = 1,3
          W(I)   = ZR(IGEOM-1+I)
          W(I+3) = ZR(IGEOM+2+I)
          XD(I)  = W(I+3) - W(I)
20      CONTINUE
       ELSE IF (NOMTE.EQ.'MECA_2D_BARRE') THEN
          W(1)   = ZR(IGEOM-1+1)
          W(2)   = ZR(IGEOM-1+2)
          W(3)   = 0.D0
          W(4)   = ZR(IGEOM-1+3)
          W(5)   = ZR(IGEOM-1+4)
          W(6)   = 0.D0
          XD(1)  = W(4) - W(1)
          XD(2)  = W(5) - W(2)
          XD(3)  = 0.D0
        ENDIF
        CALL MATROT ( ZR(IORIE) , PGL )
      ENDIF

      CALL PSCAL(3,XD,XD,XLONG0)
      XLONG0 = SQRT(XLONG0)
C
      IF(XLONG0.EQ.0.D0) THEN
         CALL UTMESS ('F','TE0248',
     &                'NOEUDS CONFONDUS POUR UN ELEMENT DE BARRE')
      ENDIF
      
C
C --- INCREMENT DE DEPLACEMENT EN REPERE LOCAL
C CORRECTION CHAVANT : DUL = INCREMENT ENTRE INSTANT
C PLUS ET INSTANT MOINS
C ---    DUL  ENTRE LE REPOS ET LE DERNIER ETAT CONVERGE
C --- INCREMENT D'ALLONGEMENT DLONG
C
      IF (NOMTE.EQ.'MECA_BARRE') THEN
        DO 21 I=1,6
          DEPLM(I) = ZR(IDEPLM+I-1)
          DEPLP(I) = ZR(IDEPLP+I-1)
 21     CONTINUE
       ELSE IF (NOMTE.EQ.'MECA_2D_BARRE') THEN
          DEPLM(1) = ZR(IDEPLM)
          DEPLM(2) = ZR(IDEPLM+1)
          DEPLM(3) = 0.D0
          DEPLM(4) = ZR(IDEPLM+2)
          DEPLM(5) = ZR(IDEPLM+3)
          DEPLM(6) = 0.D0
 
          DEPLP(1) = ZR(IDEPLP)
          DEPLP(2) = ZR(IDEPLP+1)
          DEPLP(3) = 0.D0
          DEPLP(4) = ZR(IDEPLP+2)
          DEPLP(5) = ZR(IDEPLP+3)
          DEPLP(6) = 0.D0

       ENDIF

      CALL UTPVGL ( NNO, NC, PGL, DEPLM, UML )
      CALL UTPVGL ( NNO, NC, PGL, DEPLP, DUL )

        DLONG = DUL(4) - DUL(1)

        XLONGM = XLONG0+UML(4)-UML(1)


C
C --- RECUPERATION
C ---     DE L'EFFORT NORMAL PRECEDENT MOYEN EFFNOM POUR L'ELEMENT
C ---     DE LA TEMPERATURE PRECEDENTE MOYENNE TEMPM POUR L'ELEMENT
C ---     DE LA TEMPERATURE COURANTE   MOYENNE TEMPP POUR L'ELEMENT
C
      TREF   = ZR(ITREF)
      EFFNOM = ZR(ICONTM)

      TEMPM  = ZR(ITEMPM)

      TEMPP  = ZR(ITEMPP)

C
C --- RELATION DE COMPORTEMENT
C
C     ---------------------------------------------------
      IF ( ZK16(ICOMPO) .EQ. 'VMIS_ISOT_LINE' .OR.
     &     ZK16(ICOMPO) .EQ. 'VMIS_CINE_LINE' )  THEN
C     ---------------------------------------------------
C
C --- RECUPERATION DES CARACTERISTIQUES DU MATERIAU
C
        CALL NMMABA ( ZI(IMATE), ZK16(ICOMPO), E, ALPHA, DSDE, SIGY,
     +              NCSTPM, CSTPM  )
        VIM(1)    = ZR(IVARIM)
        VIM(2)    = ZR(IVARIM+1)
        CALL NMICLB  (OPTION,ZK16(ICOMPO),E,ALPHA,XLONG0,A,ZR(IINSTM),
     &                ZR(IINSTP),XLONGM,DLONG,DSDE,SIGY,EFFNOM,TEMPM,
     &                TEMPP,TREF,VIM,EFFNOP,VIP,KLV,FONO)

        IF ( OPTION .EQ. 'RIGI_MECA_TANG' ) THEN
            CALL UTPSLG ( NNO, NC, PGL, KLV, MATUU)
        ELSE
           ZR(ICONTP)   = EFFNOP
           IF ( OPTION .EQ. 'FULL_MECA'      ) THEN
              CALL UTPSLG ( NNO, NC, PGL, KLV, MATUU )
           ENDIF
           ZR(IVARIP)   = VIP(1)
           ZR(IVARIP+1) = VIP(2)
           CALL UTPVLG ( NNO, NC, PGL, FONO, VECTU )
        ENDIF
C
C     ---------------------------------------------------
      ELSEIF ( ZK16(ICOMPO) .EQ. 'VMIS_ASYM_LINE' )  THEN
C     ---------------------------------------------------
C
C        RECUPERATION DES CARACTERISTIQUES DU MATERIAU
C
         CALL NMMABA ( ZI(IMATE), ZK16(ICOMPO), E, ALPHA, DSDE, SIGY,
     &              NCSTPM, CSTPM  )
C     
         CALL NMASYM (ZI(IMATE),OPTION,XLONG0,A,ZR(IINSTM),ZR(IINSTP),
     &                DLONG,EFFNOM,TEMPM,TEMPP,TREF,
     &                ZR(IVARIM),ZR(ICONTP),ZR(IVARIP),KLV,FONO)
C
         IF ( OPTION .EQ. 'RIGI_MECA_TANG' ) THEN
            CALL UTPSLG ( NNO, NC, PGL, KLV, MATUU )
         ELSE
            IF ( OPTION .EQ. 'FULL_MECA'      ) THEN
               CALL UTPSLG ( NNO, NC, PGL, KLV, MATUU )
            ENDIF
            CALL UTPVLG ( NNO, NC, PGL, FONO, VECTU)
         ENDIF
C
C     ---------------------------------------------------
      ELSEIF ( ZK16(ICOMPO) .EQ. 'PINTO_MENEGOTTO' ) THEN
C     ---------------------------------------------------
C
C        RECUPERATION DES CARACTERISTIQUES DU MATERIAU
C
         CALL NMMABA ( ZI(IMATE), ZK16(ICOMPO), E, ALPHA, DSDE, SIGY,
     &              NCSTPM, CSTPM  )
C     
         VIM(1)    = ZR(IVARIM)
         VIM(2)    = ZR(IVARIM+1)
         VIM(3)    = ZR(IVARIM+2)
         VIM(4)    = ZR(IVARIM+3)
         VIM(5)    = ZR(IVARIM+4)
         VIM(6)    = ZR(IVARIM+5)
         VIM(7)    = ZR(IVARIM+6)
         VIM(8)    = ZR(IVARIM+7)
         CALL NMPIME ( OPTION, ZK16(ICOMPO), ALPHA, TREF, XLONG0, A,
     +                 XLONGM, DLONG, NCSTPM, CSTPM, VIM,
     +                 EFFNOM, TEMPM, TEMPP, VIP, EFFNOP, KLV, FONO )
C
         IF ( OPTION .EQ. 'RIGI_MECA_TANG'  ) THEN
            CALL UTPSLG ( NNO, NC, PGL, KLV, MATUU )
         ELSE
            ZR(ICONTP)   = EFFNOP
            IF ( OPTION .EQ. 'FULL_MECA'      ) THEN
               CALL UTPSLG ( NNO, NC, PGL, KLV, MATUU )
            ENDIF
            ZR(IVARIP)   = VIP(1)
            ZR(IVARIP+1) = VIP(2)
            ZR(IVARIP+2) = VIP(3)
            ZR(IVARIP+3) = VIP(4)
            ZR(IVARIP+4) = VIP(5)
            ZR(IVARIP+5) = VIP(6)
            ZR(IVARIP+6) = VIP(7)
            ZR(IVARIP+7) = VIP(8)
            CALL UTPVLG ( NNO, NC, PGL, FONO, VECTU)
         ENDIF
C
C     ---------------------------------------------------
      ELSEIF ( ZK16(ICOMPO) .EQ. 'ELAS' ) THEN
C     ---------------------------------------------------
C
C        RECUPERATION DES CARACTERISTIQUES DU MATERIAU
C
         CALL NMMABA ( ZI(IMATE), ZK16(ICOMPO), E, ALPHA, DSDE, SIGY,
     &              NCSTPM, CSTPM  )
C     
         CALL NMFLGR (ZK16(ICOMPO),E,ALPHA,XLONG0,A,
     &                ZR(IINSTM),ZR(IINSTP),DLONG,
     &                EFFNOM,TEMPM,TEMPP,KLV,EFFNOP,FONO)

         IF ( OPTION .EQ. 'RIGI_MECA_TANG'  ) THEN
            CALL UTPSLG ( NNO, NC, PGL, KLV, MATUU )
         ELSE
            ZR(ICONTP)   = EFFNOP
            IF ( OPTION .EQ. 'FULL_MECA'      ) THEN
               CALL UTPSLG ( NNO, NC, PGL, KLV, MATUU )
            ENDIF
            CALL UTPVLG ( NNO, NC, PGL, FONO, VECTU )
         ENDIF
C
C     ------------
      ELSE
C     ------------
C
         OPTIOZ = OPTION
         CALL R8INIR (NEQ,0.D0,FONO,1)
         CALL R8INIR (NBT,0.D0,KLV,1)     
C RECUP DE E ET ALPHA    
         COMPEL='ELAS'
         CALL NMMABA ( ZI(IMATE), COMPEL, E, ALPHA, DSDE, SIGY,
     +              NCSTPM, CSTPM  )
C
         SIGM(1) = EFFNOM/A
         SIGM(2) = 0.D0
         SIGM(3) = 0.D0
         SIGM(4) = 0.D0
         SIGM(5) = 0.D0
         SIGM(6) = 0.D0
C 
         EPS(1) = (UML(4)-UML(1))/XLONG0
         EPS(2) = 0.D0
         EPS(3) = 0.D0 
         EPS(4) = 0.D0 
         EPS(5) = 0.D0
         EPS(6) = 0.D0  
C                 
         DEPS(1) = DLONG/XLONG0
         DEPS(2) = -(EPS(1)+DEPS(1))         
C             VALEUR FORFAITAIRE POUR LE CAS OU EPS=DEPS=0
         IF (ABS(DEPS(2)) .LE. R8MIEM()) THEN
            IF (ABS(TEMPP-TEMPM) .LE. R8MIEM()) THEN
                DEPS(2)=-0.5D0*SIGM(1)/E
             ELSE
                DEPS(2)=-0.5D0*ALPHA*(TEMPP-TEMPM)
             ENDIF
         ENDIF
         DEPS(3) = 0.D0
         DEPS(4) = 0.D0
         DEPS(5) = 0.D0
         DEPS(6) = 0.D0
C
         READ(ZK16(ICOMPO-1+2),'(I16)') NBVARI         
C
         CALL COMP1D(OPTIOZ,SIGM,EPS,DEPS,TREF,
     +               TEMPM,TEMPP,ZR(IVARIM),VIP,SIGP,DSIDEP)   
C 
         VECTEU = ((OPTION(1:9).EQ.'FULL_MECA') .OR. 
     +            (OPTION(1:9).EQ.'RAPH_MECA'))  
C     
         IF(VECTEU) THEN
C
C ---       STOCKAGE DES VARIABLES INTERNES
C         
            DO 30 I = 1,NBVARI
               ZR(IVARIP+I-1) = VIP(I)
 30         CONTINUE                   
C            
C ---       STOCKAGE DE L'EFFORT NORMAL
C
            ZR(ICONTP) = SIGP(1)*A           
C
C
C ---       CALCUL DES FORCES NODALES
C
            FONO(1) = -SIGP(1)*A
            FONO(4) =  SIGP(1)*A
            FONO(2) = -SIGP(2)*A
            FONO(5) =  SIGP(2)*A             
C
         ENDIF 
C         
C ---    CALCUL DE LA MATRICE TANGENTE
C 
         IF (OPTION .NE. 'RAPH_MECA') THEN 
            IK = 1
            DO 31 I=1,NEQ
               DO 32 J=1,I
                  KLV(IK) = DSIDEP(I,J)*A/XLONG0 
                  IK = IK + 1 
 32            CONTINUE
 31         CONTINUE 
         ENDIF 
C
C
C ---  PASSAGE DE KLV ET FONO DU REPERE LOCAL AU REPERE GLOBAL 
C
         IF ( OPTION .EQ. 'RIGI_MECA_TANG'  ) THEN
            CALL UTPSLG ( NNO, NC, PGL, KLV, MATUU )
         ELSE
            IF ( OPTION .EQ. 'FULL_MECA'      ) THEN
               CALL UTPSLG ( NNO, NC, PGL, KLV, MATUU )
            ENDIF            
            CALL UTPVLG ( NNO, NC, PGL, FONO, VECTU)
         ENDIF
C                       
C     ----------
      ENDIF
C     ----------
C
      IF (NOMTE.EQ.'MECA_BARRE') THEN
         IF ( (OPTION .EQ. 'RIGI_MECA_TANG').OR.
     +       ( OPTION .EQ. 'FULL_MECA')) THEN 
           DO 36 I=1,21
             ZR(IMATUU+I-1) = MATUU(I)
 36        CONTINUE
         ENDIF
         IF (OPTION .NE. 'RIGI_MECA_TANG') THEN
             DO 37 I=1,6
               ZR(IVECTU+I-1) = VECTU(I)
 37          CONTINUE   
         ENDIF
C
      ELSE IF (NOMTE.EQ.'MECA_2D_BARRE') THEN
         IF ( (OPTION .EQ. 'RIGI_MECA_TANG').OR.
     +       ( OPTION .EQ. 'FULL_MECA')) THEN 
            ZR(IMATUU)     =  MATUU(1)
            ZR(IMATUU+1)   =  MATUU(2)
            ZR(IMATUU+2)   =  MATUU(3)
            ZR(IMATUU+3)   =  MATUU(7)
            ZR(IMATUU+4)   =  MATUU(8)
            ZR(IMATUU+5)   =  MATUU(10)
            ZR(IMATUU+6)   =  MATUU(11)
            ZR(IMATUU+7)   =  MATUU(12)
            ZR(IMATUU+8)   =  MATUU(14)
            ZR(IMATUU+9)   =  MATUU(15)
         ENDIF
         IF (OPTION .NE. 'RIGI_MECA_TANG') THEN
C
              ZR(IVECTU)  =  VECTU(1)
              ZR(IVECTU+1) = VECTU(2)
              ZR(IVECTU+2) = VECTU(4)
              ZR(IVECTU+3) = VECTU(5)
         ENDIF

      ENDIF
C
      IF ( OPTION(1:9).EQ.'FULL_MECA'  .OR.  
     +     OPTION(1:9).EQ.'RAPH_MECA'  ) THEN
         CALL JEVECH ( 'PCODRET', 'E', JCRET )
         ZI(JCRET) = 0
      ENDIF
C
      END

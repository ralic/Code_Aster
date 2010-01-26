      SUBROUTINE TE0364 ( OPTION, NOMTE )
      IMPLICIT   NONE
      CHARACTER*16         OPTION, NOMTE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/01/2010   AUTEUR DESOZA T.DESOZA 
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
C  CALCUL CALCUL DES MATRICES DE CONTACT ET DE FROTTEMENT
C  DE COULOMB STANDARD  AVEC LA METHODE CONTINUE (ECP)
C
C  OPTION : 'RIGI_CONT' (CALCUL DES MATRICES DE CONTACT )
C           'RIGI_FROT' (CALCUL DES MATRICES DE FROTTEMENT STANDARD)
C
C  ENTREES  ---> OPTION : OPTION DE CALCUL
C           ---> NOMTE  : NOM DU TYPE ELEMENT
C ----------------------------------------------------------------------
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX --------------------
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX --------------------
C
      INTEGER      I,J,IJ
      INTEGER      NNE,NNM,NNL
      INTEGER      NDDL,NDIM,NBCPS,NBDM
      INTEGER      INDCO,INADH,INDASP,ICOMPL 
      INTEGER      IFROTT,IFORM,IUSURE
      INTEGER      TYPBAR,TYPRAC
      INTEGER      NDEXFR
      REAL*8       COEFFF,LAMBDA
      REAL*8       COEFCR,COEFCS,COEFCP
      REAL*8       COEFFR,COEFFS,COEFFP      
      REAL*8       XPR,YPR,XPC,YPC,HPG,JACOBI
      REAL*8       MMAT(81,81)
      REAL*8       TAU1(3),TAU2(3)
      REAL*8       NORM(3),MPROJN(3,3),MPROJT(3,3)
      REAL*8       RESE(3),NRESE
      REAL*8       JEU,JEUSUP
      REAL*8       DELTAT,BETA,GAMMA,THETA
      REAL*8       GEOMAE(9,3),GEOMAM(9,3)      
      REAL*8       GEOMM(3),GEOME(3)
      REAL*8       DLAGRC,DLAGRF(2)
      REAL*8       DDEPLE(3),DDEPLM(3)
      REAL*8       DEPLME(3),DEPLMM(3)
      REAL*8       FFE(9),DFFE(2,9),DDFFE(3,9)
      REAL*8       FFM(9),DFFM(2,9),DDFFM(3,9)
      REAL*8       FFL(9),DFFL(2,9),DDFFL(3,9)
      CHARACTER*8  NOMMAE,NOMMAM
      REAL*8       KAPPAN,KAPPAV,ASPERI
      REAL*8       KW,HW,CWEAR
      LOGICAL      LFROTT,LAXIS,LUSURE,LSTABC,LSTABF,LCOMPL
      LOGICAL      LADHER,LGLISS
      LOGICAL      DEBUG
      INTEGER      JPCF,JGEOM,JDEPM,JDEPDE
      INTEGER      JMATT
      REAL*8       DELUSU(3),DISSIP,PRFUSU
C           
      REAL*8       MATRCC(9,9)
      REAL*8       MATREE(27,27),MATRMM(27,27)
      REAL*8       MATREM(27,27),MATRME(27,27)
      REAL*8       MATRCE(9,27) ,MATRCM(9,27)  
      REAL*8       MATRMC(27,9) ,MATREC(27,9)
      REAL*8       MATRFF(18,18)   
      REAL*8       MATRFE(18,27),MATRFM(18,27)  
      REAL*8       MATRMF(27,18),MATREF(27,18)         
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      CALL MATINI(81,81,0.D0,MMAT  )
      CALL MATINI( 9, 9,0.D0,MATRCC)
      CALL MATINI(27,27,0.D0,MATREE)
      CALL MATINI(27,27,0.D0,MATRMM)
      CALL MATINI(27,27,0.D0,MATREM)
      CALL MATINI(27,27,0.D0,MATRME)
      CALL MATINI( 9,27,0.D0,MATRCE)
      CALL MATINI( 9,27,0.D0,MATRCM)
      CALL MATINI(27, 9,0.D0,MATREC)
      CALL MATINI(27, 9,0.D0,MATRMC)
      CALL MATINI(18,18,0.D0,MATRFF)
      CALL MATINI(18,27,0.D0,MATRFE)
      CALL MATINI(18,27,0.D0,MATRFM)
      CALL MATINI(27,18,0.D0,MATREF)
      CALL MATINI(27,18,0.D0,MATRMF)
      LUSURE = .FALSE.
      LADHER = .FALSE.
      LGLISS = .FALSE. 
      DEBUG  = .FALSE.  
      INDASP = 0
      INDCO  = 0
C
C --- RECUPERATION DES DONNEES DU CHAM_ELEM DU CONTACT (VOIR MMCHML)
C
      CALL JEVECH('PCONFR','L',JPCF  )
      XPC      =      ZR(JPCF-1+1)
      YPC      =      ZR(JPCF-1+2)
      XPR      =      ZR(JPCF-1+3)
      YPR      =      ZR(JPCF-1+4)
      TAU1(1)  =      ZR(JPCF-1+5)
      TAU1(2)  =      ZR(JPCF-1+6)
      TAU1(3)  =      ZR(JPCF-1+7)
      TAU2(1)  =      ZR(JPCF-1+8)
      TAU2(2)  =      ZR(JPCF-1+9)
      TAU2(3)  =      ZR(JPCF-1+10)
      HPG      =      ZR(JPCF-1+11)
      LAMBDA   =      ZR(JPCF-1+12)
      NDEXFR   = NINT(ZR(JPCF-1+13))
      TYPBAR   = NINT(ZR(JPCF-1+14))
      TYPRAC   = NINT(ZR(JPCF-1+15))
      IFORM    = NINT(ZR(JPCF-1+16))
      COEFCR   =      ZR(JPCF-1+17)
      COEFCS   =      ZR(JPCF-1+18)
      COEFCP   =      ZR(JPCF-1+19)
      IFROTT   = NINT(ZR(JPCF-1+20))
      COEFFF   =      ZR(JPCF-1+21)
      COEFFR   =      ZR(JPCF-1+22)
      COEFFS   =      ZR(JPCF-1+23)
      COEFFP   =      ZR(JPCF-1+24) 
      ICOMPL   = NINT(ZR(JPCF-1+25))
      ASPERI   =      ZR(JPCF-1+26)
      KAPPAN   =      ZR(JPCF-1+27)
      KAPPAV   =      ZR(JPCF-1+28)
      IUSURE   = NINT(ZR(JPCF-1+29))
      KW       =      ZR(JPCF-1+30)
      HW       =      ZR(JPCF-1+31)
      JEUSUP   =      ZR(JPCF-1+32)
      DELTAT   =      ZR(JPCF-1+33)     
      BETA     =      ZR(JPCF-1+34)
      GAMMA    =      ZR(JPCF-1+35)
      THETA    =      ZR(JPCF-1+36)    
      INDCO    = NINT(ZR(JPCF-1+37))
      INDASP   = NINT(ZR(JPCF-1+38))
      IF (HW.NE.0.D0) THEN
        CWEAR = KW/HW
      ELSE
        CWEAR = 0.D0
      ENDIF
      LCOMPL = ICOMPL.EQ.1
C
C --- TERMES DE STABILISATION
C
      LSTABC = COEFCP.NE.0D0
      LSTABF = COEFFP.NE.0D0
C
C --- RECUPERATION DE LA GEOMETRIE ET DES CHAMPS DE DEPLACEMENT
C
      CALL JEVECH('PGEOMER','E',JGEOM )
      CALL JEVECH('PDEPL_P','E',JDEPDE)
      CALL JEVECH('PDEPL_M','L',JDEPM )
C
C --- INFOS SUR LA MAILLE DE CONTACT
C
      LFROTT = IFROTT.EQ.3
      
      CALL MMELEM(NOMTE ,LFROTT,NDIM  ,NDDL  ,NOMMAE,
     &            NNE   ,NOMMAM,NNM   ,NNL   ,NBCPS ,
     &            NBDM  ,LAXIS )
C
C --- REACTUALISATION DE LA GEOMETRIE (MAILLAGE+DEPMOI)
C
      CALL MMREAC(NBDM  ,NDIM  ,NNE   ,NNM   ,JGEOM ,
     &            JDEPM ,GEOMAE,GEOMAM)   
C
C --- FONCTIONS DE FORMES ET DERIVEES 
C
      CALL MMFORM(NDIM  ,NOMMAE,NOMMAM,NNE   ,NNM   ,
     &            XPC   ,YPC   ,XPR   ,YPR   ,FFE   ,
     &            DFFE  ,DDFFE ,FFM   ,DFFM  ,DDFFM ,
     &            FFL   ,DFFL  ,DDFFL )
C
C --- MODIFICATIONS DES FF ET DFF POUR ELEMENTS DE BARSOUM
C
      IF (TYPBAR.NE.0) THEN
        CALL MMMFFM(NOMMAE,XPC   ,YPC   ,TYPBAR,FFE   ,
     &              DFFE  )
        CALL MMMFFM(NOMMAM,XPR   ,YPR   ,TYPBAR,FFM   ,
     &              DFFM  )
        CALL MMMFFM(NOMMAE,XPC   ,YPC   ,TYPBAR,FFL   ,
     &              DFFL  ) 
                    
      ENDIF
C
C --- JACOBIEN POUR LE POINT DE CONTACT 
C
      CALL MMMJAC(NOMMAE,GEOMAE,FFE   ,DFFE  ,LAXIS ,
     &            NDIM  ,JACOBI)
C
C --- CALCUL DE LA NORMALE ET DES MATRICES DE PROJECTION
C  
      CALL MMCALN(NDIM  ,TAU1  ,TAU2  ,NORM  ,MPROJN,
     &            MPROJT)
C
C --- CALCUL DES COORDONNEES ACTUALISEES
C 
      CALL MMGEOM(NDIM  ,NNE   ,NNM   ,FFE   ,FFM   ,
     &            GEOMAE,GEOMAM,GEOME ,GEOMM ) 
C
C --- CALCUL DES INCREMENTS - LAGRANGE DE CONTACT ET FROTTEMENT
C       
      CALL MMLAGM(NBDM  ,NDIM  ,NNL   ,JDEPDE,FFL   ,
     &            DLAGRC,DLAGRF)
C
C --- MISE A JOUR DES CHAMPS INCONNUS INCREMENTAUX - DEPLACEMENTS
C 
      CALL MMDEPM(NBDM  ,NDIM  ,NNE   ,NNM   ,JDEPM ,
     &            JDEPDE,FFE   ,FFM   ,DDEPLE,DDEPLM ,
     &            DEPLME,DEPLMM)
C 
C --- CALCUL USURE
C 
      IF (IUSURE .EQ. 1) THEN
        CALL MMUSUM(NDIM  ,MPROJT,DDEPLE,DDEPLM,PRFUSU,
     &              DELUSU,DISSIP)
        LUSURE = DISSIP.NE.0.D0
      ENDIF  
C
C --- CALCUL DU JEU
C
      CALL MMMJEU(NDIM,  JEUSUP,PRFUSU,NORM  ,GEOME ,
     &            GEOMM ,DDEPLE,DDEPLM,JEU   ) 
C
C --- COEFFICIENTS MODIFIES POUR FORMULATION EN THETA-VITESSE
C
      IF (IFORM .EQ. 2) THEN
        COEFCR = COEFCR/DELTAT/THETA
        COEFCS = COEFCS/DELTAT/THETA
      ENDIF
C
C --- CALCUL DES MATRICES DE CONTACT
C
      IF (OPTION.EQ.'RIGI_CONT') THEN
        LFROTT = .FALSE.
        NDEXFR = 0
        IF ((TYPBAR.NE.0).OR.(TYPRAC.NE.0)) THEN
          CALL MMMMAT('CONT',
     &                  LUSURE,LSTABC,LCOMPL,LFROTT,LSTABF,
     &                  LADHER,LGLISS,NDIM  ,NNE   ,NNM   ,
     &                  NNL   ,NBCPS ,NORM  ,TAU1  ,TAU2  ,
     &                  MPROJN,MPROJT,HPG   ,FFE   ,FFM   ,
     &                  FFL   ,JACOBI,COEFCP,COEFCR,COEFCS,
     &                  COEFFP,COEFFR,COEFFS,COEFFF,JEU   ,
     &                  LAMBDA,RESE  ,NRESE ,TYPBAR,TYPRAC,
     &                  NDEXFR,ASPERI,KAPPAN,KAPPAV,DELTAT,
     &                  BETA  ,GAMMA ,CWEAR ,DISSIP,DLAGRC,
     &                  DELUSU,MATREE,MATRMM,MATREM,MATRME,
     &                  MATRCE,MATRCM,MATRMC,MATREC,MATRCC,
     &                  MATRFE,MATRFM,MATREF,MATRMF,MATRFF)
        ELSE
          IF (INDASP.EQ.0) THEN
            CALL MMMMAT('SANS',
     &                  LUSURE,LSTABC,LCOMPL,LFROTT,LSTABF,
     &                  LADHER,LGLISS,NDIM  ,NNE   ,NNM   ,
     &                  NNL   ,NBCPS ,NORM  ,TAU1  ,TAU2  ,
     &                  MPROJN,MPROJT,HPG   ,FFE   ,FFM   ,
     &                  FFL   ,JACOBI,COEFCP,COEFCR,COEFCS,
     &                  COEFFP,COEFFR,COEFFS,COEFFF,JEU   ,
     &                  LAMBDA,RESE  ,NRESE ,TYPBAR,TYPRAC,
     &                  NDEXFR,ASPERI,KAPPAN,KAPPAV,DELTAT,
     &                  BETA  ,GAMMA ,CWEAR ,DISSIP,DLAGRC,
     &                  DELUSU,MATREE,MATRMM,MATREM,MATRME,
     &                  MATRCE,MATRCM,MATRMC,MATREC,MATRCC,
     &                  MATRFE,MATRFM,MATREF,MATRMF,MATRFF)
          ELSE IF (INDASP.EQ.1) THEN
            IF (INDCO.EQ.0) THEN
              CALL MMMMAT('SANS',
     &                  LUSURE,LSTABC,LCOMPL,LFROTT,LSTABF,
     &                  LADHER,LGLISS,NDIM  ,NNE   ,NNM   ,
     &                  NNL   ,NBCPS ,NORM  ,TAU1  ,TAU2  ,
     &                  MPROJN,MPROJT,HPG   ,FFE   ,FFM   ,
     &                  FFL   ,JACOBI,COEFCP,COEFCR,COEFCS,
     &                  COEFFP,COEFFR,COEFFS,COEFFF,JEU   ,
     &                  LAMBDA,RESE  ,NRESE ,TYPBAR,TYPRAC,
     &                  NDEXFR,ASPERI,KAPPAN,KAPPAV,DELTAT,
     &                  BETA  ,GAMMA ,CWEAR ,DISSIP,DLAGRC,
     &                  DELUSU,MATREE,MATRMM,MATREM,MATRME,
     &                  MATRCE,MATRCM,MATRMC,MATREC,MATRCC,
     &                  MATRFE,MATRFM,MATREF,MATRMF,MATRFF)
            ELSEIF (INDCO.EQ.1) THEN
              CALL MMMMAT('CONT',
     &                  LUSURE,LSTABC,LCOMPL,LFROTT,LSTABF,
     &                  LADHER,LGLISS,NDIM  ,NNE   ,NNM   ,
     &                  NNL   ,NBCPS ,NORM  ,TAU1  ,TAU2  ,
     &                  MPROJN,MPROJT,HPG   ,FFE   ,FFM   ,
     &                  FFL   ,JACOBI,COEFCP,COEFCR,COEFCS,
     &                  COEFFP,COEFFR,COEFFS,COEFFF,JEU   ,
     &                  LAMBDA,RESE  ,NRESE ,TYPBAR,TYPRAC,
     &                  NDEXFR,ASPERI,KAPPAN,KAPPAV,DELTAT,
     &                  BETA  ,GAMMA ,CWEAR ,DISSIP,DLAGRC,
     &                  DELUSU,MATREE,MATRMM,MATREM,MATRME,
     &                  MATRCE,MATRCM,MATRMC,MATREC,MATRCC,
     &                  MATRFE,MATRFM,MATREF,MATRMF,MATRFF)
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF 
          ELSE
            CALL ASSERT(.FALSE.)        
          ENDIF
        ENDIF  
      ELSE IF (OPTION.EQ.'RIGI_FROT') THEN

        IF (COEFFF.EQ.0.D0) INDCO = 0
        IF (LAMBDA.EQ.0.D0) INDCO = 0
        IF (.NOT.LFROTT)    INDCO = 0
        TYPBAR = 0
        TYPRAC = 0
        IF (INDCO.EQ.0) THEN
          CALL MMMMAT('SANS',
     &                  LUSURE,LSTABC,LCOMPL,LFROTT,LSTABF,
     &                  LADHER,LGLISS,NDIM  ,NNE   ,NNM   ,
     &                  NNL   ,NBCPS ,NORM  ,TAU1  ,TAU2  ,
     &                  MPROJN,MPROJT,HPG   ,FFE   ,FFM   ,
     &                  FFL   ,JACOBI,COEFCP,COEFCR,COEFCS,
     &                  COEFFP,COEFFR,COEFFS,COEFFF,JEU   ,
     &                  LAMBDA,RESE  ,NRESE ,TYPBAR,TYPRAC,
     &                  NDEXFR,ASPERI,KAPPAN,KAPPAV,DELTAT,
     &                  BETA  ,GAMMA ,CWEAR ,DISSIP,DLAGRC,
     &                  DELUSU,MATREE,MATRMM,MATREM,MATRME,
     &                  MATRCE,MATRCM,MATRMC,MATREC,MATRCC,
     &                  MATRFE,MATRFM,MATREF,MATRMF,MATRFF)
        ELSE IF (INDCO.EQ.1) THEN
          CALL TTPRSM(NDIM  ,DDEPLE,DDEPLM,DLAGRF,COEFFR,
     &                TAU1  ,TAU2  ,MPROJT,INADH ,RESE  ,
     &                NRESE )
          LADHER = INADH.EQ.1
          LGLISS = INADH.EQ.0
          CALL MMMMAT('FROT',
     &                  LUSURE,LSTABC,LCOMPL,LFROTT,LSTABF,
     &                  LADHER,LGLISS,NDIM  ,NNE   ,NNM   ,
     &                  NNL   ,NBCPS ,NORM  ,TAU1  ,TAU2  ,
     &                  MPROJN,MPROJT,HPG   ,FFE   ,FFM   ,
     &                  FFL   ,JACOBI,COEFCP,COEFCR,COEFCS,
     &                  COEFFP,COEFFR,COEFFS,COEFFF,JEU   ,
     &                  LAMBDA,RESE  ,NRESE ,TYPBAR,TYPRAC,
     &                  NDEXFR,ASPERI,KAPPAN,KAPPAV,DELTAT,
     &                  BETA  ,GAMMA ,CWEAR ,DISSIP,DLAGRC,
     &                  DELUSU,MATREE,MATRMM,MATREM,MATRME,
     &                  MATRCE,MATRCM,MATRMC,MATREC,MATRCC,
     &                  MATRFE,MATRFM,MATREF,MATRMF,MATRFF)
        ELSE
          CALL ASSERT(.FALSE.)
        END IF
      ELSE
        CALL ASSERT(.FALSE.)
      END IF          
C
C --- ASSEMBLAGE FINAL
C
      CALL MMMTAS(NBDM  ,NDIM  ,NNL   ,NNE   ,NNM   ,
     &            NBCPS ,LFROTT,MATRCC,MATREE,MATRMM,
     &            MATREM,MATRME,MATRCE,MATRCM,MATRMC,
     &            MATREC,MATRFF,MATRFE,MATRFM,MATRMF,
     &            MATREF,MMAT  )
C
C --- RECUPERATION DE LA MATRICE 'OUT'
C
      CALL JEVECH('PMATUUR','E',JMATT )
C
C --- FIN DE CHANGEMENT ET COPIE
C
      DO 760 J = 1,NDDL
         DO 750 I = 1,J
           IJ = (J-1)*J/2 + I
           ZR(JMATT+IJ-1) = MMAT(I,J)
           IF (DEBUG) THEN
             CALL MMMTDB(MMAT(I,J),'IJ',I,J)
           ENDIF    
 750     CONTINUE
 760  CONTINUE
C
      CALL JEDEMA()
      END

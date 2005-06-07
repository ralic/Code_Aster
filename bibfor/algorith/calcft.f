        SUBROUTINE CALCFT(OPTION,MECA,THMC,HYDR,IMATE,NDIM,DIMDEF,
     +                    DIMCON,YAMEC,YAP1,NBPHA1,YAP2,
     +                    NBPHA2,YATE,ADDETE,ADDEME,ADDEP1,ADDEP2,
     +                    ADCOTE,CONGEM,CONGEP,
     +                    DSDE,EPSV,P1,P2,T,GRAT,PHI,SATBID,PVP,
     +                    RGAZ, RHOD, CPD, BIOT, SATM, SAT,DSATP1, 
     +                    PESA, PERMFH, PERMLI, DPERML, PERMGZ,DPERMS, 
     +                    DPERMP, FICK, DFICKT, DFICKG, LAMBP,
     +                    DLAMBP, RHOL, UNSURK, ALPHA, CPL, LAMBS,
     +                    DLAMBS, VISCL, DVISCL, MAMOLG, CPG, LAMBT,
     +                    DLAMBT,VISCG, 
     +                    DVISCG, MAMOLV, CPVG, VISCVG, DVISVG,
     +                    RETCOM,LAMBCT,RHO11,H11,H12)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C MODIF ALGORITH  DATE 14/03/2005   AUTEUR D6BHHJP J.P.LEFEBVRE 
C RESPONSABLE UFBHHLL C.CHAVANT
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
C TOLE CRP_21
C ======================================================================
C ROUTINE CALC_FLUX_THERM ----------------------------------------------
C CALCULE LES CONTRAINTES GENERALISEES ET LA MATRICE TANGENTE DES FLUX -
C OUT RETCOM : RETOUR LOI DE COMPORTEMENT ------------------------------
C ======================================================================
      IMPLICIT      NONE
      INTEGER       NDIM,DIMDEF,DIMCON,IMATE,RETCOM
      INTEGER       YAMEC,YAP1,NBPHA1,YAP2,NBPHA2,YATE
      INTEGER       ADDETE,ADDEME,ADDEP1,ADDEP2,ADCOTE
      REAL*8        CONGEM(1:DIMCON),CONGEP(1:DIMCON)
      REAL*8        DSDE(1:DIMCON,1:DIMDEF),EPSV,P1,P2,MAMOLV
      INTEGER       I,J
      REAL*8        T,GRAT(3),PHI,SAT,DSATP1,PVP,LAMBDT(5),SATBID
      REAL*8        RGAZ, RHOD, CPD, BIOT, SATM, SATUR,DSATUR, PESA(3)
      REAL*8        PERMFH, PERMLI, DPERML, PERMGZ,DPERMS, DPERMP, FICK
      REAL*8        DFICKT, DFICKG, LAMBP,DLAMBP, RHOL,UNSURK,CPL
      REAL*8        ALPHA, LAMBS,DLAMBS, VISCL, DVISCL, CPG, LAMBT
      REAL*8        DLAMBT,VISCG, DVISCG, MAMOLG, CPVG, VISCVG, DVISVG
      REAL*8        LAMBCT,RHO11,H11,H12,RHO12
      CHARACTER*16  OPTION,MECA,THMC,HYDR
C    PARAMETRE POUR LA RECUP DES COEF MECA
      INTEGER      NELAS
      PARAMETER  ( NELAS=4 )
      REAL*8       ELAS(NELAS),YOUNG,ALPHA0,CS,K0,NU
      CHARACTER*8  NCRA1(NELAS)
      CHARACTER*2  CODRET(NELAS)
C ======================================================================
C --- DONNEES POUR RECUPERER LES CARACTERISTIQUES MECANIQUES -----------
C ======================================================================
      DATA NCRA1/'E','NU','RHO','ALPHA'/
C =====================================================================
C ---       RECUPERATION DES COEFFICIENTS MECANIQUES ------------------
C =====================================================================

         IF (YAMEC.EQ.1) THEN
             CALL RCVALA(IMATE,' ','ELAS',0,' ',0.D0,NELAS,NCRA1,
     &           ELAS,CODRET,'FM')
             YOUNG = ELAS(1)
             NU = ELAS(2)
             ALPHA0 = ELAS(4)
             K0 = YOUNG/3.D0/ (1.D0-2.D0*NU)
             CS = (1.D0-BIOT)/K0
         ELSE
C =====================================================================
C --- EN ABSENCE DE MECA ALPHA0 = 0 et 1/KS = 0       -------------
C =====================================================================
           ALPHA0 = 0.D0
           CS = 0.D0
         END IF
         IF (THMC.EQ.'GAZ') THEN
            SAT = 0.D0
            DSATP1 = 0.D0
         ELSE IF(THMC.EQ.'LIQU_SATU')THEN
            SAT = 1.D0
            DSATP1 = 0.D0
         ENDIF
C =====================================================================
C           LAMBDT(1) : LAMBDA
C           LAMBDT(2) : DLAMB / DEPSV
C           LAMBDT(3) : DLAMB / DP1
C           LAMBDT(4) : DLAMB / DP2
C           LAMBDT(5) : DLAMB / DT
C =====================================================================
C
         IF(THMC.EQ.'LIQU_VAPE')THEN
           RHO12=MAMOLV*PVP/RGAZ/T
           LAMBDT(1) = LAMBS*LAMBP*LAMBT + LAMBCT
           LAMBDT(2) = (BIOT-PHI)*DLAMBP*LAMBS*LAMBT
           LAMBDT(3) =(RHO12/RHO11-1.D0)* LAMBP*DLAMBS*LAMBT*DSATP1
     +            +CS*(SAT+(1.D0-SAT)*RHO12/RHO11)*(BIOT-PHI)*
     +             DLAMBP*LAMBS*LAMBT
           LAMBDT(4) =0.D0
           LAMBDT(5) = LAMBS*LAMBP*DLAMBT 
     +            +(BIOT-PHI)*(-3.D0*ALPHA0+CS*(1.D0-SAT)*
     +            RHO12*(H12-H11)/T)*DLAMBP*LAMBS*LAMBT
     +            +LAMBP*DLAMBS*LAMBT*DSATP1*RHO12*(H12-H11)/T
         ELSE
           LAMBDT(1) = LAMBS*LAMBP*LAMBT + LAMBCT
           LAMBDT(2) = (BIOT-PHI)*DLAMBP*LAMBS*LAMBT
           LAMBDT(3) = LAMBP*DLAMBS*LAMBT*DSATP1
     +            -SAT*CS*(BIOT-PHI)*DLAMBP*LAMBS*LAMBT
           LAMBDT(4) = CS*(BIOT-PHI)*DLAMBP*LAMBS*LAMBT
           LAMBDT(5) = LAMBS*LAMBP*DLAMBT 
     +            -(BIOT-PHI)*3.D0*ALPHA0*DLAMBP*LAMBS*LAMBT
         ENDIF
         
C =====================================================================
C --- CALCUL DU FLUX THERMIQUE ----------------------------------------
C =====================================================================
      IF ((OPTION(1:9).EQ.'RIGI_MECA') .OR.
     +    (OPTION(1:9).EQ.'FULL_MECA')            ) THEN
         DO 100 I=1,NDIM
            DSDE(ADCOTE+I,ADDETE+I)=DSDE(ADCOTE+I,ADDETE+I)-LAMBDT(1)
            DSDE(ADCOTE+I,ADDETE)=DSDE(ADCOTE+I,ADDETE)
     +                            - LAMBDT(5)*GRAT(I)
            IF (YAMEC.EQ.1) THEN
               DO 101 J=1,3
                  DSDE(ADCOTE+I,ADDEME+NDIM-1+J)=
     +              DSDE(ADCOTE+I,ADDEME+NDIM-1+J)-LAMBDT(2)*GRAT(I)
 101           CONTINUE    
            ENDIF
            IF (YAP1.EQ.1) THEN
               DSDE(ADCOTE+I,ADDEP1)=DSDE(ADCOTE+I,ADDEP1)
     +                            - LAMBDT(3)*GRAT(I)
               IF (YAP2.EQ.1) THEN
                  DSDE(ADCOTE+I,ADDEP2)=DSDE(ADCOTE+I,ADDEP2)
     +                               - LAMBDT(4)*GRAT(I)
               ENDIF
            ENDIF
 100     CONTINUE
      ENDIF
      IF ( (OPTION(1:9).EQ.'RAPH_MECA') .OR.
     +     (OPTION(1:9).EQ.'FULL_MECA')      ) THEN 
         DO 102 I=1,NDIM
            CONGEP(ADCOTE+I)=-LAMBDT(1)*GRAT(I)
 102     CONTINUE
      ENDIF
C =====================================================================
      END

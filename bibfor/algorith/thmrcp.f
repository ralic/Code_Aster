      SUBROUTINE THMRCP( ETAPE, IMATE, THMC, MECA, HYDR, THER,
     +                   T0, P10, P20, PHI0, PVP0, T, P1, P1M, P2, PHI,
     +                   END,
     +                   SAT, PVP, RGAZ, RHOD, CPD, BIOT, SATM, SATUR,
     +                   DSATUR, PESA, PERMFH, PERMLI, DPERML, PERMGZ,
     +                   DPERMS, DPERMP, FICK, DFICKT, DFICKG, LAMBDD,
     +                   DLAMBD, RHOL, UNSURK, ALPHA, CPL, LAMBDL,
     +                   DLAMBL, VISCL, DVISCL, MAMOLG, CPG, LAMBDG,
     +                   DLAMBG,VISCG, DVISCG, MAMOLV, CPVG, VISCVG,
     +                   DVISVG)
C =====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/10/2003   AUTEUR ROMEO R.FERNANDES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE UFBHHLL C.CHAVANT
C =====================================================================
C TOLE CRP_20
C TOLE CRP_21
C =====================================================================
C --- BUT : RECUPERER LES DONNEES MATERIAUX THM -----------------------
C =====================================================================
      IMPLICIT      NONE
      INTEGER       IMATE
      REAL*8        T0, P10, P20, PHI0, PVP0, T, P1, P2, PHI, SAT, PVP
      REAL*8        RGAZ, RHOD, CPD, BIOT, SATM, SATUR, DSATUR, PESA(3)
      REAL*8        PERMFH, PERMLI, DPERML, PERMGZ, DPERMS, DPERMP
      REAL*8        FICK, DFICKT, DFICKG, LAMBDD, DLAMBD, RHOL, UNSURK
      REAL*8        ALPHA, CPL, LAMBDL, DLAMBL, VISCL, DVISCL, CPG
      REAL*8        LAMBDG, DLAMBG, VISCG, DVISCG, MAMOLG, CPVG, VISCVG
      REAL*8        DVISVG, END, MAMOLV, P1M
      CHARACTER*8   ETAPE
      CHARACTER*16  MECA,THMC,THER,HYDR
C =====================================================================
C --- VARIABLES LOCALES -----------------------------------------------
C =====================================================================
      INTEGER       II, DIMSAT
      INTEGER       DIM1, DIM2, DIM3, DIM4, DIM5, DIM6, DIM7, DIM8
      INTEGER       DIM9, DIM10, DIM11, DIM12, DIM13, DIM14, DIM15
      INTEGER       DIM16, DIM17, DIM18, DIM19, DIM20, DIM21, DIM22
      INTEGER       DIM23, DIM24, DIM25, DIM26, DIM27, DIM28, DIM29
      INTEGER       DIM30, DIM31, DIM32, DIM33, DIM34, NCON
      PARAMETER   ( DIMSAT =  2 )
      PARAMETER   ( NCON =  9 )
      PARAMETER   ( DIM1   =  5 )
      PARAMETER   ( DIM2   =  3 )
      PARAMETER   ( DIM3   =  4 )
      PARAMETER   ( DIM4   =  4 )
      PARAMETER   ( DIM5   =  2 )
      PARAMETER   ( DIM6   =  6 )
      PARAMETER   ( DIM7   =  4 )
      PARAMETER   ( DIM8   =  2 )
      PARAMETER   ( DIM9   =  6 )
      PARAMETER   ( DIM10  =  4 )
      PARAMETER   ( DIM11  =  2 )
      PARAMETER   ( DIM12  =  2 )
      PARAMETER   ( DIM13  =  6 )
      PARAMETER   ( DIM14  =  4 )
      PARAMETER   ( DIM15  =  2 )
      PARAMETER   ( DIM16  =  5 )
      PARAMETER   ( DIM17  =  4 )
      PARAMETER   ( DIM18  =  7 )
      PARAMETER   ( DIM19  =  6 )
      PARAMETER   ( DIM20  =  8 )
      PARAMETER   ( DIM21  =  5 )
      PARAMETER   ( DIM22  = 15 )
      PARAMETER   ( DIM23  =  6 )
      PARAMETER   ( DIM24  =  3 )
      PARAMETER   ( DIM25  = 18 )
      PARAMETER   ( DIM26  =  6 )
      PARAMETER   ( DIM27  =  5 )
      PARAMETER   ( DIM28  =  1 )
      PARAMETER   ( DIM29  = 15 )
      PARAMETER   ( DIM30  =  6 )
      PARAMETER   ( DIM31  =  5 )
      PARAMETER   ( DIM32  = 11 )
      PARAMETER   ( DIM33  =  6 )
      PARAMETER   ( DIM34  =  5 )
      REAL*8        VAL1(DIM1), VAL2(DIM2), VAL3(DIM3), VAL4(DIM4)
      REAL*8        VAL5(DIM5), VAL6(DIM6), VAL7(DIM7), VAL8(DIM8)
      REAL*8        VAL9(DIM9+1), VAL10(DIM10), VAL11(DIM11), RBID1
      REAL*8        VAL12(DIM12), VAL13(DIM13+1), VAL14(DIM14)
      REAL*8        VAL15(DIM15), VAL16(DIM16+1), VAL17(DIM17)
      REAL*8        VAL18(DIM18), VAL19(DIM19), VAL20(DIM20)
      REAL*8        VAL21(DIM21), VAL22(DIM22), VAL23(DIM23)
      REAL*8        VAL24(DIM24), VAL25(DIM25), VAL26(DIM26)
      REAL*8        VAL27(DIM27), VAL28(DIM28), VAL29(DIM29)
      REAL*8        VAL30(DIM30), VAL31(DIM31), VAL32(DIM32)
      REAL*8        VAL33(DIM33), VAL34(DIM34), VALSAT(DIMSAT)
      REAL*8        VALPAR(3), COND(NCON), R8VIDE
      CHARACTER*2   CODRET(20)
      CHARACTER*4   NOMPAR(3)
      CHARACTER*8   NCRA1(DIM1), NCRA2(DIM2), NCRA3(DIM3), NCRA4(DIM4)
      CHARACTER*8   NCRA5(DIM5), NCRA6(DIM6), NCRA7(DIM7), NCRA8(DIM8)
      CHARACTER*8   NCRA9(DIM9), NCRA10(DIM10), NCRA11(DIM11)
      CHARACTER*8   NCRA12(DIM12), NCRA13(DIM13), NCRA14(DIM14)
      CHARACTER*8   NCRA15(DIM15), NCRA16(DIM16), NCRA17(DIM17)
      CHARACTER*8   NCRA18(DIM18), NCRA19(DIM19), NCRA20(DIM20)
      CHARACTER*8   NCRA21(DIM21), NCRA22(DIM22), NCRA23(DIM23)
      CHARACTER*8   NCRA24(DIM24), NCRA25(DIM25), NCRA26(DIM26)
      CHARACTER*8   NCRA27(DIM27), NCRA28(DIM28), NCRA29(DIM29)
      CHARACTER*8   NCRA30(DIM30), NCRA31(DIM31), NCRA32(DIM32)
      CHARACTER*8   NCRA33(DIM33), NCRA34(DIM34), NSAT(DIMSAT)
C =====================================================================
C --- DEFINITION DES DONNEES INITIALES --------------------------------
C =====================================================================
      DATA NCRA1  / 'TEMP'     ,
     +              'PRE1'     ,
     +              'PRE2'     ,
     +              'PORO'     ,
     +              'PRES_VAP' /
C =====================================================================
C --- DEFINITION DES DONNEES INTERMADIAIRES DANS LE CAS LIQU_SATU -----
C =====================================================================
      DATA NCRA2  / 'RHO'      ,
     +              'BIOT_COE' ,
     +              'CP'       /
      DATA NCRA3  / 'RHO'      ,
     +              'UN_SUR_K' ,
     +              'ALPHA'    ,
     +              'CP'       /
C =====================================================================
C --- DEFINITION DES DONNEES INTERMADIAIRES DANS LE CAS GAZ -----------
C =====================================================================
      DATA NCRA4  / 'R_GAZ'    ,
     +              'RHO'      ,
     +              'BIOT_COE' ,
     +              'CP'       /
      DATA NCRA5  / 'MASS_MOL' ,
     +              'CP'       /
C =====================================================================
C --- DEFINITION DES DONNEES INTERMADIAIRES DANS LE CAS LIQU_VAPE -----
C =====================================================================
      DATA NCRA6  / 'R_GAZ'    ,
     +              'RHO'      ,
     +              'BIOT_COE' ,
     +              'CP'       ,
     +              'SATU_PRE' ,
     +              'D_SATU_P' /
      DATA NCRA7  / 'RHO'      ,
     +              'UN_SUR_K' ,
     +              'ALPHA'    ,
     +              'CP'       /
      DATA NCRA8  / 'MASS_MOL' ,
     +              'CP'       /
C =====================================================================
C --- DEFINITION DES DONNEES INTERMADIAIRES DANS LE CAS LIQU_VAPE_GAZ -
C =====================================================================
      DATA NCRA9  / 'R_GAZ'    ,
     +              'RHO'      ,
     +              'BIOT_COE' ,
     +              'CP'       ,
     +              'SATU_PRE' ,
     +              'D_SATU_P' /
      DATA NCRA10 / 'RHO'      ,
     +              'UN_SUR_K' ,
     +              'ALPHA'    ,
     +              'CP'       /
      DATA NCRA11 / 'MASS_MOL' ,
     +              'CP'       /
      DATA NCRA12 / 'MASS_MOL' ,
     +              'CP'       /
C =====================================================================
C --- DEFINITION DES DONNEES INTERMADIAIRES DANS LE CAS LIQU_GAZ ------
C =====================================================================
      DATA NCRA13 / 'R_GAZ'    ,
     +              'RHO'      ,
     +              'BIOT_COE' ,
     +              'CP'       ,
     +              'SATU_PRE' ,
     +              'D_SATU_P' /
      DATA NCRA14 / 'RHO'      ,
     +              'UN_SUR_K' ,
     +              'ALPHA'    ,
     +              'CP'       /
      DATA NCRA15 / 'MASS_MOL' ,
     +              'CP'       /
C =====================================================================
C --- DEFINITION DES DONNEES INTERMADIAIRES DANS LE CAS LIQU_GAZ_ATM --
C =====================================================================
      DATA NCRA16 / 'RHO'      ,
     +              'BIOT_COE' ,
     +              'CP'       ,
     +              'SATU_PRE' ,
     +              'D_SATU_P' /
      DATA NCRA17 / 'RHO'      ,
     +              'UN_SUR_K' ,
     +              'ALPHA'    ,
     +              'CP'       /
C =====================================================================
C --- DEFINITION DES DONNEES FINALES DANS LE CAS LIQU_SATU ------------
C =====================================================================
      DATA NCRA18 / 'PESA_X'   ,
     +              'PESA_Y'   ,
     +              'PESA_Z'   ,
     +              'PERM_IN'  ,
     +              'PERM_END' ,
     +              'LAMBDA'   ,
     +              'D_LAMBDA' /
      DATA NCRA19 / 'UN_SUR_K' ,
     +              'VISC'     ,
     +              'D_VISC_T' ,
     +              'ALPHA'    ,
     +              'LAMBDA'   ,
     +              'D_LAMBDA' /
C =====================================================================
C --- DEFINITION DES DONNEES FINALES DANS LE CAS GAZ ------------------
C =====================================================================
      DATA NCRA20 / 'R_GAZ'    ,
     +              'PESA_X'   ,
     +              'PESA_Y'   ,
     +              'PESA_Z'   ,
     +              'PERM_IN'  ,
     +              'PERM_END' ,
     +              'LAMBDA'   ,
     +              'D_LAMBDA' /
      DATA NCRA21 / 'MASS_MOL' ,
     +              'VISC'     ,
     +              'D_VISC_T' ,
     +              'LAMBDA'   ,
     +              'D_LAMBDA' /
C =====================================================================
C --- DEFINITION DES DONNEES FINALES DANS LE CAS LIQU_VAPE ------------
C =====================================================================
      DATA NCRA22 / 'R_GAZ'    ,
     +              'PESA_X'   ,
     +              'PESA_Y'   ,
     +              'PESA_Z'   ,
     +              'PERM_IN'  ,
     +              'PERM_END' ,
     +              'LAMBDA'   ,
     +              'D_LAMBDA' ,
     +              'SATU_PRE' ,
     +              'D_SATU_P' ,
     +              'PERM_LIQ' ,
     +              'D_PERM_L' ,
     +              'PERM_GAZ' ,
     +              'D_PERM_S' ,
     +              'D_PERM_P' /
      DATA NCRA23 / 'UN_SUR_K' ,
     +              'VISC'     ,
     +              'D_VISC_T' ,
     +              'ALPHA'    ,
     +              'LAMBDA'   ,
     +              'D_LAMBDA' /
      DATA NCRA24 / 'MASS_MOL' ,
     +              'VISC'     ,
     +              'D_VISC_T' /
C =====================================================================
C --- DEFINITION DES DONNEES FINALES DANS LE CAS LIQU_VAPE_GAZ --------
C =====================================================================
      DATA NCRA25 / 'R_GAZ'    ,
     +              'PESA_X'   ,
     +              'PESA_Y'   ,
     +              'PESA_Z'   ,
     +              'PERM_IN'  ,
     +              'PERM_END' ,
     +              'LAMBDA'   ,
     +              'D_LAMBDA' ,
     +              'SATU_PRE' ,
     +              'D_SATU_P' ,
     +              'PERM_LIQ' ,
     +              'D_PERM_L' ,
     +              'PERM_GAZ' ,
     +              'D_PERM_S' ,
     +              'D_PERM_P' ,
     +              'FICK'     ,
     +              'D_FICK_T' ,
     +              'D_FICK_G' /
      DATA NCRA26 / 'UN_SUR_K' ,
     +              'VISC'     ,
     +              'D_VISC_T' ,
     +              'ALPHA'    ,
     +              'LAMBDA'   ,
     +              'D_LAMBDA' /
      DATA NCRA27 / 'MASS_MOL' ,
     +              'VISC'     ,
     +              'D_VISC_T' ,
     +              'LAMBDA'   ,
     +              'D_LAMBDA' /
      DATA NCRA28 / 'MASS_MOL' /
C =====================================================================
C --- DEFINITION DES DONNEES FINALES DANS LE CAS LIQU_GAZ -------------
C =====================================================================
      DATA NCRA29 / 'R_GAZ'    ,
     +              'PESA_X'   ,
     +              'PESA_Y'   ,
     +              'PESA_Z'   ,
     +              'PERM_IN'  ,
     +              'PERM_END' ,
     +              'LAMBDA'   ,
     +              'D_LAMBDA' ,
     +              'SATU_PRE' ,
     +              'D_SATU_P' ,
     +              'PERM_LIQ' ,
     +              'D_PERM_L' ,
     +              'PERM_GAZ' ,
     +              'D_PERM_S' ,
     +              'D_PERM_P' /
      DATA NCRA30 / 'UN_SUR_K' ,
     +              'VISC'     ,
     +              'D_VISC_T' ,
     +              'ALPHA'    ,
     +              'LAMBDA'   ,
     +              'D_LAMBDA' /
      DATA NCRA31 / 'MASS_MOL' ,
     +              'VISC'     ,
     +              'D_VISC_T' ,
     +              'LAMBDA'   ,
     +              'D_LAMBDA' /
C =====================================================================
C --- DEFINITION DES DONNEES FINALES DANS LE CAS LIQU_GAZ_ATM ---------
C =====================================================================
      DATA NCRA32 / 'PESA_X'   ,
     +              'PESA_Y'   ,
     +              'PESA_Z'   ,
     +              'PERM_IN'  ,
     +              'PERM_END' ,
     +              'LAMBDA'   ,
     +              'D_LAMBDA' ,
     +              'SATU_PRE' ,
     +              'D_SATU_P' ,
     +              'PERM_LIQ' ,
     +              'D_PERM_L' /
      DATA NCRA33 / 'UN_SUR_K' ,
     +              'VISC'     ,
     +              'D_VISC_T' ,
     +              'ALPHA'    ,
     +              'LAMBDA'   ,
     +              'D_LAMBDA' /
      DATA NCRA34 / 'MASS_MOL' ,
     +              'VISC'     ,
     +              'D_VISC_T' ,
     +              'LAMBDA'   ,
     +              'D_LAMBDA' /
C =====================================================================
C --- DEFINITION SATURATION -------------------------------------------
C =====================================================================
      DATA NSAT   / 'SATU_PRE' ,
     +              'D_SATU_P' /
C =====================================================================
C --- CAS DE L'INITIALISATION -----------------------------------------
C =====================================================================
      IF (ETAPE.EQ.'INITIALI') THEN
         DO 10 II = 1, DIM1
            VAL1(II) = 0.0D0
 10      CONTINUE
         VAL1(1) = R8VIDE()
         CALL RCVALA(IMATE, 'THM_INIT', 0, ' ', 0.D0,
     +                                  DIM1, NCRA1, VAL1, CODRET, ' ')
         T0   = VAL1(1)
         P10  = VAL1(2)
         P20  = VAL1(3)
         PHI0 = VAL1(4)
         PVP0 = VAL1(5)
         IF ( (THMC.EQ.'GAZ')           .OR.
     +        (THMC.EQ.'LIQU_VAPE')     .OR.
     +        (THMC.EQ.'LIQU_VAPE_GAZ') .OR.
     +        (THMC.EQ.'LIQU_GAZ')           ) THEN
            IF ( T0.EQ.R8VIDE() ) THEN
               CALL UTMESS('F','THMRCP','LA DEFINITION DE LA '//
     +           'TEMPERATURE EST OBLIGATOIRE POUR UNE LOI DE'//
     +           ' COUPLAGE DE TYPE '//THMC)
            ENDIF
         ENDIF
         IF ( T0.EQ.R8VIDE() ) T0 = 0.0D0
      ELSE IF (ETAPE.EQ.'INTERMED') THEN
C =====================================================================
C --- CAS INTERMEDIAIRE -----------------------------------------------
C =====================================================================
         IF (THMC.EQ.'LIQU_SATU') THEN
C =====================================================================
C --- LOI DE COUPLAGE DE TYPE LIQU_SATU -------------------------------
C =====================================================================
            DO 20 II = 1, DIM2
               VAL2(II) = 0.0D0
 20         CONTINUE
            DO 30 II = 1, DIM3
               VAL3(II) = 0.0D0
 30         CONTINUE
            CALL RCVALA(IMATE, 'THM_DIFFU', 0, ' ', 0.0D0,
     +                                     2, NCRA2, VAL2, CODRET, ' ')
            CALL RCVALA(IMATE, 'THM_LIQU', 0, ' ', 0.0D0,
     +                                     2, NCRA3, VAL3, CODRET, ' ')
            IF (THER.NE.' ') THEN
               CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'TEMP', T,
     +                              1, NCRA2(3), VAL2(3), CODRET, 'FM')
               CALL RCVALA(IMATE, 'THM_LIQU', 1, 'TEMP', T,
     +                              2, NCRA3(3), VAL3(3), CODRET, 'FM')
            ENDIF
            RHOD    = VAL2(1)
            BIOT    = VAL2(2)
            CPD     = VAL2(3)
            SATUR   = 1.0D0
            RHOL    = VAL3(1)
            UNSURK  = VAL3(2)
            ALPHA   = VAL3(3)
            CPL     = VAL3(4)
         ELSE IF (THMC.EQ.'GAZ') THEN
C =====================================================================
C --- LOI DE COUPLAGE DE TYPE GAZ -------------------------------------
C =====================================================================
            DO 40 II = 1, DIM4
               VAL4(II) = 0.0D0
 40         CONTINUE
            DO 50 II = 1, DIM5
               VAL5(II) = 0.0D0
 50         CONTINUE
            CALL RCVALA(IMATE, 'THM_DIFFU', 0, ' ', 0.0D0,
     +                                     3, NCRA4, VAL4, CODRET, ' ')
            CALL RCVALA(IMATE, 'THM_GAZ', 0, ' ', 0.0D0,
     +                                     1, NCRA5, VAL5, CODRET, ' ')
            IF (THER.NE.' ') THEN
               CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'TEMP', T,
     +                              1, NCRA4(4), VAL4(4), CODRET, 'FM')
               CALL RCVALA(IMATE, 'THM_GAZ', 1, 'TEMP', T,
     +                              1, NCRA5(2), VAL5(2), CODRET, 'FM')
            ENDIF
            RGAZ    = VAL4(1)
            RHOD    = VAL4(2)
            BIOT    = VAL4(3)
            CPD     = VAL4(4)
            SATUR   = 1.0D0
            MAMOLG  = VAL5(1)
            CPG     = VAL5(2)
         ELSE IF (THMC.EQ.'LIQU_VAPE') THEN
C =====================================================================
C --- LOI DE COUPLAGE DE TYPE LIQU_VAPE -------------------------------
C =====================================================================
            DO 60 II = 1, DIM6
               VAL6(II) = 0.0D0
 60         CONTINUE
            DO 70 II = 1, DIM7
               VAL7(II) = 0.0D0
 70         CONTINUE
            DO 80 II = 1, DIM8
               VAL8(II) = 0.0D0
 80         CONTINUE
            CALL RCVALA(IMATE, 'THM_DIFFU', 0, 'TEMP', 0.0D0,
     +                                   3, NCRA6, VAL6, CODRET, ' ')
            CALL RCVALA(IMATE, 'THM_LIQU', 0, ' ', 0.0D0,
     +                                   2, NCRA7, VAL7, CODRET, ' ')
            CALL RCVALA(IMATE, 'THM_VAPE_GAZ', 0, ' ', 0.0D0,
     +                                   2, NCRA8, VAL8, CODRET, ' ')
            IF (THER.NE.' ') THEN
               CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'TEMP', T,
     +                              1, NCRA6(4), VAL6(4), CODRET, 'FM')
               CALL RCVALA(IMATE, 'THM_LIQU', 1, 'TEMP', T,
     +                              2, NCRA7(3), VAL7(3), CODRET, 'FM')
            ENDIF
            IF (HYDR.EQ.'HYDR_UTIL' .OR. HYDR.EQ.'HYDR_ENDO') THEN
               CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'PCAP', P1M,
     +                              1, NCRA6(5), VAL6(5), CODRET, 'FM')
            ELSE
               CALL SATURA(HYDR,P1M,VAL6(5),RBID1  )
            ENDIF
            RGAZ    = VAL6(1)
            RHOD    = VAL6(2)
            BIOT    = VAL6(3)
            CPD     = VAL6(4)
            SATM    = VAL6(5)
            RHOL    = VAL7(1)
            UNSURK  = VAL7(2)
            ALPHA   = VAL7(3)
            CPL     = VAL7(4)
            MAMOLV  = VAL8(1)
            CPVG    = VAL8(2)
         ELSE IF (THMC.EQ.'LIQU_VAPE_GAZ') THEN
C =====================================================================
C --- LOI DE COUPLAGE DE TYPE LIQU_VAPE_GAZ ---------------------------
C =====================================================================
            DO 90 II = 1, DIM9
               VAL9(II) = 0.0D0
 90         CONTINUE
            DO 100 II = 1, DIM10
               VAL10(II) = 0.0D0
 100         CONTINUE
            DO 110 II = 1, DIM11
               VAL11(II) = 0.0D0
 110         CONTINUE
            DO 120 II = 1, DIM12
               VAL12(II) = 0.0D0
 120         CONTINUE
            CALL RCVALA(IMATE, 'THM_DIFFU', 0, ' ', 0.0D0,
     +                                     3, NCRA9, VAL9, CODRET, ' ')
            CALL RCVALA(IMATE, 'THM_LIQU', 0, ' ', 0.0D0,
     +                                   2, NCRA10, VAL10, CODRET, ' ')
            CALL RCVALA(IMATE, 'THM_GAZ', 0, ' ', 0.0D0,
     +                                   1, NCRA11, VAL11, CODRET, ' ')
            CALL RCVALA(IMATE, 'THM_VAPE_GAZ', 0, ' ', 0.0D0,
     +                                   2, NCRA12, VAL12, CODRET, ' ')
            IF (THER.NE.' ') THEN
               CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'TEMP', T,
     +                              1, NCRA9(4), VAL9(4), CODRET, 'FM')
               CALL RCVALA(IMATE, 'THM_LIQU', 1, 'TEMP', T,
     +                            2, NCRA10(3), VAL10(3), CODRET, 'FM')
               CALL RCVALA(IMATE, 'THM_GAZ', 1, 'TEMP', T,
     +                            1, NCRA11(2), VAL11(2), CODRET, 'FM')
            ENDIF
            IF (HYDR.EQ.'HYDR_UTIL' .OR. HYDR.EQ.'HYDR_ENDO') THEN
               CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'PCAP', P1M,
     +                              1, NCRA9(5), VAL9(5), CODRET, 'FM')
               CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'PCAP', P1,
     +                              2, NCRA9(5), VAL9(6), CODRET, 'FM')
            ELSE
               CALL SATURA(HYDR,P1M,VAL9(5),RBID1  )
               CALL SATURA(HYDR,P1 ,VAL9(6),VAL9(7))
            ENDIF
            RGAZ    = VAL9(1)
            RHOD    = VAL9(2)
            BIOT    = VAL9(3)
            CPD     = VAL9(4)
            SATM    = VAL9(5)
            SATUR   = VAL9(6)
            DSATUR  = VAL9(7)
            RHOL    = VAL10(1)
            UNSURK  = VAL10(2)
            ALPHA   = VAL10(3)
            CPL     = VAL10(4)
            MAMOLG  = VAL11(1)
            CPG     = VAL11(2)
            MAMOLV  = VAL12(1)
            CPVG    = VAL12(2)
         ELSE IF (THMC.EQ.'LIQU_GAZ') THEN
C =====================================================================
C --- LOI DE COUPLAGE DE TYPE LIQU_GAZ --------------------------------
C =====================================================================
            DO 130 II = 1, DIM13
               VAL13(II) = 0.0D0
 130         CONTINUE
            DO 140 II = 1, DIM14
               VAL14(II) = 0.0D0
 140        CONTINUE
            DO 150 II = 1, DIM15
               VAL15(II) = 0.0D0
 150        CONTINUE
            CALL RCVALA(IMATE, 'THM_DIFFU', 0, ' ', 0.0D0,
     +                                   3, NCRA13, VAL13, CODRET, ' ')
            CALL RCVALA(IMATE, 'THM_LIQU', 0, ' ', 0.0D0,
     +                                   2, NCRA14, VAL14, CODRET, ' ')
            CALL RCVALA(IMATE, 'THM_GAZ', 0, ' ', 0.0D0,
     +                                   1, NCRA15, VAL15, CODRET, ' ')
            IF (THER.NE.' ') THEN
               CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'TEMP', T,
     +                            1, NCRA13(4), VAL13(4), CODRET, 'FM')
               CALL RCVALA(IMATE, 'THM_LIQU', 1, 'TEMP', T,
     +                            2, NCRA14(3), VAL14(3), CODRET, 'FM')
               CALL RCVALA(IMATE, 'THM_GAZ', 1, 'TEMP', T,
     +                            1, NCRA15(2), VAL15(2), CODRET, 'FM')
            ENDIF
            IF (HYDR.EQ.'HYDR_UTIL' .OR. HYDR.EQ.'HYDR_ENDO') THEN
               CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'PCAP', P1M,
     +                            1, NCRA13(5), VAL13(5), CODRET, 'FM')
               CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'PCAP', P1,
     +                            2, NCRA13(5), VAL13(6), CODRET, 'FM')
            ELSE
               CALL SATURA(HYDR,P1M,VAL13(5),RBID1  )
               CALL SATURA(HYDR,P1,VAL13(6),VAL13(7))
            ENDIF
            RGAZ    = VAL13(1)
            RHOD    = VAL13(2)
            BIOT    = VAL13(3)
            CPD     = VAL13(4)
            SATM    = VAL13(5)
            SATUR   = VAL13(6)
            DSATUR  = VAL13(7)
            RHOL    = VAL14(1)
            UNSURK  = VAL14(2)
            ALPHA   = VAL14(3)
            CPL     = VAL14(4)
            MAMOLG  = VAL15(1)
            CPG     = VAL15(2)
         ELSE IF (THMC.EQ.'LIQU_GAZ_ATM') THEN
C =====================================================================
C --- LOI DE COUPLAGE DE TYPE LIQU_GAZ_ATM ----------------------------
C =====================================================================
            DO 160 II = 1, DIM16
               VAL16(II) = 0.0D0
 160        CONTINUE
            DO 170 II = 1, DIM17
               VAL17(II) = 0.0D0
 170        CONTINUE
            CALL RCVALA(IMATE, 'THM_DIFFU', 0, ' ', 0.0D0,
     +                                 2, NCRA16, VAL16, CODRET, ' ')
            CALL RCVALA(IMATE, 'THM_LIQU', 0, ' ', 0.0D0,
     +                                 2, NCRA17, VAL17, CODRET, ' ')
            IF (THER.NE.' ') THEN
               CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'TEMP', T,
     +                            1, NCRA16(3), VAL16(3), CODRET, 'FM')
               CALL RCVALA(IMATE, 'THM_LIQU', 1, 'TEMP', T,
     +                            2, NCRA17(3), VAL17(3), CODRET, 'FM')
            ENDIF
            IF (HYDR.EQ.'HYDR_UTIL' .OR. HYDR.EQ.'HYDR_ENDO') THEN
               CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'PCAP', P1M,
     +                            1, NCRA16(4), VAL16(4), CODRET, 'FM')
               CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'PCAP', P1,
     +                            2, NCRA16(4), VAL16(5), CODRET, 'FM')
            ELSE
               CALL SATURA(HYDR,P1M,VAL16(4),RBID1  )
               CALL SATURA(HYDR,P1,VAL16(5),VAL16(6))
            ENDIF
            RHOD    = VAL16(1)
            BIOT    = VAL16(2)
            CPD     = VAL16(3)
            SATM    = VAL16(4)
            SATUR   = VAL16(5)
            DSATUR  = VAL16(6)
            RHOL    = VAL17(1)
            UNSURK  = VAL17(2)
            ALPHA   = VAL17(3)
            CPL     = VAL17(4)
         ENDIF
      ELSE IF (ETAPE.EQ.'SATURATI') THEN
         IF (HYDR.EQ.'HYDR_UTIL' .OR. HYDR.EQ.'HYDR_ENDO') THEN
            CALL RCVALA(IMATE,'THM_DIFFU',1,'PCAP',P1,
     +                                  DIMSAT,NSAT,VALSAT,CODRET,'FM')
            SATUR  = VALSAT(1)
            DSATUR = VALSAT(2)
         ELSE
            CALL SATURA(HYDR,P1,SATUR,DSATUR)
         ENDIF
      ELSE IF (ETAPE.EQ.'FINALE') THEN
C =====================================================================
C --- CAS FINAL -------------------------------------------------------
C =====================================================================
         IF (THMC.EQ.'LIQU_SATU') THEN
C =====================================================================
C --- LOI DE COUPLAGE DE TYPE LIQU_SATU -------------------------------
C =====================================================================
            DO 180 II = 1, DIM18
               VAL18(II) = 0.0D0
 180        CONTINUE
            DO 190 II = 1, DIM19
               VAL19(II) = 0.0D0
 190        CONTINUE
            CALL RCVALA(IMATE, 'THM_DIFFU', 0, ' ', 0.0D0,
     +                       DIM18-4, NCRA18(1), VAL18(1), CODRET, ' ')
            IF (HYDR.EQ.'HYDR_UTIL') THEN
               CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'PORO', PHI,
     +                               1, NCRA18(4), VAL18(4),CODRET,' ')
            ELSE IF (HYDR.EQ.'HYDR_ENDO') THEN
               IF ( (MECA.EQ.'MAZARS')          .OR.
     +              (MECA.EQ.'ENDO_ISOT_BETON')      ) THEN
C =====================================================================
C --- ATTENTION DECALAGE VOLONTAIRE SUR LE TABLEAU VAL18 POUR ENDO ----
C =====================================================================
                  CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'ENDO', END,
     +                            1, NCRA18(5), VAL18(4), CODRET, 'FM')
               ENDIF
            ENDIF
            CALL RCVALA(IMATE, 'THM_LIQU', 1, 'TEMP', T,
     +                             DIM19-3, NCRA19, VAL19, CODRET, ' ')
            IF (THER.NE.' ') THEN
               CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'TEMP', T,
     +                            2, NCRA18(6), VAL18(6), CODRET, 'FM')
               CALL RCVALA(IMATE, 'THM_LIQU', 1, 'TEMP', T,
     +                            3, NCRA19(4), VAL19(4), CODRET, 'FM')
            ENDIF
            PESA(1) = VAL18(1)
            PESA(2) = VAL18(2)
            PESA(3) = VAL18(3)
            PERMFH  = VAL18(4)
            LAMBDD  = VAL18(6)
            DLAMBD  = VAL18(7)
            SATUR   = 1.0D0
            DSATUR  = 0.0D0
            UNSURK  = VAL19(1)
            VISCL   = VAL19(2)
            DVISCL  = VAL19(3)
            ALPHA   = VAL19(4)
            LAMBDL  = VAL19(5)
            DLAMBL  = VAL19(6)
            LAMBDG  = 0.0D0
            DLAMBG  = 0.0D0
         ELSE IF (THMC.EQ.'GAZ') THEN
C =====================================================================
C --- LOI DE COUPLAGE DE TYPE GAZ -------------------------------------
C =====================================================================
            DO 200 II = 1, DIM20
               VAL20(II) = 0.0D0
 200        CONTINUE
            DO 210 II = 1, DIM21
               VAL21(II) = 0.0D0
 210        CONTINUE
            CALL RCVALA(IMATE, 'THM_DIFFU', 0, ' ', 0.0D0,
     +                       DIM20-4, NCRA20(1), VAL20(1), CODRET, ' ')
            IF (HYDR.EQ.'HYDR_UTIL') THEN
               CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'PORO', PHI,
     +                               1, NCRA20(5), VAL20(5),CODRET,' ')
            ELSE IF (HYDR.EQ.'HYDR_ENDO') THEN
               IF ( (MECA.EQ.'MAZARS')          .OR.
     +              (MECA.EQ.'ENDO_ISOT_BETON')      ) THEN
C =====================================================================
C --- ATTENTION DECALAGE VOLONTAIRE SUR LE TABLEAU VAL18 POUR ENDO ----
C =====================================================================
                  CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'ENDO', END,
     +                            1, NCRA20(6), VAL20(5), CODRET, 'FM')
               ENDIF
            ENDIF
            CALL RCVALA(IMATE, 'THM_GAZ', 1, 'TEMP', T,
     +                             DIM21-2, NCRA21, VAL21, CODRET, ' ')
            IF (THER.NE.' ') THEN
               CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'TEMP', T,
     +                            2, NCRA20(7), VAL20(7), CODRET, 'FM')
               CALL RCVALA(IMATE, 'THM_GAZ', 1, 'TEMP', T,
     +                            2, NCRA21(4), VAL21(4), CODRET, 'FM')
            ENDIF
            RGAZ    = VAL20(1)
            PESA(1) = VAL20(2)
            PESA(2) = VAL20(3)
            PESA(3) = VAL20(4)
            PERMFH  = VAL20(5)
            LAMBDD  = VAL20(7)
            DLAMBD  = VAL20(8)
            SATUR   = 1.0D0
            DSATUR  = 0.0D0
            MAMOLG  = VAL21(1)
            VISCG   = VAL21(2)
            DVISCG  = VAL21(3)
            LAMBDG  = VAL21(4)
            DLAMBG  = VAL21(5)
            LAMBDL  = 0.0D0
            DLAMBL  = 0.0D0
         ELSE IF (THMC.EQ.'LIQU_VAPE') THEN
C =====================================================================
C --- LOI DE COUPLAGE DE TYPE LIQU_VAPE -------------------------------
C =====================================================================
            DO 220 II = 1, DIM22
               VAL22(II) = 0.0D0
 220        CONTINUE
            DO 230 II = 1, DIM23
               VAL23(II) = 0.0D0
 230        CONTINUE
            DO 240 II = 1, DIM24
               VAL24(II) = 0.0D0
 240        CONTINUE
            CALL RCVALA(IMATE, 'THM_DIFFU', 0, ' ', 0.0D0,
     +                                   4, NCRA22, VAL22, CODRET, ' ')
            IF (HYDR.EQ.'HYDR_UTIL') THEN
               CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'PORO', PHI,
     +                               1, NCRA22(5), VAL22(5),CODRET,' ')
            ELSE IF (HYDR.EQ.'HYDR_ENDO') THEN
               IF ( (MECA.EQ.'MAZARS')          .OR.
     +              (MECA.EQ.'ENDO_ISOT_BETON')      ) THEN
C =====================================================================
C --- ATTENTION DECALAGE VOLONTAIRE SUR LE TABLEAU VAL22 POUR ENDO ----
C =====================================================================
                  CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'ENDO', END,
     +                            1, NCRA22(6), VAL22(5), CODRET, 'FM')
               ENDIF
            ENDIF
            CALL RCVALA(IMATE, 'THM_LIQU', 1, 'TEMP', T,
     +                                   3, NCRA23, VAL23, CODRET, ' ')
            CALL RCVALA(IMATE, 'THM_VAPE_GAZ', 1, 'TEMP', T,
     +                                   3, NCRA24, VAL24, CODRET, ' ')
            IF (THER.NE.' ') THEN
               CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'TEMP', T,
     +                            2, NCRA22(7), VAL22(7), CODRET, 'FM')
               CALL RCVALA(IMATE, 'THM_LIQU', 1, 'TEMP', T,
     +                      DIM23-3, NCRA23(4), VAL23(4), CODRET, 'FM')
            ENDIF
            IF (HYDR.EQ.'HYDR_UTIL' .OR. HYDR.EQ.'HYDR_ENDO') THEN
               CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'PCAP', PVP-P1,
     +                            2, NCRA22(9), VAL22(9), CODRET, 'FM')
            ELSE
               CALL SATURA(HYDR,PVP-P1,VAL22(9),VAL22(10))
            ENDIF
            NOMPAR(1) = 'SAT'
            NOMPAR(2) = 'PGAZ'
            VALPAR(1) =  VAL22(9)
            VALPAR(2) =  P2
            CALL RCVALA(IMATE, 'THM_DIFFU', 2, NOMPAR, VALPAR,
     +                          5, NCRA22(11), VAL22(11), CODRET, 'FM')
            RGAZ    = VAL22( 1)
            PESA(1) = VAL22( 2)
            PESA(2) = VAL22( 3)
            PESA(3) = VAL22( 4)
            PERMFH  = VAL22( 5)
            LAMBDD  = VAL22( 7)
            DLAMBD  = VAL22( 8)
            SATUR   = VAL22( 9)
            DSATUR  = VAL22(10)
            PERMLI  = VAL22(11)
            DPERML  = VAL22(12)
            PERMGZ  = VAL22(13)
            DPERMS  = VAL22(14)
            DPERMP  = VAL22(15)
            UNSURK  = VAL23( 1)
            VISCL   = VAL23( 2)
            DVISCL  = VAL23( 3)
            ALPHA   = VAL23( 4)
            LAMBDL  = VAL23( 5)
            DLAMBL  = VAL23( 6)
            MAMOLV  = VAL24( 1)
            VISCVG  = VAL24( 2)
            DVISVG  = VAL24( 3)
            LAMBDG  = 0.0D0
            DLAMBG  = 0.0D0
         ELSE IF (THMC.EQ.'LIQU_VAPE_GAZ') THEN
C =====================================================================
C --- LOI DE COUPLAGE DE TYPE LIQU_VAPE_GAZ ---------------------------
C =====================================================================
            DO 250 II = 1, DIM25
               VAL25(II) = 0.0D0
 250        CONTINUE
            DO 260 II = 1, DIM26
               VAL26(II) = 0.0D0
 260        CONTINUE
            DO 270 II = 1, DIM27
               VAL27(II) = 0.0D0
 270        CONTINUE
            DO 280 II = 1, DIM28
               VAL28(II) = 0.0D0
 280        CONTINUE
            CALL RCVALA(IMATE, 'THM_DIFFU', 0, ' ', 0.0D0,
     +                                   4, NCRA25, VAL25, CODRET, ' ')
            IF (HYDR.EQ.'HYDR_UTIL') THEN
               CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'PORO', PHI,
     +                               1, NCRA25(5), VAL25(5),CODRET,' ')
            ELSE IF (HYDR.EQ.'HYDR_ENDO') THEN
               IF ( (MECA.EQ.'MAZARS')          .OR.
     +              (MECA.EQ.'ENDO_ISOT_BETON')      ) THEN
C =====================================================================
C --- ATTENTION DECALAGE VOLONTAIRE SUR LE TABLEAU VAL22 POUR ENDO ----
C =====================================================================
                  CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'ENDO', END,
     +                            1, NCRA25(6), VAL25(5), CODRET, 'FM')
               ENDIF
            ENDIF
            CALL RCVALA(IMATE, 'THM_LIQU', 1, 'TEMP', T,
     +                                   3, NCRA26, VAL26, CODRET, ' ')
            CALL RCVALA(IMATE, 'THM_GAZ', 1, 'TEMP', T,
     +                                   3, NCRA27, VAL27, CODRET, ' ')
            CALL RCVALA(IMATE, 'THM_VAPE_GAZ', 0, ' ', 0.0D0,
     +                                   1, NCRA28, VAL28, CODRET, ' ')
            IF (THER.NE.' ') THEN
               CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'TEMP', T,
     +                            2, NCRA25(7), VAL25(7), CODRET, 'FM')
               CALL RCVALA(IMATE, 'THM_LIQU', 1, 'TEMP', T,
     +                      DIM26-3, NCRA26(4), VAL26(4), CODRET, 'FM')
               CALL RCVALA(IMATE, 'THM_GAZ', 1, 'TEMP', T,
     +                      DIM27-3, NCRA27(4), VAL27(4), CODRET, 'FM')
            ENDIF
            IF (HYDR.EQ.'HYDR_UTIL' .OR. HYDR.EQ.'HYDR_ENDO') THEN
               CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'PCAP', P1,
     +                            2, NCRA25(9), VAL25(9), CODRET, 'FM')
            ELSE
               CALL SATURA(HYDR,P1,VAL25(9),VAL25(10))
            ENDIF
            NOMPAR(1) = 'SAT'
            NOMPAR(2) = 'PGAZ'
            NOMPAR(3) = 'TEMP'
            VALPAR(1) =  VAL25(9)
            VALPAR(2) =  P2
            VALPAR(3) =  T
            CALL RCVALA(IMATE, 'THM_DIFFU', 3, NOMPAR, VALPAR,
     +                          8, NCRA25(11), VAL25(11), CODRET, 'FM')
            RGAZ    = VAL25( 1)
            PESA(1) = VAL25( 2)
            PESA(2) = VAL25( 3)
            PESA(3) = VAL25( 4)
            PERMFH  = VAL25( 5)
            LAMBDD  = VAL25( 7)
            DLAMBD  = VAL25( 8)
            SATUR   = VAL25( 9)
            DSATUR  = VAL25(10)
            PERMLI  = VAL25(11)
            DPERML  = VAL25(12)
            PERMGZ  = VAL25(13)
            DPERMS  = VAL25(14)
            DPERMP  = VAL25(15)
            FICK    = VAL25(16)
            DFICKT  = VAL25(17)
            DFICKG  = VAL25(18)
            UNSURK  = VAL26( 1)
            VISCL   = VAL26( 2)
            DVISCL  = VAL26( 3)
            ALPHA   = VAL26( 4)
            LAMBDL  = VAL26( 5)
            DLAMBL  = VAL26( 6)
            MAMOLG  = VAL27( 1)
            VISCG   = VAL27( 2)
            DVISCG  = VAL27( 3)
            LAMBDG  = VAL27( 4)
            DLAMBG  = VAL27( 5)
            MAMOLV  = VAL28( 1)
         ELSE IF (THMC.EQ.'LIQU_GAZ') THEN
C =====================================================================
C --- LOI DE COUPLAGE DE TYPE LIQU_GAZ --------------------------------
C =====================================================================
            DO 290 II = 1, DIM29
               VAL29(II) = 0.0D0
 290        CONTINUE
            DO 300 II = 1, DIM30
               VAL30(II) = 0.0D0
 300        CONTINUE
            DO 310 II = 1, DIM31
               VAL31(II) = 0.0D0
 310        CONTINUE
            CALL RCVALA(IMATE, 'THM_DIFFU', 0, ' ', 0.0D0,
     +                                   4, NCRA29, VAL29, CODRET, ' ')
            IF (HYDR.EQ.'HYDR_UTIL') THEN
               CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'PORO', PHI,
     +                               1, NCRA29(5), VAL29(5),CODRET,' ')
            ELSE IF (HYDR.EQ.'HYDR_ENDO') THEN
               IF ( (MECA.EQ.'MAZARS')          .OR.
     +              (MECA.EQ.'ENDO_ISOT_BETON')      ) THEN
C =====================================================================
C --- ATTENTION DECALAGE VOLONTAIRE SUR LE TABLEAU VAL29 POUR ENDO ----
C =====================================================================
                  CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'ENDO', END,
     +                            1, NCRA29(6), VAL29(5), CODRET, 'FM')
               ENDIF
            ENDIF
            CALL RCVALA(IMATE, 'THM_LIQU', 1, 'TEMP', T,
     +                                   3, NCRA30, VAL30, CODRET, ' ')
            CALL RCVALA(IMATE, 'THM_GAZ', 1, 'TEMP', T,
     +                                   3, NCRA31, VAL31, CODRET, ' ')
            IF (THER.NE.' ') THEN
               CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'TEMP', T,
     +                            2, NCRA29(7), VAL29(7), CODRET, 'FM')
               CALL RCVALA(IMATE, 'THM_LIQU', 1, 'TEMP', T,
     +                      DIM30-3, NCRA30(4), VAL30(4), CODRET, 'FM')
               CALL RCVALA(IMATE, 'THM_GAZ', 1, 'TEMP', T,
     +                      DIM31-3, NCRA31(4), VAL31(4), CODRET, 'FM')
            ENDIF
            IF (HYDR.EQ.'HYDR_UTIL' .OR. HYDR.EQ.'HYDR_ENDO') THEN
               CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'PCAP', P1,
     +                            2, NCRA29(9), VAL29(9), CODRET, 'FM')
            ELSE
               CALL SATURA(HYDR,P1,VAL29(9),VAL29(10))
            ENDIF
            NOMPAR(1) = 'SAT'
            NOMPAR(2) = 'PGAZ'
            VALPAR(1) =  VAL29(9)
            VALPAR(2) =  P2
            CALL RCVALA(IMATE, 'THM_DIFFU', 2, NOMPAR, VALPAR,
     +                          5, NCRA29(11), VAL29(11), CODRET, 'FM')
            RGAZ    = VAL29( 1)
            PESA(1) = VAL29( 2)
            PESA(2) = VAL29( 3)
            PESA(3) = VAL29( 4)
            PERMFH  = VAL29( 5)
            LAMBDD  = VAL29( 7)
            DLAMBD  = VAL29( 8)
            SATUR   = VAL29( 9)
            DSATUR  = VAL29(10)
            PERMLI  = VAL29(11)
            DPERML  = VAL29(12)
            PERMGZ  = VAL29(13)
            DPERMS  = VAL29(14)
            DPERMP  = VAL29(15)
            UNSURK  = VAL30( 1)
            VISCL   = VAL30( 2)
            DVISCL  = VAL30( 3)
            ALPHA   = VAL30( 4)
            LAMBDL  = VAL30( 5)
            DLAMBL  = VAL30( 6)
            MAMOLG  = VAL31( 1)
            VISCG   = VAL31( 2)
            DVISCG  = VAL31( 3)
            LAMBDG  = VAL31( 4)
            DLAMBG  = VAL31( 5)
         ELSE IF (THMC.EQ.'LIQU_GAZ_ATM') THEN
C =====================================================================
C --- LOI DE COUPLAGE DE TYPE LIQU_GAZ_ATM ----------------------------
C =====================================================================
            DO 320 II = 1, DIM32
               VAL32(II) = 0.0D0
 320        CONTINUE
            DO 330 II = 1, DIM33
               VAL33(II) = 0.0D0
 330        CONTINUE
            DO 340 II = 1, DIM34
               VAL34(II) = 0.0D0
 340        CONTINUE
            CALL RCVALA(IMATE, 'THM_DIFFU', 0, ' ', 0.0D0,
     +                                   3, NCRA32, VAL32, CODRET, ' ')
            IF (HYDR.EQ.'HYDR_UTIL') THEN
               CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'PORO', PHI,
     +                               1, NCRA32(4), VAL32(4),CODRET,' ')
            ELSE IF (HYDR.EQ.'HYDR_ENDO') THEN
               IF ( (MECA.EQ.'MAZARS')          .OR.
     +              (MECA.EQ.'ENDO_ISOT_BETON')      ) THEN
C =====================================================================
C --- ATTENTION DECALAGE VOLONTAIRE SUR LE TABLEAU VAL32 POUR ENDO ----
C =====================================================================
                  CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'ENDO', END,
     +                            1, NCRA32(5), VAL32(4), CODRET, 'FM')
               ENDIF
            ENDIF
            CALL RCVALA(IMATE, 'THM_LIQU', 1, 'TEMP', T,
     +                                   3, NCRA33, VAL33, CODRET, ' ')
            CALL RCVALA(IMATE, 'THM_GAZ', 1, 'TEMP', T,
     +                                   3, NCRA34, VAL34, CODRET, ' ')
            IF (THER.NE.' ') THEN
               CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'TEMP', T,
     +                            2, NCRA32(6), VAL32(6), CODRET, 'FM')
               CALL RCVALA(IMATE, 'THM_LIQU', 1, 'TEMP', T,
     +                      DIM33-3, NCRA33(4), VAL33(4), CODRET, 'FM')
               CALL RCVALA(IMATE, 'THM_GAZ', 1, 'TEMP', T,
     +                      DIM34-3, NCRA34(4), VAL34(4), CODRET, 'FM')
            ENDIF
            IF (HYDR.EQ.'HYDR_UTIL' .OR. HYDR.EQ.'HYDR_ENDO') THEN
               CALL RCVALA(IMATE, 'THM_DIFFU', 1, 'PCAP', P1,
     +                            2, NCRA32(8), VAL32(8), CODRET, 'FM')
            ELSE
               CALL SATURA(HYDR,P1,SAT,RBID1)
            ENDIF
            NOMPAR(1) = 'SAT'
            NOMPAR(2) = 'PGAZ'
            VALPAR(1) =  VAL32(8)
            VALPAR(2) =  P2
            CALL RCVALA(IMATE, 'THM_DIFFU', 2, NOMPAR, VALPAR,
     +                         2, NCRA32(10), VAL32(10), CODRET, 'FM')
            PESA(1) = VAL32( 1)
            PESA(2) = VAL32( 2)
            PESA(3) = VAL32( 3)
            PERMFH  = VAL32( 4)
            LAMBDD  = VAL32( 6)
            DLAMBD  = VAL32( 7)
            SATUR   = VAL32( 8)
            DSATUR  = VAL32( 9)
            PERMLI  = VAL32(10)
            DPERML  = VAL32(11)
            UNSURK  = VAL33( 1)
            VISCL   = VAL33( 2)
            DVISCL  = VAL33( 3)
            ALPHA   = VAL33( 4)
            LAMBDL  = VAL33( 5)
            DLAMBL  = VAL33( 6)
            MAMOLG  = VAL34( 1)
            VISCG   = VAL34( 2)
            DVISCG  = VAL34( 3)
            LAMBDG  = VAL34( 4)
            DLAMBG  = VAL34( 5)
         ENDIF
         IF (HYDR.EQ.'HYDR') THEN
            CALL PERMEA(IMATE,HYDR,PHI,P1,P2,T,SATUR,DSATUR,NCON,COND)
            PERMFH = COND( 1)
            PERMLI = COND( 2)
            DPERML = COND( 3)
            PERMGZ = COND( 4)
            DPERMS = COND( 5)
            DPERMP = COND( 6)
            FICK   = COND( 7)
            DFICKT = COND( 8)
            DFICKG = COND( 9)
         ENDIF
      ENDIF
C =====================================================================
      END

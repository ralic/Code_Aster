       SUBROUTINE KITDEC(YAMEC, YATE, YAP1, YAP2, MECA, THMC, THER,
     +                   HYDR, IMATE, DEFGEM, DEFGEP, ADDEME, ADDEP1,
     +                   ADDEP2, ADDETE, NDIM, T0, P10, P20, PHI0, PVP0,
     +                   DEPSV, EPSV, DEPS, T, P1, P2, DT, DP1, DP2,
     +                   GRAT, GRAP1, GRAP2, NVITH, RETCOM,RINSTP)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C MODIF ALGORITH  DATE 23/03/2010   AUTEUR ANGELINI O.ANGELINI 
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
      IMPLICIT NONE
      INTEGER       YAMEC, YATE, YAP1, YAP2, IMATE, NVITH
      INTEGER       ADDEME, ADDEP1, ADDEP2, ADDETE, NDIM, RETCOM
      REAL*8        T0, P10, P20, PHI0, PVP0, DEPSV, EPSV, DEPS(6), T
      REAL*8        P1, P2, GRAT(3), GRAP1(3), GRAP2(3), DP1, DP2, DT
      REAL*8        DEFGEM(*), DEFGEP(*) ,RINSTP
      CHARACTER*16  MECA, THMC, THER, HYDR
C ======================================================================
C --- RECUPERATION DES DONNEES INITIALES -------------------------------
C ======================================================================
      INTEGER       IBID1, IBID2
      REAL*8        RBID1, RBID2, RBID3, RBID4, RBID5, RBID6, RBID7
      REAL*8        RBID8, RBID9, RBID10, RBID11, RBID12, RBID13, RBID14
      REAL*8        RBID15, RBID16, RBID17, RBID18, RBID19, RBID20
      REAL*8        RBID21, RBID22, RBID23, RBID24, RBID25, RBID26
      REAL*8        RBID27, RBID28, RBID29, RBID30, RBID31, RBID32
      REAL*8        RBID33, RBID34, RBID35, RBID36, RBID37, RBID38
      REAL*8        RBID39, RBID40, RBID41, RBID42, RBID43, RBID44
      REAL*8        RBID45,RBID46,RBID47,RBID48,RBID49,RBID50,RBID51
      REAL*8        RBID52 , RBID53,R3BID(6)
C ======================================================================
C --- INITIALISATION DE VARIABLES --------------------------------------
C ======================================================================
      CALL THMRCP( 'INITIALI', IMATE, THMC, MECA, HYDR, THER,
     +           T0, P10, P20, PHI0, PVP0, RBID1, RBID2,
     +           RBID3, RBID4, RBID5, RBID6,
     +           RBID8, RBID9, RBID10,  RBID11,RBID12,
     +           RBID13,RBID53,RBID14,
     +           RBID15, RBID16, RBID17,RBID18, RBID19, RBID20,
     +           RBID21, RBID22, RBID23,RBID24, RBID25, RBID26,
     +           RBID27, RBID28, RBID29,RBID30, RBID31, RBID32,
     +           RBID33, RBID34, RBID35,RBID36, RBID37, RBID38,
     +           RBID39, RBID40, RBID41,RBID42, RBID43, RBID44, RBID45,
     +           RBID46, RBID47,RBID48, RBID49, RBID50,RBID51,
     +           R3BID,  RBID52,RINSTP)     
C ======================================================================
C --- CALCUL DES VARIABLES ---------------------------------------------
C ======================================================================
      CALL CALCVA(  YAMEC, YATE, YAP1, YAP2, DEFGEM, DEFGEP,
     +              ADDEME, ADDEP1, ADDEP2, ADDETE, NDIM,
     +              T0, P10, P20, DEPSV, EPSV, DEPS, T, P1, P2,
     +              GRAT, GRAP1, GRAP2, DP1, DP2, DT, RETCOM )
C ======================================================================
      END

      SUBROUTINE LKFSXI(NMAT,MATERF,I1,DEVSIG,DSHDS,PLAS,XI,PARA,
     &                  VARA,DFDSDX,DPARDX)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/09/2012   AUTEUR FOUCAULT A.FOUCAULT 
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
C RESPONSABLE FOUCAULT A.FOUCAULT
      IMPLICIT   NONE
C     --------------------------------------------------------------
C     CALCUL DU TERME DE LETK = D(DF/DS)/DXI
C     IN  NMAT     : DIMENSION TABLE DES PARAMETRES MATERIAU
C         MATERF   : TABLE DES PARAMETRES MATERIAU 
C         I1       : TRACE DU TENSEUR DES CONTRAINTES
C         DEVISG   : DEVIATEUR DU TENSEUR DES CONTRAINTES
C         DSHDS    : DERIVVE DE SII*HTHETA PAR RAPPORT A SIGMA
C         PLAS     : BOOLEEN -> PLASTI. = TRUE - VISCOS. = FALSE
C         XI       : VARIABLE D'EXROUISSAGE XI(P OU VP)
C         PARA     : CONTIENT VALEURS DE A(XI),S(XI),M(XI)
C         VARA     : CONTIENT AG(XI),BG(XI),DG(XI) ET K(XI)
C     OUT DFDSFX   : D(DF/DS)/DXI
C         DPARDX   : VECTEUR DE LONGUEUR 3 CONTENANT :
C         DAMDX    : DERIVEE DE A(XI) PAR RAPPORT A XI
C         DSDX     : DERIVEE DE M(XI) PAR RAPPORT A XI
C         DMDX     : DERIVEE DE S(XI) PAR RAPPORT A XI
C     --------------------------------------------------------------
      INTEGER         NMAT
      REAL*8          I1,DEVSIG(6),DSHDS(6),DFDSDX(6),MATERF(NMAT,2)
      REAL*8          PARA(3),XI,VARA(4),DPARDX(3)
      LOGICAL         PLAS
C
      INTEGER         NDT,NDI,I
      REAL*8          DAMDX,SIGC,H0C,AGX,SII,HTHETA,BGX,DGX,AMX,UN
      REAL*8          VIDENT(6),ZERO,TEREXP,DAGDX,DBGDX,DDGDX
      REAL*8          KX,SIX,DKDX,DEUX,TROIS,APIC,A0,XAMS
      REAL*8          XIPIC,AVMAX,XIVMAX,MPIC,M0,MVMAX,XIE,AE,AULT
      REAL*8          ETA,XIULT,SIGP1,SPIC,SIGP2,ME,SX,S0,COS3T
      REAL*8          MULT,RCOS3T,H0E,LGLEPS,PREF,DSDX,DMDX,MX
      PARAMETER       (ZERO   = 0.D0)
      PARAMETER       (UN     = 1.D0)
      PARAMETER       (DEUX   = 2.D0)
      PARAMETER       (TROIS  = 3.D0)
      PARAMETER       (SIX    = 6.D0)
      PARAMETER       (LGLEPS = 1.0D-8 )
C     --------------------------------------------------------------
      COMMON /TDIM/   NDT,NDI
C     --------------------------------------------------------------
C -----------------------------------------
C --- RECUPERATION DES PARAMETRES MATERIAU
C -----------------------------------------
      PREF   = MATERF(1,2)

      A0     = MATERF(8,2)
      AVMAX  = UN
      M0     = MATERF(12,2)
      MVMAX  = MATERF(19,2)
      S0     = MATERF(11,2)
      SPIC   = UN
      XIVMAX = MATERF(20,2)

      SIGC   = MATERF(3,2)
      XAMS   = MATERF(6,2)
      ETA    = MATERF(7,2)
      AE     = MATERF(9,2)
      APIC   = MATERF(10,2)
      AULT   = UN
      ME     = MATERF(13,2)
      MPIC   = MATERF(14,2)
      MULT   = MATERF(15,2)
      XIULT  = MATERF(16,2)
      XIE    = MATERF(17,2)
      XIPIC  = MATERF(18,2)

      SIGP1  = MATERF(23,2)

      SIGP2  = ((MULT*(SIGC)**(AE-UN))/(ME**AE))**(UN/(AE-UN))

      AMX    = PARA(1)
      SX     = PARA(2)
      MX     = PARA(3)

      AGX    = VARA(1)
      BGX    = VARA(2)
      DGX    = VARA(3)
      KX     = VARA(4)

      RCOS3T = COS3T (DEVSIG, PREF, LGLEPS)
      CALL LKHTET(NMAT,MATERF,RCOS3T,H0E,H0C,HTHETA)      
C --------------------------------------
C --- CONSTRUCTION VARIABLES TEMPORAIRES
C --------------------------------------
C --- VECTEUR IDENTITE
      CALL LCINVE(ZERO,VIDENT)
      DO 10 I = 1, NDI
        VIDENT(I) = UN
  10  CONTINUE

C --- NORME DU DEVIATEUR DES CONTRAINTES
      CALL LCPRSC(DEVSIG,DEVSIG,SII)
      SII = SQRT(SII)
      
C --------------------------------------
C --- CALCUL DE DAMDX, DSDX ET DMDX
C --------------------------------------
      IF(PLAS)THEN
C --- SEUIL DE PLASTICITE
        IF((XI.GE.ZERO).AND.(XI.LE.XIPIC))THEN
C --- ENTRE SEUIL D'ENDOMMAGEMENT ET SEUIL DE PIC
          DAMDX = (APIC-A0)/LOG(UN+UN/XAMS)/(XI+XAMS*XIPIC)
          DMDX  = (MPIC-M0)/LOG(UN+UN/XAMS)/(XI+XAMS*XIPIC)
          DSDX  = (SPIC-S0)/LOG(UN+UN/XAMS)/(XI+XAMS*XIPIC)
        ELSEIF((XI.GT.XIPIC).AND.(XI.LE.XIE))THEN
C --- ENTRE SEUIL DE PIC ET SEUIL INTERMEDIAIRE
          DAMDX = (AE-APIC)/(XIE-XIPIC)
          DSDX  = -UN/(XIE-XIPIC)
          DMDX  = SIGC/SIGP1*((-APIC/AMX**2)*(MPIC*SIGP1/SIGC+SPIC)
     &            **(APIC/AMX)*LOG(MPIC*SIGP1/SIGC+SPIC)*DAMDX-DSDX)  
        ELSEIF((XI.GT.XIE).AND.(XI.LT.XIULT))THEN
C --- ENTRE SEUIL INTERMEDIAIRE ET SEUIL RESIDUEL
          DAMDX = (AULT-AE)/LOG(UN+UN/ETA)/(XI+ETA*XIULT-
     &            (UN+ETA)*XIE)
          DMDX  = SIGC/SIGP2*((-AE/AMX**2)*LOG(ME*SIGP2/SIGC)*
     &            (ME*SIGP2/SIGC)**(AE/AMX))*DAMDX
          DSDX  = ZERO
        ELSEIF(XI.GE.XIULT)THEN
C --- SUR SEUIL RESIDUEL
          DAMDX = ZERO
          DMDX  = ZERO
          DSDX  = ZERO
        ENDIF
      ELSE
C --- SEUIL DE VISCOSITE
        DAMDX = (AVMAX-A0)/XIVMAX
        DMDX  = (MVMAX-M0)/XIVMAX
        DSDX  = ZERO
      ENDIF
      
C --------------------------------------
C --- CALCUL DE DKDX
C --------------------------------------
      DKDX = -DAMDX*LOG(DEUX/TROIS)/(DEUX*AMX**2)*KX

C --------------------------------------
C --- CALCUL DE DAGDX
C --------------------------------------
      DAGDX = UN/(SQRT(SIX)*SIGC*H0C)*(-DMDX*KX-MX*DKDX)

C --------------------------------------
C --- CALCUL DE DBGDX
C --------------------------------------
      DBGDX = DMDX*KX/TROIS/SIGC+MX/TROIS/SIGC*DKDX

C --------------------------------------
C --- CALCUL DE DDGDX
C --------------------------------------
      DDGDX = DSDX*KX+SX*DKDX

C --------------------------------------
C --- ASSEMBLAGE DE DFDSDX
C --------------------------------------
      TEREXP = AGX*SII*HTHETA+BGX*I1+DGX
      IF(TEREXP.GT.ZERO)THEN
        DO 20 I = 1, NDT
          DFDSDX(I) = -DAMDX*SIGC*H0C*TEREXP**(AMX-UN)*
     &              (AGX*DSHDS(I)+BGX*VIDENT(I))-
     &              AMX*SIGC*H0C*((DAMDX*LOG(TEREXP)+(AMX-UN)/TEREXP
     &              *(DAGDX*SII*HTHETA+DBGDX*I1+DDGDX))*TEREXP**
     &              (AMX-UN))*(AGX*DSHDS(I)+BGX*VIDENT(I))-
     &              AMX*SIGC*H0C*TEREXP**(AMX-UN)*
     &              (DAGDX*DSHDS(I)+DBGDX*VIDENT(I))
  20    CONTINUE    
      ELSE
        CALL LCINVE(ZERO,DFDSDX) 
      ENDIF

      DPARDX(1) = DAMDX
      DPARDX(2) = DSDX
      DPARDX(3) = DMDX

      END

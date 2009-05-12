      SUBROUTINE XMOINI(NH8,NH20,NP6,NP15,NT4,NT10,NCPQ4,NCPQ8,NCPT3,
     &            NCPT6,NDPQ4,NDPQ8,NDPT3,NDPT6,NF4,NF8,NF3,NF6,NPF2,
     &            NPF3,NCH20,NCP15,NCT10,NCPCQ8,NCPCT6,NDPCQ8,NDPCT6,
     &            NDPCQ4,NDPCT3,NCPCQ4,NCPCT3,NCH8,NCP6,NCT4)
      IMPLICIT NONE

      INTEGER       NH8(4),NH20(4),NP6(4),NP15(4),NT4(4),NT10(4)
      INTEGER       NCPQ4(4),NCPQ8(4),NCPT3(4),NCPT6(4), NDPQ4(4)
      INTEGER       NDPQ8(4),NDPT3(4),NDPT6(4),NF4(4),NF8(4),NF3(4)
      INTEGER       NF6(4),NPF2(4),NPF3(4),NCH20(4),NCP15(4),NCT10(4)
      INTEGER       NCPCQ8(4),NCPCT6(4),NDPCQ8(4),NDPCT6(4)
      INTEGER       NDPCQ4(4),NDPCT3(4),NCPCQ4(4),NCPCT3(4)
      INTEGER       NCH8(4),NCP6(4),NCT4(4)
      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 12/05/2009   AUTEUR MAZET S.MAZET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE PELLET J.PELLET
C TOLE CRP_21
C
C      
C ----------------------------------------------------------------------
C
C ROUTINE XFEM APPELEE PAR MODI_MODELE_XFEM (OP0113)
C
C    BUT : INITIALISER LES COMPTEURS DES NOMBRES D'ELEMENTS
C
C ----------------------------------------------------------------------
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
      CHARACTER*32 JEXNOM,JEXNUM,JEXATR
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER    I
C
      CALL JEMARQ()

      DO 10 I=1,4
        NH8(I)=0
        NH20(I)=0
        NP6(I)=0
        NP15(I)=0
        NT4(I)=0
        NT10(I)=0
        NCPQ4(I)=0
        NCPQ8(I)=0
        NCPT3(I)=0
        NCPT6(I)=0
        NDPQ4(I)=0
        NDPQ8(I)=0
        NDPT3(I)=0
        NDPT6(I)=0
        NF4(I)=0
        NF8(I)=0
        NF3(I)=0
        NF6(I)=0
        NPF2(I)=0
        NPF3(I)=0
        NCH20(I)=0
        NCP15(I)=0
        NCT10(I)=0
        NCPCQ8(I)=0
        NCPCT6(I)=0
        NDPCQ8(I)=0
        NDPCT6(I)=0
        NDPCQ4(I)=0
        NDPCT3(I)=0
        NCPCQ4(I)=0
        NCPCT3(I)=0
        NCH8(I)=0
        NCP6(I)=0
        NCT4(I)=0
 10   CONTINUE

      CALL JEDEMA()
      END

      SUBROUTINE NMDCRG(DEPART,ITERAT,VRESI ,XA0   ,XA1   ,
     &                  XDET  )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 12/07/2011   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
      IMPLICIT NONE
      INTEGER  DEPART,ITERAT
      REAL*8   VRESI(*)
      REAL*8   XA0,XA1,XDET
C      
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (EVENEMENTS)
C
C CALCUL DE L'EXTRAPOLATION SUR LES RESIDUS
C
C ----------------------------------------------------------------------
C
C
C EXTRAPOLATION LINEAIRE (XA0 + ITER*XA1) / XDET
C
C ON DONNE UN POIDS DOUBLE AU 3 DERNIERS POINTS CELA REVIENT
C A AJOUTER DES POINTS => MEILLEURE EXTRAPOLATION
C
C IN  DEPART : DEBUT DE L'EXTRAPOLATION
C IN  ITERAT : FIN DE L'EXTRAPOLATION
C IN  VRESI  : LISTE DES VALEURS DU RESIDU ACTUEL [0,ITERAT]
C OUT XA0    : VALEUR DE L'EXTRAPOLATION
C OUT XA1    : VALEUR DE L'EXTRAPOLATION
C OUT XDET   : VALEUR DE L'EXTRAPOLATION
C
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
      REAL*8       ZERO,UN,DEUX
      PARAMETER   (ZERO=0.0D0, UN=1.0D0, DEUX=2.0D0)
C
      INTEGER      I
      REAL*8       SX,SY,SX2,SYX
      REAL*8       XN,XX
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      SX     = ZERO
      SY     = ZERO
      SX2    = ZERO
      SYX    = ZERO
      XN     = ZERO
      XA0    = ZERO
      XA1    = ZERO
      XDET   = ZERO           
C
C --- CALCUL DE L'EXTRAPOLATION
C
      DO 110 I = DEPART, ITERAT
        XX = LOG(VRESI(I+1))
        IF ( I .GT. (ITERAT - 3) ) THEN
          XN   = XN  + DEUX
          SX   = SX  + DEUX*XX
          SY   = SY  + DEUX*I
          SX2  = SX2 + DEUX*(XX**2)
          SYX  = SYX + DEUX*XX*I
        ELSE
          XN   = XN  + UN
          SX   = SX  + XX
          SY   = SY  + I
          SX2  = SX2 + XX**2
          SYX  = SYX + XX*I
        ENDIF
110   CONTINUE
      XDET   = -SX**2 + SX2*XN
      XA0    =  SX2*SY - SX*SYX
      XA1    = -(SX*SY) + SYX*XN
C
      CALL JEDEMA()
      END

      SUBROUTINE RVABSC(MAILLA,TND,NBN,TABSC,TCOOR)
      IMPLICIT NONE
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C
      INCLUDE 'jeveux.h'
      CHARACTER*8 MAILLA
      INTEGER     TND(*),NBN
      REAL*8      TABSC(*),TCOOR(*)
C
C***********************************************************************
C
C  OPERATION REALISEE
C  ------------------
C
C     CALCUL DES ABSCISSES CURVILIGNES LE LONG D' UNE LISTE DE NOEUDS
C
C  ARGUMENTS EN ENTREE
C  -------------------
C
C     MAILLA : NOM DU MAILLAGE
C     TND    : TABLE DES NUMEROS DE NOEUDS
C     NBN    : NOMBRE DE NOEUDS
C
C  ARGUMENTS EN SORTIE
C  -------------------
C
C     TABSC : LE TABLEAU DES ABSCISSES
C     TCOOR : LE TABLEAU DES COORDONNEES (ORDRE X,Y,Z)
C
C***********************************************************************
C
C  -----------------------------------------
C
C
C  ---------------------------------
C
C  VARIABLES LOCALES
C  -----------------
C
      INTEGER ACOORD,I
      REAL*8  XC,XP,YC,YP,L,ZZC,ZZP
C
C==================== CORPS DE LA ROUTINE =============================
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL JEMARQ()
      CALL JEVEUO(MAILLA//'.COORDO    .VALE','L',ACOORD)
C
      TABSC(1) = 0.0D0
C
      XP  = ZR(ACOORD + (TND(1)-1)*3 + 1-1)
      YP  = ZR(ACOORD + (TND(1)-1)*3 + 2-1)
      ZZP = ZR(ACOORD + (TND(1)-1)*3 + 3-1)
C
      TCOOR(1) = XP
      TCOOR(2) = YP
      TCOOR(3) = ZZP
C
      DO 10, I = 2, NBN, 1
C
         XC  = ZR(ACOORD + (TND(I)-1)*3 + 1-1)
         YC  = ZR(ACOORD + (TND(I)-1)*3 + 2-1)
         ZZC = ZR(ACOORD + (TND(I)-1)*3 + 3-1)
C
         L = SQRT((XC-XP)*(XC-XP)+(YC-YP)*(YC-YP)+(ZZC-ZZP)*(ZZC-ZZP))
C
         TABSC(I) = TABSC(I-1) + L
C
         TCOOR(3*(I-1) + 1) = XC
         TCOOR(3*(I-1) + 2) = YC
         TCOOR(3*(I-1) + 3) = ZZC
C
         XP  = XC
         YP  = YC
         ZZP = ZZC
C
10    CONTINUE
C
      CALL JEDEMA()
      END

      FUNCTION VERINR (NBVAL, TBINS1, TBINS2)
C
      IMPLICIT     NONE
      LOGICAL      VERINR
      INTEGER      NBVAL
      CHARACTER*19 TBINS1, TBINS2
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 29/03/2002   AUTEUR CIBHHBC R.FERNANDES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                                                                       
C                                                                       
C ======================================================================
C ======================================================================
C --- BUT : VERIFICATION QUE LES LISTES D'INSTANTS DES CHAMPS ----------
C ------- : MECANIQUES SONT IDENTIQUES ---------------------------------
C ======================================================================
C IN  : NBVAL  : DIMENSION DE LA LISTE D'INSTANT -----------------------
C --- : TBINS1 : TABLE 1 -----------------------------------------------
C --- : TBINS2 : TABLE 2 -----------------------------------------------
C OUT : VERINR : FALSE SI LES LISTES SONT IDENTIQUES -------------------
C ------------ : TRUE  SI LES LISTES SONT DIFFERENTES ------------------
C ======================================================================
C --------- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C --------- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C ======================================================================
      INTEGER       II, JTBINI, JTBIN1, JTBIN2
      REAL*8        SOMME
      CHARACTER*19  VECINS
C ======================================================================
      VECINS = '&&VERINR.VECINS'
      CALL WKVECT ( VECINS, 'V V R' , NBVAL , JTBINI )
      CALL JEVEUO (TBINS1,'L',JTBIN1)
      CALL JEVEUO (TBINS2,'L',JTBIN2)
C ======================================================================
C --- CALCUL DE LA NORME -----------------------------------------------
C ======================================================================
      SOMME  = 0.0D0
      VERINR = .FALSE.
      DO 10 II = 1, NBVAL
         ZR(JTBINI-1+II) = ZR(JTBIN1-1+II) - ZR(JTBIN2-1+II)
         SOMME = SOMME + ZR(JTBINI-1+II)
 10   CONTINUE
      IF (SOMME.GT.0.0D0) VERINR = .TRUE.
C ======================================================================
C --- DESTRUCTION DE VECTEURS INUTILES ---------------------------------
C ======================================================================
      CALL JEDETR (VECINS)
C ======================================================================
      END

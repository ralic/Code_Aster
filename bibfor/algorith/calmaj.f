      SUBROUTINE CALMAJ(OPTION,MAX,MAY,MAZ,MODEL,VESTO,MODMEC,CHAMNO,
     &                 NUM,VRAI,I,J,MIJ)
C---------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 27/11/95   AUTEUR BIBERON G.ROUSSEAU 
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
C---------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
C
CROUTINE STOCKANT LE VECTEUR PRESSION ISSUE D UNE RESOLUTION DE LAPLACE
C IN : VECSOL : VECTEUR SOLUTION K*
C OUT : VESTO : VECTEUR STOCKE K*
C---------------------------------------------------------------------
C--------- DEBUT DES COMMUNS JEVEUX ----------------------------------
      CHARACTER*32     JEXNUM, JEXNOM, JEXR8, JEXATR
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16           ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     --- FIN DES COMMUNS JEVEUX ------------------------------------
      LOGICAL       VRAI
      INTEGER       I,J
      CHARACTER*(*) MODMEC,CHAMNO,MODEL
      CHARACTER*8   K8BID
      CHARACTER*9   OPTION
      CHARACTER*14  NUM
      CHARACTER*19  MODX,MODY,MODZ,VEPRJ,VESTO,MAY,MAX,MAZ
      REAL*8        MIJ      
C ---------------------------------------------------------------

C--------- CALCUL DE LA MASSE AJOUTEE POUR UN FLUIDE------------- 
C-------------------N AYANT PAS FORCEMENT------------------------
C-----------------LA MEME DENSITE PARTOUT------------------------

C-----------PLONGEMENT DE LA PRESSION ET DES CHAMPS DE DEPL_R----
C---------------SUR LE MODELE THERMIQUE D INTERFACE--------------

       CALL PLOINT(VESTO,MODMEC,CHAMNO,NUM,I,J,VRAI,MODEL,VEPRJ,MODX,
     &             MODY,MODZ)

C-------------------CALCUL DE LA MASSE AJOUTEE-------------------
C---------------SUR LE MODELE THERMIQUE D INTERFACE--------------

       CALL CALCIN(OPTION,MAX,MAY,MAZ,MODEL,VEPRJ,MODX,
     &             MODY,MODZ,I,J,MIJ)

      
      END

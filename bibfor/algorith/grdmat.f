      SUBROUTINE GRDMAT (JCOQU, JMATE, PGL, DH, ROT)
C
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 30/06/98   AUTEUR JMBHH01 J.M.PROIX 
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
C ----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
C ----------------------------------------------------------------------
C     CALCUL DE LA MATRICE DE RAIDEUR D'UN MATERIAU ORTHOTROPE
C            EXPRIMEE DANS LE REPERE LOCAL DE L'ELEMENT.
C
C  IN  JCOQU : ADRESSE DES CARACTERISTIQUES GEOMETRIQUES DE L'ELEMENT
C  IN  JMATE : ADRESSE DU MATERIAU CODE
C  IN  PGL   : MATRICE DE PASSAGE REPERE GLOBAL -> LOCAL ELEMENT
C  OUT DH    : MATRICE DE RAIDEUR DU MATERIAU ORTHOTROPE EXPRIMEE
C                      DANS LE REPERE LOCAL DE L'ELEMENT
C  OUT ROT   : MATRICE DE PASSAGE REPERE D'ORTHOTROPIE -> LOCAL ELEMENT
C ----------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      REAL*8        VALRES(1)
      CHARACTER*2   CODRET(1)
      CHARACTER*10  PHEN
      REAL*8        ALPHA , BETA , R8DGRD , S , C
      REAL*8        ANGLL , PCL , PCT , YOUNG1, YOUNG2
      REAL*8        PGL(3,3), DH(3,3), ROT(3,3), XAB1(3,3)
C     ------------------------------------------------------------------
      ALPHA = ZR(JCOQU+1) * R8DGRD()
      BETA  = ZR(JCOQU+2) * R8DGRD()
      ANGLL = ZR(JCOQU+3) * R8DGRD()
C     --- POURCENTAGE D'ARMATURES 1 / UNITE DE LARGEUR
      PCL   = ZR(JCOQU+4)
C     --- POURCENTAGE D'ARMATURES 2 / UNITE DE LARGEUR
      PCT   = ZR(JCOQU+5)
C
      CALL RCCOMA(ZI(JMATE),'ELAS',PHEN,CODRET)
      CALL RCVALA(ZI(JMATE),PHEN,0,' ',0.D0,1,'E',VALRES,CODRET,'FM')
      YOUNG1= VALRES(1)
      YOUNG2= YOUNG1*PCT/PCL
      CALL R8INIR(9,0.D0,DH,1)
C
      DH(1,1) = YOUNG1
      DH(2,2) = YOUNG2
      DH(3,3) = 1.D-7
      CALL GRIROT ( ALPHA , BETA , ANGLL ,PGL , ROT  , C, S)
      CALL UTBTAB ('ZERO', 3 , 3 , DH , ROT , XAB1 , DH )
C
      END

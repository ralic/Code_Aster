      SUBROUTINE TE0314(OPTION,NOMTE)
      IMPLICIT     NONE
      CHARACTER*16 OPTION,NOMTE
      
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 15/06/2010   AUTEUR GRANET S.GRANET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C TOLE CRP_20
C     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
C          CORRESPONDANT A UN DEBIT HYDRAULIQUE SUR UN ELEMENT DE BORD
C          D'UN JOINT HM
C          OPTION : 'CHAR_MECA_FLUX_R'
C          
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ======================================================================
C NNO      NB DE NOEUDS DE L'ELEMENT DE BORD QUADRATIQUE
C NNO2     NB DE NOEUDS DE L'ELEMENT DE BORD LINEAIRE
C NNOS     NB DE NOEUDS EXTREMITE
C NDLNO    NB DE DDL DES NOEUDS EXTREMITE
C NDLNM    NB DE DDL DES NOEUDS MILIEUX
C NPG      NB DE POINTS DE GAUSS DE L'ELEMENT DE BORD
C ======================================================================
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER        ZI
      COMMON /IVARJE/ZI(1)
      REAL*8         ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16     ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL        ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8    ZK8
      CHARACTER*16          ZK16
      CHARACTER*24                  ZK24
      CHARACTER*32                          ZK32
      CHARACTER*80                                  ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C ======================================================================
      LOGICAL     AXI,LTEATT
      INTEGER     IRES,IFLUX,ITEMPS,IGEOM
      REAL*8      FLU1,DELTAT,R
      
      AXI = .FALSE.
          
      IF ( LTEATT(' ','AXIS','OUI') ) THEN
        AXI  = .TRUE.
      END IF
C ======================================================================
C --- RECUPERATION DES CHAMPS IN ET DES CHAMPS OUT ---------------------
C ======================================================================
      CALL JEVECH('PVECTUR','E',IRES)

C ======================================================================
C --- CAS DES FLUX -----------------------------------------------------
C ======================================================================
      IF (OPTION.EQ.'CHAR_MECA_FLUX_R') THEN
        CALL JEVECH('PFLUXR','L',IFLUX)
        CALL JEVECH('PTEMPSR','L',ITEMPS)
        CALL JEVECH('PGEOMER','L',IGEOM)
        DELTAT = ZR(ITEMPS+1)
      END IF
 
C ======================================================================
C --- OPTION CHAR_MECA_FLUX_R ----------------------
C ======================================================================

C ======================================================================
C --- SI MODELISATION = HM ---------------------------------------------
C ======================================================================
          IF (NOMTE(1:2).EQ.'HM') THEN
             FLU1 = ZR(IFLUX)
             IF (AXI) THEN
               R = ZR(IGEOM)
               ZR(IRES+6) = ZR(IRES+6) - DELTAT*FLU1*R 
             ELSE
               ZR(IRES+6) = ZR(IRES+6) - DELTAT*FLU1  
             END IF           
          END IF
      END

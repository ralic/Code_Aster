      SUBROUTINE MMSTAC(INDCOI,LVITES,JEU   ,JEUVIT,LAMBDC,
     &                  COEFAC,INDCON)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/02/2012   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      INTEGER  INDCOI,INDCON
      LOGICAL  LVITES
      REAL*8   JEU   ,JEUVIT,LAMBDC,COEFAC
C
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - CONTRAINTES ACTIVES)
C
C STATUT DE CONTACT
C
C ----------------------------------------------------------------------
C
C
C IN  INDCOI : INDICATEUR DE CONTACT INITIAL
C              - INDCOI = 0: PAS DE CONTACT
C              - INDCOI = 1: CONTACT
C IN  LVITES : .TRUE. SI FORMULATION EN VITESSE
C IN  JEU    : VALEUR DU JEU
C IN  JEUVIT : VALEUR DU GAP DES VITESSES NORMALES
C IN  LAMBDC : MULTIPLICATEUR DE CONTACT DU POINT DE CONTACT
C IN  COEFAC : COEFFICIENT D'AUGMENTATION DU CONTACT RHO_N
C OUT INDCON : INDICATEUR DE CONTACT FINAL
C              - INDCON = 0: PAS DE CONTACT
C              - INDCON = 1: CONTACT
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
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
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      REAL*8  R8PREM
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      INDCON  = INDCOI
C
C --- VERIFICATION DU SIGNE DU MULTIPLICATEUR AUGMENTE
C
      IF ((LAMBDC-COEFAC*JEU).LE.R8PREM()) THEN
        INDCON = 1
      ELSE
        INDCON = 0
      ENDIF
C
C --- FORMULATION EN VITESSE
C
      IF (LVITES) THEN
        IF ((INDCOI.EQ.0).AND.(JEUVIT.LE.0.D0)) THEN
          INDCON = 0
        END IF
      ENDIF
C
      CALL JEDEMA()
      END

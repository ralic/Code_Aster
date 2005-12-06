      SUBROUTINE RCCOME(NOMMAT,PHENO,PHENOM,CODRET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 17/05/96   AUTEUR D6BHHAM A.M.DONORE 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     NOMMAT,PHENO,PHENOM
      CHARACTER*2                           CODRET
C ----------------------------------------------------------------------
C     OBTENTION DU COMPORTEMENT COMPLET D'UN MATERIAU DONNE A PARTIR
C     D'UN PREMISSE
C
C     ARGUMENTS D'ENTREE:
C        NOMMAT : NOM DU MATERIAU
C        PHENO  : NOM DU PHENOMENE INCOMPLET
C     ARGUMENTS DE SORTIE:
C        PHENOM: NOM DU PHENOMENE COMPLET
C     CODRET : POUR CHAQUE RESULTAT, 'OK' SI ON A TROUVE, 'NO' SINON
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
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
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C ----------------------------------------------------------------------
C DEB ------------------------------------------------------------------
      CHARACTER*8     K8BID
      CHARACTER*32    NCOMP
      CODRET = 'OK'
      NCOMP = NOMMAT//'.MATERIAU.NOMRC         '
      CALL JELIRA(NCOMP,'LONUTI',NBCOMP,K8BID)
      CALL JEVEUO(NCOMP,'L',ICOMP)
      DO 10 I=1,NBCOMP
        IF ( PHENO .EQ. ZK16(ICOMP+I-1)(1:LEN(PHENO)) ) THEN
          PHENOM=ZK16(ICOMP+I-1)
          GOTO 999
        ENDIF
 10   CONTINUE
      CODRET = 'NO'
      CALL UTMESS ('A','RCCOME_01','COMPORTEMENT NON TROUVE')
      GOTO 999
C
  999 CONTINUE
C FIN ------------------------------------------------------------------
      END

      FUNCTION NBNOSO(NOMTEZ)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*) NOMTEZ
      INTEGER NBNOSO
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 10/05/96   AUTEUR CIBHHGB G.BERTRAND 
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
C-----------------------------------------------------------------------
C     BUT       : NOMBRE DE NOEUDS SOMMETS ASSOCIE A UN TYPE 
C                 D'ELEMENT DONNE
C IN  K* NOMTEZ : NOM TYPE DE L'ELEMENT
C-----------------------------------------------------------------------
C
      CHARACTER*8  ALIAS
      CHARACTER*16 NOMTE
      CHARACTER*24 CHCTE
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
      COMMON  / IVARJE / ZI(1)
      COMMON  / RVARJE / ZR(1)
      COMMON  / CVARJE / ZC(1)
      COMMON  / LVARJE / ZL(1)
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
      INTEGER            ZI
      REAL*8             ZR
      COMPLEX*16         ZC
      LOGICAL            ZL
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      NOMTE  = NOMTEZ
C
C ---- CAS DU 3D :
C      ---------
      IF  (NOMTE(6:10).EQ.'TETRA'.OR.NOMTE(6:10).EQ.'PYRAM'
     + .OR.NOMTE(6:10).EQ.'PENTA'.OR.NOMTE(6:9) .EQ.'HEXA') THEN
         ALIAS  = NOMTE(6:12)
         CHCTE  = '&INEL.'//ALIAS//'.CARACTE'
         CALL JEVETE(CHCTE,'L',JIN)
         NBFPG  = ZI(JIN+3-1)
         NBNOSO = ZI(JIN+3-1+NBFPG+1)
C
C ---- CAS DU 2D :
C      ---------
      ELSEIF  (NOMTE(5:6).EQ.'TR') THEN
         NBNOSO  = 3
      ELSEIF  (NOMTE(5:6).EQ.'QU') THEN
         NBNOSO  = 4
      ELSE
         CALL UTMESS('F','NBNOSO','LE TYPE D''ELEMENT : '//NOMTE//
     +               'N''EST PAS TRAITE.')
      ENDIF
C
      END

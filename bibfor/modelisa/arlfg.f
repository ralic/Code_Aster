      FUNCTION ARLFG(NBOIT1,NBOIT2)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 08/11/2004   AUTEUR DURAND C.DURAND 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C      METHODE ARLEQUIN : ESTIMATION DU MAILLAGE LE PLUS FIN
C ----------------------------------------------------------------------
C VARIABLE D'ENTREE
C CHARACTER*16  NBOIT1  : NOM SD BOITES DU MAILLAGE 1 (CF BOITE)
C CHARACTER*16  NBOIT2  : NOM SD BOITES DU MAILLAGE 2 (CF BOITE)
C
C RESULTAT FONCTION
C INTEGER       ARLFG   : 1 SI MAILLAGE 1 PLUS FIN QUE MAILLAGE 2
C                         2 DANS LE CAS CONTRAIRE
C ----------------------------------------------------------------------

      IMPLICIT NONE

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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
C --- VARIABLES
      CHARACTER*16 NBOIT1,NBOIT2
      INTEGER      ARLFG,P0,P1,N1,N2,I,IFM,NIV
      REAL*8       H1,H2

      CALL INFMAJ()
      CALL INFNIV(IFM,NIV)

C --- LECTURE DONNEES

      CALL JELIRA(NBOIT1//'.H','LONMAX',N1,ZK8)
      CALL JELIRA(NBOIT2//'.H','LONMAX',N2,ZK8)
      CALL JEVEUO(NBOIT1//'.H','L',P0)
      CALL JEVEUO(NBOIT2//'.H','L',P1)

C --- CALCUL DU VOLUME (SURFACE) MOYEN(NE) DES DEUX MAILLAGES

      H1 = 0.D0
      DO 10 I = 1, N1
        H1 = H1 + ZR(P0)
        P0 = P0 + 1
 10   CONTINUE
      H1 = H1/N1

      H2 = 0.D0
      DO 20 I = 1, N2
        H2 = H2 + ZR(P1)
        P1 = P1 + 1
 20   CONTINUE
      H2 = H2/N2
     
C --- COMPARAISON

      WRITE(IFM,*) 'DETECTION DES FINESSES RELATIVES :'
      WRITE(IFM,*) ' '

      IF (H1.LT.H2) THEN
        ARLFG = 1
        WRITE(IFM,*) '   MODELE 1 : FIN'
        WRITE(IFM,*) '   MODELE 2 : GROSSIER'
      ELSE
        ARLFG = 2
        WRITE(IFM,*) '   MODELE 1 : GROSSIER'
        WRITE(IFM,*) '   MODELE 2 : FIN'
      ENDIF

      WRITE(IFM,*) ' '

      END

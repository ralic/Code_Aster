      SUBROUTINE PRSTOC(VECSOL,VESTOC,J,K,IAD,NBVALE,NBREFE,NBDESC)
      IMPLICIT REAL*8  (A-H,O-Z)

C--------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 31/05/96   AUTEUR BIBERON G.ROUSSEAU 
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
C--------------------------------------------------------------------
C
C         ROUTINE STOCKANT LE VECTEUR PRESSION
C         ISSUE D' UNE RESOLUTION DE LAPLACE
C IN : VECSOL : VECTEUR SOLUTION K19
C IN : J : INDICE DE BOUCLE
C IN : IAD : ADRESSE DU VECTEUR DES NOMS DES CHAMNOS STOCKES
C IN : NBVALE,NBREFE,NBDESC : DIMENSIONS DE VECTEURS POUR UN CHAMNO
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
C      ---- FIN DES COMMUNS JEVEUX ----------------------------------
      LOGICAL       VRAI
      INTEGER       IVALE,IDESC,IREFE,IVALP,IDESP,IREFP,J,K
      INTEGER       NBREFE,NBVALE,NBDESC,IAD,NBVEC
      CHARACTER*1   K1BID
      CHARACTER*19  VECSOL,VESTOC
      CHARACTER*24  CHAINE

C -------------------------------------------------------------------
C----------------CREATION DU VECTEUR PRESSION -----------------------
C
C -----------CREATION DU TABLEAU DE VECTEURS CONTENANT---------------
C--------------------------LA PRESSION-------------------------------
C
      CALL JEMARQ()
           CHAINE = 'CBIDON'

           CALL CODENT(J,'D0',CHAINE(1:5))
           ZK24(IAD+K-1) = VESTOC(1:14)//CHAINE(1:5)

           CALL WKVECT(ZK24(IAD+K-1)(1:19)//'.VALE','V V R',NBVALE,
     +             IVALP)
           CALL WKVECT(ZK24(IAD+K-1)(1:19)//'.REFE','V V K24',NBREFE,
     +                IREFP)
           CALL WKVECT(ZK24(IAD+K-1)(1:19)//'.DESC','V V I',
     +                 NBDESC,IDESP)

           CALL JEVEUO(VECSOL//'.VALE','L',IVALE)
           CALL JELIRA(VECSOL//'.VALE','LONMAX',NBVEC,K1BID)
           CALL JEVEUO(VECSOL//'.DESC','L',IDESC)
           CALL JEVEUO(VECSOL//'.REFE','L',IREFE)

C-------------STOCKAGE DANS LE VECTEUR CREE -------------------------

            CALL R8COPY(NBVEC,ZR(IVALE),1,ZR(IVALP),1)

            DO 13 KB = 1,NBDESC
              ZI(IDESP+KB-1) = ZI(IDESC+KB-1)
13          CONTINUE

            DO 14 KB = 1,NBREFE
              ZK24(IREFP+KB-1) = ZK24(IREFE+KB-1)
14          CONTINUE


           CALL JEDETC('V',VECSOL,1)
C
 9999      CONTINUE
      CALL JEDEMA()
           END

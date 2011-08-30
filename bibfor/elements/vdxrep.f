      SUBROUTINE VDXREP(NOMTE,XI)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 30/08/2011   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE PELLET J.PELLET
C ======================================================================
C
C BUT : REMPLIR L'OBJET .DESR DANS LES ZONES 1090 ET 2000
C       POUR POUVOIR CALCULER LES MATRICES DE PASSAGE AVEC VDREPE
C      (COQUE_3D)
C
C ARGUMENTS :
C   NOMTE  IN : NOM TYPE_ELEMENT
C   XI     IN : GEOMETRIE DES NOEUDS DE L'ELEMENT
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX --------------------
      CHARACTER*16 NOMTE
      INTEGER NB1,NB2,NPGSR,I,J,K,IND,INTSR,JCARA,LZI,LZR
      REAL*8 XI(3,9)
      REAL*8 VECTA(9,2,3),VECTN(9,3),VECTG(2,3),VECTT(3,3)
      REAL*8 EPAIS,ZERO,VECTPT(9,2,3)
C
      ZERO = 0.0D0

      CALL JEVETE('&INEL.'//NOMTE(1:8)//'.DESI',' ', LZI )
      CALL JEVETE('&INEL.'//NOMTE(1:8)//'.DESR',' ', LZR )
      NB1  =ZI(LZI-1+1)
      NB2  =ZI(LZI-1+2)
      NPGSR=ZI(LZI-1+3)

      CALL JEVECH ('PCACOQU' , 'L' , JCARA)
      EPAIS = ZR(JCARA)

C     -- POUR REMPLIR LZR+1090+...  ET CALCULER VECTN :
      CALL VECTAN(NB1,NB2,XI,ZR(LZR),VECTA,VECTN,VECTPT)


C     -- POUR REMPLIR LZR+2000+... :
C     -- QUELLE VALEUR POUR IND ? FICHE ???
      IND =0
      K = 0
      DO 110 INTSR=1,NPGSR
        CALL VECTGT(IND,NB1,XI,ZERO,INTSR,ZR(LZR),EPAIS,
     &              VECTN,VECTG,VECTT)
        DO 120 J = 1, 3
          DO 130 I = 1, 3
            K = K + 1
            ZR(LZR+2000+K-1) = VECTT(I,J)
 130      CONTINUE
 120    CONTINUE
 110  CONTINUE
      END

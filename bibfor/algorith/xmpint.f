      SUBROUTINE XMPINT(NDIM  ,NFAES ,JPCPI ,JPCCF ,GEOPI )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/12/2009   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
      IMPLICIT NONE
      INTEGER       JPCPI,JPCCF
      INTEGER       NDIM,NFAES
      REAL*8        GEOPI(9)
C      
C ----------------------------------------------------------------------
C
C ROUTINE XFEM (METHODE XFEM-GG - TE)
C
C CALCUL DES COORDONNEES REELLE POUR LES POINTS
C D'INTERSECTION CONSTITUENT LA MAILLE DE CONTACT
C DANS L'ELEMENT DE CONTACT HYBRIDE X-FEM
C      
C ----------------------------------------------------------------------
C
C ----------------------------------------------------------------------
C ROUTINE SPECIFIQUE A L'APPROCHE <<GRANDS GLISSEMENTS AVEC XFEM>>,
C TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
C ----------------------------------------------------------------------
C
C
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  NFAES  : NUMERO DE LA FACETTE ESCLAVE
C IN  JPCPI  : COORDONNÉES DES POINTS D'INTERSECTION DANS L'ELEM DE REF
C IN  JPCCF  : NUM LOCAUX DES NOEUDS DES FACETTES DE CONTACT
C OUT GEOPI  : COORDONNÉES REELES DES POINTS D'INTERSECTION
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
      INTEGER       I,J
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ() 
C       
      DO 30 I=1,NDIM
C --- BOUCLE SUR LES POINTS D'INTERSECTION DE LA FACETTE
        DO 40 J=1,NDIM
          GEOPI(NDIM*(I-1)+J) = 
     &     ZR(JPCPI-1+NDIM*(INT(ZR(JPCCF-1+NDIM*(NFAES-1)+I))-1)+J)
 40     CONTINUE
 30   CONTINUE
C
      CALL JEDEMA()
      END

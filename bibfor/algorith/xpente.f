      SUBROUTINE XPENTE(PL,CNSE,N1,N2,N3,N4,N5,N6)
      IMPLICIT NONE 

      INTEGER       PL,N1,N2,N3,N4,N5,N6,CNSE(6,4)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/07/2004   AUTEUR GENIAUT S.GENIAUT 
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
C                      DÉCOUPER LE SOUS-PENTA EN SOUS-TETRAS 
C                    
C     ENTREE
C       PL                : PLACE DU 1ER SOUS-TETRA DANS CNSE
C       N1,N2,N3,N4,N5,N6 : NUMEROS DES NOEUDS DU PENTA
C
C     SORTIE
C       CNSE      : CONNECTIVITE NOMBRE DE SOUS-ÉLÉMENTS (TÉTRAS)
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C 
C ----------------------------------------------------------------------

      CALL JEMARQ()

C     ON REMPLIT 3 SOUS TETRAS DE CNSE À PARTIR DE LA PLACE PL
      CNSE(PL,1)=N1
      CNSE(PL,2)=N5
      CNSE(PL,3)=N2
      CNSE(PL,4)=N6

      CNSE(PL+1,1)=N4
      CNSE(PL+1,2)=N5
      CNSE(PL+1,3)=N1
      CNSE(PL+1,4)=N6

      CNSE(PL+2,1)=N1
      CNSE(PL+2,2)=N2
      CNSE(PL+2,3)=N3
      CNSE(PL+2,4)=N6
 
      CALL JEDEMA()
      END

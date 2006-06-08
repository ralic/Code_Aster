      SUBROUTINE PROJNO(NEWGEO,DEFICO,NORM,COORDP,POSNOM,
     &                  NBNO,JEU)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/12/2004   AUTEUR VABHHTS J.PELLET 
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
C
      IMPLICIT     NONE
      CHARACTER*24 NEWGEO
      CHARACTER*24 DEFICO
      REAL*8       NORM(3)
      REAL*8       COORDP(3)
      INTEGER      POSNOM
      INTEGER      NBNO
      REAL*8       JEU

C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : PROJEC
C ----------------------------------------------------------------------
C
C CETTE ROUTINE REALISE L'APPARIEMENT NODAL
C
C IN  NEWGEO : COORDONNEES REACTUALISEES DES NOEUDS DU MAILLAGE
C IN  DEFICO : SD DE CONTACT
C IN  NORM   : VECTEUR NORMAL
C IN  COORDP : COORDONNEES DU NOEUD ESCLAVE
C IN  POSNOM : INDICE DANS CONTNO DU NOEUD MAITRE
C OUT NBNO   : NOMBRE DE NOEUDS MAITRES CONCERNES
C OUT JEU    : VALEUR DU JEU APRES CORRECTION
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
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
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER      NO,K
      CHARACTER*24 CONTNO
      INTEGER      JNOCO,JCOOR
      REAL*8       COORDM(3),PM(3),DDOT
C ----------------------------------------------------------------------
      CALL JEMARQ ()
C
      CONTNO = DEFICO(1:16)//'.NOEUCO'
C
      CALL JEVEUO (CONTNO,'L',JNOCO)
      CALL JEVEUO (NEWGEO(1:19)//'.VALE','L',JCOOR)
C
      NBNO      = 1
      NO        = ZI(JNOCO+POSNOM-1)
      COORDM(1) = ZR(JCOOR+3*(NO-1))
      COORDM(2) = ZR(JCOOR+3*(NO-1)+1)
      COORDM(3) = ZR(JCOOR+3*(NO-1)+2)
      DO 10 K = 1,3
        PM(K) = COORDM(K)-COORDP(K)
10    CONTINUE
      JEU = DDOT(3,NORM,1,PM,1)
C
      CALL JEDEMA()
C ----------------------------------------------------------------------
      END

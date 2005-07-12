      SUBROUTINE TE0510(OPTION,NOMTE)
      IMPLICIT   NONE
      CHARACTER*16 OPTION,NOMTE

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 11/07/2005   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C.......................................................................
C
C       CALCUL DES DONNÉES TOPOLOGIQUES CONCERNANT LES INTERSECTIONS
C              DES ÉLÉMENTS ENRICHIS ET DU PLAN DE LA FISSURE
C
C
C  OPTION : 'TOPOFA' (X-FEM TOPOLOGIE DES FACETTES DE CONTACT)
C
C  ENTREES  ---> OPTION : OPTION DE CALCUL
C           ---> NOMTE  : NOM DU TYPE ELEMENT
C
C......................................................................
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX --------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR ,DDOT
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)

C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX --------------------

      CHARACTER*8   ELP,TYPMA
      CHARACTER*24  PINTER,AINTER
      INTEGER       IGEOM,JLSN,JGRLSN,JGRLST
      INTEGER       JOUT1,JOUT2,JOUT3,JOUT4,JOUT5
      INTEGER       JPTINT,JAINT,IADZI,IAZK24
      INTEGER       NINTER,NFACE,CFACE(5,3),AR(12,2),NBAR,IN,IA,NA,NB
      INTEGER       I,J,NLI,IMAX,NABS
      REAL*8        LONGAR,AL,ND(3),GRLT(3),TAU1(3),TAU2(3),NORME,PS
      REAL*8        POINT(3),NORM2,LSN,MAX
C......................................................................

      CALL JEMARQ()
C
      CALL ELREF1(ELP)
C
C     RECUPERATION DES ENTRÉES / SORTIE
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PLEVSET','L',JLSN)
      CALL JEVECH('PGRADLN','L',JGRLSN)
      CALL JEVECH('PGRADLT','L',JGRLST)

      CALL JEVECH('PPINTER','E',JOUT1)
      CALL JEVECH('PAINTER','E',JOUT2)
      CALL JEVECH('PCFACE' ,'E',JOUT3)
      CALL JEVECH('PLONCHA','E',JOUT4)
      CALL JEVECH('PBASECO','E',JOUT5)      

      CALL TECAEL(IADZI,IAZK24)
      TYPMA=ZK24(IAZK24-1+3+ZI(IADZI-1+2)+3)
      CALL CONARE(TYPMA,AR,NBAR)
      
C ----------------------------------------------------------------------
C     RECHERCHE DES INTERSECTIONS ARETES-FISSURE 
C     ET DÉCOUPAGE EN FACETTES

      PINTER='&&TE0510.PTINTER'
      AINTER='&&TE0510.ATINTER'

      CALL XCFACE(ELP,ZR(JLSN),JGRLSN,IGEOM,
     &                                PINTER,NINTER,AINTER,NFACE,CFACE)
      CALL JEVEUO(PINTER,'L',JPTINT)
      CALL JEVEUO(AINTER,'L',JAINT)

C     ARCHIVAGE DE PINTER ET AINTER
      DO 110 I=1,NINTER
        DO 111 J=1,3
          ZR(JOUT1-1+3*(I-1)+J)=ZR(JPTINT-1+3*(I-1)+J)
 111    CONTINUE
        DO 112 J=1,4
          ZR(JOUT2-1+4*(I-1)+J)=ZR(JAINT-1+4*(I-1)+J)
 112    CONTINUE
 110  CONTINUE

C     ARCHIVAGE DE CFACE
      DO 120 I=1,NFACE
        DO 121 J=1,3
          ZI(JOUT3-1+3*(I-1)+J)=CFACE(I,J)
 121    CONTINUE
 120  CONTINUE

C     ARCHIVAGE DE LONCHAM
      ZI(JOUT4-1+1)=NINTER
      ZI(JOUT4-1+2)=NFACE
      
C ----------------------------------------------------------------------
C     CALCUL DE LA BASE COVARIANTE AUX POINTS D'INTERSECTION
C     ND EST LA NORMALE À LA SURFACE : GRAD(LSN)
C     TAU1 EST LE PROJETÉ DE GRAD(LST) SUR LA SURFACE
C     TAU2 EST LE PRODUIT VECTORIEL : ND ^ TAU1
C                   (BOOK IV 01/02/05)
      DO 130 NLI=1,NINTER

        IA=NINT(ZR(JAINT-1+4*(NLI-1)+1))
        IN=NINT(ZR(JAINT-1+4*(NLI-1)+2))

        IF (IN.NE.0) THEN
          DO 131 J=1,3
            ND(J)  =ZR(JGRLSN-1+3*(IN-1)+J)
            GRLT(J)=ZR(JGRLST-1+3*(IN-1)+J)
 131      CONTINUE   
        ELSE
          CALL ASSERT(IA.NE.0)
          NA=AR(IA,1)
          NB=AR(IA,2)
          LONGAR=ZR(JAINT-1+4*(NLI-1)+3)
          AL=ZR(JAINT-1+4*(NLI-1)+4)
          DO 132 J=1,3
            ND(J)  = (1-AL/LONGAR) * ZR(JGRLSN-1+3*(NA-1)+J)
     &             +    AL/LONGAR  * ZR(JGRLSN-1+3*(NB-1)+J)
            GRLT(J)= (1-AL/LONGAR) * ZR(JGRLST-1+3*(NA-1)+J)
     &             +    AL/LONGAR  * ZR(JGRLST-1+3*(NB-1)+J)
 132      CONTINUE             
        ENDIF  

        CALL NORMEV(ND,NORME)
        PS=DDOT(3,GRLT,1,ND,1)
        DO 133 J=1,3
          TAU1(J)=GRLT(J)-PS*ND(J)
 133    CONTINUE
        CALL NORMEV(TAU1,NORME)
        
        IF (NORME.LT.1.D-12) THEN
          CALL UTMESS('A','TE0510','LE VECTEUR '//
     &    'TAU1 (DIRECTION1 DU FROTTEMENT) EST NUL. LES GRADIENTS '//
     &    'DES LEVEL SETS SONT SUREMENT COLINEAIRES EN CE POINT.')
          POINT(1)=ZR(JPTINT-1+3*(NLI-1)+1)
          POINT(2)=ZR(JPTINT-1+3*(NLI-1)+2)
          POINT(3)=ZR(JPTINT-1+3*(NLI-1)+3)
          CALL UTIMPR('L',' POINT ',3,POINT )
C         ESSAI AVEC LE PROJETE DE OX
          TAU1(1)=1.D0-ND(1)*ND(1)
          TAU1(2)=0.D0-ND(1)*ND(2)
          TAU1(3)=0.D0-ND(1)*ND(3)
          CALL NORMEV(TAU1,NORM2)
          IF (NORM2.LT.1.D-12) THEN
C           ESSAI AVEC LE PROJETE DE OY
            TAU1(1)=0.D0-ND(2)*ND(1)
            TAU1(2)=1.D0-ND(2)*ND(2)
            TAU1(3)=0.D0-ND(2)*ND(3)               
            CALL NORMEV(TAU1,NORM2)
          ENDIF
          CALL ASSERT(NORM2.GT.1.D-12)
        ENDIF
        CALL PROVEC(ND,TAU1,TAU2) 

C       ARCHIVAGE DE BASECO
        DO 134 J=1,3
          ZR(JOUT5-1+9*(NLI-1)+J)  =ND(J)
          ZR(JOUT5-1+9*(NLI-1)+J+3)=TAU1(J)
          ZR(JOUT5-1+9*(NLI-1)+J+6)=TAU2(J)
 134    CONTINUE

 130  CONTINUE

      CALL JEDETR(PINTER)
      CALL JEDETR(AINTER)
C ----------------------------------------------------------------------

      CALL JEDEMA()
      END

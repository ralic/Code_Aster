      SUBROUTINE TE0572(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 11/03/2013   AUTEUR CUVILLIE M.CUVILLIEZ 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE CUVILLIEZ
C
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*16 OPTION,NOMTE
C
C-----------------------------------------------------------------------
C
C     BUT: CALCUL DES MATRICES DE MASSE ELEMENTAIRES EN THERMIQUE
C          LINEAIRE, ELEMENTS X-FEM LINEAIRES
C
C          OPTION : 'MASS_THER'
C
C     ENTREES  ---> OPTION : OPTION DE CALCUL
C              ---> NOMTE  : NOM DU TYPE ELEMENT
C
C-----------------------------------------------------------------------
C
      INTEGER NDIM,NFH,NFE,IBID,IGEOM,NNOP,JPINTT,IMATE,ITPS,JSTNO
      INTEGER IMATTT,JCNSET,JHEAVT,JLONCH,JBASLO,JLSN,JLST,NDDLNO
      REAL*8  R8BID
      LOGICAL ISELLI
      CHARACTER*8 ELREFP
C
C ----------------------------------------------------------------------
C --- PREALABLES AU CALCUL DE LA MASSE ELEMENTAIRE
C ----------------------------------------------------------------------
C
C     ON INTERDIT LES ELTS QUADRATIQUES
      CALL ELREF1(ELREFP)
      CALL ASSERT(ISELLI(ELREFP))
C
C     CHAMPS IN 'CLASSIQUES'
      CALL JEVECH('PGEOMER','L',IGEOM )
      CALL JEVECH('PMATERC','L',IMATE )
      CALL JEVECH('PTEMPSR','L',ITPS  )
C     CHAMPS IN X-FEM
      CALL JEVECH('PSTANO' ,'L',JSTNO )
      CALL JEVECH('PPINTTO','L',JPINTT)
      CALL JEVECH('PCNSETO','L',JCNSET)
      CALL JEVECH('PHEAVTO','L',JHEAVT)
      CALL JEVECH('PLONCHA','L',JLONCH)
      CALL JEVECH('PBASLOR','L',JBASLO)
      CALL JEVECH('PLSN'   ,'L',JLSN  )
      CALL JEVECH('PLST'   ,'L',JLST  )
C     CHAMP OUT
      CALL JEVECH('PMATTTR','E',IMATTT)
C
C     ELT DE REF PARENT : RECUP NDIM ET NNOP (NOEUDS PARENT)
C     -> RQ : 'RIGI' POUR LA FAMILLE DE PG EST DONC SANS CONSQUENCE
      CALL ELREF4(' ','RIGI',NDIM,NNOP,IBID,IBID,IBID,IBID,IBID,IBID)
C
C     NBRE DE DDLS PAR NOEUD
      CALL XTHINI(NOMTE,NFH,NFE)
      NDDLNO = 1+NFH+NFE
C
C ----------------------------------------------------------------------
C --- CALCUL DE LA MATRICE DE MASSE ELEMENTAIRE
C ----------------------------------------------------------------------
C
      CALL XMASTH(NDIM,ELREFP,NNOP,IMATE,ITPS,IGEOM,ZI(JLONCH),
     &            ZI(JCNSET),JPINTT,ZR(JLSN),ZR(JLST),ZR(JBASLO),
     &            ZI(JHEAVT),NFH,NFE,ZR(IMATTT))
C
C ----------------------------------------------------------------------
C --- SUPPRESSION DES DDLS SUPERFLUS
C ----------------------------------------------------------------------
C
C     SUPPRESSION DES DDLS SUPERFLUS
      CALL XTHDDL(NFH,NDDLNO,NNOP,ZI(JSTNO),OPTION,NOMTE,ZR(IMATTT),
     &            R8BID)
C
      END

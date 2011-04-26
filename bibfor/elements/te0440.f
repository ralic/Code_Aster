      SUBROUTINE TE0440(OPTION,NOMTE)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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

      IMPLICIT NONE
      CHARACTER*16 OPTION,NOMTE
C......................................................................
C
C    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
C                          OPTIONS  CHAR_MECA_FR3D3D ET CHAR_MECA_FF3D3D
C                                   CHAR_MECA_FR2D2D ET CHAR_MECA_FF2D2D
C                          POUR LES �L�MENTS X-FEM
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................


C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

      INTEGER J,NDIM,NNO,NNOP,NNOPS,NNOS,NNOM,NDDL,NPG,SINGU
      INTEGER NFH,DDLS,NFE,DDLC,NSE,ISE,IN,INO,IBID,DDLM
      INTEGER JPINTT,JCNSET,JHEAVT,JLONCH,JLSN,JLST,JCOORS,JSTNO
      INTEGER IVECTU,IFORC,ITEMPS,IGEOM,JPMILT,IRESE
      INTEGER NFISS,JFISNO
      REAL*8  HE,RBID
      CHARACTER*8   ELREFP,ELRESE(6),FAMI(6),ENR,LAG
      CHARACTER*24  COORSE
      LOGICAL FONC,LBID,ISMALI

      DATA    ELRESE /'SE2','TR3','TE4','SE3','TR6','TE4'/
      DATA    FAMI   /'BID','RIGI','XINT','BID','RIGI','XINT'/

C-----------------------------------------------------------------------

      CALL JEMARQ()

C     ELEMENT DE REFERENCE PARENT
      CALL ELREF1(ELREFP)
      CALL ELREF4(' ','RIGI',NDIM,NNOP,NNOPS,IBID,IBID,IBID,IBID,IBID)

C     SOUS-ELEMENT DE REFERENCE : RECUP DE NNO, NPG
      IF (.NOT.ISMALI(ELREFP).AND. NDIM.LE.2) THEN
        IRESE=3
      ELSE
        IRESE=0
      ENDIF
      CALL ELREF4(ELRESE(NDIM+IRESE),FAMI(NDIM+IRESE),IBID,NNO,NNOS,NPG,
     &                                           IBID,IBID,IBID,IBID)

C     INITIALISATION DES DIMENSIONS DES DDLS X-FEM
      CALL XTEINI(NOMTE,NFH,NFE,SINGU,DDLC,NNOM,DDLS,NDDL,
     &            DDLM,NFISS,IBID)

C     PARAMETRE DU VECTEUR ELEMENTAIRE
C-------------------------------------
      CALL JEVECH('PVECTUR','E',IVECTU)

C     PARAM�TRES PROPRES � X-FEM
C-------------------------------
      CALL JEVECH('PPINTTO','L',JPINTT)
      CALL JEVECH('PCNSETO','L',JCNSET)
      CALL JEVECH('PHEAVTO','L',JHEAVT)
      CALL JEVECH('PLONCHA','L',JLONCH)
      CALL JEVECH('PLSN',   'L',JLSN)
      CALL JEVECH('PLST',   'L',JLST)
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PSTANO' ,'L',JSTNO)
C     PROPRE AUX ELEMENTS 1D ET 2D (QUADRATIQUES)
      CALL TEATTR (NOMTE,'S','XFEM',ENR,IBID)
      IF (IBID.EQ.0 .AND.(ENR.EQ.'XH'.OR.ENR.EQ.'XHC').AND. NDIM.LE.2)
     &  CALL JEVECH('PPMILTO','L',JPMILT)
      IF (NFISS.GT.1) CALL JEVECH('PFISNO','L',JFISNO)

C     PARAM�TRES DES FORCES VOLUMIQUES
C-------------------------------------

      IF ((OPTION.EQ.'CHAR_MECA_FR3D3D').OR.
     &    (OPTION.EQ.'CHAR_MECA_FR2D2D')) THEN

        FONC=.FALSE.
        IF (NDIM.EQ.3) CALL TEFREP(OPTION,NOMTE,'PFR3D3D',IFORC)
        IF (NDIM.EQ.2) CALL TEFREP(OPTION,NOMTE,'PFR2D2D',IFORC)

      ELSEIF ((OPTION.EQ.'CHAR_MECA_FF3D3D').OR.
     &        (OPTION.EQ.'CHAR_MECA_FF2D2D')) THEN

        FONC=.TRUE.
        CALL JEVECH('PTEMPSR','L',ITEMPS)
        IF (NDIM.EQ.3) CALL JEVECH('PFF3D3D','L',IFORC)
        IF (NDIM.EQ.2) CALL JEVECH('PFF2D2D','L',IFORC)

      ENDIF

C     R�CUP�RATION DE LA SUBDIVISION DE L'�L�MENT EN NSE SOUS ELEMENT
      NSE=ZI(JLONCH-1+1)

C       BOUCLE SUR LES NSE SOUS-ELEMENTS
      DO 110 ISE=1,NSE

C       COORD DU SOUS-�LT EN QUESTION
        COORSE='&&TE0440.COORSE'
        CALL WKVECT(COORSE,'V V R',NDIM*NNO,JCOORS)

C       BOUCLE SUR LES SOMMETS DU SOUS-TRIA (DU SOUS-SEG)
        DO 111 IN=1,NNO
          INO=ZI(JCNSET-1+NNO*(ISE-1)+IN)
          DO 112 J=1,NDIM
            IF (INO.LT.1000) THEN
              ZR(JCOORS-1+NDIM*(IN-1)+J)=ZR(IGEOM-1+NDIM*(INO-1)+J)
            ELSEIF (INO.GT.1000 .AND. INO.LT.2000) THEN
              ZR(JCOORS-1+NDIM*(IN-1)+J)=
     &                             ZR(JPINTT-1+NDIM*(INO-1000-1)+J)
            ELSEIF (INO.GT.2000 .AND. INO.LT.3000) THEN
              ZR(JCOORS-1+NDIM*(IN-1)+J)=
     &                             ZR(JPMILT-1+NDIM*(INO-2000-1)+J)
            ELSEIF (INO.GT.3000) THEN
              ZR(JCOORS-1+NDIM*(IN-1)+J)=
     &                             ZR(JPMILT-1+NDIM*(INO-3000-1)+J)
            ENDIF
 112      CONTINUE
 111    CONTINUE

C       FONCTION HEAVYSIDE CSTE SUR LE SS-�LT
        HE = ZI(JHEAVT-1+ISE)

        CALL XFOVOL(ELREFP,NDIM,COORSE,IGEOM,HE,NFH*NDIM,DDLC,NFE,
     &              NNOP,JLSN,JLST,IFORC,ITEMPS,IVECTU,FONC,.TRUE.)

        CALL JEDETR(COORSE)

 110  CONTINUE

C     SUPPRESSION DES DDLS SUPERFLUS
      CALL TEATTR (NOMTE,'C','XLAG',LAG,IBID)
      IF (IBID.EQ.0.AND.LAG.EQ.'ARETE') THEN
        NNOP = NNOS
      ENDIF
      CALL XTEDDL(NDIM,NFH,NFE,DDLS,NDDL,NNOP,NNOPS,ZI(JSTNO),
     &            .FALSE.,LBID,OPTION,NOMTE,RBID,ZR(IVECTU),DDLM,
     &            NFISS,JFISNO)

      CALL JEDEMA()
      END

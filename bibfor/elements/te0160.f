      SUBROUTINE TE0160 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 04/04/2002   AUTEUR VABHHTS J.PELLET 
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
C INTRODUCTION DE LA TEMPERATURE
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C    - ELEMENT:  MECABL2
C      OPTION : 'FULL_MECA'   'RAPH_MECA'   'RIGI_MECA_TANG'
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      CHARACTER*24       CARAC,FF
      CHARACTER*8        NOMRES(3),ELREFE
      CHARACTER*2        BL2, CODRET(3)
      REAL*8             A,BILINE,COEF,COEF1,COEF2,ZERO,DEMI,DEUX
      REAL*8             VALRES(3),E,ALPHA
      REAL*8             GREEN,JACOBI,NX,YTYWPQ(9),W(9),TEMPN(3),TEMPG
      INTEGER            NNO,KP,I,J,IMATUU
      INTEGER            ICARAC,IFF,IPOIDS,IVF,IGEOM,IMATE,JCRET
C
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      CALL ELREF1(ELREFE)
      ZERO = 0.D0
      DEMI = 0.5D0
C
C
      CARAC='&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO  = ZI(ICARAC)
      NPG  = ZI(ICARAC+2)
C
      FF   ='&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF,'L',IFF)
      IPOIDS=IFF
      IVF   =IPOIDS+NPG
      IYTY  =IVF   +2*NPG*NNO
      NORDRE = 3*NNO
C PARAMETRES EN ENTREE
      CALL JEVECH('PCOMPOR','L',ICOMPO)
      IF ( ZK16(ICOMPO+3)(1:9).EQ. 'COMP_INCR' ) THEN
         CALL UTMESS('F','TE0160','COMP_INCR NON VALIDE')
      ENDIF
      IF (ZK16(ICOMPO)(1:5).NE.'CABLE') THEN
        CALL UTMESS('F','TE0160_1',' RELATION : '//ZK16(ICOMPO)//
     &              ' NON IMPLANTEE SUR LES CABLES')
      ENDIF
      IF (ZK16(ICOMPO+1)(1:8).NE.'GREEN   ') THEN
        CALL UTMESS('F','TE0160_2',' DEFORMATION : '//ZK16(ICOMPO+1)//
     &              ' NON IMPLANTEE SUR LES CABLES')
      ENDIF
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      BL2 = '  '
      NOMRES(1) = 'E'
      NOMRES(2) = 'EC_SUR_E'
      NOMRES(3) = 'ALPHA'
      CALL RCVALA ( ZI(IMATE),'CABLE',0,'  ',R8BID,2,NOMRES,
     +              VALRES,CODRET , 'FM')
      CALL RCVALA ( ZI(IMATE),'CABLE',0,'  ',R8BID,1,NOMRES(3),
     +              VALRES(3),CODRET(3) , BL2)
      IF ( CODRET(3).NE.'OK' ) VALRES(3) = ZERO
      E     = VALRES(1)
      EC    = E * VALRES(2)
      ALPHA = VALRES(3)
      CALL JEVECH('PCACABL','L',LSECT)
      A = ZR(LSECT)
      PRETEN = ZR(LSECT+1)
      CALL JEVECH('PTEMPPR','L',ITEMPR)
      CALL JEVECH('PDEPLMR','L',IDEPLA)
      CALL JEVECH('PDEPLPR','L',IDEPLP)
C PARAMETRES EN SORTIE
      IF(OPTION(1:9) .EQ. 'FULL_MECA' .OR.
     &   OPTION(1:14) .EQ. 'RIGI_MECA_TANG'     ) THEN
        CALL JEVECH('PMATUUR','E',IMATUU)
      ENDIF
      IF(OPTION(1:9) .EQ. 'FULL_MECA' .OR.
     &   OPTION(1:9) .EQ. 'RAPH_MECA'     ) THEN
        CALL JEVECH('PVECTUR','E',JEFINT)
        CALL JEVECH('PCONTPR','E',LSIGMA)
      ENDIF
C
      DO 4 I=1,NNO
        TEMPN(I) = ZR(ITEMPR-1+I)
    4 CONTINUE
      DO 5 I=1,3*NNO
        W(I)=ZR(IDEPLA-1+I)+ZR(IDEPLP-1+I)
    5 CONTINUE
C
      DO 21 KP=1,NPG
        TEMPG = ZERO
        DO 10 NE=1,NNO
          TEMPG = TEMPG + ZR(IVF-1+(KP-1)*NNO+NE) * TEMPN(NE)
   10   CONTINUE
        K = (KP-1) * NORDRE * NORDRE
        JACOBI = SQRT( BILINE(NORDRE,ZR(IGEOM),ZR(IYTY+K),ZR(IGEOM)) )
C
        GREEN = (      BILINE(NORDRE,W,ZR(IYTY+K),ZR(IGEOM)) +
     &          DEMI * BILINE(NORDRE,W,ZR(IYTY+K),W))
     &       / JACOBI**2
C
        NX = E * A * GREEN
        IF (ABS(NX).LT.1.D-6) THEN
          NX = PRETEN
        ELSE
          NX = NX - E*A*ALPHA*TEMPG
        ENDIF
C
C*** LE CABLE A UN MODULE BEAUCOUP PLUS FAIBLE A LA COMPRESSION QU'A LA
C*** TRACTION. LE MODULE DE COMPRESSION PEUT MEME ETRE NUL.
C
      IF (NX.LT.0.D0) THEN
        NX    = NX * EC / E
        E     = EC
      ENDIF
C
        COEF1 = E * A * ZR(IPOIDS-1+KP) / JACOBI**3
        COEF2 = NX * ZR(IPOIDS-1+KP) / JACOBI
        CALL MATVEC (NORDRE,ZR(IYTY+K),2,ZR(IGEOM),W,YTYWPQ)
        IF(OPTION(1:9)  .EQ. 'FULL_MECA' .OR.
     &     OPTION(1:14) .EQ. 'RIGI_MECA_TANG'   ) THEN
          NELYTY = IYTY - 1 - NORDRE + K
          IMAT = IMATUU - 1
          DO 12 I=1,NORDRE
            NELYTY = NELYTY + NORDRE
C
            DO 11 J=1,I
              IMAT = IMAT +1
              ZR(IMAT) = ZR(IMAT) + COEF1*YTYWPQ(I)*YTYWPQ(J)
     &                            + COEF2*ZR(NELYTY+J)
   11       CONTINUE
   12     CONTINUE
        ENDIF
        IF(OPTION(1:9) .EQ. 'FULL_MECA' .OR.
     &     OPTION(1:9) .EQ. 'RAPH_MECA'   ) THEN
          COEF = NX * ZR(IPOIDS-1+KP) / JACOBI
          DO 16 I=1,NORDRE
            ZR(JEFINT-1+I) = ZR(JEFINT-1+I) + COEF*YTYWPQ(I)
   16     CONTINUE
          ZR(LSIGMA-1+KP) = NX
        ENDIF
   21 CONTINUE
C
      IF ( OPTION(1:9).EQ.'FULL_MECA'  .OR.
     +     OPTION(1:9).EQ.'RAPH_MECA'  ) THEN
         CALL JEVECH ( 'PCODRET', 'E', JCRET )
         ZI(JCRET) = 0
      ENDIF
C
      END

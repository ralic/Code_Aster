      SUBROUTINE TE0321 ( OPTION , NOMTE )
      IMPLICIT   NONE
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 11/04/2002   AUTEUR CIBHHLV L.VIVAN 
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
C
C    - FONCTION REALISEE:  INITIALISATION DU CALCUL DE Z EN 3D
C                          OPTION : 'META_INIT','META_INIT_ELNO'
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*24    CHCTE,CHVAL,NOMRES
      CHARACTER*16     COMPOR(3)
      CHARACTER*8     ELREFE
      CHARACTER*2     CODRET
      REAL*8          TPG0,ZERO,MS0,ZALPHA,ZBETA
      REAL*8          METAPG(189)
      INTEGER         NNO,KP,I,J,K,ITEMPE,NCMP,NNOS,NPG1
      INTEGER         IPHASI,IPHASO,IPHASN,NBPG(10)
      INTEGER         IPOIDS,IVF,IMATE,NBFPG,NDIM,JIN,JVAL,ICOMPO
C
C
      CALL ELREF1(ELREFE)
C
      CHCTE = '&INEL.'//ELREFE//'.CARACTE'
      CALL JEVETE(CHCTE,'L',JIN)
      NDIM = ZI(JIN+1-1)
      NNO = ZI(JIN+2-1)
      NBFPG = ZI(JIN+3-1)
      DO 100 I = 1,NBFPG
        NBPG(I) = ZI(JIN+3-1+I)
  100 CONTINUE
      NNOS  = ZI(JIN+3-1+NBFPG+1)
      NPG1 = NBPG(1)
C
      CHVAL = '&INEL.'//ELREFE//'.FFORMES'
      CALL JEVETE(CHVAL,'L',JVAL)
C
      IPOIDS = JVAL + (NDIM+1)*NNO*NNO
      IVF    = IPOIDS + NPG1
C
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PTEMPER','L',ITEMPE)
      CALL JEVECH('PPHASIN','L',IPHASI)
      CALL JEVECH('PCOMPOR','L',ICOMPO)
      COMPOR(1)=ZK16(ICOMPO)
C
      CALL JEVECH('PPHASOU','E',IPHASO)
      IF (OPTION .EQ. 'META_INIT_ELNO') THEN
        CALL JEVECH('PPHASNOU','E',IPHASN)
      ENDIF
C
      ZERO=0.D0


C     MATERIAU FERRITIQUE
      IF (COMPOR(1) .EQ. 'ACIER' ) THEN
C

         NOMRES = 'MS0'
         CALL RCVALA(ZI(IMATE),'META_ACIER',1,'INST',0.D0,1,NOMRES,
     &            MS0,CODRET,'FM' )
         DO 300 KP=1,NPG1
           K=(KP-1)*NNO
           TPG0 = ZERO

           DO 102 J=1,NNO
             TPG0 = TPG0 + ZR(ITEMPE+J-1)*ZR(IVF+K+J-1)
 102       CONTINUE
           DO 200 J=0,5
             METAPG(1+7*(KP-1)+J)=ZR(IPHASI+5*(KP-1)+J)
200        CONTINUE
           METAPG(1+7*(KP-1)+6)=MS0
           METAPG(1+7*(KP-1)+5)=TPG0

           DO 84 J=1,7
             ZR(IPHASO+7*(KP-1)+J-1)   = METAPG(1+7*(KP-1)+J-1)
 84        CONTINUE
300      CONTINUE

      ELSEIF (COMPOR(1)(1:4) .EQ. 'ZIRC' ) THEN
          DO 305 KP=1,NPG1
           K=(KP-1)*NNO
           TPG0 = ZERO
           DO 105 J=1,NNO
             TPG0 = TPG0 + ZR(ITEMPE+J-1)*ZR(IVF+K+J-1)
 105       CONTINUE
C ----------PROPORTION TOTALE DE LA PHASE ALPHA

           METAPG(1+3*(KP-1))=ZR(IPHASI+5*(KP-1))
           METAPG(1+3*(KP-1)+1)=ZR(IPHASI+5*(KP-1)+1)
           ZALPHA = METAPG(1+3*(KP-1)+1)+METAPG(1+3*(KP-1))

C-----------DECOMPOSITION DE LA PHASE ALPHA POUR LA MECANIQUE

           ZBETA=1-ZALPHA
           IF (ZBETA .GT. 0.1D0) THEN
             METAPG(1+3*(KP-1)) =0.D0
           ELSE
             METAPG(1+3*(KP-1))=10*(ZALPHA-0.9D0)*ZALPHA
           ENDIF
           METAPG(1+3*(KP-1)+1)=ZALPHA-METAPG(1+3*(KP-1))
C

           METAPG(1+3*(KP-1)+2)=TPG0

           DO 85 J=1,3
             ZR(IPHASO+3*(KP-1)+J-1)   = METAPG(1+3*(KP-1)+J-1)

 85        CONTINUE


305      CONTINUE
      ENDIF
C
      IF ( OPTION .EQ. 'META_INIT_ELNO' ) THEN
        IF ( NOMTE(6:13).EQ.'TETRA10 '.OR.NOMTE(6:13).EQ.'HEXA20  '
     &       .OR. NOMTE(6:13).EQ.'PENTA15 '  ) THEN
          NPG1 = NBPG(3)
        ENDIF
C
        IF (COMPOR(1)(1:4) .EQ. 'ZIRC') THEN
           NCMP = 3
        ELSE
           NCMP = 7
        ENDIF

        CALL PPGANO ( NNOS,NPG1,NCMP,ZR(IPHASO),ZR(IPHASN))
C
      ENDIF
9999  CONTINUE
      END

      SUBROUTINE TE0067 ( OPTION , NOMTE )
      IMPLICIT   NONE
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 30/03/2004   AUTEUR CIBHHLV L.VIVAN 
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
C    - FONCTION REALISEE:  CALCUL DE Z EN 2D ET AXI
C                          CHGT DE PHASE METALURGIQUE
C                          OPTION : 'META_ELGA_TEMP  ' 'META_ELNO_TEMP'
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*16       COMPOR(3)
      CHARACTER*2        CODE
      REAL*8             TPG1,TPG0,TPG2,DT10,DT21,ZERO
      REAL*8             METAPG(63),METAZI(27)
      INTEGER            KP,J,K,IADTRC,NBCB1,NBCB2,NBLEXP,JGANO
      INTEGER            NDIM,NNO,NNOS,IPOIDS,IVF,NPG,IADEXP,ICOMPO
      INTEGER            IMATE,ITEMPE,ITEMPA,ITEMPS,ITEMPI,IPHASI,IPHASO
      INTEGER            MATOS,NBHIST,NBTRC,IADCKM,IDFDE
      INTEGER            IPFTRC,JFTRC,JTRC,IPHASN,NCMP
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
C
      CALL JEVECH ( 'PMATERC', 'L', IMATE  )
      CALL JEVECH ( 'PCOMPOR', 'L', ICOMPO )
      CALL JEVECH ( 'PTEMPAR', 'L', ITEMPA )
      CALL JEVECH ( 'PTEMPER', 'L', ITEMPE )
      CALL JEVECH ( 'PTEMPIR', 'L', ITEMPI )
      CALL JEVECH ( 'PTEMPSR', 'L', ITEMPS )
      CALL JEVECH ( 'PPHASIN', 'L', IPHASI )

      CALL JEVECH ( 'PPHASOU', 'E', IPHASO )
      IF ( OPTION .EQ. 'META_ELNO_TEMP' ) THEN
         CALL JEVECH ( 'PPHASNOU', 'E', IPHASN )
      ENDIF
C
      COMPOR(1)=ZK16(ICOMPO)
      MATOS = ZI(IMATE)
      ZERO = 0.D0

      IF (COMPOR(1) .EQ. 'ACIER') THEN
         CALL JEVECH ( 'PFTRC'  , 'L', IPFTRC )
         JFTRC = ZI(IPFTRC)
         JTRC  = ZI(IPFTRC+1)

         CALL RCADMA ( MATOS, 'META_ACIER', 'TRC', IADTRC, CODE, 'FM' )
C
         NBCB1  = NINT( ZR(IADTRC+1) )
         NBHIST = NINT( ZR(IADTRC+2) )
         NBCB2  = NINT( ZR(IADTRC+1+2+NBCB1*NBHIST) )
         NBLEXP = NINT( ZR(IADTRC+1+2+NBCB1*NBHIST+1) )
         NBTRC  = NINT( ZR(IADTRC+1+2+NBCB1*NBHIST+2+NBCB2*NBLEXP+1) )
         IADEXP = 5 + NBCB1*NBHIST
         IADCKM = 7 + NBCB1*NBHIST + NBCB2*NBLEXP
C
         DO 300 KP = 1 , NPG
            K = (KP-1)*NNO
            TPG1 = ZERO
            TPG0 = ZERO
            TPG2 = ZERO
            DO 100 J=1,NNO

               TPG1 = TPG1 + ZR(ITEMPE+J-1)*ZR(IVF+K+J-1)
               TPG0 = TPG0 + ZR(ITEMPA+J-1)*ZR(IVF+K+J-1)
               TPG2 = TPG2 + ZR(ITEMPI+J-1)*ZR(IVF+K+J-1)
 100        CONTINUE

            DT10 = ZR(ITEMPS+1)
            DT21 = ZR(ITEMPS+2)

            CALL ZACIER(MATOS,NBHIST, ZR(JFTRC), ZR(JTRC),
     &              ZR(IADTRC+3), ZR(IADTRC+IADEXP),
     &              ZR(IADTRC+IADCKM), NBTRC,TPG0,TPG1,TPG2,
     &           DT10,DT21,ZR(IPHASI+7*(KP-1)),METAPG(1+7*(KP-1)) )


         DO 85 J=1,7
            ZR(IPHASO+7*(KP-1)+J-1)   = METAPG(1+7*(KP-1)+J-1)

 85      CONTINUE
300      CONTINUE

      ELSEIF (COMPOR(1)(1:4).EQ. 'ZIRC') THEN
         DO 301 KP = 1 , NPG
            K = (KP-1)*NNO
            TPG1 = ZERO
            TPG2 = ZERO
            DO 101 J=1,NNO

               TPG1 = TPG1 + ZR(ITEMPE+J-1)*ZR(IVF+K+J-1)
               TPG2 = TPG2 + ZR(ITEMPI+J-1)*ZR(IVF+K+J-1)
 101        CONTINUE

            DT21 = ZR(ITEMPS+2)

            CALL ZEDGAR(MATOS,TPG1,TPG2,DT21,ZR(IPHASI+3*(KP-1)),
     &                  METAZI(1+3*(KP-1)))

         DO 86 J=1,3
            ZR(IPHASO+3*(KP-1)+J-1)   = METAZI(1+3*(KP-1)+J-1)
 86      CONTINUE

 301     CONTINUE

      ENDIF
C
      IF ( OPTION .EQ. 'META_ELNO_TEMP' ) THEN

        IF (COMPOR(1)(1:4) .EQ. 'ZIRC') THEN
           NCMP=3
        ELSE
           NCMP = 7
        ENDIF
C
C ----- RECUPERATION DE LA MATRICE DE PASSAGE PTS DE GAUSS - NOEUDS
C
        CALL PPGAN2 (JGANO,NCMP,ZR(IPHASO),ZR(IPHASN))
C
      ENDIF
C
      CALL JEDEMA()
      END

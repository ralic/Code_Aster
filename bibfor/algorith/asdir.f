      SUBROUTINE ASDIR (MONOAP,ID,NEQ,NBSUP,NSUPP,
     +                  TCOSUP,ZRCREP,ZRDIR )
      IMPLICIT  REAL*8 (A-H,O-Z)
      INTEGER           NSUPP(*),TCOSUP(NBSUP,*)
      REAL*8            ZRCREP(NBSUP,NEQ,*),ZRDIR(NEQ,*)
      LOGICAL           MONOAP
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/10/2000   AUTEUR PIBAT01 J.PIGAT 
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
C     ------------------------------------------------------------------
C     COMMANDE : COMB_SISM_MODAL
C        CALCUL DES REPONSES DIRECTIONNELLES
C     ------------------------------------------------------------------
C IN  : MONOAP : =.TRUE.  , CAS DU MONO-SUPPORT
C                =.FALSE. , CAS DU MULTI-SUPPORT
C IN  : ID     : LA DIRECTION
C IN  : NEQ    : NOMBRE D'EQUATIONS
C IN  : NBSUP  : NOMBRE DE SUPPORTS
C IN  : NSUPP  : MAX DU NOMBRE DE SUPPORT PAR DIRECTION
C IN  : TCOSUP : VECTEUR DES TYPES DE RECOMBINAISON DES SUPPORTS
C IN  : ZRCREP : VECTEUR DES RECOMBINAISONS MODALES PAR APPUIS
C OUT : ZRDIR  : VECTEUR DES RECOMBINAISONS PAR DIRECTIONS
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CALL JEMARQ()
      ZERO = 0.D0
C
      IF ( MONOAP )  THEN
         DO 10 IN = 1,NEQ
            ZRDIR(IN,ID)=ZRCREP(NBSUP,IN,ID)
 10      CONTINUE
      ELSE
         CALL WKVECT('&&ASDIR.QUAD','V V R',NEQ,JQUA)
         CALL WKVECT('&&ASDIR.LINE','V V R',NEQ,JLIN)
         CALL WKVECT('&&ASDIR.ABS ','V V R',NEQ,JABS)
         DO 11 IN = 1,NEQ
             ZR(JQUA+IN-1) = ZERO
             ZR(JLIN+IN-1) = ZERO
             ZR(JABS+IN-1) = ZERO
 11      CONTINUE
         DO 20 IS = 1,NSUPP(ID)
            IF (TCOSUP(IS,ID).EQ.1) THEN
C              --- COMBINAISON QUADRATIQUE ---
               DO 12 IN = 1,NEQ
                  XXX          = ZRCREP(IS,IN,ID)
                  ZR(JQUA+IN-1)= ZR(JQUA+IN-1)+ XXX
 12            CONTINUE
            ELSEIF (TCOSUP(IS,ID).EQ.2) THEN
C              --- COMBINAISON LINEAIRE ---
               DO 14 IN = 1,NEQ
                  IF (ZRCREP(IS,IN,ID).GE.ZERO) THEN
                     XXX          = SQRT(ZRCREP(IS,IN,ID))
                     ZR(JLIN+IN-1)= ZR(JLIN+IN-1)+ XXX
                  ENDIF
 14            CONTINUE
            ELSE
C              --- COMBINAISON VALEUR ABSOLUE ---
               DO 16 IN = 1,NEQ
                  XXX          = SQRT(ABS(ZRCREP(IS,IN,ID)))
                  ZR(JABS+IN-1)= ZR(JABS+IN-1)+ XXX
 16            CONTINUE
            ENDIF
 20      CONTINUE
         DO 30 IN = 1,NEQ
            XX1 = ZR(JLIN+IN-1) * ZR(JLIN+IN-1)
            XX2 = ZR(JABS+IN-1) * ZR(JABS+IN-1)
            ZRDIR(IN,ID) = ZR(JQUA+IN-1)+XX1+XX2
 30      CONTINUE
         CALL JEDETR('&&ASDIR.QUAD')
         CALL JEDETR('&&ASDIR.LINE')
         CALL JEDETR('&&ASDIR.ABS ')
      ENDIF
 9999 CONTINUE
      CALL JEDEMA()
      END

       SUBROUTINE CFMAJM(RESOCO, NDIM, NBLIAC, LLF, LLF1, LLF2)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C ======================================================================
       IMPLICIT      NONE
       INTEGER       NDIM, NBLIAC, LLF, LLF1, LLF2
       CHARACTER*24  RESOCO
C ======================================================================
C ----------------------------------------------------------------------
C --- BUT : MISE A JOUR DU VECTEUR MU ----------------------------------
C ----------------------------------------------------------------------
C ======================================================================
C --------------- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------
C ======================================================================
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
C ======================================================================
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C ======================================================================
      INTEGER       JMU, ILIAC, POSIT, POSMU, JMAJMU, DIMMAJ
      INTEGER       POSNBL, POSLF0, POSLF1, POSLF2
      CHARACTER*19  MU
C ======================================================================
      CALL JEMARQ ()
C ======================================================================
      MU       = RESOCO(1:14)//'.MU'
C ======================================================================
      CALL JEVEUO (MU,    'E', JMU   )
C ======================================================================
C --- MISE A JOUR DE MU ------------------------------------------------
C ======================================================================
      POSMU  = 0
      POSNBL = 0
      POSLF0 = NBLIAC
      POSLF1 = NBLIAC + (NDIM-1)*LLF
      POSLF2 = NBLIAC + (NDIM-1)*LLF + LLF1
      DIMMAJ = NBLIAC + (NDIM-1)*LLF + LLF1 + LLF2
      CALL WKVECT ('&&CFMAJM.ORDO','V V R',DIMMAJ,JMAJMU)
      DO 20 ILIAC = 1, DIMMAJ
         ZR(JMAJMU-1+ILIAC) = ZR(JMU-1+ILIAC)
 20   CONTINUE
      DO 10 ILIAC = 1, NBLIAC + LLF + LLF1 + LLF2
         POSMU = POSMU + 1
         CALL CFTYLI(RESOCO, ILIAC, POSIT)
         GOTO (1000, 2000, 3000, 4000) POSIT
C ======================================================================
C --- CAS DU CONTACT ---------------------------------------------------
C ======================================================================
 1000    CONTINUE
         POSNBL = POSNBL + 1
         ZR(JMU-1+POSNBL) = ZR(JMAJMU-1+POSMU)
         GOTO 10
C ======================================================================
C --- CAS DU FROTTEMENT EN 2D OU SUIVANT LES DEUX DIRECTIONS EN 3D -----
C ======================================================================
 2000    CONTINUE
         POSLF0 = POSLF0 + 1
         ZR(JMU-1+POSLF0) = ZR(JMAJMU-1+POSMU)
         IF (NDIM.EQ.3) THEN
            POSMU = POSMU + 1
            ZR(JMU-1+POSLF0+LLF) = ZR(JMAJMU-1+POSMU)
         ENDIF
         GOTO 10
C ======================================================================
C --- CAS DU FROTTEMENT SUIVANT LA PREMIERE DIRECTION ------------------
C ======================================================================
 3000    CONTINUE
         POSLF1 = POSLF1 + 1
         ZR(JMU-1+POSLF1) = ZR(JMAJMU-1+POSMU)
         GOTO 10
C ======================================================================
C --- CAS DU FROTTEMENT SUIVANT LA SECONDE DIRECTION -------------------
C ======================================================================
 4000    CONTINUE
         POSLF2 = POSLF2 + 1
         ZR(JMU-1+POSLF2) = ZR(JMAJMU-1+POSMU)
 10   CONTINUE
C ======================================================================
C --- DESTRUCTION DU VECTEUR TAMPON POUR LA MISE A JOUR ----------------
C ======================================================================
      CALL JEDETR('&&CFMAJM.ORDO')
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
      END

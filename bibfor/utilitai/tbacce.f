      SUBROUTINE TBACCE ( NOMTA, NUMELI, PARA, MODE, VI, VR, VC, VK )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'

      INTEGER             NUMELI, VI
      REAL*8              VR
      COMPLEX*16          VC
      CHARACTER*(*)       NOMTA, PARA, MODE, VK
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
C RESPONSABLE DURAND C.DURAND
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C      ACCES A UNE CELLULE D'UNE LIGNE DE LA TABLE
C         EN MODE LECTURE , LE V. EST EN SORTIE
C         EN MODE ECRITURE, LE V. EST EN DONNEE
C ----------------------------------------------------------------------
C IN     : NOMTA  : NOM DE LA STRUCTURE "TABLE".
C IN     : NUMELI : NUMERO DE LA LIGNE
C IN     : PARA   : PARAMETRE
C IN     : MODE   : ACCES EN MODE ECRITURE 'E' OU LECTURE 'L'
C IN/OUT : VI     : VALEUR POUR LE PARAMETRE "I"
C IN/OUT : VR     : VALEUR POUR LE PARAMETRE "R"
C IN/OUT : VC     : VALEUR POUR LE PARAMETRE "C"
C IN/OUT : VK     : VALEUR POUR LE PARAMETRE "K"
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
      INTEGER       IRET, NBPARA, NBLIGN, JTBNP, JTBLP, J, JVALE, JVALL
      CHARACTER*1   MODACC
      CHARACTER*4   TYPE
      CHARACTER*19  NOMTAB
      CHARACTER*24  NOMJV, NOMJVL, INPAR
      CHARACTER*24 VALK
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      MODACC = MODE
      IF ( MODACC .EQ. 'L' ) THEN
      ELSEIF ( MODACC .EQ. 'E' ) THEN
      ELSE
         CALL U2MESK('F','UTILITAI4_63',1,MODACC)
      ENDIF
C
      NOMTAB = NOMTA
      CALL JEEXIN ( NOMTAB//'.TBBA', IRET )
      IF ( IRET .EQ. 0 ) THEN
         CALL U2MESS('F','UTILITAI4_64')
      ENDIF
C
      CALL JEVEUO ( NOMTAB//'.TBNP' , 'E', JTBNP )
      NBPARA = ZI(JTBNP  )
      NBLIGN = ZI(JTBNP+1)
      IF ( NBPARA .EQ. 0 ) THEN
         CALL U2MESS('F','UTILITAI4_65')
      ENDIF
      IF ( NBLIGN .EQ. 0 ) THEN
         CALL U2MESS('F','UTILITAI4_66')
      ENDIF
      IF ( NUMELI .GT. NBLIGN ) THEN
         CALL U2MESS('F','UTILITAI4_67')
      ENDIF
C
      CALL JEVEUO ( NOMTAB//'.TBLP' , 'L', JTBLP )
C
C     --- VERIFICATION QUE LE PARAMETRE EXISTE DANS LA TABLE ---
C
      INPAR = PARA
      DO 10 J = 1 , NBPARA
         IF ( INPAR .EQ. ZK24(JTBLP+4*(J-1)) ) GOTO 12
 10   CONTINUE
      VALK = INPAR
      CALL U2MESG('F', 'UTILITAI6_89',1,VALK,0,0,0,0.D0)
 12   CONTINUE
C
      TYPE   = ZK24(JTBLP+4*(J-1)+1)
      NOMJV  = ZK24(JTBLP+4*(J-1)+2)
      NOMJVL = ZK24(JTBLP+4*(J-1)+3)
C
      CALL JEVEUO ( NOMJV, MODACC, JVALE )
      CALL JEVEUO ( NOMJVL,MODACC, JVALL )
C
      IF ( TYPE(1:1) .EQ. 'I' ) THEN
         IF ( MODACC .EQ. 'L' ) THEN
            VI = ZI(JVALE+NUMELI-1)
         ELSE
            ZI(JVALE+NUMELI-1) = VI
            ZI(JVALL+NUMELI-1) = 1
         ENDIF
C
      ELSEIF ( TYPE(1:1) .EQ. 'R' ) THEN
         IF ( MODACC .EQ. 'L' ) THEN
            VR = ZR(JVALE+NUMELI-1)
         ELSE
            ZR(JVALE+NUMELI-1) = VR
            ZI(JVALL+NUMELI-1) = 1
         ENDIF
C
      ELSEIF ( TYPE(1:1) .EQ. 'C' ) THEN
         IF ( MODACC .EQ. 'L' ) THEN
            VC = ZC(JVALE+NUMELI-1)
         ELSE
            ZC(JVALE+NUMELI-1) = VC
            ZI(JVALL+NUMELI-1) = 1
         ENDIF
C
      ELSEIF ( TYPE(1:3) .EQ. 'K80' ) THEN
         IF ( MODACC .EQ. 'L' ) THEN
            VK = ZK80(JVALE+NUMELI-1)
         ELSE
            ZK80(JVALE+NUMELI-1) = VK
            ZI  (JVALL+NUMELI-1) = 1
         ENDIF
C
      ELSEIF ( TYPE(1:3) .EQ. 'K32' ) THEN
         IF ( MODACC .EQ. 'L' ) THEN
            VK = ZK32(JVALE+NUMELI-1)
         ELSE
            ZK32(JVALE+NUMELI-1) = VK
            ZI  (JVALL+NUMELI-1) = 1
         ENDIF
C
      ELSEIF ( TYPE(1:3) .EQ. 'K24' ) THEN
         IF ( MODACC .EQ. 'L' ) THEN
            VK = ZK24(JVALE+NUMELI-1)
         ELSE
            ZK24(JVALE+NUMELI-1) = VK
            ZI  (JVALL+NUMELI-1) = 1
         ENDIF
C
      ELSEIF ( TYPE(1:3) .EQ. 'K16' ) THEN
         IF ( MODACC .EQ. 'L' ) THEN
            VK = ZK16(JVALE+NUMELI-1)
         ELSE
            ZK16(JVALE+NUMELI-1) = VK
            ZI  (JVALL+NUMELI-1) = 1
         ENDIF
C
      ELSEIF ( TYPE(1:2) .EQ. 'K8' ) THEN
         IF ( MODACC .EQ. 'L' ) THEN
            VK = ZK8(JVALE+NUMELI-1)
         ELSE
            ZK8(JVALE+NUMELI-1) = VK
            ZI (JVALL+NUMELI-1) = 1
         ENDIF
      ENDIF
C
      CALL JEDEMA()
      END

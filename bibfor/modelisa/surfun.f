      SUBROUTINE SURFUN(CHAR,NOMA)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT    NONE
      CHARACTER*8 CHAR
      CHARACTER*8 NOMA

C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : CALIUN
C ----------------------------------------------------------------------
C
C AFFICHAGE DES RESULTATS DE LA LECTURE DU MOT-CLE LIAISON_UNILATERALE
C
C IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
C IN  NOMA   : NOM DU MAILLAGE
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      CHARACTER*32 JEXNUM
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
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER        IFM,NIV
      CHARACTER*24   LISNOE,POINOE,COEFG,COEFD,PARACU,DIMECU
      INTEGER        JNOE,JPOI,JCOEFG,JCOEFD,JPARA,JDIM
      INTEGER        NBNOE,NCMPG
      INTEGER        NUMNO,JDECAL
      CHARACTER*24   NOEUMA
      CHARACTER*24   CMPGCU,METHCU
      INTEGER        JCMPG,JMETH
      INTEGER        INO,ICMP,TYPCOE
      CHARACTER*8    NOMNO
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)
C
C ======================================================================
C --- INITIALISATIONS
C ======================================================================
C
      NOEUMA = NOMA // '.NOMNOE'
      LISNOE = CHAR(1:8)//'.UNILATE.LISNOE'
      POINOE = CHAR(1:8)//'.UNILATE.POINOE'
      METHCU = CHAR(1:8)//'.UNILATE.METHCU'
      PARACU = CHAR(1:8)//'.UNILATE.PARACU'
      DIMECU = CHAR(1:8)//'.UNILATE.DIMECU'
      COEFG  = CHAR(1:8)//'.UNILATE.COEFG'
      CMPGCU = CHAR(1:8)//'.UNILATE.CMPGCU'
      COEFD  = CHAR(1:8)//'.UNILATE.COEFD'
C
      CALL JEVEUO(LISNOE,'L',JNOE)
      CALL JEVEUO(POINOE,'L',JPOI)
      CALL JEVEUO(METHCU,'L',JMETH)
      CALL JEVEUO(PARACU,'L',JPARA)
      CALL JEVEUO(DIMECU,'L',JDIM)
      CALL JEVEUO(COEFG,'L',JCOEFG)
      CALL JEVEUO(CMPGCU,'L',JCMPG)
      CALL JEVEUO(COEFD,'L',JCOEFD)

C ======================================================================
C                    IMPRESSIONS POUR L'UTILISATEUR
C ======================================================================

      IF (NIV .GE. 2) THEN

        WRITE (IFM,1100) '--------------------------------------'
        WRITE (IFM,1100) '        INFOS LIAISON UNILATERALE     '
        WRITE (IFM,1100) '--------------------------------------'
        WRITE (IFM,*)
        IF (ZI(JMETH).EQ.0) THEN
          WRITE (IFM,1102) 'CONTRAINTES ACTIVES      '
        ELSEIF (ZI(JMETH).EQ.1) THEN
          WRITE (IFM,1102) 'GRADIENT CONJUGUE PROJETE'
        ELSE
          CALL U2MESS('F','MODELISA3_40')
        END IF
C
C --- INFOS GENERALES
C
        TYPCOE = ZI(JDIM+2)
        NBNOE  = ZI(JDIM)
        WRITE (IFM,*)
        WRITE (IFM,1011) NBNOE
        WRITE (IFM,*)


        DO 10 INO=1,NBNOE

          NUMNO = ZI(JNOE+INO-1)
          CALL JENUNO(JEXNUM(NOEUMA,NUMNO),NOMNO)


          WRITE (IFM,1030) NOMNO
          WRITE (IFM,1031) ' --> INEGALITE ai.Ai<C : '

          NCMPG  = ZI(JPOI+INO) - ZI(JPOI+INO-1)
          JDECAL = ZI(JPOI+INO-1)

          WRITE (IFM,1040) '     (ai,Ai)'

          DO 20 ICMP = JDECAL,JDECAL+NCMPG-1
            IF (TYPCOE.EQ.1) THEN
              WRITE (IFM,1041) '     ( ',ZR(JCOEFG-1+ICMP),' , ',
     &                         ZK8(JCMPG-1+ICMP),' )'
            ELSE
              WRITE (IFM,1042) '     ( ',ZK8(JCOEFG-1+ICMP),' , ',
     &                         ZK8(JCMPG-1+ICMP),' )'
            ENDIF
 20       CONTINUE

          WRITE (IFM,1060) '     (C)'
          IF (TYPCOE.EQ.1) THEN
            WRITE (IFM,1061) '     ( ',ZR(JCOEFD-1+INO),' )'
          ELSE
            WRITE (IFM,1062) '     ( ',ZK8(JCOEFD-1+INO),' )'
          ENDIF


 10     CONTINUE

      ENDIF
C
 1011 FORMAT ('<LIA_UNIL> NOMBRE DE NOEUDS:',I5)
 1031 FORMAT ('<LIA_UNIL> ',A25)
 1030 FORMAT ('<LIA_UNIL> NOEUD: ',A18,A8)
 1040 FORMAT ('<LIA_UNIL>',A12)
 1041 FORMAT ('<LIA_UNIL>',A7,1PE12.5,A3,A8,A2)
 1042 FORMAT ('<LIA_UNIL>',A7,A8,A3,A8,A2)
 1060 FORMAT ('<LIA_UNIL>',A8)
 1061 FORMAT ('<LIA_UNIL>',A7,1PE12.5,A2)
 1062 FORMAT ('<LIA_UNIL>',A7,A8,A2)
 1100 FORMAT ('<LIA_UNIL> ',A38)
 1102 FORMAT ('<LIA_UNIL> METHODE: ',A25)
C
      CALL JEDETR('&&SURFUN.TRAV')
C ======================================================================
      CALL JEDEMA()
C
      END

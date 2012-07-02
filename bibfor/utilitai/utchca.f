      SUBROUTINE UTCHCA(CARTEZ,MAZ,NOMAIZ,
     &                  NOCMP,TYPREZ,VALR,VALI,VALC,IER)
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      INTEGER VALI,IER
      REAL*8 VALR
      COMPLEX*16 VALC
      CHARACTER*(*) CARTEZ,MAZ,NOMAIZ,NOCMP,TYPREZ
C RESPONSABLE PELLET J.PELLET
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C ======================================================================
C     EXTRAIRE UNE VALEUR (R/I/C) DANS UNE CARTE
C ----------------------------------------------------------------------
C IN  : CARTEZ : NOM DE LA CARTE DONT ON DESIRE EXTRAIRE 1 COMPOSANTE
C IN  : MAZ  : NOM DU MAILLAGE
C IN  : NOMAIZ : NOM DE LA MAILLE A EXTRAIRE
C IN  : NOCMP  : NOM DU DDL A EXTRAIRE
C IN  : TYPREZ : TYPE DU CHAMP ET DU RESULTAT (R/I/C).
C OUT : VALR   : VALEUR REELLE EXTRAITE
C OUT : VALI   : VALEUR ENTIERE EXTRAITE
C OUT : VALC   : VALEUR COMPLEXE EXTRAITE
C OUT : IER    : CODE RETOUR.
C
C ATTENTION : CETTE ROUTINE EST UN MARTEAU PILON POUR ECRASER UNE MOUCHE
C             ELLE NE DOIT ETRE APPELEE QUE DANS TEST_RESU
C ----------------------------------------------------------------------

      INTEGER IBID,IRET,NUMA,IAD1,JCESC,JCESD,JCESL
      INTEGER JCESV,KCMP,NBCMP,INDIK8
      CHARACTER*1 TYPSCA,KBID
      CHARACTER*4 TYPE
      CHARACTER*19 CART19,CES
      CHARACTER*8 MA, NOMAIL
C     ------------------------------------------------------------------

      CALL JEMARQ()
      IER = 0
      CART19=CARTEZ
      NOMAIL=NOMAIZ
      MA=MAZ
      TYPSCA = TYPREZ

      CALL JELIRA(CART19//'.VALE','TYPE',IBID,TYPE)
      IF (TYPE.NE.'R' .AND. TYPE.NE.'I' .AND. TYPE.NE.'C')
     &  CALL U2MESK('E','UTILITAI5_29',1,TYPE)
      CALL ASSERT(TYPE.EQ.TYPSCA)

      CALL JENONU(JEXNOM(MA//'.NOMMAI',NOMAIL),NUMA)
      CALL ASSERT(NUMA.GT.0)


      CES='&&UTCHCA.CES'
      CALL CARCES(CART19,'ELEM',' ','V',CES,' ',IRET)
      CALL ASSERT(IRET.EQ.0)

      CALL JEVEUO(CES//'.CESD','L',JCESD)
      CALL JEVEUO(CES//'.CESC','L',JCESC)
      CALL JEVEUO(CES//'.CESV','L',JCESV)
      CALL JEVEUO(CES//'.CESL','L',JCESL)

      CALL JELIRA(CES//'.CESC','LONMAX',NBCMP,KBID)
      KCMP = INDIK8(ZK8(JCESC),NOCMP,1,NBCMP)
      IF (KCMP.EQ.0) CALL U2MESK('F','CALCULEL3_5',1,NOCMP)

      CALL CESEXI('C',JCESD,JCESL,NUMA,1,1,KCMP,IAD1)
      IF (IAD1.LE.0) CALL U2MESK('F','CALCULEL3_6',1,NOCMP)

      IF (TYPSCA.EQ.'R') THEN
         VALR=ZR(JCESV-1+IAD1)
      ELSEIF (TYPSCA.EQ.'C') THEN
         VALC=ZC(JCESV-1+IAD1)
      ELSEIF (TYPSCA.EQ.'I') THEN
         VALI=ZI(JCESV-1+IAD1)
      ENDIF


      CALL JEDEMA()
      END

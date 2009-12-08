      SUBROUTINE RDTCNS(MA2,CORRN,CNS1,BASE,CNS2)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 30/06/2008   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE PELLET J.PELLET
      IMPLICIT NONE
      CHARACTER*8 MA2
      CHARACTER*19 CNS1,CNS2
      CHARACTER*(*) CORRN
      CHARACTER*1 BASE
C ---------------------------------------------------------------------
C BUT: REDUIRE UN CHAM_NO_S SUR UN MAILLAGE REDUIT
C ---------------------------------------------------------------------
C ARGUMENTS:
C MA2    IN       K8  : MAILLAGE REDUIT
C CNS1   IN/JXIN  K19 : CHAM_NO_S A REDUIRE
C CNS2   IN/JXOUT K19 : CHAM_NO_S REDUIT
C BASE   IN       K1  : BASE DE CREATION POUR CNS2Z : G/V/L
C CORRN  IN       K*  : NOM DE L'OBJET CONTENANT LA CORRESPONDANCE
C                       INO_RE -> INO

C-----------------------------------------------------------------------

C---- COMMUNS NORMALISES  JEVEUX
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
C     ------------------------------------------------------------------
      INTEGER NBNO1,NBNO2,JCORRN
      INTEGER JCN1K,JCN1D,JCN1V,JCN1L,JCN1C
      INTEGER JCN2D,JCN2V,JCN2L,JCN2C
      INTEGER IBID
      INTEGER NCMP,ICMP
      INTEGER INO2,INO1
      CHARACTER*1 KBID
      CHARACTER*8 NOMGD
      CHARACTER*3 TSCA
C     ------------------------------------------------------------------
      CALL JEMARQ()

      CALL ASSERT(CNS2.NE.' ')
      CALL ASSERT(CNS1.NE.CNS2)

      CALL JEVEUO(CNS1//'.CNSK','L',JCN1K)
      CALL JEVEUO(CNS1//'.CNSD','L',JCN1D)
      CALL JEVEUO(CNS1//'.CNSC','L',JCN1C)
      CALL JEVEUO(CNS1//'.CNSV','L',JCN1V)
      CALL JEVEUO(CNS1//'.CNSL','L',JCN1L)

      NOMGD=ZK8(JCN1K-1+2)
      NBNO1=ZI(JCN1D-1+1)
      NCMP=ZI(JCN1D-1+2)
      CALL ASSERT(NCMP.GT.0)

      CALL DISMOI('F','TYPE_SCA',NOMGD,'GRANDEUR',IBID,TSCA,IBID)



C     1- CREATION DE CNS2 :
C     ---------------------------------------
      CALL CNSCRE(MA2,NOMGD,NCMP,ZK8(JCN1C),BASE,CNS2)
      CALL JEVEUO(CNS2//'.CNSD','L',JCN2D)
      CALL JEVEUO(CNS2//'.CNSC','L',JCN2C)
      CALL JEVEUO(CNS2//'.CNSV','E',JCN2V)
      CALL JEVEUO(CNS2//'.CNSL','E',JCN2L)
      NBNO2=ZI(JCN2D-1+1)
      CALL ASSERT(NBNO2.GT.0)
      CALL ASSERT(NBNO2.LE.NBNO1)


C     3- REMPLISSAGE DES OBJETS .CNSL ET .CNSV :
C     ------------------------------------------
      CALL JEVEUO(CORRN,'L',JCORRN)

      DO 20,ICMP=1,NCMP

        DO 10,INO2=1,NBNO2
          INO1=ZI(JCORRN-1+INO2)
          IF (ZL(JCN1L-1+(INO1-1)*NCMP+ICMP)) THEN
            ZL(JCN2L-1+(INO2-1)*NCMP+ICMP)=.TRUE.

            IF (TSCA.EQ.'R') THEN
              ZR(JCN2V-1+(INO2-1)*NCMP+ICMP)=ZR(JCN1V-1+(INO1-1)*NCMP+
     &          ICMP)
            ELSEIF (TSCA.EQ.'C') THEN
              ZC(JCN2V-1+(INO2-1)*NCMP+ICMP)=ZC(JCN1V-1+(INO1-1)*NCMP+
     &          ICMP)
            ELSEIF (TSCA.EQ.'I') THEN
              ZI(JCN2V-1+(INO2-1)*NCMP+ICMP)=ZI(JCN1V-1+(INO1-1)*NCMP+
     &          ICMP)
            ELSEIF (TSCA.EQ.'L') THEN
              ZL(JCN2V-1+(INO2-1)*NCMP+ICMP)=ZL(JCN1V-1+(INO1-1)*NCMP+
     &          ICMP)
            ELSEIF (TSCA.EQ.'K8') THEN
              ZK8(JCN2V-1+(INO2-1)*NCMP+ICMP)=ZK8(JCN1V-1+(INO1-1)*NCMP+
     &          ICMP)
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF

          ENDIF

   10   CONTINUE
   20 CONTINUE


      CALL JEDEMA()
      END

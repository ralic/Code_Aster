      SUBROUTINE CESMQ1(CES1Z,CES2Z,REDUIT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 18/03/2003   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE VABHHTS J.PELLET
      IMPLICIT NONE
      CHARACTER*(*) CES1Z,CES2Z
      LOGICAL REDUIT
C ------------------------------------------------------------------
C BUT: DIRE SI LE "DOMAINE DE DEFINITION" (DD) DE CES1 EST
C    PLUS REDUIT QUE CELUI DE CES2
C    C'EST A DIRE SI IL EXISTE DES MAILLES/POINTS/SOUS-POINTS
C    POUR LESQUELS CES1 A MOINS DE CMPS QUE CES2
C ------------------------------------------------------------------
C     ARGUMENTS:
C CES1Z   IN/JXIN  K19 : SD CHAM_ELEM_S
C CES2Z   IN/JXIN  K19 : SD CHAM_ELEM_S
C REDUIT  OUT      L   :
C   .FALSE. : DD(CES1) EST INCLUS DANS DD(CES2)
C   .TRUE.  : DD(CES1) N'EST PAS INCLUS DANS DD(CES2)

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
      CHARACTER*32 JEXNOM,JEXNUM
C     ------------------------------------------------------------------
      INTEGER JCE1K,JCE1D,JCE1L,JCE1C,NBMAM,NCMP2,ICMP2
      INTEGER JCE2D,JCE2L,JCE2C,JCE2K,NBPT,NBSP,IPT
      INTEGER ISP,IAD1,IAD2
      INTEGER NCMP1,ICMP1
      INTEGER IMA,KNINDI
      CHARACTER*8 NOCMP
      CHARACTER*19 CES1,CES2
C     ------------------------------------------------------------------
      CALL JEMARQ()


      CES1 = CES1Z
      CES2 = CES2Z


C     1- RECUPERATION D'INFORMATIONS DANS CES1 ET CES2 :
C     --------------------------------------------------
      CALL JEVEUO(CES1//'.CESK','L',JCE1K)
      CALL JEVEUO(CES1//'.CESD','L',JCE1D)
      CALL JEVEUO(CES1//'.CESC','L',JCE1C)
      CALL JEVEUO(CES1//'.CESL','L',JCE1L)

      CALL JEVEUO(CES2//'.CESK','L',JCE2K)
      CALL JEVEUO(CES2//'.CESD','L',JCE2D)
      CALL JEVEUO(CES2//'.CESC','L',JCE2C)
      CALL JEVEUO(CES2//'.CESL','L',JCE2L)

      CALL ASSERT(ZK8(JCE1K-1+1).EQ.ZK8(JCE2K-1+1))
      CALL ASSERT(ZK8(JCE1K-1+2).EQ.ZK8(JCE2K-1+2))
      CALL ASSERT(ZK8(JCE1K-1+3).EQ.ZK8(JCE2K-1+3))
      CALL ASSERT(ZK8(JCE1D-1+1).EQ.ZK8(JCE2D-1+1))

      NBMAM = ZI(JCE1D-1+1)

      NCMP1 = ZI(JCE1D-1+2)
      NCMP2 = ZI(JCE2D-1+2)


C     2- VERIFICATION :
C     ------------------------------------------
      REDUIT=.FALSE.
      DO 80,ICMP2 = 1,NCMP2
        NOCMP = ZK8(JCE2C-1+ICMP2)
        ICMP1 = KNINDI(8,NOCMP,ZK8(JCE1C),NCMP1)

        DO 70,IMA = 1,NBMAM
          NBPT = ZI(JCE2D-1+5+4* (IMA-1)+1)
          NBSP = ZI(JCE2D-1+5+4* (IMA-1)+2)
          DO 60,IPT = 1,NBPT
            DO 50,ISP = 1,NBSP
              CALL CESEXI('C',JCE2D,JCE2L,IMA,IPT,ISP,ICMP2,IAD2)
              IF (IAD2.GT.0) THEN
                IF (ICMP1.EQ.0) THEN
                   REDUIT=.TRUE.
                   GO TO 90
                ELSE
                   CALL CESEXI('C',JCE1D,JCE1L,IMA,IPT,ISP,ICMP1,IAD1)
                   IF (IAD1.LE.0) THEN
                     REDUIT=.TRUE.
                     GO TO 90
                   END IF
                END IF
              END IF

   50       CONTINUE
   60     CONTINUE

   70   CONTINUE
   80 CONTINUE

   90 CONTINUE


      CALL JEDEMA()
      END

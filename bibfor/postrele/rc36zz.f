      SUBROUTINE RC36ZZ ( NOMA, NOMGD, NBCMP, NOCMP, NBMA, LISTMA,
     +                    CHELEM )
      IMPLICIT   NONE
      INTEGER             NBCMP, NBMA, LISTMA(*)
      CHARACTER*8         NOMA, NOMGD
      CHARACTER*16        NOCMP(*)
      CHARACTER*24        CHELEM
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 11/09/2002   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C     MISE A ZERO D'UN CHAM_ELEM DE TYPE ELNO
C
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
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER      JCESD, JCESV, JCESL, IM, IMA, NBPT, DECAL, IAD, IPT,
     +             ICMP, IRET
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL JEEXIN ( CHELEM(1:19)//'.CESD', IRET ) 
C
      IF ( IRET .EQ. 0 ) THEN
         CALL CESCRE ( 'V', CHELEM, 'ELNO', NOMA, NOMGD, NBCMP,NOCMP,
     +                                                -1, -1, -NBCMP )
      ENDIF
C
      CALL JEVEUO ( CHELEM(1:19)//'.CESD', 'L', JCESD ) 
      CALL JEVEUO ( CHELEM(1:19)//'.CESV', 'E', JCESV ) 
      CALL JEVEUO ( CHELEM(1:19)//'.CESL', 'E', JCESL )
C
      DO 20  IM = 1 , NBMA
         IMA = LISTMA(IM)
         NBPT  = ZI(JCESD-1+5+4*(IMA-1)+1)
         DECAL = ZI(JCESD-1+5+4*(IMA-1)+4)
         DO 22  IPT = 1 , NBPT
            DO 24 ICMP = 1 , NBCMP
               IAD = DECAL + (IPT-1)*NBCMP + ICMP
               ZL(JCESL-1+IAD) = .TRUE.
               ZR(JCESV-1+IAD) = 0.D0
 24         CONTINUE
 22      CONTINUE
 20   CONTINUE
C
      CALL JEDEMA( )
C
      END

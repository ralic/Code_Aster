      SUBROUTINE NMMAFR(ITERAT,RESOCO,DEFICO,MATASS)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/06/2004   AUTEUR MABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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

      IMPLICIT NONE
      INTEGER ITERAT
      CHARACTER*14 RESOCO
      CHARACTER*19 MATASS
      CHARACTER*24 DEFICO

C ----------------------------------------------------------------------
C  EST-CE QUE L'ON FAIT UN CALCUL AVEC FROTTEMENT?
C  AUQUEL CAS ON CHANGE LE NOM DE LA MATASS
C ----------------------------------------------------------------------
C
C IN       ITERAT INT  ITERATION DE NEWTON
C IN       RESOCO K14  SD CONTACT
C OUT      MATASS K19  MATRICE ASSEMBLEE
C
C ----------------------------------------------------------------------

C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------

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

C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------

      CHARACTER*24  APPARI
      INTEGER      ICONTA,MATTAN,IBID,TYPALC

C ----------------------------------------------------------------------
      CALL JEMARQ()

      MATASS = '&&MATASS'

      IF (ITERAT.NE.0) THEN
C
         APPARI = RESOCO(1:14)//'.APPARI'
         CALL JEEXIN(APPARI,ICONTA)

         IF (ICONTA.NE.0) THEN
            CALL CFDISC(DEFICO,RESOCO,TYPALC,IBID,IBID,MATTAN)
C           PAS D'APPARIEMENT: ON SORT
            IF (TYPALC.LT.0) THEN
              GOTO 10
            ENDIF
C           SI LA MATRICE TANGENTE GLOBALE EST MODIFIEE
C           PAR LA PRESENCE DE CONTACT/FROTTEMENT
            IF (MATTAN.EQ.1) THEN
              MATASS = '&&NMASFR.MATANG'
            ENDIF
         END IF
      END IF

 10   CONTINUE

      CALL JEDEMA()
      END

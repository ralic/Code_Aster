      SUBROUTINE IRGNR1 ( IFI, NBORDR, COORD, CONNEX, POINT,
     +                    NJVPRI, NBPRI, CNSC, CNSL, CNSV, CNSD )
      IMPLICIT NONE
C
      INTEGER        NBPRI, IFI, NBORDR, CONNEX(*), POINT(*),
     +               CNSC(*), CNSL(*), CNSV(*), CNSD(*) 
      REAL*8         COORD(*)
      CHARACTER*(*)  NJVPRI
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 16/06/2003   AUTEUR CIBHHLV L.VIVAN 
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
C
C     IMPRESSION D'UN CHAM_NO AU FORMAT GMSH :
C     ELEMENT : PENTA6
C     CHAMP   : VECTORIEL ( CMP = DX, DY, DZ )
C
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                     ZK24
      CHARACTER*32                              ZK32
      CHARACTER*80                                       ZK80
      COMMON /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER      IPRI, IMA, IPOIN, LISTNO(6), J, JCNSC, JCNSL, JCNSV,
     +             JCNSD, NCMP, K, JPRI, IOR, INOE
      REAL*8       VALX, VALY, VALZ
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL JEVEUO ( NJVPRI, 'L', JPRI )
C
      DO 10 IPRI = 1 , NBPRI

         IMA = ZI(JPRI-1+IPRI)

         IPOIN = POINT(IMA)

         LISTNO(1) = CONNEX(IPOIN  )
         LISTNO(2) = CONNEX(IPOIN+1)
         LISTNO(3) = CONNEX(IPOIN+2)
         LISTNO(4) = CONNEX(IPOIN+3)
         LISTNO(5) = CONNEX(IPOIN+4)
         LISTNO(6) = CONNEX(IPOIN+5)

         DO 12 J = 1 , 3          
            WRITE(IFI,1000) (COORD(3*(LISTNO(INOE)-1)+J),INOE=1,6)
 12      CONTINUE
C
         DO 14 IOR = 1 , NBORDR

            JCNSC = CNSC(IOR)
            JCNSL = CNSL(IOR)
            JCNSV = CNSV(IOR)
            JCNSD = CNSD(IOR)
            NCMP  = ZI(JCNSD-1+2)        

            DO 16 INOE = 1 , 6

               VALX = 0.D0
               VALY = 0.D0
               VALZ = 0.D0

               DO 18 K = 1 , NCMP

                  IF ( ZK8(JCNSC-1+K) .EQ. 'DX' ) THEN
                     IF (ZL(JCNSL-1+(LISTNO(INOE)-1)*NCMP+K)) THEN
                        VALX = ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+K)
                        IF (ABS(VALX).LE.1.D-99) VALX = 0.D0
                     ENDIF

                  ELSEIF ( ZK8(JCNSC-1+K) .EQ. 'DY' ) THEN
                     IF (ZL(JCNSL-1+(LISTNO(INOE)-1)*NCMP+K)) THEN
                        VALY = ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+K)
                        IF (ABS(VALY).LE.1.D-99) VALY = 0.D0
                     ENDIF

                  ELSEIF ( ZK8(JCNSC-1+K) .EQ. 'DZ' ) THEN
                     IF (ZL(JCNSL-1+(LISTNO(INOE)-1)*NCMP+K)) THEN
                        VALZ = ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+K)
                        IF (ABS(VALZ).LE.1.D-99) VALZ = 0.D0
                     ENDIF
                  ENDIF

 18            CONTINUE

               WRITE(IFI,1000) VALX, VALY, VALZ

 16         CONTINUE

 14      CONTINUE
                 
 10   CONTINUE
C
      CALL JELIBE ( NJVPRI )
C
      CALL JEDEMA()
C
 1000 FORMAT(1P,6(E15.8,1X))
C
      END
